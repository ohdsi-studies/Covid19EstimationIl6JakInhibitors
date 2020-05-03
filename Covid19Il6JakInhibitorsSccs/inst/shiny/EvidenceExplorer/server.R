library(shiny)
library(DT)
source("DataPulls.R")
source("PlotsAndTables.R")

truncateStringDef <- function(columns, maxChars) {
  list(
    targets = columns,
    render = JS(sprintf("function(data, type, row, meta) {\n
      return type === 'display' && data != null && data.length > %s ?\n
        '<span title=\"' + data + '\">' + data.substr(0, %s) + '...</span>' : data;\n
     }", maxChars, maxChars))
  )
}

minCellCountDef <- function(columns) {
  list(
    targets = columns,
    render = JS("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return data.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
    return '<' + Math.abs(data).toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
  }")
  )
}

minCellPercentDef <- function(columns) {
  list(
    targets = columns,
    render = JS("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return (100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,') + '%';
    return '<' + Math.abs(100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,') + '%';
  }")
  )
}

minCellRealDef <- function(columns, digits = 1) {
  list(
    targets = columns,
    render = JS(sprintf("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return data.toFixed(%s).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
    return '<' + Math.abs(data).toFixed(%s).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
  }", digits, digits))
  )
}

styleAbsColorBar <- function(maxValue, colorPositive, colorNegative, angle = 90) {
  JS(sprintf("isNaN(parseFloat(value))? '' : 'linear-gradient(%fdeg, transparent ' + (%f - Math.abs(value))/%f * 100 + '%%, ' + (value > 0 ? '%s ' : '%s ') + (%f - Math.abs(value))/%f * 100 + '%%)'", 
             angle, maxValue, maxValue, colorPositive, colorNegative, maxValue, maxValue))
}

mainColumns <- c("description", 
                 "databaseId", 
                 "rr", 
                 "ci95Lb",
                 "ci95Ub",
                 "p",
                 "calibratedRr", 
                 "calibratedCi95Lb",
                 "calibratedCi95Ub",
                 "calibratedP")

mainColumnNames <- c("<span title=\"Analysis\">Analysis</span>", 
                     "<span title=\"Data source\">Data source</span>", 
                     "<span title=\"Incidence rate ratio (uncalibrated)\">IRR</span>",
                     "<span title=\"Lower bound of the 95 percent confidence interval (uncalibrated)\">LB</span>",
                     "<span title=\"Upper bound of the 95 percent confidence interval (uncalibrated)\">UB</span>", 
                     "<span title=\"Two-sided p-value (uncalibrated)\">P</span>", 
                     "<span title=\"Incidence rate (calibrated)\">Cal.IRR</span>",
                     "<span title=\"Lower bound of the 95 percent confidence interval (calibrated)\">Cal.LB</span>",
                     "<span title=\"Upper bound of the 95 percent confidence interval (calibrated)\">Cal.UB</span>", 
                     "<span title=\"Two-sided p-value (calibrated)\">Cal.P</span>")

shinyServer(function(input, output, session) {
  
  observe({
    exposureId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$exposure]
    toSubset <- tos[tos$exposureId == exposureId, ]
    outcomes <- outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeId %in% toSubset$outcomeId]
    updateSelectInput(session = session,
                      inputId = "outcome",
                      choices = outcomes)
  })
  
  resultSubset <- reactive({
    exposureId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$exposure]
    outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
    analysisIds <- sccsAnalysis$analysisId[sccsAnalysis$description %in% input$analysis]
    databaseIds <- input$database
    if (length(analysisIds) == 0) {
      analysisIds <- -1
    }
    if (length(databaseIds) == 0) {
      databaseIds <- "none"
    }
    results <- getMainResults(exposureIds = exposureId,
                              outcomeIds = outcomeId,
                              databaseIds = databaseIds,
                              analysisIds = analysisIds)
    results <- results[order(results$analysisId), ]
    if (blind) {
      results$rr <- rep(NA, nrow(results))
      results$ci95Ub <- rep(NA, nrow(results))
      results$ci95Lb <- rep(NA, nrow(results))
      results$logRr <- rep(NA, nrow(results))
      results$seLogRr <- rep(NA, nrow(results))
      results$p <- rep(NA, nrow(results))
      results$calibratedRr <- rep(NA, nrow(results))
      results$calibratedCi95Ub <- rep(NA, nrow(results))
      results$calibratedCi95Lb <- rep(NA, nrow(results))
      results$calibratedLogRr <- rep(NA, nrow(results))
      results$calibratedSeLogRr <- rep(NA, nrow(results))
      results$calibratedP <- rep(NA, nrow(results))
    }
    return(results)
  })
  
  selectedRow <- reactive({
    idx <- input$mainTable_rows_selected
    if (is.null(idx)) {
      return(NULL)
    } else {
      subset <- resultSubset()
      if (nrow(subset) == 0) {
        return(NULL)
      }
      row <- subset[idx, ]
      return(row)
    }
  })
  
  output$rowIsSelected <- reactive({
    return(!is.null(selectedRow()))
  })
  outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)
  
  output$mainTable <- renderDataTable({
    table <- resultSubset()
    if (is.null(table) || nrow(table) == 0) {
      return(NULL)
    }
    table$description <- sccsAnalysis$description[match(table$analysisId, sccsAnalysis$analysisId)]
    table <- table[, mainColumns]
    table$rr <- prettyIrr(table$rr)
    table$ci95Lb <- prettyIrr(table$ci95Lb)
    table$ci95Ub <- prettyIrr(table$ci95Ub)
    table$p <- prettyIrr(table$p)
    table$calibratedRr <- prettyIrr(table$calibratedRr)
    table$calibratedCi95Lb <- prettyIrr(table$calibratedCi95Lb)
    table$calibratedCi95Ub <- prettyIrr(table$calibratedCi95Ub)
    table$calibratedP <- prettyIrr(table$calibratedP)
    colnames(table) <- mainColumnNames
    options = list(pageLength = 15,
                   searching = FALSE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE)
    selection = list(mode = "single", target = "row")
    table <- datatable(table,
                       options = options,
                       selection = selection,
                       rownames = FALSE,
                       escape = FALSE,
                       class = "stripe nowrap compact")
    return(table)
  })
  
  output$powerTableCaption <- renderUI({
    row <- selectedRow()
    if (!is.null(row)) {
      text <- "<strong>Table 1a.</strong> The number of outcomes and the number of outcomes while exposed that
      entered the Poisson regression, as well as the minimum detectable relative risk (MDRR)."
      return(HTML(sprintf(text, input$exposure, input$outcome)))
    } else {
      return(NULL)
    }
  })
  
  output$powerTable <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      exposureId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$exposure]
      outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
      # Note: here not filtering by analysis ID, because in this study they'll be the same:
      table <- sccsTimeDist[sccsTimeDist$exposureId == exposureId &
                              sccsTimeDist$outcomeId == outcomeId &
                              sccsTimeDist$databaseId == row$databaseId, ]
      table <- table[, c("outcomes", "exposedOutcomes", "mdrr")]
      colnames(table) <- c("Outcomes",
                           "Exposed outcomes",
                           "MDRR")
      return(table)
    }
  })
  
  output$observationTimeTableCaption <- renderUI({
    row <- selectedRow()
    if (!is.null(row)) {
      text <- "<strong>Table 1b.</strong> Time (days) at risk distribution expressed as
      minimum (min), 25th percentile (P25), median, 75th percentile (P75), and maximum (max) in the target
     (<em>%s</em>) and comparator (<em>%s</em>) cohort after propensity score adjustment."
      return(HTML(sprintf(text, input$exposure, input$outcome)))
    } else {
      return(NULL)
    }
  })
  
  output$observationTimeTable <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      exposureId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$exposure]
      outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
      # Note: here not filtering by analysis ID, because in this study they'll be the same:
      table <- sccsTimeDist[sccsTimeDist$exposureId == exposureId &
                              sccsTimeDist$outcomeId == outcomeId &
                              sccsTimeDist$databaseId == row$databaseId, ]
      table <- table[, c("minObservationDays", "p10ObservationDays", "p25ObservationDays", "medianObservationDays", "p75ObservationDays", "p90ObservationDays", "maxObservationDays")]
      colnames(table) <- c("Min", "P10", "P25", "Median", "P75", "P90", "Max")
      return(table)
    }
  })
  
  output$exposureTimeTableCaption <- renderUI({
    row <- selectedRow()
    if (!is.null(row)) {
      text <- "<strong>Table 1c.</strong> Time (days) at risk distribution expressed as
      minimum (min), 25th percentile (P25), median, 75th percentile (P75), and maximum (max) in the target
     (<em>%s</em>) and comparator (<em>%s</em>) cohort after propensity score adjustment."
      return(HTML(sprintf(text, input$exposure, input$outcome)))
    } else {
      return(NULL)
    }
  })
  
  output$exposureTimeTable <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      exposureId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$exposure]
      outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
      # Note: here not filtering by analysis ID, because in this study they'll be the same:
      table <- sccsTimeDist[sccsTimeDist$exposureId == exposureId &
                              sccsTimeDist$outcomeId == outcomeId &
                              sccsTimeDist$databaseId == row$databaseId, ]
      table <- table[, c("minExposureDays", "p10ExposureDays", "p25ExposureDays", "medianExposureDays", "p75ExposureDays", "p90ExposureDays", "maxExposureDays")]
      colnames(table) <- c("Min", "P10", "P25", "Median", "P75", "P90", "Max")
      return(table)
    }
  })
  
  
  output$overlapPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      exposureId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$exposure]
      outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
      
      # Note, not filtering by analysis id;
      data <- sccsTimeDist[sccsTimeDist$exposureId == exposureId & sccsTimeDist$outcomeId == outcomeId, ]
      plot <- VennDiagram::draw.pairwise.venn(area1 = abs(data$exposureSubjects),
                                              area2 = abs(data$outcomeSubjects),
                                              cross.area = abs(data$exposureOutcomeSubjects),
                                              category = c("Exposed", "Outcome"), 
                                              col = c(rgb(0.8, 0, 0), rgb(0, 0, 0.8)),
                                              fill = c(rgb(0.8, 0, 0), rgb(0, 0, 0.8)),
                                              alpha = 0.2,
                                              fontfamily = rep("sans", 3),
                                              cat.fontfamily = rep("sans", 2),
                                              margin = 0.01,
                                              ind = FALSE)
      # Borrowed from https://stackoverflow.com/questions/37239128/how-to-put-comma-in-large-number-of-venndiagram
      idx <- sapply(plot, function(i) grepl("text", i$name))
      for (i in 1:3) {
        plot[idx][[i]]$label <- format(as.numeric(plot[idx][[i]]$label), big.mark = ",", scientific = FALSE)
      }
      grid::grid.draw(plot)
      
      return(plot)
    }
  }, res = 100)
  
  
  computeBalance <- reactive({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      exposureId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$exposure]
      outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
      
      fileName <- sprintf("covariate_value_e%s_%s.rds", exposureId, row$databaseId)
      covsExposed <- readRDS(file.path(dataFolder, fileName))
      
      fileName <- sprintf("covariate_value_e%s_o%s_%s.rds", exposureId, outcomeId, row$databaseId)
      covsExposedWithOutcome <- readRDS(file.path(dataFolder, fileName))
      
      covsExposed <- merge(covsExposed, covariate)
      covsExposedWithOutcome <- merge(covsExposedWithOutcome, covariate)
      balance <- compareCohortCharacteristics(covsExposed, covsExposedWithOutcome)
      balance$absStdDiff <- abs(balance$stdDiff)
      return(balance)
    }
  })
  
  output$charCompareTable <- renderDataTable({
    balance <- computeBalance()
    if (nrow(balance) == 0) {
      return(NULL)
    }
    
    if (input$charCompareType == "Pretty table") {
      balance <- merge(balance, covariate[, c("covariateId", "covariateAnalysisId")])
      table <- prepareTable1Comp(balance)
      options = list(pageLength = 999,
                     searching = FALSE,
                     lengthChange = FALSE,
                     ordering = FALSE,
                     paging = FALSE,
                     columnDefs = list(minCellPercentDef(1:2))
      )
      table <- datatable(table,
                         options = options,
                         rownames = FALSE,
                         escape = FALSE,
                         class = "stripe nowrap compact")
      table <- formatStyle(table = table,
                           columns = 2:3,
                           background = styleColorBar(c(0,1), "lightblue"),
                           backgroundSize = "98% 88%",
                           backgroundRepeat = "no-repeat",
                           backgroundPosition = "center")
      table <- formatStyle(table = table,
                           columns = 4,
                           background = styleAbsColorBar(1, "lightblue", "pink"),
                           backgroundSize = "98% 88%",
                           backgroundRepeat = "no-repeat",
                           backgroundPosition = "center")
      table <- formatRound(table, 4, digits = 2)
    } else {
      table <- balance
      table <- table[order(table$covariateName), ]
      table <- table[, c("covariateName", "mean1", "sd1", "mean2", "sd2", "stdDiff")]
      colnames(table) <- c("Covariate name", "Mean Target", "SD Target", "Mean Comparator", "SD Comparator", "StdDiff")
      
      options = list(pageLength = 25,
                     searching = TRUE,
                     lengthChange = TRUE,
                     ordering = TRUE,
                     paging = TRUE,
                     columnDefs = list(
                       truncateStringDef(0, 150),
                       minCellRealDef(c(1,3), 2)
                     )
      )
      table <- datatable(table,
                         options = options,
                         rownames = FALSE,
                         escape = FALSE,
                         class = "stripe nowrap compact")
      table <- formatStyle(table = table,
                           columns = c(2,4),
                           background = styleColorBar(c(0,1), "lightblue"),
                           backgroundSize = "98% 88%",
                           backgroundRepeat = "no-repeat",
                           backgroundPosition = "center")
      table <- formatStyle(table = table,
                           columns = 6,
                           background = styleAbsColorBar(1, "lightblue", "pink"),
                           backgroundSize = "98% 88%",
                           backgroundRepeat = "no-repeat",
                           backgroundPosition = "center")
      table <- formatRound(table, c(3, 5, 6), digits = 2)
    }
    return(table)
  })
  
  
  systematicErrorPlot <- reactive({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      exposureId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$exposure]
      controlResults <- getControlResults(exposureId = exposureId,
                                          analysisId = row$analysisId,
                                          databaseId = row$databaseId)
      
      plot <- plotScatter(controlResults)
      return(plot)
    }
  })
  
  output$systematicErrorPlot <- renderPlot({
    return(systematicErrorPlot())
  })
  
  output$downloadSystematicErrorPlotPng <- downloadHandler(filename = "SystematicError.png", 
                                                           contentType = "image/png", 
                                                           content = function(file) {
                                                             ggplot2::ggsave(file, plot = systematicErrorPlot(), width = 12, height = 5.5, dpi = 400)
                                                           })
  
  output$downloadSystematicErrorPlotPdf <- downloadHandler(filename = "SystematicError.pdf", 
                                                           contentType = "application/pdf", 
                                                           content = function(file) {
                                                             ggplot2::ggsave(file = file, plot = systematicErrorPlot(), width = 12, height = 5.5)
                                                           })
  
})
