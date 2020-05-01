library(shiny)
library(DT)
source("DataPulls.R")
source("PlotsAndTables.R")

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
      text <- "<strong>Table 1a.</strong> Number of subjects, follow-up time (in years), number of outcome
      events, and event incidence rate (IR) per 1,000 patient years (PY) in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after propensity score adjustment, as  well as the minimum detectable  relative risk (MDRR).
      Note that the IR does not account for any stratification."
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


  systematicErrorPlot <- reactive({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
      comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
      controlResults <- getControlResults(connection = connection,
                                          targetId = targetId,
                                          comparatorId = comparatorId,
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
