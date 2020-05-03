prepareTable1Comp <- function(balance,
                              pathToCsv = "Table1Specs.csv") {
  space <- "&nbsp;"
  specifications <- read.csv(pathToCsv, stringsAsFactors = FALSE)
  
  fixCase <- function(label) {
    idx <- (toupper(label) == label)
    if (any(idx)) {
      label[idx] <- paste0(substr(label[idx], 1, 1),
                           tolower(substr(label[idx], 2, nchar(label[idx]))))
    }
    return(label)
  }
  
  resultsTable <- data.frame()
  for (i in 1:nrow(specifications)) {
    if (specifications$analysisId[i] == "") {
      resultsTable <- rbind(resultsTable,
                            data.frame(Characteristic = specifications$label[i], value = ""))
    } else {
      idx <- balance$covariateAnalysisId == specifications$analysisId[i]
      if (any(idx)) {
        if (specifications$covariateIds[i] != "") {
          covariateIds <- as.numeric(strsplit(specifications$covariateIds[i], ";")[[1]])
          idx <- balance$covariateId %in% covariateIds
        } else {
          covariateIds <- NULL
        }
        if (any(idx)) {
          balanceSubset <- balance[idx, ]
          if (is.null(covariateIds)) {
            balanceSubset <- balanceSubset[order(balanceSubset$covariateId), ]
          } else {
            balanceSubset <- merge(balanceSubset, data.frame(covariateId = covariateIds,
                                                             rn = 1:length(covariateIds)))
            balanceSubset <- balanceSubset[order(balanceSubset$rn, balanceSubset$covariateId), ]
          }
          balanceSubset$covariateName <- fixCase(gsub("^.*: ", "", balanceSubset$covariateName))
          if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           MeanT = NA,
                                                           MeanC = NA,
                                                           StdDiff = NA,
                                                           stringsAsFactors = FALSE))
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = paste0(space,
                                                                                   space,
                                                                                   space,
                                                                                   space,
                                                                                   balanceSubset$covariateName),
                                                           MeanT = balanceSubset$mean1,
                                                           MeanC = balanceSubset$mean2,
                                                           StdDiff = balanceSubset$stdDiff,
                                                           stringsAsFactors = FALSE))
          } else {
            resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                           MeanT = balanceSubset$mean1,
                                                           MeanC = balanceSubset$mean2,
                                                           StdDiff = balanceSubset$stdDiff,
                                                           stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
  colnames(resultsTable) <- c("Characteristic", "Exposed", "Exposed with Outcome", "StdDiff")
  return(resultsTable)
}

getCoverage <- function(controlResults) {
  d <- rbind(data.frame(yGroup = "Uncalibrated",
                        logRr = controlResults$logRr,
                        seLogRr = controlResults$seLogRr,
                        ci95Lb = controlResults$ci95Lb,
                        ci95Ub = controlResults$ci95Ub,
                        trueRr = controlResults$effectSize),
             data.frame(yGroup = "Calibrated",
                        logRr = controlResults$calibratedLogRr,
                        seLogRr = controlResults$calibratedSeLogRr,
                        ci95Lb = controlResults$calibratedCi95Lb,
                        ci95Ub = controlResults$calibratedCi95Ub,
                        trueRr = controlResults$effectSize))
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$ci95Lb), ]
  d <- d[!is.na(d$ci95Ub), ]
  if (nrow(d) == 0) {
    return(NULL)
  }

  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$ci95Lb > d$trueRr | d$ci95Ub < d$trueRr

  temp2 <- aggregate(Significant ~ Group + yGroup, data = d, mean)
  temp2$coverage <- (1 - temp2$Significant)

  data.frame(true = temp2$Group, group = temp2$yGroup, coverage = temp2$coverage)
}

plotScatter <- function(controlResults) {
  size <- 2
  labelY <- 0.7
  d <- rbind(data.frame(yGroup = "Uncalibrated",
                        logRr = controlResults$logRr,
                        seLogRr = controlResults$seLogRr,
                        ci95Lb = controlResults$ci95Lb,
                        ci95Ub = controlResults$ci95Ub,
                        trueRr = controlResults$effectSize),
             data.frame(yGroup = "Calibrated",
                        logRr = controlResults$calibratedLogRr,
                        seLogRr = controlResults$calibratedSeLogRr,
                        ci95Lb = controlResults$calibratedCi95Lb,
                        ci95Ub = controlResults$calibratedCi95Ub,
                        trueRr = controlResults$effectSize))
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$ci95Lb), ]
  d <- d[!is.na(d$ci95Ub), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$ci95Lb > d$trueRr | d$ci95Ub < d$trueRr
  temp1 <- aggregate(Significant ~ Group + yGroup, data = d, length)
  temp2 <- aggregate(Significant ~ Group + yGroup, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL

  temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                            "% of CIs include ",
                            temp2$Group)
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  dd$tes <- as.numeric(as.character(dd$Group))

  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)

  d$Group <- paste("True hazard ratio =", d$Group)
  dd$Group <- paste("True hazard ratio =", dd$Group)
  alpha <- 1 - min(0.95 * (nrow(d)/nrow(dd)/50000)^0.1, 0.95)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr), environment = environment()) +
          ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
          ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.025), slope = 1/qnorm(0.025)),
                               colour = rgb(0.8, 0, 0),
                               linetype = "dashed",
                               size = 1,
                               alpha = 0.5,
                               data = dd) +
          ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.975), slope = 1/qnorm(0.975)),
                               colour = rgb(0.8, 0, 0),
                               linetype = "dashed",
                               size = 1,
                               alpha = 0.5,
                               data = dd) +
          ggplot2::geom_point(size = size,
                              color = rgb(0, 0, 0, alpha = 0.05),
                              alpha = alpha,
                              shape = 16) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::geom_label(x = log(0.15),
                              y = 0.9,
                              alpha = 1,
                              hjust = "left",
                              ggplot2::aes(label = nLabel),
                              size = 5,
                              data = dd) +
          ggplot2::geom_label(x = log(0.15),
                              y = labelY,
                              alpha = 1,
                              hjust = "left",
                              ggplot2::aes(label = meanLabel),
                              size = 5,
                              data = dd) +
          ggplot2::scale_x_continuous("Hazard ratio",
                                      limits = log(c(0.1, 10)),
                                      breaks = log(breaks),
                                      labels = breaks) +
          ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
          ggplot2::facet_grid(yGroup ~ Group) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         axis.text.y = themeRA,
                         axis.text.x = theme,
                         axis.title = theme,
                         legend.key = ggplot2::element_blank(),
                         strip.text.x = theme,
                         strip.text.y = theme,
                         strip.background = ggplot2::element_blank(),
                         legend.position = "none")

  return(plot)
}

plotLargeScatter <- function(d, xLabel) {
  d$Significant <- d$ci95Lb > 1 | d$ci95Ub < 1
  
  oneRow <- data.frame(nLabel = paste0(formatC(nrow(d), big.mark = ","), " estimates"),
                       meanLabel = paste0(formatC(100 *
                                                    mean(!d$Significant, na.rm = TRUE), digits = 1, format = "f"), "% of CIs includes 1"))
  
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)
  
  alpha <- 1 - min(0.95 * (nrow(d)/50000)^0.1, 0.95)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1/qnorm(0.025)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1/qnorm(0.975)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5) +
    ggplot2::geom_point(size = 2, color = rgb(0, 0, 0, alpha = 0.05), alpha = alpha, shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.11),
                        y = 1,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = nLabel),
                        size = 5,
                        data = oneRow) +
    ggplot2::geom_label(x = log(0.11),
                        y = 0.935,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = meanLabel),
                        size = 5,
                        data = oneRow) +
    ggplot2::scale_x_continuous(xLabel, limits = log(c(0.1,
                                                       10)), breaks = log(breaks), labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   axis.title = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  return(plot)
}




prettyIrr <- function(x) {
  result <- sprintf("%.2f", x)
  result[is.na(x) | x > 100] <- "NA"
  return(result)
}


compareCohortCharacteristics <- function(characteristics1, characteristics2) {
  
  m <- merge(data.frame(covariateId = characteristics1$covariateId,
                        mean1 = characteristics1$mean,
                        sd1 = characteristics1$sd),
             data.frame(covariateId = characteristics2$covariateId,
                        mean2 = characteristics2$mean,
                        sd2 = characteristics2$sd),
             all = TRUE)
  m$sd <- sqrt(m$sd1^2 + m$sd2^2)
  m$stdDiff <- (m$mean2 - m$mean1)/m$sd
  
  ref <- unique(rbind(characteristics1[,
                                       c("covariateId", "covariateName")],
                      characteristics2[,
                                       c("covariateId", "covariateName")]))
  m <- merge(ref, m)
  m <- m[order(-abs(m$stdDiff)), ]
  return(m)
}