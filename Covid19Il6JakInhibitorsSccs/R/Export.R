# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of Covid19Il6JakInhibitorsSccs
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Export all results to tables
#'
#' @description
#' Outputs all results to a folder called 'export', and zips them.
#'
#' @param outputFolder          Name of local folder to place results; make sure to use forward slashes
#'                              (/). Do not use a folder on a network drive since this greatly impacts
#'                              performance.
#' @param databaseId            A short string for identifying the database (e.g. 'Synpuf').
#' @param databaseName          The full name of the database.
#' @param databaseDescription   A short description (several sentences) of the database.
#' @param minCellCount          The minimum cell count for fields contains person counts or fractions.
#' @param maxCores              How many parallel cores should be used? If more cores are made
#'                              available this can speed up the analyses.
#'
#' @export
exportResults <- function(outputFolder,
                          databaseId,
                          databaseName,
                          databaseDescription,
                          minCellCount = 5,
                          maxCores) {
  exportFolder <- file.path(outputFolder, "export")
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  
  exportAnalyses(outputFolder = outputFolder,
                 exportFolder = exportFolder)
  
  exportExposures(outputFolder = outputFolder,
                  exportFolder = exportFolder)
  
  exportOutcomes(outputFolder = outputFolder,
                 exportFolder = exportFolder)
  
  exportMetadata(outputFolder = outputFolder,
                 exportFolder = exportFolder,
                 databaseId = databaseId,
                 databaseName = databaseName,
                 databaseDescription = databaseDescription,
                 minCellCount = minCellCount)
  
  exportMainResults(outputFolder = outputFolder,
                    exportFolder = exportFolder,
                    databaseId = databaseId,
                    minCellCount = minCellCount,
                    maxCores = maxCores)
  
  exportCharacterizations(outputFolder = outputFolder,
                          exportFolder = exportFolder,
                          databaseId = databaseId,
                          minCellCount = minCellCount,
                          maxCores = maxCores)
  
  # Add all to zip file -------------------------------------------------------------------------------
  ParallelLogger::logInfo("Adding results to zip file")
  zipName <- file.path(exportFolder, paste0("Results", databaseId, ".zip"))
  files <- list.files(exportFolder, pattern = ".*\\.csv$")
  oldWd <- setwd(exportFolder)
  on.exit(setwd(oldWd))
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
}

exportAnalyses <- function(outputFolder, exportFolder) {
  ParallelLogger::logInfo("Exporting analyses")
  ParallelLogger::logInfo("- sccs_analysis stable")
  
  tempFileName <- tempfile()
  
  sccsAnalysisListFile <- system.file("settings",
                                      "sccsAnalysisList.json",
                                      package = "Covid19Il6JakInhibitorsSccs")
  sccsAnalysisList <- SelfControlledCaseSeries::loadSccsAnalysisList(sccsAnalysisListFile)
  sccsAnalysisToRow <- function(sccsAnalysis) {
    ParallelLogger::saveSettingsToJson(sccsAnalysis, tempFileName)
    row <- tibble::tibble(analysisId = sccsAnalysis$analysisId,
                          description = sccsAnalysis$description,
                          definition = readChar(tempFileName, file.info(tempFileName)$size))
    return(row)
  }
  sccsAnalysis <- lapply(sccsAnalysisList, sccsAnalysisToRow)
  sccsAnalysis <- dplyr::bind_rows(sccsAnalysis)
  sccsAnalysis <- unique(sccsAnalysis)
  unlink(tempFileName)
  colnames(sccsAnalysis) <- SqlRender::camelCaseToSnakeCase(colnames(sccsAnalysis))
  fileName <- file.path(exportFolder, "sccs_analysis.csv")
  readr::write_csv(sccsAnalysis, fileName)
}

exportExposures <- function(outputFolder, exportFolder) {
  ParallelLogger::logInfo("Exporting exposures")
  ParallelLogger::logInfo("- exposure_of_interest table")
  pathToCsv <- system.file("settings", "TosOfInterest.csv", package = "Covid19Il6JakInhibitorsSccs")
  tosOfInterest <- readr::read_csv(pathToCsv, col_types = readr::cols())
  pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "Covid19Il6JakInhibitorsSccs")
  cohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols())
  createExposureRow <- function(exposureId) {
    atlasName <- as.character(cohortsToCreate$atlasName[cohortsToCreate$cohortId == exposureId])
    name <- cohortsToCreate$name[cohortsToCreate$cohortId == exposureId]
    cohortFileName <- system.file("cohorts", paste0(name, ".json"), package = "Covid19Il6JakInhibitorsSccs")
    definition <- readChar(cohortFileName, file.info(cohortFileName)$size)
    return(tibble::tibble(exposureId = exposureId,
                          exposureName = atlasName,
                          definition = definition))
  }
  exposureOfInterest <- lapply(unique(tosOfInterest$exposureId), createExposureRow)
  exposureOfInterest <- dplyr::bind_rows(exposureOfInterest)
  colnames(exposureOfInterest) <- SqlRender::camelCaseToSnakeCase(colnames(exposureOfInterest))
  fileName <- file.path(exportFolder, "exposure_of_interest.csv")
  write.csv(exposureOfInterest, fileName, row.names = FALSE)
}

exportOutcomes <- function(outputFolder, exportFolder) {
  ParallelLogger::logInfo("Exporting outcomes")
  ParallelLogger::logInfo("- outcome_of_interest table")
  pathToCsv <- system.file("settings", "TosOfInterest.csv", package = "Covid19Il6JakInhibitorsSccs")
  tosOfInterest <- readr::read_csv(pathToCsv, col_types = readr::cols())
  pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "Covid19Il6JakInhibitorsSccs")
  cohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols())
  createOutcomeRow <- function(outcomeId) {
    atlasName <- as.character(cohortsToCreate$atlasName[cohortsToCreate$cohortId == outcomeId])
    name <- as.character(cohortsToCreate$name[cohortsToCreate$cohortId == outcomeId])
    cohortFileName <- system.file("cohorts", paste0(name, ".json"), package = "Covid19Il6JakInhibitorsSccs")
    definition <- readChar(cohortFileName, file.info(cohortFileName)$size)
    return(tibble::tibble(outcomeId = outcomeId,
                      outcomeName = atlasName,
                      definition = definition))
  }
  outcomeOfInterest <- lapply(unique(tosOfInterest$outcomeId), createOutcomeRow)
  outcomeOfInterest <- dplyr::bind_rows(outcomeOfInterest)
  colnames(outcomeOfInterest) <- SqlRender::camelCaseToSnakeCase(colnames(outcomeOfInterest))
  fileName <- file.path(exportFolder, "outcome_of_interest.csv")
  readr::write_csv(outcomeOfInterest, fileName) 
  
  
  ParallelLogger::logInfo("- negative_control_outcome table")
  pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Covid19Il6JakInhibitorsSccs")
  negativeControls <- readr::read_csv(pathToCsv, col_types = readr::cols())
  negativeControls <- negativeControls[tolower(negativeControls$type) == "outcome", ]
  negativeControls <- negativeControls[, c("outcomeId", "outcomeName")]
  colnames(negativeControls) <- SqlRender::camelCaseToSnakeCase(colnames(negativeControls))
  fileName <- file.path(exportFolder, "negative_control_outcome.csv")
  readr::write_csv(negativeControls, fileName)
  
  
  # synthesisSummaryFile <- file.path(outputFolder, "SynthesisSummary.csv")
  # if (file.exists(synthesisSummaryFile)) {
  #   positiveControls <- read.csv(synthesisSummaryFile, stringsAsFactors = FALSE)
  #   pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Covid19Il6JakInhibitorsSccs")
  #   negativeControls <- read.csv(pathToCsv)
  #   positiveControls <- merge(positiveControls,
  #                             negativeControls[, c("outcomeId", "outcomeName")])
  #   positiveControls$outcomeName <- paste0(positiveControls$outcomeName,
  #                                          ", RR = ",
  #                                          positiveControls$targetEffectSize)
  #   positiveControls <- positiveControls[, c("newOutcomeId",
  #                                            "outcomeName",
  #                                            "exposureId",
  #                                            "outcomeId",
  #                                            "targetEffectSize")]
  #   colnames(positiveControls) <- c("outcomeId",
  #                                   "outcomeName",
  #                                   "exposureId",
  #                                   "negativeControlId",
  #                                   "effectSize")
  #   colnames(positiveControls) <- SqlRender::camelCaseToSnakeCase(colnames(positiveControls))
  #   fileName <- file.path(exportFolder, "positive_control_outcome.csv")
  #   write.csv(positiveControls, fileName, row.names = FALSE)
  # }
}

exportMetadata <- function(outputFolder,
                           exportFolder,
                           databaseId,
                           databaseName,
                           databaseDescription,
                           minCellCount) {
  ParallelLogger::logInfo("Exporting metadata")
  
  ParallelLogger::logInfo("- database table")
  database <- tibble::tibble(database_id = databaseId,
                         database_name = databaseName,
                         description = databaseDescription,
                         is_meta_analysis = 0)
  fileName <- file.path(exportFolder, "database.csv")
  readr::write_csv(database, fileName)
  
  
  ParallelLogger::logInfo("- sccs_time_dist table")
  getResult <- function(row) {
    sccsEraData <- SelfControlledCaseSeries::loadSccsEraData(file.path(outputFolder, "sccsOutput", row$sccsEraDataFolder[1]))
    if (is.null(sccsEraData$outcomes)) {
      return(NULL)
    } else {
      covariateRef <- ff::as.ram(sccsEraData$covariateRef)
      
      exposureCovariateId <- covariateRef$covariateId[grepl("Exposure of interest", covariateRef$covariateName)]
      
      idx <- sccsEraData$covariates$covariateId == exposureCovariateId
      if (!any(idx)) {
        observationDaysDist <- rep(0, 7)
        outcomeCount <- 0
      } else {
        exposedSubjects <- ffbase::unique.ff(sccsEraData$covariates$stratumId[idx, ])
        observationDaysPerPerson <- aggregate(time ~ stratumId, sccsEraData$outcomes[ffbase::`%in%`(sccsEraData$outcomes$stratumId,
                                                                                                    exposedSubjects), ], sum)
        observationDaysDist <- quantile(observationDaysPerPerson$time, c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1))
        
        # Temporary hack: outcome count and exposed outcome count would ideally be recorded as part of results, not meta-data
        outcomeCount <- sum(sccsEraData$outcomes$y[ffbase::`%in%`(sccsEraData$outcomes$stratumId,
                                                                  exposedSubjects), ])
      }
      
      idx <- sccsEraData$covariates$covariateId == exposureCovariateId
      if (!any(idx)) {
        exposureDaysDist <- rep(0, 7)
        exposedOutcomeCount <- 0
      } else {
        exposedEras <- sccsEraData$covariates$rowId[idx, ]
        exposureDaysPerPerson <- aggregate(time ~ stratumId, sccsEraData$outcomes[ffbase::`%in%`(sccsEraData$outcomes$rowId,
                                                                                                 exposedEras), ], sum)
        exposureDaysDist <- quantile(exposureDaysPerPerson$time, c(0, 0.1, 0.25, 0.5, 0.85, 0.9, 1))
        
        exposedOutcomeCount <- sum(sccsEraData$outcomes$y[ffbase::`%in%`(sccsEraData$outcomes$rowId,
                                            exposedEras), ])
      }
      
      row <- tibble::tibble(exposureId = row$exposureId[1],
                            outcome_id = row$outcomeId[1],
                            analysis_id = row$analysisId[1],
                            outcomes = outcomeCount,
                            exposed_outcomes = exposedOutcomeCount,
                            min_observation_days = observationDaysDist[1],
                            p10_observation_days = observationDaysDist[2],
                            p25_observation_days = observationDaysDist[3],
                            median_observation_days = observationDaysDist[4],
                            p75_observation_days = observationDaysDist[5],
                            p90_observation_days = observationDaysDist[6],
                            max_observation_days = observationDaysDist[7],
                            min_exposure_days = exposureDaysDist[1],
                            p10_exposure_days = exposureDaysDist[2],
                            p25_exposure_days = exposureDaysDist[3],
                            median_exposure_days = exposureDaysDist[4],
                            p75_exposure_days = exposureDaysDist[5],
                            p90_exposure_days = exposureDaysDist[6],
                            max_exposure_days = exposureDaysDist[7])
      return(row)
    }
  }
  pathToCsv <- system.file("settings", "TosOfInterest.csv", package = "Covid19Il6JakInhibitorsSccs")
  tosOfInterest <- readr::read_csv(pathToCsv, col_types = readr::cols())
  outcomesOfInterest <- unique(tosOfInterest$outcomeId)
  reference <- readRDS(file.path(outputFolder, "sccsOutput", "outcomeModelReference.rds"))
  reference <- reference[reference$outcomeId %in% outcomesOfInterest, ]
  # Important: can only use data that hasn't been reweighted by adjustment for event-dependent observation
  reference <- reference[reference$analysisId == 1, ]
  
  results <- plyr::llply(split(reference, reference$sccsEraDataFolder), getResult, .progress = "text")
  results <- dplyr::bind_rows(results)
  results$database_id <- rep(databaseId, nrow(results))
  results <- enforceMinCellValue(results, "outcomes", minCellCount)
  results <- enforceMinCellValue(results, "exposed_outcomes", minCellCount)
  fileName <- file.path(exportFolder, "sccs_time_dist.csv")
  readr::write_csv(results, fileName)
  rm(results)  # Free up memory
}

enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
  if (!silent) {
    percent <- round(100 * sum(toCensor)/nrow(data), 1)
    ParallelLogger::logInfo("   censoring ",
                            sum(toCensor),
                            " values (",
                            percent,
                            "%) from ",
                            fieldName,
                            " because value below minimum")
  }
  if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}

exportCharacterizations <- function(outputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount,
                              maxCores) {
  ParallelLogger::logInfo("Exporting characterizations")
  ParallelLogger::logInfo("- covariate_value table")
  fileName <- file.path(exportFolder, "covariate_value.csv")
  if (file.exists(fileName)) {
    unlink(fileName)
  }
  
  covariate <- tibble::tibble()
  
  first <- TRUE
  characterizationFolder <- file.path(outputFolder, "characterization")
  files <- list.files(characterizationFolder, pattern = "covariateData_.*", full.names = TRUE)
  pb <- txtProgressBar(style = 3)
  for (i in 1:length(files)) {
    ids <- gsub("^.*covariateData_e", "", files[i])
    if (grepl("_o", ids)) {
      exposureId <- as.numeric(gsub("_o.*", "", ids))
      outcomeId <- as.numeric(gsub(".*_o", "", ids))
    } else {
      exposureId <- as.numeric(ids)
      outcomeId <- NA
    }
    covariateData <- FeatureExtraction::loadCovariateData(files[i])
    result <- data.frame()
    if (!is.null(covariateData$covariates)) {
      counts <- as.numeric(ff::as.ram(covariateData$covariates$sumValue))
      n <- covariateData$metaData$populationSize
      binaryCovs <- data.frame(covariateId = ff::as.ram(covariateData$covariates$covariateId),
                               mean = ff::as.ram(covariateData$covariates$averageValue))
      binaryCovs$sd <- sqrt((n * counts + counts)/(n^2))
      result <- rbind(result, binaryCovs)
    }
    if (!is.null(covariateData$covariatesContinuous)) {
      continuousCovs <- data.frame(covariateId = ff::as.ram(covariateData$covariatesContinuous$covariateId),
                                   mean = ff::as.ram(covariateData$covariatesContinuous$averageValue),
                                   sd = ff::as.ram(covariateData$covariatesContinuous$standardDeviation))
      result <- rbind(result, continuousCovs)
    }
    result$databaseId <- rep(databaseId, nrow(result))
    result$exposureId <- rep(exposureId, nrow(result))
    result$outcomeId <- rep(outcomeId, nrow(result))
    if (nrow(result) > 0) {
      result <- enforceMinCellValue(result, "mean", minCellCount/covariateData$metaData$populationSize)
      result$sd[result$mean < 0] <- NA
      result$mean <- round(result$mean, 3)
      result$sd <- round(result$sd, 3)
    }
    colnames(result) <- SqlRender::camelCaseToSnakeCase(colnames(result))
    write.table(x = result,
                file = fileName,
                row.names = FALSE,
                col.names = first,
                sep = ",",
                dec = ".",
                qmethod = "double",
                append = !first)
    
    covariate <- dplyr::bind_rows(covariate, 
                                  ff::as.ram(covariateData$covariateRef)[, c("covariateId", "covariateName")])
    covariate <- unique(covariate)
    
    first <- FALSE
    setTxtProgressBar(pb, i/length(files))
  }
  close(pb)
  
  ParallelLogger::logInfo("- covariate table")
  colnames(covariate) <- SqlRender::camelCaseToSnakeCase(colnames(covariate))
  fileName <- file.path(exportFolder, "covariate.csv")
  readr::write_csv(covariate, fileName)
}

exportMainResults <- function(outputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount,
                              maxCores) {
  ParallelLogger::logInfo("Exporting main results")
  
  
  #TODO: fix
  ParallelLogger::logInfo("- sccs_result table")
  sccsSummary <- read.csv(file.path(outputFolder, "sccsSummary.csv"))
  pathToCsv <- system.file("settings", "NegativeControls.csv", package = "Covid19Il6JakInhibitorsSccs")
  negativeControls <- read.csv(pathToCsv, stringsAsFactors = FALSE)
  
  calibrate <- function(subset) {
    ncs <- merge(subset, negativeControls)
    ncs <- ncs[!is.na(ncs$`seLogRr(Exposure of interest)`) & !is.infinite(ncs$`seLogRr(Exposure of interest)`), ]
    if (nrow(ncs)  != 0) {
      null <- EmpiricalCalibration::fitMcmcNull(logRr = ncs$`logRr(Exposure of interest)`,
                                                seLogRr = ncs$`seLogRr(Exposure of interest)`)
      calP <- EmpiricalCalibration::calibrateP(null, logRr = subset$`logRr(Exposure of interest)`,
                                               seLogRr = subset$`seLogRr(Exposure of interest)`)
      subset$calP <- calP$p
      subset$calPLb <- calP$lb95ci
      subset$calPUb <- calP$ub95ci
      
      model <- EmpiricalCalibration::convertNullToErrorModel(null)
      calCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = subset$`logRr(Exposure of interest)`,
                                                                 seLogRr = subset$`seLogRr(Exposure of interest)`,
                                                                 model = model)
      
      subset$calRr <- exp(calCi$logRr)
      subset$calLb95Rr <- exp(calCi$logLb95Rr)
      subset$calUb95Rr <- exp(calCi$logUb95Rr)
      subset$calLogRr <- calCi$logRr
      subset$calSeLogRr <- calCi$seLogRr
    } 
    return(subset)
  }  
  results <- lapply(split(sccsSummary, sccsSummary$exposureId), calibrate)
  results <- dplyr::bind_rows(results)
  
  
  
  
  
  
}

