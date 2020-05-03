# Copyright 2020 Observational Health Data Sciences and Informatics
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

createCharacterization <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema = cdmDatabaseSchema,
                                   cohortTable,
                                   oracleTempSchema,
                                   outputFolder) {
  # Currently extracted aggregated covariates per exposure and per exposure-outcome. The latter is a strict 
  # subset the former, so probably more efficient to get per-person data per exposure, then filter by 
  # outcome, and aggregate. To do when switched to Andromeda.
  
  characterizationFolder <- file.path(outputFolder, "characterization")
  if (!file.exists(characterizationFolder)) {
    dir.create(characterizationFolder, recursive = TRUE)
  }
  
  # Determine washout period based on first analysis:
  sccsAnalysisListFile <- system.file("settings", "sccsAnalysisList.json", package = "Covid19Il6JakInhibitorsSccs")
  sccsAnalysisList <- SelfControlledCaseSeries::loadSccsAnalysisList(sccsAnalysisListFile)
  washoutDays <- sccsAnalysisList[[1]]$createSccsEraDataArgs$naivePeriod
  
  covariateSettings <- FeatureExtraction::createDefaultCovariateSettings()
  
  pathToCsv <- system.file("settings", "TosOfInterest.csv", package = "Covid19Il6JakInhibitorsSccs")
  tosOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)  
  exposureOutcome <- tosOfInterest[, c("exposureId", "outcomeId")]
  exposureOutcome$cohortDefinitionId <- 1:nrow(exposureOutcome)
  exposureIds <- unique(exposureOutcome$exposureId)
  
  connection <- DatabaseConnector::connect(connectionDetails)  
  on.exit(DatabaseConnector::disconnect(connection))
  
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = "#exposure_outcome",
                                 data = exposureOutcome,
                                 dropTableIfExists = TRUE,
                                 createTable = TRUE,
                                 tempTable = TRUE,
                                 oracleTempSchema = oracleTempSchema,
                                 camelCaseToSnakeCase = TRUE)
  
  ParallelLogger::logInfo("Creating cohort to characterize")
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortsToCharacterize.sql",
                                           packageName = "Covid19Il6JakInhibitorsSccs",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           exposure_database_schema = cohortDatabaseSchema,
                                           exposure_table = cohortTable,
                                           outcome_database_schema = cohortDatabaseSchema,
                                           outcome_table = cohortTable,
                                           washout_days = washoutDays,
                                           first_outcome_only = FALSE)
  DatabaseConnector::executeSql(connection, sql)
  
  sql <- "SELECT cohort_definition_id AS exposure_id, COUNT(DISTINCT subject_id) AS subjects FROM #exposure_cohort GROUP BY cohort_definition_id;"
  exposureCounts <- DatabaseConnector::renderTranslateQuerySql(connection = connection, 
                                                               sql = sql, 
                                                               oracleTempSchema = oracleTempSchema, 
                                                               snakeCaseToCamelCase = TRUE)
  sql <- "SELECT cohort_definition_id AS outcome_id, COUNT(DISTINCT subject_id) AS subjects FROM #outcome_cohort GROUP BY cohort_definition_id;"
  outcomeCounts <- DatabaseConnector::renderTranslateQuerySql(connection = connection, 
                                                              sql = sql, 
                                                              oracleTempSchema = oracleTempSchema, 
                                                              snakeCaseToCamelCase = TRUE)
  sql <- "SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS subjects FROM #eo_cohort GROUP BY cohort_definition_id;"
  eoCounts <- DatabaseConnector::renderTranslateQuerySql(connection = connection, 
                                                         sql = sql, 
                                                         oracleTempSchema = oracleTempSchema, 
                                                         snakeCaseToCamelCase = TRUE)
  eoCounts <- merge(eoCounts, exposureOutcome)
  eoCounts$cohortDefinitionId <- NULL
  subjectCounts <- dplyr::bind_rows(exposureCounts, outcomeCounts, eoCounts)
  saveRDS(subjectCounts, file.path(characterizationFolder, "subjectCounts.rds"))
  
  for (exposureId in exposureIds) {
    echaracterizationFolder <- file.path(characterizationFolder, sprintf("covariateData_e%s", exposureId))
    if (!file.exists(echaracterizationFolder)) {  
      ParallelLogger::logInfo(sprintf("Creating characteristics for exposure %s", exposureId))
      covariateData <- FeatureExtraction::getDbCovariateData(connection = connection,
                                                             oracleTempSchema = oracleTempSchema,
                                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                                             cohortTable = "#exposure_cohort",
                                                             cohortId = exposureId,
                                                             cohortTableIsTemp = TRUE,
                                                             covariateSettings = covariateSettings,
                                                             aggregated = TRUE)
      FeatureExtraction::saveCovariateData(covariateData, echaracterizationFolder)
    }
  }
  
  for (i in 1:nrow(exposureOutcome)) {
    eocharacterizationFolder <- file.path(characterizationFolder, sprintf("covariateData_e%s_o%s", exposureOutcome$exposureId[i], exposureOutcome$outcomeId[i]))
    if (!file.exists(eocharacterizationFolder)) {  
      ParallelLogger::logInfo(sprintf("Creating characteristics for exposure %s and outcome %s", exposureOutcome$exposureId[i], exposureOutcome$outcomeId[i]))
      covariateData <- FeatureExtraction::getDbCovariateData(connection = connection,
                                                             oracleTempSchema = oracleTempSchema,
                                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                                             cohortTable = "#eo_cohort",
                                                             cohortId = exposureOutcome$cohortDefinitionId[i],
                                                             cohortTableIsTemp = TRUE,
                                                             covariateSettings = covariateSettings,
                                                             aggregated = TRUE)
      FeatureExtraction::saveCovariateData(covariateData, eocharacterizationFolder)
    }
  }
  
  sql <- "TRUNCATE TABLE #exposure_cohort; 
    DROP TABLE #exposure_cohort;
    TRUNCATE TABLE #outcome_cohort; 
    DROP TABLE #outcome_cohort;
    TRUNCATE TABLE #eo_cohort;
    DROP TABLE #eo_cohort;
    TRUNCATE TABLE #exposure_outcome;
    DROP TABLE #exposure_outcome;"
  DatabaseConnector::renderTranslateExecuteSql(connection, sql, oracleTempSchema = oracleTempSchema, progressBar = FALSE, reportOverallTime = FALSE)
}