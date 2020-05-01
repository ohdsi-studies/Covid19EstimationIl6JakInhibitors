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

#' Prepare results for the Evidence Explorer Shiny app.
#'
#' @param resultsFolder   Folder where the exported zip files from one or more databases are stored. 
#' @param shinyDataFolder Folder where the data files for the Shiny app will be written.
#' 
#' @export
prepareForEvidenceExplorer <- function(resultsFolder, shinyDataFolder) {
  zipFiles <- list.files(resultsFolder, pattern = ".zip", full.names = TRUE)
  
  dontMerge <- c("covariate_value")
  
  saveSubset <- function(subset, tableName) {
    exposureId <- subset$exposureId[1]
    outcomeId <- subset$outcomeId[1]
    databaseId <- subset$databaseId[1]
    fileName <- sprintf("%s_e%s_o%s_%s.rds", tableName, exposureId, outcomeId, databaseId)
    saveRDS(subset, file.path(shinyDataFolder, fileName))
  }
  
  loadFile <- function(file, folder, overwrite) {
    # print(file)
    tableName <- gsub(".csv$", "", file)
    camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
    data <- readr::read_csv(file.path(folder, file), col_types = readr::cols(), guess_max = 1e7, locale = readr::locale(encoding = "UTF-8"))
    colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
    
    if (tableName %in% dontMerge) {
      subsets <- split(data, list(data$exposureId, data$outcomeId))
      invisible(lapply(subsets, saveSubset, tableName = tableName))
    } else {
      if (!overwrite && exists(camelCaseName, envir = .GlobalEnv)) {
        existingData <- get(camelCaseName, envir = .GlobalEnv)
        if (nrow(existingData) > 0) {
          if (nrow(data) > 0) {
            if (all(colnames(existingData) %in% colnames(data)) &&
                all(colnames(data) %in% colnames(existingData))) {
              data <- data[, colnames(existingData)]
            } else {
              stop("Table columns do no match previously seen columns. Columns in ", 
                   file, 
                   ":\n", 
                   paste(colnames(data), collapse = ", "), 
                   "\nPrevious columns:\n",
                   paste(colnames(existingData), collapse = ", "))
              
            }
          }
        }
        data <- dplyr::bind_rows(existingData, data)
        data <- unique(data)
      }
      assign(camelCaseName, data, envir = .GlobalEnv)
    }
    invisible(NULL)
  }
  tableNames <- c()
  for (i in 1:length(zipFiles)) {
    writeLines(paste("Processing", zipFiles[i]))
    tempFolder <- tempfile()
    dir.create(tempFolder)
    unzip(zipFiles[i], exdir = tempFolder)
    
    csvFiles <- list.files(tempFolder, pattern = ".csv")
    tableNames <- c(tableNames, csvFiles)
    invisible(lapply(csvFiles, loadFile, folder = tempFolder, overwrite = (i == 1)))
    
    unlink(tempFolder, recursive = TRUE)
  }
  
  tableNames <- unique(tableNames)
  tableNames <- gsub(".csv$", "", tableNames)
  tableNames <- tableNames[!tableNames == dontMerge]
  tableNames <- SqlRender::snakeCaseToCamelCase(tableNames)
  save(list = tableNames, file = file.path(shinyDataFolder, "shinyData.RData"), compress = TRUE) 
  ParallelLogger::logInfo("Shiny data saved in ", shinyDataFolder)
}

#' Launch the Evidence Explorer Shiny app
#'
#' @param shinyDataFolder  A folder where the Shiny data are stored. Use
#'                         the \code{\link{prepareForEvidenceExplorer}} function to generate these files.
#' @param blind            Should the user be blinded to the main results?
#' @param launch.browser   Should the app be launched in your default browser, or in a Shiny window.
#'                         Note: copying to clipboard will not work in a Shiny window.
#'
#' @details
#' Launches a Shiny app that allows the user to explore the study results.
#'
#' @export
launchEvidenceExplorer <- function(dataFolder, blind = TRUE, launch.browser = FALSE) {
  ensure_installed("shiny")
  # ensure_installed("shinydashboard")
  ensure_installed("DT")
  # ensure_installed("VennDiagram")
  ensure_installed("htmltools")
  appDir <- system.file("shiny", "EvidenceExplorer", package = "Covid19Il6JakInhibitorsSccs")
  shinySettings <- list(dataFolder = dataFolder,
                        blind = blind)
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm(shinySettings, envir = .GlobalEnv))
  shiny::runApp(appDir)
}

# Borrowed from devtools: https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), 
                                error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

# Borrowed and adapted from devtools: https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <- paste0(sQuote(pkg), " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}
