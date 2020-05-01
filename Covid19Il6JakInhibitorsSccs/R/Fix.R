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

#' Delete health outcomes of interest result files
#'
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#'
#' @export
deleteHoiFiles <- function(outputFolder) {
  pathToCsv <- system.file("settings", "tosOfInterest.csv", package = "Covid19Il6JakInhibitorsSccs")
  tosOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
  outcomeIds <- unique(tosOfInterest$outcomeId)
  filesToDelete <- c()
  for (outcomeId in outcomeIds) {
    filesToDelete <- c(filesToDelete,
                       list.files(outputFolder, 
                                  sprintf("_o%s", outcomeId), 
                                  recursive = TRUE, 
                                  include.dirs = TRUE, 
                                  full.names = TRUE))
    unlink(filesToDelete, recursive = TRUE)
  }
  filesToDelete <- c(filesToDelete, file.path(outputFolder, "sccsOutput", "SccsData_l1"))
  filesToDelete <- c(filesToDelete, file.path(outputFolder, "sccsSummary.csv"))
  writeLines("About to delete:")
  writeLines(filesToDelete)
  sure <- readline("Are you sure? (y/n)")  
  if (sure == "y") {
     writeLines("Deleting files")
    unlink(filesToDelete, recursive = TRUE)
  } else {
    writeLines("Not deleting files because user is not sure") 
  }
}