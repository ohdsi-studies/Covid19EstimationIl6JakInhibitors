This page describes how to rerun the study, for those who already executed the first run. 

What has changed?
=================

The previous run accidentally created empty result files for the outcomes of interest. That will be fixed in this run. Also, the previous run did not create enough artifacts to support writing a paper about the study. This new run produces output somewhat similar to our cohort studies, and includes more descriptives including exposure cohort characterization and statistical power. These new results will be shown in a Shiny app, like our cohort studies.

How to run
==========

Please use the same `outputFolder` as before. We can reuse most of what was already computed.

1. First, update the package:
  ```r
  devtools::install_github("ohdsi/OhdsiSharing")
  devtools::install_github("ohdsi-studies/Covid19EstimationHydroxychloroquine/Covid19Il6JakInhibitorsSccs")
  ```
  
2. Next, delete the empty results files by changing and running this code:
  ```r
  library(Covid19Il6JakInhibitorsSccs)
  
  # The folder where the study intermediate and result files are written:
  outputFolder <- "c:/Covid19Il6JakInhibitorsSccs"

  # Note: this function will ask you if you're sure. Reply with 'y' if you're sure:
  deleteHoiFiles(outputFolder)
  ```

2. Execute the study by modifying and executing the following code:
  ```r
  library(Covid19Il6JakInhibitorsSccs)
  
  # Optional: specify where the temporary files (used by the ff package) will be created:
  options(fftempdir = "c:/fftemp")
  
  # Maximum number of cores to be used:
  maxCores <- parallel::detectCores()
  
  # The folder where the study intermediate and result files will be written:
  outputFolder <- "c:/Covid19Il6JakInhibitorsSccs"
  
  # Details for connecting to the server:
  # See ?DatabaseConnector::createConnectionDetails for help
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "",
                                                                  server = "",
                                                                  user = "",
                                                                  password = "")
  
  # The name of the database schema where the CDM data can be found:
  cdmDatabaseSchema <- ""
  
  # The name of the database schema and table where the study-specific cohorts will be instantiated:
  cohortDatabaseSchema <- ""
  cohortTable <- ""
  
  # Some meta-information that will be used by the export function:
  databaseId <- "" # A short name of the database, to be used to create file names.
  databaseName <- "" # The full name of the database
  databaseDescription <- "" # A paragraph describing the database

  
  # If using Oracle, define a schema that can be used to emulate temp tables. Otherwise set as NULL:
  oracleTempSchema <- NULL
  
  execute(connectionDetails = connectionDetails,
          cdmDatabaseSchema = cdmDatabaseSchema,
          cohortDatabaseSchema = cohortDatabaseSchema,
          cohortTable = cohortTable,
          oracleTempSchema = oracleTempSchema,
          outputFolder = outputFolder,
          databaseId = databaseId,
          databaseName = databaseName,
          databaseDescription = databaseDescription,
          createCohorts = TRUE,
          runSccs = TRUE,
          createCharacterization = TRUE,
          runSccsDiagnostics = TRUE,
          exportResults = TRUE,
          maxCores = maxCores) 
  ```
  
3. Send the created zip file (and only the zip file) to the study coordinator. This can be done using the 'sccsIl6JakInhibitors' folder on the OHDSI SFTP server:
  ```r
  # For uploading the results. You should have received the key file from the study coordinator:
  keyFileName <- "c:/home/keyFiles/study-data-site-covid19.dat"
  userName <- "study-data-site-covid19"
  
  # Upload results to OHDSI SFTP server:
  uploadResults(outputFolder, keyFileName, userName)
  ```
