library(Covid19Il6JakInhibitorsSccs)

options(fftempdir = "s:/FFtemp")
connectionDetails <- createConnectionDetails(dbms = "pdw",
                                             server = Sys.getenv("PDW_SERVER"),
                                             port = Sys.getenv("PDW_PORT"))
studyFolder <- "s:/Covid19Il6JakInhibitorsSccs"
maxCores <- parallel::detectCores()

# Optional: E-mail settings if you want to receive an e-mail on errors or completion:
mailSettings <- list(from = Sys.getenv("mailAddress"),
                     to = c(Sys.getenv("mailToAddress")),
                     smtp = list(host.name = Sys.getenv("mailSmtpServer"),
                                 port = Sys.getenv("mailSmtpPort"),
                                 user.name = Sys.getenv("mailAddress"),
                                 passwd = Sys.getenv("mailPassword"),
                                 ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE)
ParallelLogger::addDefaultEmailLogger(mailSettings)

# CCAE settings
databaseId <- "CCAE"
cdmDatabaseSchema <- "CDM_IBM_CCAE_V1103.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_covid19sccs_ccae"
oracleTempSchema <- NULL
databaseName <- "IBM MarketScan Commercial Claims and Encounters Database"
databaseDescription <- "IBM MarketScan® Commercial Claims and Encounters Database (CCAE) represent data from individuals enrolled in United States employer-sponsored insurance health plans. The data includes adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy) as well as enrollment data from large employers and health plans who provide private healthcare coverage to employees, their spouses, and dependents. Additionally, it captures laboratory tests for a subset of the covered lives. This administrative claims database includes a variety of fee-for-service, preferred provider organizations, and capitated health plans."
outputFolder <- file.path(studyFolder, databaseId)

# MDCR settings
databaseId <- "MDCR"
cdmDatabaseSchema <- "CDM_IBM_MDCR_V1104.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_covid19sccs_mdcr"
oracleTempSchema <- NULL
outputFolder <- file.path(studyFolder, databaseId)

# MDCD settings
databaseId <- "MDCD"
cdmDatabaseSchema <- "CDM_IBM_MDCD_V1105.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_covid19sccs_mdcd"
oracleTempSchema <- NULL
outputFolder <- file.path(studyFolder, databaseId)
databaseName <- "Truven Health MarketScan® Multi-State Medicaid Database"
databaseDescription <- "Truven Health MarketScan® Multi-State Medicaid Database (MDCD) adjudicated US health insurance claims for Medicaid enrollees from multiple states and includes hospital discharge diagnoses, outpatient diagnoses and procedures, and outpatient pharmacy claims as well as ethnicity and Medicare eligibility. Members maintain their same identifier even if they leave the system for a brief period however the dataset lacks lab data. [For further information link to RWE site for Truven MDCD."


# JMDC settings
databaseId <- "JMDC"
cdmDatabaseSchema <- "CDM_JMDC_V1106.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_covid19sccs_jmdc"
oracleTempSchema <- NULL
outputFolder <- file.path(studyFolder, databaseId)

# Optum settings
databaseId <- "Optum"
cdmDatabaseSchema <- "CDM_OPTUM_EXTENDED_DOD_V1107.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_covid19sccs_optum"
oracleTempSchema <- NULL
outputFolder <- file.path(studyFolder, databaseId)

# CPRD settings
databaseId <- "CPRD"
cdmDatabaseSchema <- "CDM_CPRD_V1102.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_covid19sccs_cprd"
oracleTempSchema <- NULL
outputFolder <- file.path(studyFolder, databaseId)

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        oracleTempSchema = oracleTempSchema,
        outputFolder = outputFolder,
        databaseId = databaseId,
        createCohorts = FALSE,
        runSccs = TRUE,
        runSccsDiagnostics = TRUE,
        generateBasicOutputTable = TRUE,
        maxCores = maxCores)



# Delete erroneous analyses
pathToCsv <- system.file("settings", "tosOfInterest.csv", package = "Covid19Il6JakInhibitorsSccs")
tosOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
outcomeIds <- unique(tosOfInterest$outcomeId)
for (outcomeId in outcomeIds) {
        filesToDelete <- list.files(outputFolder, sprintf("_o%s", outcomeId), recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
        unlink(filesToDelete, recursive = TRUE)
}
unlink(file.path(outputFolder, "sccsOutput", "SccsData_l1"), recursive = TRUE)
unlink(file.path(outputFolder, "sccsSummary.csv"))
