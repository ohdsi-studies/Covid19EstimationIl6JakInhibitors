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
databaseName <- "Japan Medical Data Center"
databaseDescription <- "Japan Medical Data Center (JDMC) database consists of data from 60 Society-Managed Health Insurance plans covering workers aged 18 to 65 and their dependents (children younger than 18 years old and elderly people older than 65 years old). JMDC data includes membership status of the insured people and claims data provided by insurers under contract (e.g. patient-level demographic information, inpatient and outpatient data inclusive of diagnosis and procedures, and prescriptions as dispensed claims information). Claims data are derived from monthly claims issued by clinics, hospitals and community pharmacies; for claims only the month and year are provided however prescriptions, procedures, admission, discharge, and start of medical care as associated with a full date.\nAll diagnoses are coded using ICD-10. All prescriptions refer to national Japanese drug codes, which have been linked to ATC. Procedures are encoded using local procedure codes, which the vendor has mapped to ICD-9 procedure codes. The annual health checkups report a standard battery of measurements (e.g. BMI), which are not coded but clearly described."

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
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        createCohorts = FALSE,
        runSccs = FALSE,
        createCharacterization = FALSE,
        runSccsDiagnostics = FALSE,
        exportResults = TRUE,
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
