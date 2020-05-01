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
outputFolder <- file.path(studyFolder, databaseId)
databaseName <- "IBM MarketScan Commercial Claims and Encounters Database"
databaseDescription <- "IBM MarketScan® Commercial Claims and Encounters Database (CCAE) represent data from individuals enrolled in United States employer-sponsored insurance health plans. The data includes adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy) as well as enrollment data from large employers and health plans who provide private healthcare coverage to employees, their spouses, and dependents. Additionally, it captures laboratory tests for a subset of the covered lives. This administrative claims database includes a variety of fee-for-service, preferred provider organizations, and capitated health plans."

# MDCR settings
databaseId <- "MDCR"
cdmDatabaseSchema <- "CDM_IBM_MDCR_V1104.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_covid19sccs_mdcr"
oracleTempSchema <- NULL
outputFolder <- file.path(studyFolder, databaseId)
databaseName <- "IBM MarketScan Medicare Supplemental and Coordination of Benefits Database"
databaseDescription <- "IBM MarketScan® Medicare Supplemental and Coordination of Benefits Database (MDCR) represents health services of retirees in the United States with primary or Medicare supplemental coverage through privately insured fee-for-service, point-of-service, or capitated health plans. These data include adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy). Additionally, it captures laboratory tests for a subset of the covered lives."

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
databaseName <- "Optum’s Clinformatics® Extended Data Mart"
databaseDescription <- "Optum Clinformatics Extended DataMart is an adjudicated US administrative health claims database for members of private health insurance, who are fully insured in commercial plans or in administrative services only (ASOs), Legacy Medicare Choice Lives (prior to January 2006), and Medicare Advantage (Medicare Advantage Prescription Drug coverage starting January 2006). The population is primarily representative of commercial claims patients (0-65 years old) with some Medicare (65+ years old) however ages are capped at 90 years. It includes data captured from administrative claims processed from inpatient and outpatient medical services and prescriptions as dispensed, as well as results for outpatient lab tests processed by large national lab vendors who participate in data exchange with Optum. This dataset also provides date of death (month and year only) for members with both medical and pharmacy coverage from the Social Security Death Master File (however after 2011 reporting frequency changed due to changes in reporting requirements) and location information for patients is at the US state level."

# CPRD settings
databaseId <- "CPRD"
cdmDatabaseSchema <- "CDM_CPRD_V1102.dbo"
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "mschuemi_covid19sccs_cprd"
oracleTempSchema <- NULL
outputFolder <- file.path(studyFolder, databaseId)
databaseName <- "Clinical Practice Research Datalink"
databaseDescription <- "The Clinical Practice Research Datalink (CPRD) is a governmental, not-for-profit research service, jointly funded by the NHS National Institute for Health Research (NIHR) and the Medicines and Healthcare products Regulatory Agency (MHRA), a part of the Department of Health, United Kingdom (UK). CPRD consists of data collected from UK primary care for all ages. This includes conditions, observations, measurements, and procedures that the general practitioner is made aware of in additional to any prescriptions as prescribed by the general practitioner. In addition to primary care, there are also linked secondary care records for a small number of people.\nThe major data elements contained within this database are outpatient prescriptions given by the general practitioner (coded with Multilex codes) and outpatient clinical, referral, immunization or test events that the general practitioner knows about (coded in Read or ICD10 or LOINC codes). The database also contains the patients’ year of births and any date of deaths."

# This function will ask you if you're sure:
deleteHoiFiles(outputFolder)

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
