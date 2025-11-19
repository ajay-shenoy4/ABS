# Load required libraries
suppressPackageStartupMessages({
  library(tidyr); library(plyr); library(rJava); library(DBI)
  library(odbc); library(data.table); library(glue)
  library(ggplot2); library(dplyr)
})

options(java.parameters = "-Xmx8000m")

# Set base paths
BaseFolderloc <- "/data/dairy/EMEAProjects/Audit-Data-Loader/"
CustList <- paste0(BaseFolderloc, "Customer - Lists/")
CIloc <- paste0(BaseFolderloc, "Scripts - CI/")
LogLoc <- paste0(BaseFolderloc, "Logs/")

ColMap <- read.csv(paste0(BaseFolderloc, "NatEvalResources/AHDBPowerBIColumnHeaders.csv"))
Sys.setenv(R_CONFIG_FILE = "~/.scapi/config.yml")

# Source necessary files
source("/data/dairy/USERS/ashenoy/CI-Audits/globalINDEXMine.R")
source(paste0("/data/dairy/USERS/ashenoy/CDCB/CDCBMasterFunctionsMine.R"))
source(paste0("/data/dairy/USERS/ashenoy/CI-Audits/CIMasterFunctionsMine.R"))
source(paste0("/data/dairy/USERS/ashenoy/CI-Audits/CIPlotCreatorMine.R"))
source(paste0("/data/dairy/USERS/ashenoy/CI-Audits/KappPlottingMine.R"))
source(paste0("/data/dairy/USERS/ashenoy/CI-Audits/globalnew.R"))

substrRight <- function(x, n) substr(x, nchar(x) - n + 1, nchar(x))

# Connect to main customer metadata DB
con <- dbConnect(odbc(),
                 Driver   = db_cfg_EMEA$driver,
                 Server   = db_cfg_EMEA$server,
                 Database = db_cfg_EMEA$database,
                 UID      = db_cfg_EMEA$uid,
                 PWD      = db_cfg_EMEA$pwd,
                 Port     = db_cfg_EMEA$port)

custs <- data.table(dbGetQuery(con, "SELECT * FROM CustList"))
dbDisconnect(con)

#example datakey:
CustDataKey = "GMS-5024:91"
cat("\n\n--- Running KAPP Analysis for:", CustDataKey, "---\n")

CustRecord <- as.data.frame(custs[EvalType == "CI" & DataKey == CustDataKey])
if (nrow(CustRecord) != 1) {
  cat("ERROR: No unique match for DataKey:", CustDataKey, "\n")
  next
}

FolderName <- CustRecord$Custfolder
Source <- ifelse(CustRecord$DataSource == "GMS:MasterPlan", "GMS", CustRecord$DataSource)
Key <- CustRecord$DataKey
GenusOne <- CustRecord$AccountNumber

cat("Running:", FolderName, " | Source:", Source, " | GenusOne:", GenusOne, "\n")

sqlconn2 <- dbConnect(odbc(),
                      Driver   = db_yaml$geneadvance_sql_db$driver,
                      Server   = db_yaml$geneadvance_sql_db$server,
                      Database = db_yaml$geneadvance_sql_db$database,
                      UID      = db_yaml$geneadvance_sql_db$uid,
                      PWD      = db_yaml$geneadvance_sql_db$pwd,
                      Port     = 1433)

ChrSqlGetData <- paste0("SELECT * FROM kapp WHERE FarmID = '", GenusOne, "'")
datCust <- dbGetQuery(sqlconn2, ChrSqlGetData)
dbDisconnect(sqlconn2)

if (nrow(datCust) == 0) {
  cat("No KAPP data found for:", GenusOne, "\n")
  next
}

names(datCust) <- c("FarmID", "CustName", "Country", "KAM", "email", "HerdSize", "Heif", "Conv", "Sexed", "Beef", 
                    "GenPackSex", "GenPackConv", "GenPackBeef", "GMS", "GMSID", "BVD", "betaC", "kappaC", "NM", "TPI",
                    "Milk", "Fat", "Pro", "PL", "LIV", "SCS", "DPR", "CCR", "HCR", "SCE", "SSB", "DCE", "DSB", "UDC",
                    "FLC", "UDP", "DFM", "BDE", "TRW", "RTP", "TLG", "PTAT", "STA", "MAST", "MET", "KET", "AHI",
                    "reportbatch", "FarmID_OLD", "Polled", "active")

CustDataKeyOld <- datCust$FarmID_OLD
country <- datCust$Country

# PA Data block
PAData <- switch(Source,
                 "CIS" = GetPADataCI(getNEHerdsData(CustDataKey, Source), CustDataKey, datCust),
                 "NMR" = GetPADataCI(getNEHerdsData(CustDataKey, Source), CustDataKey, datCust),
                 "UDF" = GetPADataCI(getNEHerdsData(CustDataKey, Source), CustDataKey, datCust),
                 "GMS" = if (country == "Brazil") GetPADataCI(GetHerdDataBrazil(Key), CustDataKey, datCust)
                 else GetPADataCI(GetHerdDataGMSCI(Key), CustDataKey, datCust),
                 "ANAFI" = GetPADataCI(GetHerdDataANAFI(CustDataKey), CustDataKey, datCust),
                 { cat("Unsupported Source:", Source, "\n"); next }
)

GTDataCI <- GetGTCIData(PAData, c(GenusOne, CustDataKeyOld))
PAGTData <- if (country == "Brazil") {
  merge(GTDataCI, PAData, by.x = "visualid", by.y = "Freezebrand", all = TRUE)
} else {
  merge(PAData, GTDataCI, by.x = "IdNo", by.y = "AnimalId", all.x = TRUE)
}

ReviewData <- GetGTCITraits(GenusOne, CustDataKeyOld, PAGTData, Source)
ReviewData$GenomicIndicator <- ifelse(!is.na(ReviewData$CI), "G", "P")
ReviewData$GenomicIndicator[ReviewData$sample_status == "FAIL"] <- "P"

ReviewData$DatDOB[is.na(ReviewData$DatDOB)] <- if (country == "Brazil") {
  as.Date(ReviewData$dob[is.na(ReviewData$DatDOB)], "%m-%d-%Y")
} else {
  ReviewData$dob[is.na(ReviewData$DatDOB)]
}
ReviewData$CurrentLact[is.na(ReviewData$CurrentLact)] <- 0

ReviewData$Index <- ReviewData$ParentAverage
ReviewData$Index[!is.na(ReviewData$CI)] <- ReviewData$CI[!is.na(ReviewData$CI)]
fail_idx <- which(ReviewData$sample_status == "FAIL")
ReviewData$Index[fail_idx] <- ReviewData$ParentAverage[fail_idx]
ReviewData$CI[ReviewData$sample_status == "FAIL"] <- NA
ReviewData$Index <- round(ReviewData$Index, 0)
ReviewData$animal_idN <- ReviewData$IdNo

AuditData <- ReviewData
AuditTemp <- CIplots(AuditData, CustDataKey, CustRecord, Source)
output_dirs <- list(AuditTemp)

PlotLocation <- AuditTemp[[1]]
setwd(PlotLocation)
filename <- paste0(PlotLocation, CustDataKey, ".zip")
zip(zipfile = filename, files = list.files(PlotLocation))

cat("Audit completed for", FolderName, "with", nrow(AuditData), "animals.\n")
cat("Zipped file created at:", filename, "\n")