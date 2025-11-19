runcdcbreview <- function(CustDataKey) {
  library(stringr)
  library(ggplot2)
  library(gridExtra)
  library(ggpmisc)
  library(grid)
  library(DBI)
  library(odbc)
  library(data.table)
  library(dplyr)
  
  options(scipen = 999)
  
  # === Step 1: Set Paths ===
  BaseFolderloc <<- "/data/dairy/EMEAProjects/Audit-Data-Loader/"
  CDCBloc <<- paste0(BaseFolderloc, "Scripts - CDCB/")
  AHDBFileLoc <<- paste0(BaseFolderloc, "Scripts - AHDB/AHDB-Files")
  LogLoc <<- paste0(BaseFolderloc, "Logs/")
  DatasetLoc <<- paste0(BaseFolderloc, "Datasets/CDCB/")
  WorkDirectory <<- "/data/dairy/USERS/ashenoy/CDCB/"
  
  Sys.setenv(R_CONFIG_FILE = "~/.scapi/config.yml")
  
  source(paste0(WorkDirectory, "globalINDEX.R"))
  source(paste0(WorkDirectory, "CDCBMasterFunctionsMine.R"))
  
  # === Step 2: Connect and Load Customer ===
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = db_cfg_EMEA$driver,
                        Server = db_cfg_EMEA$server,
                        Database = db_cfg_EMEA$database,
                        UID = db_cfg_EMEA$uid,
                        PWD = db_cfg_EMEA$pwd,
                        Port = db_cfg_EMEA$port)
  
  custs <- data.table(dbGetQuery(con, "SELECT * FROM CustList"))
  dbDisconnect(con)
  
  custcreds <- custs[EvalType == "CDCB" & DataKey == CustDataKey]
  if (nrow(custcreds) == 0) {
    cat("No matching credentials found for:", CustDataKey, "\n")
    return(NULL)
  }
  
  ShortName <- custcreds$Custfolder
  cat("Running audit for", ShortName, "\n")
  
  key_parts <- unlist(strsplit(CustDataKey, ":"))
  if (length(key_parts) != 2) return(NULL)
  
  CustomerNumber <- key_parts[1]
  MasterPlan <- key_parts[2]
  
  # === Step 3: Load Herd Data ===
  CustData <<- tryCatch(GetHerdDataGMSCDCB(CustomerNumber, MasterPlan),
                        error = function(e) { cat("Data load error:", e$message, "\n"); return(NULL) })
  
  if (is.null(CustData) || nrow(CustData) == 0) {
    cat("No herd data for", CustDataKey, "\n")
    return(NULL)
  }
  
  # === Step 4: Genomic Indicator ===
  GTData <<- GetGTData(CustData)
  if (nrow(GTData) > 0) CustData <- getgenomic(CustData)
  CustData$GenomicIndicator <- ifelse(CustData$IdNo %in% GTData$AnimalIDNo, "G", "")
  
  # === Step 5: Get Sire Data ===
  BullDat <<- if (custcreds$Breed %in% c("JE", "JER")) GetSireDataJersey() else GetSireData()
  
  # === Step 6: HBN Conversion ===
  Sexed <- c(529,629,594,694,507,509,501,511,514,521,522,597,586,596,574)
  Conv <- c(29,29,94,94,7,9,1,11,14,21,22,97,80,796,200)
  SexConv <- data.frame(Sexed, Conv)
  
  SexConvFinal <- do.call(rbind, lapply(c("HO", "JE"), function(b) {
    df <- SexConv
    df$Sexed <- paste0(df$Sexed, b)
    df$Conv <- paste0(df$Conv, b)
    df
  }))
  
  convertHBN <- function(hbn_col) {
    hbn2 <- paste0(
      SexConvFinal$Conv[match(str_extract(hbn_col, ".*?(HO|JE)"), SexConvFinal$Sexed)],
      gsub(".*(HO|JE)", "", hbn_col)
    )
    hbn_col[!grepl("^NA", hbn2)] <- hbn2[!grepl("^NA", hbn2)]
    return(hbn_col)
  }
  
  CustData$SireHBN <- convertHBN(CustData$SireHBN)
  CustData$MgsHBN <- convertHBN(CustData$MgsHBN)
  CustData$MggsHBN <- convertHBN(CustData$MggsHBN)
  
  # === Step 7: Merge Sire Traits ===
  merge_sire <- function(type) {
    merge(CustData, BullDat[, c(1, 2, 4, 6:ncol(BullDat))] %>%
            setNames(paste0(type, names(.))), 
          by.x = paste0(type, "HBN"), 
          by.y = paste0(type, "HBN"), all.x = TRUE)
  }
  
  CustData <- merge_sire("Sire")
  CustData <- merge_sire("Mgs")
  CustData <- merge_sire("Mggs")
  
  # === Step 8: Trait Append ===
  Traits <- names(BullDat)[8:ncol(BullDat)]
  for (trait in Traits) {
    CustData <- CDCBAppendTrait(trait, CustData, BullDat)
    missing_rows <- is.na(CustData[[trait]])
    if (any(missing_rows)) {
      CustData[missing_rows, ] <- AppendTrait2gen(trait, CustData[missing_rows, ], BullDat)
    }
  }
  
  # === Step 9: Finalize Columns and Types ===
  CustData$ShortName <- ShortName
  CustData$GenusDTS <- format(Sys.time(), "%d-%m-%Y-%H-%M", tz = "GMT")
  CustData$DataKey <- CustDataKey
  
  Names <- read.csv(paste0(DatasetLoc, "CDCBColumnNames.csv"))
  CDCBNames <- Names$x
  
  Missing <- CDCBNames[!CDCBNames %in% names(CustData)]
  for (col in Missing) CustData[[col]] <- NA
  CustData <- CustData[, CDCBNames, drop = FALSE]
  
  CDCBColType <- read.csv(paste0(DatasetLoc, "CDCBColumnType.csv"))
  for (i in seq_len(nrow(CDCBColType))) {
    col <- CDCBColType$Column[i]
    class(CustData[[col]]) <- CDCBColType$Class[i]
  }
  
  CustData <- CustData[CustData$DatDOB <= Sys.Date(), ]
  
  # === Step 10: Write Outputs ===
  outpath <- paste0(WorkDirectory, "CustData.csv")
  write.csv(CustData, outpath, row.names = FALSE)
  
  dfCC <- read.csv(outpath)
  
  con2 <- DBI::dbConnect(odbc::odbc(),
                         Driver = db_yaml$DataProgs$driver,
                         Server = db_yaml$DataProgs$server,
                         Database = db_yaml$DataProgs$database,
                         UID = db_yaml$DataProgs$uid,
                         PWD = db_yaml$DataProgs$pwd,
                         Port = 1433)
  
  dbExecute(con2, paste0("DELETE FROM CDCBAuditData WHERE DataKey = '", CustDataKey, "'"))
  dbWriteTable(con2, "CDCBAuditData", dfCC, overwrite = FALSE, append = TRUE)
  dbDisconnect(con2)
  
  # === Step 11: Plot and ZIP ===
  source("/data/dairy/USERS/ashenoy/CDCB/myPlotCreator.R")
  AuditTemp <- CDCBplots(CustData, CustDataKey)
  PlotLocation <- AuditTemp[[1]]
  
  zipfile <- file.path(PlotLocation, paste0(CustDataKey, ".zip"))
  zip(zipfile = zipfile, files = list.files(PlotLocation, full.names = TRUE), flags = "-r9Xj")
  cat("Audit complete for", CustDataKey, "\n")
  
  return(zipfile)
}

runcdcbreview("97003538:89")
