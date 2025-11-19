library(zoo)
library(ggplot2)
library(ggcorrplot)

createFolder <- function(mainDir, subDir){
  if (! file.exists(paste(mainDir, subDir, sep = ""))){
    dir.create(file.path(mainDir, subDir))
  }
}

ProducePNG <- function(strName){
  png(strName, res = 1000, width = 22.15 * 1.25, height = 10.56 * 1.5, units = "cm" )
}

MyBlue <- rgb(125, 155, 193, maxColorValue = 255)
MyDarkBlue <- rgb(0, 57, 118, maxColorValue = 255)
MyRed <- rgb(206, 17, 65, maxColorValue = 255)
MyDarkRed <- "DarkRed"
MyGray <- rgb(169, 168, 169, maxColorValue = 255)
MyBrown<-rgb(95,69,43,maxColorValue = 255)
MyBeige<-rgb(190,149,91,maxColorValue = 255)
MyCream<-rgb(224,202,163,maxColorValue = 255)

CIplots <- function(CustData, CustDataKey, CustRecord, Source) {
  
  # Setup folders
  FolderName <- CustRecord$Custfolder
  strDirDestination <- "/data/dairy/USERS/ashenoy/CI-Audits/"
  AuditDTS <- format(Sys.time(), "%d-%m-%Y-%H-%M", tz = "GMT")
  
  if (!file.exists(paste0(strDirDestination, CustDataKey))) {
    createFolder(strDirDestination, CustDataKey)
    Sys.chmod(paste0(strDirDestination, CustDataKey), mode = '0777', use_umask = FALSE)
  }
  
  StrCustDestination <- paste0(strDirDestination, CustDataKey, "/")
  AuditFolder <- paste(CustDataKey, AuditDTS)
  createFolder(StrCustDestination, AuditFolder)
  StrAuditDestination <<- paste0(StrCustDestination, AuditFolder, "/")
  
  # Save PA data
  CustData$TreatAs <- NA
  write.csv(CustData[,c(
    "Mggs.Breed", "Mggs.CountryCode", "Mggs.BullNum", 
    "Mgs.Breed", "Mgs.CountryCode", "Mgs.BullNum", 
    "Sire.Breed", "Sire.CountryCode", "Sire.BullNum", 
    "DatDOB", "NotApplicable", "NotApplicable", "CurrentLact", 
    "AnimalBreed", "UkEartag", "Freezebrand", "IdNo", 
    "ParentAverage", "TreatAs", "HerdId")],
    paste0(StrAuditDestination, "PAData_", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".csv"),
    na = "", row.names = FALSE)
  
  # Determine GT
  GT <- !all(is.na(CustData$CI))
  
  # Save PA and GT data
  if (GT) {
    file_name <- paste0(StrAuditDestination, "PAandGTData_", format(Sys.time(), "%Y_%m_%d_%H_%M"), ".csv")
    
    if (Source == "ANAFI") {
      write.table(CustData[,c(
        "Mggs.Breed", "Mggs.CountryCode", "Mggs.BullNum", 
        "Mgs.Breed", "Mgs.CountryCode", "Mgs.BullNum", 
        "Sire.Breed", "Sire.CountryCode", "Sire.BullNum", 
        "DateOfBirth", "NotApplicable", "NotApplicable", "CurrentLact", 
        "AnimalBreed", "ETAG", "LineNumber", "Eartag", 
        "Index", "TreatAs")],
        file_name, sep = ";", dec = ",", na = "", row.names = FALSE)
    } else {
      write.table(CustData[,c(
        "Mggs.Breed", "Mggs.CountryCode", "Mggs.BullNum", 
        "Mgs.Breed", "Mgs.CountryCode", "Mgs.BullNum", 
        "Sire.Breed", "Sire.CountryCode", "Sire.BullNum", 
        "DatDOB", "NotApplicable", "NotApplicable", "CurrentLact", 
        "AnimalBreed", "Freezebrand", "NotApplicable", "UkEartag", 
        "Index", "TreatAs", "HerdId", "GenomicIndicator")],
        file_name, sep = ",", na = "", row.names = FALSE,
        col.names = c("Breed of MGGS", "Country MGGS", "MGGS", "Breed of MGS", "Country MGS", "MGS",
                      "Breed of Sire", "Country Sire", "Sire", "Birthdate", "ProdRank", "MoToBrd",
                      "Lact", "AnimalBreed", "Animal ID/No", "Alt ID", "Alt ID 2 or Permanent ID",
                      "Index", "TreatAs", "HerdId", "GenomicIndicator"))
    }
  }
  
  # Plot and analysis functions
  PAPlots(CustData, CustDataKey, GT, CustRecord)
  KAPPDataQualityReport(CustData, CustRecord)
  MatrixPlots(CustData, StrAuditDestination)
  AuditBARBOXPlots(CustData, StrAuditDestination, FolderName)
  AuditCorrPlots(CustData, StrAuditDestination, CustRecord, Source)
  YearQuarter(CustData, CustDataKey, StrAuditDestination)
  YearMonth(CustData, CustDataKey, StrAuditDestination)
  # Combine PAGTIndex + PA summary
  create_and_save_summary_tables(CustData, StrAuditDestination)
  #Append tables to graphs
  combine_plot_and_table(
    plot_path = list.files(StrAuditDestination, pattern = "PAGTIndex.*\\.png$", full.names = TRUE),
    table_path = file.path(StrAuditDestination, "Overall_Summary_and_Change.png"),
    output_path = file.path(StrAuditDestination, "Combined_PAGTIndex_Summary.png")
  )
  # Combine PAIndex + PA summary
  combine_plot_and_table(
    plot_path = list.files(StrAuditDestination, pattern = "PAIndex.*\\.png$", full.names = TRUE),
    table_path = file.path(StrAuditDestination, "PA_Summary_and_Change.png"),
    output_path = file.path(StrAuditDestination, "Combined_PAIndex_Summary.png")
  )

  # Country-specific logic
  if (NA %in% "Italy") {
    handleItalianSires(CustDataKey, CustRecord)
  }
  
  return(StrAuditDestination)
}







