###############   CDCB audit plots functions and creation
####
source("/data/dairy/USERS/ashenoy/CDCB/PlotCreatorFunctionsMine.R")

# CustData <- AuditData
# CustNumber <- CustDataKey

CDCBplots <- function(CustData, CustDataKey){
  
  DataProgDB <- DBI::dbConnect(odbc::odbc(),
                               Driver   = db_cfg_EMEA$driver,
                               Server   = db_cfg_EMEA$server,
                               Database = db_cfg_EMEA$database,
                               UID      = db_cfg_EMEA$uid,
                               PWD      = db_cfg_EMEA$pwd,
                               Port     = db_cfg_EMEA$port)
  
  
  query1 = paste0("select * from CustList")
  
  custs = data.table(dbGetQuery(DataProgDB,query1))
  dbDisconnect(DataProgDB)
  
  
  CustRecord <- custs[custs$EvalType %in% "CDCB",]
  CustRecord <- CustRecord[CustRecord$DataKey %in% CustDataKey,]
  
  #BEEEF <- as.numeric(custs[custs$DataKey == CustRecord$DataKey, "Beef"])
  BEEEF <- as.numeric(CustRecord$Beef)
  
  ## Fixing Beef percentages for customers as under 1 will be a percentage
  if(BEEEF < 1){
    # Times by 100 to get out of percentage
    BEEEF <- BEEEF * 100
    
  }
  
  
  
  ## Cust name --- clearing any / as will cause issue with naming the file
  # NAME <- custs[custs$Custfolder == CustRecord$Custfolder, "Custfolder"]
  # NAME <- gsub("/", "",NAME)
  
  
  # Getting cust name
  Shortname <- CustRecord$Custfolder
  ## Removing any / or weird charcters from the names
  if(grepl("/", Shortname)){
    Shortname <- gsub("/", "", Shortname)
  }
  
  
  NAME <- Shortname
  # 
  #   ## Dropping leading 0 from NMR numbers
  #   if(DataSource %in% "NMR"){
  #     
  #     if(grepl("^0", CustDataKey)){
  #       
  #       cat("Dropping leading zero \n")
  #       CustDataKey <- gsub("^0", "", CustDataKey)
  #       
  #     }
  #   }
  
  # # First needed line??
  Sourcedat <- CustRecord$DataSource
  if (Sourcedat %in% "GMS:MasterPlan"){Sourcedat = "GMS"}
  
  
  ######################## Plot function calls
  strDirDestination <-  "/data/dairy/USERS/ashenoy/CDCB/"
  # Shortname <- "test"
  # 
  # # Getting cust name
  # Shortname <- CustRecord$Custfolder
  # ## Removing any / or weird charcters from the names
  # if(grepl("/", Shortname)){
  #   Shortname <- gsub("/", "", Shortname)
  # }
  # 
  
  # Setting time for use for dating output folders
  AuditDTS <- format(Sys.time(), "%d-%m-%Y-%H-%M", tz = "GMT")
  # Create a folder if one does not exist
  if(! file.exists(paste0(strDirDestination, CustDataKey))){
    createFolder(strDirDestination, CustDataKey)
    Sys.chmod(paste0(strDirDestination, CustDataKey), mode = '0777', use_umask = FALSE)
  }
  # Create path variable to use to output analysis and creates folder
  StrCustDestination <- paste0(strDirDestination, CustDataKey, "/")
  AuditFolder <- paste(CustDataKey, AuditDTS)
  
  # Creating the audit folder
  createFolder(StrCustDestination, AuditFolder)
  StrAuditDestination <<- paste0(StrCustDestination, AuditFolder, "/")
  #Sourcedat <<- custs$MRO[custs$Custfolder == Shortname]
  cat(paste("/nData source is:", Sourcedat))
  
  
  ###### PLOTSSSS
  
  
  
  CustData$NotApplicable<-NA
  CustData$DatCalving<-NA
  if (CustRecord$Breed %in% "JE"){
    PAoutcols<-c("MggsBreedCode","MggsCountryCode","MggsHBN","MgsBreedCode","MgsCountryCode","MgsHBN","SireBreedCode","SireCountryCode","SireHBN","DatDOB","NotApplicable","NotApplicable","CurrentLact","AnimalBreed","NationalCowID","Freezebrand","IdNo","GenomicIndicator","NM","JPI","FM","CM","TreatAs","SireRegName","DatCalving","HerdId")
  }else{
    PAoutcols<-c("MggsBreedCode","MggsCountryCode","MggsHBN","MgsBreedCode","MgsCountryCode","MgsHBN","SireBreedCode","SireCountryCode","SireHBN","DatDOB","NotApplicable","NotApplicable","CurrentLact","AnimalBreed","NationalCowID","Freezebrand","IdNo","GenomicIndicator","NM","TPI","FM","CM","TreatAs","SireRegName","DatCalving","HerdId")
  }
  write.csv(CustData[!is.na(CustData$HerdId),PAoutcols],paste(StrAuditDestination,"PAData.csv",sep=""),row.names = F, na="")
  
  
  SireMatrix<-data.frame(unique(CustData$SireHBN))
  names(SireMatrix)<-"Sires"
  for (i in unique(CustData$FactLactClass[!is.na(CustData$FactLactClass)])){
    print(i)
    
    Sires<-data.frame(table(CustData$SireHBN[CustData$FactLactClass%in%i]))
    #If no sires for particular lactation group
    if (nrow(Sires)<1){
      next()
    }
    names(Sires)<-c("Sires",i)
    
    SireMatrix<-merge(SireMatrix,Sires,by="Sires",all.x=T)
    
  }
  
  write.csv(SireMatrix, paste(StrAuditDestination,"SireMatrix.csv",sep=""),row.names = F, na="")
  
  #CustData<<-CustData
  
  #OutputDir<-"C:/Users/ccarroll/OneDrive - Genus PLC/Documents/Genetic Reviews/South Africa/Janvos"
  
  #Indexes(CustData,FolderName,AuditFolder,intBeefPercentage=40)
  Indexes(CustData, NAME, StrAuditDestination,intBeefPercentage = BEEEF, CustRecord$Breed)
  GenerateTraitSummaryAndPlots(CustData, BullDat, StrAuditDestination)
  FertilityBar(CustData, NAME, StrAuditDestination)
  LongetivityBar(CustData, NAME, StrAuditDestination)
  ProductionBar(CustData, NAME, StrAuditDestination)
  FeedSavedBar(CustData, NAME, StrAuditDestination)
  IndexBar(CustData, NAME, StrAuditDestination, CustRecord$Breed)
  BodyCompBar(CustData, NAME, StrAuditDestination)
  FLCBar(CustData, NAME, StrAuditDestination)
  UdderCompBar(CustData, NAME, StrAuditDestination, CustRecord$Breed)
  UdderHealthBar(CustData, NAME, StrAuditDestination)
  transHealthBar(CustData, NAME, StrAuditDestination)
  Histograms(CustData, NAME, StrAuditDestination, CustRecord$Breed)
  
  #ROIPngs(CustData,FolderName,AuditFolder,name=ShortName,prop=0.6)
  ROIPngs(CustData,NAME, StrAuditDestination, name = NAME, prop = (BEEEF/100), CustRecord$Breed)
  producePhenotypes(CustData, NAME, StrAuditDestination, CustRecord$Breed)
  produceYRquarter(CustData, NAME, StrAuditDestination, CustRecord$Breed)
  produceYRMon(CustData,NAME, StrAuditDestination, CustRecord$Breed)
  
  
  
  
  
  
  
  
  
  
  return(list(StrAuditDestination, NAME))
  
  
}

