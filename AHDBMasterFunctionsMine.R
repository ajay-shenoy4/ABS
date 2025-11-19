## Master functions to get data for NE audit

###
#Load Libraries
###
require("stringr")
library("odbc")
library("DBI")
library("rstudioapi")
library("RMySQL")
library("lubridate")
library(dplyr)
library(zoo)
library(plyr)
library(ggplot2)
library(vroom)
library(readxl)
library(readr)
library(stringdist)

## Setting paths to locations
BaseFolderloc <<- "/data/dairy/EMEAProjects/Audit-Data-Loader/"
ProofFolderLoc <- paste0(BaseFolderloc, "Proof - Files/")
NEResources <- paste0(BaseFolderloc, "NatEvalResources/")
## AHDB Script Folder
AHDBloc <<- "/data/dairy/EMEAProjects/Audit-Data-Loader/Scripts - AHDB/"
Allloc  <<- "/data/dairy/EMEAProjects/Audit-Data-Loader/Scripts - All/"

#strDirDestination <<- BaseFolderloc

## Sources other scripts to get data
source(paste0(AHDBloc, "MRODataExtractFunctions.R"))
source(paste0(Allloc, "GMSDataExtractFunctions.R"))
source(paste0(AHDBloc,"ExternalOrgFiles.R"))
source(paste0(Allloc, "UKIndustyEvaluationDataFunctions.R"))
source(paste0(AHDBloc, "AHDB-GT-Data-Loader.R"))


##source(paste0(AHDBloc,"ExternalOrgFiles.R")) GT DATA MASTER SCRIPT PULL
##
### Breed codes --- NEEDED FOR ALOT !!!!
breeds <- read_excel(paste0(NEResources, "Breeds V1.xlsx"))

#This is a resource and storage folder within the destination
strFolderResource <- "NatEvalResources"
strDirResources <- paste0(BaseFolderloc, strFolderResource, "/")
strAHDBColumnSpecFile <- "AHDBHGRColumnSpec.csv"
strDataStrucFile <- "DataStructure.csv"



# File not present - needed??
#StrCountrycodes <-paste0(strDirResources, "3166CountryCodes.csv")
# Still need to set these   --- gets are got the nat resoucres using get prrof date --- 
# strMilkingPercentiles <- "MilkingPercentiles"
# strYoungstockPercentiles <- "YoungstockPercentiles"
# strNatGenom <- "GetNationalEvaluationGenomicsv2.R"#
# Current proof folder of files
#CurProofPath <- paste0(ProofFolderLoc, ProofRun,"/")
#strDirDestination <<- "/data/dairy/EMEAProjects/Audit-Data-Loader-NE/"


##Plot asthetics
GText <- "Genetic Trends Amongst Sire Usage and Dairy Animals for"
CText <- "Correlation of ###Perf### with ###Gen###"
MyBlue <- rgb(125, 155, 193, maxColorValue = 255)
MyDarkBlue <- rgb(0, 57, 118, maxColorValue = 255)
MyRed <- rgb(206, 17, 65, maxColorValue = 255)
MyDarkRed <- "DarkRed"
MyGray <- rgb(169, 168, 169, maxColorValue = 255)
MyBrown <- rgb(95,69,43,maxColorValue = 255)
MyBeige <- rgb(190,149,91,maxColorValue = 255)
MyCream <- rgb(224,202,163,maxColorValue = 255)
dblHolstienGest <- 275.5

# SC server path string  and columns to exclude from data
MROpathSC <- "/data/dairy/EMEAProjects/MRO/"
exclude <- c("FileDate", "File")
YEAR <- format(Sys.time(), "%Y")

### Function too add any missing columns from a list
fncols <- function(data, cname) {  
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- NA  
  data
}

##
#Function to get Data Structure
##
getDataStruc <- function(){
  # return(read.csv(paste0(strDirResources, strDataStrucFile), stringsAsFactors = F, fileEncoding = "windows-1252"))
  return(read.csv(paste0(strDirResources, strDataStrucFile), stringsAsFactors = F, fileEncoding = "UTF-8"))
  
  #return(read.csv(paste(strDirResources,"DataStructureNew.csv",sep="")))
}

#print(strDirResources)
#print(strDataStrucFile)
#datastructure <- read.csv("/data/dairy/EMEAProjects/Audit-Data-Loader/NatEvalResources/DataStructure.csv")
#datastructure

### Will need new function to use new folder location to search using MRO number ****************
######################################### might be able to drop as will never has single files -- will ahve seperate files from master (miling + youngstock)
##
#Function to get scan for HGR files by term and merge if mutiple files exist for the same max date
##
# bindHGR <- function(strDirCust, Term){
#   #List All AHDB HGR Files with term
#   strFiles<-c(list.files(paste(strDirSource,"\\",strDirCust,sep=""))[grepl(Term,list.files(paste(strDirSource,"\\",strDirCust,sep="")))])
#   #Convert to a data frame with the file date string
#   dfFiles<-cbind.data.frame(strFiles, strDate=str_extract(strFiles, "August.{0,5}|December.{0,5}|April.{0,5}"), stringsAsFactors = F)
#   #Convert String to a date
#   dfFiles$Date<-as.Date(paste("1",dfFiles$strDate),"%d %B %Y")
#   #Final milking file for import
#   strFiles<-dfFiles$strFiles[dfFiles$Date==max(dfFiles$Date)]
#   ###If more than one milk / youngstock file on an account for max date merge the x number of files 
#   if(length(strFiles)==1){
#     #dfOut<-read.csv(paste(strDirSource,strDirCust,"\\",strFiles,sep=""),stringsAsFactors = F, fileEncoding = "windows-1252")
#     dfOut<-read.csv(paste(strDirSource,strDirCust,"\\",strFiles,sep=""),stringsAsFactors = F)
#     
#   }else{
#     cat(paste("WARNING: Multiple HGR Files Found\n"))
#     for (i in 1:length(strFiles)){
#       cat(paste("   Running: '",strFiles[i],"'\n",sep=""))
#       if (i==1){
#         dfOut<-read.csv(paste(strDirSource,strDirCust,"\\",strFiles[i],sep=""),stringsAsFactors = F)
#       }else{   
#         dfTemp <- read.csv(paste(strDirSource,strDirCust,"\\",strFiles[i],sep=""),stringsAsFactors = F)
#         lsTemp <-StdCols(dfOut,dfTemp)
#         dfOut<-lsTemp [[1]]
#         dfTemp <-lsTemp [[2]]
#         dfOut<-rbind.data.frame(dfOut,dfTemp[!dfTemp$Identity%in%dfOut$Identity,],stringsAsFactors=F)
#       }
#     }
#   }
#   dfOut <- Standardise(dfOut, getAHDBSpec())
#   #Add "" DOB if missing
#   if(! "DOB" %in% colnames(dfOut)){
#     dfOut$DOB <- ""
#   }
#   #Add 0 CurrLact if missing
#   if(! "CurrLact" %in% colnames(dfOut)){
#     dfOut$CurrLact <- 0
#   }
#   if(! "ParentAverageIndicator" %in% colnames(dfOut)){
#     dfOut$ParentAverageIndicator <- "PI"
#   }
#   return(dfOut)
# }
# 
# ##
# #Function to get the AHDBData and bind it
# ##
# # getHGR<-function(strDirCust){
# # if(file.exists(paste(strDirSource,strDirCust,sep=""))){
# # if(length(list.files(paste(strDirSource,"\\",strDirCust,sep=""))[grepl("Milking",list.files(paste(strDirSource,"\\",strDirCust,sep="")))])==0){
# # return(NULL)
# # }
# # }else{
# # return(NULL)
# # }
# # Milking<-bindHGR(strDirCust,"Milking")
# # Youngstock<-bindHGR(strDirCust,"Youngstock")
# # All<-rbind.data.frame(Milking,Youngstock[!Youngstock$Eartag%in%Milking$Eartag,],stringsAsFactors=F)
# # All$DOB<-as.Date(All$DOB,"%d/%m/%Y")
# # return(All)
# # }
# 
# ## Updated getHGR function as AHDB changed the file names to all data to be in one file
# 
# getHGR <- function(strDirCust){
#   if(file.exists(paste(strDirSource,strDirCust,sep=""))){
#     if(length(list.files(paste(strDirSource,"\\",strDirCust,sep=""))[grepl("FullHerd",list.files(paste(strDirSource,"\\",strDirCust,sep="")))])>= 1){
#       cat("We have a Full Herd file from AHDB \n")
#       FULLHERDAHDB <- bindHGR(strDirCust,"FullHerd")
#       FULLHERDAHDB$DOB <- as.Date(FULLHERDAHDB$DOB,"%d/%m/%Y")
#       return(FULLHERDAHDB)
#     }else if (length(list.files(paste(strDirSource,"\\",strDirCust,sep=""))[grepl("Milking",list.files(paste(strDirSource,"\\",strDirCust,sep="")))])>=0){
#       Milking<-bindHGR(strDirCust,"Milking")
#       Youngstock<-bindHGR(strDirCust,"Youngstock")
#       All<-rbind.data.frame(Milking,Youngstock[!Youngstock$Eartag%in%Milking$Eartag,],stringsAsFactors=F)
#       All$DOB<-as.Date(All$DOB,"%d/%m/%Y")
#       return(All)
#     } else{
#       cat("No AHDB data within the customer's folder - please place it within their folder if they have AHDB data \n")
#       return(NULL)
#     }
#   }
# }




## Gets base dataset from each different data sources
getNEHerdsData <- function(CustDataKey,Sourcedat){
  
  if(grepl("/", CustDataKey)){
    
    cat("Dropping leading zero \n")
    CustDataKeyBU <- CustDataKey
    CustDataKey <- gsub("/", "", CustDataKey)
    
  }
  
  
  # If the data source is GMS will do the following
  if(Sourcedat %in% "GMS"){
    Data <- GetHerdDataGMSAHDB(CustDataKey)
    Data$SSireHBN <- NA
    Data$Yield305DayLact <- NA
    Data$Yield305DayLact[!is.na(Data$DaysInMilk)]<-ifelse(Data$DaysInMilk[!is.na(Data$DaysInMilk)]<305,Data$CurrentLact[!is.na(Data$DaysInMilk)]-1,Data$CurrentLact[!is.na(Data$DaysInMilk)])
    Data$Yield305DayLact[Data$CurrentLact%in%0]<-0
    #Data$KG305ME[Data$KG305ME%in% 0]<- NA
    Data$DOPN[Data$DOPN%in%0]<-NA
    Data$KG305ME[Data$KG305ME%in%0]<-NA
    Data$KGFat305ME[Data$KGFat305ME==0]<-NA
    Data$KGProt305ME[Data$KGProt305ME==0]<-NA
    Data$TestProtPc[Data$TestProtPc==0]<-NA
    Data$TestFatPc[Data$TestFatPc==0]<-NA
    Data <- Data[format(as.Date(Data$DatDOB),"%Y") >= (as.numeric(format(Sys.Date(), "%Y"))-11),]
    Data$ProdRel <- 0.33
    # Data$SireHBN<-formatC(as.numeric(Data$SireHBN),flag="0",format="fg",width=12)
    #Data$MgsHBN<-formatC(as.numeric(Data$MgsHBN),flag="0",format="fg",width=12)
    #Data$MggsHBN<-formatC(as.numeric(Data$MggsHBN),flag="0",format="fg",width=12)
  } else {
    
    # All other data sources (NMR/CIS/UDF has numerical customer herd codes)
    
    # If the Shortname for the customer has 2 MRO numbers -- gets both ****** TEST BOTH WORKS ON CLUSTER ***
    if(str_count(CustDataKey, ",") > 0){
      MRONo <- unlist(strsplit(CustDataKey, ", "))
    }else{
      MRONo <- CustDataKey
    }
    
    MRO <- Sourcedat
    
    cat("MRO Number(s):", paste(MRONo, collapse = ", "), "\n")
    
    # If MRO number has leading 0 missing it adds it back it
    if (nchar(MRONo) <5 ){
      MRONo <- str_pad(MRONo, width = 5, side = "left", pad = "0")
    }
    
    # Pulling MRO Data from Data Extraction functions
    #Data <- GetMroData(MRONo,MRO)
    Data <- MROCustData(MRONo,MRO)
    
  }
  return(Data)
}
#MRONo

# dfData<-dfHerd
# dfMergeData<-dfSires
# strFieldLinkx<-"MggsHBN"
# StrFieldLinky<-"HBN"
# Prefix<-"Mggs"
# Trait<-"FatKg"
# strBreedFieldx<-"SireBreedCode"
# strBreedFieldy<-"BreedCode"

MergeOn <- function(dfData, dfMergeData, strFieldLinkx, StrFieldLinky, Prefix, Trait, strBreedFieldx, strBreedFieldy) {
  # Clean link IDs
  dfMergeData$Link <- gsub("^0{1,12}", "", dfMergeData[, StrFieldLinky])
  
  # Handle duplicates using breed codes
  DuplicatedIDs <- names(table(dfMergeData$Link))[table(dfMergeData$Link) > 1]
  dfData$Link <- dfData[, strFieldLinkx]
  
  dfData$Link[dfData$Link %in% DuplicatedIDs] <- paste(
    formatC(as.numeric(dfData[dfData$Link %in% DuplicatedIDs, strBreedFieldx]), width = 2, flag = "0", format = "fg"),
    dfData$Link[dfData$Link %in% DuplicatedIDs],
    sep = ":"
  )
  
  dfMergeData$Link[dfMergeData$Link %in% DuplicatedIDs] <- paste(
    formatC(as.numeric(dfMergeData[dfMergeData$Link %in% DuplicatedIDs, strBreedFieldy]), width = 2, flag = "0", format = "fg"),
    dfMergeData$Link[dfMergeData$Link %in% DuplicatedIDs],
    sep = ":"
  )
  
  # Filter merge data to matching links
  dfMergeData <- dfMergeData[dfMergeData$Link %in% dfData$Link, ]
  
  # Rename columns with prefix (e.g., SireLink, SireTrait)
  colnames(dfMergeData) <- paste(Prefix, colnames(dfMergeData), sep = "")
  
  # --- Safe column check before subsetting ---
  expected_cols <- paste(Prefix, c("Link", Trait), sep = "")
  existing_cols <- expected_cols[expected_cols %in% colnames(dfMergeData)]
  
  if (length(existing_cols) < 2) {
    warning(paste("Skipping trait", Trait, "because required columns are missing."))
    return(dfData)  # Return original unchanged
  }
  
  dfMergeData <- dfMergeData[, existing_cols, drop = FALSE]
  
  # Merge on Link field
  dfData <- merge(
    dfData,
    dfMergeData,
    by.x = "Link",
    by.y = paste(Prefix, "Link", sep = ""),
    all.x = TRUE
  )
  
  return(dfData)
}

# 
# 
# MergeOnGG <- function(dfData,dfMergeData,strFieldLinkx,StrFieldLinky,Prefix,Trait,strBreedFieldx,strBreedFieldy){
#   dfMergeData$Link<-gsub("^0{1,12}","",dfMergeData[,StrFieldLinky])
#   
#   #dfMergeDataG <- dfMergeData
#   
#   DuplicatedIDs<-names(tapply(dfMergeData$Link,dfMergeData$Link,length)[tapply(dfMergeData$Link,dfMergeData$Link,length)>1])
#   dfData$Link<-dfData[,strFieldLinkx]
#   
#   ### no need to do the dup change if no dupes present
#   if(any(dfData$Link %in% DuplicatedIDs)){
#     
#     
#     dfData$Link[dfData$Link%in%DuplicatedIDs]<-paste(formatC(as.numeric(dfData[dfData$Link%in%DuplicatedIDs,strBreedFieldx]),width=2,flag="0",format="fg"),dfData$Link[dfData$Link%in%DuplicatedIDs],sep=":")
#     dfMergeData$Link[dfMergeData$Link%in%DuplicatedIDs]<-paste(formatC(as.numeric(dfMergeData[dfMergeData$Link%in%DuplicatedIDs,strBreedFieldy]),width=2,flag="0",format="fg"),dfMergeData$Link[dfMergeData$Link%in%DuplicatedIDs],sep=":")
#     
#   } else {
#     
#     
#     cat("\nNo dupes in the dfherd skipping\n")
#     # 
#     # dfData$Link[dfData$Link%in%DuplicatedIDs]<-paste(formatC(as.numeric(dfData[dfData$Link%in%DuplicatedIDs,strBreedFieldx]),width=2,flag="0",format="fg"),dfData$Link[dfData$Link%in%DuplicatedIDs],sep=":")
#     # dfMergeData$Link[dfMergeData$Link%in%DuplicatedIDs]<-paste(formatC(as.numeric(dfMergeData[dfMergeData$Link%in%DuplicatedIDs,strBreedFieldy]),width=2,flag="0",format="fg"),dfMergeData$Link[dfMergeData$Link%in%DuplicatedIDs],sep=":")
# 
#     
#   }
#   
#   # dfData$Link[dfData$Link%in%DuplicatedIDs]<-paste(formatC(as.numeric(dfData[dfData$Link%in%DuplicatedIDs,strBreedFieldx]),width=2,flag="0",format="fg"),dfData$Link[dfData$Link%in%DuplicatedIDs],sep=":")
#   # dfMergeData$Link[dfMergeData$Link%in%DuplicatedIDs]<-paste(formatC(as.numeric(dfMergeData[dfMergeData$Link%in%DuplicatedIDs,strBreedFieldy]),width=2,flag="0",format="fg"),dfMergeData$Link[dfMergeData$Link%in%DuplicatedIDs],sep=":")
#   # 
#   dfMergeData<-dfMergeData[dfMergeData$Link%in%dfData$Link,]
#   
#   colnames(dfMergeData) <- paste(Prefix, colnames(dfMergeData), sep = "")
#   dfMergeData<-dfMergeData[,c(paste(Prefix,c("Link",Trait),sep=""))]
#   
#   dfData<-merge(dfData,dfMergeData,by.x="Link",by.y=paste(Prefix,"Link",sep=""),all.x=T)
#   
#   
#   #dfData<-merge(dfData,setNames(dfMergeData,paste(Prefix,colnames(dfMergeData),sep=""))[,c(paste(Prefix,c("Link",Trait),sep=""))],by.x="Link",by.y=paste(Prefix,"Link",sep=""),all.x=T)
#   
#   return(dfData)
# }

# dfMergeData<-getDS()
# 
# 
# dfData<-dfx
# dfMergeData<-dfSires
# 
# strFieldLinkx<-"SireHBN"
# StrFieldLinky<-"HBN"
# Prefix<-"Sire"
# #Trait<-"PLI"
# Trait<-"MilkKg"
# 
# strBreedFieldx<-"SireBreedCode"
# strBreedFieldy<-"BreedCode"
# # 
# dfx <- dfHerd
# dfSires
# Trait <-  dfDataStruc$Trait[2]
# Source <- Sourcedat

## Issue with new connection missing breedcode


#GGds <- getDS()

#AppendTraitDB <- function(dfx,GGds,Trait,Source){

AppendTraitDB <- function(dfx, dfSires, Trait, Source) {
  # Merge data from ancestors
  dfx <- MergeOn(dfx, dfSires, "SireHBN", "HBN", "Sire", Trait, "SireBreedCode", "BreedCode")
  dfx <- MergeOn(dfx, dfSires, "MgsHBN",  "HBN", "Mgs",  Trait, "MgsBreedCode", "BreedCode")
  dfx <- MergeOn(dfx, dfSires, "MggsHBN", "HBN", "Mggs", Trait, "MggsBreedCode", "BreedCode")
  
  if (Source != "GMS") {
    dfx <- MergeOn(dfx, dfSires, "SSireHBN", "HBN", "Preg", Trait, "SSireBreedCode", "BreedCode")
  }
  
  if (Trait == "RearLegRearView") {
    assign("dfx_debug", dfx, envir = .GlobalEnv)
  }
  
  # Create lactation classification variables
  dfx$FactLacTemp <- as.numeric(dfx$CurrentLact)
  dfx$FactLacTemp[dfx$FactLacTemp > 4] <- 5
  dfx$FactLacTemp[dfx$LactClass == "Calf"] <- -1
  
  dfx$FactLacTempT <- as.numeric(dfx$CurrentLact)
  dfx$FactLacTempT[dfx$FactLacTempT > 3] <- 4
  dfx$FactLacTempT[dfx$LactClass == "Calf"] <- -1
  
  # Build trait columns
  trait_cols <- paste(c("Sire", "Mgs", "Mggs"), Trait, sep = "")
  
  # Check if required trait columns exist
  missing_cols <- trait_cols[!trait_cols %in% colnames(dfx)]
  if (length(missing_cols) > 0) {
    warning(paste("Trait", Trait, "skipped due to missing columns:", paste(missing_cols, collapse = ", ")))
    return(dfx)
  }
  
  # Safely compute lactation means
  MyMeans <- tryCatch({
    aggregate(dfx[, trait_cols], by = list(dfx$FactLacTemp), mean, na.rm = TRUE)
  }, error = function(e) {
    message("❌ Error during aggregation for trait:", Trait)
    message("💥 Problem: ", e$message)
    return(NULL)
  })
  
  if (is.null(MyMeans)) {
    warning(paste("Skipping trait", Trait, "due to aggregation error."))
    return(dfx)
  }
  
  # Rename and recode lactation groups
  colnames(MyMeans)[1] <- "LactationF"
  MyMeans$FactLac <- as.character(MyMeans$LactationF)
  MyMeans$FactLac[MyMeans$FactLac == "4"] <- "4+"
  MyMeans$FactLac[MyMeans$FactLac == "-1"] <- "Calf"
  MyMeans$FactLac[MyMeans$FactLac == "0"] <- "Heifer"
  
  # Fill in missing trait values using MyMeans
  for (Level in c("Sire", "Mgs", "Mggs")) {
    target_col <- paste(Level, Trait, sep = "")
    fill_vals <- MyMeans[match(dfx$LactClass[dfx[[target_col]] %in% NA], MyMeans$FactLac), target_col]
    dfx[is.na(dfx[[target_col]]), target_col] <- fill_vals
  }
  
  # Create composite genetic estimate
  dfx$Missing <- MyMeans[match(dfx$FactLacTempT + 1, MyMeans$LactationF), paste("Mggs", Trait, sep = "")]
  dfx[[paste("TGen", Trait, sep = "")]] <-
    0.5 * dfx[[paste("Sire", Trait, sep = "")]] +
    0.25 * dfx[[paste("Mgs", Trait, sep = "")]] +
    0.125 * dfx[[paste("Mggs", Trait, sep = "")]] +
    0.125 * dfx$Missing
  
  # Replace missing trait values with TGen estimate
  if (Trait %in% colnames(dfx)) {
    dfx[is.na(dfx[[Trait]]), Trait] <- dfx[is.na(dfx[[Trait]]), paste("TGen", Trait, sep = "")]
  } else {
    dfx[[Trait]] <- dfx[[paste("TGen", Trait, sep = "")]]
  }
  
  # If PregPA applies
  if (Source != "GMS") {
    dfx[[paste("PregPA", Trait, sep = "")]] <- (dfx[[Trait]] + dfx[[paste("Preg", Trait, sep = "")]]) / 2
  }
  
  # Columns to remove
  drop_cols <- c("FactLacTemp", "FactLacTempT", "Missing",
                 paste("Mgs", Trait, sep = ""),
                 paste("Mggs", Trait, sep = ""))
  
  if (Trait != "PLI") {
    drop_cols <- c(drop_cols, paste("TGen", Trait, sep = ""))
  } else {
    cat(paste(sum(is.na(dfx$PLI)), "animals are omitted from the audit\n"))
  }
  
  return(dfx[, !colnames(dfx) %in% drop_cols])
}


##
#Function to calculate the cross generation approximation
##
AppendTrait <- function(dfx,dfDS,dfNHM,Trait,Source){
  if(Trait %in% c("Feed.Advantage", "Enviro.Cow","RTP","TLG","MSP")){
    
    dfx<-MergeOn(dfx,dfNHM,"SireHBN","HBN","Sire",Trait,"SireBreedCode","BreedCode")
    dfx<-MergeOn(dfx,dfNHM,"MgsHBN","HBN","Mgs",Trait,"MgsBreedCode","BreedCode")
    dfx<-MergeOn(dfx,dfNHM,"MggsHBN","HBN","Mggs",Trait,"MggsBreedCode","BreedCode")
    
  } else {
    
    dfx<-MergeOn(dfx,dfDS,"SireHBN","HBN","Sire",Trait,"SireBreedCode","BreedCode")
    dfx<-MergeOn(dfx,dfDS,"MgsHBN","HBN","Mgs",Trait,"MgsBreedCode","BreedCode")
    dfx<-MergeOn(dfx,dfDS,"MggsHBN","HBN","Mggs",Trait,"MggsBreedCode","BreedCode")
    
  }
  if(!Source%in%"GMS"){
    dfx<-MergeOn(dfx,dfNHM,"SSireHBN","HBN","Preg",Trait,"SSireBreedCode","BreedCode")
  }
  #dfx<-merge(dfx,setNames(dfDS,paste("Sire",colnames(dfDS),sep=""))[,c("SireLink",paste("Sire",Trait,sep=""))],by.x="SireHBN",by.y="SireLink",all.x=T)
  #dfx<-merge(dfx,setNames(dfDS,paste("Mgs",colnames(dfDS),sep=""))[,c("MgsLink",paste("Mgs",Trait,sep=""))],by.x="MgsHBN",by.y="MgsLink",all.x=T)
  #dfx<-merge(dfx,setNames(dfDS,paste("Mggs",colnames(dfDS),sep=""))[,c("MggsLink",paste("Mggs",Trait,sep=""))],by.x="MggsHBN",by.y="MggsLink",all.x=T)
  #dfx<-merge(dfx,setNames(dfNHM,paste("Preg",colnames(dfNHM),sep=""))[,c("PregLink",paste("Preg",Trait,sep=""))],by.x="SSireHBN",by.y="PregLink",all.x=T)
  dfx$FactLacTemp<-as.numeric(dfx$CurrentLact)
  dfx$FactLacTemp[as.numeric(dfx$CurrentLact)>4]<-5
  dfx$FactLacTemp[dfx$LactClass=="Calf"]<-(-1)
  dfx$FactLacTempT<-as.numeric(dfx$CurrentLact)
  dfx$FactLacTempT[as.numeric(dfx$CurrentLact)>3]<-4
  dfx$FactLacTempT[dfx$LactClass=="Calf"]<-(-1)
  #dfx$FactLacTemp[dfx$FactLacTemp>4]<-5
  MyMeans<-aggregate(list(dfx[,paste(c("Sire","Mgs","Mggs"),Trait,sep="")]),by=list(dfx$FactLacTemp),mean,na.rm=T)
  colnames(MyMeans)<-gsub("Group.1","LactationF",colnames(MyMeans))
  MyMeans$FactLac<-MyMeans$LactationF
  MyMeans$FactLac[MyMeans$FactLac=="4"]<-"4+"
  MyMeans$FactLac[MyMeans$FactLac=="-1"]<-"Calf"
  MyMeans$FactLac[MyMeans$FactLac=="0"]<-"Heifer"
  
  # Fix to correct the error with Some Lactation Classes have no mean within  ***Appended Trait function***
  
  # if(any(is.na(MyMeans[,paste(c("Sire","Mgs","Mggs"),Trait,sep="")]))){
  #   stop("Unhandeled error in Appended Trait function. Some Lactation Classes have no mean!")
  # }
  for(Level in c("Sire","Mgs","Mggs")){
    dfx[is.na(dfx[,paste(Level,Trait,sep="")]),paste(Level,Trait,sep="")]<-MyMeans[match(dfx[is.na(dfx[,paste(Level,Trait,sep="")]),"LactClass"],MyMeans$FactLac),paste(Level,Trait,sep="")]
    #dfx[is.na(dfx[,paste(Level,Trait,sep="")]),"LactClass"]
  }
  dfx$Missing<-MyMeans[match(dfx$FactLacTempT+1,MyMeans$LactationF),paste("Mggs",Trait,sep="")]
  dfx[,paste("TGen",Trait,sep="")]<-
    (dfx[,paste("Sire",Trait,sep="")]*0.5)+
    (dfx[,paste("Mgs",Trait,sep="")]*0.25)+
    (dfx[,paste("Mggs",Trait,sep="")]*0.125)+
    #(dfx[,"Missing"]+0.125) this lin e was a plus but wrong
    (dfx[,"Missing"]*0.125)
  if(Trait%in%colnames(dfx)){
    
    dfx[is.na(dfx[,Trait]),Trait]<-dfx[is.na(dfx[,Trait]),paste("TGen",Trait,sep="")]
    
  }else{
    dfx[,Trait]<-dfx[,paste("TGen",Trait,sep="")]
  }
  if(!Source%in%"GMS"){
    dfx[,paste("PregPA",Trait,sep="")]<-(dfx[,Trait]+dfx[,paste("Preg",Trait,sep="")])/2
  }
  Drop<-c("FactLacTemp","FactLacTempT","Missing",paste("Mgs",Trait,sep=""),paste("Mggs",Trait,sep=""))
  
  if(Trait!="PLI"){
    Drop<-c(Drop,paste("TGen",Trait,sep=""))
  }else{
    cat(paste(sum(is.na(dfx$PLI)),"animals are ommited from the audit"))
  }
  
  return(dfx[,!colnames(dfx)%in%Drop])
}


# 
# AppendTraitCOPY <- function(dfx,dfDS,dfNHM,Trait,Source){
#   if(Trait %in% c("Feed.Advantage", "Enviro.Cow")){
#     
#     dfx<-MergeOn(dfx,dfNHM,"SireHBN","HBN","Sire",Trait,"SireBreedCode","BreedCode")
#     dfx<-MergeOn(dfx,dfNHM,"MgsHBN","HBN","Mgs",Trait,"MgsBreedCode","BreedCode")
#     dfx<-MergeOn(dfx,dfNHM,"MggsHBN","HBN","Mggs",Trait,"MggsBreedCode","BreedCode")
#     
#   } else {
#     
#     dfx<-MergeOn(dfx,dfDS,"SireHBN","HBN","Sire",Trait,"SireBreedCode","BreedCode")
#     dfx<-MergeOn(dfx,dfDS,"MgsHBN","HBN","Mgs",Trait,"MgsBreedCode","BreedCode")
#     dfx<-MergeOn(dfx,dfDS,"MggsHBN","HBN","Mggs",Trait,"MggsBreedCode","BreedCode")
#     
#   }
#   if(!Source%in%"GMS"){
#     dfx<-MergeOn(dfx,dfNHM,"SSireHBN","HBN","Preg",Trait,"SSireBreedCode","BreedCode")
#   }
#   #dfx<-merge(dfx,setNames(dfDS,paste("Sire",colnames(dfDS),sep=""))[,c("SireLink",paste("Sire",Trait,sep=""))],by.x="SireHBN",by.y="SireLink",all.x=T)
#   #dfx<-merge(dfx,setNames(dfDS,paste("Mgs",colnames(dfDS),sep=""))[,c("MgsLink",paste("Mgs",Trait,sep=""))],by.x="MgsHBN",by.y="MgsLink",all.x=T)
#   #dfx<-merge(dfx,setNames(dfDS,paste("Mggs",colnames(dfDS),sep=""))[,c("MggsLink",paste("Mggs",Trait,sep=""))],by.x="MggsHBN",by.y="MggsLink",all.x=T)
#   #dfx<-merge(dfx,setNames(dfNHM,paste("Preg",colnames(dfNHM),sep=""))[,c("PregLink",paste("Preg",Trait,sep=""))],by.x="SSireHBN",by.y="PregLink",all.x=T)
#   dfx$FactLacTemp<-as.numeric(dfx$CurrentLact)
#   dfx$FactLacTemp[as.numeric(dfx$CurrentLact)>4]<-5
#   dfx$FactLacTemp[dfx$LactClass=="Calf"]<-(-1)
#   dfx$FactLacTempT<-as.numeric(dfx$CurrentLact)
#   dfx$FactLacTempT[as.numeric(dfx$CurrentLact)>3]<-4
#   dfx$FactLacTempT[dfx$LactClass=="Calf"]<-(-1)
#   #dfx$FactLacTemp[dfx$FactLacTemp>4]<-5
#   MyMeans<-aggregate(list(dfx[,paste(c("Sire","Mgs","Mggs"),Trait,sep="")]),by=list(dfx$FactLacTemp),mean,na.rm=T)
#   colnames(MyMeans)<-gsub("Group.1","LactationF",colnames(MyMeans))
#   MyMeans$FactLac<-MyMeans$LactationF
#   MyMeans$FactLac[MyMeans$FactLac=="4"]<-"4+"
#   MyMeans$FactLac[MyMeans$FactLac=="-1"]<-"Calf"
#   MyMeans$FactLac[MyMeans$FactLac=="0"]<-"Heifer"
#   
#   # Fix to correct the error with Some Lactation Classes have no mean within  ***Appended Trait function***
#   
#   # if(any(is.na(MyMeans[,paste(c("Sire","Mgs","Mggs"),Trait,sep="")]))){
#   #   stop("Unhandeled error in Appended Trait function. Some Lactation Classes have no mean!")
#   # }
#   for(Level in c("Sire","Mgs","Mggs")){
#     dfx[is.na(dfx[,paste(Level,Trait,sep="")]),paste(Level,Trait,sep="")]<-MyMeans[match(dfx[is.na(dfx[,paste(Level,Trait,sep="")]),"LactClass"],MyMeans$FactLac),paste(Level,Trait,sep="")]
#     #dfx[is.na(dfx[,paste(Level,Trait,sep="")]),"LactClass"]
#   }
#   dfx$Missing<-MyMeans[match(dfx$FactLacTempT+1,MyMeans$LactationF),paste("Mggs",Trait,sep="")]
#   dfx[,paste("TGen",Trait,sep="")]<-
#     (dfx[,paste("Sire",Trait,sep="")]*0.5)+
#     (dfx[,paste("Mgs",Trait,sep="")]*0.25)+
#     (dfx[,paste("Mggs",Trait,sep="")]*0.125)+
#     #(dfx[,"Missing"]+0.125) this lin e was a plus but wrong
#     (dfx[,"Missing"]*0.125)
#   if(Trait%in%colnames(dfx)){
#     
#     dfx[is.na(dfx[,Trait]),Trait]<-dfx[is.na(dfx[,Trait]),paste("TGen",Trait,sep="")]
#     
#   }else{
#     dfx[,Trait]<-dfx[,paste("TGen",Trait,sep="")]
#   }
#   if(!Source%in%"GMS"){
#     dfx[,paste("PregPA",Trait,sep="")]<-(dfx[,Trait]+dfx[,paste("Preg",Trait,sep="")])/2
#   }
#   Drop<-c("FactLacTemp","FactLacTempT","Missing",paste("Mgs",Trait,sep=""),paste("Mggs",Trait,sep=""))
#   
#   if(Trait!="PLI"){
#     Drop<-c(Drop,paste("TGen",Trait,sep=""))
#   }else{
#     cat(paste(sum(is.na(dfx$PLI)),"animals are ommited from the audit"))
#   }
#   
#   return(dfx[,!colnames(dfx)%in%Drop])
# }
# 

##
#Function to Append Genomic Data on
##

# appendGenomic <- function(dfx,gtData,dfTrait,gtTrait){
#   #gtData$Link<-substring(gtData$AnimalInterbullID,8,19)
#   cat(paste0("Merging data against GT Data -- dfherd trait:",dfTrait, "- and GT trait:", gtTrait, "\n" ))
#   dfx <- merge(dfx,setNames(gtData[,c("Link",gtTrait,"PTA_mfp_reliability_lac_all")],
#                             c("Link",paste("G.",gtTrait,sep=""),
#                               "PTA_mfp_reliability_lac_all")),by.x="IdNo",by.y="Link",all.x=T)
#   dfx[which(dfx$ProdRel<dfx$PTA_mfp_reliability_lac_all&(!is.na(dfx[,paste("G.",gtTrait,sep="")]))),dfTrait]<-dfx[which(dfx$ProdRel<dfx$PTA_mfp_reliability_lac_all&(!is.na(dfx[,paste("G.",gtTrait,sep="")]))),paste("G.",gtTrait,sep="")]
#   Drop<-c(paste("G.",gtTrait,sep=""),"PTA_mfp_reliability_lac_all")
#   
#   
#   dfx <-dfx[,!colnames(dfx) %in% Drop]
#   
#   #return(dfx[,!colnames(dfx) %in% Drop])
#   return(dfx)
# }

appendGenomicReplace <- function(dfx, gtData, dfTrait, gtTrait){
  #gtData$Link<-substring(gtData$AnimalInterbullID,8,19)
  cat(paste0("REPLACING data against GT Data -- dfherd trait:",dfTrait, "- and GT trait:", gtTrait, "\n" ))
  
  #### need this for testing
  
  dfx[,dfTrait] <- gtData[,gtTrait]
  
  return(dfx)
}

 
prepareAHDBSpecAndRename <- function(AHDBloc, strDirResources, strAHDBColumnSpecFile) {
  
  # --- Load Raw AHDB Data ---
  AHDB_raw <- read.csv(paste0(AHDBloc, "AHDB-Files/AHDBAllData", getCurrentProofRun(), ".csv"))
  
  # --- Load AHDB Column Spec ---
  AHDBspec_new <- read.csv(paste0(strDirResources, strAHDBColumnSpecFile), stringsAsFactors = FALSE)
  
  # --- Filter Spec to Inputs Present in Raw Data ---
  AHDBspec_filtered <- AHDBspec_new[AHDBspec_new$Input %in% colnames(AHDB_raw), ]
  
  # --- Add Any Manually Expected Missing Columns ---
  missing_columns <- c("Herd", "X.PLI.Rel", "HealthyCow", "EnviroCow", "Feed.Advantage")
  new_rows <- data.frame(Input = missing_columns, Output = missing_columns, stringsAsFactors = FALSE)
  AHDBspec_final <- rbind(AHDBspec_filtered, new_rows)
  
  # --- Save Final Spec ---
  write.csv(AHDBspec_final, file = paste0(strDirResources, "AHDBspec_final.csv"), row.names = FALSE)
  
  # --- Create Rename Mapping ---
  rename_map <- setNames(AHDBspec_final$Output, AHDBspec_final$Input)
  
  # --- Rename Columns in Raw Data ---
  AHDB_renamed <- AHDB_raw
  matched_cols <- intersect(colnames(AHDB_renamed), names(rename_map))
  colnames(AHDB_renamed)[colnames(AHDB_renamed) %in% matched_cols] <- rename_map[matched_cols]
  
  # --- Return Named Outputs ---
  return(list(
    renamed_raw = AHDB_renamed,
    spec = AHDBspec_final
  ))
}