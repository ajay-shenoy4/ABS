##############################
# National Evaluation Genomic Testing Data Master Loader
##############################

### --- Set Global Paths ---
print("Loading all needed paths and files.")
BaseFolderloc <<- "/data/dairy/EMEAProjects/Audit-Data-Loader/"
CustList <<- paste0(BaseFolderloc, "Customer - Lists/")
AHDBloc <<- paste0(BaseFolderloc, "Scripts - AHDB/")
AHDBFileLoc <<- paste0(AHDBloc, "AHDB-Files/")
LogLoc <<- paste0(BaseFolderloc, "Logs/")
DatasetLoc <<- paste0(BaseFolderloc, "Datasets/AHDB/")
AHDBCust <- read.csv(paste0(CustList, "AHDBCust.csv"))

### --- Global Variables ---
LogColNames <- c("Shortname", "MRO", "MRONo", "AHDBNum", "AHDBCount", "GTCount", "OverallCount", "LastRan")
AuditDTS <- format(Sys.time(), "%Y-%m-%d %H:%M")

### --- Load Mapping Files and Utility Scripts ---
ColMap <- read.csv(paste0(BaseFolderloc, "NatEvalResources/AHDBPowerBIColumnHeaders.csv"))
source("/data/dairy/USERS/ashenoy/AHDB/globalINDEXMine.R")
source("/data/dairy/USERS/ashenoy/AHDB/AHDBMasterFunctionsMine.R") #<-- no need to run again, takes long
source("/data/dairy/USERS/ashenoy/AHDB/myPlotCreator.R")  # Load plot functions
source("/data/dairy/USERS/ashenoy/AHDB/PlotCreatorFunctionsMine.R")

### --- Pull Customer Record from SQL ---
DataProgDB <- DBI::dbConnect(odbc::odbc(),
                             Driver = db_cfg_EMEA$driver,
                             Server = db_cfg_EMEA$server,
                             Database = db_cfg_EMEA$database,
                             UID = db_cfg_EMEA$uid,
                             PWD = db_cfg_EMEA$pwd,
                             Port = db_cfg_EMEA$port)

custs <- data.table(dbGetQuery(DataProgDB, "SELECT * FROM CustList"))
dbDisconnect(DataProgDB)
head(custs)

print("Grabbing customer information")
### --- Set Customer ---
CustDataKey <- "42645" # Good Data
CustRecord <- custs[EvalType == "AHDB" & DataKey == CustDataKey, ] |> as.data.frame()
head(CustRecord)

if (nrow(CustRecord) != 1) {
  stop("\n Error: No unique AHDB customer record found for this CustDataKey.\n")
}

print("Loading herd data based on customer info")

### --- Load Herd Data ---
Sourcedat <- ifelse(CustRecord$DataSource == "GMS:MasterPlan", "GMS", CustRecord$DataSource)
Sourcedat

#need to find what file NEHerdsData accesses
dfHerd <- getNEHerdsData(CustDataKey, Sourcedat)
head(dfHerd)

if (empty(dfHerd)) {
  cat("No MRO data for customer. Logging failure and stopping.\n")
  
  Log <- read.csv(paste0(LogLoc, "AHDB-Audit-Log.csv"))
  logRow <- as.data.frame(list(CustRecord$Custfolder, CustRecord$MRO, CustRecord$MROnum,
                               "No Data under MRO Number", "Failed", "Failed", "Failed", AuditDTS),
                          col.names = LogColNames)
  
  if (Shortname %in% Log$Shortname) {
    Log[Log$Shortname == Shortname, ] <- logRow
  } else {
    Log <- rbind(Log, logRow)
  }
  
  write.csv(Log, paste0(LogLoc, "AHDB-Audit-Log.csv"), row.names = FALSE)
  stop()
}

print("dfHerd after getting customer info")
head(dfHerd)

### --- Load Customer's AHDB Master Data ---
# Load and rename AHDB data
result <- prepareAHDBSpecAndRename(AHDBloc, strDirResources, strAHDBColumnSpecFile)

# Access the full renamed dataset
AHDBMaster <- result$renamed_raw

# Subset for the current customer
AHDBNumber <- CustRecord$AHDB
print(paste("Customer AHDB Number:", AHDBNumber))

dfAHDB <- AHDBMaster[AHDBMaster$Herd == AHDBNumber, ]
head(dfAHDB)

# Handle missing AHDB rows
if (nrow(dfAHDB) == 0) {
  dfAHDB <- AHDBMaster[1, ]
  dfAHDB$LineNumber <- 12345678
}

head(dfAHDB)
head(dfHerd)

### --- Merge AHDB Data with Herd Data ---
if (is.null(dfAHDB)) {
  cat("No AHDB Data Found - MRO-only output.\n")
  dfHerd$GenomicIndicator <- NA
} else {
  # Link Herd IdNo to AHDB Eartags
  dfHerd$HBNLink <- dfHerd$IdNo
  updateIdx <- !dfHerd$HBNLink %in% dfAHDB$Eartag & dfHerd$HBN %in% dfAHDB$Eartag
  dfHerd$HBNLink[updateIdx] <- dfHerd$HBN[updateIdx]
  
  # Merge AHDB onto herd data with suffixes
  dfHerd <- merge(dfHerd, dfAHDB, by.x = "HBNLink", by.y = "Eartag", all.x = TRUE, all.y = TRUE, suffixes = c("", ".A"))
  
  # Fill NA values in original columns using '.A' columns
  for (col in colnames(dfHerd)) {
    if (endsWith(col, ".A")) {
      base_col <- sub("\\.A$", "", col)
      if (base_col %in% colnames(dfHerd)) {
        na_idx <- is.na(dfHerd[[base_col]]) & !is.na(dfHerd[[col]])
        dfHerd[[base_col]][na_idx] <- dfHerd[[col]][na_idx]
      }
    }
  }
  
  # Handle special case for SireHBN (must be before dropping .A columns)
  if ("SireHBN" %in% colnames(dfHerd) && "SireHBN.A" %in% colnames(dfHerd)) {
    dfHerd$SireHBN[is.na(dfHerd$SireHBN)] <- dfHerd$SireHBN.A[is.na(dfHerd$SireHBN)]
  }
  
  # Remove the '.A' columns
  dfHerd <- dfHerd[, !grepl("\\.A$", colnames(dfHerd))]
  
  # Apply breed and age filters
  dfHerd <- DropOldHeifer(dfHerd)
  dfHerd <- SireBreedsDropped(dfHerd, BreedsDropped)
  dfHerd <- AnimalBreedsDropped(dfHerd, BreedsDropped)
  
  dfHerd <- dfHerd[, !(colnames(dfHerd) %in% c("HBNLink"))]
  dfHerd$CurrentLact[is.na(dfHerd$CurrentLact)] <- dfHerd$CurrLact[is.na(dfHerd$CurrentLact)]
  
  # Clean DOB fields
  dfHerd$DOB[dfHerd$DOB == ""] <- NA
  dobFixIdx <- is.na(dfHerd$DatDOB) & !is.na(dfHerd$DOB)
  if (sum(!is.na(dfHerd$DOB) & is.na(dfHerd$DatDOB)) > 5) {
    dfHerd$DatDOB[is.na(dfHerd$DatDOB) & !is.na(dfHerd$DOB)] <- format(dfHerd$DOB[is.na(dfHerd$DatDOB) & !is.na(dfHerd$DOB)], "%d/%m/%Y")
  }
  
  dfHerd$PLI[dfHerd$PLI == -999] <- NA
  
  # Remove rows with all NA values
  dfHerd <- dfHerd[!apply(is.na(dfHerd), 1, all), ]
  
  # Reset row numbering
  row.names(dfHerd) <- NULL
}
print("AHDB data is merged onto dfHerd")
head(dfHerd)
colnames(dfHerd)

### --- Lactation Classes and Derived Fields ---
dfHerd$ProdRel[is.na(dfHerd$ProdRel)] <- 33
dfHerd$ExGI <- ifelse(is.na(dfHerd$GenomicIndicator), "", dfHerd$GenomicIndicator)
dfHerd$ExPLI <- dfHerd$PLI
dfHerd$GenomicIndicator[is.na(dfHerd$GenomicIndicator)] <- ""

dfHerd$LactClass <- dfHerd$CurrentLact
dfHerd$LactClass[dfHerd$CurrentLact > 3] <- "4+"

zeroLactIdx <- which(!is.na(dfHerd$LactClass) & dfHerd$LactClass == "0")
dfHerd$LactClass[zeroLactIdx] <- ifelse(as.Date(dfHerd$DatDOB[zeroLactIdx], "%d/%m/%Y") > (Sys.Date() - 365), "Calf", "Heifer")

dfHerd$FactLactClass <- factor(dfHerd$LactClass, levels = rev(c("Pregnancy", "Calf", "Heifer", "1", "2", "3", "4+")), ordered = TRUE)

print("dfHerd with lact info added")
head(dfHerd)
colnames(dfHerd)

### --- Days Open and 305-Day Milk Equivalents ---
if(! Sourcedat %in% "GMS"){
  dfHerd[which(dfHerd$ResultPD),"DOPN"] <- as.numeric(as.Date(dfHerd$DatLatestService[which(dfHerd$ResultPD)], "%d/%m/%Y") - 
                                                        as.Date(dfHerd$DatCalving[which(dfHerd$ResultPD)], "%d/%m/%Y"))
  
  ScalingFactor <- data.frame(Lactation_Number = as.character(c(1, 2, 3, 4, 5, 6 : 1000)[1 : max(as.numeric(dfHerd[, "Yield305DayLact"]), na.rm = T)]),
                              Scale = as.numeric(c(1.31, 1.17, 1.07, 1.02, 1, rep(1, 995))[1 : max(as.numeric(dfHerd[, "Yield305DayLact"]), na.rm = T)]))
  dfHerd$KG305ME <- as.numeric(dfHerd$Yield305Day) * ScalingFactor$Scale[match(dfHerd$Yield305DayLact, ScalingFactor$Lactation_Number)]
  
  
  dfHerd$KG305ME[dfHerd$KG305ME==0] <- NA
  dfHerd$KGFat305ME <- dfHerd$KG305ME * (as.numeric(dfHerd$TestFatPc) / 100)
  dfHerd$KGFat305ME[dfHerd$KGFat305ME == 0] <- NA
  dfHerd$KGProt305ME<-dfHerd$KG305ME * (as.numeric(dfHerd$TestProtPc) / 100)
  dfHerd$KGProt305ME[dfHerd$KGProt305ME == 0] <- NA
  dfHerd$TestProtPc[dfHerd$TestProtPc == 0] <- NA
  dfHerd$TestFatPc[dfHerd$TestFatPc == 0] <- NA
  
  dfHerd$TestFatPc <- as.numeric(dfHerd$TestFatPc)
  dfHerd$TestProtPc <- as.numeric(dfHerd$TestProtPc)
}

print("dfHerd with 305 info added")
head(dfHerd)
colnames(dfHerd)

print("Data structure labels")
dfDataStruc <<- getDataStruc()
dfDataStruc

### --- Load Sire Data and Proofs ---
dfSires <- getUKBullDataDB(dfHerd)
print("Sire data")
head(dfSires)

# ---- Fix TB Advantage in dfSires ----
if ("TB_Advantage" %in% colnames(dfSires)) {
  colnames(dfSires)[colnames(dfSires) == "TB_Advantage"] <- "TBAdvantage"
}

if ("TBAdvantage" %in% names(dfSires)) {
  dfSires$TBAdvantage <- as.numeric(as.character(dfSires$TBAdvantage)) / 100
}

ProofRun <- "01_2025"
### --- Load or Create dfAuditFullProofs ---
#Chris and George are editing uk code, default to the older proofrun that i know works
proofFile <- paste0(ProofFolderLoc, ProofRun, "/", "dfAuditFullProofs.csv")
if (file.exists(proofFile)) {
  cat(paste0("Reading dfAuditFullProofs from ", proofFile, "\n"))
  dfAuditFullProofs <<- read.csv(proofFile)
} else {
  dfAuditFullProofs <<- getLastProofs("HOL")
  write.csv(dfAuditFullProofs, proofFile, row.names = FALSE)
}

head(proofFile)

### --- Load GT Data and Merge Genomics ---
if (!exists("GTDataMaster")) GTDataMaster <- getAHDBGenomics()
head(GTDataDB_look)

# Column renames for consistency
GTDataMaster$Link <- substring(GTDataMaster$AnimalInterbullID, 8, 19)

GTData <- GTDataMaster[GTDataMaster$Link %in% dfHerd$IdNo, ]
cat(paste0("Number GT animals for Customer: ", nrow(GTData), "\n"))

### --- Drop Duplicates and Add Missing Columns ---
dfHerd <- dfHerd[nchar(dfHerd$IdNo) >= 12, ]
dfHerd <- dfHerd[!duplicated(dfHerd$IdNo), ]
head(dfHerd)
colnames(dfHerd)

# Define category renaming mapping
category_map <- c(
  "Udder Care" = "Fitness",
  "Components" = "Production",
  "Enviro Cow" = "Enviro Cow",
  "Feed Advantage" = "Feed Advantage",
  "FertilityIndex" = "FertilityIndex",
  "Trait" = "Robot",
  "Lifespan" = "Lifespan"
)

# Rename categories in dfDataStruc if they match keys in category_map
dfDataStruc$Category <- ifelse(
  dfDataStruc$Category %in% names(category_map),
  category_map[dfDataStruc$Category],
  dfDataStruc$Category
)

# Functional Type recoding
dfDataStruc <- dfDataStruc %>%
  mutate(Category = case_when(
    Category == "Production" & Trait == "MilkKg"  ~ "Production Kg",
    Category == "Production" & Trait == "FatKg" ~ "Production Kg",
    Category == "Production" & Trait == "ProtKg"  ~ "Production Kg",
    Category == "Production" & Trait == "FatPc" ~ "Production Pc",
    Category == "Production" & Trait == "ProtPc" ~ "Production Pc",
    Category == "Fitness" & Trait == "SCC" ~ "Milk Fitness",
    Category == "Fitness" & Trait == "Mastitis" ~ "Milk Fitness",
    Category == "LamenessAdvantage" & Trait == "LamenessAdvantage" ~ "Motion",
    Category == "Feet and Legs" & Trait == "LegsFeet" ~ "Motion",
    Category == "Functional Type" & Trait == "Mammary"  ~ "Type",
    Category == "Functional Type" & Trait == "LegsFeet" ~ "Motion",
    Trait == "Temperament" & Category == "Robot" ~ "Robot Extra",
    TRUE ~ Category
  ))

### Add new GT traits to dfDataStruc
new_traits <- data.frame(
  Category   = c(
    "Calving", "Fitness", "SCI", "ACI", "Body Composite Extra", "Body Composite Extra", 
    "Rump", "Rump", "Incomplete - No Sire Data, Do Not Use", "Feet and Legs Conformation", "Udder Composite", "Udder Composite", "Udder Composite",
    "Udder Composite", "Teats", "Teats", "Robot - Extra", "Motion", "Calving", "Fitness", "Fitness"),
  Trait      = c(
    "CalfSurvival", "DigitalDermatitis", "SCI", "ACI", "BodyDepth", "Angularity",
    "RumpAngle", "RumpWidth", "RearLegRearView", "FootAngle", "ForeUdderAttachment", "RearUdderHeight", "UdderSupport",
    "UdderDepth", "FrontTeatPlaceRearView", "TeatPlacementSideView", "Temperament", "Locomotion", "GestationLength", "TBAdvantage", "HealthyCow"),
  Genomic    = c(
    "Calf_survival", "DigitalDermatitis", "SCI", "ACI", "BODY_DEPTH_GEBV", "ANGULARITY_GEBV",
    "RUMP_ANGLE_GEBV", "RUMP_WIDTH_GEBV", "REAR_LEG_SIDE_GEBV", "FOOT_ANGLE_GEBV", "FORE_UDD_ATT_GEBV", "REAR_UDDER_HT_GEBV", "UDDER_SUPP_GEBV", 
    "UDDER_DEPTH_GEBV", "FRONT_TEAT_PL_GEBV", "TEAT_POS_SIDE_GEBV", "TEMPERAMENT_GEBV", "LOCOMOTION_GEBV", "GestationLength", "TBAdvantage", "HealthyCow"),
  Label      = c(
    "Calf Survival", "Digital Dermatitis", "SCI", "ACI", "Body Depth", "Angularity",
    "Rump Angle", "Rump Width", "Rear Legs Side", "Foot Angle", "Fore Udder Attachment", "Rear Udder Height", "Udder Support",
    "Udder Depth", "Front Teat Placement", "Teat Placement Side View", "Temperament", "Locomotion", "Gestation Length", "TB Advantage", "Healthy Cow"),
  Phenotype  = "",
  Nearest    = 1,
  Increasing = TRUE
)

# 4. Remove overlapping traits from dfDataStruc to allow overwrite
dfDataStruc <- dfDataStruc[!(dfDataStruc$Trait %in% new_traits$Trait), ]

# 5. Append updated new_traits
dfDataStruc <- rbind(dfDataStruc, new_traits)

# 6. Deduplicate by Trait
dfDataStruc <- dfDataStruc[!duplicated(dfDataStruc$Trait), ]
dfDataStruc

# Move LamenessAdvantage row above LegsFeet in dfDataStruc
if (all(c("LamenessAdvantage", "LegsFeet") %in% dfDataStruc$Trait)) {
  la_row <- dfDataStruc[dfDataStruc$Trait == "LamenessAdvantage", ]
  dfDataStruc <- dfDataStruc[dfDataStruc$Trait != "LamenessAdvantage", ]
  
  legsfeet_index <- which(dfDataStruc$Trait == "LegsFeet")[1]
  
  if (!is.na(legsfeet_index)) {
    dfDataStruc <- rbind(
      dfDataStruc[1:(legsfeet_index - 1), ],
      la_row,
      dfDataStruc[legsfeet_index:nrow(dfDataStruc), ]
    )
  } else {
    # If LegsFeet not found, just append LamenessAdvantage to end
    dfDataStruc <- rbind(dfDataStruc, la_row)
  }
}
dfDataStruc

print("Trait data structure with new traits added")
neededcols <- c(dfDataStruc$Trait[dfDataStruc$Genomic != ""], "GenomicIndicator")
neededcols
dfHerd <- fncols(dfHerd, neededcols)
dfxGT <- dfHerd[dfHerd$IdNo %in% GTData$Link, ]

colnames(dfHerd)
colnames(dfxGT)
colnames(GTData)
dfDataStruc

### --- Merge GT Genomic Data ---
if (nrow(GTData) == 0) {
  cat("Skipping GT merge - no GT rows for this customer.\n")
} else {
  dfxGT <- dfxGT[order(dfxGT$IdNo), ]
  GTData <- GTData[order(GTData$Link), ]
  
  if (!all(dfxGT$IdNo %in% GTData$Link) || !all(GTData$Link %in% dfxGT$IdNo)) {
    stop("GTData mismatch with dfHerd. Check order or missing IDs.\n")
  }
  
  for (i in which(dfDataStruc$Genomic != "")) {
    dfTrait <- dfDataStruc$Trait[i]
    gtTrait <- dfDataStruc$Genomic[i]
    
    if (gtTrait %in% colnames(GTData)) {
      dfxGT <- appendGenomicReplace(dfxGT, GTData, dfTrait, gtTrait)
    } else {
      cat(paste0("Warning: GTData does not contain column ", gtTrait, ", skipping.\n"))
    }
  }
  
  dfxGT <- appendGenomicReplace(dfxGT, GTData, "GenomicIndicator", "GenomicIndicator")
  print("dfxGT")
  print(head(dfxGT))
  
  extra_cols <- setdiff(colnames(dfxGT), colnames(dfHerd))
  
  for (col in extra_cols) {
    dfHerd[[col]] <- NA
  }
  
  dfHerd <- rbind(
    dfHerd[!dfHerd$Eartag %in% dfxGT$Eartag, ],
    dfxGT
  )
}

cat("Data Load and GT Merge Completed.\n")
head(dfHerd)
head(dfxGT)

colnames(dfHerd)
colnames(dfxGT)

##########################################
# === Correct Way: Calculate Genomic Indicator Flags ===
##########################################

# Set all blanks as Calculated PA (Pedigree Average)
dfHerd$GenomicIndicator[dfHerd$GenomicIndicator %in% ""] <- "CalcPA"

# If animal appears in AHDB with GenomicIndicator = "G", label as "G"
dfHerd[dfHerd$IdNo %in% dfAHDB$Eartag[dfAHDB$GenomicIndicator %in% "G"], "GenomicIndicator"] <- "G"

# For animals with PLI, originally "CalcPA", and existing in AHDB, set as "AHDB"
dfHerd[dfHerd$IdNo %in% dfAHDB$Eartag &
         !is.na(dfHerd$PLI) &
         dfHerd$GenomicIndicator %in% "CalcPA", "GenomicIndicator"] <- "AHDB"

# For animals in GTData but not already flagged as "G", set as "G"
dfHerd[dfHerd$IdNo %in% GTData$ear_tag &
         !dfHerd$GenomicIndicator %in% "G", "GenomicIndicator"] <- "G"

##########################################
# === Days Open Cleanup (DOPN Filtering) ===
##########################################

# Set Days Open > 250 to NA (for better data quality in reports/plots)
excessDOPN <- sum(dfHerd$DOPN > 250, na.rm = TRUE)
cat(excessDOPN, "animals with DOPN > 250 set to NA.\n")
dfHerd$DOPN[dfHerd$DOPN > 250] <- NA
  
##########################################
# === HBN Swap: NMR Missing Issue Fix ===
##########################################

# If all HBNs are NA, use Eartag field
if (all(is.na(dfHerd$HBN))) {
  cat("\n Swapped HBNs to Eartags for all NA rows (NMR issue).\n")
  dfHerd$HBN <- dfHerd$Eartag
}

# === Inspect Available Traits and Data Structure ===
cat("=== dfDataStruc Trait List ===\n")
print(dfDataStruc$Trait)

cat("\n=== dfHerd Column Names ===\n")
print(colnames(dfHerd))

cat("\n=== dfSires Column Names ===\n")
print(colnames(dfSires))

colnames(dfSires)[colnames(dfSires) == "Lameness"] <- "LamenessAdvantage"

print(colnames(dfSires))

# Check if raw trait exists in dfSires and dfHerd
for (trait in dfDataStruc$Trait) {
  in_sires <- trait %in% colnames(dfSires)
  in_herd  <- trait %in% colnames(dfHerd)
  
  cat("\nTrait:", trait, "\n")
  cat("  Exists in dfSires? ", in_sires, "\n")
  cat("  Exists in dfHerd?  ", in_herd, "\n")
}

cat("\n=== Starting Trait Loop ===\n")

##########################################
# === Trait Appending (Main Loop) ===
##########################################
for (MyTrait in dfDataStruc$Trait) {
  cat("Running Trait:", MyTrait, "\n")
  dfHerd <- AppendTraitDB(dfHerd, dfSires, MyTrait, Sourcedat)
}

# Drop dummy rows from dfHerd (same logic as original)
dfHerd <- dfHerd[!dfHerd$LineNumber %in% 12345678, ]

# Function to clean and rearrange columns in a dataframe as you want
cleanAndRearrangeCols <- function(df) {
  # 1. Use EnviroCow to fill in missing values in Enviro.Cow, then drop EnviroCow
  if (all(c("EnviroCow", "Enviro.Cow") %in% colnames(df))) {
    idx <- is.na(df[["Enviro.Cow"]]) & !is.na(df[["EnviroCow"]])
    df$Enviro.Cow[idx] <- df$EnviroCow[idx]
    df$EnviroCow <- NULL
    cat("Filled Enviro.Cow with EnviroCow values where Enviro.Cow was NA, then dropped EnviroCow.\n")
  }
  
  # 2. Rename columns
  rename_map <- c(
    "X.PLI.Rel" = "PLIRel"
  )
  
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x %in% names(rename_map)) rename_map[[x]] else x
  })
  
  # 3. Move Lameness and LamenessAdvantage to positions 62 and 63
  if (all(c("Lameness", "LamenessAdvantage") %in% colnames(df))) {
    cols <- colnames(df)
    
    # Remove those columns from current positions
    cols <- cols[!(cols %in% c("Lameness", "LamenessAdvantage"))]
    
    # Insert them at positions 62 and 63 (if df has fewer than 61 cols, append at end)
    pos <- min(61, length(cols))  # zero-based: position 62 means index 61 in R (1-based)
    
    # Build new col order: first up to pos, then Lameness, LamenessAdvantage, then rest
    new_order <- c(
      cols[1:pos],
      "Lameness",
      "LamenessAdvantage",
      if(pos < length(cols)) cols[(pos+1):length(cols)] else NULL
    )
    
    df <- df[, new_order]
  }
  
  return(df)
}


# Apply cleaning to both dataframes
dfHerd <- cleanAndRearrangeCols(dfHerd)
dfxGT <- cleanAndRearrangeCols(dfxGT)
colnames(dfHerd)
##########################################
# === Save Final Herd Data Before Logging ===
##########################################

dfHerdAUDIT <- dfHerd  # Save full final dataset for audit/reporting use later

##########################################
# === LOGGING FINAL COUNTS ===
##########################################

# Capture final AHDB row count for customer
CustAHDB <- if (empty(dfAHDB)) 0 else nrow(dfAHDB)

cat("Writing to Log Master: FINISHED\n")

# (Your old log updating block can go here if you want to reactivate it)

cat("Writing to Log Master: FINISHED\n")

# (Your old log updating block can go here if you want to reactivate it)

##########################################
# === GMS Special Output Standardization ===
##########################################

if (Sourcedat %in% "GMS") {
  dfHerd$LineNumber <- dfHerd$Freezebrand
  dfHerd$UkEartag <- dfHerd$IdNo
  dfHerd$Eartag <- dfHerd$IdNo
  dfHerd$Sire.CountryCode <- dfHerd$SireCountry
  dfHerd$SSireBreedCode <- dfHerd$SireBreedCode
  dfHerd$SireBreed <- dfHerd$SireBreedCode
  
  # Clean NaN values in key columns
  na_fix_cols <- c("SireFeed.Advantage", "SireEnviro.Cow", "FeedAdvantage", "Enviro.Cow")
  for (col in na_fix_cols) {
    if (col %in% colnames(dfHerd) && any(dfHerd[[col]] %in% "NaN", na.rm = TRUE)) {
      dfHerd[[col]][dfHerd[[col]] %in% "NaN"] <- NA
    }
  }
}  

head(dfHerd)

#############################################
# === FINAL COLUMN STANDARDIZATION & EXPORT ===
#############################################
# --- 1. Fix HerdId if NA ---
if (any(is.na(dfHerd$HerdId))) {
  HerdID <- unique(dfHerd$HerdId[!is.na(dfHerd$HerdId)])
  dfHerd$HerdId <- HerdID
}

# --- 2. Drop rows with missing IdNo (core ID column) ---
dfHerd <- dfHerd[!is.na(dfHerd$IdNo), ]

# 1. Vectorized renaming using ColMap
name_map <- setNames(ColMap$Output, ColMap$Input)
colnames(dfHerd) <- ifelse(colnames(dfHerd) %in% names(name_map),
                           name_map[colnames(dfHerd)],
                           colnames(dfHerd))

# 2. Identify duplicate column names
dup_names <- unique(colnames(dfHerd)[duplicated(colnames(dfHerd))])

# 3. Merge values in duplicate columns
for (name in dup_names) {
  dup_cols <- which(colnames(dfHerd) == name)
  
  # If more than one column shares the name
  if (length(dup_cols) > 1) {
    # Combine values: take first non-NA across the duplicates
    merged_col <- dfHerd[[dup_cols[1]]]
    for (j in 2:length(dup_cols)) {
      merged_col <- ifelse(!is.na(merged_col), merged_col, dfHerd[[dup_cols[j]]])
    }
    
    # Assign merged column back and drop others
    dfHerd[[name]] <- merged_col
    dfHerd <- dfHerd[, -dup_cols[-1]]  # Keep the first, drop the rest
  }
}

head(dfHerd)

# --- Drop Unwanted Columns ---

# Check for duplicate columns before dropping
dup_cols <- colnames(dfHerd)[duplicated(colnames(dfHerd))]
if (length(dup_cols) > 0) {
  cat("Duplicate columns before drop_cols:\n")
  print(dup_cols)
}

drop_cols <- c(
  "Link", "CalvingDate", "HerdName", "HerdAddress1", "MilkStatus", "DatLatestMetritis",
  "PreviousLactProdIndex", "ExtendedYield", "SSireName", "LastMastitisDate", "LastMetritisDate",
  "DatPD", "DatLatestService", "DatLatestMastitis", "PedigreeStatus", "ExGI", "ExPLI"
)

dfHerd <- dfHerd[, !(colnames(dfHerd) %in% drop_cols)]

# View final columns
head(dfHerd)

# --- 5. Drop Extra Columns ---
dfHerd <- dfHerd[, !(colnames(dfHerd) %in% drop_cols)]

head(dfHerd)
colnames(dfHerd)

#############################################
# === FINAL QC FIXES BEFORE OUTPUT ===
#############################################

# --- Fix DOB Formats ---
dobIdx <- grepl("/", dfHerd$DatDOB) & !is.na(dfHerd$DatDOB)
dfHerd$DatDOB[dobIdx] <- format(as.Date(dfHerd$DatDOB[dobIdx], "%d/%m/%Y"), "%Y-%m-%d")
dfHerd$DatDOB <- as.Date(dfHerd$DatDOB, "%Y-%m-%d")

# --- Round PLI Values ---
dfHerd$PLI <- round(dfHerd$PLI, 0)

# --- Add Audit Timestamps and Data Key ---
dfHerd$GenusDTS <- format(Sys.time(), "%d-%m-%Y-%H-%M", tz = "GMT")
dfHerd$DataKey <- CustDataKey

# --- Drop Animals with Invalid PLI ---
dfHerd <- dfHerd[!dfHerd$PLI %in% -999, ]

# --- Freezebrand: Clean Non-numeric Entries ---
dfHerd$Freezebrand[grepl("[A-Za-z]", dfHerd$Freezebrand)] <- "1"
dfHerd$LineNumber[grepl("[A-Za-z]", dfHerd$LineNumber)] <- "1"

# --- Final HerdId Check ---
HerdID <- unique(dfHerd$HerdId)

#############################################
# === EXPORT FINAL OUTPUT ===
#############################################

output_file <- paste0(DatasetLoc, "dfHerd.csv")

# Note: Uncomment if write permission is restored
# write.csv(dfHerd, output_file, row.names = FALSE)  

cat("Final Herd dataset path:", output_file, "\n")

# Also save to working directory (assumed writable)
write.csv(dfHerd, file = file.path(getwd(), "dfHerd.csv"), row.names = FALSE)

# Optional read-back (ensure output_file is accessible)
dfCC <- read.csv(output_file)

# Convert large ID-like columns to character to avoid overflow in SQL
id_cols <- c("MgsHBN", "MggsHBN", "SSireHBN", "IdNo", "DamHBN")
dfCC[id_cols] <- lapply(dfCC[id_cols], as.character)

##########################################
# === Write Final Data to SQL + Generate Plots + Zip Folder ===
##########################################

# Load DB config YAML
default_config_path <- normalizePath("~/.scapi/db_config.yml", mustWork = FALSE)
db_config_path <- normalizePath("~/.scapi/config.yml", mustWork = FALSE)

config_file <- if (file.exists(db_config_path)) db_config_path else default_config_path

if (!file.exists(config_file)) {
  stop("DB config file not found at: ", config_file)
}

db_yaml <- read_yaml(config_file)

# --- 1. Connect to SQL Server ---  
sqlconn2 <- DBI::dbConnect(odbc::odbc(),
                            Driver   = db_yaml$DataProgs$driver,
                            Server   = db_yaml$DataProgs$server,
                            Database = db_yaml$DataProgs$database,
                            UID      = db_yaml$DataProgs$uid,
                            PWD      = db_yaml$DataProgs$pwd,
                            Port     = 1433)
  
# --- 2. Delete Previous Customer Rows from Audit Table ---
CustDataKey <- unique(dfCC$DataKey)
cat("Deleting old rows for DataKey(s):", paste(CustDataKey, collapse = ", "), "...\n")

# Safely build delete query for multiple keys
keys_str <- paste(shQuote(CustDataKey, type = "sh"), collapse = ", ")
deleteQuery <- sprintf("DELETE FROM AHDBAuditData WHERE DataKey IN (%s)", keys_str)

rows_deleted <- dbExecute(sqlconn2, deleteQuery)
cat(rows_deleted, "old rows removed from AHDBAuditData.\n")

# --- 3: Check for potential issues before writing new data ---

# 1. Check for column mismatches
db_columns <- dbListFields(sqlconn2, "AHDBAuditData")
missing_in_db <- setdiff(names(dfCC), db_columns)
extra_in_db <- setdiff(db_columns, names(dfCC))

if (length(missing_in_db) > 0) {
  warning("Columns in dfCC not found in AHDBAuditData table:", paste(missing_in_db, collapse = ", "))
}

if (length(extra_in_db) > 0) {
  cat("Columns in AHDBAuditData table but not in dfCC (will be NULL or defaulted):", paste(extra_in_db, collapse = ", "), "\n")
}

# 2. Check for numeric values out of safe range
numeric_issues <- sapply(dfCC, function(x) is.numeric(x) && any(abs(x) > 1e9, na.rm = TRUE))
bad_columns <- names(dfCC)[numeric_issues]

if (length(bad_columns) > 0) {
  warning("Numeric value(s) may exceed SQL limits in columns:", paste(bad_columns, collapse = ", "))
  for (col in bad_columns) {
    cat(paste0("Problematic values in ", col, ":\n"))
    print(dfCC[abs(dfCC[[col]]) > 1e9, col, drop = FALSE])
  }
}

# 3. Convert all factors to characters to avoid coercion issues
dfCC[] <- lapply(dfCC, function(x) if (is.factor(x)) as.character(x) else x)

# 4. Optional: Remove or convert very large numerics before inserting (example commented)
# dfCC$SomeColumn[dfCC$SomeColumn > 1e9] <- NA  

cat("DataKey values present:", unique(dfCC$DataKey), "\n")
str(dfCC[c("MgsHBN", "MggsHBN", "SSireHBN", "IdNo", "DamHBN")])

# --- 3. Write New Audit Data ---
cat("Writing", nrow(dfCC), "new rows to AHDBAuditData...\n")

tryCatch({
  dbWriteTable(sqlconn2, "AHDBAuditData", dfCC, overwrite = FALSE, append = TRUE)
  cat("Data write complete.\n")
}, error = function(e) {
  message("Failed to write data to SQL: ", e$message)
})

# --- 4. Close SQL Connection ---
dbDisconnect(sqlconn2)
cat("SQL Connection Closed.\n")

# --- 5. Generate Audit Plots ---
cat("Running AHDB Audit Plots...\n")
dam_eartag = "UK581723102636"
sire_eartag = "000011720553"
AuditData <- dfHerdAUDIT  # This is the full final herd dataset for audit
AuditTemp <- AHDBplots(AuditData, CustDataKey, dam_eartag, sire_eartag, dfDataStruc)
PlotLocation <- AuditTemp[[1]]
CustName <- AuditTemp[[2]]
NOanms <- nrow(AuditData)
cat("Plots generated at:", PlotLocation, "\n")

# --- 6. Zip the Plot Folder ---
colnames(dfHerd)
filename <- paste0(PlotLocation, CustDataKey, ".zip")

tryCatch({
  utils::zip(zipfile = filename, files = list.files(PlotLocation, full.names = TRUE))
  cat("Zip file created:", filename, "\n")
}, error = function(e) {
  message("Failed to create zip file: ", e$message)
})

# --- 7. Copy all files + zip to Working Directory ---
#working_dir <- getwd()

# Copy plot images and CSVs
#file.copy(list.files(PlotLocation, full.names = TRUE), working_dir, overwrite = TRUE)

# Copy zip file
#file.copy(filename, working_dir, overwrite = TRUE)

#cat("All files copied to working directory:", working_dir, "\n")