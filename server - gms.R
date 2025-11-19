library(shiny)
library(vroom)
library(odbc)
library(DBI)
library(DT)
library(lubridate)
library(dplyr)
library(yaml)


GetGENEadvanceID <- function(Cust, sqlconn) {
  Query0 <- paste0("SELECT [id],[account_id],[account_type_code],[internal_account_id],[is_active]
                   FROM [dbo].[farm_account_lookup]
                   WHERE account_type_code = 'esdw_account_lookup' AND account_id = '", Cust, "'")
  
  HerdID <- dbGetQuery(sqlconn, Query0)
  
  isolate(if (nrow(HerdID) < 1) {
    showModal(modalDialog(title = "Palette", "Please enter a correct GENEadvance number",
                          footer = modalButton("Dismiss"), size = "l", easyClose = FALSE))
  })
  
  return(HerdID$internal_account_id)
}

CowtoMgs <- function(HerdID, Type, sqlconn) {
  Query1 <- paste0(
    "SELECT 
        A.ear_tag AS OffspringEarTag,
        A.gender AS OffspringGender, 
        A.parity AS OffspringLact, 
        A.date_of_birth AS OffspringDateOfBirth,
        A.breed AS OffspringBreed,
        A.country AS OffspringCountry,
        A.registered_name AS OffspringFullName,
        A.short_name AS OffspringName, 
        A.full_ear_tag AS OffspringFullEarTag,
        A.visual_id AS OffspringAnimalID,

        A.sire_id AS SireId, 
        A.sire_full_name AS SireFullName, 
        A.sire_breed_code AS SireBreed,
        A.sire_ear_tag AS SireEarTag,
        A.sire_name AS SireName,
        
        A.dam_ear_tag AS DamEarTagLong, 
        A.dam_name AS DamName,
        A.dam_ear_tag_parsed AS DamEarTagShort,
        
        A.maternal_grand_sire_ear_tag AS MaternalGrandSireEarTag, 
        A.maternal_grand_sire_name AS MaternalGrandSireName, 
        
        ISNULL(B.bull_num, A.sire_ear_tag) AS SireHBN,
        B.country AS SireCountry,
        
        A.is_deleted
      FROM animal_data A
      LEFT JOIN (
        SELECT * FROM (
          SELECT *, ROW_NUMBER() OVER(PARTITION BY bovine_id ORDER BY id ASC) AS row_nr
          FROM bovine_bull_lookup 
          WHERE primary_id = 1 AND is_deleted = 0
        ) results
        WHERE row_nr = 1
      ) B ON A.bovine_id = B.bovine_id
      WHERE A.internal_account_id = '", HerdID, "'"
  )
  
  CustData <- dbGetQuery(sqlconn, Query1)
  
  # Filter based on Type
  if (Type == "Active") {
   CustData <- CustData[CustData$is_deleted == 0, ]
  } else if (Type == "Historical") {
   CustData <- CustData[CustData$is_deleted == 1, ]
  }
  return(CustData)
}

GetMggs <- function(HerdID, sqlconn) {
  Query2 <- paste0(
    "SELECT 
        A.ear_tag AS DamEarTagShort,
        A.gender AS DamGender, 
        A.parity AS DamLact, 
        A.date_of_birth AS DamDateOfBirth,
        A.breed AS DamBreed,
        A.country AS DamCountry,
        A.registered_name AS DamFullName,
        A.full_ear_tag AS DamEarTagLong,
        A.visual_id AS DamAnimalID,

        A.sire_id AS MaternalGrandSireId, 
        A.sire_full_name AS MaternalGrandSireFullName, 
        A.sire_breed_code AS MaternalGrandSireBreed,

        A.dam_ear_tag AS MaternalGrandDamEarTag, 
        A.dam_name AS MaternalGrandDamName,
        A.dam_ear_tag_parsed AS MaternalGrandDamEarTagParsed,
        
        A.maternal_grand_sire_ear_tag AS MaternalGreatGrandSireEarTag, 
        A.maternal_grand_sire_name AS MaternalGreatGrandSireName, 
        B.country AS MgsCountry,
        
        A.is_deleted
      FROM animal_data A
      LEFT JOIN (
        SELECT * FROM (
          SELECT *, ROW_NUMBER() OVER(PARTITION BY bovine_id ORDER BY id ASC) AS row_nr
          FROM bovine_bull_lookup 
          WHERE primary_id = 1 AND is_deleted = 0
        ) results
        WHERE row_nr = 1
      ) B ON A.bovine_id = B.bovine_id
      WHERE A.internal_account_id = '", HerdID, "'"
  )
  
  CustData2 <- dbGetQuery(sqlconn, Query2)
  
  return(CustData2)
}

CalfForecast <- function(HerdID, sqlconn) {
  Query3 <- paste0(
    "SELECT
      A.service_date AS ServiceDate, 
      A.due_date AS DueDate, 
      A.dam_eartag AS DamEarTagShort
     FROM calf_forecast A
     WHERE A.internal_account_id = '", HerdID, "'"
  )
  
  CustData3 <- dbGetQuery(sqlconn, Query3)
  return(CustData3)
}

ReproStatus <- function(HerdID, sqlconn) {
  Query4 <- paste0(
    "SELECT
      A.repro_type_id AS ReproType, 
      A.ear_tag AS OffspringEarTag, 
      A.start_date AS StartDate, 
      A.resulted_in_pregnancy AS ResultedInPregnancy
     FROM reproduction_data A
     WHERE A.internal_account_id = '", HerdID, "' OR A.id = '", HerdID, "'"
  )
  
  CustData4 <- dbGetQuery(sqlconn, Query4)
  return(CustData4)
}

GetGreatGrandSires <- function(HerdID, sqlconn) {
  Query5 <- paste0(
    "SELECT 
        A.ear_tag AS MaternalGrandDamEarTagShort,
        A.gender AS MaternalGrandDamGender, 
        A.parity AS MaternalGrandDamLact, 
        A.date_of_birth AS MaternalGrandDamDateOfBirth,
        A.breed AS MaternalGrandDamBreed,
        A.country AS MaternalGrandDamCountry,
        A.registered_name AS MaternalGrandDamFullName,
        A.full_ear_tag AS MaternalGrandDamEarTagLong,
        A.visual_id AS MaternalGrandDamAnimalID,

        A.sire_id AS MaternalGreatGrandSireId, 
        A.sire_full_name AS MaternalGreatGrandSireFullName, 
        A.sire_breed_code AS MaternalGreatGrandSireBreed,

        A.dam_ear_tag AS MaternalGreatGrandDamEarTag, 
        A.dam_name AS MaternalGreatGrandDamName,
        A.dam_ear_tag_parsed AS MaternalGreatGrandDamEarTagParsed,
        
        A.maternal_grand_sire_ear_tag AS MaternalGreatGreatGrandSireEarTag, 
        A.maternal_grand_sire_name AS MaternalGreatGreatGrandSireName, 
        B.country AS MggsCountry,
        
        A.is_deleted
      FROM animal_data A
      LEFT JOIN (
        SELECT * FROM (
          SELECT *, ROW_NUMBER() OVER(PARTITION BY bovine_id ORDER BY id ASC) AS row_nr
          FROM bovine_bull_lookup 
          WHERE primary_id = 1 AND is_deleted = 0
        ) results
        WHERE row_nr = 1
      ) B ON A.bovine_id = B.bovine_id
      WHERE A.internal_account_id = '", HerdID, "'"
  )
  
  CustData5 <- dbGetQuery(sqlconn, Query5)
  
  return(CustData5)
}


#Will get you your login credentials for the data, file can be named db_config.yml or config.yml
#If running it as is does not work make the config.yml be default and db_config be the main
# Define possible config paths
default_config_path <- normalizePath("~/.scapi/db_config.yml", mustWork = FALSE)
db_config_path <- normalizePath("~/.scapi/config.yml", mustWork = FALSE)

# Choose which config file to use
if (file.exists(db_config_path)) {
  config_file <- db_config_path
} else {
  config_file <- default_config_path
}
# Load YAML config
db_yaml <- read_yaml(config_file)

shinyServer(function(input, output) {
  
  x <- eventReactive(input$LoadData,{
    
    # Create one shared DB connection
    sqlconn <- DBI::dbConnect(odbc::odbc(),
                              Driver   = db_yaml$DigProd_bullapp$driver,
                              Server   = db_yaml$DigProd_bullapp$server,
                              Database = db_yaml$DigProd_bullapp$database,
                              UID      = db_yaml$DigProd_bullapp$uid,
                              PWD      = db_yaml$DigProd_bullapp$pwd,
                              Port     = 1433)
    
    on.exit(dbDisconnect(sqlconn), add = TRUE)  # Ensures it's disconnected even if error occurs
    
    Cust <- input$GENEadvance
    Type <- input$Type
    
    withProgress(message = "Loading data...", value = 0, {
      missing_parts <- c()
      
      # Step 1: Get internal herd ID
      HerdID <- GetGENEadvanceID(Cust, sqlconn)
      #HerdID <- "D241D7A8-AAE2-45C1-B320-B3E29D98DE5D"
      
      # Step 2: Load main datasets
      incProgress(0.2, detail = "Loading Cow to Mgs data")
      CowMgs <- as.data.frame(CowtoMgs(HerdID, Type, sqlconn))
      if (nrow(CowMgs) == 0) {
        cat("No data found: Cow to Mgs\n")
        missing_parts <- c(missing_parts, "Cow to Mgs")
      }      
      
      incProgress(0.2, detail = "Loading Mggs data")
      Mggs <- as.data.frame(GetMggs(HerdID, sqlconn))
      if (nrow(Mggs) == 0) {
        cat("No data found: Mggs\n")
        missing_parts <- c(missing_parts, "Mggs")
      }
      
      incProgress(0.1, detail = "Loading More Mggs data")
      Mggs2 <- as.data.frame(GetGreatGrandSires(HerdID, sqlconn))
      if (nrow(Mggs2) == 0) {
        cat("No data found: Great Grand Sires\n")
        missing_parts <- c(missing_parts, "Great Grand Sires")
      }
      
      incProgress(0.2, detail = "Loading Calf Forecast data")
      CalfData <- as.data.frame(CalfForecast(HerdID, sqlconn))
      if (nrow(CalfData) == 0) {
        cat("No data found: Calf Forecast\n")
        missing_parts <- c(missing_parts, "Calf Forecast")
      }
      
      incProgress(0.1, detail = "Loading Repro Status data")
      ReproData <- as.data.frame(ReproStatus(HerdID, sqlconn))
      if (nrow(ReproData) == 0) {
        cat("No data found: Repro Status\n")
        missing_parts <- c(missing_parts, "Repro Status")
      }
      
      # Step 3: Clean up missing dam eartag entries
      if (all(is.na(CowMgs$DamEarTagShort)) || all(is.na(CowMgs$DamEarTagLong))) {
        showNotification("Warning: Dam ear tags missing entirely — skipping filtering", type = "warning")
      } else {
        CowMgs <- CowMgs[!is.na(CowMgs$DamEarTagShort) & !is.na(CowMgs$DamEarTagLong), ]
      }
      
      Mggs   <- Mggs[!is.na(Mggs$DamEarTagShort) & !is.na(Mggs$DamEarTagLong), ]
      
      if (length(missing_parts) > 0) {
        showNotification(
          paste("Warning: No data found for", paste(missing_parts, collapse = ", ")),
          type = "warning", duration = 10
        )
      }
      
      # Step 4: Inspect loaded data
      cat("== CowMgs ==\n"); print(head(CowMgs))
      cat("== Mggs ==\n"); print(head(Mggs))
      cat("== Mggs ==\n"); print(head(Mggs2))
      cat("== Calf Forecast ==\n"); print(head(CalfData))
      cat("== Repro Data ==\n"); print(head(ReproData))
      
      # Step 4: Merge CowMgs + Mggs
      DataM <- merge(CowMgs, Mggs, by = c("DamEarTagShort", "DamEarTagLong"), all.x = TRUE)
      DataM$OffspringDateOfBirth <- as.Date(DataM$OffspringDateOfBirth)
      DataM$AgeMonths <- interval(DataM$OffspringDateOfBirth, Sys.Date()) %/% months(1)
      
      cat("== After merging CowMgs + Mggs ==\n"); print(head(DataM))
      
      DataM <- merge(DataM, Mggs2, by.x = "MaternalGrandDamEarTagParsed", by.y = "MaternalGrandDamEarTagShort", all.x = TRUE)
      
      # Step 5: Merge with Calf Forecast
      CalfData$ServiceDate <- as.Date(CalfData$ServiceDate)
      CalfData$DueDate <- as.Date(CalfData$DueDate)
      DataM <- merge(DataM, CalfData, by.x = "OffspringEarTag", by.y = "DamEarTagShort", all.x = TRUE)
      
      cat("== After merging with Calf data ==\n"); print(head(DataM))
      
      # Step 6: Days Carrying Calf
      DataM$DaysCarryingCalf <- NA_integer_
      valid_dates <- !is.na(DataM$ServiceDate)
      
      DataM$DaysCarryingCalf[valid_dates] <- as.integer(
        mapply(function(srv, due) {
          target <- if (!is.na(due) && abs(difftime(due, srv, units = "days")) < abs(difftime(Sys.Date(), srv, units = "days"))) {
            due
          } else {
            Sys.Date()
          }
          as.integer(difftime(target, srv, units = "days"))
        }, DataM$ServiceDate[valid_dates], DataM$DueDate[valid_dates], SIMPLIFY = TRUE)
      )
      
      # Step 7: Merge Repro Status
      ReproData <- ReproData %>%
        arrange(OffspringEarTag, StartDate) %>%
        group_by(OffspringEarTag) %>%
        mutate(
          PregCheck = ifelse(ReproType == 1 & lead(ReproType) == 2, TRUE, FALSE),
          PregCheck = ifelse(is.na(PregCheck), FALSE, PregCheck)
        ) %>%
        ungroup() %>%
        filter(ReproType != 2) %>%
        arrange(OffspringEarTag, desc(StartDate)) %>%
        distinct(OffspringEarTag, .keep_all = TRUE) %>%
        mutate(
          ReproStatus = case_when(
            ReproType == 1 & ResultedInPregnancy == TRUE ~ "PREG",
            ReproType == 1 & ResultedInPregnancy == FALSE & PregCheck == TRUE ~ "OPEN",
            ReproType == 1 & ResultedInPregnancy == FALSE & PregCheck == FALSE ~ "BRED",
            ReproType == 3 ~ "DNB",
            ReproType == 6 ~ "FRSH",
            ReproType == 4 ~ "ABRT",
            TRUE ~ "UNKN"
          )
        )
      DataM <- merge(DataM, ReproData[, c("OffspringEarTag", "ReproStatus")], by = "OffspringEarTag", all.x = TRUE)
      
      # Step 8: Assign missing ReproStatus as CALF or HEIFER based on Age and data availability
      DataM <- DataM %>%
        mutate(
          ReproStatus = case_when(
            !is.na(ReproStatus) ~ ReproStatus,  # Keep existing values
            is.na(ReproStatus) & AgeMonths < 12 & is.na(ServiceDate) ~ "CALF",
            is.na(ReproStatus) & AgeMonths >= 12 & is.na(ServiceDate) ~ "HEIFER",
            TRUE ~ "UNKN"  # Catch-all fallback
          )
        )
      
      # Step 9: Fix Heifers over 28 months → UNKN
      DataM <- DataM %>%
        mutate(
          ReproStatus = ifelse(ReproStatus == "HEIFER" & AgeMonths >= 28, "UNKN", ReproStatus)
        )
      
      # Step 10: Sort and finalize
      DataM <- DataM[, sort(names(DataM))]
      cat("== Final Merged DataM ==\n"); print(head(DataM))
      
      # Step 11: Create single lineage display columns (Sire, MGS, MGGS) - now using ID → HBN → Eartag → Name
      DataM <- DataM %>%
        mutate(
          SireDisplay = case_when(
            !is.na(SireId) ~ as.character(SireId),
            is.na(SireId) & !is.na(SireHBN) ~ as.character(SireHBN),
            is.na(SireId) & is.na(SireHBN) & !is.na(SireEarTag) ~ as.character(SireEarTag),
            TRUE ~ as.character(SireName)
          ),
          MgsDisplay = case_when(
            !is.na(MaternalGrandSireId) ~ as.character(MaternalGrandSireId),
            is.na(MaternalGrandSireId) & !is.na(MaternalGrandSireEarTag) ~ as.character(MaternalGrandSireEarTag),
            TRUE ~ as.character(MaternalGrandSireName)
          ),
          MggsDisplay = case_when(
            !is.na(MaternalGreatGrandSireId) ~ as.character(MaternalGreatGrandSireId),
            is.na(MaternalGreatGrandSireId) & !is.na(MaternalGreatGrandSireEarTag) ~ as.character(MaternalGreatGrandSireEarTag),
            TRUE ~ as.character(MaternalGreatGrandSireName)
          )
        )
      
      # Optional: If you want to prefix the source (ID / Tag / Name), use this version instead:
      # DataM <- DataM %>%
      #   mutate(
      #     SireDisplay = case_when(
      #       !is.na(SireId) ~ paste0("ID: ", SireId),
      #       is.na(SireId) & !is.na(SireEarTag) ~ paste0("Tag: ", SireEarTag),
      #       TRUE ~ paste0("Name: ", SireName)
      #     ),
      #     MgsDisplay = case_when(
      #       !is.na(MaternalGrandSireId) ~ paste0("ID: ", MaternalGrandSireId),
      #       is.na(MaternalGrandSireId) & !is.na(MaternalGrandSireEarTag) ~ paste0("Tag: ", MaternalGrandSireEarTag),
      #       TRUE ~ paste0("Name: ", MaternalGrandSireName)
      #     ),
      #     MggsDisplay = case_when(
      #       !is.na(MaternalGreatGrandSireId) ~ paste0("ID: ", MaternalGreatGrandSireId),
      #       is.na(MaternalGreatGrandSireId) & !is.na(MaternalGreatGrandSireEarTag) ~ paste0("Tag: ", MaternalGreatGrandSireEarTag),
      #       TRUE ~ paste0("Name: ", MaternalGreatGrandSireName)
      #     )
      #   )
      
      DataM <<- DataM
      return(DataM)
    })
  })
  
  # Desired column order and names
  full_display_cols <- c(
    "MaternalGreatGrandSireId", "MaternalGreatGrandSireEarTag", "MggsCountry", "MaternalGreatGrandSireBreed", 
    "MaternalGrandSireId", "MaternalGrandSireEarTag", "MaternalGrandSireName", "MgsCountry", "MaternalGrandSireBreed", 
    "MaternalGrandDamAnimalID", "MaternalGrandDamEarTag", "MaternalGrandDamName", "MaternalGrandDamCountry", "MaternalGrandDamBreed", "MaternalGrandDamDateOfBirth",  
    "SireId", "SireHBN", "SireEarTag", "SireName", "SireCountry", "SireBreed", 
    "DamAnimalID", "DamEarTagLong", "DamName", "DamCountry", "DamBreed", "DamDateOfBirth", 
    "OffspringAnimalID", "OffspringFullEarTag", "OffspringName", "OffspringCountry", "OffspringBreed", "OffspringGender", "OffspringDateOfBirth", "AgeMonths", 
    "OffspringLact", "ReproStatus", "ServiceDate", "DueDate", "DaysCarryingCalf"
    )
  
  full_renamed_cols <- c(
    "Mggs Animal ID", "Mggs Ear Tag", "Mggs Country", "Mggs Breed",
    "Mgs Animal ID", "Mgs Ear Tag", "Mgs Name", "Mgs Country", "Mgs Breed",
    "Mgd Animal ID", "Mgd Ear Tag", "Mgd Name", "Mgd Country", "Mgd Breed", "Mgd Date of Birth",
    "Sire Animal ID", "Sire HBN", "Sire Ear Tag", "Sire Name", "Sire Country", "Sire Breed",
    "Dam Animal ID", "Dam Ear Tag", "Dam Name", "Dam Country", "Dam Breed", "Dam Date Of Birth",
    "Animal ID", "Ear Tag", "Name", "Country", "Breed", "Sex", "Date Of Birth", "Age in Months",
    "Lactation", "Reproduction Status", "Service Date", "Due Date", "Days Carrying Calf"
  )
  
  # GMS Columns Only
  GMS_display_cols <- c(
    "MggsDisplay", "MgsDisplay", "SireDisplay", "OffspringAnimalID", "OffspringCountry",
    "OffspringDateOfBirth", "AgeMonths", "OffspringLact", "ReproStatus", "OffspringBreed")
  GMS_renamed_cols <- c(
    "Maternal Great Grand Sire", "Maternal Grand Sire", "Sire", "Animal ID", "Country",
    "Date Of Birth", "Age in Months", "Lactation", "Reproduction Status", "Breed")
  
  # Render datatable for the advisor
  output$Bar <- DT::renderDT({
    req(input$DataView)
    
    if (input$DataView == "GMS Columns Only") {
      cols_to_show <- GMS_display_cols
      renamed <- GMS_renamed_cols
    } else {
      cols_to_show <- full_display_cols
      renamed <- full_renamed_cols
    }
    
    datatable(
      setNames(x()[, cols_to_show, drop = FALSE], renamed),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Download button for CSV
  output$DownloadData <- downloadHandler(
    filename = function() {
      Cust <- input$GENEadvance
      paste(Cust, "_Downloaded_", format(Sys.time(), "%d_%m_%Y"), ".csv", sep = "")
    },
    content = function(file) {
      if (input$DataView == "GMS Columns Only") {
        cols_to_show <- GMS_display_cols
        renamed <- GMS_renamed_cols
      } else {
        cols_to_show <- full_display_cols
        renamed <- full_renamed_cols
      }
      
      write.csv(
        setNames(x()[, cols_to_show, drop = FALSE], renamed),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
})