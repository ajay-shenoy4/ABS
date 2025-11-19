#source the helped functions to get all the data
source("/data/dairy/USERS/ashenoy/bucketing/code/testhelpers.R")

#function scapi setup in testhelpers.R
# Initializes and returns connections to multiple databases (GMS, GENEadvance, Starburst).
# Also sets up environment paths and reads database configuration from a YAML file
conns <- scapi_setup()

#Get herd list filtering information by connecting to geneadvance and starburst in scapi_setup
herd_list <- dbGetQuery(conns$gsdb, "select * from kapp")

# Vector of file paths, named if you want
#to maintain in the future add new datasets to here
file_paths <- c(
  "2023_06_02" = "GA_CI_Buckets/ci_table_20230602.csv",
  "2023_09_18" = "GA_CI_Buckets/ci_table_20230918.csv",
  "2024_01_29" = "GA_CI_Buckets/ci_table_20240129.csv",
  "2024_04_01" = "GA_CI_Buckets/ci_table_20240401.csv",
  "2024_08_19" = "GA_CI_Buckets/ci_table_20240819.csv",
  "2025_03_20" = "GA_CI_Buckets/ci_table_20250320.csv",
  "2025_08_14" = "GA_CI_Buckets/ci_table_20250814.csv"
)

#function clean and align herd lists in testhelpers.R
# Assume you have `herd_list` already loaded for the reference column order
cleaned_herds <- clean_and_align_herd_lists(file_paths, ref_df = herd_list)

# Access individual dataframes:
#to maintain in the future add new datasets to here
herd_list_2023_06_02 <- cleaned_herds[["2023_06_02"]]
herd_list_2023_09_18 <- cleaned_herds[["2023_09_18"]]
herd_list_2024_01_29 <- cleaned_herds[["2024_01_29"]]
herd_list_2024_04_01 <- cleaned_herds[["2024_04_01"]]
herd_list_2024_08_19 <- cleaned_herds[["2024_08_19"]]
herd_list_2025_03_20 <- cleaned_herds[["2025_03_20"]]
herd_list_2025_08_14 <- cleaned_herds[["2025_08_14"]]

#function process and bucket herds in testhelpers.R
# Assume cleaned_herds is your named list of dataframes from your earlier cleaning function
herd_list_bucketed <- process_and_bucket_herds(cleaned_herds, save_dir = "/data/dairy/USERS/ashenoy/bucketing/csvs")

# Access individual processed dataframes:
#to maintain in the future add new datasets to here
herd_list_bucketed_2023_06_02 <- herd_list_bucketed[["2023_06_02"]]
herd_list_bucketed_2023_09_18 <- herd_list_bucketed[["2023_09_18"]]
herd_list_bucketed_2024_01_29 <- herd_list_bucketed[["2024_01_29"]]
herd_list_bucketed_2024_04_01 <- herd_list_bucketed[["2024_04_01"]]
herd_list_bucketed_2024_08_19 <- herd_list_bucketed[["2024_08_19"]]
herd_list_bucketed_2025_03_20 <- herd_list_bucketed[["2025_03_20"]]
herd_list_bucketed_2025_08_14 <- herd_list_bucketed[["2025_08_14"]]

#helper functions get unassigned or multiple buckets, summarize buckets, summarize by country in testhelpers.R
# --- Function to run summaries and save all CSVs ---
generate_and_save_summaries <- function(bucketed_list, save_dir = ".", id_col = "farmid") {
  # Create dir if needed
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  
  # For unassigned/multi bucket rows per dataframe
  unassigned_multi_list <- lapply(bucketed_list, get_unassigned_or_multiple_buckets, id_col = id_col)
  
  # Summaries by bucket
  bucket_summaries <- lapply(bucketed_list, summarize_buckets)
  
  # Summaries by country
  country_summaries <- lapply(bucketed_list, summarize_by_country)
  
  # Write individual CSVs
  for (name in names(bucketed_list)) {
    write.csv(bucket_summaries[[name]], file = file.path(save_dir, paste0("bucket_summary_", name, ".csv")), row.names = FALSE)
    write.csv(country_summaries[[name]], file = file.path(save_dir, paste0("country_summary_", name, ".csv")), row.names = FALSE)
    write.csv(unassigned_multi_list[[name]], file = file.path(save_dir, paste0("unassigned_multi_", name, ".csv")), row.names = FALSE)
  
    # Assign each individual summary DF to global env
    assign(paste0("bucket_summary_", name), bucket_summaries[[name]], envir = .GlobalEnv)
    assign(paste0("country_summary_", name), country_summaries[[name]], envir = .GlobalEnv)
    assign(paste0("unassigned_multi_", name), unassigned_multi_list[[name]], envir = .GlobalEnv)
  }
  
  # Combined summaries with dates parsed
  combined_bucket_summary <- bind_rows(bucket_summaries, .id = "date") %>% mutate(date = ymd(date))
  combined_country_summary <- bind_rows(country_summaries, .id = "date") %>% mutate(date = ymd(date))
  combined_herds <- bind_rows(bucketed_list, .id = "date") %>% mutate(date = ymd(date))
  
  # Save combined files
  write.csv(combined_bucket_summary, file.path(save_dir, "combined_bucket_summary.csv"), row.names = FALSE)
  write.csv(combined_country_summary, file.path(save_dir, "combined_country_summary.csv"), row.names = FALSE)
  write.csv(combined_herds, file.path(save_dir, "combined_herd_list_bucketed.csv"), row.names = FALSE)
  
  # Return all results as a list for further use
  list(
    unassigned_multi = unassigned_multi_list,
    bucket_summary = bucket_summaries,
    country_summary = country_summaries,
    combined_bucket_summary = combined_bucket_summary,
    combined_country_summary = combined_country_summary,
    combined_herds = combined_herds
  )
}

# Assume your herd_list_bucketed is your named list of bucketed dfs, e.g.:
#to maintain in the future add new datasets to here
herd_list_bucketed <- list(
  "2023_06_02" = herd_list_bucketed_2023_06_02,
  "2023_09_18" = herd_list_bucketed_2023_09_18,
  "2024_01_29" = herd_list_bucketed_2024_01_29,
  "2024_04_01" = herd_list_bucketed_2024_04_01,
  "2024_08_19" = herd_list_bucketed_2024_08_19,
  "2025_03_20" = herd_list_bucketed_2025_03_20,
  "2025_08_14" = herd_list_bucketed_2025_08_14
)

# Run summaries and save CSVs
results <- generate_and_save_summaries(herd_list_bucketed, save_dir = "/data/dairy/USERS/ashenoy/bucketing/csvs", id_col = "farmid")

# You can access individual results like:
results$unassigned_multi[["2023_06_02"]]
results$bucket_summary[["2023_06_02"]]
results$country_summary[["2023_06_02"]]

# If you haven't read them in:
#to maintain in the future add new datasets to here
herds_2023_06_02 <- read_csv("bucketing/csvs/herd_list_bucketed_2023_06_02.csv")
herds_2023_09_18 <- read_csv("bucketing/csvs/herd_list_bucketed_2023_09_18.csv")
herds_2024_01_29 <- read_csv("bucketing/csvs/herd_list_bucketed_2024_01_29.csv")
herds_2024_04_01 <- read_csv("bucketing/csvs/herd_list_bucketed_2024_04_01.csv")
herds_2024_08_19 <- read_csv("bucketing/csvs/herd_list_bucketed_2024_08_19.csv")
herds_2025_03_20 <- read_csv("bucketing/csvs/herd_list_bucketed_2025_03_20.csv")
herds_2025_08_14 <- read_csv("bucketing/csvs/herd_list_bucketed_2025_08_14.csv")
colnames(herds_2023_06_02)

#to get the proportions per country
#to maintain in the future add new datasets to here
herd_data_list <- list(
  "2023-06-02" = herds_2023_06_02,
  "2023-09-18" = herds_2023_09_18,
  "2024-01-29" = herds_2024_01_29,
  "2024-04-01" = herds_2024_04_01,
  "2024-08-19" = herds_2024_08_19,
  "2025-03-20" = herds_2025_03_20,
  "2025-08-14" = herds_2025_08_14
)
#uses generate bucket trends function in testhelpers.R
bucket_trends <- generate_bucket_trends(herd_data_list)

#to make the bucket proportions of the whole world, uses plot bucket trends function in testhelpers.R
plot_bucket_trends(bucket_trends)

# Call the function to get bucket proportions in each country, function in testhelpers.R
plot_bucket_trends_by_country(herd_data_list)

#graph buckets counts and proportions across countries
plot_herd_count_bucket_trends(herd_data_list)

## Creates a horizontal stacked bar chart showing total herd size per country, broken down by bucket.
#function in testhelpers.R
#to maintain in the future add new datasets to here
#plot_herd_by_country_and_bucket(herds_2023_06_02, "2023_06_02")
#plot_herd_by_country_and_bucket(herds_2023_09_18, "2023_09_18")
#plot_herd_by_country_and_bucket(herds_2024_01_29, "2024_01_29")
#plot_herd_by_country_and_bucket(herds_2024_04_01, "2024_04_01")
#plot_herd_by_country_and_bucket(herds_2024_08_19, "2024_08_19")
#plot_herd_by_country_and_bucket(herds_2025_03_20, "2025_03_20")
#plot_herd_by_country_and_bucket(herds_2025_08_14, "2025_08_14")

# Generates a horizontal bar plot showing number of herds per country, grouped by genetic bucket.
#function in testhelpers.R
#to maintain in the future add new datasets to here
#plot_herd_count_by_country_and_bucket(herds_2023_06_02, "2023_06_02")
#plot_herd_count_by_country_and_bucket(herds_2023_09_18, "2023_09_18")
#plot_herd_count_by_country_and_bucket(herds_2024_01_29, "2024_01_29")
#plot_herd_count_by_country_and_bucket(herds_2024_04_01, "2024_04_01")
#plot_herd_count_by_country_and_bucket(herds_2024_08_19, "2024_08_19")
#plot_herd_count_by_country_and_bucket(herds_2025_03_20, "2025_03_20")
#plot_herd_count_by_country_and_bucket(herds_2025_08_14, "2025_08_14")

# Create a named list of bucket summary data with dates as names
#to maintain in the future add new datasets to here
summary_list <- list(
  `2023-06-02` = bucket_summary_2023_06_02,
  `2023-09-18` = bucket_summary_2023_09_18,
  `2024-01-29` = bucket_summary_2024_01_29,
  `2024-04-01` = bucket_summary_2024_04_01,
  `2024-08-19` = bucket_summary_2024_08_19,
  `2025-03-20` = bucket_summary_2025_03_20,
  `2025-08-14` = bucket_summary_2025_08_14
)

# graphs the scores of each bucket, function in testhelpers.R
plot_historical_bucket_scores(summary_list)

#makes a bar graph of the bucket values we input into the code, function in testhelpers.R
plot_trait_ranges(bucket_rules)

#combining bucketed herds data
#to maintain in the future add new datasets to here
combined_herds <- bind_rows(
  `2023-06-02` = herd_list_bucketed_2023_06_02,
  `2023-09-18` = herd_list_bucketed_2023_09_18,
  `2024-01-29` = herd_list_bucketed_2024_01_29,
  `2024-04-01` = herd_list_bucketed_2024_04_01,
  `2024-08-19` = herd_list_bucketed_2024_08_19,
  `2025-03-20` = herd_list_bucketed_2025_03_20,
  `2025-08-14` = herd_list_bucketed_2025_08_14,
  .id = "date"
) %>%
  mutate(
    date = ymd(date)
  )

#makes a bar graph of the actual bucket values, function in testhelpers.R
plot_bucket_trait_ranges(combined_herds)

#add labels per dataset
#to maintain in the future add new datasets to here
herds_2023_06_02 <- read_csv("bucketing/csvs/herd_list_bucketed_2023_06_02.csv") %>% mutate(date = as.Date("2023-06-02"))
herds_2023_09_18 <- read_csv("bucketing/csvs/herd_list_bucketed_2023_09_18.csv") %>% mutate(date = as.Date("2023-09-18"))
herds_2024_01_29 <- read_csv("bucketing/csvs/herd_list_bucketed_2024_01_29.csv") %>% mutate(date = as.Date("2024-01-29"))
herds_2024_04_01 <- read_csv("bucketing/csvs/herd_list_bucketed_2024_04_01.csv") %>% mutate(date = as.Date("2024-04-01"))
herds_2024_08_19 <- read_csv("bucketing/csvs/herd_list_bucketed_2024_08_19.csv") %>% mutate(date = as.Date("2024-08-19"))
herds_2025_03_20 <- read_csv("bucketing/csvs/herd_list_bucketed_2025_03_20.csv") %>% mutate(date = as.Date("2025-03-20"))
herds_2025_08_14 <- read_csv("bucketing/csvs/herd_list_bucketed_2025_08_14.csv") %>% mutate(date = as.Date("2025-08-14"))

selected_countries <- c("Brazil", "Italy", "USA", "UK", "Chile", "Spain", "Canada", "Argentina", "Mexico")

#combine
#to maintain in the future add new datasets to here
all_herds <- bind_rows(
  herds_2023_06_02,
  herds_2023_09_18,
  herds_2024_01_29,
  herds_2024_04_01,
  herds_2024_08_19,
  herds_2025_03_20,
  herds_2025_08_14
)

#shows how production traits change by the countries we selected
plot_production_trait_trends(all_herds, selected_countries,
                             output_file = "/data/dairy/USERS/ashenoy/bucketing/plots/production_trait_trends_by_country.png")

#shows how health traits change by the countries we selected
plot_health_trait_trends(all_herds, selected_countries,
                         output_dir = "/data/dairy/USERS/ashenoy/bucketing/plots/")

#shows how conformation traits change by the countries we selected
plot_conformation_trait_trends(all_herds, selected_countries,
                         output_dir = "/data/dairy/USERS/ashenoy/bucketing/plots/")
