# Initializes and returns connections to multiple databases (GMS, GENEadvance, Starburst).
# Also sets up environment paths and reads database configuration from a YAML file.
scapi_setup <- function() {
  suppressPackageStartupMessages({
    library(tidyverse)
    library(dplyr)
    library(ggplot2)
    library(readr)
    library(tidyr)
    library(Rscapi)
    library(tibble)
    library(purrr)
    library(dplyr)
    library(tidyverse)
    library(lubridate)
    library(xlsx)
    library(odbc)
    library(yaml)
    library(mailR)
    library(DBI)
    library(RJDBC)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(purrr)
    library(forcats)
  })
  
  # Adjust PATH env var
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste("/usr/local/bin", old_path, sep = ":"))
  
  # Config names
  conf_name_gms <- Sys.getenv('R_CONFIG_GMSDB', unset = 'palette_gms')
  conf_name_sc <- Sys.getenv('R_CONFIG_SCDB', unset = 'geneadvance_sql_db')
  config_file <- Sys.getenv('R_CONFIG_FILE', unset = '~/.scapi/config.yml')
  
  # Read config yaml file
  db_yaml <- read_yaml(config_file)
  db_cfg_gms <- db_yaml[[conf_name_gms]]
  db_cfg_sc <- db_yaml[[conf_name_sc]]
  
  # Connect to GMS SQL Server
  gms <- dbConnect(odbc(),
                   Driver = db_cfg_gms$driver,
                   Server = db_cfg_gms$server,
                   Database = db_cfg_gms$database,
                   UID = db_cfg_gms$uid,
                   PWD = db_cfg_gms$pwd,
                   Port = db_cfg_gms$port)
  
  # Connect to GENEadvance SQL Server
  gsdb <- dbConnect(odbc(),
                    Driver = db_cfg_sc$driver,
                    Server = db_cfg_sc$server,
                    Database = db_cfg_sc$database,
                    UID = db_cfg_sc$uid,
                    PWD = db_cfg_sc$pwd,
                    Port = db_cfg_sc$port)
  
  # Connect to starburst (GSD)
  config_dir <- '~/.scapi'
  config_filename <- normalizePath(file.path(config_dir, 'config.yml'), mustWork = TRUE)
  full_yaml_file <- read_yaml(config_filename)
  cfg <- full_yaml_file$starburst
  rm(full_yaml_file)
  
  starburst <- DBI::dbConnect(
    drv = RJDBC::JDBC(
      "io.trino.jdbc.TrinoDriver",
      "/data/apps/trino/jdbc/starburst",
      "`"
    ),
    url = "jdbc:trino://starburst.genusplc.com:443",
    user = cfg$user,
    password = cfg$password,
    SSL = 'true'
  )
  
  # Return all connections in a list
  return(list(
    gms = gms,
    gsdb = gsdb,
    starburst = starburst
  ))
}

# Loads, filters, and aligns multiple herd CSVs by removing invalid farm IDs/countries and standardizing column order.
clean_and_align_herd_lists <- function(
    file_paths,       # Named character vector or list of CSV file paths
    bad_farmids = c(99999999, 88008131, 87654321, 77777777),
    bad_countries = c("test", "other"),
    ref_df             # Reference dataframe to get desired column order
) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
    library(tidyr)
    library(lubridate)
    library(ggplot2)
  })
  
  # Step 1: Read CSVs and convert colnames to lowercase
  df_list <- map(file_paths, ~ {
    df <- read.csv(.x)
    colnames(df) <- tolower(colnames(df))
    df
  })
  
  # Step 2: Clean data frames by removing bad farmids and countries
  df_list_cleaned <- map(df_list, ~ .x %>%
                           filter(!(farmid %in% bad_farmids),
                                  !(tolower(country) %in% bad_countries)))
  
  # Step 3: Get union of all columns across all dfs
  all_columns <- unique(unlist(map(df_list_cleaned, colnames)))
  
  # Step 4: Add missing columns (with NA) to each df and reorder columns
  df_list_aligned <- map(df_list_cleaned, function(df) {
    missing_cols <- setdiff(all_columns, colnames(df))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        df[[col]] <- NA
      }
    }
    df <- df[all_columns]
    df
  })
  
  # Step 5: Reorder columns to match reference dataframe's column order (only intersecting columns)
  desired_col_order <- colnames(ref_df)
  reorder_columns <- function(df, col_order) {
    common_cols <- intersect(col_order, colnames(df))
    df[, common_cols, drop = FALSE]
  }
  
  df_list_final <- map(df_list_aligned, reorder_columns, col_order = desired_col_order)
  
  # Return named list
  return(df_list_final)
}

# --- Trait groups ---
production_traits   <- c("milk", "fat", "pro")
fitness_traits      <- c("ahi", "liv", "pl", "scs", "ccr", "dpr", "hcr", "dce", "dsb", "sce", "ssb",
                         "fs", "rfi", "mast", "met", "ket")
conformation_traits <- c("bwc", "flc", "ptat", "udc", "bde", "rpa", "str", "dfm", "sta", "trw", "fls", "fta", 
                         "rlr", "rls", "fua", "ruh", "ruw", "ucl", "udp", "ftp", "rtp", "tlg")

# --- Bucket rules (global) ---
bucket_rules <- list(
  list(Production = c(80,100), Fitness = c(0,20),  Conformation = c(0,20),  Bucket = "NM$like"),
  list(Production = c(35,55), Fitness = c(35,55), Conformation = c(0,15),  Bucket = "NM$like"),
  list(Production = c(35,50), Fitness = c(20,45), Conformation = c(16,20), Bucket = "TPIlike"),
  list(Production = c(15,50), Fitness = c(10,45), Conformation = c(15,45), Bucket = "PHC"),
  list(Production = c(0,45),  Fitness = c(50,100),Conformation = c(0,30),  Bucket = "HEALTH"),
  list(Production = c(50,80), Fitness = c(0,45),  Conformation = c(0,45),  Bucket = "PRODUCTION"),
  list(Production = c(0,45),  Fitness = c(0,40),  Conformation = c(50,100),Bucket = "CONFORMATION")
)

# Cleans AHI values, scores herds by production/fitness/conformation traits, assigns them to defined buckets, and saves results.
process_and_bucket_herds <- function(df_list, save_dir = ".", prefix = "herd_list_bucketed_") {
  
  # --- Helper: clean AHI column ---
  clean_ahi_column <- function(df) {
    df$ahi[df$ahi == "NULL"] <- NA
    df$ahi <- as.numeric(df$ahi)
    df$ahi[is.na(df$ahi)] <- 0
    return(df)
  }
  
  # --- Distance to bounding box ---
  compute_distance_to_box <- function(point, rule_box) {
    prod <- point[1]; fit <- point[2]; conf <- point[3]
    clipped_prod <- max(rule_box$Production[1], min(prod, rule_box$Production[2]))
    clipped_fit <- max(rule_box$Fitness[1], min(fit, rule_box$Fitness[2]))
    clipped_conf <- max(rule_box$Conformation[1], min(conf, rule_box$Conformation[2]))
    sqrt((prod - clipped_prod)^2 + (fit - clipped_fit)^2 + (conf - clipped_conf)^2)
  }
  
  assign_closest_bucket <- function(prod, fit, conf) {
    distances <- sapply(bucket_rules, function(rule) {
      compute_distance_to_box(c(prod, fit, conf), rule)
    })
    closest_index <- which.min(distances)
    return(bucket_rules[[closest_index]]$Bucket)
  }
  
  # --- Bucket assignment with special cases and default ---
  assign_bucket <- function(prod, fit, conf) {
    if (prod >= 100) return("PRODUCTION")
    if (fit  >= 100) return("HEALTH")
    if (conf >= 100) return("CONFORMATION")
    
    for (rule in bucket_rules) {
      if (prod >= rule$Production[1] && prod <= rule$Production[2] &&
          fit  >= rule$Fitness[1]    && fit  <= rule$Fitness[2] &&
          conf >= rule$Conformation[1] && conf <= rule$Conformation[2]) {
        return(rule$Bucket)
      }
    }
    return("Unassigned")
  }
  
  # --- Score and bucket function ---
  score_and_bucket <- function(df) {
    production_cols <- intersect(production_traits, colnames(df))
    fitness_cols <- intersect(fitness_traits, colnames(df))
    conformation_cols <- intersect(conformation_traits, colnames(df))
    
    df <- df %>%
      rowwise() %>%
      mutate(
        production_score = sum(abs(c_across(all_of(production_cols))), na.rm = TRUE),
        fitness_score = sum(abs(c_across(all_of(fitness_cols))), na.rm = TRUE),
        conformation_score = sum(abs(c_across(all_of(conformation_cols))), na.rm = TRUE)
      ) %>%
      filter((production_score + fitness_score + conformation_score) > 10) %>%
      mutate(
        bucket = assign_bucket(production_score, fitness_score, conformation_score)
      ) %>%
      mutate(
        bucket = if_else(
          bucket == "Unassigned",
          assign_closest_bucket(production_score, fitness_score, conformation_score),
          bucket
        )
      ) %>%
      ungroup()
    
    return(df)
  }
  
  # Process each df in the list
  processed_list <- lapply(names(df_list), function(name) {
    df <- df_list[[name]]
    df <- clean_ahi_column(df)
    df <- score_and_bucket(df)
    
    # Save CSV
    write.csv(df, file = file.path(save_dir, paste0(prefix, name, ".csv")), row.names = FALSE)
    
    return(df)
  })
  names(processed_list) <- names(df_list)
  
  return(processed_list)
}

# Identifies herds that are either unassigned to a bucket or appear in multiple buckets for the same farm ID.
get_unassigned_or_multiple_buckets <- function(df, id_col = "farmid") {
  unassigned_rows <- df %>% filter(bucket == "Unassigned")
  
  multi_bucket_ids <- df %>%
    group_by(across(all_of(id_col))) %>%
    summarise(n_buckets = n_distinct(bucket), .groups = "drop") %>%
    filter(n_buckets > 1) %>%
    pull(!!sym(id_col))
  
  multi_bucket_rows <- df %>% filter((!!sym(id_col)) %in% multi_bucket_ids)
  
  combined <- bind_rows(unassigned_rows, multi_bucket_rows) %>% distinct()
  
  return(combined)
}

# Produces summary statistics (size, counts, trait stats) grouped by bucket.
summarize_buckets <- function(df) {
  df %>%
    group_by(bucket) %>%
    summarise(
      countries      = paste(unique(country), collapse = ", "),
      total_herdsize = sum(herdsize, na.rm = TRUE),
      avg_herdsize   = mean(herdsize, na.rm = TRUE),
      n_herds        = n(),
      total_heifers  = sum(heif, na.rm = TRUE),
      total_conv     = sum(conv, na.rm = TRUE),
      total_sexed    = sum(sexed, na.rm = TRUE),
      total_beef     = sum(beef, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      df %>%
        group_by(bucket) %>%
        summarise(across(where(is.numeric), list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE)
        ), .names = "{.col}_{.fn}"), .groups = "drop"),
      by = "bucket"
    )
}

# Summarizes herd counts, sizes, and trait stats grouped by country.
summarize_by_country <- function(df) {
  df %>%
    group_by(country) %>%
    summarise(
      buckets        = paste(unique(bucket), collapse = ", "),
      total_herdsize = sum(herdsize, na.rm = TRUE),
      avg_herdsize   = mean(herdsize, na.rm = TRUE),
      n_herds        = n(),
      total_heifers  = sum(heif, na.rm = TRUE),
      total_conv     = sum(conv, na.rm = TRUE),
      total_sexed    = sum(sexed, na.rm = TRUE),
      total_beef     = sum(beef, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      df %>%
        group_by(country) %>%
        summarise(across(where(is.numeric), list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE)
        ), .names = "{.col}_{.fn}"), .groups = "drop"),
      by = "country"
    )
}

#FUNCTIONS FOR GRAPHING START HERE

# Calculates percentage of herds per bucket for a single dataframe and assigns a date label.
process_file <- function(df, date_label) {
  df %>%
    count(bucket) %>%
    mutate(
      total = sum(n),
      percent = 100 * n / total,
      date = as.Date(date_label)
    ) %>%
    select(date, bucket, percent, n)
}

# Combines processed bucket proportions across timepoints into a single long-format trend table.
generate_bucket_trends <- function(named_dfs) {
  library(dplyr)
  library(tibble)
  
  process_file <- function(df, date_label) {
    df %>%
      count(bucket) %>%
      mutate(
        total = sum(n),
        percent = 100 * n / total,
        date = as.Date(date_label)
      ) %>%
      select(date, bucket, percent, n)
  }
  
  # Process all dataframes in the named list
  bucket_trends <- purrr::imap_dfr(named_dfs, process_file)
  
  # Custom vjust logic
  bucket_trends <- bucket_trends %>%
    mutate(
      vjust = case_when(
        date == as.Date("2024-04-01") & bucket == "NM$like" ~ -1.5,
        date == as.Date("2024-01-29") & bucket == "NM$like" ~ -1.2,
        date == as.Date("2023-06-02") & bucket == "PHC" ~ 1.4,
        date == as.Date("2023-09-18") & bucket == "TPIlike" ~ -1,
        TRUE ~ -0.5
      ),
      hjust = -0.3
    )
  
  return(bucket_trends)
}

# Plots temporal trends of herd proportions by bucket using a line chart with labels.
plot_bucket_trends <- function(bucket_trends, output_file = "/data/dairy/USERS/ashenoy/bucketing/plots/bucket_proportions_world.png") {
  library(ggplot2)
  library(scales)
  
  w <- ggplot(bucket_trends, aes(x = date, y = percent, color = bucket)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    geom_text(aes(label = n, vjust = vjust, hjust = hjust), size = 4, show.legend = FALSE) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_x_date(expand = expansion(mult = c(0.02, 0.1))) +
    labs(
      title = "",
      x = "Date",
      y = "Percentage of Herds",
      color = ""
    ) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom")
  
  print(w)  # Optional if you want to view it in RStudio
  
  ggsave(output_file, plot = w, width = 8, height = 7, dpi = 300)
}

# Creates faceted line plots showing herd bucket proportions by country, using both count and herd size.
plot_bucket_trends_by_country <- function(herd_data_list, 
                                          selected_countries = c("Brazil", "Italy", "USA", "UK", "Chile", "Spain", "Canada", "Argentina", "Mexico"),
                                          output_file = "/data/dairy/USERS/ashenoy/bucketing/plots/bucket_proportions_count_and_size.png") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  
  # Internal helper to process each file
  process_file <- function(df, date_label) {
    df %>%
      filter(!country %in% c("test", "Other")) %>%
      group_by(date = as.Date(date_label), country, bucket) %>%
      summarise(
        n = n(),
        total_herdsize = sum(herdsize, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(date, country) %>%
      mutate(
        percent_count = 100 * n / sum(n),
        percent_size  = 100 * total_herdsize / sum(total_herdsize)
      ) %>%
      ungroup()
  }
  
  # Step 1: Combine all processed data
  bucket_trends_country <- purrr::map2_dfr(herd_data_list, names(herd_data_list), process_file)
  
  # Step 2: Filter countries
  bucket_trends_country <- bucket_trends_country %>%
    filter(country %in% selected_countries)
  
  # Step 3: Pivot to long format
  bucket_trends_long <<- bucket_trends_country %>%
    pivot_longer(
      cols = c(percent_count, percent_size),
      names_to = "metric",
      values_to = "percent"
    ) %>%
    mutate(
      metric = recode(metric,
                      percent_count = "Herd Count",
                      percent_size = "Herd Size")
    )
  
  # Step 4: Plot
  z <- ggplot(bucket_trends_long, 
              aes(x = date, y = percent, 
                  color = bucket, 
                  linetype = metric, 
                  group = interaction(bucket, metric))) +
    geom_line(size = 1.1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_linetype_manual(values = c("Herd Count" = "dashed", "Herd Size" = "solid")) +
    labs(
      title = "",
      x = "Date",
      y = "Percentage",
      color = "Bucket",
      linetype = "Metric"
    ) +
    facet_wrap(~ country) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
  
  print(z)
  ggsave(output_file, plot = z, width = 14, height = 8, dpi = 300)
}

# Visualizes temporal changes in herd bucket counts by country using faceted line plots.
plot_herd_count_bucket_trends <- function(herd_data_list, 
                                          selected_countries = c("USA", "Italy", "Brazil", "Mexico", "UK", "Chile", "Argentina", "Spain", "Canada"),
                                          output_file = "/data/dairy/USERS/ashenoy/bucketing/plots/bucket_proportions_by_country_count.png") {
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(purrr)
  
  # Helper function to process individual files
  process_file_count <- function(df, date_label) {
    df %>%
      filter(!country %in% c("test", "Other"),
             country %in% selected_countries) %>%  # filter early
      mutate(country = as.character(country)) %>%
      group_by(date = as.Date(date_label), country, bucket) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(date, country) %>%
      mutate(percent = 100 * n / sum(n)) %>%
      ungroup()
  }
  
  # Step 1: Apply to all inputs
  bucket_trends_country_count <- map2_dfr(herd_data_list, names(herd_data_list), process_file_count)
  
  # Step 2: Filter selected countries
  bucket_trends_country_count <- bucket_trends_country_count %>%
    filter(country %in% selected_countries) %>%
    mutate(country = factor(country, levels = selected_countries))
  
  # Step 3: Plot
  z_count <- ggplot(bucket_trends_country_count, aes(x = date, y = percent, color = bucket)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    labs(
      title = "",
      x = "Date",
      y = "Percentage of Herds",
      color = "Bucket"
    ) +
    facet_wrap(~ country) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
  
  print(z_count)
  
  # Step 4: Save
  ggsave(output_file, plot = z_count, width = 14, height = 8, dpi = 300)
}

# Creates a horizontal stacked bar chart showing total herd size per country, broken down by bucket.
plot_herd_by_country_and_bucket <- function(data, file_name_suffix) {
  formatted_date <- format(as.Date(file_name_suffix, format = "%Y_%m_%d"), "%m/%d/%Y")
  
  country_split <- data %>%
    group_by(bucket, country) %>%
    summarise(
      n_herds = n(),
      total_herdsize = sum(herdsize, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(country) %>%
    mutate(
      country_total = sum(total_herdsize),
      percent = 100 * total_herdsize / country_total
    ) %>%
    ungroup()
  
  p <- ggplot(country_split, aes(x = reorder(country, -country_total), y = total_herdsize, fill = bucket)) +
    geom_col(position = "stack") +
    coord_flip() +
    geom_text(
      data = filter(country_split, percent > 10, country %in% c()),
      aes(label = paste0(round(percent), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 4
    ) +
    labs(
      title = paste("Total Herd Size by Country -", formatted_date),
      x = "Country",
      y = "Total Herd Size"
    ) +
    scale_fill_discrete(name = NULL) +  # Remove "bucket" legend title
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
  
  ggsave(
    filename = paste0("/data/dairy/USERS/ashenoy/bucketing/plots/herd_size_by_country_and_bucket_", file_name_suffix, ".png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  #print(p)
}


# Generates a horizontal bar plot showing number of herds per country, grouped by genetic bucket.
plot_herd_count_by_country_and_bucket <- function(data, file_name_suffix) {
  formatted_date <- format(as.Date(file_name_suffix, format = "%Y_%m_%d"), "%m/%d/%Y")
  
  country_split <- data %>%
    group_by(bucket, country) %>%
    summarise(
      n_herds = n(),
      .groups = "drop"
    ) %>%
    group_by(country) %>%
    mutate(
      country_total = sum(n_herds),
      percent = 100 * n_herds / country_total
    ) %>%
    ungroup()
  
  p <- ggplot(country_split, aes(x = reorder(country, -country_total), y = n_herds, fill = bucket)) +
    geom_col(position = "stack") +
    coord_flip() +
    geom_text(
      data = filter(country_split, percent > 100),  # only label segments > 10%
      aes(label = paste0(round(percent), "%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 4
    ) +
    labs(
      title = paste("Number of Herds by Country -", formatted_date),
      x = "Country",
      y = "Number of Herds"
    ) +
    scale_fill_discrete(name = NULL) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
  
  ggsave(
    filename = paste0("/data/dairy/USERS/ashenoy/bucketing/plots/herd_count_by_country_and_bucket_", file_name_suffix, ".png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  #print(p)
}

#graph the scores of each bucket
plot_historical_bucket_scores <- function(
    summary_list,
    output_file = "/data/dairy/USERS/ashenoy/bucketing/plots/historical_means_medians.png",
    score_cols = c(
      "production_score_mean", "production_score_median",
      "fitness_score_mean", "fitness_score_median",
      "conformation_score_mean", "conformation_score_median"
    )
) {
  # Load required libraries
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(stringr)
    library(ggplot2)
  })
  
  # Combine all summary dataframes into one with a 'date' column
  combined_data <- imap_dfr(summary_list, ~ mutate(.x, date = as.Date(.y)))
  
  # Reshape data to long format for easier plotting
  long_data <- combined_data %>%
    select(bucket, date, all_of(score_cols)) %>%
    pivot_longer(
      cols = -c(bucket, date),
      names_to = c("score_type", "stat"),
      names_pattern = "(.*)_score_(.*)",
      values_to = "value"
    )
  
  # Plot
  w <- ggplot(long_data, aes(x = date, y = value, color = bucket, linetype = stat, group = interaction(bucket, stat))) +
    geom_line() +
    geom_point() +
    facet_wrap(~ score_type, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Bucket Score Trends Over Time",
      x = "Date",
      y = "Score",
      color = "Bucket",
      linetype = "Statistic"
    )
  
  # Display plot and save to file
  print(w)
  ggsave(output_file, plot = w, width = 10, height = 6, dpi = 300)
}

#makes a bar graph of the bucket values we input into the code, function in testhelpers.R
plot_trait_ranges <- function(bucket_rules, 
                              save_path = "/data/dairy/USERS/ashenoy/bucketing/plots/trait_ranges_by_index.png",
                              width = 8, height = 7, dpi = 300) {
  
  range_df <- map_dfr(bucket_rules, function(rule) {
    tibble(
      Index = rule$Bucket,
      Trait = c("Production", "Fitness", "Conformation"),
      Min = c(rule$Production[1], rule$Fitness[1], rule$Conformation[1]),
      Max = c(rule$Production[2], rule$Fitness[2], rule$Conformation[2])
    )
  })
  
  # Set factor order for Traits
  range_df$Trait <- factor(range_df$Trait, levels = c("Production", "Fitness", "Conformation"))
  
  # Calculate position offsets for better spacing
  range_df <- range_df %>%
    mutate(
      Index = factor(Index, levels = unique(Index)),
      x_group = as.numeric(Index),
      x_numeric = x_group * 1.5,
      trait_offset = case_when(
        Trait == "Production" ~ -0.25,
        Trait == "Fitness" ~ 0,
        Trait == "Conformation" ~ 0.25
      ),
      xmin = x_numeric + trait_offset - 0.1,
      xmax = x_numeric + trait_offset + 0.1
    )
  
  # Build the plot
  p <- ggplot(range_df) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = Min, ymax = Max, fill = Trait), color = "black") +
    scale_x_continuous(
      breaks = unique(range_df$x_numeric),
      labels = levels(range_df$Index),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    labs(
      title = "",
      x = "Custom Index",
      y = "Score Range (%)",
      fill = ""
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  print(p)
  
  # Save plot to file
  ggsave(filename = save_path, plot = p, width = width, height = height, dpi = dpi)
}

#makes a bar graph of the actual bucket values, function in testhelpers.R
plot_bucket_trait_ranges <- function(df, output_dir = "/data/dairy/USERS/ashenoy/bucketing/plots/") {
  library(tidyverse)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # --- Step 1: Clamp out-of-range values and identify invalid rows ---
  bucket_stats_raw <- df %>%
    group_by(bucket) %>%
    summarise(
      Production_min = min(production_score, na.rm = TRUE),
      Production_max = max(production_score, na.rm = TRUE),
      Fitness_min = min(fitness_score, na.rm = TRUE),
      Fitness_max = max(fitness_score, na.rm = TRUE),
      Conformation_min = min(conformation_score, na.rm = TRUE),
      Conformation_max = max(conformation_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  bucket_stats_clamped <<- bucket_stats_raw %>%
    mutate(
      Production_min = pmax(0, pmin(100, Production_min)),
      Production_max = pmax(0, pmin(100, Production_max)),
      Fitness_min = pmax(0, pmin(100, Fitness_min)),
      Fitness_max = pmax(0, pmin(100, Fitness_max)),
      Conformation_min = pmax(0, pmin(100, Conformation_min)),
      Conformation_max = pmax(0, pmin(100, Conformation_max))
    )
  
  invalid_rows <- df %>%
    filter(
      production_score < 0 | production_score > 100 |
        fitness_score < 0 | fitness_score > 100 |
        conformation_score < 0 | conformation_score > 100
    )
  if (nrow(invalid_rows) > 0) {
    message("Invalid rows found:")
    print(invalid_rows)
  }
  
  # --- Step 2: Manually define bucket rules (can parameterize if needed) ---
  #Need to update this every time with the actual values
  rule_df <- tibble::tibble(
    bucket = c("NM$like", "TPIlike", "PHC", "HEALTH", "PRODUCTION", "CONFORMATION"),
    Production_min   = c(34, 35, 15, 0, 50, 15),
    Production_max   = c(95, 50, 50, 45, 100, 20),
    Fitness_min      = c(5, 20, 10, 50, 0, 10),
    Fitness_max      = c(55, 46, 50, 100, 49, 30),
    Conformation_min = c(0, 16, 15, 0, 0, 55),
    Conformation_max = c(20, 20, 45, 30, 33, 70)
  )
  
  bucket_rules_new <- rule_df %>%
    pmap(function(bucket, Production_min, Production_max,
                  Fitness_min, Fitness_max,
                  Conformation_min, Conformation_max) {
      list(
        Production = c(Production_min, Production_max),
        Fitness = c(Fitness_min, Fitness_max),
        Conformation = c(Conformation_min, Conformation_max),
        Bucket = bucket
      )
    })
  
  # --- Step 3: Convert to plotting dataframe ---
  range_df <- map_dfr(bucket_rules_new, function(rule) {
    tibble(
      Index = rule$Bucket,
      Trait = c("Production", "Fitness", "Conformation"),
      Min = c(rule$Production[1], rule$Fitness[1], rule$Conformation[1]),
      Max = c(rule$Production[2], rule$Fitness[2], rule$Conformation[2])
    )
  })
  
  range_df$Trait <- factor(range_df$Trait, levels = c("Production", "Fitness", "Conformation"))
  
  range_df <- range_df %>%
    mutate(
      Index = factor(Index, levels = unique(Index)),
      x_group = as.numeric(Index),
      x_numeric = x_group * 1.5,
      trait_offset = case_when(
        Trait == "Production" ~ -0.25,
        Trait == "Fitness" ~ 0,
        Trait == "Conformation" ~ 0.25
      ),
      xmin = x_numeric + trait_offset - 0.1,
      xmax = x_numeric + trait_offset + 0.1
    )
  
  # --- Step 4: Plot ---
  g <- ggplot(range_df) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = Min, ymax = Max, fill = Trait),
      color = "black"
    ) +
    scale_x_continuous(
      breaks = unique(range_df$x_numeric),
      labels = levels(range_df$Index),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    labs(
      title = "Bucket Trait Score Ranges",
      x = "Custom Index",
      y = "Score Range (%)",
      fill = "Trait Group"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  # Save to file
  output_file <- file.path(output_dir, "actual_trait_ranges_by_index.png")
  ggsave(output_file, plot = g, width = 8, height = 7, dpi = 300)
  message("Plot saved to: ", output_file)
}

#shows how production traits change by the countries we selected
plot_production_trait_trends <- function(all_herds, selected_countries, output_file = "/data/dairy/USERS/ashenoy/bucketing/plots/production_trait_trends_by_country.png") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Step 1: Filter and calculate stats
  stats_by_country <- all_herds %>%
    filter(country %in% selected_countries) %>%
    group_by(country, date) %>%
    summarise(
      milk_mean = mean(abs(milk), na.rm = TRUE),
      milk_median = median(abs(milk), na.rm = TRUE),
      fat_mean = mean(abs(fat), na.rm = TRUE),
      fat_median = median(abs(fat), na.rm = TRUE),
      pro_mean = mean(abs(pro), na.rm = TRUE),
      pro_median = median(abs(pro), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Step 2: Order countries by total herd size
  country_order <- all_herds %>%
    filter(country %in% selected_countries) %>%
    group_by(country) %>%
    summarise(total_herdsize = sum(herdsize, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_herdsize)) %>%
    pull(country)
  
  # Step 3: Pivot longer for plotting
  stats_long <- stats_by_country %>%
    pivot_longer(
      cols = -c(country, date),
      names_to = c("trait", "statistic"),
      names_sep = "_",
      values_to = "value"
    ) %>%
    mutate(country = factor(country, levels = country_order))
  
  # Step 4: Plot
  g <- ggplot(stats_long, aes(x = date, y = value, color = trait, linetype = statistic)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_linetype_manual(values = c(mean = "solid", median = "dotted")) +
    facet_wrap(~ country, scales = "free_y") +
    scale_y_continuous(limits = c(0, 42)) +
    labs(
      title = "",
      x = "Date",
      y = "Value",
      color = "Trait",
      linetype = "Statistic"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
  
  print(g)
  
  # Step 5: Save
  suppressWarnings(
    ggsave(output_file, plot = g, width = 14, height = 8, dpi = 300)
  )
}

#shows how health traits change by the countries we selected
plot_health_trait_trends <- function(all_herds, selected_countries, output_dir = ".") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
  library(stringr)
  
  # Define trait groups
  fitness_traits   <- c("pl", "liv", "scs", "ahi")
  fertility_traits <- c("dpr", "hcr", "ccr")
  calving_traits   <- c("sce", "dce", "ssb", "dsb")
  other_traits     <- c("fs", "rfi")
  all_traits       <- c(fitness_traits, fertility_traits, calving_traits, other_traits)
  
  # Compute trait stats by country/date
  stats_by_country <- all_herds %>%
    filter(country %in% selected_countries) %>%
    group_by(country, date) %>%
    summarise(across(all_of(all_traits),
                     list(mean = ~mean(abs(.x), na.rm = TRUE),
                          median = ~median(abs(.x), na.rm = TRUE)),
                     .names = "{col}_{fn}"),
              .groups = "drop")
  
  # Reshape to long format
  stats_long <- stats_by_country %>%
    pivot_longer(
      cols = -c(country, date),
      names_to = c("trait", "statistic"),
      names_sep = "_",
      values_to = "value"
    )
  
  # Create group mapping
  trait_groups <- tibble(
    trait = all_traits,
    group = c(
      rep("Fitness", length(fitness_traits)),
      rep("Fertility", length(fertility_traits)),
      rep("Calving", length(calving_traits)),
      rep("Feeding", length(other_traits))
    )
  )
  
  stats_long <- stats_long %>%
    left_join(trait_groups, by = "trait")
  
  # Order countries by herdsize
  country_order <- all_herds %>%
    filter(country %in% selected_countries) %>%
    group_by(country) %>%
    summarise(total_herdsize = sum(herdsize, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_herdsize)) %>%
    pull(country)
  
  stats_long <- stats_long %>%
    mutate(country = factor(country, levels = country_order))
  
  # Define internal plotting function
  plot_traits_by_group <- function(data, trait_group_name) {
    data_group <- data %>% filter(group == trait_group_name)
    
    y_limits <- switch(trait_group_name,
                       "Fitness" = c(0, 20),
                       "Fertility" = c(0, 22),
                       "Calving" = c(0, 2),
                       "Feeding" = c(0, 3.5),
                       c(NA, NA))
    
    ggplot(data_group, aes(x = date, y = value, color = trait, linetype = statistic)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_linetype_manual(values = c(mean = "solid", median = "dotted")) +
      facet_wrap(~ country, scales = "free_y") +
      scale_y_continuous(limits = y_limits) +
      labs(
        title = paste(trait_group_name, "Trait Trends by Country"),
        x = "Date",
        y = "Value",
        color = "Trait",
        linetype = "Statistic"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  }
  
  # List of groups to plot and save
  trait_groups_list <- c("Fitness", "Fertility", "Calving", "Feeding")
  
  for (group_name in trait_groups_list) {
    plot <- plot_traits_by_group(stats_long, group_name)
    
    # Print if interactive
    print(plot)
    
    # Save
    out_path <- file.path(output_dir, paste0(tolower(group_name), "_traits_by_country.png"))
    suppressWarnings(ggsave(out_path, plot, width = 14, height = 8, dpi = 300))
  }
}

#shows how conformation traits change by the countries we selected
plot_conformation_trait_trends <- function(all_herds, selected_countries, output_dir = ".") {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
  library(stringr)
  
  # Define trait groups
  conformation_composites <- c("ptat", "udc", "flc", "bwc")
  conformation_body <- c("sta", "str", "bde", "dfm", "rpa", "trw")
  conformation_feet_legs <- c("rls", "rlr", "fta", "fls")
  conformation_udder <- c("fua", "ruh", "ruw", "ucl", "udp")
  conformation_udder_teats <- c("ftp", "rtp", "tlg")
  all_traits       <- c(conformation_composites, conformation_body, conformation_feet_legs, conformation_udder, conformation_udder_teats)
  
  # Compute trait stats by country/date
  stats_by_country <- all_herds %>%
    filter(country %in% selected_countries) %>%
    group_by(country, date) %>%
    summarise(across(all_of(all_traits),
                     list(mean = ~mean(abs(.x), na.rm = TRUE),
                          median = ~median(abs(.x), na.rm = TRUE)),
                     .names = "{col}_{fn}"),
              .groups = "drop")
  
  # Reshape to long format
  stats_long <- stats_by_country %>%
    pivot_longer(
      cols = -c(country, date),
      names_to = c("trait", "statistic"),
      names_sep = "_",
      values_to = "value"
    )
  
  # Create group mapping
  trait_groups <- tibble(
    trait = all_traits,
    group = c(
      rep("Composites", length(conformation_composites)),
      rep("Body", length(conformation_body)),
      rep("Feet & Legs", length(conformation_feet_legs)),
      rep("Udder", length(conformation_udder)),
      rep("Udder Teats", length(conformation_udder_teats))
    )
  )
  
  stats_long <- stats_long %>%
    left_join(trait_groups, by = "trait")
  
  # Order countries by herdsize
  country_order <- all_herds %>%
    filter(country %in% selected_countries) %>%
    group_by(country) %>%
    summarise(total_herdsize = sum(herdsize, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_herdsize)) %>%
    pull(country)
  
  stats_long <- stats_long %>%
    mutate(country = factor(country, levels = country_order))
  
  # Define internal plotting function
  plot_traits_by_group <- function(data, trait_group_name) {
    data_group <- data %>% filter(group == trait_group_name)
    
    y_limits <- switch(trait_group_name,
                       "Composites" = c(0, 8),
                       "Body" = c(0, 9),
                       "Feet & Legs" = c(0, 1.5),
                       "Udder" = c(0, 3),
                       "Udder Teats" = c(0, 6))
    
    ggplot(data_group, aes(x = date, y = value, color = trait, linetype = statistic)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_linetype_manual(values = c(mean = "solid", median = "dotted")) +
      facet_wrap(~ country, scales = "free_y") +
      scale_y_continuous(limits = y_limits) +
      labs(
        title = paste(trait_group_name, "Trait Trends by Country"),
        x = "Date",
        y = "Value",
        color = "Trait",
        linetype = "Statistic"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  }
  
  # List of groups to plot and save
  trait_groups_list <- c("Composites", "Body", "Feet & Legs", "Udder", "Udder Teats")
  
  for (group_name in trait_groups_list) {
    plot <- plot_traits_by_group(stats_long, group_name)
    
    # Print if interactive
    print(plot)
    
    # Save
    out_path <- file.path(output_dir, paste0(tolower(group_name), "_traits_by_country.png"))
    suppressWarnings(ggsave(out_path, plot, width = 14, height = 8, dpi = 300))
  }
}
