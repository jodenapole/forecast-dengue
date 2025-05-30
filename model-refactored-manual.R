set.seed(7)
library("BART")
library("MASS")
library(httr)
library(jsonlite)
library(mlr3)
library(ggplot2)
library(dplyr)
library(zoo)
library(forecast)
library(glue)
library(gganimate)
library(gifski)
library(transformr)
library(parallel)
library(forecast)
library(tidyr)
library(ggrepel)
library(viridis)

# Get API key
API_KEY <- Sys.getenv("MOSQLIMATE_API_KEY")

# FUNCTIONS #
set.seed(7)

# Function to collect and normalize Google Trends data with overlapping 9-month windows
collect_and_normalize_gtrends <- function() {
  # Define periods for data collection (9-month windows with 3-month overlap)
  # Note: 2023 is now a single period from Jan to Dec
  periods <- list(
    list(start = "2011-01-02", end = "2011-09-25", name = "2011_Q1-Q3"),
    list(start = "2011-06-26", end = "2012-03-25", name = "2011_Q3-2012_Q1"),
    list(start = "2012-01-01", end = "2012-09-30", name = "2012_Q1-Q3"),
    list(start = "2012-07-01", end = "2013-03-31", name = "2012_Q3-2013_Q1"),
    list(start = "2012-12-30", end = "2013-09-29", name = "2013_Q1-Q3"),
    list(start = "2013-06-30", end = "2014-03-30", name = "2013_Q3-2014_Q1"),
    list(start = "2013-12-29", end = "2014-09-28", name = "2014_Q1-Q3"),
    list(start = "2014-06-29", end = "2015-03-29", name = "2014_Q3-2015_Q1"),
    list(start = "2014-12-28", end = "2015-09-27", name = "2015_Q1-Q3"),
    list(start = "2015-06-28", end = "2016-03-27", name = "2015_Q3-2016_Q1"),
    list(start = "2015-12-27", end = "2016-09-25", name = "2016_Q1-Q3"),
    list(start = "2016-06-26", end = "2017-03-26", name = "2016_Q3-2017_Q1"),
    list(start = "2017-01-01", end = "2017-09-24", name = "2017_Q1-Q3"),
    list(start = "2017-06-25", end = "2018-03-25", name = "2017_Q3-2018_Q1"),
    list(start = "2017-12-31", end = "2018-09-30", name = "2018_Q1-Q3"),
    list(start = "2018-07-01", end = "2019-03-31", name = "2018_Q3-2019_Q1"),
    list(start = "2018-12-30", end = "2019-09-29", name = "2019_Q1-Q3"),
    list(start = "2019-06-30", end = "2020-03-29", name = "2019_Q3-2020_Q1"),
    list(start = "2019-12-29", end = "2020-09-27", name = "2020_Q1-Q3"),
    list(start = "2020-06-28", end = "2021-03-28", name = "2020_Q3-2021_Q1"),
    list(start = "2020-12-27", end = "2021-09-26", name = "2021_Q1-Q3"),
    list(start = "2021-06-27", end = "2022-03-27", name = "2021_Q3-2022_Q1"),
    list(start = "2021-12-26", end = "2022-09-25", name = "2022_Q1-Q3"),
    list(start = "2022-06-26", end = "2023-03-26", name = "2022_Q3-2023_Q1"),
    list(start = "2023-01-01", end = "2023-12-30", name = "2023_Q1-Q4"),  # Full year 2023
    list(start = "2023-12-31", end = "2024-12-22", name = "2024_Q1-Q4")  # Full year 2024
  )
  
  # Process each period - either using gtrendsR package or loading local CSV files
  period_data_list <- list()
  
  # Loading from local CSV files
  message("Loading Google Trends data from local CSV files")
  
  # Define file mapping - this handles the 2023 special case
  file_mapping <- list()
  for (i in seq_along(periods)) {
    period <- periods[[i]]
    
    # For 2023, we use the same file for both periods (full year)
    file_name <- sprintf("dengue_trends_%s.csv", period$name)
    
    file_mapping[[i]] <- list(
      period = period,
      file_name = file_name,
      date_range = c(as.Date(period$start), as.Date(period$end))
    )
  }
  
  # Process each file, respecting the correct date ranges
  processed_periods <- c()
  
  for (i in seq_along(file_mapping)) {
    mapping <- file_mapping[[i]]
    period <- mapping$period
    file_name <- mapping$file_name
    date_range <- mapping$date_range
    
    if (i %in% processed_periods) {
      message(sprintf("Skipping period %d - already processed", i))
      next
    }
    
    message(sprintf("Processing file for period %d/%d: %s", i, length(periods), period$name))
    
    if (file.exists(file_name)) {
      # Read CSV file
      tryCatch({
        # For standard Google Trends CSV format
        # First find where the data starts (after metadata)
        lines <- readLines(file_name)
        data_start <- grep("^(Week|Date|Semana|Data)", lines)[1]
        
        if (!is.na(data_start)) {
          # Create a temporary file with just the data section
          temp_file <- tempfile(fileext = ".csv")
          writeLines(lines[data_start:length(lines)], temp_file)
          
          # Read the data section
          trends_data <- read.csv(temp_file, check.names = FALSE, stringsAsFactors = FALSE)
          file.remove(temp_file)
          
          # First column is date, second column is interest
          date_col <- colnames(trends_data)[1]
          interest_col <- colnames(trends_data)[2]
          
          period_data <- data.frame(
            date = trends_data[[date_col]],
            hits = trends_data[[interest_col]],
            stringsAsFactors = FALSE
          )
          
          # Convert date strings (might need adjustment based on your format)
          period_data$date <- as.Date(period_data$date)
          # Remove any NA dates
          period_data <- period_data[!is.na(period_data$date), ]
          
          # Filter to the specific date range for this period
          period_data <- period_data[period_data$date >= date_range[1] & 
                                       period_data$date <= date_range[2], ]
          
          period_data$period <- i
          period_data_list[[i]] <- period_data
          processed_periods <- c(processed_periods, i)
          
          message(sprintf("  Loaded %d data points", nrow(period_data)))
          
          # Special handling for full-year 2023 file
          if (grepl("2023", period$name) && grepl("Q1-Q3", period$name)) {
            # Check if there are other periods in 2023 that need data from this file
            for (j in seq_along(file_mapping)) {
              if (j != i && !j %in% processed_periods && grepl("2023", file_mapping[[j]]$period$name)) {
                # Extract data for the other 2023 period
                other_range <- file_mapping[[j]]$date_range
                other_data <- period_data[period_data$date >= other_range[1] & 
                                            period_data$date <= other_range[2], ]
                
                if (nrow(other_data) > 0) {
                  other_data$period <- j
                  period_data_list[[j]] <- other_data
                  processed_periods <- c(processed_periods, j)
                  message(sprintf("  Also extracted %d points for period %d", nrow(other_data), j))
                }
              }
            }
          }
        } else {
          message("  Could not find data section in file")
        }
      }, error = function(e) {
        message(sprintf("  Error reading file: %s", e$message))
      })
    } else {
      message(sprintf("  File not found: %s", file_name))
    }
  }
  
  
  # Check if we have any data
  if (length(period_data_list) == 0) {
    stop("No Google Trends data could be collected or loaded")
  }
  
  # Remove empty elements from the list
  period_data_list <- period_data_list[!sapply(period_data_list, is.null)]
  
  # Normalize data across overlapping periods
  message("\nNormalizing data across periods...")
  
  # Start with the first period
  normalized_data <- period_data_list[[1]]
  
  # Process each subsequent period
  for (i in 2:length(period_data_list)) {
    current_period <- period_data_list[[i]]
    
    # Find overlap between this period and already normalized data
    overlap_start <- max(min(normalized_data$date), min(current_period$date))
    overlap_end <- min(max(normalized_data$date), max(current_period$date))
    
    prev_overlap <- normalized_data[normalized_data$date >= overlap_start & 
                                      normalized_data$date <= overlap_end, ]
    curr_overlap <- current_period[current_period$date >= overlap_start & 
                                     current_period$date <= overlap_end, ]
    
    # Match by date
    overlap <- merge(prev_overlap, curr_overlap, by = "date", suffixes = c("_prev", "_curr"))
    
    if (nrow(overlap) > 0) {
      # Calculate scaling factors
      scaling_factors <- overlap$hits_prev / overlap$hits_curr
      
      # Use median scaling factor to avoid outliers
      scaling_factor <- median(scaling_factors, na.rm = TRUE)
      
      message(sprintf("  Period %d: Found %d overlap points, scaling factor: %.4f", 
                      i, nrow(overlap), scaling_factor))
      
      # Scale the current period
      current_period$hits <- current_period$hits * scaling_factor
      
      # Add non-overlapping points to the normalized data
      new_points <- current_period[!current_period$date %in% normalized_data$date, ]
      normalized_data <- rbind(normalized_data, new_points)
    } else {
      message(sprintf("  Warning: No overlap for period %d, cannot normalize properly", i))
      
      # Add points without scaling as a last resort
      new_points <- current_period[!current_period$date %in% normalized_data$date, ]
      normalized_data <- rbind(normalized_data, new_points)
    }
  }
  
  # Sort by date
  normalized_data <- normalized_data[order(normalized_data$date), ]
  
  # Add epiweek format to match with dengue data
  normalized_data$year <- as.numeric(format(normalized_data$date, "%Y"))
  normalized_data$week <- as.numeric(format(normalized_data$date, "%V"))
  normalized_data$epiweek <- paste0(normalized_data$year, 
                                    sprintf("%02d", normalized_data$week))
  
  # Summarize by epiweek for matching with dengue data
  weekly_gtrends <- normalized_data %>%
    group_by(epiweek, year, week) %>%
    summarize(
      hits = mean(hits, na.rm = TRUE),
      date = min(date),
      .groups = "drop"
    )
  
  # Summary statistics
  message("\nGoogle Trends data summary:")
  message(sprintf("Date range: %s to %s", min(normalized_data$date), max(normalized_data$date)))
  message(sprintf("Total data points: %d", nrow(normalized_data)))
  message(sprintf("Unique epiweeks: %d", nrow(weekly_gtrends)))
  
  # Return both the normalized data and weekly aggregates
  return(list(
    normalized_data = normalized_data,
    weekly_data = weekly_gtrends
  ))
}

# Calling API for climate and dengue
api_calls <- function(api_name) {
  if (api_name == "climate") {
    # API CALL FOR CLIMATE
    climate_weekly_api <- "https://api.mosqlimate.org/api/datastore/climate/weekly/"
    all_items <- list()  # Store items in a list for more efficient collection
    total_pages_estimate <- 500
    
    for (pagenumber in 1:total_pages_estimate) {
      params <- list(
        page = pagenumber,
        per_page = 300,
        start = 201101, 
        end = 202452,
        geocode = 3304557
      )
      
      headers <- add_headers(
        `X-UID-Key` = API_KEY
      )
      
      resp <- GET(climate_weekly_api, query = params, headers)
      
      # Handle HTTP errors
      if (http_error(resp)) {
        message(paste("Error:", status_code(resp)))
        break
      }
      
      # Safe JSON parsing
      tryCatch({
        json_content <- fromJSON(content(resp, "text"))
        items <- json_content$items
        
        # Store this page's items
        if (length(items) > 0 && nrow(items) > 0) {
          all_items[[pagenumber]] <- items
          message(sprintf("Page %d: Retrieved %d items", pagenumber, nrow(items)))
        }
        
        # Break if we got fewer items than requested (likely the last page)
        if (length(items) == 0 || nrow(items) < 300) {
          message("Reached last page of data")
          break
        }
        
      }, error = function(e) {
        message(paste("Error parsing response:", e$message))
        break
      })
    }
    
    # Combine all items into a single data frame
    climate_data <- do.call(rbind, all_items)
    
    # Add week_of_year column once after all data is collected
    if (nrow(climate_data) > 0) {
      climate_data$week_of_year <- as.numeric(substr(climate_data$epiweek, 5, 6))
    }
    
    api_response <- climate_data
  }
  
  if (api_name == "dengue") {
    # API CALL FOR DENGUE CASES 2019 - 2023
    dengue_data <- data.frame()
    dengue_api <- "https://api.mosqlimate.org/api/datastore/infodengue/?disease=dengue"
    date <- paste0("&start=2010-12-30&end=2024-12-22")
    geocode <- paste0("&geocode=3304557")
    total_pages_estimate <- 500
    for (pagenumber in 1:total_pages_estimate) { # Loop until there are no more pages
      
      pagination <- paste0("&page=", pagenumber, "&per_page=100")
      url <- paste0(dengue_api, date, geocode, pagination)
      
      resp <- GET(url, add_headers(.headers = c("X-UID-Key" = API_KEY)))
      
      if (http_error(resp)) {
        message(paste("Error:", status_code(resp))) # Handle errors gracefully
        break  # Exit the loop if there's an API error
      }
      
      json_content <- fromJSON(content(resp, "text"))
      items <- json_content$items
      
      dengue_data <- rbind(dengue_data, items)  # Append the current page's items
      
      if (nrow(items) < 100) { # Check if the current page is empty (no more data)
        print(nrow(items))
        break # Exit the loop if there's no more data
      }
      
    }
    api_response <- dengue_data[nrow(dengue_data):1,]
  }
  
  return(api_response)
}

# Helper functions for feature engineering
create_temp_competence_optimality <- function(temp_data) {
  # Initialize result vector with same length as input
  competence_scores <- numeric(length(temp_data))
  
  # Apply the optimality scale based on temperature ranges
  for (i in 1:length(temp_data)) {
    temp <- temp_data[i]
    
    # Apply the simplified vector competence optimality scale
    if ((temp >= 25 && temp <= 32)) {
      # High transmission efficiency
      competence_scores[i] <- 3
    } else if ((temp >= 23 && temp < 25) || (temp > 32 && temp <= 34)) {
      # Moderate transmission efficiency
      competence_scores[i] <- 2
    } else {
      # Low transmission efficiency
      competence_scores[i] <- 1
    }
  }
  
  return(competence_scores)
}

create_humidity_risk <- function(humidity_data) {
  # Initialize result vector with same length as input
  humidity_scores <- numeric(length(humidity_data))
  
  # Apply the risk scale based on humidity ranges
  for (i in 1:length(humidity_data)) {
    humidity <- humidity_data[i]
    
    # Apply the humidity risk scale
    if (humidity < 75) {
      # Low risk
      humidity_scores[i] <- 1
    } else if (humidity >= 75 && humidity <= 80) {
      # Medium risk
      humidity_scores[i] <- 2
    } else {
      # High risk (above 80%)
      humidity_scores[i] <- 3
    }
  }
  
  return(humidity_scores)
}

create_optimal_conditions <- function(temp_competence, humidity_risk) {
  # Initialize result vector
  optimal_conditions <- numeric(length(temp_competence))
  
  # Set to 1 when both conditions are optimal (both are 3)
  for (i in 1:length(temp_competence)) {
    if (temp_competence[i] == 3 && humidity_risk[i] > 1) {
      optimal_conditions[i] <- 1
    } else {
      optimal_conditions[i] <- 0
    }
  }
  
  return(optimal_conditions)
}

fill_lag_columns <- function(data, temp_weekly_avg) {
  # Define pairs of columns (original column, average column)
  column_pairs <- list(
    list("temp_med_lag1", "avg_tempmed_lag1"),
    list("temp_med_lag2", "avg_tempmed_lag2"),
    list("temp_med_lag3", "avg_tempmed_lag3"),
    list("temp_med_lag4", "avg_tempmed_lag4"),
    list("umid_med_lag1", "avg_umidmed_lag1"),
    list("umid_med_lag2", "avg_umidmed_lag2"),
    list("umid_med_lag3", "avg_umidmed_lag3"),
    list("umid_med_lag4", "avg_umidmed_lag4"),
    list("precip_tot_lag1", "avg_precip_tot_lag1"),
    list("precip_tot_lag2", "avg_precip_tot_lag2"),
    list("precip_tot_lag3", "avg_precip_tot_lag3"),
    list("precip_tot_lag4", "avg_precip_tot_lag4")
  )
  
  # Create a lookup table for each weekly average column
  lookup_tables <- list()
  for (col_pair in column_pairs) {
    avg_col <- col_pair[[2]]
    lookup_tables[[avg_col]] <- setNames(
      temp_weekly_avg[[avg_col]],
      temp_weekly_avg$week_of_year
    )
  }
  
  # For each column pair
  for (col_pair in column_pairs) {
    col <- col_pair[[1]]
    avg_col <- col_pair[[2]]
    
    # Find rows with NA values
    na_rows <- which(is.na(data[[col]]))
    
    if (length(na_rows) > 0) {
      # Replace NA values with the corresponding week's average
      data[[col]][na_rows] <- lookup_tables[[avg_col]][as.character(data$week_of_year[na_rows])]
    }
  }
  
  return(data)
}

# FEATURE ENGINEERING FUNCTION
feature_engineering <- function(dengue_data_ordered, climate_data, gtrends_results) {
  # LAGGED CASES
  dengue_data_ordered$casos_lag1 <-  dplyr::lag(dengue_data_ordered$casos, 1)
  dengue_data_ordered$casos_lag1[1] = 147
  
  dengue_data_ordered$casos_lag2 <-  dplyr::lag(dengue_data_ordered$casos, 2)
  dengue_data_ordered$casos_lag2[1] = 124
  dengue_data_ordered$casos_lag2[2] = 147
  
  dengue_data_ordered$casos_lag3 <-  dplyr::lag(dengue_data_ordered$casos, 3)
  dengue_data_ordered$casos_lag3[1] = 138
  dengue_data_ordered$casos_lag3[2] = 124
  dengue_data_ordered$casos_lag3[3] = 147
  
  dengue_data_ordered$casos_lag4 <-  dplyr::lag(dengue_data_ordered$casos, 4)
  dengue_data_ordered$casos_lag4[1] = 150
  dengue_data_ordered$casos_lag4[2] = 138
  dengue_data_ordered$casos_lag4[3] = 124
  dengue_data_ordered$casos_lag4[4] = 147
  
  # MOV STD DEVIATION
  dengue_data_ordered$casoslag1_mov_sd <- rollapply(dengue_data_ordered$casos_lag1,
                                                    width = 3,
                                                    FUN = sd,
                                                    fill = NA,
                                                    align = "right")
  dengue_data_ordered$casoslag1_mov_sd[1] <- sd(c(147, 124, 138))
  dengue_data_ordered$casoslag1_mov_sd[2] <- sd(c(179, 147, 124))
  dengue_data_ordered$casoslag1_mov_sd[3] <- sd(c(179, 186, 147))
  
  # MOVING AVERAGE
  dengue_data_ordered$avg <- rollmean(dengue_data_ordered$casos_lag1,
                                      k = 3,
                                      fill = NA,
                                      align = "right")
  dengue_data_ordered$avg[1] <- (147 + 124 + 138) / 3
  dengue_data_ordered$avg[2] <- (179 + 147 + 124) / 3
  
  # MOVING WEIGHTED AVERAGE
  for (week in 1:length(dengue_data_ordered$casos_lag1)) {
    if (week == 1) {
      dengue_data_ordered$wavg[1] <- ((147 * 3) + (124 * 2) + (138)) / 6
    }
    
    if (week == 2) {
      dengue_data_ordered$wavg[2] <- ((179 * 3) + (147 * 2) + (124)) / 6
    }
    
    if (week == 3) {
      dengue_data_ordered$wavg[3] <- ((186 * 3) + (179 * 2) + (147)) / 6
    }
    
    
    if(week > 3) {
      dengue_data_ordered$wavg[week] <- ((dengue_data_ordered$casos_lag1[week] * 3) + (dengue_data_ordered$casos_lag2[week] * 2) + (dengue_data_ordered$casos_lag3[week])) / 6
      
    }
  }
  
  
  # REPRESENT SEASONALITY
  dengue_data_ordered$week_of_year <- as.numeric(substr(dengue_data_ordered$SE, 5, 6))
  dengue_data_ordered$sin_year <- sin(2 * pi * (dengue_data_ordered$week_of_year / 52))
  dengue_data_ordered$cos_year <- cos(2 * pi * (dengue_data_ordered$week_of_year / 52))
  
  # Create a "decay rate" feature specifically for the declining phase
  dengue_data_ordered$decay_rate <- 0
  decay_window <- 3  # Number of weeks to look back
  
  for (i in (decay_window+1):nrow(dengue_data_ordered)) {
    # Only calculate for declining phases
    if (all(diff(dengue_data_ordered$casos[(i-decay_window):i]) < 0)) {
      # Calculate average percentage change during decline
      declines <- c()
      for (j in 1:decay_window) {
        week_decline <- (dengue_data_ordered$casos[i-j+1] - dengue_data_ordered$casos[i-j]) / 
          dengue_data_ordered$casos[i-j]
        declines <- c(declines, week_decline)
      }
      dengue_data_ordered$decay_rate[i] <- mean(declines)
    }
  }
  
  #TEMP MED LAG
  dengue_data_ordered$temp_med_lag1 <- dplyr::lag(climate_data$temp_med_avg, 1)
  dengue_data_ordered$temp_med_lag2 <- dplyr::lag(climate_data$temp_med_avg, 2)
  dengue_data_ordered$temp_med_lag3 <- dplyr::lag(climate_data$temp_med_avg, 3)
  dengue_data_ordered$temp_med_lag4 <- dplyr::lag(climate_data$temp_med_avg, 4)
  
  #UMID MED LAG
  dengue_data_ordered$umid_med_lag1 <- dplyr::lag(climate_data$umid_med_avg, 1)
  dengue_data_ordered$umid_med_lag2 <- dplyr::lag(climate_data$umid_med_avg, 2)
  dengue_data_ordered$umid_med_lag3 <- dplyr::lag(climate_data$umid_med_avg, 3)
  dengue_data_ordered$umid_med_lag4 <- dplyr::lag(climate_data$umid_med_avg, 4)
  
  #Preciptation Total Lag
  dengue_data_ordered$precip_tot_lag1 <- dplyr::lag(climate_data$precip_tot_sum, 1)
  dengue_data_ordered$precip_tot_lag2 <- dplyr::lag(climate_data$precip_tot_sum, 2)
  dengue_data_ordered$precip_tot_lag3 <- dplyr::lag(climate_data$precip_tot_sum, 3)
  dengue_data_ordered$precip_tot_lag4 <- dplyr::lag(climate_data$precip_tot_sum, 4)
  dengue_data_ordered$precip_tot_lag5 <- dplyr::lag(climate_data$precip_tot_sum, 5)
  dengue_data_ordered$precip_tot_lag6 <- dplyr::lag(climate_data$precip_tot_sum, 6)
  dengue_data_ordered$precip_tot_lag7 <- dplyr::lag(climate_data$precip_tot_sum, 7)
  
  # Calculate average lag values by week of year
  temp_weekly_avg <- subset(dengue_data_ordered, SE <202401) %>%
    group_by(week_of_year) %>%
    summarize(
      avg_tempmed_lag1 = mean(temp_med_lag1, na.rm = TRUE),
      avg_umidmed_lag1 = mean(umid_med_lag1, na.rm = TRUE),
      avg_precip_tot_lag1 = mean(precip_tot_lag1, na.rm = TRUE),
      
      avg_tempmed_lag2 = mean(temp_med_lag2, na.rm = TRUE),
      avg_umidmed_lag2 = mean(umid_med_lag2, na.rm = TRUE),
      avg_precip_tot_lag2 = mean(precip_tot_lag2, na.rm = TRUE),
      
      avg_tempmed_lag3 = mean(temp_med_lag3, na.rm = TRUE),
      avg_umidmed_lag3 = mean(umid_med_lag3, na.rm = TRUE),
      avg_precip_tot_lag3 = mean(precip_tot_lag3, na.rm = TRUE),
      
      avg_tempmed_lag4 = mean(temp_med_lag4, na.rm = TRUE),
      avg_umidmed_lag4 = mean(umid_med_lag4, na.rm = TRUE),
      avg_precip_tot_lag4 = mean(precip_tot_lag4, na.rm = TRUE),
      avg_precip_tot_lag5 = mean(precip_tot_lag5, na.rm = TRUE),
      avg_precip_tot_lag6 = mean(precip_tot_lag6, na.rm = TRUE),
      avg_precip_tot_lag7 = mean(precip_tot_lag7, na.rm = TRUE)
    )
  
  dengue_data_ordered <- fill_lag_columns(dengue_data_ordered, temp_weekly_avg)
  
  # Optimal temperature window
  dengue_data_ordered$temp_competence_optimality <- 
    create_temp_competence_optimality(dengue_data_ordered$temp_med_lag3)
  
  # Optimal humidity window
  dengue_data_ordered$humidity_risk <- 
    create_humidity_risk(dengue_data_ordered$umid_med_lag3)
  
  # Flag for when humidity and temperature are optimal together
  dengue_data_ordered$optimal_conditions <- 
    create_optimal_conditions(
      dengue_data_ordered$temp_competence_optimality,
      dengue_data_ordered$humidity_risk
    )
  
  dengue_data_ordered$temp_competence_humidty_risk <- dengue_data_ordered$temp_competence_optimality * dengue_data_ordered$humidity_risk
  
  # Log growth rate between lag1 and lag2
  dengue_data_ordered$log_lag_growth <- 0
  for (i in 3:nrow(dengue_data_ordered)) {
    if (dengue_data_ordered$casos_lag2[i] > 0 && dengue_data_ordered$casos_lag1[i] > 0) {
      dengue_data_ordered$log_lag_growth[i] <- 
        log(dengue_data_ordered$casos_lag1[i] / dengue_data_ordered$casos_lag2[i])
    }
  }
  
  # Temperature volatility (important for vector breeding)
  dengue_data_ordered$temp_volatility <- rollapply(climate_data$temp_med_avg,
                                                   width = 4,
                                                   FUN = sd,
                                                   fill = NA,
                                                   align = "right")
  dengue_data_ordered$temp_volatility[is.na(dengue_data_ordered$temp_volatility)] <- 
    mean(dengue_data_ordered$temp_volatility, na.rm=TRUE)
  
  # Sustained favorable conditions (count of consecutive weeks with optimal conditions)
  dengue_data_ordered$favorable_weeks_count <- 0
  counter <- 0
  for (i in 1:nrow(dengue_data_ordered)) {
    if (dengue_data_ordered$temp_competence_humidty_risk[i] >= 6) {  # Threshold for favorable conditions
      counter <- counter + 1
    } else {
      counter <- 0
    }
    dengue_data_ordered$favorable_weeks_count[i] <- counter
  }
  
  
  # Combined climate condition transitions
  dengue_data_ordered$climate_state_change <- 0
  for (i in 2:nrow(dengue_data_ordered)) {
    # Define previous and current climate states
    prev_temp_state <- dengue_data_ordered$temp_competence_optimality[i-1]
    curr_temp_state <- dengue_data_ordered$temp_competence_optimality[i]
    
    prev_humid_state <- dengue_data_ordered$humidity_risk[i-1]
    curr_humid_state <- dengue_data_ordered$humidity_risk[i]
    
    # Detect state changes
    if (prev_temp_state != curr_temp_state || prev_humid_state != curr_humid_state) {
      dengue_data_ordered$climate_state_change[i] <- 1
    }
  }
  
  # Enhanced climate state change feature - add context about type of change
  dengue_data_ordered$climate_state_direction <- 0
  for (i in 2:nrow(dengue_data_ordered)) {
    # Define previous and current climate states
    prev_temp_state <- dengue_data_ordered$temp_competence_optimality[i-1]
    curr_temp_state <- dengue_data_ordered$temp_competence_optimality[i]
    
    prev_humid_state <- dengue_data_ordered$humidity_risk[i-1]
    curr_humid_state <- dengue_data_ordered$humidity_risk[i]
    
    # Calculate "favorability score" for previous and current states
    prev_favorability <- prev_temp_state * prev_humid_state
    curr_favorability <- curr_temp_state * curr_humid_state
    
    # Detect if conditions became more or less favorable
    if (curr_favorability > prev_favorability) {
      # Conditions improved (more favorable for mosquitoes)
      dengue_data_ordered$climate_state_direction[i] <- 1
    } else if (curr_favorability < prev_favorability) {
      # Conditions worsened (less favorable for mosquitoes)
      dengue_data_ordered$climate_state_direction[i] <- -1
    } else {
      # No change in favorability, but possibly a change in state type
      if (prev_temp_state != curr_temp_state || prev_humid_state != curr_humid_state) {
        dengue_data_ordered$climate_state_direction[i] <- 0.5 * sign(curr_temp_state - prev_temp_state)
      } else {
        dengue_data_ordered$climate_state_direction[i] <- 0
      }
    }
  }
  
  # Create a combined feature that indicates both if a change occurred and in which direction
  dengue_data_ordered$climate_change_context <- dengue_data_ordered$climate_state_change * dengue_data_ordered$climate_state_direction
  
  # Add temporal stability - how stable have conditions been?
  dengue_data_ordered$temp_humid_risk_stability <- 0
  
  for (i in 4:nrow(dengue_data_ordered)) {
    recent_risk_values <- dengue_data_ordered$temp_competence_humidty_risk[(i-3):i]
    dengue_data_ordered$temp_humid_risk_stability[i] <- sd(recent_risk_values)
  }
  
  # Fill first 3 values
  dengue_data_ordered$temp_humid_risk_stability[1:3] <- mean(dengue_data_ordered$temp_humid_risk_stability, 
                                                             na.rm = TRUE)
  
  dengue_data_ordered$SE_char <- as.character(dengue_data_ordered$SE)
  dengue_data_ordered <- left_join(dengue_data_ordered, gtrends_results$weekly_data, by = c("SE_char" = "epiweek"))
  
  # Create lag features
  dengue_data_ordered$gtrends_lag1 <- dplyr::lag(dengue_data_ordered$hits, 1)
  dengue_data_ordered$gtrends_lag2 <- dplyr::lag(dengue_data_ordered$hits, 2)
  dengue_data_ordered$gtrends_lag3 <- dplyr::lag(dengue_data_ordered$hits, 3)
  dengue_data_ordered$gtrends_lag4 <- dplyr::lag(dengue_data_ordered$hits, 4)
  dengue_data_ordered$gtrends_lag5 <- dplyr::lag(dengue_data_ordered$hits, 5)
  
  # Create moving average features
  dengue_data_ordered$gtrends_lag2_mov_sd <- rollapply(
    dengue_data_ordered$gtrends_lag2,
    width = 3,
    FUN = mean,
    fill = NA,
    align = "right"
  )
  
  
  # Handle missing values - use weekly average by season
  gtrends_weekly_avgs <- subset(dengue_data_ordered, SE <202401) %>%
    group_by(week_of_year) %>%
    summarize(
      avg_hits = mean(hits, na.rm = TRUE),
      avg_gtrends_lag1 = mean(gtrends_lag1, na.rm = TRUE),
      avg_gtrends_lag2 = mean(gtrends_lag2, na.rm = TRUE),
      avg_gtrends_lag3 = mean(gtrends_lag3, na.rm = TRUE),
      avg_gtrends_lag4 = mean(gtrends_lag4, na.rm = TRUE),
      avg_gtrends_lag5 = mean(gtrends_lag5, na.rm = TRUE),
      avg_gtrends_lag2_mov_sd = mean(gtrends_lag2_mov_sd, na.rm = TRUE)
    )
  
  # Fill any remaining NAs with overall averages
  gtrends_cols <- c("hits", "gtrends_lag1", "gtrends_lag2", "gtrends_lag3", "gtrends_lag4", "gtrends_lag5",
                    "gtrends_lag2_mov_sd")
  
  for (col in gtrends_cols) {
    if (any(is.na(dengue_data_ordered[[col]]))) {
      col_avg <- mean((subset(dengue_data_ordered, SE <202401))[[col]], na.rm = TRUE)
      dengue_data_ordered[[col]][is.na(dengue_data_ordered[[col]])] <- col_avg
    }
  }
  
  # Rename 'decay_rate' to 'decay' to match the expected column name
  dengue_data_ordered$decay <- dengue_data_ordered$decay_rate
  
  return(
    list(
      dengue_data_engineered = dengue_data_ordered,
      climate_data_engineered = climate_data
    )
  )
}

# Function to create train/test splits with different feature sets
create_train_test_splits <- function(dengue_data_engineered, climate_data_engineered, feature_set) {
  # TRAIN TEST SPLIT
  dengue_data_train <- subset(dengue_data_engineered, SE <202301)
  dengue_data_test <- head(subset(dengue_data_engineered, SE >202252), 52)
  dengue_data_validation <- subset(dengue_data_engineered, SE > 202352)
  climate_data_train <- subset(climate_data_engineered, epiweek <202301)
  climate_data_test <- head(subset(climate_data_engineered, epiweek >202252), 52)
  climate_data_validation <- subset(climate_data_engineered, epiweek >202352)
  
  # Define feature sets
  if (feature_set == "historical") {
    x_train <- cbind(
      dengue_data_train$casos_lag1,
      dengue_data_train$casos_lag2,
      dengue_data_train$casos_lag3,
      dengue_data_train$casos_lag4,
      dengue_data_train$sin_year,
      dengue_data_train$cos_year,
      dengue_data_train$casoslag1_mov_sd,
      dengue_data_train$avg,
      dengue_data_train$wavg,
      dengue_data_train$decay
    )
    
    feature_names <- c(
      "casos_lag1",
      "casos_lag2",
      "casos_lag3",
      "casos_lag4",
      "sin_year",
      "cos_year",
      "casoslag1_mov_sd",
      "avg",
      "wavg",
      "decay"
    )
    
  }
  
  if (feature_set == "climate_gtrends_historical") {
    x_train <- cbind(
      dengue_data_train$casos_lag1,
      dengue_data_train$casos_lag2,
      dengue_data_train$casos_lag3,
      dengue_data_train$casos_lag4,
      dengue_data_train$temp_med_lag4,
      dengue_data_train$umid_med_lag4,
      dengue_data_train$precip_tot_lag4,
      dengue_data_train$gtrends_lag3,
      dengue_data_train$gtrends_lag4,
      dengue_data_train$gtrends_lag2_mov_sd,
      dengue_data_train$temp_competence_optimality,
      dengue_data_train$humidity_risk,
      dengue_data_train$temp_competence_humidty_risk,
      dengue_data_train$sin_year,
      dengue_data_train$cos_year,
      dengue_data_train$casoslag1_mov_sd,
      dengue_data_train$avg,
      dengue_data_train$wavg,
      dengue_data_train$decay
    )
    
    feature_names <- c(
      "casos_lag1",
      "casos_lag2",
      "casos_lag3",
      "casos_lag4",
      "temp_med_lag4",
      "umid_med_lag4",
      "precip_tot_lag4",
      "gtrends_lag3",
      "gtrends_lag4",
      "gtrends_lag2_mov_sd",
      "temp_competence_optimality",
      "humidity_risk",
      "temp_competence_humidty_risk",
      "sin_year",
      "cos_year",
      "casoslag1_mov_sd",
      "avg",
      "wavg",
      "decay"
    )
  }
  
  x_train_df <- as.data.frame(x_train)
  colnames(x_train_df) <- feature_names
  
  lambda <- BoxCox.lambda(dengue_data_train$casos)
  y_train <- cbind(BoxCox(dengue_data_train$casos, lambda))
  
  # Prepare y_test and y_validation with necessary columns for prediction functions
  y_test <- cbind(
    dengue_data_test$casos,
    dengue_data_test$hits,
    dengue_data_test$gtrends_lag5,
    climate_data_test$temp_med_avg,
    climate_data_test$umid_med_avg,
    climate_data_test$precip_tot_sum,
    dengue_data_test$temp_competence_optimality,
    dengue_data_test$humidity_risk,
    dengue_data_test$temp_competence_humidty_risk,
    dengue_data_test$temp_volatility,
    dengue_data_test$temp_humid_risk_stability
  )
  
  y_test <- as.data.frame(y_test)
  
  colnames(y_test) <- c(
    "casos",
    "hits",
    "gtrends_lag5",
    "temp_med_avg",
    "umid_med_avg",
    "precip_tot_sum",
    "temp_competence_optimality",
    "humidity_risk",
    "temp_competence_humidty_risk",
    "temp_volatility",
    "temp_humid_risk_stability"
  )
  
  y_test$week_number <- 1:52
  
  y_validation <- cbind(
    dengue_data_validation$casos,
    dengue_data_validation$hits,
    climate_data_validation$temp_med_avg,
    climate_data_validation$umid_med_avg,
    climate_data_validation$precip_tot_sum,
    dengue_data_validation$temp_competence_optimality,
    dengue_data_validation$humidity_risk,
    dengue_data_validation$temp_competence_humidty_risk,
    dengue_data_validation$temp_volatility,
    dengue_data_validation$temp_humid_risk_stability
  )
  
  y_validation <- as.data.frame(y_validation)
  
  colnames(y_validation) <- c(
    "casos",
    "hits",
    "temp_med_avg",
    "umid_med_avg",
    "precip_tot_sum",
    "temp_competence_optimality",
    "humidity_risk",
    "temp_competence_humidty_risk",
    "temp_volatility",
    "temp_humid_risk_stability"
  )
  
  y_validation$week_number <- 1:52
  
  return(
    list(
      x_train_df = x_train_df,
      y_train = y_train,
      y_test = y_test,
      y_validation = y_validation,
      lambda = lambda,
      feature_set = feature_set,
      feature_names = feature_names
    )
  )
}

# Training the BART model
bart_model <- function (x_train, y_train, k, power, ntree, burn = 13000, nd = 5000) {
  set.seed(7)
  post <- wbart(x_train, y_train, nskip = burn, ndpost = nd, k=k, power = power, ntree = ntree, sparse = FALSE)
  
  return (post)
}

# Helper function to generate recursive features for predictions
generate_recursive_features <- function(week, week_base, mean_predictions, y_test, model_type) {
  # Get average precipitation lag
  avg_precip_lag <- ((y_test$precip_tot_sum[week_base]) + 
                       (y_test$precip_tot_sum[week_base - 1]) + 
                       (y_test$precip_tot_sum[week_base - 2]) + 
                       (y_test$precip_tot_sum[week_base -3])) / 4
  
  # Get actual values for comparison
  v1 <- y_test$casos[week_base]
  v2 <- y_test$casos[week_base - 1]
  v3 <- y_test$casos[week_base - 2]
  v4 <- y_test$casos[week_base - 3]
  
  # Set up lag values based on which week we're predicting
  if (week == 1) {
    current_lag1 <- v1
    current_lag2 <- v2
    current_lag3 <- v3
    current_lag4 <- v4
  } else if (week == 2) {
    current_lag1 <- mean_predictions[1]
    current_lag2 <- v1
    current_lag3 <- v2
    current_lag4 <- v3
  } else if (week == 3) {
    current_lag1 <- mean_predictions[2]
    current_lag2 <- mean_predictions[1]
    current_lag3 <- v1
    current_lag4 <- v2
  } else if (week == 4) {
    current_lag1 <- mean_predictions[3]
    current_lag2 <- mean_predictions[2]
    current_lag3 <- mean_predictions[1]
    current_lag4 <- v1
  } else {
    current_lag1 <- mean_predictions[4]
    current_lag2 <- mean_predictions[3]
    current_lag3 <- mean_predictions[2]
    current_lag4 <- mean_predictions[1]
  }
  
  # Common features across all models
  current_gtrends_lag1 <- y_test$hits[week_base]
  current_gtrends_lag2 <- y_test$hits[week_base - 1] 
  current_gtrends_lag3 <- y_test$hits[week_base - 2] 
  current_gtrends_lag4 <- y_test$hits[week_base - 3] 
  current_gtrends_lag5 <- y_test$gtrends_lag5
  current_gtrends_lag2_mov_sd <- sd(c(current_gtrends_lag2, current_gtrends_lag3, current_gtrends_lag4))
  
  if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
    declines <- c((current_lag1 - current_lag2) / current_lag2,
                  (current_lag2 - current_lag3) / current_lag3)
    current_decay <- mean(declines)
  } else { current_decay <- 0 }
  
  current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
  current_umid_med_lag4 <- y_test$umid_med_avg[week_base]
  current_precip_tot_lag4 <- avg_precip_lag
  
  current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
  current_humidity_risk <- y_test$humidity_risk[week_base]
  current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
  
  mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
  avg <- mean(c(current_lag1, current_lag2, current_lag3))
  wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
  
  week_number <- y_test$week_number[week_base + week]
  
  # Create prediction data based on model type
  if (model_type == "historical") {
    recursive_data <- data.frame(
      casos_lag1 = current_lag1,
      casos_lag2 = current_lag2,
      casos_lag3 = current_lag3,
      casos_lag4 = current_lag4,
      sin_year = sin(2 * pi * week_number/52),
      cos_year = cos(2 * pi * week_number/52),
      casoslag1_mov_sd = mov_sd,
      avg = avg,
      wavg = wavg,
      decay = current_decay
    )
  }
  
  if (model_type == "climate_gtrends_historical") {
    recursive_data <- data.frame(
      casos_lag1 = current_lag1,
      casos_lag2 = current_lag2,
      casos_lag3 = current_lag3,
      casos_lag4 = current_lag4,
      gtrends_lag3 = current_gtrends_lag3,
      gtrends_lag4 = current_gtrends_lag4,
      gtrends_lag2_mov_sd = current_gtrends_lag2_mov_sd,
      temp_med_lag4 = current_temp_med_lag4,
      umid_med_lag4 = current_umid_med_lag4,
      precip_tot_lag4 = current_precip_tot_lag4,
      temp_competence_optimality = current_temp_competence_optimality,
      humidity_risk = current_humidity_risk,
      temp_competence_humidty_risk = current_temp_competence_humidty_risk,
      sin_year = sin(2 * pi * week_number/52),
      cos_year = cos(2 * pi * week_number/52),
      casoslag1_mov_sd = mov_sd,
      avg = avg,
      wavg = wavg,
      decay = current_decay
    )
  }
  
  return(recursive_data)
}

# Create a function to generate predictions for a range of base weeks
generate_predictions_across_year <- function(start_week, end_week, weeks_ahead, post, y_test, model_type, lambda) {
  # Create a data frame to store all predictions
  all_predictions <- data.frame()
  
  # Loop through each base week
  for (week_base in start_week:end_week) {
    # Initialize vectors for the current set of predictions
    mean_predictions_2023 <- numeric(weeks_ahead)
    li_predictions_2023 <- numeric(weeks_ahead)
    ui_predictions_2023 <- numeric(weeks_ahead)
    
    
    # Initialize vectors for quantiles and metrics
    q1 <- numeric(weeks_ahead)
    q2.5 <- numeric(weeks_ahead)
    q5 <- numeric(weeks_ahead)
    q10 <- numeric(weeks_ahead)
    q25 <- numeric(weeks_ahead)
    q50 <- numeric(weeks_ahead)
    q75 <- numeric(weeks_ahead)
    q90 <- numeric(weeks_ahead)
    q95 <- numeric(weeks_ahead)
    q97.5 <- numeric(weeks_ahead)
    q99 <- numeric(weeks_ahead)
    
    is_50_score <- numeric(weeks_ahead)
    is_50_width <- numeric(weeks_ahead)
    is_50_under <- numeric(weeks_ahead)
    is_50_over <- numeric(weeks_ahead)
    
    is_80_score <- numeric(weeks_ahead)
    is_80_width <- numeric(weeks_ahead)
    is_80_under <- numeric(weeks_ahead)
    is_80_over <- numeric(weeks_ahead)
    
    is_90_score <- numeric(weeks_ahead)
    is_90_width <- numeric(weeks_ahead)
    is_90_under <- numeric(weeks_ahead)
    is_90_over <- numeric(weeks_ahead)
    
    is_95_score <- numeric(weeks_ahead)
    is_95_width <- numeric(weeks_ahead)
    is_95_under <- numeric(weeks_ahead)
    is_95_over <- numeric(weeks_ahead)
    
    wis_score <- numeric(weeks_ahead)
    wis_ae <- numeric(weeks_ahead)
    wis_width <- numeric(weeks_ahead)
    wis_under <- numeric(weeks_ahead)
    wis_over <- numeric(weeks_ahead)
    
    coverage_50 <- numeric(weeks_ahead)
    coverage_80 <- numeric(weeks_ahead)
    coverage_90 <- numeric(weeks_ahead)
    coverage_95 <- numeric(weeks_ahead)
    
    # Get actual values for comparison
    actual_values <- y_test$casos[(week_base + 1):(week_base + weeks_ahead)]
    
    # Generate predictions for each week ahead
    for (week in 1:weeks_ahead) {
      # Generate recursive features
      recursive_data <- generate_recursive_features(week, week_base, mean_predictions_2023, y_test, model_type)
      
      # Make prediction
      quantile_levels <- c(0.01, 0.025, 0.05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 
                           0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.975, 0.99)
      prediction <- predict(post, newdata = recursive_data)
      median_prediction <- InvBoxCox(apply(prediction, 2, median), lambda)
      mean_prediction <- InvBoxCox(apply(prediction, 2, mean), lambda)
      li_prediction <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.025), lambda)
      ui_prediction <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.975), lambda)
      
      # Extract all quantiles
      q1[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.01), lambda)
      q2.5[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.025), lambda)
      q5[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.05), lambda)
      q10[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.1), lambda)
      q25[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.25), lambda)
      q50[week] <- InvBoxCox(apply(prediction, 2, median), lambda)
      q75[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.75), lambda)
      q90[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.9), lambda)
      q95[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.95), lambda)
      q97.5[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.975), lambda)
      q99[week] <- InvBoxCox(apply(prediction, 2, quantile, probs = 0.99), lambda)
      
      # Store standard predictions
      mean_predictions_2023[week] <- mean_prediction
      li_predictions_2023[week] <- li_prediction
      ui_predictions_2023[week] <- ui_prediction
      
      # Create a dictionary-like structure for quantiles
      quantiles <- list(
        q1 = q1[week],
        q2.5 = q2.5[week],
        q5 = q5[week],
        q10 = q10[week],
        q25 = q25[week],
        q50 = q50[week],
        q75 = q75[week],
        q90 = q90[week],
        q95 = q95[week],
        q97.5 = q97.5[week],
        q99 = q99[week]
      )
      
      # Get actual value for this week
      actual <- actual_values[week]
      
      # Calculate interval scores for standard intervals
      # 50% interval
      is_50 <- calculate_interval_score(
        q25[week],
        q75[week],
        actual,
        0.5
      )
      
      # 80% interval
      is_80 <- calculate_interval_score(
        q10[week],
        q90[week],
        actual,
        0.2
      )
      
      # 90% interval
      is_90 <- calculate_interval_score(
        q5[week],
        q95[week],
        actual,
        0.1
      )
      
      # 95% interval
      is_95 <- calculate_interval_score(
        q2.5[week],
        q97.5[week],
        actual,
        0.05
      )
      
      # Store interval scores
      is_50_score[week] = is_50$score
      is_50_width[week] = is_50$width
      is_50_under[week] = is_50$under_penalty
      is_50_over[week] = is_50$over_penalty
      
      is_80_score[week] = is_80$score
      is_80_width[week] = is_80$width
      is_80_under[week] = is_80$under_penalty
      is_80_over[week] = is_80$over_penalty
      
      is_90_score[week] = is_90$score
      is_90_width[week] = is_90$width
      is_90_under[week] = is_90$under_penalty
      is_90_over[week] = is_90$over_penalty
      
      is_95_score[week] = is_95$score
      is_95_width[week] = is_95$width
      is_95_under[week] = is_95$under_penalty
      is_95_over[week] = is_95$over_penalty
      
      # Calculate proper WIS
      wis_result <- calculate_proper_wis(quantiles, actual)
      
      # Store WIS
      wis_score[week] = wis_result$score
      wis_ae[week] = wis_result$absolute_error
      wis_width[week] = wis_result$width
      wis_under[week] = wis_result$under_penalty
      wis_over[week] = wis_result$over_penalty
      
      # Coverage metrics
      coverage_50[week] = as.numeric(actual >= q25[week] & actual <= q75[week])
      coverage_80[week] = as.numeric(actual >= q10[week] & actual <= q90[week])
      coverage_90[week] = as.numeric(actual >= q5[week] & actual <= q95[week])
      coverage_95[week] = as.numeric(actual >= q2.5[week] & actual <= q97.5[week])
    }
    
    # After generating all predictions for this base week, add them to the overall data frame
    for (i in 1:weeks_ahead) {
      prediction_week <- week_base + i
      
      # Skip if prediction week is out of range
      if (prediction_week > 52) {
        next
      }
      
      new_row <- data.frame(
        base_week = week_base,
        prediction_week = prediction_week,
        actual = y_test$casos[prediction_week],
        predicted = mean_predictions_2023[i],
        predicted_median = q50[i],
        lower_ci = li_predictions_2023[i],
        upper_ci = ui_predictions_2023[i],
        
        # Store all quantiles from the vectors we created
        q1 = q1[i],
        q2.5 = q2.5[i],
        q5 = q5[i],
        q10 = q10[i],
        q25 = q25[i],
        q50 = q50[i],
        q75 = q75[i],
        q90 = q90[i],
        q95 = q95[i],
        q97.5 = q97.5[i],
        q99 = q99[i],
        
        # Store interval scores
        is_50_score = is_50_score[i],
        is_50_width = is_50_width[i],
        is_50_under = is_50_under[i],
        is_50_over = is_50_over[i],
        
        is_80_score = is_80_score[i],
        is_80_width = is_80_width[i],
        is_80_under = is_80_under[i],
        is_80_over = is_80_over[i],
        
        is_90_score = is_90_score[i],
        is_90_width = is_90_width[i],
        is_90_under = is_90_under[i],
        is_90_over = is_90_over[i],
        
        is_95_score = is_95_score[i],
        is_95_width = is_95_width[i],
        is_95_under = is_95_under[i],
        is_95_over = is_95_over[i],
        
        # Store WIS components
        wis_score = wis_score[i],
        wis_ae = wis_ae[i],
        wis_width = wis_width[i],
        wis_under = wis_under[i],
        wis_over = wis_over[i],
        
        # Store coverage metrics
        coverage_50 = coverage_50[i],
        coverage_80 = coverage_80[i],
        coverage_90 = coverage_90[i],
        coverage_95 = coverage_95[i],
        
        # Include horizon and absolute error
        horizon = i,
        ae = abs(y_test$casos[prediction_week] - mean_predictions_2023[i])
      )
      
      all_predictions <- rbind(all_predictions, new_row)
    }
    
    # Print progress
    cat(sprintf("Processed base week %d/%d\r", week_base, end_week))
  }
  # Calculate PIT values
  all_predictions$pit <- calculate_pit_enhanced(all_predictions)
  
  return(all_predictions)
}

# Unified grid search function for BART model hyperparameter optimization
grid_search_bart <- function(
    k_range,
    power_range,
    ntree_range,
    start_week,
    end_week,
    weeks_ahead,
    x_train,
    y_train,
    y_test,
    lambda,
    feature_set = "climate_gtrends_historical"
) {
  # Create a dataframe to store results
  results <- data.frame(
    k_value = numeric(),
    power_value = numeric(),
    ntree_value = integer(),
    mean_wis = numeric(),
    mean_ae = numeric(),
    rmse = numeric(),
    mae = numeric(),
    coverage_95 = numeric(),
    file_path = character(),
    stringsAsFactors = FALSE
  )
  
  # Track the best model
  best_model <- list(
    k_value = NA,
    power_value = NA, 
    ntree_value = NA,
    mean_wis = Inf,
    post = NULL,
    predictions = NULL
  )
  
  # Total number of combinations for progress tracking
  total_combinations <- length(k_range) * length(power_range) * length(ntree_range)
  current_combination <- 0
  
  # Map feature_set to model_type for generate_predictions_across_year
  model_type_map <- list(
    "historical" = "historical",
    "climate_gtrends_historical" = "climate_gtrends_historical"
  )
  
  model_type <- model_type_map[[feature_set]]
  
  # Iterate through all combinations
  for (k in k_range) {
    for (power in power_range) {
      for (ntree in ntree_range) {
        # Update progress counter
        current_combination <- current_combination + 1
        model_name <- sprintf("k_%.1f_power_%.1f_ntree_%d", k, power, ntree)
        
        cat(sprintf("\nRunning combination %d/%d: %s\n", 
                    current_combination, total_combinations, model_name))
        
        # Set hyperparameters and train model
        set.seed(7)  # Ensure reproducibility for each run
        burn <- 13000
        nd <- 5000
        
        cat("Training BART model...\n")
        current_post <- wbart(
          x_train, y_train, 
          nskip = burn, 
          ndpost = nd, 
          k = k, 
          power = power, 
          ntree = ntree, 
          sparse = FALSE
        )
        
        # Generate predictions
        cat("Generating predictions...\n")
        all_predictions <- generate_predictions_across_year(
          start_week = start_week, 
          end_week = end_week, 
          weeks_ahead = weeks_ahead,
          post = current_post,
          y_test = y_test,
          model_type = model_type,
          lambda = lambda
        )
        
        # Store metrics
        cat("Storing metrics...\n")
        metrics <- store_model_metrics(
          all_predictions,
          model_name = model_name,
          save_path = getwd()
        )
        
        # Extract overall metrics
        overall_metrics <- metrics$overall_metrics
        horizon_metrics <- metrics$horizon_metrics
        
        # Calculate mean WIS across all horizons for comparison
        mean_wis_value <- mean(horizon_metrics$mean_wis)
        mean_ae_value <- mean(horizon_metrics$mean_ae)
        mean_rmse <- mean(horizon_metrics$rmse)
        mean_mae <- mean(horizon_metrics$mae)
        mean_coverage <- mean(horizon_metrics$coverage_95)
        
        # Using glue to create the folder path
        folder_path <- glue("{toupper(feature_set)}_{model_name}")
        
        # Create the directory if it doesn't exist
        if (!dir.exists(glue("{folder_path}"))) {
          dir.create(folder_path, recursive = TRUE)
          print(glue("Created folder: {folder_path}"))
        }
        
        # Now save the RDS file inside the folder
        saveRDS(current_post, glue("{folder_path}/MODEL_{model_name}.rds"))
        saveRDS(all_predictions, glue("{folder_path}/PREDICTIONS_{model_name}.rds"))
        saveRDS(x_train, glue("{folder_path}/X_TRAIN_{model_name}.rds"))
        saveRDS(y_train, glue("{folder_path}/Y_TRAIN_{model_name}.rds"))
        
        # Add to results dataframe
        results <- rbind(results, data.frame(
          k_value = k,
          power_value = power,
          ntree_value = ntree,
          mean_wis = mean_wis_value,
          mean_ae = mean_ae_value,
          rmse = mean_rmse,
          mae = mean_mae,
          coverage_95 = mean_coverage,
          file_path = metrics$files$horizon_file
        ))
        
        # Save intermediate results
        write.csv(results, glue("{toupper(feature_set)}_grid_search_results.csv"), row.names = FALSE)
        
        # Check if this is the best model so far
        if (mean_wis_value < best_model$mean_wis) {
          cat(sprintf("New best model found! Mean WIS: %.4f\n", mean_wis_value))
          best_model$k_value <- k
          best_model$power_value <- power
          best_model$ntree_value <- ntree
          best_model$mean_wis <- mean_wis_value
          best_model$post <- current_post
          best_model$predictions <- all_predictions
          
          # Save the best model
          saveRDS(current_post, glue("best_{feature_set}_bart_model.rds"))
          saveRDS(all_predictions, glue("best_{feature_set}_model_predictions.rds"))
        }
      }
    }
  }
  
  # Sort results by mean_wis (ascending)
  results <- results[order(results$mean_wis), ]
  
  # Save final results
  write.csv(results, glue("{toupper(feature_set)}_final_grid_search_results.csv"), row.names = FALSE)
  
  # Print the best hyperparameters
  cat("\n===== GRID SEARCH COMPLETED =====\n")
  cat(sprintf("Feature set: %s\n", feature_set))
  cat(sprintf("Best hyperparameters found:\n"))
  cat(sprintf("k_value: %.1f\n", best_model$k_value))
  cat(sprintf("power_value: %.1f\n", best_model$power_value))
  cat(sprintf("ntree_value: %d\n", best_model$ntree_value))
  cat(sprintf("Mean WIS: %.4f\n", best_model$mean_wis))
  
  return(list(
    results = results,
    best_model = best_model
  ))
}

# Create animated visualization
create_prediction_animation_year <- function(all_predictions, output_format, output_file, y_test) {
  # Create a dataframe with all actual values for reference
  full_actual <- data.frame(
    week = 1:52,
    actual = y_test$casos
  )
  
  # Get the unique base weeks to use as animation frames
  base_week <- unique(all_predictions$base_week)
  
  # Create the plot
  p <- ggplot() +
    # Plot full year of actual values
    geom_line(data = full_actual, aes(x = week, y = actual), color = "black", alpha = 0.5) +
    geom_point(data = full_actual, aes(x = week, y = actual), color = "black", alpha = 0.5, size = 1) +
    
    # Plot predictions for current base week
    geom_ribbon(data = all_predictions, 
                aes(x = prediction_week, ymin = lower_ci, ymax = upper_ci,
                    group = base_week), 
                fill = "red", alpha = 0.2) +
    geom_line(data = all_predictions, 
              aes(x = prediction_week, y = predicted, group = base_week), 
              color = "red", size = 1) +
    geom_point(data = all_predictions, 
               aes(x = prediction_week, y = predicted, group = base_week), 
               color = "red", size = 2) +
    
    # Add labels and theme
    labs(title = "Dengue Case Predictions for 2023",
         subtitle = "Base Week: {closest_state}",
         x = "Week of Year",
         y = "Number of Cases",
         caption = "Actual (black) vs Predictions (red) | RMSE: {predictions_data[predictions_data$base_week == closest_state, 'rmse'][1]}") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)) +
    
    # Set up animation
    transition_states(base_week, 
                      transition_length = 2, 
                      state_length = 3) +
    ease_aes('cubic-in-out')
  
  # Render and save the animation
  animated_plot <- animate(p, 
                           nframes = length(base_week) * 5, 
                           fps = 5, 
                           width = 1280, 
                           height = 720,
                           renderer = if(output_format == "gif") {
                             gifski_renderer(output_file)
                           } else if(output_format == "mp4") {
                             av_renderer(output_file)
                           }
  )
  
  return(animated_plot)
}

# Plot full year
plot_full_year <- function(predictions, y_test) {
  p <- ggplot() +
    # Plot the actual values as a black line
    geom_line(data = data.frame(week = 1:52, actual = y_test$casos),
              aes(x = week, y = actual), color = "black", size = 1) +
    geom_point(data = data.frame(week = 1:52, actual = y_test$casos),
               aes(x = week, y = actual), color = "black", size = 2) +
    
    # Add the predictions as red points with confidence intervals
    geom_ribbon(data = predictions, 
                aes(x = prediction_week, ymin = lower_ci, ymax = upper_ci), 
                fill = "red", alpha = 0.2) +
    geom_line(data = predictions, 
              aes(x = prediction_week, y = predicted, group = base_week), 
              color = "red", linetype = "dashed", size = 0.5) +
    geom_point(data = predictions, 
               aes(x = prediction_week, y = predicted), 
               color = "red", size = 2) +
    
    # Add labels and formatting
    labs(title = "Dengue Case Predictions vs Actual Values (2023)",
         x = "Week of Year",
         y = "Number of Cases",
         caption = "Actual (black) vs Predictions (red with confidence intervals)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  plot(p)
  
}

# Plot var imp
plot_var_imp <- function(model, feature_names) {
  # Assuming 'post' is your trained BART model and x_train has column names
  var_imp <- model$varcount.mean
  
  # Normalize to percentages
  var_imp_perc <- (var_imp/sum(var_imp)) * 100
  
  names(var_imp) <- feature_names
  
  # Create a data frame for plotting (much easier with ggplot2)
  importance_df <- data.frame(
    Variable = names(var_imp),  # Use names from your training data
    Importance = var_imp_perc
  )
  
  # ggplot2 bar plot with labels
  ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) + # reorder for descending order
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() + # Horizontal bars for easier reading of labels
    labs(x = "Variável",
         y = "Importância") +
    theme_bw() # Clean theme
  
}

# Calculates the interval score for a single prediction interval
calculate_interval_score <- function(lower, upper, actual, alpha) {
  interval_width <- upper - lower
  under_penalty <- 2/alpha * (lower - actual) * (actual < lower)
  over_penalty <- 2/alpha * (actual - upper) * (actual > upper)
  
  is_score <- interval_width + under_penalty + over_penalty
  
  return(list(
    score = is_score,
    width = interval_width,
    under_penalty = under_penalty,
    over_penalty = over_penalty
  ))
}

# Calculates the weighted interval score using multiple prediction intervals
calculate_proper_wis <- function(quantiles_list, actual) {
  # Extract quantiles from the list
  all_quantiles <- c(
    quantiles_list$q1,
    quantiles_list$q2.5,
    quantiles_list$q5,
    quantiles_list$q10, 
    quantiles_list$q25,
    quantiles_list$q50,
    quantiles_list$q75,
    quantiles_list$q90,
    quantiles_list$q95,
    quantiles_list$q97.5,
    quantiles_list$q99
  )
  
  # Define the corresponding levels
  all_levels <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99)
  
  # Sort them
  sorted_indices <- order(all_levels)
  quantile_levels <- all_levels[sorted_indices]
  quantile_values <- all_quantiles[sorted_indices]
  
  # Extract median
  median_value <- quantiles_list$q50
  
  # Identify pairs of quantiles for central prediction intervals
  n_quantiles <- length(quantile_levels)
  mid_point <- which(quantile_levels >= 0.5)[1]
  
  interval_pairs <- list()
  interval_alphas <- c()
  
  # Create symmetric interval pairs
  for (i in 1:(mid_point-1)) {
    upper_idx <- (n_quantiles + 1) - i
    
    # Calculate the alpha (complement of the interval coverage)
    alpha <- (1 - (quantile_levels[upper_idx] - quantile_levels[i]))
    
    interval_pairs[[length(interval_pairs) + 1]] <- c(i, upper_idx)
    interval_alphas <- c(interval_alphas, alpha)
  }
  
  # Calculate all interval scores
  is_values <- numeric(length(interval_pairs))
  widths <- numeric(length(interval_pairs))
  under_penalties <- numeric(length(interval_pairs))
  over_penalties <- numeric(length(interval_pairs))
  
  for (i in 1:length(interval_pairs)) {
    pair <- interval_pairs[[i]]
    alpha <- interval_alphas[i]
    
    lower <- quantile_values[pair[1]]
    upper <- quantile_values[pair[2]]
    
    # Calculate interval score components
    width <- upper - lower
    under_penalty <- 2/alpha * (lower - actual) * (actual < lower)
    over_penalty <- 2/alpha * (actual - upper) * (actual > upper)
    is_score <- width + under_penalty + over_penalty
    
    # Store values
    is_values[i] <- is_score
    widths[i] <- width
    under_penalties[i] <- under_penalty
    over_penalties[i] <- over_penalty
  }
  
  # Calculate absolute error for median
  ae <- abs(actual - median_value)
  
  # Weights for the WIS calculation
  # Alpha/2 for intervals, 1/2 for the median (as per the paper)
  interval_weights <- interval_alphas / 2
  median_weight <- 0.5
  all_weights <- c(median_weight, interval_weights)
  
  # Calculate the weighted components
  weighted_ae <- median_weight * ae
  weighted_widths <- sum(interval_weights * widths)
  weighted_unders <- sum(interval_weights * under_penalties)
  weighted_overs <- sum(interval_weights * over_penalties)
  
  # Calculate WIS
  wis <- (weighted_ae + weighted_widths + weighted_unders + weighted_overs) / (median_weight + sum(interval_weights))
  
  # Return the components
  return(list(
    score = wis,
    absolute_error = ae,
    width = weighted_widths / sum(interval_weights),
    under_penalty = weighted_unders / sum(interval_weights),
    over_penalty = weighted_overs / sum(interval_weights)
  ))
}

# Enhanced PIT calculation with more quantiles
calculate_pit_enhanced <- function(all_predictions) {
  # Extract actual values and predictive distribution quantiles
  pit_values <- numeric(nrow(all_predictions))
  
  # Define all quantile columns
  quantile_cols <- c("q1", "q2.5", "q5", "q10", "q25", "q50", "q75", "q90", "q95", "q97.5", "q99")
  quantile_levels <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99)
  
  for (i in 1:nrow(all_predictions)) {
    actual <- all_predictions$actual[i]
    
    # Extract all quantiles for this row
    quantiles <- as.numeric(all_predictions[i, quantile_cols])
    
    # Find where the actual value falls
    if (actual <= quantiles[1]) {
      # Below lowest quantile
      pit_values[i] <- runif(1, 0, quantile_levels[1])
    } else if (actual >= quantiles[length(quantiles)]) {
      # Above highest quantile
      pit_values[i] <- runif(1, quantile_levels[length(quantiles)], 1)
    } else {
      # Find the interval containing the actual value
      for (j in 1:(length(quantiles)-1)) {
        if (actual >= quantiles[j] && actual <= quantiles[j+1]) {
          # Linear interpolation within the interval
          width <- quantile_levels[j+1] - quantile_levels[j]
          position <- (actual - quantiles[j]) / (quantiles[j+1] - quantiles[j])
          pit_values[i] <- quantile_levels[j] + width * position
          break
        }
      }
    }
  }
  
  return(pit_values)
}

plot_wis_decomposition <- function(all_predictions) {
  # Aggregate the components by horizon
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(RColorBrewer)
  
  # Get unique horizons and sort them
  horizons <- sort(unique(all_predictions$horizon))
  
  # Calculate mean values for each component by horizon
  wis_summary <- all_predictions %>%
    group_by(horizon) %>%
    summarize(
      width = mean(wis_width, na.rm = TRUE),
      under = mean(wis_under, na.rm = TRUE),
      over = mean(wis_over, na.rm = TRUE),
      ae = mean(wis_ae, na.rm = TRUE),
      total = mean(wis_score, na.rm = TRUE)
    )
  
  # Reshape data for stacked bar plot
  wis_long <- wis_summary %>%
    pivot_longer(
      cols = c(ae, width, under, over),
      names_to = "component",
      values_to = "value"
    ) %>%
    mutate(component = factor(component, 
                              levels = c("ae", "width", "under", "over"),
                              labels = c("Erro Absoluto", "Largura do Intervalo", 
                                         "Penalidade por Subestimação", "Penalidade por Superestimação")))
  
  # Set up colors (matching the RColorBrewer "Set2" palette used in the original)
  colors <- brewer.pal(4, "Set2")
  
  # Create the plot
  p <- ggplot() +
    # Add stacked bars for components
    geom_bar(data = wis_long, 
             aes(x = factor(horizon), y = value, fill = component),
             stat = "identity", position = "stack", width = 0.7) +
    
    # Add line for total WIS
    geom_line(data = wis_summary,
              aes(x = factor(horizon), y = total, group = 1),
              color = "black", size = 1) +
    
    # Add points for total WIS
    geom_point(data = wis_summary,
               aes(x = factor(horizon), y = total),
               color = "black", size = 3) +
    
    # Add text labels for total values
    geom_text(data = wis_summary,
              aes(x = factor(horizon), 
                  y = total + max(total) * 0.05,
                  label = round(total, 1)),
              size = 3.5) +
    
    # Set colors, labels, and theme
    scale_fill_manual(values = colors) +
    labs(
      x = "Horizonte (semanas)",
      y = "Magnitude do componente",
      fill = "Componente") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray90")
    )
  
  return(p)
}

# Coverage plot for multiple interval levels
plot_multi_coverage <- function(all_predictions) {
  # Calculate coverage by horizon for different interval levels
  coverage_data <- all_predictions %>%
    group_by(horizon) %>%
    summarize(
      coverage_50 = mean(coverage_50, na.rm = TRUE),
      coverage_80 = mean(coverage_80, na.rm = TRUE),
      coverage_90 = mean(coverage_90, na.rm = TRUE),
      coverage_95 = mean(coverage_95, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = starts_with("coverage_"),
                 names_to = "interval",
                 values_to = "coverage") %>%
    mutate(
      nominal_level = as.numeric(gsub("coverage_", "", interval)) / 100,
      interval = factor(interval, 
                        levels = c("coverage_50", "coverage_80", "coverage_90", "coverage_95"),
                        labels = c("50%", "80%", "90%", "95%"))
    )
  
  # Create coverage plot
  p <- ggplot(coverage_data, aes(x = horizon, y = coverage, color = interval, group = interval)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_line(aes(y = nominal_level, group = interval), linetype = "dashed") +
    facet_wrap(~ interval) +
    labs(
      x = "Horizonte (semanas)",
      y = "Cobertura empírica",
      color = "Cobertura"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return(p)
}

# PIT Histogram
plot_pit_histogram <- function(all_predictions, model_name = "BART", bins = 10) {
  p <- ggplot(all_predictions, aes(x = pit)) +
    geom_histogram(bins = bins, fill = "steelblue", color = "black") +
    labs(
      x = "Transformada Integral de Probabilidade",
      y = "Frequência"
    ) +
    geom_hline(yintercept = nrow(all_predictions) / bins, linetype = "dashed", color = "red") +
    theme_minimal()
  
  return(p)
}

# Plot calibration curve - showing predicted vs actual values
plot_calibration <- function(all_predictions, model_name = "BART") {
  p <- ggplot(all_predictions, aes(x = predicted, y = actual)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      x = "Valores Preditos",
      y = "Valores Reais"
    ) +
    theme_minimal()
  
  return(p)
}

# Simple metrics by point predicted
horizon_metrics <- function(predictions) {
  horizon_metrics <- predictions %>%
    group_by(horizon) %>%
    summarize(
      mean_ae = mean(ae),
      mean_wis = mean(wis_score),
      coverage_95 = mean(actual >= lower_ci & actual <= upper_ci)
    )
  
  return (horizon_metrics)
}

# Function to calculate, store, and retrieve model evaluation metrics
store_model_metrics <- function(all_predictions, model_name, save_path) {
  # If model_name is not provided, use a timestamp
  if (is.null(model_name)) {
    model_name <- format(Sys.time(), "model_%Y%m%d_%H%M%S")
  }
  
  # If save_path is not provided, use the current working directory
  if (is.null(save_path)) {
    save_path <- getwd()
  }
  
  # Calculate metrics by horizon
  horizon_metrics <- all_predictions %>%
    group_by(horizon) %>%
    summarize(
      mean_ae = mean(ae, na.rm = TRUE),
      mean_wis = mean(wis_score, na.rm = TRUE),
      coverage_95 = mean(actual >= lower_ci & actual <= upper_ci, na.rm = TRUE),
      rmse = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
      mae = mean(abs(actual - predicted), na.rm = TRUE),
      mape = mean(abs((actual - predicted) / actual) * 100, na.rm = TRUE)
    )
  
  # Calculate overall metrics
  overall_metrics <- all_predictions %>%
    summarize(
      mean_ae = mean(ae, na.rm = TRUE),
      mean_wis = mean(wis_score, na.rm = TRUE),
      coverage_95 = mean(actual >= lower_ci & actual <= upper_ci, na.rm = TRUE),
      rmse = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
      mae = mean(abs(actual - predicted), na.rm = TRUE),
      mape = mean(abs((actual - predicted) / actual) * 100, na.rm = TRUE)
    )
  
  # Add model name and horizon information
  horizon_metrics$model_name <- model_name
  overall_metrics$model_name <- model_name
  overall_metrics$horizon <- "overall"
  
  # Combine horizon-specific and overall metrics
  all_metrics <- horizon_metrics
  
  # Create metrics directory if it doesn't exist
  metrics_dir <- file.path(save_path, "model_metrics")
  if (!dir.exists(metrics_dir)) {
    dir.create(metrics_dir, recursive = TRUE)
  }
  
  # File paths for metrics
  horizon_file <- file.path(metrics_dir, paste0(model_name, "_horizon_metrics.csv"))
  overall_file <- file.path(metrics_dir, paste0(model_name, "_overall_metrics.csv"))
  combined_file <- file.path(metrics_dir, paste0(model_name, "_all_metrics.csv"))
  all_models_file <- file.path(metrics_dir, "all_models_comparison.csv")
  
  # Save individual metrics files
  write.csv(horizon_metrics, horizon_file, row.names = FALSE)
  
  # Return the metrics as a list
  return(list(
    horizon_metrics = horizon_metrics,
    overall_metrics = overall_metrics,
    all_metrics = all_metrics,
    files = list(
      horizon_file = horizon_file,
      overall_file = overall_file,
      combined_file = combined_file,
      all_models_file = all_models_file
    )
  ))
}

# Function to visualize grid search results
visualize_grid_search_results <- function(results_file = "final_grid_search_results.csv") {
  # Load results
  results <- read.csv(results_file)
  
  # Create summary plots
  library(ggplot2)
  library(gridExtra)
  
  # 1. Heatmap of mean_wis for k_value vs power_value (averaged over ntree_value)
  heatmap_k_power <- results %>%
    group_by(k_value, power_value) %>%
    summarize(mean_wis = mean(mean_wis)) %>%
    ggplot(aes(x = k_value, y = power_value, fill = mean_wis)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    labs(title = "Mean WIS by k and power values",
         x = "k value", y = "power value") +
    theme_minimal()
  
  # 2. Heatmap of mean_wis for k_value vs ntree_value (averaged over power_value)
  heatmap_k_ntree <- results %>%
    group_by(k_value, ntree_value) %>%
    summarize(mean_wis = mean(mean_wis)) %>%
    ggplot(aes(x = k_value, y = ntree_value, fill = mean_wis)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    labs(title = "Mean WIS by k and ntree values",
         x = "k value", y = "ntree value") +
    theme_minimal()
  
  # 3. Heatmap of mean_wis for power_value vs ntree_value (averaged over k_value)
  heatmap_power_ntree <- results %>%
    group_by(power_value, ntree_value) %>%
    summarize(mean_wis = mean(mean_wis)) %>%
    ggplot(aes(x = power_value, y = ntree_value, fill = mean_wis)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    labs(title = "Mean WIS by power and ntree values",
         x = "power value", y = "ntree value") +
    theme_minimal()
  
  # 4. Boxplot of mean_wis by ntree_value
  boxplot_ntree <- ggplot(results, aes(x = factor(ntree_value), y = mean_wis)) +
    geom_boxplot() +
    labs(title = "Distribution of Mean WIS by Number of Trees",
         x = "Number of Trees", y = "Mean WIS") +
    theme_minimal()
  
  # 5. Find the top 5 best models
  top_models <- results %>%
    arrange(mean_wis) %>%
    head(5)
  
  # Print the top 5 models
  print("Top 5 Models by Mean WIS:")
  print(top_models)
  
  # Combine plots
  grid.arrange(heatmap_k_power, heatmap_k_ntree, heatmap_power_ntree, boxplot_ntree, ncol = 2)
  
  # Return the visualization objects
  return(list(
    heatmap_k_power = heatmap_k_power,
    heatmap_k_ntree = heatmap_k_ntree,
    heatmap_power_ntree = heatmap_power_ntree,
    boxplot_ntree = boxplot_ntree,
    top_models = top_models
  ))
}

plot_multi_baseweek_predictions <- function(all_predictions, y_test, selected_base_weeks = NULL, 
                                            max_base_weeks = 5, prediction_color_palette = "colorbrewer",
                                            highlight_weeks = NULL, show_ci = TRUE) {
  require(ggplot2)
  require(dplyr)
  require(viridis)  # For viridis color palette
  require(RColorBrewer)
  
  # Create a dataframe with all actual values for reference
  full_actual <- data.frame(
    week = 1:nrow(y_test),
    actual = y_test$casos
  )
  
  # Get unique base_weeks, sorted
  all_base_weeks <- sort(unique(all_predictions$base_week))
  
  # If specific base weeks are not provided, select evenly distributed ones
  if (is.null(selected_base_weeks)) {
    if (length(all_base_weeks) > max_base_weeks) {
      # Select evenly spaced base weeks
      step_size <- floor(length(all_base_weeks) / max_base_weeks)
      indices <- seq(1, length(all_base_weeks), by = step_size)
      selected_base_weeks <- all_base_weeks[indices[1:min(max_base_weeks, length(indices))]]
    } else {
      selected_base_weeks <- all_base_weeks
    }
  } else {
    # Ensure selected_base_weeks are valid
    selected_base_weeks <- selected_base_weeks[selected_base_weeks %in% all_base_weeks]
    if (length(selected_base_weeks) == 0) {
      stop("None of the provided base weeks are valid!")
    }
  }
  
  # Filter predictions to only include selected base weeks
  filtered_predictions <- all_predictions %>%
    filter(base_week %in% selected_base_weeks)
  
  # Create a custom label for each base week
  filtered_predictions$base_week_label <- paste0("Semana ", filtered_predictions$base_week)
  
  # Setup colors - either viridis or a colorbrewer palette
  n_colors <- length(selected_base_weeks)
  if (prediction_color_palette == "viridis") {
    color_values <- viridis(n_colors, option = "viridis")
  } else if (prediction_color_palette %in% rownames(brewer.pal.info)) {
    max_colors <- brewer.pal.info[prediction_color_palette, "maxcolors"]
    if (n_colors <= max_colors) {
      color_values <- brewer.pal(n_colors, prediction_color_palette)
    } else {
      color_values <- colorRampPalette(brewer.pal(max_colors, prediction_color_palette))(n_colors)
    }
  } else {
    color_values <- rainbow(n_colors)
  }
  
  # Create the plot
  p <- ggplot() +
    # Plot full year of actual values
    geom_line(data = full_actual, aes(x = week, y = actual), 
              color = "black", size = 1, alpha = 0.9) +
    geom_point(data = full_actual, aes(x = week, y = actual), 
               color = "black", alpha = 0.8, size = 2) +
    
    # Add confidence intervals if requested
    {if(show_ci) geom_ribbon(data = filtered_predictions, 
                             aes(x = prediction_week, 
                                 ymin = lower_ci, 
                                 ymax = upper_ci,
                                 fill = base_week_label), 
                             alpha = 0.1)} +
    
    # Plot predictions for each base week
    geom_line(data = filtered_predictions, 
              aes(x = prediction_week, y = predicted, 
                  color = base_week_label, group = base_week),
              size = 0.9, alpha = 0.8) +
    
    geom_point(data = filtered_predictions, 
               aes(x = prediction_week, y = predicted, 
                   color = base_week_label),
               size = 2.5, alpha = 0.7) +
    
    # Highlight the base weeks with vertical lines
    geom_vline(data = data.frame(base_week = selected_base_weeks,
                                 base_week_label = paste0("Semana ", selected_base_weeks)),
               aes(xintercept = base_week, color = base_week_label),
               linetype = "dashed", size = 0.7, alpha = 0.7) +
    
    # Highlight specific weeks if provided
    {if(!is.null(highlight_weeks)) 
      geom_vline(xintercept = highlight_weeks, 
                 color = "darkred", linetype = "dotted", size = 1)} +
    
    # Add labels and theme
    labs(x = "Semana",
         y = "Número de Casos",
         color = "Semana Base",
         fill = "Semana Base") +
    
    scale_color_manual(values = color_values) +
    scale_fill_manual(values = color_values) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray90")
    )
  
  # Add specialized annotations if needed
  if (!is.null(highlight_weeks)) {
    # Add labels for highlighted weeks
    highlight_data <- data.frame(
      week = highlight_weeks,
      y = max(full_actual$actual) * 1.05,
      label = paste("Semana", highlight_weeks)
    )
    
    p <- p + 
      geom_text(data = highlight_data,
                aes(x = week, y = y, label = label),
                color = "darkred", size = 3.5, fontface = "bold")
  }
  
  # Add performance metrics for each base week
  performance_summary <- filtered_predictions %>%
    group_by(base_week, base_week_label) %>%
    summarize(
      RMSE = sqrt(mean((actual - predicted)^2)),
      MAE = mean(abs(actual - predicted)),
      Mean_WIS = mean(wis_score),
      .groups = "drop"
    )
  
  # Create a small table-like visualization for metrics
  p_metrics <- ggplot(performance_summary, 
                      aes(y = reorder(base_week_label, base_week))) +
    geom_text(aes(x = 1, label = sprintf("Week %d", base_week)), 
              hjust = 0, size = 3.5) +
    geom_text(aes(x = 2, label = sprintf("RMSE: %.1f", RMSE)), 
              hjust = 0, size = 3.5) +
    geom_text(aes(x = 3, label = sprintf("MAE: %.1f", MAE)), 
              hjust = 0, size = 3.5) +
    geom_text(aes(x = 4, label = sprintf("WIS: %.1f", Mean_WIS)), 
              hjust = 0, size = 3.5) +
    scale_x_continuous(limits = c(0.5, 5)) +
    theme_void() +
    labs(title = "Performance Metrics by Base Week") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  return(list(
    main_plot = p,
    metrics_plot = p_metrics
  ))
}

# MAIN EXECUTION FLOW
set.seed(7)

# CREATING IMPORTANT VARIABLES #
gtrends_results <- collect_and_normalize_gtrends()
climate_data <- api_calls(api_name = "climate")
dengue_data_ordered <- api_calls(api_name = "dengue")

# Feature engineering
engineered_data <- feature_engineering(dengue_data_ordered, climate_data, gtrends_results)
dengue_data_engineered <- engineered_data$dengue_data_engineered
climate_data_engineered <- engineered_data$climate_data_engineered

# Create train/test splits for different feature sets
splits_climate_gtrends <- create_train_test_splits(dengue_data_engineered, climate_data_engineered, 
                                                   feature_set = "climate_gtrends_historical")

x_train <- splits_climate_gtrends$x_train_df
y_train <- splits_climate_gtrends$y_train
y_test <- splits_climate_gtrends$y_test
y_validation <- splits_climate_gtrends$y_validation
lambda <- splits_climate_gtrends$lambda

# best_model <- readRDS("models-rj/best_model/MODEL_k_2.5_power_0.5_ntree_200.rds")
# best_x_train <- readRDS("models-rj/best_model/X_TRAIN_k_2.5_power_0.5_ntree_200.rds")
# best_y_train <- readRDS("models-rj/best_model/Y_TRAIN_k_2.5_power_0.5_ntree_200.rds")

# all.equal(best_x_train, x_train)

# Train the model from the ground up
post_model <- bart_model(x_train, y_train, k = 2.5, power = 0.5, ntree = 200L)

# Generate predictions for all base weeks
all_predictions <- generate_predictions_across_year(
  start_week = 6, 
  end_week = 6,
  weeks_ahead = 4, 
  post_model, 
  y_test, 
  "climate_gtrends_historical",
  lambda
)

# Example plots and analysis
# print(horizon_metrics(all_predictions))
plot_full_year(all_predictions, y_test)
plot_var_imp(post_model, splits_climate_gtrends$feature_names)
plot_wis_decomposition(all_predictions)
plot_multi_coverage(all_predictions)
plot_pit_histogram(all_predictions)
plot_calibration(all_predictions)

results <- plot_multi_baseweek_predictions(all_predictions, y_test, 
                                           selected_base_weeks = c(6, 12, 23, 35, 46))
print(results$main_plot)

# Store the metrics
model_metrics <- store_model_metrics(
  all_predictions,
  model_name = "best_climate_gtrends_historical",
  save_path = getwd()
)

# Example grid search for different feature sets
if (TRUE) {  # Set to TRUE to run grid search
  # # Historical features only
  # splits_historical <- create_train_test_splits(dengue_data_engineered, climate_data_engineered, 
  #                                               feature_set = "historical")
  # 
  # results_historical <- grid_search_bart(
  #   k_range = seq(0.5, 3, by = 0.5),
  #   power_range = seq(0.5, 3, by = 0.5),
  #   ntree_range = seq(50, 250, by = 50),
  #   start_week = 4,
  #   end_week = 48,
  #   weeks_ahead = 4,
  #   x_train = splits_historical$x_train_df,
  #   y_train = splits_historical$y_train,
  #   y_test = splits_historical$y_test,
  #   lambda = splits_historical$lambda,
  #   feature_set = "historical"
  # )
  
  # All features combined
  results_all <- grid_search_bart(
    k_range = seq(2.5, 3, by = 0.5),
    power_range = seq(1, 2, by = 0.5),
    ntree_range = seq(100, 200, by = 50),
    start_week = 4,
    end_week = 48,
    weeks_ahead = 4,
    x_train = splits_climate_gtrends$x_train_df,
    y_train = splits_climate_gtrends$y_train,
    y_test = splits_climate_gtrends$y_test,
    lambda = splits_climate_gtrends$lambda,
    feature_set = "climate_gtrends_historical"
  )
  
  # Visualize results
  # vis <- visualize_grid_search_results("CLIMATE_GTRENDS_HISTORICAL_final_grid_search_results.csv")
}
