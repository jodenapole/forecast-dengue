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
library(tidyr)

# Get API key
API_KEY <- Sys.getenv("MOSQLIMATE_API_KEY")

# FUNCTIONS #

# Calling API for climate and dengue
api_calls <- function(api_name) {
  if (api_name == "climate") {
    # API CALL FOR CLIMATE 2019 - 2023
    climate_weekly_api <- "https://api.mosqlimate.org/api/datastore/climate/weekly/"
    climate_data <- data.frame()
    params <- list(
      page = 1,
      per_page = 300,
      start = 201901,
      end = 202352,
      geocode = 3304557
      # uf = UF,
    )
    headers <- add_headers(
      `X-UID-Key` = API_KEY
    )
    resp <- GET(climate_weekly_api, query = params, headers)
    json_content <- fromJSON(content(resp, "text"))
    items <- json_content$items
    climate_data <- rbind(climate_data, items)
    climate_data$week_of_year <- as.numeric(substr(climate_data$epiweek, 5, 6))
    
    api_response <- climate_data
  }
  
  
  if (api_name == "dengue") {
    # API CALL FOR DENGUE CASES 2019 - 2023
    dengue_data <- data.frame()
    dengue_api <- "https://api.mosqlimate.org/api/datastore/infodengue/?disease=dengue"
    date <- paste0("&start=2018-12-30&end=2023-12-30")
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

# CREATING FEATURES
feature_creation <- function() {
  # Temperature-based competence optimality scale (1-3)
  create_temp_competence_optimality <- function(temp_data) {
    # Initialize result vector with same length as input
    competence_scores <- numeric(length(temp_data))
    
    # Apply the optimality scale based on temperature ranges
    for (i in 1:length(temp_data)) {
      temp <- temp_data[i]
      
      # Apply the simplified vector competence optimality scale
      if ((temp >= 25 && temp <= 32)) {
        # High transmission efficiency (combined your two highest categories)
        competence_scores[i] <- 3
      } else if ((temp >= 23 && temp < 25) || (temp > 32 && temp <= 34)) {
        # Moderate transmission efficiency
        competence_scores[i] <- 2
      } else {
        # Low transmission efficiency (combined your two lowest categories)
        competence_scores[i] <- 1
      }
    }
    
    return(competence_scores)
  }
  
  # Humidity-based risk scale (1-3) 
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
  
  # Create binary feature for optimal combined conditions
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
  
  # Vectorized approach
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
      list("umid_med_lag4", "avg_umidmed_lag4")
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
  
  
  
  # LAGGED CASES
  dengue_data_ordered$casos_lag1 <-  dplyr::lag(dengue_data_ordered$casos, 1)
  dengue_data_ordered$casos_lag1[1] = 147
  
  dengue_data_ordered$casos_lag2 <-  dplyr::lag(dengue_data_ordered$casos, 2)
  dengue_data_ordered$casos_lag2[1] = 124 # Same logic applies
  dengue_data_ordered$casos_lag2[2] = 147 # Same logic applies
  
  dengue_data_ordered$casos_lag3 <-  dplyr::lag(dengue_data_ordered$casos, 3)
  dengue_data_ordered$casos_lag3[1] = 138 # Same logic applies
  dengue_data_ordered$casos_lag3[2] = 124 # Same logic applies
  dengue_data_ordered$casos_lag3[3] = 147 # Same logic applies
  
  dengue_data_ordered$casos_lag4 <-  dplyr::lag(dengue_data_ordered$casos, 4)
  dengue_data_ordered$casos_lag4[1] = 150 # Same logic applies
  dengue_data_ordered$casos_lag4[2] = 138 # Same logic applies
  dengue_data_ordered$casos_lag4[3] = 124 # Same logic applies
  dengue_data_ordered$casos_lag4[4] = 147 # Same logic applies
  
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
  
  # Calculate average lag values by week of year
  temp_weekly_avg <- dengue_data_ordered %>%
    group_by(week_of_year) %>%
    summarize(
      avg_tempmed_lag1 = mean(temp_med_lag1, na.rm = TRUE),
      avg_umidmed_lag1 = mean(umid_med_lag1, na.rm = TRUE),
      
      avg_tempmed_lag2 = mean(temp_med_lag2, na.rm = TRUE),
      avg_umidmed_lag2 = mean(umid_med_lag2, na.rm = TRUE),
      
      avg_tempmed_lag3 = mean(temp_med_lag3, na.rm = TRUE),
      avg_umidmed_lag3 = mean(umid_med_lag3, na.rm = TRUE),
      
      avg_tempmed_lag4 = mean(temp_med_lag4, na.rm = TRUE),
      avg_umidmed_lag4 = mean(umid_med_lag4, na.rm = TRUE)
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
  
  # TRAIN TEST SPLIT
  dengue_data_train <- subset(dengue_data_ordered, SE <202301)
  dengue_data_test <- subset(dengue_data_ordered, SE >202252)
  climate_data_train <- subset(climate_data, epiweek <202301)
  climate_data_test <- subset(climate_data, epiweek >202252)
  
  # ORGANAZING DATA IN TRAIN SAMPLES
  x_train <- cbind(
    dengue_data_train$casos_lag1,
    dengue_data_train$casos_lag2,
    dengue_data_train$casos_lag3,
    dengue_data_train$casos_lag4,
    # dengue_data_train$temp_med_lag1,
    # dengue_data_train$temp_med_lag2,
    # dengue_data_train$temp_med_lag3,
    # dengue_data_train$temp_med_lag4,
    # dengue_data_train$umid_med_lag1,
    # dengue_data_train$umid_med_lag2,
    # dengue_data_train$umid_med_lag3,
    # dengue_data_train$umid_med_lag4,
    # dengue_data_train$temp_competence_optimality,
    # dengue_data_train$humidity_risk,
    # dengue_data_train$optimal_conditions,
    # dengue_data_train$temp_competence_humidty_risk,
    dengue_data_train$sin_year,
    dengue_data_train$cos_year,
    dengue_data_train$casoslag1_mov_sd,
    dengue_data_train$avg,
    dengue_data_train$wavg,
    dengue_data_train$decay
  )
  
  x_train_df <- as.data.frame(x_train)
  
  colnames(x_train_df) <- c(
    "casos_lag1",
    "casos_lag2",
    "casos_lag3",
    "casos_lag4",
    # "temp_med_lag1",
    # "temp_med_lag2",
    # "temp_med_lag3",
    # "temp_med_lag4",
    # "umid_med_lag1",
    # "umid_med_lag2",
    # "umid_med_lag3",
    # "umid_med_lag4",
    # "temp_competence_optimality",
    # "optimal_conditions",
    # "humidity_risk",
    # "temp_competence_humidty_risk",
    "sin_year",
    "cos_year",
    "casoslag1_mov_sd",
    "avg",
    "wavg",
    "decay"
  )
  
  y_train <- cbind(
    dengue_data_train$casos
  )
  
  y_test <- cbind(
    dengue_data_test$casos,
    climate_data_test$temp_med_avg,
    dengue_data_test$temp_competence_optimality,
    dengue_data_test$humidity_risk,
    dengue_data_test$temp_competence_humidty_risk
    # dengue_data_test$optimal_conditions
  )
  
  y_test <- as.data.frame(y_test) # only used for reference, not to train or test.
  
  colnames(y_test) <- c(
    "casos",
    "temp_med_avg",
    "temp_competence_optimality",
    "humidity_risk",
    "temp_competence_humidty_risk"
    # "optimal_conditions"
  )
  
  y_test$week_number <- 1:52
  
  return(
    list(
      x_train_df = x_train_df,
      y_train = y_train,
      y_test = y_test
    )
  )
}

# Training the BART model
bart_model <- function () {
  set.seed(7)
  burn <- 13000
  nd <- 5000
  k_value <- 2
  power_value <- 2
  ntree_value <- 150L
  post <- wbart(x_train, y_train, nskip = burn, ndpost = nd, k=k_value, power = power_value, ntree = ntree_value)
  
  return (post)
}

# Create a function to generate predictions for a range of base weeks
generate_predictions_across_year <- function(start_week, end_week, weeks_ahead) {
  # Create a data frame to store all predictions
  all_predictions <- data.frame()
  
  # Loop through each base week
  for (week_base in start_week:end_week) {
    # Initialize vectors for the current set of predictions
    mean_predictions_2023 <- numeric(weeks_ahead)
    li_predictions_2023 <- numeric(weeks_ahead)
    ui_predictions_2023 <- numeric(weeks_ahead)
    
    # Get recent values for prediction
    v1 <- y_test$casos[week_base]
    v2 <- y_test$casos[week_base - 1]
    v3 <- y_test$casos[week_base - 2]
    v4 <- y_test$casos[week_base - 3]
    
    # Get actual values for comparison
    actual_values <- y_test$casos[(week_base + 1):(week_base + weeks_ahead)]
    
    # Generate predictions for each week ahead
    for (week in 1:weeks_ahead) {
      # Set up lag values based on which week we're predicting
      if (week == 1) {
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag1 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag2 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag3 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag4 <- y_test$temp_med_avg[week_base]
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        current_lag1 <- v1
        current_lag2 <- v2
        current_lag3 <- v3
        current_lag4 <- v4
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else {
          current_decay <- 0
        }
        week_number <- y_test$week_number[week_base + week]
      } else if (week == 2) {
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag1 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag2 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag3 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag4 <- y_test$temp_med_avg[week_base]
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        current_lag1 <- mean_predictions_2023[1]
        current_lag2 <- v1
        current_lag3 <- v2
        current_lag4 <- v3
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else {
          current_decay <- 0
        }
        week_number <- y_test$week_number[week_base + week]
      } else if (week == 3) {
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag1 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag2 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag3 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag4 <- y_test$temp_med_avg[week_base]
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        current_lag1 <- mean_predictions_2023[2]
        current_lag2 <- mean_predictions_2023[1]
        current_lag3 <- v1
        current_lag4 <- v2
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else {
          current_decay <- 0
        }
        week_number <- y_test$week_number[week_base + week]
      } else if (week == 4) {
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag1 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag2 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag3 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag4 <- y_test$temp_med_avg[week_base]
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        current_lag1 <- mean_predictions_2023[3]
        current_lag2 <- mean_predictions_2023[2]
        current_lag3 <- mean_predictions_2023[1]
        current_lag4 <- v1
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else {
          current_decay <- 0
        }
        week_number <- y_test$week_number[week_base + week]
      } else if (week > 4) {
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag1 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag2 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag3 <- y_test$temp_med_avg[week_base]
        current_umid_med_lag4 <- y_test$temp_med_avg[week_base]
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        current_lag1 <- mean_predictions_2023[4]
        current_lag2 <- mean_predictions_2023[3]
        current_lag3 <- mean_predictions_2023[2]
        current_lag4 <- mean_predictions_2023[1]
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else {
          current_decay <- 0
        }
        week_number <- y_test$week_number[week_base + week]
      }
      
      # Create prediction data
      recursive_data <- data.frame(
        casos_lag1 = current_lag1,
        casos_lag2 = current_lag2,
        casos_lag3 = current_lag3,
        casos_lag4 = current_lag4,
        # temp_med_lag1 = current_temp_med_lag1,
        # temp_med_lag2 = current_temp_med_lag2,
        # temp_med_lag3 = current_temp_med_lag3,
        # temp_med_lag4 = current_temp_med_lag4,
        # umid_med_lag1 = current_umid_med_lag1,
        # umid_med_lag2 = current_umid_med_lag2,
        # umid_med_lag3 = current_umid_med_lag3,
        # umid_med_lag4 = current_umid_med_lag4,
        # temp_competence_optimality = current_temp_competence_optimality,
        # humidity_risk = current_humidity_risk,
        # temp_competence_humidty_risk = current_temp_competence_humidty_risk,
        # optimal_conditions = current_optimal_conditions,
        sin_year = sin(2 * pi * week_number/52),
        cos_year = cos(2 * pi * week_number/52),
        casoslag1_mov_sd = mov_sd,
        avg = avg,
        wavg = wavg,
        decay = current_decay
      )
      
      # Make prediction
      quantile_levels <- c(0.01, 0.025, 0.05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 
                           0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.975, 0.99)
      prediction <- predict(post, newdata = recursive_data)
      
      quantile_predictions <- lapply(quantile_levels, function(q) {
        apply(prediction, 2, quantile, probs = q)
      })
      quantiles = setNames(quantile_predictions, paste0("q", quantile_levels * 100))
      
      mean_prediction <- apply(prediction, 2, mean)
      median_prediction <- apply(prediction, 2, median)
      li_prediction <- apply(prediction, 2, quantile, probs = 0.025)
      ui_prediction <- apply(prediction, 2, quantile, probs = 0.975)

      
      # Store predictions
      mean_predictions_2023[week] <- mean_prediction
      li_predictions_2023[week] <- li_prediction
      ui_predictions_2023[week] <- ui_prediction
    }

    # Add the current predictions to the overall data frame
    for (i in 1:weeks_ahead) {
      prediction_week <- week_base + i
      
      # Skip if prediction week is out of range
      if (prediction_week > 52) {
        next
      }
      
      # Actual value
      actual <- y_test$casos[prediction_week]
      
      
      
      # Calculate interval scores for standard intervals
      # 50% interval
      is_50 <- calculate_interval_score(
        quantiles$q25[1],
        quantiles$q75[1],
        actual,
        0.5
      )
      
      # 80% interval
      is_80 <- calculate_interval_score(
        quantiles$q10[1],
        quantiles$q90[1],
        actual,
        0.2
      )
      
      # 90% interval
      is_90 <- calculate_interval_score(
        quantiles$q5[1],
        quantiles$q95[1],
        actual,
        0.1
      )
      
      # 95% interval
      is_95 <- calculate_interval_score(
        quantiles$q2.5[1],
        quantiles$q97.5[1],
        actual,
        0.05
      )
      
      # Calculate proper WIS
      wis_result <- calculate_proper_wis(quantiles, actual)

      
      new_row <- data.frame(
        base_week = week_base,
        prediction_week = prediction_week,
        actual = y_test$casos[prediction_week],
        predicted = mean_predictions_2023[i],
        predicted_median = median_prediction,
        lower_ci = li_predictions_2023[i],
        upper_ci = ui_predictions_2023[i],
        
        # Store all quantiles for reference
        q1 = quantiles$q1[1],
        q2.5 = quantiles$q2.5[1],
        q5 = quantiles$q5[1],
        q10 = quantiles$q10[1],
        q25 = quantiles$q25[1],
        q50 = median_prediction,
        q75 = quantiles$q75[1],
        q90 = quantiles$q90[1],
        q95 = quantiles$q95[1],
        q97.5 = quantiles$q97.5[1],
        q99 = quantiles$q99[1],
        
        horizon = i,
        ae = abs(actual - mean_predictions_2023[i]),

        # Store interval scores
        is_50_score = is_50$score,
        is_50_width = is_50$width,
        is_50_under = is_50$under_penalty,
        is_50_over = is_50$over_penalty,
        
        is_80_score = is_80$score,
        is_80_width = is_80$width,
        is_80_under = is_80$under_penalty,
        is_80_over = is_80$over_penalty,
        
        is_90_score = is_90$score,
        is_90_width = is_90$width,
        is_90_under = is_90$under_penalty,
        is_90_over = is_90$over_penalty,
        
        is_95_score = is_95$score,
        is_95_width = is_95$width,
        is_95_under = is_95$under_penalty,
        is_95_over = is_95$over_penalty,
        
        # Store WIS
        wis_score = wis_result$score,
        wis_ae = wis_result$absolute_error,
        wis_width = wis_result$width,
        wis_under = wis_result$under_penalty,
        wis_over = wis_result$over_penalty,
        
        # Coverage metrics
        coverage_50 = as.numeric(actual >= quantiles$q25[i] & 
                                   actual <= quantiles$q75[i]),
        coverage_80 = as.numeric(actual >= quantiles$q10[i] & 
                                   actual <= quantiles$q90[i]),
        coverage_90 = as.numeric(actual >= quantiles$q5[i] & 
                                   actual <= quantiles$q95[i]),
        coverage_95 = as.numeric(actual >= quantiles$q2.5[i] & 
                                   actual <= quantiles$q97.5[i])
        
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

# Create animated visualization
create_prediction_animation_year <- function(all_predictions, output_format, output_file) {
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
    
    # Highlight the base week with a vertical line
    # geom_vline(aes(xintercept = base_week), color = "blue", linetype = "dashed") +
    
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
                           width = 800, 
                           height = 600,
                           renderer = if(output_format == "gif") {
                             gifski_renderer(output_file)
                           } else if(output_format == "mp4") {
                             av_renderer(output_file)
                           }
  )
  
  return(animated_plot)
}

# Plot full year
plot_full_year <- function() {
  p <- ggplot() +
    # Plot the actual values as a black line
    geom_line(data = data.frame(week = 1:52, actual = y_test$casos),
              aes(x = week, y = actual), color = "black", size = 1) +
    geom_point(data = data.frame(week = 1:52, actual = y_test$casos),
               aes(x = week, y = actual), color = "black", size = 2) +
    
    # Add the predictions as red points with confidence intervals
    geom_ribbon(data = all_predictions, 
                aes(x = prediction_week, ymin = lower_ci, ymax = upper_ci), 
                fill = "red", alpha = 0.2) +
    geom_line(data = all_predictions, 
              aes(x = prediction_week, y = predicted, group = base_week), 
              color = "red", linetype = "dashed", size = 0.5) +
    geom_point(data = all_predictions, 
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
plot_var_imp <- function() {
  # Assuming 'post' is your trained BART model and x_train has column names
  var_imp <- post$varcount.mean
  
  # Normalize to percentages
  var_imp_perc <- (var_imp/sum(var_imp)) * 100
  
  names(var_imp) <- c(
    "casos_lag1",
    "casos_lag2",
    "casos_lag3",
    "casos_lag4",
    "temp_med_lag1",
    "temp_med_lag2",
    "temp_med_lag3",
    "temp_med_lag4",
    "umid_med_lag1",
    "umid_med_lag2",
    "umid_med_lag3",
    "umid_med_lag4",
    "temp_competence_optimality",
    "humidity_risk",
    "temp_competence_humidty_risk",
    # "optimal_conditions",
    "sin_year",
    "cos_year",
    "casoslag1_mov_sd",
    "avg",
    "wavg",
    "decay"
  )
  # Create a data frame for plotting (much easier with ggplot2)
  importance_df <- data.frame(
    Variable = names(var_imp),  # Use names from your training data
    Importance = var_imp_perc
  )
  
  
  
  # ggplot2 bar plot with labels
  ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) + # reorder for descending order
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() + # Horizontal bars for easier reading of labels
    labs(title = "Variable Importance (%)",
         x = "Variable",
         y = "Importance") +
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
calculate_proper_wis <- function(quantile_predictions, actual) {
  
  # Extract quantiles and levels
  quantile_names <- names(quantile_predictions)
  quantile_levels <- as.numeric(gsub("q", "", quantile_names)) / 100
  
  # Sort quantiles by level
  sorted_indices <- order(quantile_levels)
  quantile_levels <- quantile_levels[sorted_indices]
  quantile_values <- sapply(quantile_predictions[sorted_indices], function(x) x[1])
  
  
  # Extract median
  median_value <- quantile_predictions$q50
  
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
  
  return(list(
    score = wis,
    absolute_error = ae,
    width = weighted_widths / sum(interval_weights),
    under_penalty = weighted_unders / sum(interval_weights),
    over_penalty = weighted_overs / sum(interval_weights),
    components = data.frame(
      alpha = c(NA, interval_alphas),
      is_value = c(NA, is_values),
      width = c(NA, widths),
      under = c(NA, under_penalties),
      over = c(NA, over_penalties),
      weight = all_weights,
      type = c("Median AE", paste0((1-interval_alphas)*100, "% CI"))
    )
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

# Alternative approach using base R plots if ggplot is causing issues
plot_wis_decomposition_base <- function(all_predictions, legend_offset = -0.5) {
  # Aggregate the components by horizon
  horizons <- sort(unique(all_predictions$horizon))
  
  # Create empty matrices to store results
  width_vals <- numeric(length(horizons))
  under_vals <- numeric(length(horizons))
  over_vals <- numeric(length(horizons))
  ae_vals <- numeric(length(horizons))
  total_vals <- numeric(length(horizons))
  
  # Fill matrices with aggregated values
  for (i in 1:length(horizons)) {
    h <- horizons[i]
    horizon_data <- all_predictions[all_predictions$horizon == h, ]
    
    width_vals[i] <- mean(horizon_data$wis_width, na.rm = TRUE)
    under_vals[i] <- mean(horizon_data$wis_under, na.rm = TRUE)
    over_vals[i] <- mean(horizon_data$wis_over, na.rm = TRUE)
    ae_vals[i] <- mean(horizon_data$wis_ae, na.rm = TRUE)
    total_vals[i] <- mean(horizon_data$wis_score, na.rm = TRUE)
  }
  
  # Create stacked bar plot
  barplot_data <- rbind(ae_vals, width_vals, under_vals, over_vals)
  
  # Get colors from RColorBrewer
  library(RColorBrewer)
  colors <- brewer.pal(4, "Set2")
  
  # Create the plot
  par(mar = c(5, 6, 6, 8) + 0.1)  # Adjust margins for legend
  
  bp <- barplot(barplot_data, col = colors, 
                names.arg = horizons,
                xlab = "Forecast Horizon (weeks)",
                ylab = "Score Component",
                main = "Weighted Interval Score Decomposition by Horizon",
                ylim = c(0, max(total_vals) * 1.2),  # Add space for legend
                border = "white",
                beside = FALSE)
  
  # Add total WIS line
  lines(bp, total_vals, lwd = 2, col = "black")
  points(bp, total_vals, pch = 19, col = "black", cex = 1.5)
  
  # Add text labels for totals
  text(bp, total_vals + max(total_vals) * 0.05, 
       round(total_vals, 1), cex = 0.8)
  
  # Add legend
  legend(x = par("usr")[2] + (par("usr")[2] - par("usr")[1]) * legend_offset/8, 
         y = par("usr")[4] * 0.9,  # Position at 90% of the top
         legend = c("Absolute Error", "Interval Width", "Underprediction Penalty", "Overprediction Penalty", "Total WIS"),
         fill = c(colors, NA),
         border = c(rep("white", 4), NA),
         lty = c(NA, NA, NA, NA, 1),
         lwd = c(NA, NA, NA, NA, 2),
         pch = c(NA, NA, NA, NA, 19),
         col = c(colors, "black"),
         bg = "white",
         cex = 0.8,
         xpd = TRUE)
  # Return the plot invisibly
  invisible(bp)
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
                        labels = c("50% PI", "80% PI", "90% PI", "95% PI"))
    )
  
  # Create coverage plot
  p <- ggplot(coverage_data, aes(x = horizon, y = coverage, color = interval, group = interval)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_line(aes(y = nominal_level, group = interval), linetype = "dashed") +
    facet_wrap(~ interval) +
    labs(
      title = "Prediction Interval Coverage by Horizon",
      subtitle = "Dashed lines indicate ideal coverage levels",
      x = "Forecast Horizon (weeks)",
      y = "Empirical Coverage",
      color = "Interval"
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
      title = paste("PIT Histogram -", model_name),
      x = "Probability Integral Transform",
      y = "Frequency"
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
      title = paste("Calibration Plot -", model_name),
      x = "Predicted Values",
      y = "Actual Values"
    ) +
    theme_minimal()
  
  return(p)
}

# Metrics by point predicted
horizon_metrics <- function() {
  horizon_metrics <- all_predictions %>%
    group_by(horizon) %>%
    summarize(
      mean_ae = mean(ae),
      mean_is = mean(is_score),
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
      # mean_is = mean(is_score, na.rm = TRUE),
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
      # mean_is = mean(is_score, na.rm = TRUE),
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
  # write.csv(overall_metrics, overall_file, row.names = FALSE)
  # write.csv(all_metrics, combined_file, row.names = FALSE)
  
  # Update the all models comparison file
  # write.csv(all_metrics, all_models_file, row.names = FALSE)
  
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

# Function to visualize detailed metrics for a specific model
visualize_model_metrics <- function(model_name, metrics_dir = NULL) {
  # Default to current working directory if not specified
  if (is.null(metrics_dir)) {
    metrics_dir <- file.path(getwd(), "model_metrics")
  }
  
  # Load the model's metrics
  model_file <- file.path(metrics_dir, paste0(model_name, "_all_metrics.csv"))
  
  if (!file.exists(model_file)) {
    stop(paste("Metrics for model", model_name, "not found."))
  }
  
  model_metrics <- read.csv(model_file)
  
  # Filter out the overall row for horizon plots
  horizon_metrics <- model_metrics %>%
    filter(horizon != "overall")
  
  # Create plots
  
  # 1. Accuracy metrics by horizon
  p1 <- ggplot(horizon_metrics, aes(x = horizon)) +
    geom_line(aes(y = mean_ae, color = "Mean Absolute Error")) +
    geom_line(aes(y = rmse, color = "RMSE")) +
    labs(
      title = paste("Accuracy Metrics by Horizon -", model_name),
      x = "Forecast Horizon (weeks)",
      y = "Error",
      color = "Metric"
    ) +
    theme_minimal()
  
  # 2. IS/WIS components by horizon
  # Reshape data for stacked bar chart
  is_components <- horizon_metrics %>%
    select(horizon, mean_is, model_name) %>%
    mutate(metric_type = "Interval Score")
  
  wis_components <- horizon_metrics %>%
    select(horizon, mean_wis, model_name) %>%
    mutate(metric_type = "Weighted Interval Score")
  
  p2 <- ggplot() +
    geom_line(data = is_components, aes(x = horizon, y = mean_is, color = "Interval Score")) +
    geom_line(data = wis_components, aes(x = horizon, y = mean_wis, color = "Weighted Interval Score")) +
    labs(
      title = paste("Interval Scores by Horizon -", model_name),
      x = "Forecast Horizon (weeks)",
      y = "Score",
      color = "Metric"
    ) +
    theme_minimal()
  
  # 3. Coverage by horizon
  p3 <- ggplot(horizon_metrics, aes(x = horizon, y = coverage_95)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
    labs(
      title = paste("95% Interval Coverage by Horizon -", model_name),
      x = "Forecast Horizon (weeks)",
      y = "Coverage",
      caption = "Red dashed line indicates ideal 95% coverage"
    ) +
    theme_minimal()
  
  # Return all plots
  return(list(
    accuracy_plot = p1,
    interval_scores_plot = p2,
    coverage_plot = p3,
    metrics = model_metrics
  ))
}


# CREATING IMPORTANT VARIABLES #
climate_data <- api_calls(api_name = "climate")
dengue_data_ordered <- api_calls(api_name = "dengue")

x_train <- feature_creation() %>%
  .$x_train_df
y_train <- feature_creation() %>%
  .$y_train
y_test <- feature_creation() %>%
  .$y_test

post <- bart_model()

# Generate predictions for all base weeks
all_predictions <- generate_predictions_across_year(start_week = 4, end_week = 4, weeks_ahead = 4) 

# Create and save the animation
# output_format = "mp4"
# create_prediction_animation_year(all_predictions, output_format, glue("dengue_predictions_2023_comparison_test.{output_format}"))


# Plots to analysis:
plot_full_year()
plot_var_imp()
plot_wis_decomposition_base(all_predictions)
plot_multi_coverage(all_predictions)
# plot_pit_histogram(all_predictions)
# plot_calibration(all_predictions)

# Store the metrics with a descriptive name
model_metrics <- store_model_metrics(
  all_predictions, 
  model_name = glue("BART"), 
  save_path = getwd()
)
print(model_metrics$horizon_metrics)





