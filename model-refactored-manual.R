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
        
# Get API key
API_KEY <- Sys.getenv("MOSQLIMATE_API_KEY")

# FUNCTIONS #

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
        start = 201101,  # Extended start date from 2011
        end = 202352,
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
    date <- paste0("&start=2010-12-30&end=2023-12-30")
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
  }
  
  # Humidity-based risk scale (1-3) 
  create_humidity_risk <- function(humidity_data) {
  }
  
  # Create binary feature for optimal combined conditions
  create_optimal_conditions <- function(temp_competence, humidity_risk) {
  }
  
  # Vectorized approach with precipitation included
  fill_lag_columns <- function(data, temp_weekly_avg) {
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
  
  #Preciptation Total Lag
  dengue_data_ordered$precip_tot_lag1 <- dplyr::lag(climate_data$precip_tot_sum, 1)
  dengue_data_ordered$precip_tot_lag2 <- dplyr::lag(climate_data$precip_tot_sum, 2)
  dengue_data_ordered$precip_tot_lag3 <- dplyr::lag(climate_data$precip_tot_sum, 3)
  dengue_data_ordered$precip_tot_lag4 <- dplyr::lag(climate_data$precip_tot_sum, 4)
  
  # Calculate average lag values by week of year
  temp_weekly_avg <- dengue_data_ordered %>%
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
      avg_precip_tot_lag4 = mean(precip_tot_lag4, na.rm = TRUE)
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
    dengue_data_train$temp_med_lag1,
    dengue_data_train$temp_med_lag2,
    dengue_data_train$temp_med_lag3,
    dengue_data_train$temp_med_lag4,
    dengue_data_train$umid_med_lag1,
    dengue_data_train$umid_med_lag2,
    dengue_data_train$umid_med_lag3,
    dengue_data_train$umid_med_lag4,
    dengue_data_train$precip_tot_lag1,
    dengue_data_train$precip_tot_lag2,
    dengue_data_train$precip_tot_lag3,
    dengue_data_train$precip_tot_lag4,
    dengue_data_train$temp_competence_optimality,
    dengue_data_train$humidity_risk,
    dengue_data_train$temp_competence_humidty_risk,
    # dengue_data_train$optimal_conditions,
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
    "temp_med_lag1",
    "temp_med_lag2",
    "temp_med_lag3",
    "temp_med_lag4",
    "umid_med_lag1",
    "umid_med_lag2",
    "umid_med_lag3",
    "umid_med_lag4",
    "precip_tot_lag1",
    "precip_tot_lag2",
    "precip_tot_lag3",
    "precip_tot_lag4",
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
  
  lambda <- BoxCox.lambda(dengue_data_train$casos)
  y_train <- cbind(BoxCox(dengue_data_train$casos, lambda))
  
  y_test <- cbind(
    dengue_data_test$casos,
    climate_data_test$temp_med_avg,
    climate_data_test$umid_med_avg,
    climate_data_test$precip_tot_sum,
    dengue_data_test$temp_competence_optimality,
    dengue_data_test$humidity_risk,
    dengue_data_test$temp_competence_humidty_risk
    # dengue_data_test$optimal_conditions
  )
  
  y_test <- as.data.frame(y_test) # only used for reference, not to train or test.
  
  colnames(y_test) <- c(
    "casos",
    "temp_med_avg",
    "umid_med_avg",
    "precip_tot_sum",
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
      y_test = y_test,
      lambda = lambda
    )
  )
}

# Training the BART model
bart_model <- function () {
  set.seed(7)
  burn <- 13000
  nd <- 5000
  k_value <- 2.5
  power_value <- 0.5
  ntree_value <- 200L
  post <- wbart(x_train, y_train, nskip = burn, ndpost = nd, k=k_value, power = power_value, ntree = ntree_value, sparse = TRUE)
  
  return (post)
}

# Create a function to generate predictions for a range of base weeks
generate_predictions_across_year <- function(start_week, end_week, weeks_ahead, post) {
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
    
    # Get recent values for prediction
    v1 <- y_test$casos[week_base]
    v2 <- y_test$casos[week_base - 1]
    v3 <- y_test$casos[week_base - 2]
    v4 <- y_test$casos[week_base - 3]
    
    avg_precip_lag <- ((y_test$precip_tot_sum[week_base]) + (y_test$precip_tot_sum[week_base - 1]) + (y_test$precip_tot_sum[week_base - 2]) + (y_test$precip_tot_sum[week_base -3])) / 4
    
    # Get actual values for comparison
    actual_values <- y_test$casos[(week_base + 1):(week_base + weeks_ahead)]

    # Temperature volatility (using 4-week window)
    current_temp_volatility <- sd(c(
      y_test$temp_med_avg[week_base],
      y_test$temp_med_avg[week_base - 1],
      y_test$temp_med_avg[week_base - 2],
      y_test$temp_med_avg[week_base - 3]
    ))
    
    # Get favorable_weeks_count from test data
    current_favorable_weeks_count <- y_test$favorable_weeks_count[week_base]
    
    # Get dry_then_wet from test data
    current_dry_then_wet <- y_test$dry_then_wet[week_base]
    
    # Get climate_state_change from test data
    current_climate_state_change <- y_test$climate_state_change[week_base]
    
    # Generate predictions for each week ahead
    for (week in 1:weeks_ahead) {
      # Set up lag values based on which week we're predicting
      if (week == 1) {
        current_lag1 <- v1
        current_lag2 <- v2
        current_lag3 <- v3
        current_lag4 <- v4
        
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else { current_decay <- 0 }
        
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        
        current_umid_med_lag1 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag2 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag3 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag4 <- y_test$umid_med_avg[week_base]
        
        current_precip_tot_lag1 <- avg_precip_lag
        current_precip_tot_lag2 <- avg_precip_lag
        current_precip_tot_lag3 <- avg_precip_lag
        current_precip_tot_lag4 <- avg_precip_lag
        
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
        
        if (current_lag2 > 0) {
          current_lag1_growth_rate <- (current_lag1 - current_lag2) / current_lag2
        } else { current_lag1_growth_rate <- 0 }
        
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag1_vs_lag4_growth <- 1
        } else { current_lag1_vs_lag4_growth <- 0 }
        
        current_log_lag_growth <- log((current_lag1 + 1)/(current_lag2 + 1))
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag_sustained_growth <- 1
        } else { current_lag_sustained_growth <- 0 }
        
        current_lag_growth_accel <- (current_lag1 - current_lag2) - (current_lag2 - current_lag3)
        current_avg_growth_rate <- (avg - (mean(c(current_lag2, current_lag3, current_lag4)))) / (mean(c(current_lag2, current_lag3, current_lag4)))
        current_temp_volatility <- current_temp_volatility
        current_favorable_weeks_count <- current_favorable_weeks_count
        current_dry_then_wet <- current_dry_then_wet
        current_climate_state_change <- current_climate_state_change
        
        current_climate_change_context <- y_test$climate_change_context[week_base]
        current_climate_improving <- y_test$climate_improving[week_base]
        current_climate_worsening <- y_test$climate_worsening[week_base]
        
        current_is_summer <- y_test$is_summer[week_base + week]
        current_is_autumn <- y_test$is_autumn[week_base + week]
        current_is_winter <- y_test$is_winter[week_base + week]
        current_is_spring <- y_test$is_spring[week_base + week]
        
        current_temp_humid_risk_summer <- current_temp_competence_humidty_risk * current_is_summer
        current_temp_humid_risk_autumn <- current_temp_competence_humidty_risk * current_is_autumn
        current_temp_humid_risk_winter <- current_temp_competence_humidty_risk * current_is_winter
        current_temp_humid_risk_spring <- current_temp_competence_humidty_risk * current_is_spring
        
        current_casos_lag1_summer <- current_lag1 * current_is_summer
        current_casos_lag1_autumn <- current_lag1 * current_is_autumn
        current_casos_lag1_winter <- current_lag1 * current_is_winter
        current_casos_lag1_spring <- current_lag1 * current_is_spring
        
        current_near_season_change <- y_test$near_season_change[week_base + week]
        current_spring_to_summer_transition <- y_test$spring_to_summer_transition[week_base + week]
        
        current_temp_humid_risk_stability <- y_test$temp_humid_risk_stability[week_base]
        current_temp_humid_risk_trend <- y_test$temp_humid_risk_trend[week_base]
        
        current_risk_vs_8wk_avg <- y_test$risk_vs_8wk_avg[week_base]
        current_risk_seasonal_zscore <- y_test$risk_seasonal_zscore[week_base]
        
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        week_number <- y_test$week_number[week_base + week]
        
      } else if (week == 2) {
        current_lag1 <- mean_predictions_2023[1]
        current_lag2 <- v1
        current_lag3 <- v2
        current_lag4 <- v3
        
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else { current_decay <- 0 }
        
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        
        current_umid_med_lag1 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag2 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag3 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag4 <- y_test$umid_med_avg[week_base]
        
        current_precip_tot_lag1 <- avg_precip_lag
        current_precip_tot_lag2 <- avg_precip_lag
        current_precip_tot_lag3 <- avg_precip_lag
        current_precip_tot_lag4 <- avg_precip_lag
        
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        
        current_climate_change_context <- y_test$climate_change_context[week_base]
        current_climate_improving <- y_test$climate_improving[week_base]
        current_climate_worsening <- y_test$climate_worsening[week_base]
        
        current_is_summer <- y_test$is_summer[week_base + week]
        current_is_autumn <- y_test$is_autumn[week_base + week]
        current_is_winter <- y_test$is_winter[week_base + week]
        current_is_spring <- y_test$is_spring[week_base + week]
        
        current_temp_humid_risk_summer <- current_temp_competence_humidty_risk * current_is_summer
        current_temp_humid_risk_autumn <- current_temp_competence_humidty_risk * current_is_autumn
        current_temp_humid_risk_winter <- current_temp_competence_humidty_risk * current_is_winter
        current_temp_humid_risk_spring <- current_temp_competence_humidty_risk * current_is_spring
        
        current_casos_lag1_summer <- current_lag1 * current_is_summer
        current_casos_lag1_autumn <- current_lag1 * current_is_autumn
        current_casos_lag1_winter <- current_lag1 * current_is_winter
        current_casos_lag1_spring <- current_lag1 * current_is_spring
        
        current_near_season_change <- y_test$near_season_change[week_base + week]
        current_spring_to_summer_transition <- y_test$spring_to_summer_transition[week_base + week]
        
        current_temp_humid_risk_stability <- y_test$temp_humid_risk_stability[week_base]
        current_temp_humid_risk_trend <- y_test$temp_humid_risk_trend[week_base]
        
        current_risk_vs_8wk_avg <- y_test$risk_vs_8wk_avg[week_base]
        current_risk_seasonal_zscore <- y_test$risk_seasonal_zscore[week_base]
        
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
        
        if (current_lag2 > 0) {
          current_lag1_growth_rate <- (current_lag1 - current_lag2) / current_lag2
        } else { current_lag1_growth_rate <- 0 }
        
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag1_vs_lag4_growth <- 1
        } else { current_lag1_vs_lag4_growth <- 0 }
        
        current_log_lag_growth <- log((current_lag1 + 1)/(current_lag2 + 1))
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag_sustained_growth <- 1
        } else { current_lag_sustained_growth <- 0 }
        
        current_lag_growth_accel <- (current_lag1 - current_lag2) - (current_lag2 - current_lag3)
        current_avg_growth_rate <- (avg - (mean(c(current_lag2, current_lag3, current_lag4)))) / (mean(c(current_lag2, current_lag3, current_lag4)))
        current_temp_volatility <- current_temp_volatility
        current_favorable_weeks_count <- ifelse(current_favorable_weeks_count > 0, 
                                                current_favorable_weeks_count + 1, 0)
        current_dry_then_wet <- 0  # Reset since it's a specific pattern
        current_climate_state_change <- 0  # Assume no change for future weeks
        
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        week_number <- y_test$week_number[week_base + week]
      } else if (week == 3) {
        current_lag1 <- mean_predictions_2023[2]
        current_lag2 <- mean_predictions_2023[1]
        current_lag3 <- v1
        current_lag4 <- v2
        
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else { current_decay <- 0 }
        
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        
        current_umid_med_lag1 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag2 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag3 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag4 <- y_test$umid_med_avg[week_base]
        
        current_precip_tot_lag1 <- avg_precip_lag
        current_precip_tot_lag2 <- avg_precip_lag
        current_precip_tot_lag3 <- avg_precip_lag
        current_precip_tot_lag4 <- avg_precip_lag
        
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        
        current_climate_change_context <- y_test$climate_change_context[week_base]
        current_climate_improving <- y_test$climate_improving[week_base]
        current_climate_worsening <- y_test$climate_worsening[week_base]
        
        current_is_summer <- y_test$is_summer[week_base + week]
        current_is_autumn <- y_test$is_autumn[week_base + week]
        current_is_winter <- y_test$is_winter[week_base + week]
        current_is_spring <- y_test$is_spring[week_base + week]
        
        current_temp_humid_risk_summer <- current_temp_competence_humidty_risk * current_is_summer
        current_temp_humid_risk_autumn <- current_temp_competence_humidty_risk * current_is_autumn
        current_temp_humid_risk_winter <- current_temp_competence_humidty_risk * current_is_winter
        current_temp_humid_risk_spring <- current_temp_competence_humidty_risk * current_is_spring
        
        current_casos_lag1_summer <- current_lag1 * current_is_summer
        current_casos_lag1_autumn <- current_lag1 * current_is_autumn
        current_casos_lag1_winter <- current_lag1 * current_is_winter
        current_casos_lag1_spring <- current_lag1 * current_is_spring

        current_near_season_change <- y_test$near_season_change[week_base + week]
        current_spring_to_summer_transition <- y_test$spring_to_summer_transition[week_base + week]
        
        current_temp_humid_risk_stability <- y_test$temp_humid_risk_stability[week_base]
        current_temp_humid_risk_trend <- y_test$temp_humid_risk_trend[week_base]
        
        current_risk_vs_8wk_avg <- y_test$risk_vs_8wk_avg[week_base]
        current_risk_seasonal_zscore <- y_test$risk_seasonal_zscore[week_base]
        
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
        
        if (current_lag2 > 0) {
          current_lag1_growth_rate <- (current_lag1 - current_lag2) / current_lag2
        } else { current_lag1_growth_rate <- 0 }
        
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag1_vs_lag4_growth <- 1
        } else { current_lag1_vs_lag4_growth <- 0 }
        
        current_log_lag_growth <- log((current_lag1 + 1)/(current_lag2 + 1))
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag_sustained_growth <- 1
        } else { current_lag_sustained_growth <- 0 }
        
        current_lag_growth_accel <- (current_lag1 - current_lag2) - (current_lag2 - current_lag3)
        current_avg_growth_rate <- (avg - (mean(c(current_lag2, current_lag3, current_lag4)))) / (mean(c(current_lag2, current_lag3, current_lag4)))
        current_temp_volatility <- current_temp_volatility
        current_favorable_weeks_count <- ifelse(current_favorable_weeks_count > 0, 
                                                current_favorable_weeks_count + 2, 0)
        current_dry_then_wet <- 0
        current_climate_state_change <- 0
        
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        week_number <- y_test$week_number[week_base + week]
      } else if (week == 4) {
        current_lag1 <- mean_predictions_2023[3]
        current_lag2 <- mean_predictions_2023[2]
        current_lag3 <- mean_predictions_2023[1]
        current_lag4 <- v1
        
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else { current_decay <- 0 }
        
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        
        current_umid_med_lag1 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag2 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag3 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag4 <- y_test$umid_med_avg[week_base]
        
        current_precip_tot_lag1 <- avg_precip_lag
        current_precip_tot_lag2 <- avg_precip_lag
        current_precip_tot_lag3 <- avg_precip_lag
        current_precip_tot_lag4 <- avg_precip_lag
        
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        
        current_climate_change_context <- y_test$climate_change_context[week_base]
        current_climate_improving <- y_test$climate_improving[week_base]
        current_climate_worsening <- y_test$climate_worsening[week_base]
        
        current_is_summer <- y_test$is_summer[week_base + week]
        current_is_autumn <- y_test$is_autumn[week_base + week]
        current_is_winter <- y_test$is_winter[week_base + week]
        current_is_spring <- y_test$is_spring[week_base + week]
        
        current_temp_humid_risk_summer <- current_temp_competence_humidty_risk * current_is_summer
        current_temp_humid_risk_autumn <- current_temp_competence_humidty_risk * current_is_autumn
        current_temp_humid_risk_winter <- current_temp_competence_humidty_risk * current_is_winter
        current_temp_humid_risk_spring <- current_temp_competence_humidty_risk * current_is_spring
        
        current_casos_lag1_summer <- current_lag1 * current_is_summer
        current_casos_lag1_autumn <- current_lag1 * current_is_autumn
        current_casos_lag1_winter <- current_lag1 * current_is_winter
        current_casos_lag1_spring <- current_lag1 * current_is_spring

        current_near_season_change <- y_test$near_season_change[week_base + week]
        current_spring_to_summer_transition <- y_test$spring_to_summer_transition[week_base + week]
        
        current_temp_humid_risk_stability <- y_test$temp_humid_risk_stability[week_base]
        current_temp_humid_risk_trend <- y_test$temp_humid_risk_trend[week_base]
        
        current_risk_vs_8wk_avg <- y_test$risk_vs_8wk_avg[week_base]
        current_risk_seasonal_zscore <- y_test$risk_seasonal_zscore[week_base]
        
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
        
        if (current_lag2 > 0) {
          current_lag1_growth_rate <- (current_lag1 - current_lag2) / current_lag2
        } else { current_lag1_growth_rate <- 0 }
        
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag1_vs_lag4_growth <- 1
        } else { current_lag1_vs_lag4_growth <- 0 }
        
        current_log_lag_growth <- log((current_lag1 + 1)/(current_lag2 + 1))
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag_sustained_growth <- 1
        } else { current_lag_sustained_growth <- 0 }
        
        current_lag_growth_accel <- (current_lag1 - current_lag2) - (current_lag2 - current_lag3)
        current_avg_growth_rate <- (avg - (mean(c(current_lag2, current_lag3, current_lag4)))) / (mean(c(current_lag2, current_lag3, current_lag4)))
        current_temp_volatility <- current_temp_volatility
        current_favorable_weeks_count <- ifelse(current_favorable_weeks_count > 0, 
                                                current_favorable_weeks_count + 3, 0)
        current_dry_then_wet <- 0
        current_climate_state_change <- 0
        
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        week_number <- y_test$week_number[week_base + week]
      } else if (week > 4) {
        current_lag1 <- mean_predictions_2023[4]
        current_lag2 <- mean_predictions_2023[3]
        current_lag3 <- mean_predictions_2023[2]
        current_lag4 <- mean_predictions_2023[1]
        
        if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
          declines <- c((current_lag1 - current_lag2) / current_lag2,
                        (current_lag2 - current_lag3) / current_lag3)
          current_decay <- mean(declines)
        } else { current_decay <- 0 }
        
        current_temp_med_lag1 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag2 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag3 <- y_test$temp_med_avg[week_base]
        current_temp_med_lag4 <- y_test$temp_med_avg[week_base]
        
        current_umid_med_lag1 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag2 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag3 <- y_test$umid_med_avg[week_base]
        current_umid_med_lag4 <- y_test$umid_med_avg[week_base]
        
        current_precip_tot_lag1 <- avg_precip_lag
        current_precip_tot_lag2 <- avg_precip_lag
        current_precip_tot_lag3 <- avg_precip_lag
        current_precip_tot_lag4 <- avg_precip_lag
        
        current_temp_competence_optimality <- y_test$temp_competence_optimality[week_base]
        current_humidity_risk <- y_test$humidity_risk[week_base]
        current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[week_base]
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        
        current_climate_change_context <- y_test$climate_change_context[week_base]
        current_climate_improving <- y_test$climate_improving[week_base]
        current_climate_worsening <- y_test$climate_worsening[week_base]
        
        current_is_summer <- y_test$is_summer[week_base + week]
        current_is_autumn <- y_test$is_autumn[week_base + week]
        current_is_winter <- y_test$is_winter[week_base + week]
        current_is_spring <- y_test$is_spring[week_base + week]
        
        current_temp_humid_risk_summer <- current_temp_competence_humidty_risk * current_is_summer
        current_temp_humid_risk_autumn <- current_temp_competence_humidty_risk * current_is_autumn
        current_temp_humid_risk_winter <- current_temp_competence_humidty_risk * current_is_winter
        current_temp_humid_risk_spring <- current_temp_competence_humidty_risk * current_is_spring
        
        current_casos_lag1_summer <- current_lag1 * current_is_summer
        current_casos_lag1_autumn <- current_lag1 * current_is_autumn
        current_casos_lag1_winter <- current_lag1 * current_is_winter
        current_casos_lag1_spring <- current_lag1 * current_is_spring
        
        current_near_season_change <- y_test$near_season_change[week_base + week]
        current_spring_to_summer_transition <- y_test$spring_to_summer_transition[week_base + week]
        
        current_temp_humid_risk_stability <- y_test$temp_humid_risk_stability[week_base]
        current_temp_humid_risk_trend <- y_test$temp_humid_risk_trend[week_base]
        
        current_risk_vs_8wk_avg <- y_test$risk_vs_8wk_avg[week_base]
        current_risk_seasonal_zscore <- y_test$risk_seasonal_zscore[week_base]
        
        mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
        avg <- mean(c(current_lag1, current_lag2, current_lag3))
        wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6

        if (current_lag2 > 0) {
          current_lag1_growth_rate <- (current_lag1 - current_lag2) / current_lag2
        } else { current_lag1_growth_rate <- 0 }
        
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag1_vs_lag4_growth <- 1
        } else { current_lag1_vs_lag4_growth <- 0 }
        
        current_log_lag_growth <- log((current_lag1 + 1)/(current_lag2 + 1))
        if (current_lag1 > current_lag2 && current_lag2 > current_lag3 && current_lag3 > current_lag4) {
          current_lag_sustained_growth <- 1
        } else { current_lag_sustained_growth <- 0 }
        
        current_lag_growth_accel <- (current_lag1 - current_lag2) - (current_lag2 - current_lag3)
        current_avg_growth_rate <- (avg - (mean(c(current_lag2, current_lag3, current_lag4)))) / (mean(c(current_lag2, current_lag3, current_lag4)))
        current_temp_volatility <- current_temp_volatility
        current_favorable_weeks_count <- ifelse(current_favorable_weeks_count > 0, 
                                                current_favorable_weeks_count + week - 1, 0)
        current_dry_then_wet <- 0
        current_climate_state_change <- 0
        
        current_optimal_conditions <- y_test$optimal_conditions[week_base]
        week_number <- y_test$week_number[week_base + week]
      }
      
      # Create prediction data
      recursive_data <- data.frame(
        casos_lag1 = current_lag1,
        casos_lag2 = current_lag2,
        casos_lag3 = current_lag3,
        casos_lag4 = current_lag4,
        temp_med_lag1 = current_temp_med_lag1,
        temp_med_lag2 = current_temp_med_lag2,
        temp_med_lag3 = current_temp_med_lag3,
        temp_med_lag4 = current_temp_med_lag4,
        umid_med_lag1 = current_umid_med_lag1,
        umid_med_lag2 = current_umid_med_lag2,
        umid_med_lag3 = current_umid_med_lag3,
        umid_med_lag4 = current_umid_med_lag4,
        precip_tot_lag1 = current_precip_tot_lag1,
        precip_tot_lag2 = current_precip_tot_lag2,
        precip_tot_lag3 = current_precip_tot_lag3,
        precip_tot_lag4 = current_precip_tot_lag4,
        temp_competence_optimality = current_temp_competence_optimality,
        humidity_risk = current_humidity_risk,
        temp_competence_humidty_risk = current_temp_competence_humidty_risk,
        # optimal_conditions = current_optimal_conditions,
        # lag1_growth_rate = current_lag1_growth_rate,
        # lag1_vs_lag4_growth = current_lag1_vs_lag4_growth,
        # log_lag_growth = current_log_lag_growth,
        # lag_sustained_growth = current_lag_sustained_growth,
        # lag_growth_accel = current_lag_growth_accel,
        # avg_growth_rate = current_avg_growth_rate,
        # temp_volatility = current_temp_volatility,
        # favorable_weeks_count = current_favorable_weeks_count,
        # dry_then_wet = current_dry_then_wet,
        # is_summer <- current_is_summer,
        # is_autumn <- current_is_autumn,
        # is_winter <- current_is_winter,
        # is_spring <- current_is_spring,
        # climate_change_context = current_climate_change_context,
        # climate_improving = current_climate_improving,
        # climate_worsening = current_climate_worsening,
        # temp_humid_risk_summer = current_temp_humid_risk_summer,
        # temp_humid_risk_autumn = current_temp_humid_risk_autumn,
        # temp_humid_risk_winter = current_temp_humid_risk_winter,
        # temp_humid_risk_spring = current_temp_humid_risk_spring,
        # casos_lag1_summer = current_casos_lag1_summer,
        # casos_lag1_autumn = current_casos_lag1_autumn,
        # casos_lag1_winter = current_casos_lag1_winter, 
        # casos_lag1_spring = current_casos_lag1_spring,
        # near_season_change = current_near_season_change,
        # spring_to_summer_transition = current_spring_to_summer_transition,
        # temp_humid_risk_stability = current_temp_humid_risk_stability,
        # temp_humid_risk_trend = current_temp_humid_risk_trend,
        # risk_vs_8wk_avg = current_risk_vs_8wk_avg,
        # risk_seasonal_zscore = current_risk_seasonal_zscore,
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
    025
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
plot_full_year <- function(predictions) {
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
plot_var_imp <- function(model) {
  # Assuming 'post' is your trained BART model and x_train has column names
  var_imp <- model$varcount.mean
  
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
    "precip_tot_lag1",
    "precip_tot_lag2",
    "precip_tot_lag3",
    "precip_tot_lag4",
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

# Simple metrics by point predicted
horizon_metrics <- function() {
  horizon_metrics <- all_predictions %>%
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

# Grid search function for BART model hyperparameter optimization
grid_search_bart <- function(
    k_range = seq(0.5, 3, by = 0.5),
    power_range = seq(0.5, 3, by = 0.5),
    ntree_range = seq(50, 300, by = 50),
    start_week = 4,
    end_week = 48,
    weeks_ahead = 4
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
          sparse = TRUE
        )
        
        # Generate predictions
        cat("Generating predictions...\n")
        all_predictions <- generate_predictions_across_year(
          start_week = start_week, 
          end_week = end_week, 
          weeks_ahead = weeks_ahead,
          post = current_post
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
        write.csv(results, "grid_search_results.csv", row.names = FALSE)
        
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
          saveRDS(current_post, "best_bart_model.rds")
          saveRDS(all_predictions, "best_model_predictions.rds")
        }
      }
    }
  }
  
  # Sort results by mean_wis (ascending)
  results <- results[order(results$mean_wis), ]
  
  # Save final results
  write.csv(results, "final_grid_search_results.csv", row.names = FALSE)
  
  # Print the best hyperparameters
  cat("\n===== GRID SEARCH COMPLETED =====\n")
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


# CREATING IMPORTANT VARIABLES #
climate_data <- api_calls(api_name = "climate")
dengue_data_ordered <- api_calls(api_name = "dengue")

x_train <- feature_creation() %>%
  .$x_train_df
y_train <- feature_creation() %>%
  .$y_train
y_test <- feature_creation() %>%
  .$y_test
lambda <- feature_creation() %>%
  .$lambda

# train the model from the ground up
post_model <- bart_model()

# Generate predictions for all base weeks
all_predictions <- generate_predictions_across_year(start_week = 4, end_week = 48, weeks_ahead = 4, post_model)

# Create and save the animation
# output_format = "mp4"
# create_prediction_animation_year(all_predictions, output_format, glue("k_2.5_power_0.5_ntree_200_sparsefalse_evenmoreexpandedfeatures.{output_format}"))


# Plots to analysis:
print(horizon_metrics())
print(best_metrics)

plot_full_year(all_predictions)
plot_var_imp(post_model)
# plot_wis_decomposition_base(all_predictions)
# plot_multi_coverage(all_predictions)
# plot_pit_histogram(all_predictions)
# plot_calibration(all_predictions)


# Store the metrics with a descriptive name
model_metrics <- store_model_metrics(
  all_predictions,
  model_name = glue("k_2.5_power_0.5_ntree_200_sparsefalse_evenmoreexpandedfeatures"),
  save_path = getwd()
)
# print(model_metrics$horizon_metrics)



# Run the grid search
# results <- grid_search_bart(
#   k_range = seq(0.5, 3, by = 0.5),
#   power_range = seq(0.5, 3, by = 0.5),
#   ntree_range = seq(50, 300, by = 50)
# )
# 
# vis <- visualize_grid_search_results()

# best_model trained
# best_model <- readRDS("best_model/best_bart_model.rds")

# best model predictions
# best_predictions <- readRDS("best_model/best_model_predictions.rds")

# best model metrics
# best_metrics <- read.csv("model_metrics/k_2.5_power_0.5_ntree_200_horizon_metrics.csv")


# Function to analyze why the model is overpredicting at specific base weeks
analyze_problem_weeks <- function(post, base_weeks = c(31), weeks_ahead = 4) {
  # Store results
  analysis_results <- list()
  
  # Overall variable importance from the BART model
  var_imp <- post$varcount.mean
  var_imp_perc <- (var_imp/sum(var_imp)) * 100
  
  # Set column names to match training data
  names(var_imp_perc) <- c(
    "casos_lag1", "casos_lag2", "casos_lag3", "casos_lag4",
    "temp_med_lag1", "temp_med_lag2", "temp_med_lag3", "temp_med_lag4",
    "umid_med_lag1", "umid_med_lag2", "umid_med_lag3", "umid_med_lag4",
    "precip_tot_lag1", "precip_tot_lag2", "precip_tot_lag3", "precip_tot_lag4",
    "temp_competence_optimality", "humidity_risk", "temp_competence_humidty_risk",
    "sin_year", "cos_year", "casoslag1_mov_sd", "avg", "wavg", "decay",
    "temp_volatility", "favorable_weeks_count", "dry_then_wet", "climate_state_change"
  )
  
  # For each problematic base week
  for (base_week in base_weeks) {
    cat(sprintf("\n=== Analyzing base week %d ===\n", base_week))
    
    # Create a results table for this week
    week_analysis <- data.frame(
      feature = character(),
      value = numeric(),
      importance = numeric(),
      percentile = numeric(),
      stringsAsFactors = FALSE
    )
    
    # For each prediction horizon (1-4 weeks ahead)
    for (horizon in 1:weeks_ahead) {
      cat(sprintf("\n  --- Horizon %d ---\n", horizon))
      
      # Get actual value
      actual_value <- y_test$casos[base_week + horizon]
      
      # Get predicted value from all_predictions dataframe
      pred_row <- all_predictions[all_predictions$base_week == base_week & 
                                    all_predictions$horizon == horizon, ]
      
      if (nrow(pred_row) > 0) {
        predicted_value <- pred_row$predicted[1]
        prediction_error <- (predicted_value - actual_value) / actual_value * 100
        cat(sprintf("  Actual: %d, Predicted: %.1f, Error: %.1f%%\n", 
                    actual_value, predicted_value, prediction_error))
      } else {
        cat("  No prediction found for this horizon\n")
        next
      }
      
      # Generate the feature values that would have been used for this prediction
      prediction_features <- generate_feature_vector_for_analysis(base_week, horizon)
      
      # Analyze each feature's contribution
      for (feature_name in names(prediction_features)) {
        feature_value <- prediction_features[[feature_name]]
        feature_imp <- var_imp_perc[feature_name]
        
        # Get the percentile of this feature value relative to all values in training data
        # Skip if feature doesn't exist in x_train
        if (feature_name %in% colnames(x_train)) {
          all_values <- x_train[[feature_name]]
          percentile <- ecdf(all_values)(feature_value) * 100
          
          # Add to results
          week_analysis <- rbind(week_analysis, data.frame(
            feature = feature_name,
            value = feature_value,
            importance = feature_imp,
            percentile = percentile,
            horizon = horizon
          ))
        }
      }
    }
    
    # Sort by importance
    week_analysis <- week_analysis[order(-week_analysis$importance), ]
    
    # Add to overall results
    analysis_results[[as.character(base_week)]] <- week_analysis
    
    # Print most important features with extreme values
    cat("\nTop potentially problematic features (high importance + extreme values):\n")
    problematic <- week_analysis[week_analysis$importance > 2 & 
                                   (week_analysis$percentile > 90 | week_analysis$percentile < 10), ]
    
    if (nrow(problematic) > 0) {
      print(problematic[, c("feature", "value", "importance", "percentile", "horizon")])
    } else {
      cat("No extreme values found in important features\n")
    }
    
    # Print top 10 most important features regardless of extremeness
    cat("\nTop 10 most important features:\n")
    top_important <- week_analysis[!duplicated(week_analysis$feature), ][1:10, ]
    print(top_important[, c("feature", "importance")])
    
    # Print features with the most extreme values
    cat("\nFeatures with extreme values (regardless of importance):\n")
    extreme_features <- week_analysis[week_analysis$percentile > 95 | week_analysis$percentile < 5, ]
    
    if (nrow(extreme_features) > 0) {
      extreme_features <- extreme_features[!duplicated(extreme_features$feature), ]
      extreme_features <- extreme_features[order(abs(extreme_features$percentile - 50), decreasing = TRUE), ]
      print(extreme_features[1:min(10, nrow(extreme_features)), c("feature", "value", "percentile", "importance")])
    } else {
      cat("No extreme values found\n")
    }
  }
  
  # Create visualizations
  for (base_week in base_weeks) {
    # Create a plot showing feature importance vs percentile
    week_data <- analysis_results[[as.character(base_week)]]
    
    # Get unique features (remove duplicates from different horizons)
    unique_features <- week_data[!duplicated(week_data$feature), ]
    
    # Create visualization
    p <- ggplot(unique_features, aes(x = percentile, y = importance, label = feature)) +
      geom_point(aes(size = importance, color = abs(percentile - 50)), alpha = 0.7) +
      geom_text_repel(data = unique_features[unique_features$importance > 3 | 
                                               abs(unique_features$percentile - 50) > 40, ],
                      size = 3, max.overlaps = 15) +
      scale_color_gradient(low = "blue", high = "red") +
      labs(title = paste("Feature Analysis for Base Week", base_week),
           subtitle = "Importance vs. Percentile of Feature Values",
           x = "Percentile of Feature Value",
           y = "Feature Importance (%)",
           color = "Extremeness",
           size = "Importance") +
      theme_minimal() +
      geom_vline(xintercept = 50, linetype = "dashed", color = "gray") +
      geom_vline(xintercept = c(10, 90), linetype = "dotted", color = "gray") +
      scale_x_continuous(breaks = seq(0, 100, 10))
    
    print(p)
  }
  
  return(analysis_results)
}

# Helper function to generate feature vector for a specific week and horizon
generate_feature_vector_for_analysis <- function(base_week, horizon) {
  # Get recent values for prediction
  v1 <- y_test$casos[base_week]
  v2 <- y_test$casos[base_week - 1]
  v3 <- y_test$casos[base_week - 2]
  v4 <- y_test$casos[base_week - 3]
  
  avg_precip_lag <- ((y_test$precip_tot_sum[base_week]) + 
                       (y_test$precip_tot_sum[base_week - 1]) + 
                       (y_test$precip_tot_sum[base_week - 2]) + 
                       (y_test$precip_tot_sum[base_week -3])) / 4
  
  # Get week number for seasonality
  week_number <- y_test$week_number[base_week + horizon]
  
  # Initialize all features that will be used
  current_temp_med_lag1 <- y_test$temp_med_avg[base_week]
  current_temp_med_lag2 <- y_test$temp_med_avg[base_week]
  current_temp_med_lag3 <- y_test$temp_med_avg[base_week]
  current_temp_med_lag4 <- y_test$temp_med_avg[base_week]
  current_umid_med_lag1 <- y_test$umid_med_avg[base_week]
  current_umid_med_lag2 <- y_test$umid_med_avg[base_week]
  current_umid_med_lag3 <- y_test$umid_med_avg[base_week]
  current_umid_med_lag4 <- y_test$umid_med_avg[base_week]
  current_precip_tot_lag1 <- avg_precip_lag
  current_precip_tot_lag2 <- avg_precip_lag
  current_precip_tot_lag3 <- avg_precip_lag
  current_precip_tot_lag4 <- avg_precip_lag
  current_temp_competence_optimality <- y_test$temp_competence_optimality[base_week]
  current_humidity_risk <- y_test$humidity_risk[base_week]
  current_temp_competence_humidty_risk <- y_test$temp_competence_humidty_risk[base_week]
  
  # New features
  current_temp_volatility <- ifelse("temp_volatility" %in% colnames(y_test), 
                                    y_test$temp_volatility[base_week], 
                                    sd(c(y_test$temp_med_avg[base_week:(base_week-3)])))
  
  current_favorable_weeks_count <- ifelse("favorable_weeks_count" %in% colnames(y_test), 
                                          y_test$favorable_weeks_count[base_week], 0)
  
  current_dry_then_wet <- ifelse("dry_then_wet" %in% colnames(y_test), 
                                 y_test$dry_then_wet[base_week], 0)
  
  current_climate_state_change <- ifelse("climate_state_change" %in% colnames(y_test), 
                                         y_test$climate_state_change[base_week], 0)
  
  # Set case lags based on horizon
  if (horizon == 1) {
    current_lag1 <- v1
    current_lag2 <- v2
    current_lag3 <- v3
    current_lag4 <- v4
  } else if (horizon == 2) {
    # For horizon 2, we'd use the predicted value from horizon 1
    horizon1_pred_row <- all_predictions[all_predictions$base_week == base_week & 
                                           all_predictions$horizon == 1, ]
    
    if (nrow(horizon1_pred_row) > 0) {
      current_lag1 <- horizon1_pred_row$predicted[1]
    } else {
      # If no prediction found, use the actual value as fallback
      current_lag1 <- y_test$casos[base_week + 1]
    }
    
    current_lag2 <- v1
    current_lag3 <- v2
    current_lag4 <- v3
  } else if (horizon == 3) {
    # For horizon 3, use predictions from horizons 1 and 2
    horizon1_pred_row <- all_predictions[all_predictions$base_week == base_week & 
                                           all_predictions$horizon == 1, ]
    horizon2_pred_row <- all_predictions[all_predictions$base_week == base_week & 
                                           all_predictions$horizon == 2, ]
    
    if (nrow(horizon2_pred_row) > 0) {
      current_lag1 <- horizon2_pred_row$predicted[1]
    } else {
      current_lag1 <- y_test$casos[base_week + 2]
    }
    
    if (nrow(horizon1_pred_row) > 0) {
      current_lag2 <- horizon1_pred_row$predicted[1]
    } else {
      current_lag2 <- y_test$casos[base_week + 1]
    }
    
    current_lag3 <- v1
    current_lag4 <- v2
  } else if (horizon == 4) {
    # For horizon 4, use predictions from horizons 1, 2, and 3
    horizon1_pred_row <- all_predictions[all_predictions$base_week == base_week & 
                                           all_predictions$horizon == 1, ]
    horizon2_pred_row <- all_predictions[all_predictions$base_week == base_week & 
                                           all_predictions$horizon == 2, ]
    horizon3_pred_row <- all_predictions[all_predictions$base_week == base_week & 
                                           all_predictions$horizon == 3, ]
    
    if (nrow(horizon3_pred_row) > 0) {
      current_lag1 <- horizon3_pred_row$predicted[1]
    } else {
      current_lag1 <- y_test$casos[base_week + 3]
    }
    
    if (nrow(horizon2_pred_row) > 0) {
      current_lag2 <- horizon2_pred_row$predicted[1]
    } else {
      current_lag2 <- y_test$casos[base_week + 2]
    }
    
    if (nrow(horizon1_pred_row) > 0) {
      current_lag3 <- horizon1_pred_row$predicted[1]
    } else {
      current_lag3 <- y_test$casos[base_week + 1]
    }
    
    current_lag4 <- v1
  }
  
  # Calculate derived values
  mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
  avg <- mean(c(current_lag1, current_lag2, current_lag3))
  wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
  
  # Calculate decay
  if (current_lag1 < current_lag2 && current_lag2 < current_lag3) {
    declines <- c((current_lag1 - current_lag2) / current_lag2,
                  (current_lag2 - current_lag3) / current_lag3)
    current_decay <- mean(declines)
  } else {
    current_decay <- 0
  }
  
  # Create feature vector
  features <- list(
    casos_lag1 = current_lag1,
    casos_lag2 = current_lag2,
    casos_lag3 = current_lag3,
    casos_lag4 = current_lag4,
    temp_med_lag1 = current_temp_med_lag1,
    temp_med_lag2 = current_temp_med_lag2,
    temp_med_lag3 = current_temp_med_lag3,
    temp_med_lag4 = current_temp_med_lag4,
    umid_med_lag1 = current_umid_med_lag1,
    umid_med_lag2 = current_umid_med_lag2,
    umid_med_lag3 = current_umid_med_lag3,
    umid_med_lag4 = current_umid_med_lag4,
    precip_tot_lag1 = current_precip_tot_lag1,
    precip_tot_lag2 = current_precip_tot_lag2,
    precip_tot_lag3 = current_precip_tot_lag3,
    precip_tot_lag4 = current_precip_tot_lag4,
    temp_competence_optimality = current_temp_competence_optimality,
    humidity_risk = current_humidity_risk,
    temp_competence_humidty_risk = current_temp_competence_humidty_risk,
    sin_year = sin(2 * pi * week_number/52),
    cos_year = cos(2 * pi * week_number/52),
    casoslag1_mov_sd = mov_sd,
    avg = avg,
    wavg = wavg,
    decay = current_decay,
    temp_volatility = current_temp_volatility,
    favorable_weeks_count = current_favorable_weeks_count,
    dry_then_wet = current_dry_then_wet,
    climate_state_change = current_climate_state_change
  )
  
  return(features)
}

# Analyze the problematic weeks
problem_analysis <- analyze_problem_weeks(post_model, c(31))
