# ==========================================
# Dengue Prediction Model using BART
# ==========================================

# Load required libraries
library(BART)
library(MASS)
library(httr)
library(jsonlite)
library(mlr3)
library(ggplot2)
library(dplyr)
library(zoo)
library(forecast)

# Set global parameters
set.seed(7)
GEOCODE <- "3304557"  # Rio de Janeiro
API_KEY <- "jodenapole:27ab1f15-5cf9-4bae-a1e8-971baf371e9d"

# Disable SSL verification (not recommended for production)
httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))

# ===========================================
# Data Collection Functions
# ===========================================

#' Fetch climate data from API
#' 
#' @param start_date Start date in YYYY-MM-DD format
#' @param end_date End date in YYYY-MM-DD format
#' @param geocode Location code
#' @return Dataframe with climate data
fetch_climate_data <- function(start_date, end_date, geocode = GEOCODE) {
  climate_api <- "https://api.mosqlimate.org/api/datastore/climate/?"
  climate_data <- data.frame()
  total_pages_estimate <- 500
  
  for (page in 1:total_pages_estimate) {
    pagination <- paste0("page=", page, "&per_page=100")
    date_param <- paste0("&start=", start_date, "&end=", end_date)
    geocode_param <- paste0("&geocode=", geocode)
    
    url <- paste0(climate_api, pagination, date_param, geocode_param)
    resp <- GET(url, add_headers(.headers = c("X-UID-Key" = API_KEY)))
    
    if (http_error(resp)) {
      message(paste("Error:", status_code(resp)))
      break
    }
    
    json_content <- fromJSON(content(resp, "text"))
    items <- json_content$items
    
    climate_data <- rbind(climate_data, items)
    
    if (nrow(items) < 100) {
      break
    }
  }
  
  return(climate_data)
}

#' Fetch dengue case data from API
#' 
#' @param start_date Start date in YYYY-MM-DD format
#' @param end_date End date in YYYY-MM-DD format
#' @param geocode Location code
#' @return Dataframe with dengue case data
fetch_dengue_data <- function(start_date, end_date, geocode = GEOCODE) {
  dengue_api <- "https://api.mosqlimate.org/api/datastore/infodengue/?disease=dengue"
  dengue_data <- data.frame()
  total_pages_estimate <- 500
  
  for (page in 1:total_pages_estimate) {
    pagination <- paste0("&page=", page, "&per_page=100")
    date_param <- paste0("&start=", start_date, "&end=", end_date)
    geocode_param <- paste0("&geocode=", geocode)
    
    url <- paste0(dengue_api, date_param, geocode_param, pagination)
    resp <- GET(url, add_headers(.headers = c("X-UID-Key" = API_KEY)))
    
    if (http_error(resp)) {
      message(paste("Error:", status_code(resp)))
      break
    }
    
    json_content <- fromJSON(content(resp, "text"))
    items <- json_content$items
    
    dengue_data <- rbind(dengue_data, items)
    
    if (nrow(items) < 100) {
      break
    }
  }
  
  # Reverse order to get ascending dates
  dengue_data_ordered <- dengue_data[nrow(dengue_data):1, ]
  return(dengue_data_ordered)
}

#' Aggregate daily data to weekly
#' 
#' @param daily_data Daily climate data
#' @param weeks Number of weeks
#' @return Weekly aggregated data
aggregate_to_weekly <- function(daily_data, weeks) {
  daily_data$week_number <- rep(1:weeks, each = 7, length.out = nrow(daily_data))
  
  weekly_data <- daily_data %>%
    group_by(week_number) %>%
    summarize(across(everything(), mean, na.rm = TRUE))
  
  weekly_data <- weekly_data %>%
    mutate(week_of_year = (week_number - 1) %% 52 + 1)
  
  return(weekly_data)
}

# ===========================================
# Feature Engineering Functions
# ===========================================

#' Create lagged features for dengue cases
#' 
#' @param data Dengue data
#' @param lag_values Initial values for lags if needed
#' @return Data with lag features
create_lag_features <- function(data, lag_values = NULL) {
  # Create lag features
  data$casos_lag1 <- dplyr::lag(data$casos, 1)
  data$casos_lag2 <- dplyr::lag(data$casos, 2)
  data$casos_lag3 <- dplyr::lag(data$casos, 3)
  data$casos_lag4 <- dplyr::lag(data$casos, 4)
  
  # Fill in the first few values if provided
  if (!is.null(lag_values) && length(lag_values) == 4) {
    # lag1
    data$casos_lag1[1] <- lag_values[1]
    
    # lag2
    data$casos_lag2[1] <- lag_values[2]
    data$casos_lag2[2] <- lag_values[1]
    
    # lag3
    data$casos_lag3[1] <- lag_values[3]
    data$casos_lag3[2] <- lag_values[2]
    data$casos_lag3[3] <- lag_values[1]
    
    # lag4
    data$casos_lag4[1] <- lag_values[4]
    data$casos_lag4[2] <- lag_values[3]
    data$casos_lag4[3] <- lag_values[2]
    data$casos_lag4[4] <- lag_values[1]
  }
  
  return(data)
}

#' Create time series features (moving stats, seasonality)
#' 
#' @param data Data with lag features
#' @param window_size Window size for moving calculations
#' @return Data with time series features added
create_time_series_features <- function(data, window_size = 3) {
  # Moving standard deviation
  data$casoslag1_mov_sd <- rollapply(
    data$casos_lag1,
    width = window_size,
    FUN = sd,
    fill = NA,
    align = "right"
  )
  
  # Handle first few values for moving sd
  if (window_size <= nrow(data)) {
    for (i in 1:min(window_size-1, nrow(data))) {
      if (i+window_size-1 <= nrow(data)) {
        data$casoslag1_mov_sd[i] <- sd(data$casos_lag1[i:(i+window_size-1)])
      }
    }
  }
  
  # Moving average
  data$avg <- rollmean(
    data$casos_lag1,
    k = window_size,
    fill = NA,
    align = "right"
  )
  
  # Handle first few values for moving average
  if (window_size <= nrow(data)) {
    for (i in 1:min(window_size-1, nrow(data))) {
      if (i+window_size-1 <= nrow(data)) {
        data$avg[i] <- mean(data$casos_lag1[i:(i+window_size-1)])
      }
    }
  }
  
  # Calculate weighted average
  data$wavg <- numeric(nrow(data))
  
  for (week in 1:nrow(data)) {
    if (week <= 3) {
      # For the first 3 weeks use lag values if available
      if (week == 1 && length(data$casos_lag1) >= 3) {
        data$wavg[1] <- ((data$casos_lag1[1] * 3) + (data$casos_lag2[1] * 2) + (data$casos_lag3[1])) / 6
      } else if (week == 2 && length(data$casos_lag1) >= 3) {
        data$wavg[2] <- ((data$casos_lag1[2] * 3) + (data$casos_lag1[1] * 2) + (data$casos_lag2[1])) / 6
      } else if (week == 3 && length(data$casos_lag1) >= 3) {
        data$wavg[3] <- ((data$casos_lag1[3] * 3) + (data$casos_lag1[2] * 2) + (data$casos_lag1[1])) / 6
      }
    } else {
      # For subsequent weeks, use previously calculated values
      data$wavg[week] <- ((data$casos[week - 1] * 3) + (data$casos[week - 2] * 2) + (data$casos[week - 3])) / 6
    }
  }
  
  # Weighted average - regular average difference
  data$diffavg <- data$wavg - data$avg
  
  # Extract week number from SE column if available
  if ("SE" %in% colnames(data)) {
    data$week_of_year <- as.numeric(substr(data$SE, 5, 6))
  } else if (!"week_of_year" %in% colnames(data)) {
    # If we don't have week_of_year, create a placeholder
    data$week_of_year <- 1:nrow(data) %% 52
    data$week_of_year[data$week_of_year == 0] <- 52
  }
  
  # Seasonal components
  data$sin_year <- sin(2 * pi * (data$week_of_year / 52))
  data$cos_year <- cos(2 * pi * (data$week_of_year / 52))
  
  return(data)
}

#' Create climate interaction features
#' 
#' @param data Dengue data with seasonal components
#' @param climate_data Weekly climate data
#' @return Data with climate interaction features
create_climate_interactions <- function(data, climate_data = NULL) {
  if (!is.null(climate_data) && "umid_med" %in% colnames(climate_data) && 
      "temp_med" %in% colnames(climate_data) && nrow(climate_data) >= nrow(data)) {
    
    # Use climate data if available
    data$humidity_sin_interaction <- climate_data$umid_med[1:nrow(data)] * data$sin_year
    data$humidity_cos_interaction <- climate_data$umid_med[1:nrow(data)] * data$cos_year
    data$temp_sin_interaction <- climate_data$temp_med[1:nrow(data)] * data$sin_year
    data$temp_cos_interaction <- climate_data$temp_med[1:nrow(data)] * data$cos_year
    
  } else if ("umidmed" %in% colnames(data) && "tempmed" %in% colnames(data)) {
    # Use columns in dengue data if climate data not provided
    data$humidity_sin_interaction <- data$umidmed * data$sin_year
    data$humidity_cos_interaction <- data$umidmed * data$cos_year
    data$temp_sin_interaction <- data$tempmed * data$sin_year
    data$temp_cos_interaction <- data$tempmed * data$cos_year
  }
  
  return(data)
}

# ===========================================
# Model Training and Evaluation Functions
# ===========================================

#' Prepare model input data
#' 
#' @param dengue_data Processed dengue data
#' @param include_climate Whether to include climate variables
#' @return List with x_train and y_train
prepare_model_data <- function(dengue_data, include_climate = FALSE) {
  # Select the correct set of features based on whether climate should be included
  if (include_climate) {
    x_train <- cbind(
      dengue_data$casos_lag1,
      dengue_data$casos_lag2,
      dengue_data$casos_lag3,
      dengue_data$casos_lag4,
      dengue_data$sin_year,
      dengue_data$cos_year,
      dengue_data$humidity_sin_interaction,
      dengue_data$humidity_cos_interaction,
      dengue_data$casoslag1_mov_sd,
      dengue_data$avg,
      dengue_data$temp_sin_interaction,
      dengue_data$temp_cos_interaction
    )
    
    colnames(x_train) <- c(
      "casos_lag1", "casos_lag2", "casos_lag3", "casos_lag4",
      "sin_year", "cos_year", 
      "humidity_sin_interaction", "humidity_cos_interaction",
      "casoslag1_mov_sd", "avg",
      "temp_sin_interaction", "temp_cos_interaction"
    )
  } else {
    x_train <- cbind(
      dengue_data$casos_lag1,
      dengue_data$casos_lag2,
      dengue_data$casos_lag3,
      dengue_data$casos_lag4,
      dengue_data$sin_year,
      dengue_data$cos_year,
      dengue_data$casoslag1_mov_sd,
      dengue_data$avg
    )
    
    colnames(x_train) <- c(
      "casos_lag1", "casos_lag2", "casos_lag3", "casos_lag4",
      "sin_year", "cos_year", "casoslag1_mov_sd", "avg"
    )
  }
  
  # Convert to data frame
  x_train_df <- as.data.frame(x_train)
  
  # Get target values
  y_train <- cbind(dengue_data$casos)
  
  return(list(
    x_train = x_train,
    y_train = y_train,
    x_train_df = x_train_df
  ))
}

#' Calculate RMSE
#' 
#' @param actual Actual values
#' @param predicted Predicted values
#' @return RMSE value
calculate_rmse <- function(actual, predicted) {
  mse <- mean((predicted - actual)^2)
  rmse <- sqrt(mse)
  return(rmse)
}

#' Calculate R-squared
#' 
#' @param actual Actual values
#' @param predicted Predicted values
#' @return R-squared value
calculate_r2 <- function(actual, predicted) {
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  return(r_squared)
}

#' Training metrics tracking
#' 
#' @param metrics_df Current metrics dataframe
#' @param burn Burn parameter
#' @param ndpost Number of posterior draws
#' @param k K value
#' @param power Power value
#' @param ndtree Number of trees
#' @param rmse RMSE value
#' @param last_week Last week used
#' @return Updated metrics dataframe
update_metrics <- function(metrics_df, burn, ndpost, k, power, ndtree, rmse, last_week) {
  new_row <- data.frame(
    burn = burn, 
    ndpost = ndpost, 
    k = k, 
    power = power, 
    ndtree = ndtree, 
    rmse = rmse, 
    last_week = last_week
  )
  
  updated_metrics <- rbind(metrics_df, new_row)
  return(updated_metrics)
}

#' Plot model prediction results over full year
#' 
#' @param mean_predictions Mean predictions
#' @param lower_ci Lower confidence interval
#' @param upper_ci Upper confidence interval 
#' @param y_val Full year of actual data
#' @param weeks_label Starting week label
#' @param weeks_predicted Number of weeks predicted
#' @param rmse RMSE value
#' @return Nothing, creates plots
plot_results <- function(mean_predictions, y_val, weeks_label, weeks_predicted, 
                         rmse, lower_ci = NULL, upper_ci = NULL) {
  
  # Create a full data frame with all weeks of actual data
  full_plot_data <- data.frame(
    week_number = 1:length(y_val),
    actual_cases = y_val
  )
  
  # Add prediction data to the full data frame
  # Only add predictions for the specific weeks we've predicted
  full_plot_data$predicted <- NA  # Initialize with NA for all weeks
  full_plot_data$lower_ci <- NA   # Initialize lower confidence interval
  full_plot_data$upper_ci <- NA   # Initialize upper confidence interval
  
  # Add prediction data starting at the weeks_label position
  prediction_weeks <- weeks_label:(weeks_label + weeks_predicted - 1)
  full_plot_data$predicted[prediction_weeks] <- mean_predictions
  
  # Add confidence intervals if available
  if(!is.null(lower_ci) && !is.null(upper_ci)) {
    full_plot_data$lower_ci[prediction_weeks] <- lower_ci
    full_plot_data$upper_ci[prediction_weeks] <- upper_ci
  }
  
  # Print the predictions vs actual for the predicted weeks
  prediction_data <- data.frame(
    week = prediction_weeks,
    actual = y_val[prediction_weeks],
    predicted = mean_predictions
  )
  print("Prediction Results:")
  print(prediction_data)
  
  # Create the full year plot using ggplot2
  p1 <- ggplot(full_plot_data, aes(x = week_number)) +
    geom_line(aes(y = actual_cases, color = "Actual Dengue Cases"), linewidth = 1) +
    geom_point(aes(y = actual_cases, color = "Actual Dengue Cases"), size = 2) +
    geom_point(aes(y = predicted, color = "Prediction"), size = 3, na.rm = TRUE) +
    geom_line(aes(y = predicted, color = "Prediction"), linewidth = 1, na.rm = TRUE) +
    labs(title = paste("Dengue Cases Predictions - RMSE:", round(rmse, 2)),
         subtitle = paste("Predictions starting from week", weeks_label),
         x = "Week Number",
         y = "Number of Cases",
         color = "Data Type") +
    scale_color_manual(values = c("Actual Dengue Cases" = "black", "Prediction" = "red")) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # If confidence intervals are available, create a second plot with them
  if(!is.null(lower_ci) && !is.null(upper_ci)) {
    p2 <- ggplot(full_plot_data, aes(x = week_number)) +
      geom_line(aes(y = actual_cases, color = "Actual Dengue Cases"), linewidth = 1) +
      geom_point(aes(y = actual_cases, color = "Actual Dengue Cases"), size = 2) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
                  fill = "red", alpha = 0.2, na.rm = TRUE) +
      geom_point(aes(y = predicted, color = "Prediction"), size = 3, na.rm = TRUE) +
      geom_line(aes(y = predicted, color = "Prediction"), linewidth = 1, na.rm = TRUE) +
      labs(title = paste("Dengue Cases Predictions with Confidence Intervals - RMSE:", round(rmse, 2)),
           subtitle = paste("Predictions starting from week", weeks_label),
           x = "Week Number",
           y = "Number of Cases",
           color = "Data Type") +
      scale_color_manual(values = c("Actual Dengue Cases" = "black", "Prediction" = "red")) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
    print(p1)
    print(p2)
  } else {
    print(p1)
  }
}

#' Plot variable importance
#' 
#' @param bart_model Trained BART model
#' @param feature_names Names of features
#' @return Nothing, creates plot
plot_var_imp <- function(bart_model, feature_names) {
  # Get variable importance
  var_imp <- bart_model$varcount.mean
  
  # Normalize to percentages
  var_imp_perc <- (var_imp/sum(var_imp)) * 100
  
  # Assign feature names
  names(var_imp_perc) <- feature_names
  
  # Create a data frame for plotting
  importance_df <- data.frame(
    Variable = names(var_imp_perc),
    Importance = var_imp_perc
  )
  
  # Create plot
  p <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = "Variable Importance (%)",
         x = "Variable",
         y = "Importance (%)") +
    theme_bw()
  
  print(p)
}

#' Generate predictions for future weeks
#' 
#' @param model Trained BART model
#' @param last_week Last week to use as base
#' @param recent_values Recent values to use for prediction
#' @param weeks_to_predict Number of weeks to predict
#' @param weekly_climate Weekly climate averages if needed
#' @return List with predictions and confidence intervals
predict_future_weeks <- function(model, last_week, recent_values, weeks_to_predict = 4, weekly_climate = NULL) {
  mean_predictions <- numeric(weeks_to_predict)
  li_predictions <- numeric(weeks_to_predict)
  ui_predictions <- numeric(weeks_to_predict)
  
  # Extract initial lags from recent values
  v1 <- recent_values[1]  # most recent case count
  v2 <- recent_values[2]  # 1 week before
  v3 <- recent_values[3]  # 2 weeks before
  v4 <- recent_values[4]  # 3 weeks before
  
  # Generate predictions recursively
  for (week in 1:weeks_to_predict) {
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
    } else {
      current_lag1 <- mean_predictions[3]
      current_lag2 <- mean_predictions[2]
      current_lag3 <- mean_predictions[1]
      current_lag4 <- v1
    }
    
    # Calculate moving statistics
    mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
    avg <- mean(c(current_lag1, current_lag2, current_lag3))
    wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
    
    # Current week number
    week_number <- (last_week + week) %% 52
    week_number <- ifelse(week_number == 0, 52, week_number)
    
    # Create prediction data
    recursive_data <- data.frame(
      casos_lag1 = current_lag1,
      casos_lag2 = current_lag2,
      casos_lag3 = current_lag3,
      casos_lag4 = current_lag4,
      sin_year = sin(2 * pi * week_number/52),
      cos_year = cos(2 * pi * week_number/52),
      casoslag1_mov_sd = mov_sd,
      avg = avg
    )
    
    # Make prediction
    prediction <- predict(model, newdata = recursive_data)
    mean_prediction <- apply(prediction, 2, mean)
    li_prediction <- apply(prediction, 2, quantile, probs = 0.025)
    ui_prediction <- apply(prediction, 2, quantile, probs = 0.975)
    
    # Store predictions
    mean_predictions[week] <- mean_prediction
    li_predictions[week] <- li_prediction
    ui_predictions[week] <- ui_prediction
  }
  
  return(list(
    mean = mean_predictions,
    lower = li_predictions,
    upper = ui_predictions
  ))
}

# ===========================================
# Main Execution
# ===========================================

#' Run the entire dengue prediction workflow
#' 
#' @param train_years Years to use for training (e.g., c("2019", "2020", "2021", "2022"))
#' @param test_year Year to use for testing (e.g., "2023")
#' @param prediction_week Week to start prediction from
#' @param weeks_to_predict Number of weeks to predict
#' @param model_params List with BART model parameters
#' @return Trained model and prediction results
run_dengue_prediction <- function(train_years, test_year, prediction_week = 7, 
                                  weeks_to_predict = 4, 
                                  model_params = list(burn = 13000, ndpost = 5000, 
                                                      k = 2, power = 2, ntree = 150)) {
  # Initialize results
  results <- list()
  metrics <- data.frame()
  
  # === DATA COLLECTION ===
  
  # Training data
  train_start <- paste0(min(train_years), "-01-01")
  train_end <- paste0(max(train_years), "-12-31")
  
  message("Fetching training climate data...")
  climate_data_train <- fetch_climate_data(train_start, train_end)
  climate_data_weekly_train <- aggregate_to_weekly(climate_data_train, 52 * length(train_years))
  
  message("Fetching training dengue data...")
  dengue_data_train <- fetch_dengue_data(train_start, train_end)
  
  # Test data
  test_start <- paste0(test_year, "-01-01")
  test_end <- paste0(test_year, "-12-31")
  
  message("Fetching test climate data...")
  climate_data_test <- fetch_climate_data(test_start, test_end)
  climate_data_weekly_test <- aggregate_to_weekly(climate_data_test, 52)
  
  message("Fetching test dengue data...")
  dengue_data_test <- fetch_dengue_data(test_start, test_end)
  
  # === FEATURE ENGINEERING ===
  
  message("Creating training features...")
  # For training data, use last 4 values from previous year if available
  previous_year_end <- as.numeric(train_years[1]) - 1
  if (previous_year_end >= 2018) {
    previous_data <- fetch_dengue_data(
      paste0(previous_year_end, "-12-01"),
      paste0(previous_year_end, "-12-31")
    )
    
    if (nrow(previous_data) >= 4) {
      lag_values <- tail(previous_data$casos, 4)
    } else {
      # Example fallback values (you should replace with better estimates if possible)
      lag_values <- c(179, 147, 124, 138)
    }
  } else {
    # Example fallback values
    lag_values <- c(179, 147, 124, 138)
  }
  
  # Create features for training data
  dengue_train_processed <- dengue_data_train %>%
    create_lag_features(lag_values) %>%
    create_time_series_features() %>%
    create_climate_interactions(climate_data_weekly_train)
  
  # For test data, use last 4 values from training data
  test_lag_values <- tail(dengue_train_processed$casos, 4)
  
  # Create features for test data
  dengue_test_processed <- dengue_data_test %>%
    create_lag_features(test_lag_values) %>%
    create_time_series_features() %>%
    create_climate_interactions(climate_data_weekly_test)
  
  # === MODEL TRAINING ===
  
  message("Preparing model data...")
  train_data <- prepare_model_data(dengue_train_processed)
  
  message("Training BART model...")
  bart_model <- wbart(
    train_data$x_train,
    train_data$y_train, 
    nskip = model_params$burn, 
    ndpost = model_params$ndpost, 
    k = model_params$k, 
    power = model_params$power, 
    ntree = model_params$ntree
  )
  
  # === MODEL EVALUATION ===
  
  # Make predictions on full test set
  test_data <- prepare_model_data(dengue_test_processed)
  full_test_predictions <- predict(bart_model, newdata = test_data$x_train)
  mean_full_predictions <- apply(full_test_predictions, 2, mean)
  
  # Calculate RMSE for full test set
  full_rmse <- calculate_rmse(dengue_test_processed$casos, mean_full_predictions)
  message(paste("Full test set RMSE:", round(full_rmse, 2)))
  
  # === RECURSIVE PREDICTION ===
  
  message("Generating future predictions...")
  # Get recent values for starting prediction
  recent_values <- c(
    dengue_test_processed$casos[prediction_week],
    dengue_test_processed$casos[prediction_week-1],
    dengue_test_processed$casos[prediction_week-2],
    dengue_test_processed$casos[prediction_week-3]
  )
  
  # Generate predictions
  predictions <- predict_future_weeks(
    model = bart_model,
    last_week = prediction_week,
    recent_values = recent_values,
    weeks_to_predict = weeks_to_predict
  )
  
  # Get actual values for comparison
  actual_values <- dengue_test_processed$casos[(prediction_week+1):(prediction_week+weeks_to_predict)]
  
  # Calculate RMSE for predictions
  prediction_rmse <- calculate_rmse(actual_values, predictions$mean)
  message(paste("Prediction RMSE:", round(prediction_rmse, 2)))
  
  # Update metrics
  metrics <- update_metrics(
    metrics, 
    model_params$burn, 
    model_params$ndpost, 
    model_params$k, 
    model_params$power, 
    model_params$ntree, 
    prediction_rmse,
    prediction_week
  )
  
  # Plot results
  plot_results(
    mean_predictions = predictions$mean,
    y_val = dengue_test_processed$casos,
    weeks_label = prediction_week + 1,
    weeks_predicted = weeks_to_predict,
    rmse = prediction_rmse,
    lower_ci = predictions$lower,
    upper_ci = predictions$upper
  )
  
  # Plot variable importance
  feature_names <- colnames(train_data$x_train_df)
  plot_var_imp(bart_model, feature_names)
  
  # Return results
  results <- list(
    model = bart_model,
    train_data = train_data,
    test_data = test_data,
    metrics = metrics,
    predictions = predictions,
    actual_values = actual_values,
    rmse = prediction_rmse
  )
  
  return(results)
}

# ===========================================
# Example Usage
# ===========================================

# Run the full workflow
run_example <- function() {
  # Define parameters
  train_years <- c("2019", "2020", "2021", "2022")
  test_year <- "2023"
  prediction_week <- 7
  weeks_to_predict <- 4
  
  model_params <- list(
    burn = 13000,
    ndpost = 5000,
    k = 2, 
    power = 2, 
    ntree = 150
  )
  
  # Run the model
  results <- run_dengue_prediction(
    train_years = train_years,
    test_year = test_year,
    prediction_week = prediction_week,
    weeks_to_predict = weeks_to_predict,
    model_params = model_params
  )
  
  return(results)
}

# Uncomment to run
results <- run_example()
