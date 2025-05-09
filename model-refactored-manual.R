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
  
  # 7. Create a "decay rate" feature specifically for the declining phase
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
  
  # Apply the function to your data
  dengue_data_ordered <- fill_lag_columns(dengue_data_ordered, temp_weekly_avg)
  
  # TRAIN TEST SPLIT
  dengue_data_train <- subset(dengue_data_ordered, SE <202301)
  dengue_data_test <- subset(dengue_data_ordered, SE >202252)
  climate_data_train <- subset(climate_data, epiweek <202301)
  
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
    dengue_data_test$casos
  )
  
  y_test <- as.data.frame(y_test) # only used for reference, not to train or test.
  
  colnames(y_test) <- c(
    "casos"
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

# CREATING IMPORTANT VARIABLES #
climate_data <- api_calls(api_name = "climate")
dengue_data_ordered <- api_calls(api_name = "dengue")
x_train <- feature_creation() %>%
  .$x_train_df
y_train <- feature_creation() %>%
  .$y_train
y_test <- feature_creation() %>%
  .$y_test


# TRAINING MODEL
set.seed(7)
burn <- 13000
nd <- 5000
k_value <- 2
power_value <- 2
ntree_value <- 150L
post <- wbart(x_train, y_train, nskip = burn, ndpost = nd, k=k_value, power = power_value, ntree = ntree_value )

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
    
    weekly_averages <- subset(climate_data, epiweek <202301) %>%
      group_by(week_of_year) %>%
      summarize(
        avg_humidity = mean(umid_med_avg, na.rm = TRUE),
        avg_temperature = mean(temp_med_avg, na.rm = TRUE),
      )
    
    # Generate predictions for each week ahead
    for (week in 1:weeks_ahead) {
      # Set up lag values based on which week we're predicting
      if (week == 1) {
        current_temp_med_lag1 <- weekly_averages$avg_temperature[week_base]
        current_temp_med_lag2 <- weekly_averages$avg_temperature[week_base - 1]
        current_temp_med_lag3 <- weekly_averages$avg_temperature[week_base - 2]
        current_temp_med_lag4 <- weekly_averages$avg_temperature[week_base - 3]
        current_umid_med_lag1 <- weekly_averages$avg_humidity[week_base]
        current_umid_med_lag2 <- weekly_averages$avg_humidity[week_base - 1]
        current_umid_med_lag3 <- weekly_averages$avg_humidity[week_base - 2]
        current_umid_med_lag4 <- weekly_averages$avg_humidity[week_base - 3]
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
        current_temp_med_lag1 <- weekly_averages$avg_temperature[week_base + 1]
        current_temp_med_lag2 <- weekly_averages$avg_temperature[week_base]
        current_temp_med_lag3 <- weekly_averages$avg_temperature[week_base - 1]
        current_temp_med_lag4 <- weekly_averages$avg_temperature[week_base - 2]
        current_umid_med_lag1 <- weekly_averages$avg_humidity[week_base + 1]
        current_umid_med_lag2 <- weekly_averages$avg_humidity[week_base]
        current_umid_med_lag3 <- weekly_averages$avg_humidity[week_base - 1]
        current_umid_med_lag4 <- weekly_averages$avg_humidity[week_base - 2]
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
        current_temp_med_lag1 <- weekly_averages$avg_temperature[week_base + 2]
        current_temp_med_lag2 <- weekly_averages$avg_temperature[week_base + 1]
        current_temp_med_lag3 <- weekly_averages$avg_temperature[week_base]
        current_temp_med_lag4 <- weekly_averages$avg_temperature[week_base - 1]
        current_umid_med_lag1 <- weekly_averages$avg_humidity[week_base + 2]
        current_umid_med_lag2 <- weekly_averages$avg_humidity[week_base + 1]
        current_umid_med_lag3 <- weekly_averages$avg_humidity[week_base]
        current_umid_med_lag4 <- weekly_averages$avg_humidity[week_base - 1]
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
        current_temp_med_lag1 <- weekly_averages$avg_temperature[week_base + 3]
        current_temp_med_lag2 <- weekly_averages$avg_temperature[week_base + 2]
        current_temp_med_lag3 <- weekly_averages$avg_temperature[week_base + 1]
        current_temp_med_lag4 <- weekly_averages$avg_temperature[week_base]
        current_umid_med_lag1 <- weekly_averages$avg_humidity[week_base + 3]
        current_umid_med_lag2 <- weekly_averages$avg_humidity[week_base + 2]
        current_umid_med_lag3 <- weekly_averages$avg_humidity[week_base + 1]
        current_umid_med_lag4 <- weekly_averages$avg_humidity[week_base]
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
        current_temp_med_lag1 <- weekly_averages$avg_temperature[week_base + (week - 1)]
        current_temp_med_lag2 <- weekly_averages$avg_temperature[week_base + (week - 2)]
        current_temp_med_lag3 <- weekly_averages$avg_temperature[week_base + (week - 3)]
        current_temp_med_lag4 <- weekly_averages$avg_temperature[week_base + (week - 4)]
        current_umid_med_lag1 <- weekly_averages$avg_humidity[week_base + (week - 1)]
        current_umid_med_lag2 <- weekly_averages$avg_humidity[week_base + (week - 2)]
        current_umid_med_lag3 <- weekly_averages$avg_humidity[week_base + (week - 3)]
        current_umid_med_lag4 <- weekly_averages$avg_humidity[week_base + (week - 4)]
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
        temp_med_lag1 = current_temp_med_lag1,
        temp_med_lag2 = current_temp_med_lag2,
        temp_med_lag3 = current_temp_med_lag3,
        temp_med_lag4 = current_temp_med_lag4,
        umid_med_lag1 = current_umid_med_lag1,
        umid_med_lag2 = current_umid_med_lag2,
        umid_med_lag3 = current_umid_med_lag3,
        umid_med_lag4 = current_umid_med_lag4,
        sin_year = sin(2 * pi * week_number/52),
        cos_year = cos(2 * pi * week_number/52),
        casoslag1_mov_sd = mov_sd,
        avg = avg,
        wavg = wavg,
        decay = current_decay
      )
      
      # Make prediction
      prediction <- predict(post, newdata = recursive_data)
      mean_prediction <- apply(prediction, 2, mean)
      li_prediction <- apply(prediction, 2, quantile, probs = 0.025)
      ui_prediction <- apply(prediction, 2, quantile, probs = 0.975)
      
      # Store predictions
      mean_predictions_2023[week] <- mean_prediction
      li_predictions_2023[week] <- li_prediction
      ui_predictions_2023[week] <- ui_prediction
    }
    
    # Calculate RMSE for current prediction
    # current_rmse <- calculate_rmse(actual_values, mean_predictions_2023)
    
    # Add the current predictions to the overall data frame
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
        lower_ci = li_predictions_2023[i],
        upper_ci = ui_predictions_2023[i],
        horizon = i  # How many weeks ahead prediction
        # rmse = current_rmse
      )
      
      all_predictions <- rbind(all_predictions, new_row)
    }
    
    # Print progress
    cat(sprintf("Processed base week %d/%d\r", week_base, end_week))
  }
  
  return(all_predictions)
}

# Generate predictions for all base weeks
all_predictions <- generate_predictions_across_year(start_week = 29, end_week = 29, weeks_ahead = 4) 
output_format = "mp4"

# Create and save the animation
create_prediction_animation_year(all_predictions, output_format, glue("dengue_predictions_2023_comparison.{output_format}"))

plot_full_year()




