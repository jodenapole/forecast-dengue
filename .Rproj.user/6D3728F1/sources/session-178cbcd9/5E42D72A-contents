library("BART")
library("MASS")
library(httr)
library(jsonlite)
library(mlr3)
library(ggplot2)
library(dplyr)
library(zoo)
library(forecast)

httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)) # THE API CERTIFICATE IS EXPIRED, REMOVE THIS AS SOON AS POSSIBLE

# API CALL FOR CLIMATE 2019 - 2022
climate_api <- "https://api.mosqlimate.org/api/datastore/climate/?"
page <- 1
date <- paste0("&start=2019-01-01&end=2022-12-31")
geocode <- paste0("&geocode=3304557")
total_pages_estimate <- 500
pb <- txtProgressBar(min = 0, max = total_pages_estimate, style = 3)

climate_data <- data.frame()
"https://api.mosqlimate.org/api/datastore/climate/?start=2024-01-01&end=2024-02-01&page=1&per_page=300"
for (pagenumber in 1:total_pages_estimate) { # Loop until there are no more pages
  
  pagination <- paste0("page=", pagenumber, "&per_page=100")
  url <- paste0(climate_api, pagination, date, geocode)
  
  resp <- GET(url)
  
  if (http_error(resp)) {
    message(paste("Error:", status_code(resp))) # Handle errors gracefully
    break  # Exit the loop if there's an API error
  }
  
  json_content <- fromJSON(content(resp, "text"))
  items <- json_content$items
  
  climate_data <- rbind(climate_data, items)  # Append the current page's items
  
  if (nrow(items) < 100) { # Check if the current page is empty (no more data)
    print(nrow(items))
    break # Exit the loop if there's no more data
  }
  
}

#ORGANAZING DATA FROM DAILY TO WEEKLY 2019 - 2022
climate_data$week_number <- rep(1:208, each = 7, length.out = nrow(climate_data))
climate_data_weekly <- climate_data %>%
  group_by(week_number) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

# CREATING A COLUMN WEEK YEAR
climate_data_weekly <- climate_data_weekly %>%
  mutate(week_of_year = (week_number - 1) %% 52 + 1) #  Calculates week within the year (1-52)



# API CALL FOR CLIMATE 2023
climate_api <- "https://api.mosqlimate.org/api/datastore/climate/?"
date <- paste0("&start=2023-01-01&end=2023-12-31")
geocode <- paste0("&geocode=3304557")
total_pages_estimate <- 500

climate_data_test <- data.frame()

for (pagenumber in 1:total_pages_estimate) { # Loop until there are no more pages
  
  pagination <- paste0("page=", pagenumber, "&per_page=100")
  url <- paste0(climate_api, pagination, date, geocode)
  
  resp <- GET(url)
  
  if (http_error(resp)) {
    message(paste("Error:", status_code(resp))) # Handle errors gracefully
    break  # Exit the loop if there's an API error
  }
  
  json_content <- fromJSON(content(resp, "text"))
  items <- json_content$items
  
  climate_data_test <- rbind(climate_data_test, items)  # Append the current page's items
  
  if (nrow(items) < 100) { # Check if the current page is empty (no more data)
    print(nrow(items))
    break # Exit the loop if there's no more data
  }
  
}

#ORGANAZING DATA FROM DAILY TO WEEKLY 2023
climate_data_test$week_number <- rep(1:52, each = 7, length.out = nrow(climate_data_test))
climate_data_test_weekly <- climate_data_test %>%
  group_by(week_number) %>%
  summarize(across(everything(), mean, na.rm = TRUE))


# API CALL FOR DENGUE CASES 2019 - 2022
dengue_api <- "https://api.mosqlimate.org/api/datastore/infodengue/?disease=dengue"
date <- paste0("&start=2019-01-01&end=2022-12-31")
geocode <- paste0("&geocode=3304557")
total_pages_estimate <- 500

dengue_data <- data.frame()

for (pagenumber in 1:total_pages_estimate) { # Loop until there are no more pages
  
  pagination <- paste0("&page=", pagenumber, "&per_page=100")
  url <- paste0(dengue_api, date, geocode, pagination)
  
  resp <- GET(url)
  
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

# ORDERING DATA IN ASCENDING ORDER
dengue_data_ordered <- dengue_data[nrow(dengue_data):1,]


# API CALL FOR DENGUE CASES 2023
dengue_api <- "https://api.mosqlimate.org/api/datastore/infodengue/?disease=dengue"
date <- paste0("&start=2023-01-01&end=2023-12-31")
geocode <- paste0("&geocode=3304557")
total_pages_estimate <- 500

dengue_data_test <- data.frame()

for (pagenumber in 1:total_pages_estimate) { # Loop until there are no more pages
  
  pagination <- paste0("&page=", pagenumber, "&per_page=100")
  url <- paste0(dengue_api, date, geocode, pagination)
  
  resp <- GET(url)
  
  if (http_error(resp)) {
    message(paste("Error:", status_code(resp))) # Handle errors gracefully
    break  # Exit the loop if there's an API error
  }
  
  json_content <- fromJSON(content(resp, "text"))
  items <- json_content$items
  
  dengue_data_test <- rbind(dengue_data_test, items)  # Append the current page's items
  
  if (nrow(items) < 100) { # Check if the current page is empty (no more data)
    print(nrow(items))
    break # Exit the loop if there's no more data
  }
  
}

# ORDERING DATA IN ASCENDING ORDER
dengue_data_ordered_test <- dengue_data_test[nrow(dengue_data_test):1,]
dengue_data_ordered_test <- dengue_data_ordered_test[-nrow(dengue_data_ordered_test), ]


## FEATURE ENGENEERING TRAIN ##
window_size = 3 # All std dev will have the same window size


# 1 WEEK LAG
dengue_data_ordered$casos_lag1 <- dplyr::lag(dengue_data_ordered$casos, 1)
dengue_data_ordered$casos_lag1[1] = 179 # Since we're lagging the data in 1 week, the first week becomes NA. This is the cases for the last week of 2018

# 2 WEEK LAG
dengue_data_ordered$casos_lag2 <- dplyr::lag(dengue_data_ordered$casos, 2)
dengue_data_ordered$casos_lag2[1] = 147 # Same logic applies
dengue_data_ordered$casos_lag2[2] = 179 # Same logic applies

# 3 WEEK LAG
dengue_data_ordered$casos_lag3 <- dplyr::lag(dengue_data_ordered$casos, 3)
dengue_data_ordered$casos_lag3[1] = 124 # Same logic applies
dengue_data_ordered$casos_lag3[2] = 147 # Same logic applies
dengue_data_ordered$casos_lag3[3] = 179 # Same logic applies

# 4 WEEK LAG
dengue_data_ordered$casos_lag4 <- dplyr::lag(dengue_data_ordered$casos, 4)
dengue_data_ordered$casos_lag4[1] = 138 # Same logic applies
dengue_data_ordered$casos_lag4[2] = 124 # Same logic applies
dengue_data_ordered$casos_lag4[3] = 147 # Same logic applies
dengue_data_ordered$casos_lag4[4] = 179 # Same logic applies



# MOVING STD DEVIATION
dengue_data_ordered$casoslag1_mov_sd <- rollapply(dengue_data_ordered$casos_lag1,
                                                  width = window_size,
                                                  FUN = sd,
                                                  fill = NA,
                                                  align = "right")
dengue_data_ordered$casoslag1_mov_sd[1] <- sd(c(179, 147, 124))
dengue_data_ordered$casoslag1_mov_sd[2] <- sd(c(186, 179, 147))

# MOVING AVERAGE
dengue_data_ordered$avg <- rollmean(dengue_data_ordered$casos_lag1,
                                    k = 3,
                                    fill = NA,
                                    align = "right")
dengue_data_ordered$avg[1] <- mean(179, 147, 124)
dengue_data_ordered$avg[2] <- mean (186, 179, 147)

# MOVING WEIGHTED AVERAGE
for (week in 1:length(dengue_data_ordered$casos)) {
  if (week == 1) {
    dengue_data_ordered$wavg[1] <- ((179 * 3) + (147 * 2) + (124)) / 6
  }
  
  if (week == 2) {
    dengue_data_ordered$wavg[2] <- ((186 * 3) + (179 * 2) + (147)) / 6
  }
  
  if (week == 3) {
    dengue_data_ordered$wavg[3] <- ((201 * 3) + (186 * 2) + (179)) / 6
  }
  
  
  if(week > 3) {
    dengue_data_ordered$wavg[week] <- ((dengue_data_ordered$casos[week - 1] * 3) + (dengue_data_ordered$casos[week - 2] * 2) + (dengue_data_ordered$casos[week - 3])) / 6
    
  }
}

# WEIGHTED AVERAGE - REGULAR AVERAGE
dengue_data_ordered$diffavg =  dengue_data_ordered$wavg -  dengue_data_ordered$avg

# REPRESENT SEASONALITY
dengue_data_ordered$week_of_year <- as.numeric(substr(dengue_data_ordered$SE, 5, 6))
dengue_data_ordered$sin_year <- sin(2 * pi * (dengue_data_ordered$week_of_year / 52))
dengue_data_ordered$cos_year <- cos(2 * pi * (dengue_data_ordered$week_of_year / 52))


# HUMIDITY SEASON INTERACTION
dengue_data_ordered$humidity_sin_interaction <- dengue_data_ordered$umidmed * dengue_data_ordered$sin_year
dengue_data_ordered$humidity_cos_interaction <- dengue_data_ordered$umidmed * dengue_data_ordered$cos_year

# TEMPERATURE SEASON INTERACTION
dengue_data_ordered$temp_sin_interaction <- (dengue_data_ordered$tempmed * dengue_data_ordered$sin_year)
dengue_data_ordered$temp_cos_interaction <- (dengue_data_ordered$tempmed * dengue_data_ordered$cos_year)



## FEATURE ENGENEERING TEST ##


# 1 WEEK LAG
dengue_data_ordered_test$casos_lag1 <- dplyr::lag(dengue_data_ordered_test$casos, 1)
dengue_data_ordered_test$casos_lag1[1] = 163 # Since we're lagging the data in 1 week, the first week becomes NA. This is the cases for the last week of 2022

# 2 WEEK LAG
dengue_data_ordered_test$casos_lag2 <- dplyr::lag(dengue_data_ordered_test$casos, 2)
dengue_data_ordered_test$casos_lag2[1] = 117 # Same logic applies
dengue_data_ordered_test$casos_lag2[2] = 163 # Same logic applies

# 3 WEEK LAG
dengue_data_ordered_test$casos_lag3 <- dplyr::lag(dengue_data_ordered_test$casos, 3)
dengue_data_ordered_test$casos_lag3[1] = 123 # Same logic applies
dengue_data_ordered_test$casos_lag3[2] = 117 # Same logic applies
dengue_data_ordered_test$casos_lag3[3] = 163 # Same logic applies

# 4 WEEK LAG
dengue_data_ordered_test$casos_lag4 <- dplyr::lag(dengue_data_ordered_test$casos, 4)
dengue_data_ordered_test$casos_lag4[1] = 98 # Same logic applies
dengue_data_ordered_test$casos_lag4[2] = 123 # Same logic applies
dengue_data_ordered_test$casos_lag4[3] = 117 # Same logic applies
dengue_data_ordered_test$casos_lag4[4] = 163 # Same logic applies


# MOVING STD DEVIATION
dengue_data_ordered_test$casoslag1_mov_sd <- rollapply(dengue_data_ordered_test$casos_lag1,
                                                       width = window_size,
                                                       FUN = sd,
                                                       fill = NA,
                                                       align = "right")
dengue_data_ordered_test$casoslag1_mov_sd[1] <- sd(c(163, 117, 123))
dengue_data_ordered_test$casoslag1_mov_sd[2] <- sd(c(230, 163, 117))

# MOVING AVERAGE
dengue_data_ordered_test$avg <- rollmean(dengue_data_ordered_test$casos_lag1,
                                         k = 3,
                                         fill = NA,
                                         align = "right")
dengue_data_ordered_test$avg[1] <- mean(163, 117, 123)
dengue_data_ordered_test$avg[2] <- mean (230, 163, 117)

# MOVING WEIGHTED AVERAGE
for (week in 1:length(dengue_data_ordered_test$casos)) {
  if (week == 1) {
    dengue_data_ordered_test$wavg[1] <- ((163 * 3) + (117 * 2) + (123)) / 6
  }
  
  if (week == 2) {
    dengue_data_ordered_test$wavg[2] <- ((230 * 3) + (163 * 2) + (117)) / 6
  }
  
  if (week == 3) {
    dengue_data_ordered_test$wavg[3] <- ((218 * 3) + (230 * 2) + (163)) / 6
  }
  
  if(week > 3) {
    dengue_data_ordered_test$wavg[week] <- ((dengue_data_ordered_test$casos[week - 1] * 3) + (dengue_data_ordered_test$casos[week - 2] * 2) + (dengue_data_ordered_test$casos[week - 3])) / 6
    
  }
}

# WEIGHTED AVERAGE - REGULAR AVERAGE
dengue_data_ordered_test$diffavg = dengue_data_ordered_test$wavg - dengue_data_ordered_test$avg

# REPRESENT SEASONALITY
dengue_data_ordered_test$week_of_year <- as.numeric(substr(dengue_data_ordered_test$SE, 5, 6))
dengue_data_ordered_test$sin_year <- sin(2 * pi * (dengue_data_ordered_test$week_of_year / 52))
dengue_data_ordered_test$cos_year <- cos(2 * pi * (dengue_data_ordered_test$week_of_year / 52))

# HUMIDITY SEASON INTERACTION
dengue_data_ordered_test$humidity_sin_interaction <- climate_data_test_weekly$umid_med * dengue_data_ordered_test$sin_year
dengue_data_ordered_test$humidity_cos_interaction <- climate_data_test_weekly$umid_med * dengue_data_ordered_test$cos_year

# TEMPERATURE SEASON INTERACTION
dengue_data_ordered_test$temp_sin_interaction <- (climate_data_test_weekly$temp_med * dengue_data_ordered_test$sin_year)
dengue_data_ordered_test$temp_cos_interaction <- (climate_data_test_weekly$temp_med * dengue_data_ordered_test$cos_year)



# ORGANAZING DATA IN TRAIN SAMPLES
x_train <- cbind(
  # dengue_data_ordered$tempmin,
  # dengue_data_ordered$tempmed,
  # dengue_data_ordered$tempmax,
  dengue_data_ordered$casos_lag1,
  dengue_data_ordered$casos_lag2,
  dengue_data_ordered$casos_lag3,
  dengue_data_ordered$casos_lag4,
  dengue_data_ordered$sin_year,
  dengue_data_ordered$cos_year,
  # dengue_data_ordered$humidity_sin_interaction,
  # dengue_data_ordered$humidity_cos_interaction,
  dengue_data_ordered$casoslag1_mov_sd,
  dengue_data_ordered$avg
  # dengue_data_ordered$temp_sin_interaction,
  # dengue_data_ordered$temp_cos_interaction
)

x_train_wavg <- cbind(
  # dengue_data_ordered$tempmin,
  # dengue_data_ordered$tempmed,
  # dengue_data_ordered$tempmax,
  dengue_data_ordered$casos_lag1,
  dengue_data_ordered$casos_lag2,
  dengue_data_ordered$casos_lag3,
  dengue_data_ordered$casos_lag4,
  dengue_data_ordered$sin_year,
  dengue_data_ordered$cos_year,
  # dengue_data_ordered$humidity_sin_interaction,
  # dengue_data_ordered$humidity_cos_interaction,
  dengue_data_ordered$casoslag1_mov_sd,
  dengue_data_ordered$wavg
  # dengue_data_ordered$temp_sin_interaction,
  # dengue_data_ordered$temp_cos_interaction
)

# x_train <- apply(x_train, 2, scale)

x_train_df <- as.data.frame(x_train)

colnames(x_train_df) <- c(
  "casos_lag1",
  "casos_lag2",
  "casos_lag3",
  "casos_lag4",
  "sin_year",
  "cos_year",
  # "humidity_sin_interaction",
  # "humidity_cos_interaction",
  "casoslag1_mov_sd",
  "avg"
  # "temp_sin_interaction",
  # "temp_cos_interaction"
  )



y_train <- cbind(
  dengue_data_ordered$casos
)


# USING 2023 DATA FOR VALIDATION
x_val2023 <- cbind(
  # dengue_data_ordered_test$tempmin,
  # dengue_data_ordered_test$tempmed,
  # dengue_data_ordered_test$tempmax,
  dengue_data_ordered_test$casos_lag1,
  dengue_data_ordered_test$casos_lag2,
  dengue_data_ordered_test$casos_lag3,
  dengue_data_ordered_test$casos_lag4,
  dengue_data_ordered_test$sin_year,
  dengue_data_ordered_test$cos_year,
  # dengue_data_ordered_test$humidity_sin_interaction,
  # dengue_data_ordered_test$humidity_cos_interaction,
  dengue_data_ordered_test$casoslag1_mov_sd,
  dengue_data_ordered_test$avg
  # dengue_data_ordered_test$temp_sin_interaction,
  # dengue_data_ordered_test$temp_cos_interaction
)


y_val2023 <- cbind(dengue_data_ordered_test$casos)

# x_val2023 <- apply(x_val2023, 2, scale)

y_val2023_df <- as.data.frame(y_val2023) # only used for reference, not to train or test.

colnames(y_val2023_df) <- c(
  "casos"
)

y_val2023_df$week_number <- 1:52


# FUNCTIONS FOR EVALUATING MODEL AND TRACKING METRICS
metrics <- data.frame(
  burn = numeric(),
  ndpost = numeric(),
  k = numeric(),
  power = numeric(),
  ndtree = numeric(),
  rmse = numeric(),
  r2 = numeric()
)

calculate_rmse <- function(y, mean_pred) {
  mse <- mean((mean_pred - y)^2)
  rmse <- sqrt(mse)
  return(rmse)
}

calculate_r2 <- function(y, mean_pred) {
  ss_total <- sum((y - mean(y))^2)   # Total sum of squares
  ss_residual <- sum((y - mean_pred)^2) # Residual sum of squares
  r_squared <- 1 - (ss_residual / ss_total)
  return(r_squared)
}

update_metrics <- function(burn, ndpost, k, power, ndtree, rmse, last_week){
  new_row <- data.frame(burn = burn, ndpost = ndpost, k = k, power = power, ndtree = ndtree, rmse = rmse, last_week = last_week)
  metrics <<- rbind(metrics, new_row) # Use rbind to add a row
  print(metrics)
}

plot_results <- function(mean_predictions_unseen) {
  residual <- head(y_val2023, weeks_predicted) - mean_predictions_unseen
  # acf(residual, main ="ACF of Residuals")
  #hist(residual, main = "Histogram of Residuals", xlab = "Residuals")
  #qqnorm(residual)
  #qqline(residual)
  # plot(mean_predictions_unseen, residual, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")
  # abline(h = 0, col = "red")
  
  # VISUAL COMPARISON BETWEEN MODEL PREDICTION AND DENGUE CASES
  plot_data <- data.frame(
    x = weeks_label:(weeks_label + 3),
    mean_prediction_unseen = mean_predictions_unseen,
    #residual = residual,
    casos = head(predicted_values, weeks_predicted)
  )
  
  print(plot_data)
  
  # Create the overlapping plot using ggplot2
  ggplot(plot_data, aes(x = x)) +
    geom_line(aes(y = mean_predictions_unseen, color = "Prediction")) +  # Line for mean_train
    geom_point(aes(y = casos, color = "Dengue Cases")) +      # Line for dengue cases
    labs(title = paste("RMSE: ", new_rmse, "Weeks: ", weeks_label),
         x = "Week Number",  # You can customize the x-axis label
         y = "Values",      # Customize the y-axis label
         color = "Data") +   # Customize the legend title
    scale_color_manual(values = c("Prediction" = "blue", "Dengue Cases" = "red")) +  # Set line colors
    theme_bw()
  
  
  ggplot(plot_data, aes(x = x)) +
    geom_ribbon(aes(y = mean_predictions_unseen,
                    ymin = li_predictions_2023,
                    ymax = ui_predictions_2023),
                show.legend = F, alpha = 0.2) +  # Line for mean_train
    geom_line(aes(y = mean_predictions_unseen, color = "Prediction")) +  # Line for mean_train
    geom_point(aes(y = casos, color = "Dengue Cases")) +      # Line for dengue cases
    labs(title = paste("RMSE: ", new_rmse, "Weeks: ", weeks_label),
         x = "Week Number",  # You can customize the x-axis label
         y = "Values",      # Customize the y-axis label
         color = "Data") +   # Customize the legend title
    scale_color_manual(values = c("Prediction" = "blue", "Dengue Cases" = "red")) +  # Set line colors
    theme_bw()
  
  
  
}

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
    "sin_year",
    "cos_year",
    # "humid_sin_interaction",
    # "humid_cos_interaction",
    "casoslag1_mov_sd",
    "wavg"
    # "temp_sin_interaction",
    # "temp_cos_interaction"
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

# TRAINING

set.seed(7)
burn <- 13000
nd <- 5000
k_value <- 2
power_value <- 2
ntree_value <- 150L
post <- wbart(x_train, y_train, nskip = burn, ndpost = nd, k=k_value, power = power_value, ntree = ntree_value )
post_wavg <- wbart(x_train_wavg, y_train, nskip = burn, ndpost = nd, k=k_value, power = power_value, ntree = ntree_value )


# plot(post$sigma, ylab = "post$sigma", type = "l")
# abline(v = burn, lwd = 2, col = "red")

prediction <- predict(post, newdata = x_val2023)
mean_predictions_2023 <- apply(prediction, 2, mean)
# 
new_rmse <- calculate_rmse(y_val2023, mean_predictions_2023)
# new_r2 <- calculate_r2(y_val2023, mean_predictions_2023)
# update_metrics(burn, nd, k_value, power_value, ntree_value, new_rmse, new_r2)




# TESTING

week_base = 7

for (last_week in week_base:week_base) {
  
  weeks_predicted = 4
  mean_predictions_2023 <- numeric(weeks_predicted) # Initialize vector to store predictions
  li_predictions_2023 <- numeric(weeks_predicted)
  ui_predictions_2023 <- numeric(weeks_predicted)
  
  # 4 last cases from 2022
  last_cases_2022 <- tail(dengue_data_ordered$casos, 4)
  
  predicted_values = rbind(
    y_val2023_df$casos[last_week + 1],
    y_val2023_df$casos[last_week + 2],
    y_val2023_df$casos[last_week + 3],
    y_val2023_df$casos[last_week + 4]
  )
  
  v1 <- y_val2023_df$casos[last_week]
  v2 <- y_val2023_df$casos[last_week - 1]
  v3 <- y_val2023_df$casos[last_week - 2]
  v4 <- y_val2023_df$casos[last_week - 3]
  
  weekly_averages <- climate_data_weekly %>%
    group_by(week_of_year) %>%
    summarize(
      avg_humidity = mean(umid_med, na.rm = TRUE),
      avg_temperature = mean(temp_med, na.rm = TRUE), 
    )
  weekly_averages$sin_week = sin(2 * pi * (weekly_averages$week_of_year / 52))
  weekly_averages$cos_week = cos(2 * pi * (weekly_averages$week_of_year / 52))
  
  weekly_averages$avg_humid_sin_interaction = weekly_averages$avg_humidity * weekly_averages$sin_week
  weekly_averages$avg_humid_cos_interaction = weekly_averages$avg_humidity * weekly_averages$cos_week
  
  weekly_averages$avg_temp_sin_interaction = weekly_averages$avg_temperature * weekly_averages$sin_week
  weekly_averages$avg_temp_cos_interaction = weekly_averages$avg_temperature * weekly_averages$cos_week
  
  for (week in 1:weeks_predicted) {
    
    if(week==1){
      current_lag1 <- v1 # using the last value of the train dataset as the first lag1 value
      current_lag2 <- v2
      current_lag3 <- v3
      current_lag4 <- v4
      mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
      wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
      avg <- (current_lag1) + (current_lag2) + (current_lag3) / 3
      week_number <- y_val2023_df$week_number[last_week + week]
    } else if (week == 2) {
      current_lag1 <- mean_predictions_2023[1]
      current_lag2 <- v1
      current_lag3 <- v2
      current_lag4 <- v3
      mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
      wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
      week_number <- y_val2023_df$week_number[last_week + week]
      avg <- (current_lag1) + (current_lag2) + (current_lag3) / 3
    } else if (week == 3) {
      current_lag1 <- mean_predictions_2023[2]
      current_lag2 <- mean_predictions_2023[1]
      current_lag3 <- v1
      current_lag4 <- v2
      mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
      wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
      week_number <- y_val2023_df$week_number[last_week + week]
      avg <- (current_lag1) + (current_lag2) + (current_lag3) / 3
    } else if (week == 4) {
      current_lag1 <- mean_predictions_2023[3]
      current_lag2 <- mean_predictions_2023[2]
      current_lag3 <- mean_predictions_2023[1]
      current_lag4 <- v1
      mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
      wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
      week_number <- y_val2023_df$week_number[last_week + week]
      avg <- (current_lag1) + (current_lag2) + (current_lag3) / 3
    } else {
      current_lag1 <- mean_predictions_2023[4]
      current_lag2 <- mean_predictions_2023[3]
      current_lag3 <- mean_predictions_2023[2]
      current_lag4 <- mean_predictions_2023[1]
      mov_sd <- sd(c(current_lag1, current_lag2, current_lag3))
      wavg <- ((current_lag1 * 3) + (current_lag2 * 2) + (current_lag3)) / 6
      week_number <- y_val2023_df$week_number[last_week + week]
      avg <- (current_lag1) + (current_lag2) + (current_lag3) / 3
    }
    
    recursive_data <- data.frame(
      casos_lag1 = current_lag1,
      casos_lag2 = current_lag2,
      casos_lag3 = current_lag3,
      casos_lag4 = current_lag4,
      sin_year = sin(2 * pi * week_number/52),
      cos_year = cos(2 * pi * week_number/52),
      # humidity_sin_interaction = weekly_averages$avg_humid_sin_interaction[week_base + 1],
      # humidity_cos_interaction = weekly_averages$avg_humid_cos_interaction[week_base + 1],
      casoslag1_mov_sd = mov_sd,
      avg
      # temp_sin_interaction = weekly_averages$avg_temp_sin_interaction[week_base + 1],
      # temp_cos_interaction = weekly_averages$avg_temp_cos_interaction[week_base + 1]
      
    )

    
    # test_data <- as.data.frame(recursive_data)
    # test_data$casos <- y_val2023[week]
    # lm_predictions <- predict(lm_model, newdata = test_data)
    
    
    prediction <- predict(post, newdata = recursive_data)
    mean_prediction_unseen <- apply(prediction, 2, mean)
    li_prediction_unseen <- apply(prediction, 2, quantile, probs = 0.025)
    ui_prediction_unseen <- apply(prediction, 2, quantile, probs = 0.975)
    
    
    
    
    mean_predictions_2023[week] <- mean_prediction_unseen
    li_predictions_2023[week] <- li_prediction_unseen
    ui_predictions_2023[week] <- ui_prediction_unseen
  }
  
  
  
  new_rmse <- calculate_rmse(predicted_values, mean_predictions_2023)
  print(new_rmse)
  update_metrics(burn, nd, k_value, power_value, ntree_value, new_rmse, last_week)
}

mean_predictions_unseen <- head(mean_predictions_2023, weeks_predicted)
weeks_label = week_base + 1 


plot_results(mean_predictions_unseen)

plot_var_imp()

