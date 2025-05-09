# Dengue Forecasting Model

A Bayesian Additive Regression Trees (BART) model for predicting dengue case numbers in Rio de Janeiro, Brazil.

## Overview

This project implements a machine learning model to forecast dengue fever cases using climate data and historical case records. The model leverages the BART algorithm to capture complex non-linear relationships between environmental factors and disease spread.

## Data Sources

The model uses two primary data sources via the MosqLimate API:

1. **Climate Data (2019-2023)**: Weekly temperature and humidity metrics
2. **Dengue Case Data (2019-2023)**: Weekly reported dengue cases

## Methodology

### Feature Engineering

- **Lagged Case Features**: 1-4 week lags of previous case counts
- **Climate Lags**: 1-4 week lags of temperature and humidity
- **Statistical Features**: Moving averages, weighted averages, and standard deviations
- **Seasonality**: Sine and cosine transformations of week numbers

### Model Architecture

The implementation uses Bayesian Additive Regression Trees (BART), a non-parametric Bayesian regression approach:

- **Algorithm**: BART via the `wbart()` function from the BART package
- **Hyperparameters**:
  - Burn-in period: 13,000 iterations (model warmup)
  - Posterior draws: 5,000 (samples for inference)
  - k value: 2 (prior parameter controlling tree node depth)
  - Power value: 2 (prior parameter for tree structure)
  - Number of trees: 150 (ensemble size)
  
These fixed hyperparameters were selected without documented optimization, which represents an opportunity for improvement through cross-validation or Bayesian optimization techniques.

## Visualization

The project includes a function to create animated visualizations showing:
- Actual dengue cases (black line)
- Predicted cases with confidence intervals (red line/band)
- Evolving predictions as the base week changes

## Requirements

- R 4.x
- Libraries:
  - BART
  - MASS
  - httr
  - jsonlite
  - mlr3
  - ggplot2
  - dplyr
  - zoo
  - forecast
  - glue
  - gganimate
  - gifski
  - transformr

## Code Structure

The implementation follows this logical organization:

1. **Data Acquisition**:
   - API connection to MosqLimate service
   - Separate calls for climate and dengue case data
   - Pagination handling for large result sets

2. **Feature Engineering**:
   - Time-based lag creation (cases, temperature, humidity)
   - Statistical derivatives (moving averages, standard deviations)
   - Seasonal transformations (sine/cosine of week number)
   - Missing value imputation using weekly averages

3. **Model Training**:
   - Train/test split (pre-2023 vs. 2023)
   - Data preparation and normalization
   - BART model training with specified hyperparameters

4. **Forecasting Functions**:
   - `generate_predictions_across_year()`: Iterates through base weeks
   - Recursive prediction logic with horizon-specific feature handling
   - Confidence interval calculation from posterior samples

5. **Visualization**:
   - `create_prediction_animation_year()`: Generates animated forecasts
   - ggplot2/gganimate rendering of predictions vs. actuals

## Usage

1. **Environment Setup**:
   ```R
   # Install required packages
   install.packages(c("BART", "MASS", "httr", "jsonlite", "mlr3", 
                     "ggplot2", "dplyr", "zoo", "forecast", "glue", 
                     "gganimate", "gifski", "transformr"))
   ```

2. **API Configuration**:
   - Replace API key with your MosqLimate credentials
   - Update the geocode parameter (3304557 = Rio de Janeiro)

3. **Execution**:
   - Run the full script or execute sections sequentially
   - Adjust prediction parameters in `generate_predictions_across_year()` call
   - Output visualization formats can be modified (gif/mp4)

