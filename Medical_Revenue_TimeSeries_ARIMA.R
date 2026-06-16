################################################################################
# Project:      Medical Revenue Time Series ARIMA Forecasting
# Objective:    Forecast revenue using ARIMA modeling and time series analysis
# Author:       Kim Shellenberger
# Date:         2024
# Description:  Complete time series analysis pipeline including data loading,
#               stationarity testing, model identification, decomposition,
#               and ACF/PACF analysis for revenue forecasting.
################################################################################

# SECTION 1: LOAD REQUIRED PACKAGES
library(tidyverse)   # Data manipulation
library(ggplot2)     # Data visualization
library(tseries)     # Time series analysis (ADF test)
library(seasonal)    # Seasonal decomposition
library(forecast)    # ARIMA and forecasting functions
library(rmarkdown)   # Report generation
library(knitr)       # Markdown utilities

# SECTION 2: LOAD AND INSPECT DATA
# TODO: Update file path to your data location
data <- read.csv("medical_time_series.csv")

head(data)
str(data)

# SECTION 3: TIME SERIES VISUALIZATION
# Plot original time series to identify trends and patterns
ggplot(data, aes(x = Day, y = Revenue)) +
  geom_line(color = "steelblue", size = 0.7) +
  labs(title = "Medical Service Revenue Over Time",
       x = "Day",
       y = "Revenue ($)",
       subtitle = "Raw revenue time series") +
  theme_minimal()

# SECTION 4: DATA QUALITY CHECKS
# Check for gaps in the time series (missing days)
print(paste("Any gaps in measurement:", any(diff(data$Day) != 1)))

# Calculate sequence length for train-test split
length_sequence <- nrow(data)
print(paste("Total observations:", length_sequence))

# SECTION 5: STATIONARITY TESTING
# Augmented Dickey-Fuller (ADF) test on original series
cat("\nADF Test on Original Revenue Series:\n")
adf_original <- adf.test(data$Revenue)
print(adf_original)
# Interpretation: p-value < 0.05 suggests stationarity (no unit root)

# SECTION 6: DIFFERENCING FOR STATIONARITY
# Apply first-order differencing to remove trend
data$diff_revenue <- c(NA, diff(data$Revenue))

# Test differenced series for stationarity
cat("\nADF Test on First-Differenced Revenue Series:\n")
adf_differenced <- adf.test(data$diff_revenue[-1], alternative = "stationary")
print(adf_differenced)

# SECTION 7: TRAIN-TEST SPLIT
# Allocate 80% for training, 20% for testing (typical split)
length_sequence <- nrow(data)
train_size <- round(0.8 * length_sequence)
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):length_sequence, ]

print(paste("Training set size:", nrow(train_data)))
print(paste("Test set size:", nrow(test_data)))

# SECTION 8: EXPORT TRAIN-TEST DATA
# TODO: Update file paths to your output locations
write.csv(train_data, file = "train_data.csv", row.names = FALSE)
write.csv(test_data, file = "test_data.csv", row.names = FALSE)
print("Train and test data exported.")

# SECTION 9: TIME SERIES DECOMPOSITION
# Decompose original revenue series into components (trend, seasonal, random)
decomposed_ts <- decompose(ts(data$Revenue, frequency = 2), type = "multiplicative")
plot(decomposed_ts, main = "Time Series Decomposition")
print("Decomposition complete: Trend | Seasonal | Random components identified")

# SECTION 10: AUTOCORRELATION ANALYSIS
# ACF and PACF plots inform ARIMA parameter selection
cat("\nAutocorrelation Function (ACF) Analysis:\n")
acf(data$Revenue, main = "ACF - Original Revenue", lag.max = 20)

# SECTION 11: SPECTRAL ANALYSIS
# Examine frequency domain characteristics
spec.pgram(data$Revenue, main = "Spectral Density - Revenue")

# Identify ARIMA model
arima_model <- auto.arima(data$Revenue)
arima_model 

# Perform forecast
forecast_result <- forecast(arima_model, h = 120)
forecast_result

# Ensure same length of test data and forecasted values
test_data <- test_data[1:min(length(test_data$Revenue), length(forecast_result$mean)), ]
forecast_values <- forecast_result$mean[1:min(length(test_data$Revenue), length(forecast_result$mean))]

# Remove NA values from test data and forecasted values
test_data <- test_data[!is.na(test_data$Revenue), ]
forecast_values <- forecast_values[!is.na(test_data$Revenue)]

# Calculate RMSE
rmse <- sqrt(mean((test_data$Revenue - forecast_values)^2))

#residual plot
residplot(forecast_result)

# Print RMSE
print(paste("RMSE:", round(rmse, 4)))

# Plot forecast with training, test data, and 95% confidence interval
plot(forecast_result, main = "Forecast with 95% Confidence Interval")
lines(train_data$Day, train_data$Revenue, col = "blue")  # Train data
lines(test_data$Day, test_data$Revenue, col = "red")    # Test data
lines(forecast_result$mean, col = "black")              # Forecast
lines(forecast_result$lower[,2], col = "green", lty = 2) # 95% confidence interval lower bound
lines(forecast_result$upper[,2], col = "green", lty = 2) # 95% confidence interval upper bound
legend("topleft", legend = c("Train Data", "Test Data", "Forecast", "95% Confidence Interval"), 
       col = c("blue", "red", "black", "green"), lty = c(1, 1, 1, 2))
