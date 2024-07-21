library(tidyverse)
library(ggplot2)
library(tseries)
library(seasonal)
library(forecast)
library(rmarkdown)
library(knitr)
            
# Load and inspect the data
data <- read.csv('/Users/kimshellenberger/Desktop/medical_time_series .csv')
head(data)
str(data)

# 1. Line graph visualization
ggplot(data, aes(x = Day, y = Revenue)) +
  geom_line() +
  labs(title = "Time Series Visualization",
       x = "Day",
       y = "Revenue")


# Check for any gaps in measurement
print(paste("Any gaps in measurement:", any(diff(data$Day) != time_step)))

# Length of the sequence
length_sequence <- nrow(data)


# 3. Evaluate stationarity
adf.test(data$Revenue)

# Take first-order difference to remove trend
data$diff_revenue <- c(NA, diff(data$Revenue))

# Evaluate stationarity
adf_result <- adf.test(data$diff_revenue[-1], alternative = "stationary")
print(adf_result)

# Data preparation and train-test split
length_sequence <- nrow(data)
train_size <- round(0.8 * length_sequence)
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):length_sequence, ]

# Part IV: Model Identification and Analysis
# Write train data to CSV
write.csv(train_data, file = "/Users/kimshellenberger/Desktop/train_data.csv", row.names = FALSE)

# Write test data to CSV
write.csv(test_data, file = "/Users/kimshellenberger/Desktop/test_data.csv", row.names = FALSE)

# Decompose the time series using the original Revenue series
decomposed_ts <- decompose(ts(data$Revenue, frequency = 2), "multiplicative")

# Plot the decomposed components
plot(decomposed_ts)

# Check autocorrelation function
acf(data$Revenue)
# Plot autocorrelation function
acf(data$Revenue, main = "Autocorrelation Function", lag.max = 20)

# Check spectral density
spec.pgram(data$Revenue)

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
