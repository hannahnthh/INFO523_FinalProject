# Load necessary libraries
#install.packages("stats")
library(dplyr)
library(forecast)
library(ggplot2)
library(tseries)
library(astsa)
library(tidyr)

# Load historical data
his_data <- read.csv("his_data.csv")

# Split data to get training data (prior to or equal to Year 2010) and test data (after 2010) for Program 18 only
his_train <- his_data %>% 
  filter(Year <= 2010 & Program == "Program 18") %>%
  select(Year, Enrolled)

his_test <- his_data %>%
  filter(Year > 2010 & Program == "Program 18") %>%
  select(Year, Enrolled)

# Create a time series plot
ggplot(his_train, aes(x = Year, y = Enrolled)) +
  geom_line() +
  labs(title = "Historical Enrollment Data for Program 18",
       x = "Year", y = "Enrollment")

# Plot the ACF
acf(his_train$Enrolled)



# Check for stationarity using ADF test. This data is stationary.
adf_test_enrolled <- adf.test(his_train$Enrolled)
print(adf_test_enrolled)

# Generate diagnostic plots, including the ACF and PACF of residuals, histogram of residuals, and a Q-Q plot.
model_diagnostics <- forecast:::tsdisplay(residuals(model), lag.max = 20, main = "Model Diagnostics")

# Display the diagnostic plots
print(model_diagnostics)
# Create ARIMA models for enrollment
model <- auto.arima(his_train$Enrolled)
summary(model)
# Test models built above for year 2011 and above using ARIMA models

predicted_values1 <- forecast(model, h=12)
print(predicted_values1)
print(predicted_values1$mean)

# Plot predictions and real enrollment for 2011-2022 for Program 18 to see if predictive model is reliable
plot(his_test$Year, his_test$Enrolled, 
     type = "n", ylim = range(c(0, max(predicted_values1$mean, his_test$Enrolled))),

     main = "ARIMA Model Predictions",
     xlab = "Year", ylab = "Enrollment")

lines(his_test$Year, his_test$Enrolled, col = "blue")
lines(his_test$Year, predicted_values1$mean, col = "red")

legend("bottomright", c("Actual", "Predicted"), lty = c(1, 1), col = c("blue", "red"))

# Calculate prediction accuracy
percentdiff <- mean(abs(predicted_values1$mean - his_test$Enrolled) / his_test$Enrolled * 100)
print(paste("Average Percent Difference Between Prediction vs. Actual:", round(percentdiff, 1), "%"))

# Forecast value for 5 years ( 2023-2027)
predicted_values2 <- forecast(model, h=17)
#get predictions from last 5 results for 2023-2027
predictions_next5years <- predicted_values2$mean[(length(predicted_values2$mean) - 4):length(predicted_values2$mean)]
print(predictions_next5years)