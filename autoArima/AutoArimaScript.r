# R program to illustrate 
# Time Series Analysis 
# Using ARIMA model in R 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install the library for forecast() 
install.packages("forecast") 

# library required for forecasting 
library(forecast) 

# Output to be created as png file 
png(file = "TimeSeries2GFG.png") 

# Plotting graph without forecasting 
plot(EuStockMarkets[, "DAX"], 
     main = "Graph without forecasting", 
     col.main = "darkgreen") 

# Saving the file 
dev.off() 

# Output to be created as png file 
png(file = "TimeSeriesARIMA2GFG.png") 

# Fitting model using arima model 
fit <- auto.arima(EuStockMarkets[, "DAX"]) 

# Next 50 forecasted values 
forecastedValues <- forecast(fit, 50) 

# Print forecasted values 
print(forecastedValues) 

plot(forecastedValues, main = "Graph with forecasting", 
     col.main = "darkgreen") 

# saving the file 
dev.off() 
