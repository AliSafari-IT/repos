# R program to illustrate 
# Time Series Analysis 
# Using ARIMA model in R 

library(rstudioapi)
library(xlsx)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# Install the library for forecast() 
install.packages("forecast") 

# library required for forecasting 
library(forecast) 


Data_daily = read.csv("EAN541448860004121675_daily.csv")  # read csv file 
Data_daily
print(Data_daily) 
myDailySeries = Data_daily[, "Consumption_Type_A"]
print(myDailySeries)

# output to be created as png file 
png(file = "TimeSeries_Data_daily.png") 

# Plotting graph without forecasting 
plot(Data_daily[, "Consumption_Type_A"], 
     main = "Graph without forecasting", 
     col.main = "darkgreen") 

# Saving the file 
dev.off() 

# Output to be created as png file 
png(file = "TimeSeriesARIMA_Data_daily.png") 

# Fitting model using arima model 
fit <- auto.arima(Data_daily[, "Consumption_Type_A"]) 
print(fit) 


# Next 50 forecasted values 
forecastedValues <- forecast(fit, 7) 


# Print forecasted values 
print(forecastedValues) 

plot(forecastedValues, main = "Graph with forecasting", 
     col.main = "darkgreen") 

# saving the file 
dev.off() 

#export the Arima results
sink("ARIMA_result_Daily.txt") 
arima(myDailySeries, order = c(5, 1, 1), method = "ML") 
sink()
