# R program to illustrate 
# Time Series Analysis 
# Using ARIMA model in R 

library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# Install the library for forecast() 
install.packages("forecast") 

# library required for forecasting 
library(forecast) 


Data_Q1 = read.csv("Data_Q1_A")  # read csv file 
Data_Q1
Data_Q1[, "Data_Q1"]
# output to be created as png file 
png(file = "TimeSeries_DataQ1A.png") 

# Plotting graph without forecasting 
plot(Data_Q1[, "Data_Q1"], 
     main = "Graph without forecasting", 
     col.main = "darkgreen") 

# Saving the file 
dev.off() 

# Output to be created as png file 
png(file = "TimeSeriesARIMA_DataQ1A.png") 

# Fitting model using arima model 
fit <- auto.arima(Data_Q1[, "Data_Q1"]) 
fit
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals') 
# Next 1 forecasted value 
forecastedValues <- forecast(fit, 1) 
forecastedValues
# Print forecasted values 
print(forecastedValues) 

plot(forecastedValues, main = "Graph with forecasting", 
     col.main = "darkgreen") 



# saving the file 
dev.off() 



