# R program to illustrate 
# Time Series Analysis 
# Using ARIMA model in R 

library(rstudioapi)
library(xlsx)
library(fUnitRoots) # for analyzing trends and unit roots in time series
library(forecast) # library required for forecasting 
library(lmtest)
library(FitAR)   #Subset AR Model Fitting

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# Install the library for forecast() 
install.packages("forecast") 





Data_daily = read.csv("EAN541448860004121675_daily.csv")  # read csv file 


myDailySeries = Data_daily[, "Consumption_Type_A"]

tsData = myDailySeries
tsData

urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(Data_daily, differences=1)
plot(tsstationary)
#Normally the autocorrelation continues to decrease as the lag increases, confirming that there is no linear association between observations separated by larger lags.
acf(Data_daily,lag.max=34) #to determine the number of first differences required to make the time series non-seasonal


timeseriesseasonallyadjusted <- tsData- timeseriescomponents$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)


acf(tsstationary, lag.max=100)
pacf(tsstationary, lag.max=100)

fitARIMA <- arima(tsData, order=c(4,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")

coeftest(fitARIMA)#the fitted coefficients and the standard error (s.e.) for each coefficient.
confint(fitARIMA) #Observing the coefficients we can exclude the insignificant ones. We can use a function this confint() for this purpose.

#Box-Ljung test
acf(fitARIMA$residuals)

boxresult-LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)


auto.arima(tsData, trace=TRUE)  #Auto Arima keeps an iteration until no lower AIC can be found.


#The parameters of that ARIMA model can be used as a predictive model for making forecasts for future values of the time series once the best-suited model is selected for time series data.
predict(fitARIMA,n.ahead = 5)
futurVal <- forecast.Arima(fitARIMA,h=10, level=c(99.5))
plot.forecast(futurVal)



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
