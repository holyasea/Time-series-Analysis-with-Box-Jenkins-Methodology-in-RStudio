# Time-series-Analysis-with-Box-Jenkins-Methodology-in-R
#Load the libraries
library(readxl)
library(tseries)
library(forecast)
library(ggplot2)
library(e1071)
frame()

#Create a time series objects
#Exports, January 1997 to June 2017
Exports <- read_excel("/home/holyweek/R/Exports.xlsx")

#Imports, July 1998 to June 2017
Imports <- read_excel("/home/holyweek/R/Imports.xlsx")

#Creating a time series object
exportSeries <- ts(Exports, start = c(1997, 1), end = c(2017,6), frequency = 12)
importSeries <- ts(Imports, start = c(1998, 7), end = c(2017,6), frequency = 12)

#Creating a time series plot
plot.ts(exportSeries, main = "Time Series Plot of Exports", 
        xlab = "Years", ylab = "Value of Exports in Million USD", col = "Blue",
        panel.first = grid())
plot.ts(importSeries, main = "Time Series Plot of Imports", 
        xlab = "Years", ylab = "Value of Imports in Million USD", col = "red", 
        panel.first = grid())

#Decomposing the series, Additive or Multiplicative
decExports <- decompose(exportSeries, type = "multiplicative")
decImports <- decompose(importSeries, type = "multiplicative")

#Plot of the decomposed Exports
plot(decExports$seasonal, main = "Plot of the Seasonal Element, Exports", 
     xlab = "Years", ylab = "Seasonal element", col = "green", 
     panel.first = grid())
plot(decExports$trend, main = "Plot of the Trend Element, Exports", 
     xlab = "Years", ylab = "Trend element", col = "green", 
     panel.first = grid())
plot(decExports$random, main = "Plot of the Random Element, Exports", 
     xlab = "Years", ylab = "Random or Irregular element", col = "green", 
     panel.first = grid())

#Plot of the decomposed Imports
plot(decImports$seasonal, main = "Plot of the Seasonal Element, Imports", 
     xlab = "Years", ylab = "Seasonal element", col = "orange", 
     panel.first = grid())
plot(decImports$trend, main = "Plot of the Trend Element, Imports", 
     xlab = "Years", ylab = "Trend element", col = "orange", 
     panel.first = grid())
plot(decImports$random, main = "Plot of the Random Element, Imports", 
     xlab = "Years", ylab = "Random or Irregular element", col = "orange", 
     panel.first = grid())

#Summary statistics for Imports
summary(importSeries)
kurtosis(importSeries)
skewness(importSeries)

#Summary statistics for Exports
summary(exportSeries)
kurtosis(exportSeries)
skewness(exportSeries)

#Test for stationarity
adf.test(importSeries, alternative = "stationary")
adf.test(exportSeries, alternative = "stationary")

#Create  differenced Series
d.importSeries <- diff(importSeries)
d.exportSeries <- diff(exportSeries)

#Plot the differneced series
ggplot2::autoplot(d.exportSeries, col = "red", 
                  main = "Time Series plot of Differenced Exports")
ggplot2::autoplot(d.importSeries, col = "blue", 
                  main = "Time Series plot of Differenced Imports")

#Test for stationarity for the differnced variable
adf.test(d.importSeries, alternative = "stationary")
adf.test(d.exportSeries, alternative = "stationary")

#Summary statistics for the differenced series
summary(d.importSeries)
summary(d.exportSeries)

#ACF for original series
ggAcf(exportSeries)
ggAcf(importSeries)

#ACF for differenced series
ggAcf(d.exportSeries)
ggAcf(d.importSeries)

#PACF for original series
ggPacf(exportSeries)
ggPacf(importSeries)

#PACF for differenced series
ggPacf(d.exportSeries)
ggPacf(d.importSeries)

#ARIMA and SARIMA MODEL
#Forecasting Exports
exportsPredSARIMA <- forecast((auto.arima(exportSeries, max.p = 5, 
                              max.q = 4, max.d = 1, max.P = 5, max.Q = 4, 
                              max.D = 4, seasonal = TRUE)), h = 48, level = c(85, 95))
exportsPredARIMA <- forecast((auto.arima(exportSeries, max.p = 5, 
                              max.q = 4, max.d = 1, max.P = 5, max.Q = 4, 
                              max.D = 4, seasonal = FALSE)), h = 48, level = c(85, 95))

#Forecasting Imports
importsPredSARIMA <- forecast((auto.arima(importSeries, max.p = 5, 
                              max.q = 4, max.d = 1, max.P = 5, max.Q = 4, 
                              max.D = 4, seasonal = TRUE)), h = 48, level = c(85, 95))
importsPredARIMA <- forecast((auto.arima(importSeries, max.p = 5, 
                              max.q = 4, max.d = 1, max.P = 5, max.Q = 4, 
                              max.D = 4, seasonal = FALSE)), h = 48, level = c(85, 95))

#Plotting Residuals of ARIMA for Imports 
qqnorm(importsPredARIMA$residuals, col = "Blue", 
       main = "Quantile- Quantile plot for residuals of ARIMA, Imports")
qqline(importsPredARIMA$residuals)

#Plotting Residuals of ARIMA for Exports 
qqnorm(exportsPredARIMA$residuals, col = "Blue", 
       main = "Quantile- Quantile plot for residuals of ARIMA, Exports")
qqline(exportsPredARIMA$residuals)

#Plotting Residuals of SARIMA for Imports 
qqnorm(importsPredSARIMA$residuals, col = "Blue", 
       main = "Quantile- Quantile plot for residuals of SARIMA, Imports")
qqline(importsPredSARIMA$residuals)

#Plotting Residuals of SARIMA for Exports 
qqnorm(exportsPredSARIMA$residuals, col = "Blue", 
       main = "Quantile- Quantile plot for residuals of SARIMA, Exports")
qqline(exportsPredSARIMA$residuals)

#Exponetial Smoothing using, Holt-Winter Approach for Exports
exportsHOLTadd <- HoltWinters(exportSeries, seasonal = "additive")
exportsHOLTmult <- HoltWinters(exportSeries, seasonal = "multiplicative")

#Exponetial Smoothing using, Holt-Winter Approach for Imports
importsHOLTadd <- HoltWinters(importSeries, seasonal = "additive")
importsHOLTmult <- HoltWinters(importSeries, seasonal = "multiplicative")

#Prediction with Exports using Holt-Winters Approach
exportsPredHOLTadd <- hw(exportSeries, h  = 48, seasonal = "additive", level = c(85, 95))
exportsPredHOLTmult <- hw(exportSeries, h  = 48, seasonal = "multiplicative", level = c(85, 95))

#Plotting Residuals of HOLT-Winters for Imports 
qqnorm(importsPredHOLTmult$residuals, col = "Blue", 
       main = "Quantile- Quantile plot for residuals of Holt-Winters Approach, Imports")
qqline(importsPredHOLTmult$residuals)

#Plotting Residuals of HOLT-Winters for Exports 
qqnorm(exportsPredHOLTmult$residuals, col = "Blue", 
       main = "Quantile- Quantile plot for residuals of Holt-Winters Approach, Exports")
qqline(exportsPredHOLTmult$residuals)

#Prediction with Imports using Holt Winters Approach
importsPredHOLTadd <- hw(importSeries, h  = 48, seasonal = "additive", level = c(85, 95))
importsPredHOLTmult <- hw(importSeries, h  = 48, seasonal = "multiplicative", level = c(85, 95))


#Testing for better fit for the models, Exports Series
#using Mariano test, Ho: is that method 1 and method 2 have same levels of accuracy.
dm.test(exportsPredSARIMA$residuals, exportsPredARIMA$residuals, h = 48)
dm.test(exportsPredHOLTmult$residuals, exportsPredSARIMA$residuals, h = 48)
dm.test(exportsPredHOLTmult$residuals, exportsPredARIMA$residuals, h = 48)
dm.test(exportsPredHOLTadd$residuals, exportsPredmult$residuals, h = 48)

#Using ME, RMSE, MAE, MPE, MAPE and MASE
accuracy(exportsPredARIMA)
accuracy(exportsPredSARIMA)
accuracy(exportsPredHOLTmult)

#Testing for better fit for the models
#using Mariano test, Ho: is that method 1 and method 2 have same levels of accuracy.
dm.test(importsPredSARIMA$residuals, importsPredARIMA$residuals, h = 48)
dm.test(importsPredHOLTmult$residuals, importsPredSARIMA$residuals, h = 48)
dm.test(importsPredHOLTmult$residuals, importsPredARIMA$residuals, h = 48)
dm.test(importsPredHOLTadd$residuals, exportsPredmult$residuals, h = 48)

#Using ME, RMSE, MAE, MPE, MAPE and MASE
accuracy(importsPredARIMA)
accuracy(importsPredSARIMA)
accuracy(importsPredHOLTmult)
accuracy(importsPredHOLTadd)

#Actual predicted values
print(exportsPredmult) 
print(importsPredSARIMA)

#Plot of the predicted values, SARIMA
ggplot2::autoplot(exportSeries) + autolayer(exportsPredHOLTmult)
ggplot2::autoplot(importSeries) + autolayer(importsPredSARIMA)
#End
