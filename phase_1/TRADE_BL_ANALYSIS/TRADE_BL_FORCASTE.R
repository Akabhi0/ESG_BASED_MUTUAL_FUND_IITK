#forecasting the data for FDI 
# 1-visualise the time series
# 2-stationarize the series
# 3-Plot ACF/PACF chart find optimal parameter
# 4-Build the ARIMA model
# 5-Make predictions
library(forecast)
library(tseries)
library(readxl)

#Import Sales  Dataset
dataExcel <- read_excel('/Users/akabhi/Desktop/IIT Kanpur/GDP_ANALYSIS_DATA.xlsx')

#Convert sales_k column to Time Series object - 1971 to 1984
TB_ts <- ts(dataExcel$`TRADE BL ( Billions of US $)`, start=c(2000), frequency=1)

#Plot Sales Time series using autoplot (forecast library)
autoplot(TB_ts)

#Stationarity : A stationary process has a mean and variance 
#               that do not change overtime and the process does not have trend.

#Perform ADF Test
#Null Hypothesis - Non Stationary 
#               (Do NOT Reject if P value > sig lvl (1%, 5%, 10%) / 0.05)
adfTest(TB_ts, lag= 1)

#Null Hypothesis not Rejected - Series is Non Stationary

#We will use first order difference to make it Stationary
TB_ts_d1 <- diff(TB_ts, differences = 1)
adfTest(TB_ts_d1, lag= 1)

#Check plot
autoplot(TB_ts_d1)

#Since P is very small and less than sig lvl - 
#                 we accept alternate hypothesis
#ARIMA (p,d,q)

#Run PACF test to select AR term or the p term - correlation between lags
Pacf(FDI_ts_d1) #choosing 1

#Run ACF test to select MA term or the q term - 
Acf(TB_ts_d1) #choosing 1

#BASIC ARIMA - does not work that good
tsMod <- Arima(y = TB_ts,order = c(1,1,1))

#Summary of the model
tsMod

forecast(tsMod,h=5)
#Plot Sales with forecast 
autoplot(forecast(tsMod,h=12))

