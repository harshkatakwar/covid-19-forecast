#MA605 
#Assignment 1
#forecasting covid-19 cases of Belgium for 10 days

#libraries
library(fpp2)
library(readr)
library(ggplot2)
library(forecast)
library(TTR)
library(dplyr)

#clear all the variables
rm(list=ls())

#read data
data <- read_csv("spain.csv")
glimpse(data)

#timeseries object
#start
s <- seq(as.Date("2020-02-01"), as.Date("2020-02-01"), by = "day")
#end
e <- seq(as.Date("2020-04-06"), as.Date("2020-04-06"), by = "day")
data_ts <- ts(data[, 2], start = c(2020, as.numeric(format(s[1], "%j")))  , end = c(2020, as.numeric(format(e[1], "%j")) ), frequency = 365.25)
plot.ts(data_ts)

#Preliminary analysis

#naive, to be used for benchmark 3056.1973
naive_data=snaive(data[ , 2], h=10)
print(summary(naive_data))
checkresiduals(naive_data)

#Holt's model 882.82
holt_model <- holt(data_ts, h = 10)
summary(holt_model)
checkresiduals(holt_model)

#ARIMA ARIMA(3,1,0) (2,1,2) 853.58
arima_data=auto.arima(data_ts, d=1, D=0, stepwise=FALSE, approximation = FALSE, trace=TRUE)
print(arima_data)
checkresiduals(arima_data)

#Forecasting
fcst_data=forecast(arima_data,h=10)
autoplot(fcst_data)+ xlab("Time") + ylab("Confirmed cases")