
rm(list=ls())

library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)

#read CSV
dat <- read_csv("india1.csv")
glimpse(dat)

#train and test data
dat_train = subset(dat, class == 'Train1')
dat_test = subset(dat, class == 'Test')

#number of rows in Train and test
nrow(dat_train); nrow(dat_test)

dat_demo <- read_csv("demo.csv")
glimpse(dat_demo)

dat_dts <- ts(dat_demo[, 2], start = 1 , frequency = 365.25)
plot.ts(dat_dts)




#timeseries object
s <- seq(as.Date("2020-01-01"), as.Date("2020-01-01"), by = "day")
e <- seq(as.Date("2020-02-08"), as.Date("2020-02-08"), by = "day")
dat_ts <- ts(dat_train[, 2], start = c(2020, as.numeric(format(s[1], "%j")) ) , end = c(2020, as.numeric(format(e[1], "%j")) ), frequency = 365.25)
plot.ts(dat_ts)





s_t <- seq(as.Date("2020-03-27"), as.Date("2020-03-27"), by = "day")
e_t <- seq(as.Date("2020-04-04"), as.Date("2020-04-04"), by = "day")



dtest_ts <- ts(dat_test[, 2], start = c(2020, as.numeric(format(s_t[1], "%j")) ) , end = c(2020, as.numeric(format(e_t[1], "%j")) ), frequency = 365.25)

plot.ts(dtest_ts)

plot(dtest_ts)

autoplot(dtest_ts) + ggtitle("Time plot") + ylab("Confirmed cases")+xlab("Time")
#
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

holt_model <- holt(dtest_ts, h = 10)
summary(holt_model)
autoplot(holt_model)

arima_model <- auto.arima(dtest_ts)
summary(arima_model)
checkresiduals(arima_model)

#forecast with ARIMA

fcast<-forecast(arima_model,h=10)
autoplot(fcast)


