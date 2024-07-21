library(tidyverse)
library(stats)
library(aTSA)
library(TTR)
install.packages("TTR")
#read in file
setwd("C:\\Users\\Kim\\Desktop\\D213\\")
m <-read.csv ("med.csv")
m

#Run a time series
t <- ts(m$Revenue, start = 0, frequency = 365)
head(t)

#plot time series
ts.plot(t, xlab = "Year", ylab = "Revenue in Millions", main = "Time Series - Revenue")

#Dickey Fuller test
adf.test(m$Revenue)

#differencing b/c of non-stationarity
dt <- diff(m$Revenue)
adf.test(dt)

#time seried using the staionary data
dtts <- ts(dt, start = 0, frequency = 365 )

#plot time series
ts.plot(dtts, xlab = "Year", ylab = "Revenue in Millions", main = "Time Series Stationary - Revenue")

train <- ts(dt[c(1:(length(dt)*.6))], start = 0, frequency = 365)
test <- ts(dt[-c(1:(length(dt)*.4))], start = 0, frequency = 365)
write_csv(as.data.frame(dt), "med_clean.csv")
capture.output(summary(test), file = "test.csv")
capture.output(summary(train), file = "train.csv")

#decompose for seasonality
de <- decompose(t, type = "additive")
plot(de)

#Autocorrelation
acf(dtts, lag.max = 12)

#Box test
Box.test(dtts, lag=12, type="Ljung-Box")

spec <- spectrum(dtts, log='no', span = 365, plot =TRUE)

arima(med, order = c(0L, 0L, 0L),
      seasonal = list(order = c(0L, 0L, 0L), period = NA),
      xreg = NULL, include.mean = TRUE,
      transform.pars = TRUE,
      fixed = NULL, init = NULL,
      method = c("CSS-ML", "ML", "CSS"), n.cond,
      SSinit = c("Gardner1980", "Rossignol2011"),
      optim.method = "BFGS",
      optim.control = list(), kappa = 1e6)
