library(readr)
install.packages("forecast")
library(forecast)
wallmart <- read.csv("WMT.csv")
View(wallmart)
#I. Create a time plot of the differenced series.
library(stats)
closing_prices <- wallmart$Close
diff_series <- diff(closing_prices)
plot(diff_series, type = "l", xlab = "Time", ylab = "Differenced Closing Prices",main = "Time Plot of Differenced Series")
#II. Which of the following is/are relevant for testing whether this stock is a random walk?
#a. The autocorrelations of the close prices series
acf(closing_prices, main = "Autocorrelation of Close Prices Series")
#Significant autocorrelations at higher lags may indicate nonrandom behavior.
#b. The AR(1) slope coefficient
closing_prices <- arima.sim(n=100, list(ar=0.7))
ar_model <- arima(closing_prices, order = c(1, 0, 0))
summary(ar_model)
ar_model$coef[1]
#C The AR(1) constant coefficient
ar_model <- arima(closing_prices, order = c(1, 0, 0))
summary(ar_model)
ar_model$coef[2]

Question | Correct Answer(s)
ii | a, b
iv | c