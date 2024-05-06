library(readr)
library(tidyverse)
library(forecast)
library(itsmr)
library(aTSA)
library(EnvStats)
library(tseries)

nyg <- read.csv("C:/Users/Sam/Desktop/nygl.csv")

nygw <- nyg$Wins
plot.ts(nygw, ylab="Winning Percentage", xlab="Years Since 1966", main="New York Giants Yearly Win Percentage")
adf.test(nygw)
diff_nyg <- ts(diff(nygw, differences = 1))
adf.test(diff_nyg)
plot(diff_nyg, ylab="Differenced Winning Percentage", xlab="Years Since 1966", main="Differenced Yearly Win Percentage")
acf(nygw, main="ACF Plot for Giants Winning Percentage")
pacf(nygw, ylim=range(-0.25, 0.5), main="Partial ACF Plot for Giants Winning Percentage")
acf(diff_nyg, main="ACF Plot for Differenced Giants Winning Percentage")
pacf(diff_nyg, ylim=range(-0.5, 0.25), main="Partial ACF Plot for Differenced Giants Winning Percentage")
#ARIMA(0,1,1), (-0.701) or (-0.7725)

lambda_nyg1 <- boxcox(nygw, optimize = TRUE)
lambda_nyg2 <- BoxCox.lambda(nygw)
nyg_bc <- boxcoxTransform(nygw, lambda = lambda_nyg2)
plot.ts(nyg_bc)
adf.test(nyg_bc)

auto.arima(nygw)
auto.arima(diff_nyg)
auto.arima(nygw, d = 1)
arima(nygw, order = c(0,1,1))
arima(nygw, order = c(1,0,1))

nyg_011 <- arima(nygw, order = c(0,1,1))
forecast::forecast(nyg_011, h=5)
nyg_101 <- arima(nygw, order = c(1,0,1))
forecast::forecast(nyg_101, h=5)
nyg_hw <- HoltWinters(nygw, gamma=F)
forecast::forecast(nyg_hw, h=5)

train_nyg <- head(nygw, n=49)
test_nyg <- tail(nygw, n=6)
nyg_val_hw <- HoltWinters(train_nyg, gamma=F)
fore_1 <- forecast::forecast(nyg_val_hw, h=6)
fore_1
error_hw <- test_nyg - fore_1$mean
rmse_hw <- sqrt(abs(mean(error_hw)))
mae_hw <- mean(abs(error_hw))

nyg_val_011 <- arima(train_nyg, order=c(0, 1, 1))
fore_2 <- forecast::forecast(nyg_val_011, h=6)
fore_2
error_011 <- test_nyg - fore_2$mean
rmse_011 <- sqrt(abs(mean(error_011)))
mae_011 <- mean(abs(error_011))

nyg_val_101 <- arima(train_nyg, order=c(1,0,1))
fore_3 <- forecast::forecast(nyg_val_101, h=6)
fore_3
error_101 <- test_nyg - fore_3$mean
rmse_101 <- sqrt(abs(mean(error_101)))
mae_101 <- mean(abs(error_101))

rmse_hw
rmse_011
rmse_101

mae_hw
mae_011
mae_101

plot(fore_1, ylim=range(0,1))
plot(fore_2)
plot(fore_3)