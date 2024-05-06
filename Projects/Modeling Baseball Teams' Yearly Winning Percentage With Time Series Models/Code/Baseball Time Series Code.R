library(readr)
library(tidyverse)
library(forecast)
library(itsmr)
library(aTSA)
library(EnvStats)
library(tseries)

#read in data
cin <- read.csv("C:/Users/Sam/Desktop/wcin.csv")

cinc <- cin$Wins

#figure 1
plot.ts(cinc, ylab="Winning Percentage", xlab="Years Since 1947", main="Cinncinnati Reds Yearly Win Percentage")

#augmented dickey-fuller test
adf.test(cinc)
#test statistic = -2.242, lag = 4, p = 0.4768
#fail to reject h(0) of data being non-stationary

#difference the data 1 time
diff_cin <- ts(diff(cinc, differences = 1))

#augmented dickey-fuller test
adf.test(diff_cin)
#test statistic = -5.362, lag = 4, p < 0.01
#reject h(0) of once-differenced data being non-stationary, meaning d = 1

#figure 2
plot(diff_cin, ylab="Differenced Winning Percentage", xlab="Years Since 1947", main="Differenced Yearly Win Percentage")

#exponential smoothing with alpha = 0.6
cin_es <- ses(cinc,alpha=0.6,initial="simple")

#figure 3
plot(fitted(cin_es), ylab="Smoothed Winning Percentage", xlab="Years Since 1947", main="Exponential Smoothing of Reds Data")

#not included in report, but added the original data to compare with exponentially smoothed data
lines(cinc, col="red")

#augmented dickey-fuller test for exponentially smoothed data (not differenced)
adf.test(fitted(cin_es))
#test statistic = -1.4639, lag = 4, p = 0.7938
#fail to reject h(0) of exponentially smoothed data being non-stationary

#augmented dickey-fuller test for exponentially smoothed differenced data
cin_esd <- ses(diff_cin,alpha=0.6,initial="simple")
adf.test(fitted(cin_esd))
#test statistic = -5.0864, lag = 4, p < 0.01
#reject h(0) of exponentially smoothed, once-differenced data being non-stationary, meaning d = 1

#next 2 figures not included in report, but they are ACF and PACF plots of non-differenced data
acf(cinc, main="ACF Plot for Reds Winning Percentage")
pacf(cinc, ylim=range(-0.25, 0.5), main="Partial ACF Plot for Reds Winning Percentage")
#acf plot shows that our time series decays to 0

#figures 4a and 4b
acf(diff_cin, main="ACF Plot for Differenced Reds Winning Percentage")
pacf(diff_cin, ylim=range(-0.5, 0.25), main="Partial ACF Plot for Differenced Reds Winning Percentage")
#acf and pacf at lag 1 are equal
#acf cuts off after lag 1, pacf decays to 0, meaning q = 1

#testing similar order models to confirm ARIMA(0,1,1)
arima(cinc, order=c(2,1,1)) #LL = 99.93, AIC = -191.86
arima(cinc, order=c(2,0,0)) #LL = 102.98, AIC = -197.96
arima(cinc, order=c(1,1,0)) #LL = 95.95, AIC = -187.9
arima(cinc, order=c(0,1,1)) #LL = 98.97, AIC = -193.94
arima(cinc, order=c(0,1,2)) #LL = 99.12, AIC = -192.23
arima(cinc, order=c(1,1,1)) #LL = 99.22, AIC = -192.43
#for d = 1 models, ARIMA(0,1,1) has lowest AIC
#important to note that while ARIMA(2,0,0) has lower AIC, we can't use it since we rejected the null hypothesis that the data is stationary, so d can't be 0
#for ARIMA(0,1,1), coefficient (theta) = -0.661 or -0.6604, using acf and pacf at lag 1 = -0.46 = theta/(1 + theta^2)

#prepare data validation testing by using the first 66 years as training data, following 7 years as test data
train_cinc <- head(cinc, n=66)
test_cin <- tail(cinc, n=7)

#table 1 data
#holt-winters forecast
cinc_val_hw <- HoltWinters(train_cinc, gamma=F)
fore_1 <- forecast::forecast(cinc_val_hw, h=7)
fore_1

#ARIMA(0,1,1) forecast
cinc_val_arima <- arima(train_cinc, order=c(0, 1, 1))
fore_2 <- forecast::forecast(cinc_val_arima, h=7)
fore_2

#exp smooth forecast
cinc_val_es <- ses(train_cinc,alpha=0.6,initial="simple")
fore_3 <- forecast::forecast(cinc_val_es, h=7)
fore_3

#figures 5a, 5b, 5c
plot(fore_1)
plot(fore_2)
plot(fore_3)

#table 2 data
#holt-winters
error_hw <- test_cin - fore_1$mean
rmse_hw <- sqrt(abs(mean(error_hw)))
mae_hw <- mean(abs(error_hw))
mape_hw <- mean(abs((error_hw*100)/test_cin))
rmse_hw
mae_hw
mape_hw

#ARIMA(0,1,1)
error_arima <- test_cin - fore_2$mean
rmse_arima <- sqrt(abs(mean(error_arima)))
mae_arima <- mean(abs(error_arima))
mape_arima <- mean(abs((error_arima*100)/test_cin))
rmse_arima
mae_arima
mape_arima

#exp smooth
error_es <- test_cin - fore_3$mean
rmse_es <- sqrt(abs(mean(error_es)))
mae_es <- mean(abs(error_es))
mape_es <- mean(abs((error_es*100)/test_cin))
rmse_es
mae_es
mape_es




