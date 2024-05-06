library(ggpubr)
library(nortest)
library(tsoutliers)
library(tseries)

data <- c(16.31, 18.66, 18.15, 17.47, 16.97, 16.42, 20.84, 15.59, 13.40, 16.56, 15.11, 19.89, 23.37, 15.89, 17.96, 14.90, 17.69, 19.53, 15.64, 15.56, 15.74, 14.87, 17.93, 20.80, 19.82, 19.62, 21.45, 19.50, 16.47, 15.08, 15.36, 12.95, 13.71, 19.08)
#normal probability plot
qqnorm(data, xlab="Z-score", ylab="Observed Data", main="Normal Probability Plot") 
qqline(data)

#qq plot
ggqqplot(data)

#histogram
hist(data)

#shapiro-wilk test
shapiro.test(data)

#anderson-darling test
ad.test(data)

#jarque-bera test
jarque.bera.test(data)