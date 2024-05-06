october.data <- read.csv("C:/Users/Sam/Desktop/october.csv")
april.data <- read.csv("C:/Users/Sam/Desktop/april.csv")

april.high <- april.data$max
april.low <- april.data$min
april.rain <- april.data$prec
october.high <- october.data$max
october.low <- october.data$min
october.rain <- october.data$prec

hist(october.high, col = "gray", border = "white", xlab = "High Temps in October")
hist(october.low, col = "gray", border = "white", xlab = "Low Temps in October")
hist(october.rain, col = "gray", border = "white", xlab = "Rain in October")
hist(april.high, col = "gray", border = "white", xlab = "High Temps in April")
hist(april.low, col = "gray", border = "white", xlab = "Low Temps in April")
hist(april.rain, col = "gray", border = "white", xlab = "Rain in April")

n_april = length(april.high)
n_october = length(october.high)
mean_april.high = mean(april.high)
mean_april.low = mean(april.low)
mean_april.rain = mean(april.rain)
mean_october.high = mean(october.high)
mean_october.low = mean(october.low)
mean_october.rain = mean(october.rain)

mean_april.high
mean_october.high
mean_april.low
mean_october.low
mean_april.rain
mean_october.rain

n_april = length(april.high)
mean_april.high = mean(april.high)
t = 1/(sd(april.high))
a = 64
b = 15
p_mean_april.high = 1/(1 + n_april*t) * a + n_april*t/(1 + n_april*t) * mean_april.high
p_sd_april.high = 1/(1 + n_april*t)
Res = rnorm(n = 1000, mean = p_mean_april.high, sd = p_sd_april.high)
hist(Res, col = "gray", border = "white", xlab = "High Temps in April")
p_mean_april.high

n_october = length(october.high)
t1 = 1/(sd(october.high))
a1 = 64
b1 = 15
p_mean_october.high = 1/(1 + n_october*t1) * a1 + n_october*t1/(1 + n_october*t1) * mean_october.high
p_sd_october.high = 1/(1 + n_october*t1)
Res = rnorm(n = 1000, mean = p_mean_october.high, sd = p_sd_october.high)
hist(Res, col = "gray", border = "white", xlab = "High Temps in October")
p_mean_october.high

n_april = length(april.low)
mean_april.low = mean(april.low)
t2 = 1/(sd(april.low))
a1 = 41
b1 = 11
p_mean_april.low = 1/(1 + n_april*t2) * a1 + n_april*t2/(1 + n_april*t2) * mean_april.low
p_sd_april.low = 1/(1 + n_april*t2)
Res = rnorm(n = 1000, mean = p_mean_april.low, sd = p_sd_april.low)
hist(Res, col = "gray", border = "white", xlab = "Low Temps in April")
p_mean_april.low

n_october = length(october.low)
mean_october.low = mean(october.low)
t3 = 1/(sd(october.low))
a1 = 41
b1 = 11
p_mean_october.low = 1/(1 + n_october*t3) * a1 + n_october*t3/(1 + n_october*t3) * mean_october.low
p_sd_october.low = 1/(1 + n_october*t3)
Res = rnorm(n = 1000, mean = p_mean_october.low, sd = p_sd_october.low)
hist(Res, col = "gray", border = "white", xlab = "Low Temps in October")
p_mean_october.low

n_april = length(april.high)
mean_april.rain = mean(april.rain)
t4 = 1/(sd(april.rain))
a2 = 0.1
b2 = 0.03
p_mean_april.rain = 1/(1 + n_april*t4) * a2 + n_april*t4/(1 + n_april*t4) * mean_april.rain
p_sd_april.rain = 1/(1 + n_april*t4)
Res = rnorm(n = 1000, mean = p_mean_april.rain, sd = p_sd_april.rain)
hist(Res, col = "gray", border = "white", xlab = "Rain in April")
p_mean_april.rain

n_october = length(october.rain)
mean_october.rain = mean(october.rain)
t5 = 1/(sd(october.rain))
a2 = 0.1
b2 = 0.03
p_mean_october.rain = 1/(1 + n_october*t5) * a2 + n_october*t5/(1 + n_october*t5) * mean_october.rain
p_sd_october.rain = 1/(1 + n_october*t5)
Res = rnorm(n = 1000, mean = p_mean_october.rain, sd = p_sd_october.rain)
hist(Res, col = "gray", border = "white", xlab = "Rain in October")
mean_october.rain