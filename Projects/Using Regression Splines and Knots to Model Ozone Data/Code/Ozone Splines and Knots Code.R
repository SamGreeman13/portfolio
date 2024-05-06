rm(list = ls())
library("splines")
ozone.data <- read.table("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ozone.data", header=TRUE)

#Regression splines function for fixed knots
get.knots <- function(x, basis.knots=c(0.25, 0.5, 0.75)){
  return(quantile(x, probs=basis.knots))
}

#Regression splines function for basis
get.basis <- function(x, knots, intercept.index = FALSE){
  basis <- t(apply(matrix(x, ncol=1), 1, function(x){(x - knots)*((x - knots)>0)}))
  basis <- cbind(x, x^2, x^3, basis^3)
  if(intercept.index){
    basis <- cbind(1, basis)
  }
  return(basis)
}

#Regression splines function for range
get.range <- function(x){
  return(seq(min(x), max(x), 1))
}

#Full regression splines function
reg.splines <- function(x, y_in, name){
  data <- data.frame(y=y_in, x=get.basis(x, knots=get.knots(x)))
  x.new <- data.frame(x=get.basis(get.range(x), knots=get.knots(x)))
  b.model <- lm(y ~ ., data=data)
  y_hat <- predict(b.model, newdata=x.new, interval="predict")
  plot(x, y, xlab=name, ylab="Ozone", main="Regression Splines")
  lines(get.range(x), y_hat[,1], lwd=2)
  lines(get.range(x), y_hat[,2], lty=2)
  lines(get.range(x), y_hat[,3], lty=2)
  legend("topleft", legend=c("Fitted Value", "Prediction Interval"), lwd=c(2,1), lty=c(1, 2))
}

#regression splines plots
y <- ozone.data$ozone
reg.splines(ozone.data$radiation, y, "Radiation")
reg.splines(ozone.data$temperature, y, "Temperature")
reg.splines(ozone.data$wind, y, "Wind")

#regression plots function
b.model <- lm(ozone ~ get.basis(radiation, knots=get.knots(radiation)) + get.basis(temperature, knots=get.knots(temperature)) + get.basis(wind, knots=get.knots(wind)), data=ozone.data)
y_hat <- predict(b.model)

#regression plots
reg.plot <- function(x, y, name){
  y.order <- order(x)
  plot(x[y.order], y[y.order], xlab=name, ylab="Ozone", main="Regression Splines")
  lines(x[y.order], y_hat[y.order], lwd=2)
}
reg.plot(ozone.data$radiation, y, "Radiation")
reg.plot(ozone.data$temperature, y, "Temperature")
reg.plot(ozone.data$wind, y, "Wind")

#Natural splines function
nat.splines <- function(x, y_in, name){
  data <- data.frame(y=y_in, x=ns(x, knots=get.knots(x)))
  x.new <- data.frame(x=ns(get.range(x), knots=get.knots(x)))
  b.model <- lm(y ~ ., data=data)
  y_hat <- predict(b.model, newdata=x.new, interval="predict")
  plot(x, y, xlab=name, ylab="Ozone", main="Natural Splines")
  lines(get.range(x), y_hat[,1], lwd=2)
  lines(get.range(x), y_hat[,2], lty=2)
  lines(get.range(x), y_hat[,3], lty=2)
  legend("topleft", legend=c("Fitted Value", "Prediction Interval"), lwd=c(2,1), lty=c(1, 2))
}

#Natural splines plots
nat.splines(ozone.data$radiation, y, "Radiation")
nat.splines(ozone.data$temperature, y, "Temperature")
nat.splines(ozone.data$wind, y, "Wind")

#Natural plots
natural.model <- lm(ozone ~ ns(radiation, knots=get.knots(radiation)) + ns(temperature, knots=get.knots(temperature)) + ns(wind, knots=get.knots(wind)), data=ozone.data)
y_predicted <- predict(natural.model)

nat.plot <- function(x, y, name){
  y.order <- order(x)
  plot(x[y.order], y[y.order], xlab=name, ylab="Ozone", main="Natural Splines")
  lines(x[y.order], y_hat[y.order], lwd=2)
}

nat.plot(ozone.data$radiation, y, "Radiation")
nat.plot(ozone.data$temperature, y, "Temperature")
nat.plot(ozone.data$wind, y, "Wind")

#Smoothing splines function and plots
x <- ozone.data$wind
smooth.plot <- function(x, y, name){
  plot(x, y, xlab=name, ylab="Ozone", main="Smoothing Splines")
  lines(smooth.spline(x, y), lwd=2)
}
smooth.plot(ozone.data$radiation, y, "Radiation")
smooth.plot(ozone.data$temperature, y, "Temperature")
smooth.plot(ozone.data$wind, y, "Wind")