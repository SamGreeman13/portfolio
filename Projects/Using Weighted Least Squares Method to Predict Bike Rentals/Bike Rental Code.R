library(tidyverse)
library(janitor)
library(readxl)
library(readr)
library(ggridges)
library(e1071)
library(ISLR)
library(randomForest)
library(rpart)
library(caret)
library(pls)
library(gbm)
library(pander)
library(zoo)
library(corrplot)
library(lubridate)
library(carData)
library(car)
library(plotly)
library(ggpubr)
library(glmnet)
library(GGally)

#data processing
options(scipen = 200) #change all number to numeric
options(scipen = 1)
bicycle <- read.csv("C:/Users/Sam/Desktop/day.csv", header = TRUE)
attach(bicycle)
##NOTE## The line below originally read 'format = "%m/%d/%Y"'
bicycle$dteday = as.Date(dteday, format= "%Y-%m-%d")  #change the factor date to dateformat

##NOTE## I commented the below line since it was clearing bike_2011
#year(bicycle$dteday) <- as.numeric(paste("20", as.character(year(bicycle$dteday)), sep=""))  

#add special event variable
###NOTE: positive---represents the holidays which is likely that more people go out and use bikes. 
###extre_weather---represents the day in 2012 that the extreme weather happened.(We marked them by the information we got from the paper that the prof sent before.)
bicycle= bicycle %>% 
  mutate(weathersit = as.factor(weathersit)) %>% 
  mutate(special_event = dteday) %>% 
  mutate(special_event = as.character(special_event)) %>% 
  mutate(special_event = ifelse(special_event %in% c("2011-07-04", "2011-11-23", "2011-12-24", "2011-10-08", "2011-05-27", "2011-11-22", "2011-11-12", "2011-04-16", "2011-03-23", "2011-05-13", "2011-02-11", "2011-01-23", "2011-09-29", "2012-07-04", "2012-11-23", "2012-12-24", "2012-10-08", "2012-05-27", "2012-11-22", "2012-11-12", "2012-04-16", "2012-03-23", "2012-05-13", "2012-02-11", "2012-01-23", "2012-09-29"), "positive",
                                ifelse(special_event %in% c("2012-10-29", "2012-10-30", "2012-10-19", "2012-09-18", "2012-07-18", "2012-06-01", "2012-12-04", "2012-10-07", "2012-05-21", "2012-04-07", "2012-05-26", "2012-09-15", "2012-10-11", "2012-10-12", "2012-01-29"),"extre_weather", "normal"))) %>% 
  mutate(special_event = as.factor(special_event)) %>% 
  mutate(season = ifelse(season %in% c("2"), "spring",
                         ifelse(season %in% c("3"), "summer",
                                ifelse(season %in% c("4"), "fall", "winter")))) %>% 
  mutate(weathersit = ifelse(weathersit %in% c("1"), "clear/partial_cloudy",
                             ifelse(weathersit %in% c("2"), "mist_cloudy",
                                    ifelse(weathersit %in% c("3"), "light_snow/rain/storm", "heavy_snow/rain/storm")))) %>% 
  mutate(holiday = ifelse(holiday %in% c("1"), "yes", "no")) %>% 
  mutate(workingday = ifelse(workingday %in% c("0"), "holiday_and_weekend", "weekday"))

#change the form of some variables
bicycle$season <- as.factor(bicycle$season)
bicycle$yr <- as.factor(bicycle$yr)
bicycle$mnth <- as.factor(bicycle$mnth)
bicycle$holiday <- as.factor(bicycle$holiday)
bicycle$workingday <- as.factor(bicycle$workingday)
bicycle$weathersit <- as.factor(bicycle$weathersit)
#change the factor level of season variable
bicycle$season = factor(bicycle$season, levels(bicycle$season)[c(3,4,2,1)])
bicycle$weathersit = factor(bicycle$weathersit, levels(bicycle$weathersit)[c(1,3,2)])

bicycle = bicycle %>% 
  mutate(date = c(seq(1:365), seq(1:366))) 
#separate the data into two parts: in year 2011/2012
bike_2011 = subset(bicycle, dteday >= "2011-01-01" & dteday <= "2011-12-31")
bike_2012 = subset(bicycle, dteday >= "2012-01-01" & dteday <= "2012-12-31")
detach(bicycle)

attach(bike_2011)
#We draw the scatter matrix (without discrete variables)
pairs(~ registered + instant + temp + atemp + hum + windspeed + casual + registered, data = bike_2011, upper.panel = NULL)
DataSet <- data.frame(temp, atemp, registered, casual, hum, windspeed)
ggpairs(DataSet)
#Then we have:Correlation Matrices + Correlation Plot
#X <- cbind(bike_2011 %>% select(instant,temp:registered))
#corr_data <- cor(X)
#round(corr_data, 3)
#corrplot(corr_data, method = "number")
#box plot
#Make a scatterplot of each covariate against 'count'
#I used these to determine the categorical variables to include in the initial model
#boxplot
p1 = ggplot(bike_2011, aes(x = workingday, y = registered, fill = workingday)) + geom_boxplot() + xlab("Weekday or Holiday") + ylab("Registered Users")+theme(legend.title = element_text(size=16)) +theme(legend.text = element_text(size=13))+ ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5))

p2 = ggplot(bike_2011, aes(x = season, y = registered, fill = season)) + geom_boxplot() + xlab("Season") + ylab("Registered Users")+theme(legend.title = element_text(size=16)) +theme(legend.text = element_text(size=13))+ ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5))
ggarrange(p1,p2, ncol=2, nrow = 1)
#scatter plot

#date-cnt
p3 <- ggplot(bike_2011, aes(x = dteday, y = registered, color = season)) + geom_point() + xlab("Date") + ylab("Registered Count")+theme(legend.title = element_text(size=16)) +theme(legend.text = element_text(size=13))+ geom_smooth(method = "lm") + ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5))
#temperature - cnt
p4 <- ggplot(bike_2011, aes(x=atemp, y=registered, color= season)) + geom_point() + xlab("Adjusted Temp") + ylab("Registered Count") +theme(legend.title = element_text(size=16)) +theme(legend.text = element_text(size=13))+ geom_smooth(method = "lm") + ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5))
#humidity - cnt
p5 <- ggplot(bike_2011, aes(x=hum, y=registered, color= season)) + geom_point() + xlab("Humidity") + ylab("Registered Count") + geom_smooth(method = "lm") + ggtitle("(c)")+
  theme(plot.title = element_text(hjust = 0.5))
#windspeed -cnt
p6 <- ggplot(bike_2011, aes(x=windspeed, y=registered, color= season)) + geom_point() + xlab("Wind Speed") + ylab("Registered Count") + geom_smooth(method = "lm") + ggtitle("(d)")+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p3,p4, ncol=2, nrow = 1)

#plot in report
#ggplot(bike_2011, aes(x=atemp, y=registered, color= season)) + geom_point() + xlab("Adjusted Temp") + ylab("Registered Count") + geom_smooth(method = "lm") 

#initial model
# Create an initial model based on the variables above:
m.mls.initial <- lm(registered~date+I(date^2)+atemp+season+windspeed+workingday+weathersit+season*atemp, data = bike_2011)
options(scipen = 200) #change all number to numeric
options(scipen = 1)
summary(m.mls.initial)

###QQ plot and hitogram plot of initial OLS model
par(mfrow=c(1,2))
# Standardized Residuals of Initial Model
StanResInitial <- rstandard(m.mls.initial)
# (1) histogram of residual
hist(StanResInitial,50, xlab = "standard residual",main="(a)", font.main =1)

#qqplot
q1_initial <- qqnorm(StanResInitial, plot.it = FALSE)
plot(range(q1_initial$x, q1_initial$x), range(q1_initial$y, q1_initial$y), ylab = "Standardized Residuals", xlab ="Theoretical Quantiles")
points(q1_initial, col="blue", pch = 19, cex = 0.3)
qqline(StanResInitial,lty=2)+ title("(b)", font.main =1, adj = 0.5)

#Stepwise Variable Selection
# Variable selection
#################################################################################
#Stepwise to choose variable
###refine
StepBIC <- step(m.mls.initial,direction="both", data=bike_2011, k=log(365))
#we get exact same result

#Final OLS model
#OLS model
m.mls.step_interaction <-lm(registered~date+I(date^2)+atemp+season+windspeed+workingday+weathersit+season*atemp, data = bike_2011)
summary(m.mls.step_interaction)
###############################################################################
#WLS model
# Calculate fitted values from a regression of absolute residuals vs SensorCO
wts <- 1/fitted(lm(abs(residuals(m.mls.step_interaction)) ~ registered))^2
#refit the model with weight
m.mls.step_interaction_wls <-lm(registered~date+I(date^2)+atemp+season+windspeed+workingday+weathersit+season*atemp, data = bike_2011, weights=wts)
options(scipen = 200) #change all number to numeric
options(scipen = 1)
summary(m.mls.step_interaction_wls) #after checking, RMSE is little bit smaller, but Y vs Y_hat almost the same.

# Training
#std residual for WLS
StanResInitial_wls <- rstandard(m.mls.step_interaction_wls)

######Combine the residual plot together
par(mfrow=c(2,2))
#combine std residual vs. fitted in OLS and Weighted
#OLS
plot(fitted(m.mls.step_interaction),StanResInitial,xlab="fitted value of initial model", ylab="Standardized Residuals",xlim=c(0, 5000), ylim=c(-6, 6), col="blue", pch = 19, cex = 0.5)
abline(h=2,lty=2)
abline(h=-2,lty=2)+ title("(a)", adj =0.5, line = 0.5, cex.main=1.6, font.main =1)
#Weighted
plot(fitted(m.mls.step_interaction_wls),StanResInitial_wls,xlab="fitted value of weighted model", ylab="Standardized Residuals",xlim=c(0, 5000), ylim=c(-6, 6), col="blue", pch = 19, cex = 0.5)
abline(h=2,lty=2)
abline(h=-2,lty=2)+ title("(b)", adj =0.5, line = 0.5, cex.main=1.6, font.main =1)

##########
#together with the casual plots
plot(c.mls.initial$fitted.values, StanResInitial_c, col = "blue",xlim = c(-600,2000), ylim = c(-6,6), xlab = "fitted value  of initial model", ylab = "Standard Residual", pch = 20)
abline(h=2, lty = 2)
abline(h=-2, lty = 2)+title("(c)", adj =0.5, line = 0.5, cex.main=1.6, font.main =1)

plot(c.mls.weighted$fitted.values, StandResWeight_c, xlim = c(-600,2000), ylim = c(-6,6), col = "blue", xlab = "fitted value of weighted model", ylab = "Standard Residual", pch = 20)
abline(h=2, lty=2)+title("(d)", adj =0.5, line = 0.5, cex.main=1.6, font.main =1)
abline(h=-2, lty=2)
#########

#plot Y v.s. Y_hat in WLS model
par(mfrow=c(1,1))
plot(date, registered, pch = 20, col = "red", xlim = c(1,400), ylim = c(-500, 5000), xlab = "day", ylab = "Count of Users")+ title("", adj =0, line = 0.5, cex.main=1.1, font.main =1)
points(date, m.mls.step_interaction$fitted.values, pch = 20, col = "blue", xlim = c(1,400), ylim = c(-500, 5000), xlab = "day", ylab  = "Count of Users")
legend ("topleft", legend = c("registered", "fitted registered"), col =c("red", "blue"), lty=0, cex=1.2, pch=20, bty = "n")
lines(date, registered, pch = 20, col ="red", type = "b", lty = 1)
lines(date, m.mls.step_interaction$fitted.values, pch = 20, col ="blue", type = "b", lty = 1)

# Validation ------------------------------------------------------------------------------------------------
#delete the entries of data which include extre_weather date
bike_2012= bike_2012 %>% 
  filter(special_event != "extre_weather")
# Residuals for training data
ResMLS <- resid(m.mls.step_interaction_wls)
# Residuals for validation data
output<-predict(m.mls.step_interaction_wls, se.fit = TRUE, newdata = bike_2012)
ResMLSValidation <- bike_2012$registered - output$fit

####MSE for OLS model-Training
ResMLS_OLS = resid(m.mls.step_interaction)
# Mean Square Error for training data
mean((ResMLS_OLS)^2)  #summation of (Yi-Yi_hat)^2/n

#####
# Mean Square Error for WLS model-Training
mean((ResMLS)^2)  #summation of (Yi-Yi_hat)^2/n

# Mean Square Error for validation data
mean((ResMLSValidation)^2)

# Relative Mean Square Error for validation data
mean((ResMLSValidation)^2) / mean((bike_2012$cnt)^2)
# summation of (Yi-Yi_hat)^2/summation of Yi^2

# Now the RMSE: 0.1518124, which is smaller than 0.1520601 we got before

#Y vs. Y_hat
par(mfrow=c(1,1))
#registered
plot(bike_2012$date,bike_2012$registered, pch = 20, col = "red", xlim = c(1,400), ylim = c(-800, 7000), xlab = "day", ylab = "Count of Users", type = 'b')+ title("", adj =0, line = 0.5, cex.main=1.1, font.main =1)
points(bike_2012$date, output$fit, pch = 20, col = "blue", xlim = c(1,400), ylim = c(-800, 7000), xlab = "day", ylab  = "Count of Users", type = 'b')
legend ("topleft", legend = c("registered", "fitted registered"), col =c("red", "blue"), lty=0, cex=1.2, pch=20, bty = "n")

#############################
New model: response variable -- casual
attach(bike_2011)

# matrix plot of continuos variables 
pairs(~casual+instant+temp+atemp+hum+windspeed,data=bike_2011, upper.panel = NULL)
DataSet<-data.frame(temp,atemp,casual,hum,windspeed)
ggpairs(DataSet)

### choose "atemp"  between "temp" and "atemp"

# scatter plot of discrete variables
p2_1 <- ggplot(bike_2011, aes(x = dteday, y = casual, color = season)) + geom_point() + xlab("Date") + ylab("Causal Count") +theme(legend.title = element_text(size=16)) +theme(legend.text = element_text(size=13))+ geom_smooth(method = "lm") + ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5))
# temperature - casual
p2_2 <- ggplot(bike_2011, aes(x=atemp, y=casual, color= season)) + geom_point() + xlab("Adjusted Temp") + ylab("Causal Count") +theme(legend.title = element_text(size=16)) +theme(legend.text = element_text(size=13))+ geom_smooth(method = "lm") + ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5))
# humandity - casual
p2_3 <- ggplot(bike_2011, aes(x=hum, y=casual, color= season)) + geom_point() + xlab("Humidity") + ylab("Casual Count") + geom_smooth(method = "lm") + ggtitle("(c)")+
  theme(plot.title = element_text(hjust = 0.5))
# windspeed-casual
p2_4 <- ggplot(bike_2011, aes(x=windspeed, y=casual, color= season)) + geom_point() + xlab("Wind Speed") + ylab("Causal Count") + geom_smooth(method = "lm") + ggtitle("(d)")+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p2_1, p2_2, ncol = 2, nrow = 1)

### consider the interaction of season & humidity and season & windspeed

# boxplot of discrete variables 
p2_5 =ggplot(bike_2011, aes(x = workingday, y = casual, fill = workingday)) + geom_boxplot() + xlab("Weekday or Holiday") + ylab("Casual Users")+theme(legend.title = element_text(size=16)) +theme(legend.text = element_text(size=13))+ ggtitle("(a)")+
  theme(plot.title = element_text(hjust = 0.5))

p2_6 =ggplot(bike_2011, aes(x = season, y = casual, fill = season)) + geom_boxplot() + xlab("Season") + ylab("Casual Users")+theme(legend.title = element_text(size=16)) +theme(legend.text = element_text(size=13))+ ggtitle("(b)")+
  theme(plot.title = element_text(hjust = 0.5))
ggarrange(p1,p2, ncol=2, nrow = 1)

ggarrange(p2_5, p2_6, ncol = 2, nrow = 1)

# Initial model for casual
c.mls.initial <- lm(casual~atemp + season+windspeed+workingday+weathersit+season*atemp + date + I(date^2), data = bike_2011)

options(scipen = 200) #change all number to numeric
options(scipen = 1)
summary(c.mls.initial)
# Residual
Res_c <- resid(c.mls.initial)

par(mfrow = c (1,2))
# (a)standard residual v.s. casual
StanResInitial_c <- rstandard(c.mls.initial)
plot(bike_2011$casual,StanResInitial_c,xlab="Count of Casual Users", ylab="Standardized Residuals", col="blue", xlim = c(0,3500), ylim = c(-6,6), pch = 19, cex = 0.5)
abline(h=2,lty=2)
abline(h=-2,lty=2)+ title("(a)", adj =0, line = 0.5, cex.main=1.6, font.main =1)

# (b)standard residual v.s. fitted 
plot(fitted(c.mls.initial),StanResInitial_c,xlab="Fitted", ylab="Standardized Residuals", col="blue",xlim = c(-600,2000), ylim = c(-6,6), pch = 19, cex = 0.5)
abline(h=2,lty=2)
abline(h=-2,lty=2)+ title("(b)", adj = 0.5, line = 0.5, cex.main=1.6, font.main =1)

# histgram and qq-plot

# (1) histogram of residual
hist(StanResInitial_c,50, xlab = "standard residual",main="(a)", font.main =1)

# (2) qq-plot & qq-line
qq_initial_c <- qqnorm(StanResInitial_c, plot.it = FALSE)
plot(range(qq_initial_c$x, qq_initial_c$x), range(qq_initial_c$y, qq_initial_c$y), ylab = "Standardized Residuals", xlab ="Theoretical Quantiles")
points(qq_initial_c, col="blue", pch = 19, cex = 0.3)
qqline(StanResInitial_c,lty=2)+ title("(b)", font.main =1, adj = 0.5)

# Variable selection by stepwise
step_model_interaction_c <- step(lm(formula = casual ~ atemp + season+windspeed+workingday+weathersit+season*atemp + date + I(date^2),data = bike_2011))

### after the stepwise, variables chosen out are the same as those in initial model.

# add weight in the initial model 
# calculatet the weighte value
wts_c <- 1/fitted(lm(abs(residuals(c.mls.initial)) ~ bike_2011$casual))^2

#refit the model with weight
c.mls.weighted <- lm(casual ~ atemp + season+windspeed+workingday+weathersit+season*atemp + date + I(date^2), data = bike_2011,weights=wts_c)

summary(c.mls.weighted)

# the ressidual of weighted model
ResWeight_c <- resid(c.mls.weighted)

# standard residual 
StandResWeight_c <- rstandard(c.mls.weighted)

# validation 
#delete the entries of data which include extre_weather date
bike_2012= bike_2012 %>% 
  filter(special_event != "extre_weather")

# Residaul for validation data
outcome_c <- predict(c.mls.weighted, se.fit = TRUE, newdata = bike_2012)
ResMLSValidation_c <- bike_2012$casual - outcome_c$fit

# compare residual of traing and validation (plot)
#par(mfrow=c(1,1))
#plot(bike_2011$casual,Res_c,xlab="casual ", ylab="Residuals",xlim=c(0,3500), ylim=c(-600,2500),  col=c("blue"), lty=0, cex=1, pch=19)
#points(bike_2012$casual,ResMLSValidation_c,xlab="casual", ylab="Residuals",xlim=c(0,3500), ylim=c(-600,2500),col="red", lty=0, cex=1, pch=19)
#legend(3, 4000, legend=c("Training","Validation"), col=c("blue","red"), lty=0, cex=0.7, pch=19)

#MSE for training OLS
Res_OLS <- resid(c.mls.initial)
mean((Res_OLS)^2) 

# Mean Square Error for training data WLS
mean((ResWeight_c)^2)  #summation of (Yi-Yi_hat)^2/n

# Mean Square Error for validation data
mean((ResMLSValidation_c)^2)

# Relative Mean Square Error for validation data
mean((ResMLSValidation_c)^2) / mean((bike_2012$casual)^2)
# summation of (Yi-Yi_hat)^2/summation of Yi^2

#training
# plot of 
plot(bike_2011$date, bike_2011$casual, col = "red", type = "b", pch = 20,ylim = c(-400, 3500), ylab = "Count of Users",xlab = "day")
points(bike_2011$date, c.mls.weighted$fitted.values, col ="blue" , type = "b",ylim = c(-400, 3500), pch = 20)
legend("topleft", legend = c("casual", "fitted casual"), col = c("red", "blue"), cex = 1.2, lty = 0, pch = 20, bty = "n")

# Prediction
# y and y hat 
par(mfrow = c (1,1))
plot(bike_2012$date, bike_2012$casual, pch = 20, col = "red", xlim = c(1,400), ylim = c(-700, 3500), xlab = "day", ylab = "Count of Users", type = 'b')+ title("", adj =0, line = 0.5, cex.main=1.1, font.main =1)
points(bike_2012$date, outcome_c$fit, pch = 20, col = "blue", xlim = c(1,400), ylim = c(-700, 3500), xlab = "day", ylab  = "Count of Users", type = 'b')
legend ("topleft", legend = c("casual", "fitted casual"), col =c("red", "blue"), lty=0, cex=1.2, pch=20, bty = "n")

# compare weighted model and initial model 
# fitted value v.s. standard residual 
plot(c.mls.initial$fitted.values, StanResInitial_c, col = "red", xlab = "fitted value", ylab = "Standard Residual", pch = 20)
points(c.mls.weighted$fitted.values, StandResWeight_c, col = "blue", xlab = " ", ylab = " ", pch = 20)
abline(h=2, lty=2)
abline(h=-2, lty=2)+title("(a)", adj =0.5, line = 0.5, cex.main=1.6, font.main =1)
legend("topleft", legend = c("initial", "weighted"), col = c("red", "blue"), lty = 0, pch = 4, cex = 1.2, bty="n" )


# plot them separately
par(mfrow = c(1,2))
plot(c.mls.initial$fitted.values, StanResInitial_c, col = "blue",xlim = c(-600,2000), ylim = c(-6,6), xlab = "fitted value  of initial model", ylab = "Standard Residual", pch = 20)
abline(h=2, lty = 2)
abline(h=-2, lty = 2)+title("(a)", adj =0.5, line = 0.5, cex.main=1.6, font.main =1)

plot(c.mls.weighted$fitted.values, StandResWeight_c, xlim = c(-600,2000), ylim = c(-6,6), col = "blue", xlab = "fitted value of weighted model", ylab = "Standard Residual", pch = 20)
abline(h=2, lty=2)+title("(b)", adj =0.5, line = 0.5, cex.main=1.6, font.main =1)
abline(h=-2, lty=2)