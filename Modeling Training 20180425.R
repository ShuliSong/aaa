##### ---------------------------------------------------- Moldeing training ----------------------------- ###############################################################################
##### ---------------------------------------------------- Moldeing training ----------------------------- ###############################################################################
##### ---------------------------------------------------- Moldeing training ----------------------------- ###############################################################################
##### ---------------------------------------------------- Moldeing training ----------------------------- ###############################################################################

############### Packages you need to install and loading before modeling training ######################

###########   install Packages  ###############

install.packages("car")
install.packages("e1071")
install.packages("DAAG")
install.packages("lattice")
install.packages("gdata")
install.packages("caTools")
install.packages("gplots")
install.packages("bitops")
install.packages("ROCR")
install.packages("rpart")    
install.packages("partykit")
install.packages("rpart.plot")
install.packages("party")
install.packages("tseries")

###########   Loading Packages  ###############
library(car)
library(e1071)
library(DAAG)
library(lattice)
library(gdata)
library(caTools)
library(gplots)
library(bitops)
library(ROCR)
library(rpart)    
library(partykit)
library(rpart.plot)
library(party)
library(tseries)

############### Packages you need to install and loading before modeling training ######################


##### ==================================================== Linear Regression ============================= #####################################################################
##### ==================================================== Linear Regression ============================= #####################################################################


install.packages("car")

library(car)

#dataset used: cars
View(cars)
?cars

# Linear regression is used to predict the value of an outcome variable Y based on one or more input predictor variables X. 
# The aim is to establish a linear relationship (a mathematical formula) between the predictor variable(s) and the response variable,
# so that, we can use this formula to estimate the value of the response Y, when only the predictors (Xs) values are known.


# --------------------------------------------------------------------------------#
# ----------------------------Graphical Analysis----------------------------------#
# --------------------------------------------------------------------------------#

# The aim of this exercise is to build a simple regression model that we can use to predict Distance (dist) by establishing a statistically significant linear relationship with Speed (speed). 
# 1. Scatter plot: Visualize the linear relationship between the predictor and response
# 2. Box plot: To spot any outlier observations in the variable. Having outliers in your predictor can drastically affect the predictions as they can easily affect the direction/slope of the line of best fit.
# 3. Density plot: To see the distribution of the predictor variable. Ideally, a close to normal distribution (a bell shaped curve), without being skewed to the left or right is preferred. Let us see how to make each one of them.
# 4. Correlation: a statistical measure that suggests the level of linear dependence between two variables.
#1.scatter plot
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

#2. BoxPlot – Check for outliers
par(mfrow=c(1,2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

#3. Density plot – Check if the response variable is close to normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")

#correlation
cor(cars$speed, cars$dist)  # calculate correlation between speed and distance 
#> [1] 0.8068949


# --------------------------------------------------------------------------------#
# ----------------------------Build Linear Model----------------------------------#
# --------------------------------------------------------------------------------#


linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)
#> Call:
#> lm(formula = dist ~ speed, data = cars)
#> 
#> Coefficients:
#> (Intercept)        speed  
#>     -17.579        3.932

# --------------------------------------------------------------------------------#
# ----------------------------Linear Regression Diagnostics-----------------------#
# --------------------------------------------------------------------------------#

summary(linearMod)  # model summary
#> Call:
#> lm(formula = dist ~ speed, data = cars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -29.069  -9.525  -2.272   9.215  43.201 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
#> speed         3.9324     0.4155   9.464 1.49e-12 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 15.38 on 48 degrees of freedom
#> Multiple R-squared:  0.6511, Adjusted R-squared:  0.6438 
#> F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12


##### ==================================================== Linear Regression ============================= #####################################################################
##### ==================================================== Linear Regression ============================= #####################################################################


##### ==================================================== Logistic Regression ============================= #####################################################################
##### ==================================================== Logistic Regression ============================= #####################################################################

#### --------------- install packages ----------------- ####

## please install these packages below before you begin your logistic regression parts

install.packages("DAAG")
install.packages("lattice")
install.packages("gdata")
install.packages("caTools")
install.packages("gplots")
install.packages("bitops")
install.packages("ROCR")

#### --------------- Loading packages ----------------- ####

## loading your packages or you will not run the code successfully in the logistic regression.

library(DAAG)
library(lattice)
library(gdata)
library(caTools)
library(gplots)
library(bitops)
library(ROCR)

#### --------------- review the top 6 rows of the dataset ----------------- ####
head(anesthetic)

#### --------------- conditional density plot ----------------- ####

## We can see the patient is close to motionless by the conc's increasing

cdplot(factor(nomove)~conc,data=anesthetic,main='conditional density',ylab='move',xlab='conc')

#### --------------- Logistic Regression --------------------- ####

anes1 <- glm(nomove~conc,family=binomial(link='logit'),data=anesthetic)
summary(anes1)

# Call:
#   glm(formula = nomove ~ conc, family = binomial(link = "logit"), 
#       data = anesthetic)

# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.76666  -0.74407   0.03413   0.68666   2.06900  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)   -6.469      2.418  -2.675  0.00748 **
#   conc           5.567      2.044   2.724  0.00645 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 41.455  on 29  degrees of freedom
# Residual deviance: 27.754  on 28  degrees of freedom
# AIC: 31.754
# 
# Number of Fisher Scoring iterations: 5

## Explanation1: The conc's cofficient is 5.567,and the intercept is -6.469
## Explanation2: You can get the regression model formula:
#                nomove = -6.469 + 5.567*conc

#### --------------- Write the ROC curve --------------------- ####

## Get the predicted results from the model based on the model "anes1" we runned above 
pre=predict(anes1,type='response')

## This function is used to transform the input data into a standardized format.
pred=prediction(pre,anesthetic$nomove)

## get the performance "tpr" & "fpr" from the model
perf=performance(pred,'tpr','fpr')

## Draw the ROC curve
plot(perf)
abline(a=0,b=1)

## Draw the Logistic Regression fit chart
x=seq(from=0,to=3,length.out=30)
y=predict(anes1,data.frame(conc=x),type='response')

anestot=aggregate(anesthetic[,c('move','nomove')],by=list(conc=anesthetic$conc),FUN=sum)
anestot$conc=as.numeric(as.character(anestot$conc))
anestot$total=apply(anestot[,c('move','nomove')],1,sum)
anestot$prop=anestot$nomove/anestot$total

plot(y~x,type = "l",main='Logistic Regression Curve',ylab='Probability of patient not move',xlab='volume of anesthesia')
points(anestot$prop ~ anestot$conc,col='red')

##### ==================================================== Logistic Regression ============================= #####################################################################
##### ==================================================== Logistic Regression ============================= #####################################################################


##### ==================================================== Decision Tree ============================= #####################################################################
##### ==================================================== Decision Tree ============================= #####################################################################

#### --------------- install packages ----------------- ####

## please install these packages below before you begin your logistic regression parts

install.packages("rpart")    
install.packages("partykit")
install.packages("rpart.plot")
install.packages("party")

#### --------------- Loading packages ----------------- ####

## loading your packages or you will not run the code successfully in the logistic regression.
library(rpart)    
library(partykit)
library(rpart.plot)
library(party)

## View the dataset
View(kyphosis)
help(kyphosis)
head(kyphosis)


## Set the formular

tree <- Kyphosis~Age + Number + Start

## Set ct:complexity pamemeter
## rpart.control: Set a few parameter for the tree,the parameter as below.
## minsplit:minimum number of node,20 means the tree will stop split when the node great than 20.
## xval:number of cross validation
## cp:complexity parameter,指某个点的复杂度，对每一步拆分,模型的拟合优度必须提高的程度

ct <- rpart.control(xval=10, minsplit=20, cp=0.1) 

## Decision Modeling
fit <- rpart(tree,data=kyphosis,method="class",control=ct,  
             parms = list(prior = c(0.65,0.35), split = "information")); 

summary(fit)

## View the importance of each variables
fit$variable.importance

## View the cp for each classify ste
fit$cp

## Draw the tree, method 1
rpart.plot(fit,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="decision tree")

# fit.pru<-prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]) 
# fit.pru$cp
# 
# rpart.plot(fit.pru,branch=1, extra=106, under=TRUE, faclen=0,
#            cex=0.8, main="决策树")

## Draw the tree, method 2
plot(as.party(fit), tp_args = list(id = FALSE), main="decision tree")

## How to read the decision tree results

##### ==================================================== Time Series ============================= #####################################################################
##### ==================================================== Time Series ============================= #####################################################################

install.packages("tseries")

library(tseries)

data(AirPassengers)

## View the data type
class(AirPassengers)
# [1] "ts"

## View the start time
start(AirPassengers)
# [1] 1949 1

## View the end time
end(AirPassengers)
# [1] 1960 12

## View frequency of the model
frequency(AirPassengers)
# [1] 12

#The number of passengers are distributed across the spectrum
summary(AirPassengers)
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# 104.0 180.0 265.5 280.3 360.5 622.0

## Draw the time series chart
plot(AirPassengers)

## fit a straight line 
abline(reg=lm(AirPassengers~time(AirPassengers)))

## get the cycle of the raw time series data
cycle(AirPassengers)

## Draw the plot of the average Passengers for each year
plot(aggregate(AirPassengers,FUN=mean))

## Draw the boxplot of the Passengers vs cycle
boxplot(AirPassengers~cycle(AirPassengers))

## Do the adf test for the AirPassengers data
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

## ACF plot
acf(log(AirPassengers))

## ACF abd PACF after do the "difference",acf and pacf used for decied the parameter of the ARMA model.
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

fit <- arima(log(AirPassengers), c(0,1,1),seasonal = list(order = c(0,1,1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))
##### ==================================================== Time Series ============================= #####################################################################
##### ==================================================== Time Series ============================= #####################################################################

##### ==================================================== After Training ============================= #####################################################################

##  package "alr3" contains a lot of data for a few regression models, you can choose one dataset for practice.

install.packages("alr3")
libary(alr3)

## list all the dataset in your loaded packages
data()
data(package = .packages(all.available = TRUE))

# --------------------------------------------------------------------------------#
# ----------------------------homework session------------------------------------#
# --------------------------------------------------------------------------------#

# Linear Regression
# datasest used: mtcars

##### ==================================================== After Training ============================= #####################################################################

