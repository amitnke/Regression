setwd("C:/Users/c015062/Documents/R")
library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
attach(Boston)
lm.fit <- lm(medv~lstat)
#names() function in order to find out 
#other pieces of information are stored in lm.fit.
names(lm.fit)
#confint() command can be used obtain a confidence interval 
#for the coefficient estimates
confint(lm.fit)
#The predict() function can be used to produce confidence intervals and
#prediction intervals for the prediction of medv for a given value of lstat.
#Confidence interval gives a range of E[y|x]. whereas Prediction interval gives a 
#range of y itself. 

predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval = "confidence")
predict(lm.fit ,data.frame(lstat =(c(5 ,10 ,15))),interval = "prediction")

plot(lstat, medv)
#The lwd=3 command causes the width of the regression line to be increased by a factor of 3;
abline (lm.fit ,lwd =3)
abline (lm.fit ,lwd =3, col ="red")
plot(lstat ,medv ,col ="red")
plot(lstat ,medv ,pch =20)
plot(lstat ,medv ,pch ="+")
plot (1:20 ,1:20, pch =1:20)

#par() function splits the display screen into separate panels so that multiple plots can be viewed simultaneously.
par(mfrow =c(2,2))
plot(lm.fit)
#Alternatively, we can compute the residuals from a linear regression fit
#using the residuals() function. The function rstudent() will return the
#residuals() rstudent() studentized residuals, 
#and we can use this function to plot the residuals
#against the fitted values.
plot(predict(lm.fit), residuals(lm.fit))#resuduals
plot(predict(lm.fit), rstudent(lm.fit)) #standerized residuals
#Leverage statistics can be computed for any number of predictors using the
#hatvalues() function.
plot(hatvalues(lm.fit))
The which.max() function identifies the index of the largest element of a which.max(). 
which.max(hatvalues (lm.fit))

##############################
library(MASS)
library(ISLR)
attach(Boston)
lm.fit = lm(medv~lstat+age ,data=Boston)
#regression using all variable
lm.fitall = lm(medv~., data = Boston)
?summary.lm
names(summary(lm.fitall))
summary(lm.fitall)$r.squared
#The vif() function, part of the car package, can be used to compute variance inflation
#factors.
require(car)
#Instead of inspecting the correlation matrix, a better way to assess multi- collinearity
#is to compute the variance inflation factor (VIF). The VIF is the ratio of the variance of ˆßj 
#when fitting the full model divided by the variance of ˆßj if fit on its own. 
#The smallest possible value for VIF is 1, which indicates the complete absence of collinearity.
vif(lm.fitall)
summart(firall)
#In the above regression output, age has a high p-value.
#So we may wish to run a regression excluding this predictor. The following
#syntax results in a regression using all predictors except age.
lm.fit1=lm(medv~.-age ,data=Boston)
#it can also be done using update
lm.fit1=update(lm.fit , ~.-age)
#################################
#Interaction Term
#The syntax lstat:black tells R to include an interaction term between
#lstat and black.
#The syntax lstat*age simultaneously includes lstat, age,
#and the interaction term lstat×age as predictors; it is a shorthand for
#lstat+age+lstat:age.
summary (lm(medv~lstat*age ,data=Boston ))
##################################
#Non Linear transformation of the predictor
#given a predictor X, we can create a predictor X2
#using I(X^2)
lm.fit2 <- lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
#The near-zero p-value associated with the quadratic term suggests that
#it leads to an improved model. We use the anova() function to further
#quantify the extent to which the quadratic fit is superior to the linear fit.
lm.fit <- lm(medv~lstat)
anova(lm.fit, lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5 = lm(medv~poly(lstat,5))
summary(lm.fit5)
summary(lm(medv~log(rm), data = Boston))

##Qualitative Predictor



