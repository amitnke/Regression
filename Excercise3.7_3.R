#10
library(ISLR)
attach(Carseats)
names(Carseats)
#a
lm1 <- lm(Sales ~ Price + Urban + US)
summary(lm1)
#bThe linear regression suggests a relationship between price and sales given the low p-value of the t-statistic. The coefficient states a negative relationship between Price and Sales: as Price increases, Sales decreases.
#The linear regression suggests that there isn’t a relationship between the location of the store and the number of sales based on the high p-value of the t-statistic.
#The linear regression suggests there is a relationship between whether the store is in the US or not and the amount of sales. The coefficient states a positive relationship between USYes and Sales: if the store is in the US, the sales will increase by approximately 1201 units.
Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes
#Price and USYes, based on the p-values, F-statistic, and p-value of the F-statistic.
lm2 = lm(Sales ~ Price + US)
summary(lm2)
#Based on the RSE and R^2 of the linear regressions, they both fit the data similarly, with linear regression from (e) fitting the data slightly better.
confint(lm2)#95% confidence intervals for the coefficients
#evidence of outliner or high leverage observations in the model
plot(predict(lm2), rstudent(lm2))
#All studentized residual appears to be bounded by (-3, 3), So no potentials outliners are suggested from the linear regression. 
par(mfrow=c(2,2))
plot(lm2)
#There are few observations that greatly exceeds (p+1)/n(0.0076) on the leveage-statis plot that suggest that the corresponding points have high leveage. 
