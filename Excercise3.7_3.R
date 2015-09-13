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
