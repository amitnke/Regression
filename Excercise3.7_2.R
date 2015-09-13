
Auto = read.csv("/Users/amitkumar/R/data/Auto.csv", header = T, na.strings="?")
Auto <- na.omit(Auto)
attach(Auto)
names(Auto)
#9. a
plot(Auto) # pairs(Auto)
#9. b - matrix of correlation among quantitative variables
str(Auto)
cor(Auto[-9])
#Multi linear regression excluding name varibale
lm2 <- lm(mpg ~ . , data = Auto[-9])
summary(lm2)
#i. yes, these is a relationship between the predictor and the response by testing the null hypothesis of whether all regression coefficients are zero. 
#The F-static is far from 1(with a small p value). 
#ii. Looking at the p-values associated with each predictor’s t-statistic, we see that displacement, weight, year, and origin have a statistically significant relationship, while cylinders, horsepower, and acceleration do not.
#iii. The regression coefficient for year, 0.7508, suggests that for every one year, mpg increases by the coefficient. In other words, cars become more fuel efficient every year by almost 1 mpg / year.
#9.d. 