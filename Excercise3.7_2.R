
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
par(mfrow=c(2,2))
plot(lm2)
#This plot does n't appear to be accurate
#because there is a discernible curve pattern to the residuals plots. From the leverage plot, point 14 appears to have high leverage, although not a high 
#magnitude residual.
plot(predict(lm2), rstudent(lm2))
#There are possible outliner as seen in the plot of studentlized residuals because
# there are data with a value greater than 3. 
#9e. 
lm.fit2 <- lm(mpg~cylinders*displacement + displacement*weight)
summary(lm.fit2)
#From the correlation matrix, I obtained the two highest correlated pairs and used them in picking my interaction effects. 
#From the p-values, we can see that the interaction between displacement and weight is statistically signifcant, while the interactiion between cylinders and displacement is not.
#9.f
lm.fit3 <- lm(mpg ~ log(weight) + sqrt(horsepower) + acceleration + I(acceleration^2))
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)
plot(predict(lm.fit3), rstudent(lm.fit3))
#Apparently, from the p-value, the og(weight), sqrt(horsepower), and acceleration^2 all have statistical significance of some sort. The residuals plot has less of a discernible pattern than the plot of all linear regression terms. The studentized residuals displays potential outliers (>3). The leverage plot indicates more than three points with high leverage.
#However, 2 problems are observed from the above plots: 1) the residuals vs fitted plot indicates heteroskedasticity (unconstant variance over mean) in the model. 2) The Q-Q plot indicates somewhat unnormality of the residuals.
#So, a better transformation need to be applied to our model. From the correlation matrix in 9a., displacement, horsepower and weight show a similar nonlinear pattern against our response mpg. This nonlinear pattern is very close to a log form. So in the next attempt, we use log(mpg) as our response variable.
#The outputs show that log transform of mpg yield better model fitting (better R^2, normality of residuals).
lm.fit2<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
plot(predict(lm.fit2),rstudent(lm.fit2))

The end
