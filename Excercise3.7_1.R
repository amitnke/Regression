Auto = read.csv("/Users/amitkumar/R/data/Auto.csv", header = T, na.strings="?")
Auto <- na.omit(Auto)
attach(Auto)
str(Auto)
summary(Auto)
names(Auto)
lm1 <- lm(mpg~horsepower)
summary(lm1)
#High F-static(>1) and lower p-value(=0) depicts the strong relationship between Predictor and response. 
?lm
names(lm1)
#To calculate the residual error relative to the response we use the mean of the response and the RSE. 
#The mean of mpg is 23.4459. 
#The RSE of the lm.fit was 4.906 which indicates a percentage error of 20.9248%. 
#The R2 of the lm.fit was about 0.6059, meaning 60.5948% of the variance in mpg is explained by horsepower.
#iv). 
#What is the predicted mpg associated with a horsepower of 98?
predict(lm1, data.frame(horsepower=c(98)), interval="confidence")
#What are the associated 95% confidence and predicted interval
predict(lm1, data.frame(horsepower=c(98)), interval="prediction")

#b). 
plot(horsepower, mpg)#Plot response and predictor
abline(lm.fit)#Plot least square regression line
#c). 
par(mfrow=c(2,2))#created the frame 2,2
plot(lm1)
#Based on the residuals plots, there is some evidence of non-linearity.
