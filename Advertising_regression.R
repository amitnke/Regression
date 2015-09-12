advertising = read.csv("Advertising.csv", header = T)

# estimate b1 manually

b1 = cov(advertising$TV, advertising$Sales)/var(advertising$TV)
#covarience of one variable is squared sum of difference of variable and mean(varaiable)
#covarience of two variable is multiplication of difference of both the variable and corresponding mean of that variable. 


#b1 = 0.04753664

# estimate b0 manually

b0 <- mean(advertising$Sales) - b1*mean(advertising$TV)

#b0 = 7.032594

# calculate the advertising of launch data

xmean <- mean(advertising$TV)

xdenom <- (advertising$TV-xmean)*(advertising$TV-xmean)

xdenom <- sum(xdenom)

RSS_term <- (advertising$Sales - b0 - (b1*advertising$TV))*(advertising$Sales - b0 - (b1*advertising$TV))

#Residual Sum Square

RSS <- sum(RSS_term)
#Residual Sum Square is sum of square of difference of each individual y and mean(y). 
#µ is the population mean of random variable Y. 
#
#Var(ˆµ) = SE(ˆµ)2 ˆµ is the estimate of µ. =s2/n
#where s is the standard deviation of each of the realizations yi of Y

#Resudual Standard Error

RSE <- sqrt(RSS/(nrow(advertising) - 2))

#Standard deviation of each of the realization of yi of Y. 

sigma_square <- RSE

#Standard Error of B0

a <- 1/nrow(advertising)

b <- (xmean*xmean)/xdenom

SE_b0 <- sigma_square * (a + b)

SE_b0 <- sqrt(SE_b0)

#Sandard error of b1

SE_b1 <- sigma_square/xdenom 

SE_b1 <- sqrt(SE_b1)        

#95% confidence interval for b1 (0.042, 0.053)

#lower limit

lb1 <- b1 - (2*SE_b1)

#Upper limit

ub1 <- b1 + (2*SE_b1)

#95% confidence interval of b0 (6.525338, 7.53985)

lb0 <- b0 - (2*SE_b0)

ub0 <- b0 + (2*SE_b0)

#Therefore we can conclude that in the absence of any sale, sale will on average 

# fall somewhere between 6525 and 7539. Furthermore, increase in each $1000 in TV advertising will increase the sale between 

# 42 to 53 unit. 

#t-Statistics - Gives the number of standard distribution that b1 is

#away from 0.

t1 <- b1/SE_b1

t0 <- b0/SE_b0

mean_Sale <- mean(advertising$Sales)

#Percentage error of the model- lack of fit of the model

Percent_error <- (RSE/mean_Sale)*100

?

TSS <- sum((advertising$Sales - mean_Sale)^2)

R2 <- 1 - (RSS/TSS)

?

#Likely R2, correlation is also linear relationship between X and Y

corr <- cov(advertising$TV, advertising$Sales) /

       (sd(advertising$TV) * sd(advertising$Sales))

#or

cor(advertising$TV, advertising$Sales)

#corr = 0.7822244

#Linear regression model
attach(advertising)
plot(Sales~TV, advertising)
ad_model1 = lm(Sales~TV, data = advertising)
ad_model1
summary(ad_model1)
abline(ad_model1, col = "red")
names(ad_model1)
ad_model1$coefficients
ad_model1$residuals
#In the case of the advertising data, the 95% confidence interval for ß0
#is [6.130, 7.935] and the 95% confidence interval for ß1 is [0.042, 0.053].
confint(ad_model1)
predict(ad_model1,data.frame(lstat=c(5,10,15)),interval="confidence")
plot(ad_model1)
### Multiple linear regression
names(advertising)
ad_model2 = lm(Sales~TV+Radio, data = advertising)
summary(ad_model2)
plot(ad_model2)

advertising$X <- NULL
ad_model3 = lm(Sales~.,data = advertising)
summary(ad_model3)
par(mfrow=c(2,2))
layout(matrix(1:9, 3, 3))
plot(ad_model3)






