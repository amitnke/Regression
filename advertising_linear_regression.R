advertising = read.csv("Advertising.csv", header = T)
# estimate b1 manually
b1 = cov(advertising$TV, advertising$Sales)/var(advertising$TV)
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

TSS <- sum((advertising$Sales - mean_Sale)^2)
R2 <- 1 - (RSS/TSS)

#Likely R2, correlation is also linear relationship between X and Y
corr <- cov(advertising$TV, advertising$Sales) /
       (sd(advertising$TV) * sd(advertising$Sales))
#or
cor(advertising$TV, advertising$Sales)
#corr = 0.7822244
#We can also conclude that R2 = corr*corr
#The squared correlation and the R^2 statistic are identical
# computing the slope using correlation
#b1 can be computed as 
corr * (sd(advertising$Sales) / sd(advertising$TV))
#0.04753664
# confirming the regression line using the lm function (not in text)
plot(advertising$Sales~advertising$TV)
mean.Sales = mean(advertising$Sales, na.rm = TRUE)
abline(h = mean.Sales)

model_advertising <- lm(Sales ~ TV, data = advertising)
model_advertising
abline(model_advertising, col = "red")


termplot(model_advertising)
summary(model_advertising)

cor(advertising[-c(1,5)])
summary(advertising[-c(1,5)])
pairs(advertising[-c(1,5)])
#Some more information
library(psych)
pairs.panels(advertising[-c(1,5)])
advertising <- advertising[-1]
attach(advertising)
model_advertising2 <- lm(Sales ~ ., data = advertising)
termplot(model_advertising2)
summary(model_advertising2)
#Interaction Effect
advertising$tvr <- TV*Radio
lm3 <- lm(Sales~TV + Radio + tvr, advertising)
summary(lm3)


