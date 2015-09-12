credit <- read.csv("Credit.csv", header = T)
attach(credit)
credit <- credit[-1]
names(credit)
lm1 <- lm(Balance~Limit+Age, credit)
summary(lm1)
#Coefficient of model
estimate_lm1 <- coef(summary(lm1))[,"Estimate"] 
coef_lm1 <- coef(summary(lm1))[,1] 
#Standard Error
std_err <- coef(summary(lm1))[, "Std. Error"]
se_lm1 <- coef(summary(lm1))[, 2]
#T value of model
t_lm1 <- coef(summary(lm1))[,3]
#Pr value
pr_lm1 <- coef(summary(lm1))[,4]
#coeff_range
coef_max <- estimate_lm1 + (2*std_err)
coef_max <- estimate_lm1 - (2*std_err)
#Residual of model
resid(lm1)
#Predicted values of model
fitted(lm1)
#Evaluating the result of model
layout(matrix(1:4, 2, 2))
plot(lm1)
lm2 <- lm(Balance~Age + Rating + Limit,credit )
#Variance Inflation Factor is to predict the collinearity in the dataset
VIF(lm2)
