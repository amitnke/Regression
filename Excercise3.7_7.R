#14 - a
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
##Y = 2X1+0.3X2 + e
## B0 = 2, B1 = 2, B2 = 0.3

#b - 
cor(x1, x2)

plot(x1, x2)

lm.fit = lm(y~x1+x2)
summary(lm.fit)
#ß0=2.0533,ß1=1.6336,ß3=0.5588
#The regression coefficients are close to the true coefficients, although with high standard error. We can reject the null hypothesis for ß1 because its p-value is below 5%.
#We cannot reject the null hypothesis for ß2 because its p-value is much above the 5% typical cutoff, over 60%.
#14)d - 
lm.fit = lm(y~x1)
summary(lm.fit)
#ß0=2.1124,ß1=1.9759
#RSE = 1.055 and P  < 1 and , we can reject null hypothesis. 
#14). e - 
lm.fit = lm(y~x2) 
summary(lm.fit)
#Yes we can reject as low p value and high F static. 
#f. No, because x1 and x2 have collinearity, it is hard to distinguish their effects when regressed upon together. 
#When they are regressed upon separately, the linear relationship between y and each predictor is indicated more clearly.

#g - 
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)
lm.fit1 <- lm(y~x1+x2)

summary(lm.fit1)
#Null hypothesis is rejected for x2 instead of x1. 
lm.fit2 <- lm(y~x1)
summary(lm.fit2)
#Null hypothesis is rejected for x1 as well. 
lm.fit3 <- lm(y~x2)
summary(lm.fit3)
#Null hypothesis is rejected for x2. 
#In the first model, it shifts x1 to statistically insignificance and 
#shifts x2 to statistiscal significance from the change in p-values between the two linear regressions.
par(mfrow=c(2,2))
plot(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit3)
##In the first and third models, the point becomes a high leverage point. 
plot(predict(lm.fit1), rstudent(lm.fit1))
plot(predict(lm.fit2), rstudent(lm.fit2))
plot(predict(lm.fit3), rstudent(lm.fit3))
#Looking at the standerized residuals, we don't observe point too far from the [3] value cutoff. 
expect for the second linear regression: y~x1.  

#15. 
library(MASS)
summary(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston)
attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn)#yes
lm.indus = lm(crim~indus)
summary(lm.indus)#yes
## [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"     "dis"     "rad"     "tax"     "ptratio"
#[12] "black"   "lstat"   "medv"  
lm.chas = lm(crim~chas)
summary(lm.chas)#no
lm.nox = lm(crim~nox)
summary(lm.nox)#yes
lm.rm = lm(crim~rm)
summary(lm.rm)#yes
lm.age = lm(crim~age)
summary(lm.age)#yes
lm.dis = lm(crim~dis)
summary(lm.dis)#yes
lm.rad = lm(crim~rad)
summary(lm.rad)#yes
lm.tax = lm(crim~tax)
summary(lm.tax)#yes
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio)#yes
lm.black = lm(crim~black)
summary(lm.black)#yes
lm.lstat = lm(crim~lstat)
summary(lm.lstat)#yes
lm.medv = lm(crim~medv)
summary(lm.medv)#yes
#All, except chas. Plot each linear regression using “plot(lm)” to see residuals.
#15b. 
lm.all = lm(crim~., data = Boston)
summary(lm.all)
#zn, dis, rad, black, medv
#15. c
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]

plot(x,y)
#coefficient for nox is approximately -10 in univariate model and 31 in multiple regression model.
#15-d.
lm.zn = lm(crim~poly(zn, 3))
summary(lm.zn)#yes 1, 2
lm.indus = lm(crim~poly(indus,3))
summary(lm.indus)#yes 1, 2, 3
lm.chas = lm(crim~poly(chas))##Qualitative predictor
#############
lm.nox = lm(crim~poly(nox,3))
summary(lm.nox)#yes 1,2, 3
lm.rm = lm(crim~poly(rm,3))
summary(lm.rm)#yes 1,2
lm.age = lm(crim~poly(age,3))
summary(lm.age)#yes 1,2,3
lm.dis = lm(crim~poly(dis,3))
summary(lm.dis)#yes 1,2,3
lm.rad = lm(crim~poly(rad,3))
summary(lm.rad)#yes 1,2
lm.tax = lm(crim~poly(tax,3))
summary(lm.tax)#yes 1,2,3
lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio)#yes 1,2, 3
lm.black = lm(crim~poly(black,3))
summary(lm.black)#yes 1
lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat)#yes 1, 2
lm.medv = lm(crim~poly(medv,3))
summary(lm.medv)#yes 1,2,3
#See inline comments above, the answer is yes for most, except for black and chas.



