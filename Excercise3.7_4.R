set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)
lm.fit <- lm(y~x+0)
summary(lm.fit)
#a. The p-value of the t-statistic is near zero so the null hypothesis is rejected.
lm.fit2 <- lm(x~y+0)
#The p-value of the t-statistic is near zero so the null hypothesis is rejected.
#c. Both results in (a) and (b) reflect the same line created in 11a. In other words, y=2x+ϵ could also be written x=0.5∗(y−ϵ).
summary(lm.fit2)
lm1 <-lm(y~x) 
lm2 <- lm(x~y)
summary(lm1)
summary(lm2)
#You can see the t-statistic is the same for the two linear regressions
rm(list=ls())
#12 - When  the sum of the sum of the squares of the observed y-values are equal to the sum of the squares of the observed x-values. 
#12 - b
set.seed(1)
x = rnorm(100)
y <- 2*x
lm.fit <- lm(y~x+0)
lm.fit2 <- lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
#The regression coefficients are different for each linear regression. 
#12-c
set.seed(1)
rm(list=ls())
x <- rnorm(100)
y <- sample(x, 100)
sum(x^2)
sum(y^2)
lm.fit <- lm(y~x+0)
lm.fit2 <- lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
#The regression coefficients are the same for each linear regression. So long as sum sum(x^2) = sum(y^2) the condition in 12a. will be satisfied. Here we have simply taken all the xi in a different order and made them negative.

