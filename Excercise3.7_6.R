#13). h
set.seed(1)
eps1 = rnorm(100, 0, 0.125)
x1 = rnorm(100)
y1 = -1 + 0.5*x1 + eps1
plot(x1, y1)
lm.fit1 = lm(y1~x1)
summary(lm.fit1)

## 
## Call:
## lm(formula = y1 ~ x1)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.29052 -0.07545  0.00067  0.07288  0.28665 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -0.9864     0.0113   -87.3   <2e-16 ***
## x1            0.4999     0.0118    42.2   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.113 on 98 degrees of freedom
## Multiple R-squared:  0.948,  Adjusted R-squared:  0.947 
## F-statistic: 1.78e+03 on 1 and 98 DF,  p-value: <2e-16



abline(lm.fit1, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
#We can see the average reduce in Residuals and Residual standard error. 

#13). i -
set.seed(1)
eps2 = rnorm(100, 0, 0.5)
x2 = rnorm(100)
y2 = -1 + 0.5*x2 + eps2
plot(x2, y2)
lm.fit2 = lm(y2~x2)
summary(lm.fit2)
## 
## Call:
## lm(formula = y2 ~ x2)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.1621 -0.3018  0.0027  0.2915  1.1466 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -0.9456     0.0452   -20.9   <2e-16 ***
## x2            0.4995     0.0474    10.6   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.451 on 98 degrees of freedom
## Multiple R-squared:  0.532,  Adjusted R-squared:  0.527 
## F-statistic:  111 on 1 and 98 DF,  p-value: <2e-16
#Increase in Residual and Residual Standard error
abline(lm.fit2, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
##13_. J - 
confint(lm.fit)
confint(lm.fit1)
confint(lm.fit2)

#All intervals seem to be centered on approximately 0.5, with the second fit’s interval being narrower than the first fit’s interval and the last fit’s interval being wider than the first fit’s interval.

