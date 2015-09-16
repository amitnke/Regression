#13). a. Create a vector x containing 100 observations drawn from N(0,1) distribution. 
set.seed(1)
x = rnorm(100)
#13). b. eps vector containing 100 observations drawn from N(0, 0.25). i.e, normal distribution with mean zero and variance 0.25
?rnorm# it takes sd as argument = sqrt(variance)
eps = rnorm(100, 1, sqrt(0.25))
#13). c. Y = -1 + 0.5X + eps
y = -1 + 0.5*x + eps
# y is of length 100, B0 = -1, B1 = 0.5
#13). d. scatterplot between x and y
plot(x, y)
#Linear relationship between x and y with a positive scope. 
#13). e - Fit a least square linear model 
lm.fit <- lm(y~x)
summary(lm.fit)
#The linear regression fits a model close to the true value of the coefficients as was constructed. The model
#has a large F-statistic with a near-zero p-value so the null hypothesis can be rejected.
#13). f - Display a least square line on the scatterplot - d. 
plot(x, y)
abline(lm.fit, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col = 3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

lm.fit_seq <- lm(y~x+I(x^2)
summary(lm.fit_seq)
#There is evidence that model fit has increased over the training data given the slight increase in R2 and RSE. Although, the p-value of the t-statistic suggests that there isn’t a relationship between y and x2.
abline(lm.fit_seq, lwd=3, col=4)
#13). h - 