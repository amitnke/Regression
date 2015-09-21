library(ISLR)
data(package= "ISLR")
lm1 <- glm(default~balance, family=binomial)

lm2 <- glm(default~student, family=binomial)

lm3 <- glm(default ~ balance + income + student, family = binomial)
