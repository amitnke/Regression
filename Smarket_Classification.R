library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)# Error - x must be numeric
cor(Smarket[-9])#Produces a matrix that contains all of the pairwise correlations among the predictors in a dataset. 
# The first command below gives an error message because teh Directiob variable is qualitative. 
#As one would expect the correlation between the lag variable and today's return are close to zero. 
#The only substantial relationship is between Year and Volume. 
attach(Smarket)
plot(Year, Volume)
plot(Volume)
#############################Logistic Regression###############################
#Logistic regression to predict Direction using Lag1 through Lag5 and Volume.# 
glm.fit <- glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
summary(glm.fit)
#The smallest p value is associated to Lag1. The negative coefficient for this predictor suggest that if the market 
#had a positive erturn yesterday, then it less likely to go up today. 
names(glm.fit)
glm.fit$coefficients # or coef(glm.fit)
summary(glm.fit)$coefficients # summary(glm.fit)$coef
#To get p value of the model
summary(glm.fit)$coefficients[,4]
###Prediction() function can be used to predict Direction given predictors###
##The type="response" option tells R to output probabilities of the form P(Y = 1|X), 
#as opposed to other information such as the logit. 
glm.probs <- predict(glm.fit, type = "response")##Training data will be used for prediction if no data provided to prediction fucntion. 
contrasts(Direction)#It indicates that R has created a dummy ariable with 1 for up. 
#Commands to translate prediction into Up/Down
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"
#Confustion matrix to estimate the correct prediction
table(glm.pred, Direction)
mean(glm.pred==Direction)#The diagonal elements of the confusion matrix indicate correct predictions. 
#while the off-diagonals represent incorrect predictions. 
##############Dividing the training and test data#############################
train <- (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
#Smarket[train,] is the training data
glm.fit2 <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
                data=Smarket, family=binomial, subset = train)
glm.probs2 = predict(glm.fit2, Smarket.2005, type = "response")
dim(Smarket.2005)
glm.pred=rep("Down", 252)
glm.pred[glm.probs2 > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)##Gives set error rate
##Using predictors that have no no relationship with the response tends to 
# cause a deterioration in the test error rate. 
glm.fit3 <- glm(Direction~Lag1+Lag2, data = Smarket, family = binomial, subset = train)
glm.probs3 = predict(glm.fit3, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs3 > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
##################################################
predict(glm.fit3, newdata=data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")
