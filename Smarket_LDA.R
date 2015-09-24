library(MASS)
train <- (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
lda.fit = lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
lda.fit
#
plot(lda.fit)
