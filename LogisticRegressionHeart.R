heart = read.table("/Users/amitkumar/R/data/heart.data", sep = ",", header = T)
names(heart)
heart = heart[-c(1, 5, 7)]
heartfit = glm(chd~., data = heart, family=binomial)
summary(heartfit)
