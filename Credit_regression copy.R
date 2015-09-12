credit = read.csv("Credit.csv", header = T)
attach(credit)
names(credit)
credit <- credit[-1]
str(credit)
lm1 <- lm(Balance~Income+Student, credit)
summary(lm1)

credit$isi <- unclass(Student) * Income
isi
names(credit)
lm2 <- lm(Balance~Income+Student+isi, credit)
summary(lm2)