home_data <- read.csv("~/R/data/home_data.csv", header = T)
fix(home_data)
names(home_data)
head(home_data)
str(home_data)
smp_size = floor(0.8*nrow(home_data))
set.seed(123)
train_ind = sample(seq_len(nrow(home_data)), size = smp_size)
train = home_data[train_ind,]
test = home_data[-train_ind,]
lm1 = lm(train$price~train$sqft_living, data = train)
summary(lm1)
