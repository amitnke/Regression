#Qualitative Predictor
fix(Carseats)
names(Carseats)
str(Carseats)
#Carseat contains ShelveLoc as Qualitative variable and R generated dummy variable automotically for this. 
lm.fir <- lm(Sales~.+Income:Advertising+Price:Age, data= Carseats)
summary(lm.fir)
#The contrasts() function returns the coding that R uses for the dummy variables. 
contrasts(ShelveLoc)

