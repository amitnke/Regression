# First we get our data.

mydata <- read.table("/Users/amitkumar/R/Data/POLS310DATA/panel80.txt")
# attach(mydata) # In case you want to work with the variable names directly
names(mydata) # This shows us all the variable names.
# options(scipen=20) # suppress "scientific" notation
options(scipen=NULL) # Brings things back to normal
# Now we want to make a binary response variable that has a vote for Carter=0 and a vote for Reagan=1. mysubsetdata1 <- subset(mydata, VOTE == 1 | VOTE==2) # This keeps only those respondents who actually voted for Carter or Reagan.
mysubsetdata1 <- subset(mydata, VOTE == 1 | VOTE == 2)# This keeps only those respondents who actually voted for Carter or Reagan
#This keeps only the variables that we are using.
#mysubsetdata2
# In VOTE, a vote for Reagan=1 and a vote for Carter=2. We change that in the loop below when we create the new variable binaryvote.
binaryvote <- 0 # This initializes the binaryvote variable.
for (i in 1:nrow(mysubsetdata2)) {
        if (mysubsetdata2$VOTE[i]==2) binaryvote[i]=0 else binaryvote[i]=1
}
# Here is another way to do the same thing in one line.
binaryvote2 <- ifelse((mysubsetdata2$VOTE == 1), 1, 0)
mysubsetdata3<-cbind(mysubsetdata2, binaryvote, binaryvote2)
mysubsetdata3
# Now we do the logistic regression.
library(MASS)
logitvote.model <- glm(binaryvote ~ INC + AGE + PARTYID, family=binomial, data=mysubsetdata3) 
summary(logitvote.model)
# Now we get the individual risk factors (or odds ratios).
round(exp(cbind(Estimate=coef(logitvote.model), confint(logitvote.model))), 2) # The 2 at the end is the number of decimals we want.
