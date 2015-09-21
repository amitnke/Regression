#####################
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
summary(wine)
#178 records and 14 columns 
str(wine)
head(wine)
#There is one raw per wine sample that contains data on concentration of 13 different chemicals in wine, derived from 3 different cultivars. 
#####################
##Plotting Multivariate Data##
library(car)
#“scatterplotMatrix()” function to plot the multivariate data.
wine[2:6]
scatterplotMatrix(wine[2:6])
#the diagonal cells show histograms of each of the variables
attach(wine)
plot(V4, V5)
#If we want to label the data points by their group (the cultivar of wine here), 
#we can use the “text” function in R to plot some text beside every data point. In this case, the cultivar of wine is stored in the column V1 of the variable “wine”, so we type:
text(wine$V4, wine$V5, wine$V1, cex=0.7, pos=4, col="red")
install.packages("RColorBrewer")
require(RColorBrewer)
#to make a profile plot of the concentrations of the first five chemicals in the wine samples (stored in columns V2, V3, V4, V5, V6 of variable “wine”)
names <- c("V2","V3","V4","V5","V6")
mylist <- list(V2,V3,V4,V5,V6)
makeProfilePlot <- function(mylist,names)
{
        require(RColorBrewer)
        # find out how many variables we want to include
        numvariables <- length(mylist)
        # choose 'numvariables' random colours
        colours <- brewer.pal(numvariables,"Set1")
        # find out the minimum and maximum values of the variables:
        mymin <- 1e+20
        mymax <- 1e-20
        for (i in 1:numvariables)
        {
                vectori <- mylist[[i]]
                mini <- min(vectori)
                maxi <- max(vectori)
                if (mini < mymin) { mymin <- mini }
                if (maxi > mymax) { mymax <- maxi }
        }
        # plot the variables
        for (i in 1:numvariables)
        {
                vectori <- mylist[[i]]
                namei <- names[i]
                colouri <- colours[i]
                if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
                else         { points(vectori, col=colouri,type="l")                                     }
                lastxval <- length(vectori)
                lastyval <- vectori[length(vectori)]
                text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
        }
}
makeProfilePlot(mylist,names)
