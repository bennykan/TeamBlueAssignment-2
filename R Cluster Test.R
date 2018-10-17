library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(corrgram)
library(GGally)
library(ggthemes) 
library(DMwR)
library(gridExtra)
library(rattle)
library(randomForest)
library(fpc)


data_elect <- read.csv("C:/Users/tblakeley/Documents/YorkU/Assignment 2/electiondata.csv",header = TRUE, na.strings = c("NA","","#NA","?"))


str(data_elect)
summary(data_elect)

colSums(is.na(data_elect))

list_of_numcols = sapply(data_elect, is.numeric)
numcols = data_elect[ , list_of_numcols]





elect.stand <- scale(numcols)  # To standarize the variables

# K-Means
k.means.fit <- kmeans(elect.stand, 20) # k = 3



o=order(k.means.fit $cluster)
data.frame(data_elect$county_name[o],k.means.fit $cluster[o])


plot(data_elect$Clinton, data_elect$Trump, type="n", xlim=c(0,1), xlab="Clinton", ylab="Trump")
text(x=data_elect$Clinton, y=data_elect$Trump, labels=data_elect$county_name,col=k.means.fit $cluster+1)



#In k.means.fit are contained all the elements of the cluster output:
attributes(k.means.fit)

# Centroids:
k.means.fit$centers

# Clusters:
k.means.fit$cluster

# Cluster size:
k.means.fit$size

#A fundamental question is how to determine the value of the parameter k. 
#If we looks at the percentage of variance explained as a function of the number of clusters: 
#One should choose a number of clusters so that adding another cluster doesn’t give much better 
#modeling of the data. More precisely, if one plots the percentage of variance explained by the 
#clusters against the number of clusters, the first clusters will add much information 
#(explain a lot of variance), but at some point the marginal gain will drop, 
#giving an angle in the graph. The number of clusters is chosen at this point, hence the “elbow criterion”.

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(elect.stand, nc=50) 


library(cluster)
clusplot(elect.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

