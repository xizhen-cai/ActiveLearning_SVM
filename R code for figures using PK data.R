# R code to create figures in the paper "Active-Learning Class Activities and Shiny Applications for Teaching Support Vector Classifier"
# this is the file to create related figures for the PK dataset, for the admission dataset, see a different file. 
rm(list = ls()) #remove other preloaded datasets and environment

library(e1071)  #load the package for svm function
PKdata <- read.csv("PKdata.csv") #load the dataset 
attach(PKdata) #attach the dataset

## Figure 3.1 Display of a preschool data set where the feature space is linearly separable
## Also Figure 1 in the Class Activity Worksheet 
plot(PKdata[Class=="PK1",-3],pch=19,col="#D55E00",cex=1.2,xlim=c(0,10),ylim=c(0,10),xlab="West-East",ylab="South-North")
points(PKdata[Class=="PK2",-3],pch=19,col="#000000",cex=1.2) #add points in a different class 
legend("bottomright",paste(c("PK1","PK2")),pch=19, col=c("#D55E00","#000000"),cex=1)

## Figure 3.2 Two candidate separating lines (in blue) given the preschool data set
## Also Figure 2 in the Class Activity Worksheet
# fit the maximum margin classifier to the PK data 
svm.fit <- svm(as.factor(Class)~.,data=PKdata,type="C-classification",kernel="linear",scale=FALSE,cost=1000) 
cf <- coef(svm.fit)
c0 <- -cf[1]/cf[3] #intercept of the classifier to be added to the feature space scatterplot
c1 <- -cf[2]/cf[3] #slope of the classifier to be added to the feature space scatterplot

par(mfrow=c(1,2))
plot(PKdata[Class=="PK1",-3],pch=19,col="#D55E00",cex=1.2,xlim=c(0,10),ylim=c(0,10),xlab="West-East",ylab="South-North")
points(PKdata[Class=="PK2",-3],pch=19,col="#000000",cex=1.2)
legend("bottomright",paste(c("PK1","PK2")),pch=19, col=c("#D55E00","#000000"),cex=1)
abline(12.5,c1+0.5,col="#56B4E9",lwd=2) #add a line which separates the two classes

plot(PKdata[Class=="PK1",-3],pch=19,col="#D55E00",cex=1.2,xlim=c(0,10),ylim=c(0,10),xlab="West-East",ylab="South-North")
points(PKdata[Class=="PK2",-3],pch=19,col="#000000",cex=1.2)
legend("bottomright",paste(c("PK1","PK2")),pch=19, col=c("#D55E00","#000000"),cex=1)
abline(11.5,c1,col="#56B4E9",lwd=2) #add a different line which separates the two classes

## Figure 3 in the Class Activity Solutions
# first find a function to find endpoint for a perpendicular segment 
# from the point (x0,y0) to the line with intercept a and slope b
perp.segment.coord <- function(x0, y0, a, b){
  x1 <- (x0+b*y0-a*b)/(1+b^2)
  y1 <- a + b*x1
  list(x0=x0, y0=y0, x1=x1, y1=y1)
}

par(mfrow=c(1,2))
plot(PKdata[Class=="PK1",-3],pch=19,col="#D55E00",cex=1.2,xlim=c(0,10),ylim=c(0,10),xlab="West-East",ylab="South-North")
points(PKdata[Class=="PK2",-3],pch=19,col="#000000",cex=1.2)
legend("bottomright",paste(c("PK1","PK2")),pch=19, col=c("#D55E00","#000000"),cex=1)
abline(12.5,c1+0.5,col="#56B4E9",lwd=2) # add the seperation line
set1 <- perp.segment.coord(svm.fit$SV[2,1],svm.fit$SV[2,2],12.5,c1+0.5) # add a segment from the support vector to the separation line
segments(set1$x0, set1$y0, set1$x1, set1$y1, col="#56B4E9", lty=3, lwd=2)
points(7.45,6.25,cex=2,col="#56B4E9",lwd=2,pch=1)

plot(PKdata[Class=="PK1",-3],pch=19,col="#D55E00",cex=1.2,xlim=c(0,10),ylim=c(0,10),xlab="West-East",ylab="South-North")
points(PKdata[Class=="PK2",-3],pch=19,col="#000000",cex=1.2)
legend("bottomright",paste(c("PK1","PK2")),pch=19, col=c("#D55E00","#000000"),cex=1)
abline(11.5,c1,col="#56B4E9",lwd=2) # add a different seperation line
set2 <- perp.segment.coord(3.2,3.5,11.5,c1) # add a segment from the support vector to the separation line
segments(set2$x0, set2$y0, set2$x1, set2$y1, col="#56B4E9", lty=3, lwd=2)
points(3.2,3.5,cex=2,col="#56B4E9",lwd=2,pch=1)
