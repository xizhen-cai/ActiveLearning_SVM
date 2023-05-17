# R code to create figures in the paper "Active-Learning Class Activities and Shiny Applications for Teaching Support Vector Classifier"
# this is the file to create related figures for the admission dataset, for the pk dataset, see a different file. 
rm(list = ls()) #remove other preloaded datasets and environment

library(e1071) #load the package for svm function
mydata <- read.csv("admission.csv",header=TRUE) #load the dataset into R
mydata.trim <- mydata[1:59,] #create the trimmed dataset without the boundary admission cases

# Figure 2.1 display the trimmed admission dataset by different decision of admission
plot(mydata.trim[mydata.trim$decision==1,2:3],col="#000000",xlim=c(2,4),ylim=c(300,700),pch=19,cex=1)
points(mydata.trim[mydata.trim$decision==2,2:3],col="#D55E00",pch=19, cex=1)
legend("bottomright", col=c("#000000","#D55E00"), paste(c("Admit","Reject")),pch=19)

# Figure 2.2 display the trimmed admission dataset and several possible separating lines
plot(mydata.trim[mydata.trim$decision==1,2:3],col="#000000",xlim=c(2,4),ylim=c(300,700),pch=19,cex=1)
points(mydata.trim[mydata.trim$decision==2,2:3],col="#D55E00",pch=19, cex=1)
legend("bottomright", col=c("#000000","#D55E00"), paste(c("Admit","Reject")),pch=19)
abline(1203.757, -234.9349,lwd=2,col="#000000")
abline(1500,-350,lty=2,lwd=2,col="#000000")
abline(1580,-390,lty=3,lwd=2,col="#000000")
abline(1450,-345,lty=4,lwd=2,col="#000000")

# Figure 2.3 display the full admission dataset where the feature space cannot be separated by a straight line
# Also Figure 3 in the Class Activity Worksheet
plot(mydata[mydata$decision_binary==1,2:3],col="#000000",xlim=c(2,4),ylim=c(300,700),pch=19,cex=0.8)
points(mydata[mydata$decision_binary==0,2:3],col="#D55E00",pch=19,cex=0.8)
legend("bottomright", col=c("#000000","#D55E00"), paste(c("Admit","Reject")),pch=19,cex=0.8)

# Figure 3.10 display the full admission dataset, with the additional observations highlighted
plot(mydata[mydata$decision_binary==1,2:3],col="#000000",xlim=c(2,4),ylim=c(300,700),pch=19,cex=0.8)
points(mydata[mydata$decision_binary==0,2:3],col="#D55E00",pch=19,cex=0.8)
legend("bottomright", col=c("#000000","#D55E00"), paste(c("Admit","Reject")),pch=19,cex=0.8)
label_set <- mydata[60:64,2:3] #identify the dataset removed for the trimmed dataset
points(label_set,cex=2,col="#56B4E9",lwd=2,pch=1) #add points to the scatterplot
text(label_set+0.08,paste(c("1","2","3","4","5"))) #highlight the points using labels 1-5

# Figure 3.11 display the support vector classifiers with three different cost parameters based on the full admission dataaset
# Also Figure 4 & Figure 5 in the Class Activity Worksheet
par(mfrow=c(1,3), mar=c(4,4,4,4)) #reset the plotting region and margins
# create plot for cost parameter=10
plot(mydata[mydata$decision_binary==1,2:3],col="#000000",xlim=c(2,4),ylim=c(300,700),pch=19,cex=0.8,main="cost=10") 
points(mydata[mydata$decision_binary==0,2:3],col="#D55E00",pch=19,cex=0.8)
legend("bottomright", col=c("#000000","#D55E00"), paste(c("Admit","Reject")),pch=19,cex=0.8)
adm.data <- mydata[,c(2,3,5)] #subset to only have the relevant variables 
#fit a support vector classifier with cost parameter 10
svm.adm.c10 <- svm(as.factor(decision_binary)~.,data=adm.data,type="C-classification",kernel="linear",scale=FALSE,cost=10) 
cf.c10 <- coef(svm.adm.c10) #get the coefficients for the classifier
c0.c10 <- -cf.c10[1]/cf.c10[3] #calculate the intercept of the line to be added on top of feature space
c1.c10 <- -cf.c10[2]/cf.c10[3] #calculate the slope of the line to be added on top of feature space
abline(c0.c10,c1.c10,col="#56B4E9",lwd=2)
abline(-(cf.c10[1] + 1)/cf.c10[3], -cf.c10[2]/cf.c10[3], col = "#56B4E9",lty=2) #add line which is 1 margin above from the classifier
abline(-(cf.c10[1] - 1)/cf.c10[3], -cf.c10[2]/cf.c10[3], col = "#56B4E9",lty=2) #add line which is 1 margin below from the classifier

#create plot for cost parameter=1
plot(mydata[mydata$decision_binary==1,2:3],col="#000000",xlim=c(2,4),ylim=c(300,700),pch=19,cex=0.8,main="cost=1")
points(mydata[mydata$decision_binary==0,2:3],col="#D55E00",pch=19,cex=0.8)
legend("bottomright", col=c("#000000","#D55E00"), paste(c("Admit","Reject")),pch=19,cex=0.8)
adm.data <- mydata[,c(2,3,5)]
svm.adm.c1 <- svm(as.factor(decision_binary)~.,data=adm.data,type="C-classification",kernel="linear",scale=FALSE,cost=1)
cf.c1 <- coef(svm.adm.c1)
c0.c1 <- -cf.c1[1]/cf.c1[3]
c1.c1 <- -cf.c1[2]/cf.c1[3]
abline(c0.c1,c1.c1,col="#56B4E9",lwd=2)
abline(-(cf.c1[1] + 1)/cf.c1[3], -cf.c1[2]/cf.c1[3], col = "#56B4E9",lty=2)
abline(-(cf.c1[1] - 1)/cf.c1[3], -cf.c1[2]/cf.c1[3], col = "#56B4E9",lty=2)

#create plot for cost parameter=0.5
plot(mydata[mydata$decision_binary==1,2:3],col="#000000",xlim=c(2,4),ylim=c(300,700),pch=19,cex=0.8,main="cost=0.5")
points(mydata[mydata$decision_binary==0,2:3],col="#D55E00",pch=19,cex=0.8)
legend("bottomright", col=c("#000000","#D55E00"), paste(c("Admit","Reject")),pch=19,cex=0.8)
adm.data <- mydata[,c(2,3,5)]
svm.adm.c.5 <- svm(as.factor(decision_binary)~.,data=adm.data,type="C-classification",kernel="linear",scale=FALSE,cost=0.5)
cf.c.5 <- coef(svm.adm.c.5)
c0.c.5 <- -cf.c.5[1]/cf.c.5[3]
c1.c.5 <- -cf.c.5[2]/cf.c.5[3]
abline(c0.c.5,c1.c.5,col="#56B4E9",lwd=2)
abline(-(cf.c.5[1] + 1)/cf.c.5[3], -cf.c.5[2]/cf.c.5[3], col = "#56B4E9",lty=2)
abline(-(cf.c.5[1] - 1)/cf.c.5[3], -cf.c.5[2]/cf.c.5[3], col = "#56B4E9",lty=2)

# Figure 3.12 Display of the support vector classifier using cost parameter C0 = 10 based on the full admission dataset
par(mfrow=c(1,1))
plot(mydata[mydata$decision_binary==1,2:3],col="#000000",xlim=c(2,4),ylim=c(300,700),pch=19,cex=0.8,main="cost=10")
points(mydata[mydata$decision_binary==0,2:3],col="#D55E00",pch=19,cex=0.8)
legend("bottomright", col=c("#000000","#D55E00"), paste(c("Admit","Reject")),pch=19,cex=0.8)
cf.c10 <- coef(svm.adm.c10) #get the coefficients for the classifier
c0.c10 <- -cf.c10[1]/cf.c10[3] #calculate the intercept of the line to be added on top of feature space
c1.c10 <- -cf.c10[2]/cf.c10[3] #calculate the slope of the line to be added on top of feature space
abline(c0.c10,c1.c10,col="#56B4E9",lwd=2)
abline(-(cf.c10[1] + 1.05)/cf.c10[3], -cf.c10[2]/cf.c10[3], col = "#56B4E9",lty=2) #add line which is 1 margin above from the classifier
abline(-(cf.c10[1] - 1.05)/cf.c10[3], -cf.c10[2]/cf.c10[3], col = "#56B4E9",lty=2) #add line which is 1 margin below from the classifier

label_set.c10 <- mydata[as.numeric(rownames(svm.adm.c10$SV)),2:3] #identify the support vectors
points(label_set.c10[, 1:2], cex=1.5, col="#56B4E9",lwd=2, pch=1) #highlight the points 
text(label_set.c10+0.055,paste(c("1","2","3","4","5","6","7")))  
points(mydata[c(50,56),2:3],cex=1.5,col="#56B4E9",lwd=2,pch=1)
text(mydata[c(50,56),2]-0.07,mydata[c(50,56),3],paste(c("8","9")))

# Figure 3.12 Display of the support vector classifier using cost parameter C0 = 1 based on the full admission dataset
plot(mydata[mydata$decision_binary==1,2:3],col="#000000",xlim=c(2,4),ylim=c(300,700),pch=19,cex=0.8,main="cost=1")
points(mydata[mydata$decision_binary==0,2:3],col="#D55E00",pch=19,cex=0.8)
legend("bottomright", col=c("#000000","#D55E00"), paste(c("Admit","Reject")),pch=19,cex=0.8)
abline(c0.c1,c1.c1,col="#56B4E9",lwd=2)
abline(-(cf.c1[1] + 1)/cf.c1[3], -cf.c1[2]/cf.c1[3], col = "#56B4E9",lty=2)
abline(-(cf.c1[1] - 1)/cf.c1[3], -cf.c1[2]/cf.c1[3], col = "#56B4E9",lty=2)

label_set.c1 <- mydata[c(1,2,3,20,21,24,31,36,44,46,50,56,62:64),2:3]
points(label_set.c1 ,cex=2,col="#56B4E9",lwd=1.5,pch=1)
text(label_set.c1[-c(2,5,9,11,12),]+0.07,paste(c("1","3","4","6","7","8","10","13","14","15")))
text(label_set.c1[c(2,5,9),1],label_set.c1[c(2,5,9),2]+20,paste(c("2","5","9")))
text(label_set.c1[c(12),1],label_set.c1[c(12),2]-18,paste(c("12")))
text(label_set.c1[c(11),1],label_set.c1[c(11),2]+20,paste(c("11")))
