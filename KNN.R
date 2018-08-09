accuracy <- function(table){
  return((table[1,1] + table[2,2]) / (table[1,1] + table[1,2] + table[2,1] + table[2,2]))
}
sensitivy <- function(table){
  return((table[2,2]) / (table[1,2] + table[2,2]))
}
specificity <- function(table){
  return((table[1,1]) / (table[1,1] + table[1,2]))
}
F1 <- function(table){
  return(((2 * sensitivy(table) * specificity(table)) / (sensitivy(table) + specificity(table))))
}

#read csv
#dimension reduction
olympics<-read.csv("C:\\Users\\Smera\\Documents\\DirR\\olympics\\finalproj.R\\olympics.csv", stringsAsFactors = FALSE)
olympics<-olympics[,-c(1,7,9,12)]
olympicsOmit<-olympics
#lose as 0 and won as a 1 
olympicsOmit$Medal[which(is.na(olympicsOmit$Medal))]<-0
olympicsOmit$Medal[which(!(olympicsOmit$Medal==0))]<-1
olympicsOmit <- na.omit(olympicsOmit)
table(olympicsOmit$Medal)
#Creating a new dataframe with even quantities of wins and losses to optimise our results 
subsetLoser<-subset(olympicsOmit, olympicsOmit$Medal==0)
table(subsetLoser$Medal)
#import dplyr 
library(dplyr)
#even number 
logOlympics<-sample_n(subsetLoser, 30181)
subsetWinner<-subset(olympicsOmit, olympicsOmit$Medal==1)
glmOlympics<-rbind(logOlympics, subsetWinner)


#splitting into test and train
splitOlympics<-glmOlympics[,-c(1,2,6:10)]
library(caTools)
set.seed(100)
splitOlympics$Age<-as.numeric(splitOlympics$Age)
splitOlympics$Height<-as.numeric(splitOlympics$Age)
split <- sample.split(splitOlympics$Age, SplitRatio = 0.7)
knnTrain <- subset(splitOlympics, split == TRUE)
knnTest <- subset(splitOlympics, split == FALSE)
#View(knnTest)
library(class)
knnMatrix<-table(knnTrain$Medal)
cl <- factor(c(rep("0",knnMatrix[1]), rep("1",knnMatrix[2])))
library(NbClust)
library(factoextra)
knn<-knn(knnTrain, knnTest, cl, k = 2, l = 0, prob = FALSE, use.all = TRUE)
predKnn<-table(knnTest$Medal, knn)
accuracyKnn<-accuracy(predKnn)
F1Knn<-F1(predKnn)
sesitivityKnn<-sensitivy(predKnn)
specificityKnn<-specificity(predKnn )

#Finding Area under the ROC curve
roc<-roc(as.numeric(knn), as.numeric(knnTest$Medal))
plot(roc)
auc<-auc(roc)

write.csv(knnTest, file="knnTest.csv", row.names = FALSE, na="")
write.csv(knnTrain, file="knnTrain.csv", row.names = FALSE, na="")


