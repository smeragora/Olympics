#so in the presentation I refered to the problem I had with the glm here is the old code that I ran into problems with
#my partner ran the most updated glm on their code so play around with the set seed if you get a new factors error
#making a vector to perform glm 
olympics<-read.csv("C:\\Users\\Smera\\Documents\\DirR\\check\\olympics.csv", stringsAsFactors = FALSE)
olympics<-olympics[,-c(1,7,9,12)]
olympics1NA<- olympics
olympics1NA$Medal[which(is.na(olympics1NA$Medal))]<-0
olympics1NA$Medal[which(!(olympics1NA$Medal==0))]<-1
olympics1NA <- na.omit(olympics1NA)
sum(is.na(olympics1NA$Weight))

olympics1NA$Name<-as.factor(olympics1NA$Name)
olympics1NA$Sex<-as.factor(olympics1NA$Sex)
olympics1NA$NOC<-as.factor(olympics1NA$NOC)
olympics1NA$Season<-as.factor(olympics1NA$Season)
olympics1NA$Sport<-as.factor(olympics1NA$Sport)
olympics1NA$Event<-as.factor(olympics1NA$Event)
class(olympics1NA$Season)
olympics1NA$Medal<-as.numeric(olympics1NA$Medal)



#splitting olympics into test and train data using CA tools 
library(caTools)
set.seed(100)
split <- sample.split(olympics1NA$Sport, SplitRatio = 0.7)
olympicsTrain <- subset(olympics1NA, split == TRUE)
olympicsTest <- subset(olympics1NA, split == FALSE)


#TODO: Creat a new data set from the original. retain all the ones in the new dataset 
olympicsTest[,c(3:5)]<-as.data.frame(scale(olympicsTest[,c(3:5)]))
olympicsTrain[,c(3:5)]<-as.data.frame(scale(olympicsTrain[,c(3:5)]))
log.model <- glm(Medal ~ Height+Sport+Sex+Age+NOC, family = binomial(), olympicsTrain)
prediction <- predict(log.model, olympicsTest, type = "response")
pred<-table(olympicsTest$Medal, prediction >= 0.5)
accuracy<-(52281+774)/(52281+774+454+8342)
sens<-(774)/(774+8342)
spec<-(52281)/(52281+454)

table(olympicsTest$Medal)
table(olympicsTrain$Medal)

cor(olympicsTest$Height, olympicsTest$Weight)
cor(olympicsTest$Age, olympicsTest$Weight)
unique(olympics$Sport)
unique(olympics$Event)
