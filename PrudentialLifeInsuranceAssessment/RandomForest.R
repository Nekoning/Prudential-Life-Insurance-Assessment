install.packages("caret")
install.packages("mice")
install.packages("Hmisc")
install.packages("DMwR")

library(dplyr)
library(caret)
library(mice)
library(Hmisc)
library(DMwR)
library(XLConnect)
library(ggplot2)


trainData<- read.csv("train.csv")
testData<-read.csv("test.csv")
summary(trainData)
str(trainData)
str(testData)
#determining pattern of missing values in data
md.pattern(trainData)
md.pattern(testData)

#imputing blank values with mean of the column
trainData$Employment_Info_1 [is.na(trainData$Employment_Info_1)] <- mean(trainData$Employment_Info_1, na.rm=TRUE)
trainData$Employment_Info_4 [is.na(trainData$Employment_Info_4)] <- mean(trainData$Employment_Info_4, na.rm=TRUE)
trainData$Employment_Info_6 [is.na(trainData$Employment_Info_6)] <- mean(trainData$Employment_Info_6, na.rm=TRUE)
trainData$Insurance_History_5 [is.na(trainData$Insurance_History_5)] <- mean(trainData$Insurance_History_5, na.rm=TRUE)
trainData$Family_Hist_2 [is.na(trainData$Family_Hist_2)] <- mean(trainData$Family_Hist_2, na.rm=TRUE)
trainData$Family_Hist_3 [is.na(trainData$Family_Hist_3)] <- mean(trainData$Family_Hist_3, na.rm=TRUE)
trainData$Family_Hist_4 [is.na(trainData$Family_Hist_4)] <- mean(trainData$Family_Hist_4, na.rm=TRUE)
trainData$Family_Hist_5 [is.na(trainData$Family_Hist_5)] <- mean(trainData$Family_Hist_5, na.rm=TRUE)
trainData$Medical_History_1 [is.na(trainData$Medical_History_1)] <- mean(trainData$Medical_History_1, na.rm=TRUE)
trainData$Medical_History_10 [is.na(trainData$Medical_History_10)] <- mean(trainData$Medical_History_10, na.rm=TRUE)
trainData$Medical_History_15 [is.na(trainData$Medical_History_15)] <- mean(trainData$Medical_History_15, na.rm=TRUE)
trainData$Medical_History_24 [is.na(trainData$Medical_History_24)] <- mean(trainData$Medical_History_24, na.rm=TRUE)
trainData$Medical_History_32 [is.na(trainData$Medical_History_32)] <- mean(trainData$Medical_History_32, na.rm=TRUE)

testData$Employment_Info_1 [is.na(testData$Employment_Info_1)] <- mean(testData$Employment_Info_1, na.rm=TRUE)
testData$Employment_Info_4 [is.na(testData$Employment_Info_4)] <- mean(testData$Employment_Info_4, na.rm=TRUE)
testData$Employment_Info_6 [is.na(testData$Employment_Info_6)] <- mean(testData$Employment_Info_6, na.rm=TRUE)
testData$Insurance_History_5 [is.na(testData$Insurance_History_5)] <- mean(testData$Insurance_History_5, na.rm=TRUE)
testData$Family_Hist_2 [is.na(testData$Family_Hist_2)] <- mean(testData$Family_Hist_2, na.rm=TRUE)
testData$Family_Hist_3 [is.na(testData$Family_Hist_3)] <- mean(testData$Family_Hist_3, na.rm=TRUE)
testData$Family_Hist_4 [is.na(testData$Family_Hist_4)] <- mean(testData$Family_Hist_4, na.rm=TRUE)
testData$Family_Hist_5 [is.na(testData$Family_Hist_5)] <- mean(testData$Family_Hist_5, na.rm=TRUE)
testData$Medical_History_1 [is.na(testData$Medical_History_1)] <- mean(testData$Medical_History_1, na.rm=TRUE)
testData$Medical_History_10 [is.na(testData$Medical_History_10)] <- mean(testData$Medical_History_10, na.rm=TRUE)
testData$Medical_History_15 [is.na(testData$Medical_History_15)] <- mean(testData$Medical_History_15, na.rm=TRUE)
testData$Medical_History_24 [is.na(testData$Medical_History_24)] <- mean(testData$Medical_History_24, na.rm=TRUE)
testData$Medical_History_32 [is.na(testData$Medical_History_32)] <- mean(testData$Medical_History_32, na.rm=TRUE)

#writign data in a new file
write.csv(trainData, file="trainData.csv")
write.csv(testData, file="testData.csv")

#reading the new file
trainDataNew<- read.csv("trainData.csv")
testDataNew<- read.csv("testData.csv")





#Decision tree
library(rpart)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

decisiontree <- rpart(Response~.,data=trainDataNew,
             method="class")
plot(decisiontree)
text(decisiontree)

Prediction <- predict(decisiontree, testDataNew, type = "class")
submit <- data.frame(Id = testDataNew$Id, Response = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
myf<-read.csv("myfirstdtree.csv")

## for the same tree but with prune
decisiontree <- rpart(Response~.,data=trainDataNew,
                      method="class")
pdecisiontree<-prune(decisiontree,cp= decisiontree$cptable[which.min(decisiontree$cptable[,"xerror"]),"CP"])
Prediction <- predict(pdecisiontree, testDataNew, type = "class")
submit <- data.frame(Id = testDataNew$Id, Response = Prediction)
write.csv(submit, file = "myfirstdprunetree.csv", row.names = FALSE)
mytreeprune<-read.csv("myfirstdprunetree.csv")

## another example with decision tree
library(rpart)
fit <- rpart(Response~.,
             data=trainDataNew,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
Prediction <- predict(fit, testDataNew, type = "class")
submit <- data.frame(Id = testDataNew$Id, Response = Prediction)
write.csv(submit, file = "myfirstdtree2.csv", row.names = FALSE)
myf21<-read.csv("myfirstdtree2.csv")

# the same with prune 
fit <- rpart(Response~.,
             data=trainDataNew,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
ptree<-prune(fit,cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
Prediction1 <- predict(ptree, testDataNew, type = "class")
submit <- data.frame(Id = testDataNew$Id, Response = Prediction1)
write.csv(submit, file = "myfirstdtree2prune.csv", row.names = FALSE)
myfprune<-read.csv("myfirstdtree2prune.csv")
 
####### desicion tree 3
fit <- rpart(Response~.,
             data=trainDataNew,
             method="class",
             control=rpart.control(minsplit=2, cp=0.005))
library(rpart.plot)
 new.fit <- prp(fit,snip=TRUE)$obj
 Prediction <- predict(fit, testDataNew, type = "class")
 submit <- data.frame(Id = testDataNew$Id, Response = Prediction)
 write.csv(submit, file = "myfirstdtree3.csv", row.names = FALSE)
 myf2<-read.csv("myfirstdtree3.csv")
 
 #### with prune
 fit <- rpart(Response~.,
              data=trainDataNew,
              method="class",
              control=rpart.control(minsplit=2, cp=0.005))
 library(rpart.plot)
 new.fit <- prp(fit,snip=TRUE)$obj
 ptree<-prune(fit,cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
 Prediction <- predict(ptree, testDataNew, type = "class")
 submit <- data.frame(Id = testDataNew$Id, Response = Prediction)
 write.csv(submit, file = "myfirstdtree3prune.csv", row.names = FALSE)
 myf2prune<-read.csv("myfirstdtree3prune.csv")
 
#######


#RANDOM FOREST-Be aware if you change ntree it might take much time to run the program 
library(randomForest)

modelRan <- randomForest(Response~., data = trainDataNew, importance = TRUE, ntree = 3)

pred <- predict(modelRan, testDataNew)
sol <- data.frame(Id = testDataNew$Id, Response = as.integer(round(pred)))
write.csv(sol, file = "rfResult.csv", row.names = FALSE)
resultsRan<-read.csv("rfResult.csv")



#data cleaning


nominal_features <- c("Product_Info_1", "Product_Info_2", "Product_Info_3", 
                      "Product_Info_5", "Product_Info_6", "Product_Info_7", 
                      "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", 
                      "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", 
                      "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7",
                      "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", 
                      "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", 
                      "Insurance_History_9", "Family_Hist_1", "Medical_History_2", 
                      "Medical_History_3", "Medical_History_4", "Medical_History_5", 
                      "Medical_History_6", "Medical_History_7", "Medical_History_8", 
                      "Medical_History_9", "Medical_History_11", "Medical_History_12", 
                      "Medical_History_13", "Medical_History_14", "Medical_History_16", 
                      "Medical_History_17", "Medical_History_18", "Medical_History_19", 
                      "Medical_History_20", "Medical_History_21", "Medical_History_22", 
                      "Medical_History_23", "Medical_History_25", "Medical_History_26", 
                      "Medical_History_27", "Medical_History_28", "Medical_History_29", 
                      "Medical_History_30", "Medical_History_31", "Medical_History_33", 
                      "Medical_History_34", "Medical_History_35", "Medical_History_36", 
                      "Medical_History_37", "Medical_History_38", "Medical_History_39", 
                      "Medical_History_40", "Medical_History_41")
continuous_features <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", 
                         "Employment_Info_4", "Employment_Info_6", "Insurance_History_5", 
                         "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5")
discrete_features <- c("Medical_History_1", "Medical_History_10", "Medical_History_15", 
                       "Medical_History_24", "Medical_History_32")
InsuredInfo_nominal <- c("InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_4", 
                         "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7")
Insurance_History_nominal <- c("Insurance_History_1", "Insurance_History_2", "Insurance_History_3", 
                               "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", 
                               "Insurance_History_9")
Medical_History_nominal <- c("Medical_History_3", "Medical_History_4", "Medical_History_5", 
                             "Medical_History_6", "Medical_History_7", "Medical_History_8", 
                             "Medical_History_9", "Medical_History_11", "Medical_History_12", 
                             "Medical_History_13", "Medical_History_14", "Medical_History_16", 
                             "Medical_History_17", "Medical_History_18", "Medical_History_19", 
                             "Medical_History_20", "Medical_History_21", "Medical_History_22", 
                             "Medical_History_23", "Medical_History_25", "Medical_History_26", 
                             "Medical_History_27", "Medical_History_28", "Medical_History_29", 
                             "Medical_History_30", "Medical_History_31", "Medical_History_33", 
                             "Medical_History_34", "Medical_History_35", "Medical_History_36", 
                             "Medical_History_37", "Medical_History_38", "Medical_History_39", 
                             "Medical_History_40", "Medical_History_41")
for (feature in nominal_features) {
  trainData[, feature] <- as.factor(trainData[, feature])
}
for (feature in continuous_features) {
  trainData[is.na(trainData[, feature]), feature] <- median(trainData[, feature], na.rm = TRUE)
  trainData[, feature] <- as.numeric(trainData[, feature])
  
}
Mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
for (feature in discrete_features) {
  trainData[is.na(trainData[, feature]), feature] <- Mode(trainData[, feature])
  trainData[, feature] <- as.numeric(trainData[, feature])
  
}
####test data
for (feature in nominal_features) {
  testData[, feature] <- as.factor(testData[, feature])
}
for (feature in continuous_features) {
  testData[is.na(testData[, feature]), feature] <- median(testData[, feature], na.rm = TRUE)
  testData[, feature] <- as.numeric(testData[, feature])
  
}
Mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
for (feature in discrete_features) {
  testData[is.na(testData[, feature]), feature] <- Mode(testData[, feature])
  testData[, feature] <- as.numeric(testData[, feature])
  
}


table(trainData$Response)
table(trainData$Response)
str(testData)

#Decision################################################takes forever to run don't know if it is right
decisiontree1 <- rpart(Response~.,data=trainData,
                      method="class")
plot(decisiontree1)
text(decisiontree1)

Prediction <- predict(decisiontree1, testData, type = "class")
submit <- data.frame(Id = testData$Id, Response = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
myf<-read.csv("myfirstdtreeVersion1.csv")


