library(dplyr)
library(caret)
library(mice)
library(Hmisc)
library(DMwR)
library(XLConnect)
library(ggplot2)
library(randomForest)
library(rpart)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

trainData<- read.csv("train.csv")
testData<- read.csv("test.csv")

#--Cleaning





#--------------------- data cleaning ---------------------

#class of the data
class(trainData)
class(testData)

#determining the dimensions of the data 
dim(trainData)
dim(testData)

#column names of the data
names(trainData)
names(testData)

#understandig the structure of the data
str(trainData)
str(testData)

#better version of data
glimpse(trainData)
glimpse(testData)

#view the top data
head(trainData)

#view the bottom data
tail(trainData)
tail(testData)

#details of data
summary(trainData)
summary(testData)

#determining pattern of missing values in data
md.pattern(trainData)
md.pattern(testData)

#scaling the data
#trainData<-preProcess(trainData, method=c("scale"))
#testData<-preProcess(testData, method=c("scale"))





######------------ Filling the missing data------------ #######
################---------Train data------------#################
#deciding variables
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

# Convert binary columns into 1-0
feat.bin <- c("Medical_Keyword_1", "Medical_Keyword_2", "Medical_Keyword_3", "Medical_Keyword_4", "Medical_Keyword_5",
              "Medical_Keyword_6", "Medical_Keyword_7", "Medical_Keyword_8", "Medical_Keyword_9", "Medical_Keyword_10",
              "Medical_Keyword_11", "Medical_Keyword_12", "Medical_Keyword_13", "Medical_Keyword_14", "Medical_Keyword_15",
              "Medical_Keyword_16", "Medical_Keyword_17", "Medical_Keyword_18", "Medical_Keyword_19", "Medical_Keyword_20",
              "Medical_Keyword_21", "Medical_Keyword_22", "Medical_Keyword_23", "Medical_Keyword_24", "Medical_Keyword_25",
              "Medical_Keyword_26", "Medical_Keyword_27", "Medical_Keyword_28", "Medical_Keyword_29", "Medical_Keyword_30",
              "Medical_Keyword_31", "Medical_Keyword_32", "Medical_Keyword_33", "Medical_Keyword_34", "Medical_Keyword_35",
              "Medical_Keyword_36", "Medical_Keyword_37", "Medical_Keyword_38", "Medical_Keyword_39", "Medical_Keyword_40",
              "Medical_Keyword_41", "Medical_Keyword_42", "Medical_Keyword_43", "Medical_Keyword_44", "Medical_Keyword_45",
              "Medical_Keyword_46", "Medical_Keyword_47", "Medical_Keyword_48")

for (f in feat.bin) {
  levels <- unique(trainData[[f]])
  trainData[[f]] <- as.integer(factor(trainData[[f]], levels=levels))-1
}

################---------Test data------------#################
#deciding variables
Categorical_variables <- c("Product_Info_1", "Product_Info_2", "Product_Info_3", 
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

#creating dummy variable
for (feature in Categorical_variables) {
  testData[, feature] <- as.factor(testData[, feature])
}

#--------------------------------------------------------------------------------------------------------

#continuous variables
continuous_variables <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", 
                          "Employment_Info_4", "Employment_Info_6", "Insurance_History_5", 
                          "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5")

#Imputing continuous data with median
for (feature in continuous_variables) {
  testData[is.na(testData[, feature]), feature] <- median(testData[, feature], na.rm = TRUE)
  testData[, feature] <- as.numeric(testData[, feature])
  
}
#--------------------------------------------------------------------------------------------------------

#discrete variables
discrete_variables <- c("Medical_History_1", "Medical_History_10", "Medical_History_15", 
                        "Medical_History_24", "Medical_History_32")

#Calculating mode
Mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Imputing discrete data with median
for (feature in discrete_variables) {
  testData[is.na(testData[, feature]), feature] <- Mode(testData[, feature])
  testData[, feature] <- as.numeric(testData[, feature])
}


# Convert binary columns into 1-0
feat.bin <- c("Medical_Keyword_1", "Medical_Keyword_2", "Medical_Keyword_3", "Medical_Keyword_4", "Medical_Keyword_5",
              "Medical_Keyword_6", "Medical_Keyword_7", "Medical_Keyword_8", "Medical_Keyword_9", "Medical_Keyword_10",
              "Medical_Keyword_11", "Medical_Keyword_12", "Medical_Keyword_13", "Medical_Keyword_14", "Medical_Keyword_15",
              "Medical_Keyword_16", "Medical_Keyword_17", "Medical_Keyword_18", "Medical_Keyword_19", "Medical_Keyword_20",
              "Medical_Keyword_21", "Medical_Keyword_22", "Medical_Keyword_23", "Medical_Keyword_24", "Medical_Keyword_25",
              "Medical_Keyword_26", "Medical_Keyword_27", "Medical_Keyword_28", "Medical_Keyword_29", "Medical_Keyword_30",
              "Medical_Keyword_31", "Medical_Keyword_32", "Medical_Keyword_33", "Medical_Keyword_34", "Medical_Keyword_35",
              "Medical_Keyword_36", "Medical_Keyword_37", "Medical_Keyword_38", "Medical_Keyword_39", "Medical_Keyword_40",
              "Medical_Keyword_41", "Medical_Keyword_42", "Medical_Keyword_43", "Medical_Keyword_44", "Medical_Keyword_45",
              "Medical_Keyword_46", "Medical_Keyword_47", "Medical_Keyword_48")

for (f in feat.bin) {
  levels <- unique(testData[[f]])
  testData[[f]] <- as.integer(factor(testData[[f]], levels=levels))-1
}

#--------------------------------------------------------------------------------

#writing data in a new file
write.csv(trainData, file="trainData.csv")
write.csv(testData, file="testData.csv")

#reading the new file
trainDataNew<- read.csv("trainData.csv")
testDataNew<- read.csv("testData.csv")



############Decision Tree########################

library(forecast)
library(rpart)
library(XLConnect)


set.seed(123)

index<-sample(1:nrow(trainDataNew),size = 0.7*nrow(trainDataNew)) 

# subset weather to include only the elements in the index
train <-trainDataNew[index,] 

# subset weather to include all but the elements in the index
test <- trainDataNew[-index,] 

nrow(train)


nrow(test)

###########Baseline###########################################
best.guess <- mean(train$Response) 

RMSE.baseline <- sqrt(mean((best.guess-test$Response)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-test$Response))
MAE.baseline

######Regression Tree
rt <- rpart(Response ~., data=train, method="anova")

test.pred.rtree <- predict(rt,test) 


RMSE.rtree <- sqrt(mean((test.pred.rtree-test$Response)^2))

RMSE.rtree
MAE.rtree <- mean(abs(test.pred.rtree-test$Response))
MAE.rtree
printcp(rt)

min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]

min.xerror

rt.pruned <- prune(rt,cp = min.xerror) 
fancyRpartPlot(rt.pruned)

test.pred.rtree.p <- predict(rt.pruned,test)
RMSE.rtree.pruned <- sqrt(mean((test.pred.rtree.p-test$Response)^2))

RMSE.rtree.pruned

MAE.rtree.pruned <- mean(abs(test.pred.rtree.p-test$Response))
MAE.rtree.pruned

### matrix#############
confusion.matrix<-table(test$Response,test.pred.rtree.p)
accuracy.percent<-100*sum(diag(confusion.matrix)/sum(confusion.matrix))
## percent of Accuracy
print(accuracy.percent)


## Accuracy [1] 38.16447

##########Second Try######################################


# Set random seed. Don't remove this line.
set.seed(123)

n <- nrow(trainDataNew)
shuffled <- trainDataNew[sample(n),]
# The shuffled dataset is already loaded into your workspace

# Initialize the accs vector
accs <- rep(0,10)

for (i in 1:10) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/10)*nrow(shuffled))) + 1):((i*round((1/10) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Response ~ ., train, method = "anova")
  
  # Make a prediction on the test set using tree
  pred <- predict(tree, test, type = "vector")
  
  # Assign the confusion matrix to conf
  conf <- table(test$Response, pred)
  print(RMSE.rtree.second <- sqrt(mean((pred-test$Response)^2)))
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}

# 
print(accs)

#Accuracy 47



#Validation


set.seed(12)
split_model <- createDataPartition(y=trainDataNew$Response,    # Split 
                                   list = FALSE,      # Return indexes as a vector
                                   p=0.75,            # 75% of data in the training set
                                   times=1)           # Make 1 split

training_set <- trainDataNew[split_model,]     # Get the new training set
validation_set <- trainDataNew[-split_model,]  # Get the validation set

nrow(training_set)/nrow(trainDataNew)      # Check proportion in each partition
nrow(validation_set)/nrow(trainDataNew)

set.seed(12)

trainDataNew$Response <- as.factor(trainDataNew$Response) # Convert target to factor*

# Create a trainControl object to control how the train function creates the model
train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                              number = 10,             # Use 10 partitions
                              repeats = 2)             # Repeat 2 times

# Set required parameters for the model type we are using**
tune_grid = expand.grid(cp=c(0.001))


# Use the train() function to create the model
validated_tree <- train(Response ~ BMI+Ht+Wt+Ins_Age+Employment_Info_1+Employment_Info_3+Insurance_History_2+InsuredInfo_5
                        +InsuredInfo_6+InsuredInfo_7+Medical_History_4+Medical_History_5+Medical_History_6+Medical_History_13
                        +Medical_History_15+Medical_History_16+Medical_History_17+Medical_History_18+Medical_History_20
                        +Medical_History_23+Medical_History_27+Medical_History_30+Medical_History_35+Medical_History_39
                        +Medical_History_40+Medical_History_41+Medical_Keyword_3+Medical_Keyword_48+Product_Info_2
                        +Product_Info_4+Product_Info_7,
                        data=training_set,                 # Data set
                        method="rpart",                     # Model type(decision tree)
                        trControl= train_control,           # Model control options
                        tuneGrid = tune_grid,               # Required model parameters
                        maxdepth = 5,                       # Additional parameters***
                        minbucket=5)                          

validated_tree 

predictforTest<-predict(validated_tree,testDataNew)
print(RMSE.rtree.third <- sqrt(mean((pred-test$Response)^2)))
submit <- data.frame(Id = testDataNew$Id, Response = round(predictforTest))




##Accuracy ~55%

############################################


#______________________________________________Another Validation_________________

library(caret)
tc <- trainControl("cv",10)
rpart.grid <- expand.grid(.cp=0.001)

(train.rpart <- train(Response ~., data=trainDataNew, method="rpart",trControl=tc,tuneGrid=rpart.grid))


#### Another way how to get Accuracy 55%
######__________________________________________________


#########################################Random Forest

set.seed(123)

n <- nrow(trainDataNew)
shuffled <- trainDataNew[sample(n),]
# The shuffled dataset is already loaded into your workspace

# Initialize the accs vector
accs <- rep(0,10)

for (i in 1:10) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/10)*nrow(shuffled))) + 1):((i*round((1/10) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- randomForest(train$Response ~ ., data = train, ntree = 100)
  
  # Make a prediction on the test set using tree
  pred <- predict(tree, test, type = "class")
  
  # Assign the confusion matrix to conf
  conf <- table(test$Response, pred)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}

# 
mean(accs)
print(accs)

###Putting together some data
accuracy <- data.frame(Method = c("Baseline","Full tree","Pruned tree","K-Fold Cross Validation", " With Traincontrol obeject"),RMSE   = c(RMSE.baseline,RMSE.rtree,RMSE.rtree.pruned, 2.03788,RMSE.rtree.third)) 

accuracy$RMSE <- round(accuracy$RMSE,2)

accuracy

#Calculate the occurancy of the best validation

n_occur <- data.frame(table(submit$Response))


