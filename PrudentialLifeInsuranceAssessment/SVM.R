library(dplyr)
library(caret)
library(mice)
library(Hmisc)
library(DMwR)
library(XLConnect)
#library(ggplot2)


trainData<- read.csv("C:/Users/roops1411/Desktop/ADS/train.csv")
testData<- read.csv("C:/Users/roops1411/Desktop/ADS/test.csv")

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


######------------ Removing missing data over 60%------------ #######
################---------Train data------------#################

#Function for Count of NAs
blanks_count=function(vector){
  s=0
  for(i in 1:length(vector)){
    if(is.na(vector[i]==TRUE)) s=s+1
  }
  return(s)
}

sum(is.na(trainData))

#Percentage of missigness per predictor(missingness rate)
C=data.frame(lapply(trainData, blanks_count))*100/nrow(trainData)
#C

#Excluding variables that have missingness rate over 50%
selected=subset(colnames(C),C[1,]<60)
trainData<-trainData[,selected]


######------------ Removing missing data over 60%------------ #######
################---------Test data------------#################

#Function for Count of NAs
blanks_count=function(vector){
  s=0
  for(i in 1:length(vector)){
    if(is.na(vector[i]==TRUE)) s=s+1
  }
  return(s)
}

sum(is.na(testData))

#Percentage of missigness per predictor(missingness rate)
C=data.frame(lapply(testData, blanks_count))*100/nrow(testData)
# C

#Excluding variables that have missingness rate over 50%
selected=subset(colnames(C),C[1,]<60)
testData<-testData[,selected]


######------------ Filling the missing data------------ #######
################---------Train data------------#################
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
  trainData[, feature] <- as.factor(trainData[, feature])
}

#--------------------------------------------------------------------------------------------------------

#continuous variables
continuous_variables <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", 
                          "Employment_Info_4", "Employment_Info_6", "Insurance_History_5", 
                          "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5")

#Imputing continuous data with median
for (feature in continuous_variables) {
  trainData[is.na(trainData[, feature]), feature] <- median(trainData[, feature], na.rm = TRUE)
  trainData[, feature] <- as.numeric(trainData[, feature])
  
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
write.csv(trainData, file="C:/Users/roops1411/Desktop/ADS/trainData.csv")
write.csv(testData, file="C:/Users/roops1411/Desktop/ADS/testData.csv")

#reading the new file
trainDataNew<- read.csv("C:/Users/roops1411/Desktop/ADS/trainData.csv")
testDataNew<- read.csv("C:/Users/roops1411/Desktop/ADS/testData.csv")



#--------------------- data Preprocessing -------------------------------------

#preprocessing data


#plotting data frame
data.frame(trainDataNew, row.names = NULL, check.rows = FALSE)
data.frame(testDataNew, row.names = NULL, check.rows = FALSE)

#ploting the data
#plotcp(trainDataNew)
#plotcp(testDataNew)

#----------------------SVM Regression------------------------------------------------------
# loading neccessary packages and dataset
library(caret)
#This is the  SVM package that has to be loaded
library(e1071)
#Read the data set "GermanCredit"
trainDataNew<- read.csv("C:/Users/roops1411/Desktop/ADS/trainData.csv")
testDataNew<- read.csv("C:/Users/roops1411/Desktop/ADS/testData.csv")

dataset = trainDataNew
dataset1 = testDataNew
# Making an index for creating the subset of the data set from 1 to n row
index<-1:nrow(dataset)
index1<-1:nrow(dataset1)

#Creating a subset with 1% of the data in the data set
sample_index<-sample(index,trunc(length(index)*1/100))
#sample_index = sample(60000,10000)
new_set <- dataset[sample_index,]

#Looking at the structure of some object
str(trainDataNew)


#Creating the train dataset and the test dataset with the subset that is been formed
smp_size <- floor(0.9 * nrow(new_set))
train_ind <- sample(seq_len(nrow(new_set)), size = smp_size)

train_dataset <-  new_set[train_ind, ]
test_dataset = new_set[-train_ind,] 


#To find the most important columnspresent in the data set
rm <- lm(Response ~., data = trainDataNew)
summary(rm)

#Train and tune the SVM
m1 <- tune.svm(Response ~ ., data = train_dataset, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(m1)



#Applying the SVM kernel 
model <- svm(Response ~ Product_Info_4 + Product_Info_6 + Ins_Age + Ht + Wt + BMI + Employment_Info_3
             + Employment_Info_5 + InsuredInfo_2 + InsuredInfo_5 +InsuredInfo_6 + InsuredInfo_7 + Insurance_History_1
             + Insurance_History_2 + Insurance_History_3 + Family_Hist_2 +Family_Hist_3 +Family_Hist_4
             + Family_Hist_5 + Medical_History_3 +Medical_History_4 + Medical_History_11 + Medical_History_12
             + Medical_History_13 + Medical_History_17 + Medical_History_22 + Medical_History_23
             + Medical_History_27 + Medical_History_29 + Medical_History_30 + Medical_History_31
             + Medical_History_35 + Medical_History_39 + Medical_History_40 + Medical_Keyword_2
             + Medical_Keyword_3  + Medical_Keyword_6 + Medical_Keyword_9  + Medical_Keyword_15 + Medical_Keyword_19
             + Medical_Keyword_22 + Medical_Keyword_25 + Medical_Keyword_33 + Medical_Keyword_34 + Medical_Keyword_38
             + Medical_Keyword_41 + Medical_Keyword_45 , data = train_dataset, kernel = "radial", gamma = 0.1, cost = 10)
print(model)
summary(model)
plot(model,train_dataset[,])

#Actual vs Observed value
prediction <- predict(model, test_dataset)
tab <- table(pred = prediction, true = test_dataset[,128])

pr <- round(prediction)

#length(prediction)
#length(pr)
#length(test_dataset$Response)

summary(tab)
summary(prediction)

#Confusion matrix
table(pr, test_dataset$Response)

#Mean square error
svm = predict(model,test_dataset)
MSE <-mean((svm-test_dataset$Response)^2)
MSE
RMSE <- sqrt(MSE)
RMSE
