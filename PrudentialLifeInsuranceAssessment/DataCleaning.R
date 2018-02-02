library(dplyr)
library(caret)
library(mice)
library(Hmisc)
library(DMwR)
library(XLConnect)
#library(ggplot2)


trainData<- read.csv("D:\\Course\\INFO7390 Advanced Data Science\\ADS_Midterm_Project\\train.csv")
testData<- read.csv("D:\\Course\\INFO7390 Advanced Data Science\\ADS_Midterm_Project\\test.csv")
  
#--------------------- Understanding the data---------------------

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



######------------ Filling the missing data------------ #######
################---------Train data------------#################
#deciding variables

#----------------------------------------Converting categorical values to numeric----------------------------------------------------------------

trainData$Product_Info_2 <- as.numeric(trainData$Product_Info_2)
is.numeric(trainData$Product_Info_2)
trainData$Product_Info_1 <- as.numeric(trainData$Product_Info_1)
is.numeric(trainData$Product_Info_1)
trainData$Product_Info_3 <- as.numeric(trainData$Product_Info_3)
is.numeric(trainData$Product_Info_3)
trainData$Product_Info_5 <- as.numeric(trainData$Product_Info_5)
is.numeric(trainData$Product_Info_5)
trainData$Product_Info_7 <- as.numeric(trainData$Product_Info_7)
is.numeric(trainData$Product_Info_7)
trainData$Employment_Info_2 <- as.numeric(trainData$Employment_Info_2)
is.numeric(trainData$Employment_Info_2)
trainData$Employment_Info_3 <- as.numeric(trainData$Employment_Info_3)
is.numeric(trainData$Employment_Info_3)
trainData$Employment_Info_5 <- as.numeric(trainData$Employment_Info_5)
is.numeric(trainData$Employment_Info_5)
trainData$InsuredInfo_1 <- as.numeric(trainData$InsuredInfo_1)
is.numeric(trainData$InsuredInfo_1)
trainData$InsuredInfo_2 <- as.numeric(trainData$InsuredInfo_2)
is.numeric(trainData$InsuredInfo_2)
trainData$InsuredInfo_3 <- as.numeric(trainData$InsuredInfo_3)
is.numeric(trainData$InsuredInfo_3)
trainData$InsuredInfo_4 <- as.numeric(trainData$InsuredInfo_4)
is.numeric(trainData$InsuredInfo_4)
trainData$InsuredInfo_5 <- as.numeric(trainData$InsuredInfo_5)
is.numeric(trainData$InsuredInfo_5)
trainData$InsuredInfo_6 <- as.numeric(trainData$InsuredInfo_6)
is.numeric(trainData$InsuredInfo_6)
trainData$InsuredInfo_7 <- as.numeric(trainData$InsuredInfo_7)
is.numeric(trainData$InsuredInfo_7)
trainData$Insurance_History_1 <- as.numeric(trainData$Insurance_History_1)
is.numeric(trainData$Insurance_History_1)
trainData$Insurance_History_2 <- as.numeric(trainData$Insurance_History_2)
is.numeric(trainData$Insurance_History_2)
trainData$Insurance_History_3 <- as.numeric(trainData$Insurance_History_3)
is.numeric(trainData$Insurance_History_3)
trainData$Insurance_History_4 <- as.numeric(trainData$Insurance_History_4)
is.numeric(trainData$Insurance_History_4)
trainData$Insurance_History_7 <- as.numeric(trainData$Insurance_History_7)
is.numeric(trainData$Insurance_History_7)
trainData$Insurance_History_8 <- as.numeric(trainData$Insurance_History_8)
is.numeric(trainData$Insurance_History_8)
trainData$Insurance_History_9 <- as.numeric(trainData$Insurance_History_9)
is.numeric(trainData$Insurance_History_9)
trainData$Family_Hist_1 <- as.numeric(trainData$Family_Hist_1)
is.numeric(trainData$Family_Hist_1)

#------------------------------------------------------------------------------------------

Categorical_variables <- c("Product_Info_1", "Product_Info_2","Product_Info_3", 
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

#----------------------------------------Converting categorical values to numeric----------------------------------------------------------------

testData$Product_Info_2 <- as.numeric(testData$Product_Info_2)
is.numeric(testData$Product_Info_2)
testData$Product_Info_1 <- as.numeric(testData$Product_Info_1)
is.numeric(testData$Product_Info_1)
testData$Product_Info_3 <- as.numeric(testData$Product_Info_3)
is.numeric(testData$Product_Info_3)
testData$Product_Info_5 <- as.numeric(testData$Product_Info_5)
is.numeric(testData$Product_Info_5)
testData$Product_Info_7 <- as.numeric(testData$Product_Info_7)
is.numeric(testData$Product_Info_7)
testData$Employment_Info_2 <- as.numeric(testData$Employment_Info_2)
is.numeric(testData$Employment_Info_2)
testData$Employment_Info_3 <- as.numeric(testData$Employment_Info_3)
is.numeric(testData$Employment_Info_3)
testData$Employment_Info_5 <- as.numeric(testData$Employment_Info_5)
is.numeric(testData$Employment_Info_5)
testData$InsuredInfo_1 <- as.numeric(testData$InsuredInfo_1)
is.numeric(testData$InsuredInfo_1)
testData$InsuredInfo_2 <- as.numeric(testData$InsuredInfo_2)
is.numeric(testData$InsuredInfo_2)
testData$InsuredInfo_3 <- as.numeric(testData$InsuredInfo_3)
is.numeric(testData$InsuredInfo_3)
testData$InsuredInfo_4 <- as.numeric(testData$InsuredInfo_4)
is.numeric(testData$InsuredInfo_4)
testData$InsuredInfo_5 <- as.numeric(testData$InsuredInfo_5)
is.numeric(testData$InsuredInfo_5)
testData$InsuredInfo_6 <- as.numeric(testData$InsuredInfo_6)
is.numeric(trainData$InsuredInfo_6)
testData$InsuredInfo_7 <- as.numeric(testData$InsuredInfo_7)
is.numeric(testData$InsuredInfo_7)
testData$Insurance_History_1 <- as.numeric(testData$Insurance_History_1)
is.numeric(testData$Insurance_History_1)
testData$Insurance_History_2 <- as.numeric(testData$Insurance_History_2)
is.numeric(testData$Insurance_History_2)
testData$Insurance_History_3 <- as.numeric(testData$Insurance_History_3)
is.numeric(testData$Insurance_History_3)
testData$Insurance_History_4 <- as.numeric(testData$Insurance_History_4)
is.numeric(testData$Insurance_History_4)
testData$Insurance_History_7 <- as.numeric(testData$Insurance_History_7)
is.numeric(testData$Insurance_History_7)
testData$Insurance_History_8 <- as.numeric(testData$Insurance_History_8)
is.numeric(testData$Insurance_History_8)
testData$Insurance_History_9 <- as.numeric(testData$Insurance_History_9)
is.numeric(testData$Insurance_History_9)
testData$Family_Hist_1 <- as.numeric(testData$Family_Hist_1)
is.numeric(testData$Family_Hist_1)


#------------------------------------------------------------------------------------------


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
write.csv(trainData, file="D:\\Course\\INFO7390 Advanced Data Science\\ADS_Midterm_Project\\trainData.csv")
write.csv(testData, file="D:\\Course\\INFO7390 Advanced Data Science\\ADS_Midterm_Project\\testData.csv")

#reading the new file
trainDataNew<- read.csv("D:\\Course\\INFO7390 Advanced Data Science\\ADS_Midterm_Project\\trainData.csv")
testDataNew<- read.csv("D:\\Course\\INFO7390 Advanced Data Science\\ADS_Midterm_Project\\testData.csv")


#plotting data frame
data.frame(trainDataNew, row.names = NULL, check.rows = FALSE)
data.frame(testDataNew, row.names = NULL, check.rows = FALSE)


