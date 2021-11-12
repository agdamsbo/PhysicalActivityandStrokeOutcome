# Data
## Import from previous work
dta<-read.csv("/Volumes/Data/exercise/source/background.csv",na.strings = c("NA","","unknown"),colClasses = "character")


## Cleaning and enhancing
dta$pase_drop<-factor(ifelse((dta$pase_0_q=="q_2"|dta$pase_0_q=="q_3"|dta$pase_0_q=="q_4")&dta$pase_06_q=="q_1","yes","no"),levels = c("no","yes"))
dta$pase_drop[is.na(dta$pase_6)]<-NA
dta$pase_drop[is.na(dta$pase_0)]<-NA

## Selection of data set and formatting
library(dplyr)
dta_f<-dta %>% filter(pase_0_q != "q_1" & !is.na(pase_drop))


variable_names<-c("age","sex","weight","height",
                     "bmi",
                     "smoke_ever",
                     "civil",
                     "diabetes", 
                     "hypertension",
                     "pad", 
                     "afli",  
                     "ami", 
                     "tci",
                     "nihss_0",
                     "thrombolysis", 
                     "thrombechtomy",
                     "rep_any","pase_0_q","pase_drop")


library(daDoctoR)
dta2<-dta_f[,variable_names]

dta2<-col_num(c("age","weight","height","bmi","nihss_0"),dta2)
dta2<-col_fact(c("sex","smoke_ever","civil","diabetes", "hypertension","pad", "afli",  "ami", "tci","thrombolysis", "thrombechtomy","rep_any","pase_0_q","pase_drop"),dta2)

## Partitioning
library(caret)
set.seed(100)

## Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(dta2$pase_drop, p=0.8, list=FALSE)

## Step 2: Create the training  dataset
trainData <- dta2[trainRowNumbers,]

## Step 3: Create the test dataset
testData <- dta2[-trainRowNumbers,]
y_test = testData[,"pase_drop"]

# Store X and Y for later use.
x = trainData %>% select(!matches("pase_drop"))
y = trainData[,"pase_drop"]

# Normalization and dummy binaries

# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(pase_drop ~ ., data=trainData)

# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_mat <- predict(dummies_model, newdata = trainData)

# # Convert to dataframe
trainData <- data.frame(trainData_mat)

# # See the structure of the new dataset
str(trainData)

dummies_model <- dummyVars(pase_drop ~ ., data=testData)
testData_mat <- predict(dummies_model, newdata = testData)
testData <- data.frame(testData_mat)
preProcess_range_model <- preProcess(testData, method='range')
testData <- predict(preProcess_range_model, newdata = testData)
testData$pase_drop<-y_test

# Imputation

library(RANN)  # required for knnInpute
preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
# preProcess_missingdata_model

trainData <- predict(preProcess_missingdata_model, newdata = trainData)  # Giver fejl??
anyNA(trainData)

# skimr::skim(trainData)
# skimr::skim(x)

preProcess_range_model <- preProcess(trainData, method='range')
trainData <- predict(preProcess_range_model, newdata = trainData)

# Append the Y variable
trainData$pase_drop <- y


# Export
write.csv(trainData,"/Users/au301842/PhysicalActivityandStrokeOutcome/data/trainData.csv",row.names = FALSE)
write.csv(testData,"/Users/au301842/PhysicalActivityandStrokeOutcome/data/testData.csv",row.names = FALSE)
