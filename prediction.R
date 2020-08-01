#Data processing


library(AppliedPredictiveModeling)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(rattle)
library(dplyr)
setwd("C:/Users/ecaeshh/Documents/R Programs DIrectory/Course-8-Prediction")


if (!file.exists("pml-training.csv")) {
        fileurl_training<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        filepath_training <- file.path(getwd(),"pml-training.csv")
        download.file(fileurl_training, destfile = filepath_training)
   }

if (!file.exists("pml-testing.csv")) {
        fileurl_testing<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        filepath_testing <- file.path(getwd(),"pml-testing.csv")
        download.file(fileurl_testing, destfile = filepath_testing)
        
}


data_df <- read.csv("pml-training.csv",na.strings=c("NA","#DIV/0!",""))
testcases_df <- read.csv("pml-testing.csv",na.strings=c("NA","#DIV/0!",""))


#Data Cleaning

exclude <- grep("name|timestamp|window|X", colnames(data_df), value=F) 
data_df <- data_df[,-exclude]
testcases_df <- testcases_df[,-exclude]

nonblank_log <- !sapply(data_df, is.na)
nonblank_ratio <- apply(nonblank_log,2,sum)/length(data_df[,1])
nonblankColumns<-names(data_df[which(nonblank_ratio>0.98)])


clean_data<- na.omit(data_df[,nonblankColumns])
modifiednonblankColumns <- nonblankColumns[-which(nonblankColumns=="classe")]
cleaned_testcases <- testcases_df[,modifiednonblankColumns]
cleaned_testcases<- na.omit(cleaned_testcases)



set.seed(3433)
inTrain <- createDataPartition(clean_data$classe, p = 3/4)[[1]]

myTraining <- clean_data[ inTrain,-c(1,2)]
myTesting <- clean_data[-inTrain,-c(1,2)]


#Approach 1: Decision Tree
mod_tree <- rpart(classe~.,data=myTraining,method="class")
fancyRpartPlot(mod_tree)
predict_tree <- predict(mod_tree,myTesting,type="class")
confusionMatrix(myTesting$classe,predict_tree)


#Approach 2: Random Forest
set.seed(233)
mod_rf <- randomForest(classe~.,data=myTraining,method="class")
predict_rf<- predict(mod_rf,myTesting)
confusionMatrix(predict_rf,myTesting$classe)


#Predicting test cases
set.seed(7865)
predict_rf<- predict(mod_rf,cleaned_testcases)

