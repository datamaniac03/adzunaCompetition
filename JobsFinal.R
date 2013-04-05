#Job Salary Prediction
#Adzuna or whatever
#Let's do something descent for a change

#Init
rm(list=ls())
ls()

setwd("D:/Wacax/Kaggle Data Analysis/Job Salary Prediction")

#Load Data
colClassesData <- read.csv("Train_rev1.csv", nrows = 1000)
colClassesData <- sapply(colClassesData[1, ], class)
trainData <- read.csv("Train_rev1.csv", colClasses = colClassesData)

colClassesData <- read.csv("Valid_rev1.csv", nrows = 1000)
colClassesData <- sapply(colClassesData[1, ], class)
valData <- read.csv("Valid_rev1.csv", colClasses = colClassesData)

colClassesData <- read.csv("random_forest_benchmark_rev1.csv", nrows = 1000)
colClassesData <- sapply(colClassesData[1, ], class)
valDataResults <- read.csv("random_forest_benchmark_rev1.csv", colClasses = colClassesData)

colClassesTest <- read.csv("Test_rev1.csv", nrows = 1000)
colClassesTest <- sapply(colClassesTest[1, ], class)
testData <- read.csv("Test_rev1.csv", colClasses = colClassesTest)

#Define Targets
y <- trainData[ ,11]
y2 <- valDataResults[, 2]
y <- c(y, y2)

trainData <- rbind(trainData[c(1:9, 12)], valData)
rm(valData, valDataResults, y2)

#Join both csvs and binarize them toghether
lengthTrain <- dim(trainData)[1]
BinDataRaw <- rbind(trainData, testData) #leaving columns 10 & 11 out because they are the values that will be trained upon
rm(testData, trainData)

#Binarize Data
source('D:/Wacax/Kaggle Data Analysis/Job Salary Prediction/binarize.R')

BinData <- rep(0, dim(BinDataRaw)[1])
#for (i in c(5, 6, 7, 9, 10)){
for (i in c(6, 7, 9, 10)){
  newBinData<- sapply(as.numeric(BinDataRaw[, i]), binarize,diag(max(as.numeric(BinDataRaw[, i]))))
  BinData <- cbind(BinData, t(newBinData))
  BinData <- BinData[, 2:ncol(BinData)]
  
}

rm(BinDataRaw, newBinData, i)
X <- BinData[1:lengthTrain, ]
Test <- BinData[(lengthTrain+1):(dim(BinData)[1]), ]
rm(BinData, lengthTrain)


#Import Statistical libraries
library(kernlab)

#Some support vector machines
testModel <- ksvm(X, log(y), type = "eps-svr")
prediction <- exp(predict(testModel, Test))

