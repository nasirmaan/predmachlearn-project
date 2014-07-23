require(caret)
library(ggbiplot)
library(randomForest)

setwd("C:/Dropbox/datasci-jhi/machinelearning/course_project/")
data <- read.csv("pml-training.csv", stringsAsFactor=F)
data$classe <- as.factor(data$classe)

excl = vector()
for (i in 1:159) {
  if ((class(data[, i]) == "character") | (sum(is.na(data[, i])) > 19000)) {
    excl = append(excl, i)
  }
  if (class(data[, i]) == "integer"){
    data[, i] = as.numeric(data[, i])
  }
}
data = data[-excl]; ncol = ncol(data)
inTrain <- createDataPartition(y=data$classe, p=0.75, list=F)
trainData <- data[inTrain, ]
testData <- data[-inTrain, ]

rf <-randomForest(classe~.,data=trainData, mtry=2, ntree=1000, 
                  keep.forest=TRUE, importance=TRUE)
pr = predict(rf, testData)
t = table (obs=testData$classe, predict=pr)
prop.table(t, 1)
