require(caret)
library(ggbiplot)
library(randomForest)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

setwd("C:/Dropbox/datasci-jhi/machinelearning/course_project/")
trainData <- read.csv("pml-training.csv", stringsAsFactor=F)
testData <- read.csv("pml-testing.csv", stringsAsFactor=F)
trainData$classe <- as.factor(trainData$classe)

excl = vector()
for (i in 1:159) {
  if ((class(trainData[, i]) == "character") | (sum(is.na(trainData[, i])) > 19000)) {
    excl = append(excl, i)
  }
  if (class(trainData[, i]) == "integer"){
    trainData[, i] = as.numeric(trainData[, i])
  }
}
testData = testData[-excl]
trainData = trainData[-excl]
ncol = ncol(trainData)
rf <-randomForest(classe~.,data=trainData, mtry=2, ntree=1000, 
                  keep.forest=TRUE, importance=TRUE)
pred = predict(rf, testData)
pml_write_files(pred)