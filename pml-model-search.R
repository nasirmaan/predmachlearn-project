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

# corr_mat = cor(trainData[, -ncol], use='complete.obs')
# pca = prcomp(formula = ~., data = trainData[, -ncol], cor = TRUE, 
#              na.action=na.exclude, scale=T, center = T)
# g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
#               groups = trainData$classe, ellipse = TRUE, 
#               circle = TRUE)
# 
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', 
#                legend.position = 'top')

#trans = preProcess(trainData[, -ncol], method="pca", pcaComp=27)
# trans = preProcess(trainData[, -ncol], method=c("BoxCox", "center",  "scale", "pca"),
#                    pcaComp=27)
# PC = predict(trans, trainData[, -ncol])
# modelFit = train(trainData$classe ~ ., method='glm', data=PC)

tmpData = trainData[1:1000, ]
modelFit <- train(classe ~ ., method='rf', data=tmpData, prox=T)

#Model Analytics
findMod = modelFit$finalModel
qplot(findMod$fitted, findMod$residuals, colour=classe, data=trainData)
plot(findMod$residuals, pch=19)


#Confusion Matrix
testPC <- predict (trans, testData[, -ncol])
cnfmat = confusionMatrix(testData$classe, predict(modelFit, testData))

lapply(names(trainData), function (x) class(trainData[, x]))