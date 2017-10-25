library(AppliedPredictiveModeling)
data(twoClassData)
print(str(predictors))
print(str(classes))

##### Data Splitting
library(caret)
set.seed(1)
trainingRows <- createDataPartition(classes, p=.80, list=FALSE)
print(head(trainingRows))

trainPredictors <- predictors[trainingRows, ]
trainClasses <- classes[trainingRows]
testPredictors <- predictors[-trainingRows, ]
testClasses <- classes[-trainingRows]

str(trainPredictors)
str(testPredictors)

###### Resampling
set.seed(1)
repeatedSplits <- createDataPartition(trainClasses, p=.80, times=3)
str(repeatedSplits)

## 10 fold
set.seed(1)
cvSplits <- createFolds(trainClasses, k = 10, returnTrain=TRUE)
str(cvSplits)
fold1 <- cvSplits[[1]]
cvPredictors1 <- trainPredictors[fold1,]
cvClasses1 <- trainClasses[fold1]
str(nrow(trainPredictors))
str(nrow(cvPredictors1))