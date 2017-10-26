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


##### Basic Model Building in R
# formula 
# The formula y ∼ . can be used to indicate that all of the columns in the
# data set (except y) should be used as a predictor.
# modelFunction(price ~ numBedrooms + numBaths + acres, data = housingData)
trainPredictors <- as.matrix(trainPredictors)
knnFit <- knn3(x=trainPredictors, y = trainClasses, k = 5)
print(knnFit)

testPredictions <- predict(knnFit, newdata=testPredictors, type='class')
print(head(testPredictions))
str(testPredictions)


##### Determination of Tuning Parameters
library(caret)
data(GermanCredit)
GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL
set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest  <- GermanCredit[-inTrain, ]

# The chapters directory of the AppliedPredictiveModeling package
set.seed(1056)
svmFit <- train(Class ~., 
                data=GermanCreditTrain, 
                method="svmRadial",
                preProc=c("center", "scale"),
                tuneLength=10,
                trControl = trainControl(method="repeatedcv",repeats=5))
# Using the option tuneLength = 10, the cost values 2−2, 2−2
# . . . 27 are evaluate
# By default, the basic bootstrap will be used to calculate performance measures

print(svmFit)
plot(svmFit, scales = list(x=list(log=2)))
predictedClasses <- predict(svmFit, GermanCreditTest)
print(predictedClasses)

predictedProbs <- predict(svmFit, newdata=GermanCreditTest, type="prob")
print(head(predictedProbs))
