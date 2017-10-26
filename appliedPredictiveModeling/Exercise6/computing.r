library(AppliedPredictiveModeling)
library(lattice)
library(caret)
data(solubility)
print(ls(pattern = "^solT"))

set.seed(2)
print(sample(names(solTrainX), 8))


##### Ordinary Linear Regression
trainingData <- solTrainXtrans
## Add the solubility outcome
trainingData$Solubility <- solTrainY
lmFitAllPredictors <- lm(Solubility~., data=trainingData)
print(summary(lmFitAllPredictors))

lmPred1 <- predict(lmFitAllPredictors, solTestXtrans)
print(head(lmPred1))

lmValues1 <- data.frame(obs=solTestY, pred=lmPred1)
testSummary <- defaultSummary(lmValues1)
print(testSummary)

## Huber approach
# library(MASS)
# rmlFirAllPredictors <- rlm(Solubility~., data = trainingData)
set.seed(100)
lmFit1 <- train(x = solTrainXtrans, y = solTrainY,
                method = "lm", trControl=trainControl(method="cv", number=10))
print(lmFit1)

##  predicted values versus the observed values
# plot the points (type = 'p') and a background grid ('g')
xyplot(solTrainY ~ predict(lmFit1), type=c("p", "g"), xlab = "Predicted", ylab="Observed")
#  residuals versus the predicted values
xyplot(resid(lmFit1) ~ predict(lmFit1), type=c("p", "g"), xlab="Predicted", ylab = "Residuals")

## drop correleted relation
corThresh <- .9
tooHigh <- findCorrelation(cor(solTrainXtrans), corThresh)
corrPred <- names(solTrainXtrans)[tooHigh]
trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered <- solTestXtrans[, -tooHigh]
lmFiltered <- train(solTrainXtrans, solTrainY, method='lm', trControl=trainControl(method="cv", number=10))
print(lmFiltered)

## Robust linear regression
#pca
set.seed(100)
rlmPCA <- train(solTrainXtrans, solTrainY, method='rlm', preProcess="pca", trControl=trainControl(method="cv", number=10))
print(rlmPCA)


##### Partial Least Squares
library(pls)
print("Partial Least Squares")
plsFit <- plsr(Solubility ~., data = trainingData)
print(solTestXtrans)
results <- predict(plsFit, solTestXtrans[1:5, ], ncomp = 1:2)
print(results)

set.seed(100)
plsTune <- train(solTrainXtrans, solTrainY, method='pls', tuneLength=20, trControl=trainControl(method="cv", number=10))
print(plsTune)

##### Penalized Regression Models
library(elasticnet)
ridgeModel <- enet(x = as.matrix(solTrainXtrans, y = soltrainY, lambda= 0.001))
ridgePred <- predict(ridgeModel, newx = as.maxtrix(solTestXtrans),
                     s = 1, mode = "fraction", type="git")
print(head(ridgePred$fit))

ridgeGrid <- data.frame(.lambda=seq(0, .1, length=15))
set.seed(100)
ridgeRegFit <- train(solTrainXtrans, y=solTrainY,
                     methos="ridge",
                     tuneGrid=ridgeGrid,
                     trControl=trainControl(method="cv", number=10),
                     preProc = c("center", "scale"))
print(ridgeRegFit)

# The lars package contains the lars function, the elasticnet package has enet,
# and the glmnet package has a function of the same name