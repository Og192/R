library(AppliedPredictiveModeling)
library(lattice)
library(caret)
data(solubility)
library(earth)
library(kernlab)

##### Neural Networks

# This would create a single model with 5 hidden units
# nnetFit <- nnet(predictors,     
#                 outcome, 
#                 size=5, 
#                 decay=0.01, 
#                 linout=TRUE,
#                 trace=FALSE, 
#                 maxit=500, 
#                 MaxNWts=5 *(ncol(predictors) + 1) + 5 + 1)
# predict(nnetFit, newData)

ctrl = trainControl(method="cv", number=10)

tooHigh <- findCorrelation(cor(solTrainXtrans), cutoff=.75)
trainXnnet <- solTrainXtrans[, -tooHigh]
testXnnet <- solTestXtrans[, -tooHigh]
## Create a specific candidate set of models to evaluate
nnetGrid <- expand.grid(.decay=c(0, 0.01, .1),
                        .size=c(1:10),
                        .bag=FALSE)
set.seed(100)
#  the linear relationship between the hidden
# units and the prediction can be used with the option linout = TRUE.
nnetTune <- train(solTrainXtrans, solTrainY,
                  method="avNNet",
                  trControl = ctrl,
                  preProc= c("center", "scale"),
                  linout = TRUE,
                  trace = FALSE,
                  MaxNWts = 10 * (ncol(trainXnnet) + 1) + 10 + 1,
                  maxit = 500)
print(nnetTune)