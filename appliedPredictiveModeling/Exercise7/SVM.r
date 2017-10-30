library(AppliedPredictiveModeling)
library(lattice)
library(caret)
data(solubility)
library(earth)
library(kernlab)

svmRTuned <- train(solTrainXtrans, solTrainY,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   tuneLength = 14,
                   trControl = trainControl(method="cv"))
print(svmRTuned)
print(svmRTuned$finalModel)
