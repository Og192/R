library(AppliedPredictiveModeling)
library(lattice)
library(caret)
data(solubility)
library(earth)
library(kernlab)

marsFit <- earth(solTrainXtrans, solTrainY)
print(marsFit)
summ <- summary(marsFit)
print(summ)
plotmo(marsFit)

marsGrid <- expand.grid(.degree=1:2, .nprune=2:38)
set.seed(100)
marsTuned <- train(solTrainXtrans, solTrainY,
                   method="earth",
                   tuneGrid = marsGrid,
                   trControl = trainControl(method="cv"))
print(marsTuned)
print(head(predict(marsTuned, solTestXtrans)))

print(varImp(marsTuned))