library(rpart)
library(party)
library(caret)
library(AppliedPredictiveModeling)
data(solubility)
library(rJava)
library(RWeka)

m5tree <- M5P(solTrainY~., data=solTrainXtrans)
m2ruls <- M5Rules(solTrainY~., data=solTrainXtrans)
m5tree <- M5P(solTrainY~., data=solTrainXtrans,
                control = Weka_control(M=10))
set.seed(100)
m2Tune <- train(solTrainXtrans, solTrainY,
                method = "M5",
                trControl = trainControl(method="cv"),
                control = Weka_control(M=10))
plot(m5Tune)