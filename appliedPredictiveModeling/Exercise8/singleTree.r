library(rpart)
library(party)
library(caret)
library(AppliedPredictiveModeling)
data(solubility)
rpartTree <- rpart(solTrainY~., data=solTrainXtrans)
print(rpartTree)

ctreeTree <- ctree(solTrainY~., data=solTrainXtrans)
print(ctreeTree)
plot(ctreeTree)

rpartTune <- train(solTrainXtrans, solTrainY,
                method = "rpart2",
                tuneLength = 10,
                trControl = trainControl(method="cv"))
library(partykit)
rpartTree2 <- as.party(rpartTree)
plot(rpartTree2)