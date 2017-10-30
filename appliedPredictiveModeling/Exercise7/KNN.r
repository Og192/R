knnDescr <- solTrainXtrans[, -nearZeroVar(solTrainXtrans)]
set.seed(100)
knnTune <- train(knnDescr,
                 solTrainY,
                 method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(.k= 1:20),
                 trControl = trainControl(method = "cv"))
print(knnTune)