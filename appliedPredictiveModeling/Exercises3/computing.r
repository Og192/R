library(AppliedPredictiveModeling)
data(segmentationOriginal)
segData <- subset(segmentationOriginal, Case=="Train")
cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
# Now remove the columns
segData <- segData[, -(1:3)]

statusColNum <- grep("Status", names(segData))
print(statusColNum)
segData <- segData[, -statusColNum]

##### Transformations

## skewness
library(e1071)
# For one predictor
sk <- skewness(segData$AngleCh1)
print(sk)
sks <- apply(segData, 2, skewness)
print(head(sks))
# hist(segData$AvgIntenCh1)

library(caret)
hist(segData$AreaCh1)
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
print(Ch1AreaTrans)
# The original data
print(head(segData$AreaCh1))
result <- predict(Ch1AreaTrans, head(segData$AreaCh1))
print(result)
print("819^(-.9) - 1)/(-.9)")
print((819^(-.9) - 1)/(-.9))

## PCA
pcaObject <- prcomp(segData, center=TRUE, scale.=TRUE)
percentVariance <- pcaObject$sd ^ 2 / sum(pcaObject$sd ^ 2) * 100
print(percentVariance[1:3])
# transformed values 
print(head(pcaObject$x[, 1:5]))
# loadings
print(head(pcaObject$rotation[, 1:3]))

## sensitive to outliers
#spatialSign(segData)

### preProcess
# BoxCox, centering, scaling, imputation, feature extraction, and then spatial sign
trans <- preProcess(segData, method=c("BoxCox", "center", "scale", "pca"))
print(trans)

transformed <- predict(trans, segData)
print(head(transformed[, 1:5]))


##### Filtering
