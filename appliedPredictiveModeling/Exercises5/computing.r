observed <- c(0.22, 0.83, -0.12, 0.89, -0.23, -1.30, -0.15, -1.4,
 0.62, 0.99, -0.18, 0.32, 0.34, -0.30, 0.04, -0.87,
 0.55, -1.30, -1.15, 0.20)
predicted <- c(0.24, 0.78, -0.66, 0.53, 0.70, -0.75, -0.41, -0.43,
 0.49, 0.79, -1.19, 0.06, 0.75, -0.07, 0.43, -0.42,
 -0.25, -0.64, -1.26, -0.07)
residualValues <- observed - predicted
summary(residualValues)
# Observed values versus predicted values
# It is a good idea to plot the values on a common scale.
axisRange <- extendrange(c(observed, predicted))
plot(observed, predicted, ylim=axisRange, xlim=axisRange)
# Add a 45 degree reference line
abline(0,1 , col="darkgrey", lty=2)

# Predicted values versus residuals
plot(predicted, residualValues, ylab="residual")
abline(h = 0, col="darkgrey", lty=2)

library(caret)
r2 <- R2(predicted, observed)
print(r2)
# Simple correlation
corr <- cor(predicted, observed)
print(corr)
# spearman's rank correlation
# method can be one of “pearson”, “kendall”, “spearman”
## pearson : cov(x, y)/(SD(x) * SD(y)), ps: SD is for standard deviation
## 
## spearman:   X = (1,10,100,101)替换成(1,2,3,4) and calculate pearson
##             Y = (21,10,15,13)替换成(4,1,3,2)
## kendall: (n_concordant_pairs - n_discordantPairs) / (0.5 * n(n-1))
## given Xi, Yi, Xj, Yj
## if (Xi<Yi and Xj<Yj) or (Xi>Yi and Xj>Yj) concordant
## else discordant
spearman <- cor(predicted, observed, method="spearman")
print(spearman)

rmse <- RMSE(predicted, observed)
print(rmse)

