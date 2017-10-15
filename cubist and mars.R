library(earth)
earth.mod <- earth(Volume ~ ., data = trees)
data("ozone1")
earth.mod <- earth(O3 ~ ., data = ozone1,degree = 3) 

plotmo(earth.mod)
summary(earth.mod, digits = 2, style = "pmax")
earth.mod$bx
earth.mod$dirs
earth.mod$cuts
earth.mod$selected.terms
earth.mod$coefficients
earth.mod$terms
earth.mod$termcond
earth.mod$prune.terms

library(Cubist)
library(mlbench)
data(BostonHousing)
BostonHousing$chas <- as.numeric(BostonHousing$chas) - 1
set.seed(1)
inTrain <- sample(1:nrow(BostonHousing), floor(.8*nrow(BostonHousing)))
trainingPredictors <- BostonHousing[ inTrain, -14]
testPredictors<- BostonHousing[-inTrain, -14]
trainingOutcome <- BostonHousing$medv[ inTrain]
testOutcome<- BostonHousing$medv[-inTrain]
modelTree <- cubist(x = trainingPredictors, y = trainingOutcome)
modelTree
dotplot(modelTree,what = "splits")
dotplot(modelTree,what = "coefs")

