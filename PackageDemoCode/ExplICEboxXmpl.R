#install.packages("ICEbox")
## Not run:
require(ICEbox)
require(randomForest)
require(MASS) #has Boston Housing data, Pima
data(Boston) #Boston Housing data
X = Boston
y = X$medv
X$medv = NULL
## build a RF:
bh_rf = randomForest(X, y)
## Create an'ice'object for the predictor "age":
  bh.ice = ice(object = bh_rf, X = X, y = y, predictor = "lstat",
               frac_to_build = .1)
  bh.dice = dice(bh.ice)
  summary(bh.ice)
  print(bh.ice)
  summary(bh.dice)
  plot(bh.dice)
  plot(bh.ice)
## cluster the curves into 2 groups.
clusterICE(bh.ice, nClusters = 2, plot_legend = TRUE)
## cluster the curves into 3 groups, start all at 0.
clusterICE(bh.ice, nClusters = 3, plot_legend = TRUE, center = TRUE)
## End(Not run)