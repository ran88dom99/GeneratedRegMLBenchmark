library(mlr)
library(mlbench)
configureMlr(on.learner.error = "warn")
look.into<-data("BostonHousing2")
str("BostonHousing2")
data(BostonHousing2, package = "mlbench")
is.data.frame(BostonHousing2)
regr.task = makeRegrTask(id = "bh", data = BostonHousing, target = "medv")
regr.task
str(getTaskData(regr.task))
regr.lrn = makeLearner("regr.gbm",fix.factors.prediction = T, par.vals = list(n.trees = 500, interaction.depth = 3))
regr.lrn$par.vals
regr.lrn$par.set
regr.lrn$predict.type

getHyperPars(regr.lrn)
getParamSet(regr.lrn)
getLearnerPackages(regr.lrn)
regr.lrn
## Change the prediction type, predict a factor with class labels instead of probabilities
#classif.lrn = setPredictType(classif.lrn, "response")
## Change hyperparameter values
#cluster.lrn = setHyperPars(cluster.lrn, centers = 4)
## Go back to default hyperparameter values
#regr.lrn = removeHyperPars(regr.lrn, c("n.trees", "interaction.depth"))

## List everything in mlr
lrns = listLearners()
head(lrns[c("class", "package")])
## List classifiers that can output probabilities
lrns = listLearners("classif", properties = "prob")
head(lrns[c("class", "package")])
## List classifiers that can be applied to iris (i.e., multiclass) and output probabilities
lrns = listLearners(iris.task, properties = "prob")
head(lrns[c("class", "package")])
## The calls above return character vectors, but you can also create learner objects
head(listLearners("cluster", create = TRUE), 2)
listLear<-listLearners("cluster", create = TRUE)
mod = train(regr.lrn, regr.task)
mod
names(mod)
#> [1] "learner"       "learner.model" "task.desc"     "subset"       
#> [5] "features"      "factor.levels" "time"          "dump"

mod$learner
#> Learner cluster.kmeans from package stats,clue
#> Type: cluster
#> Name: K-Means; Short name: kmeans
#> Class: cluster.kmeans
#> Properties: numerics,prob
#> Predict-Type: response
#> Hyperparameters: centers=4

mod$features
#> [1] "x" "y"

mod$time
#> [1] 0.002

## Extract the fitted model
getLearnerModel(mod)
#> K-means clustering with 4 clusters of sizes 23, 17, 15, 20
#> 
## Get the number of observations
n = getTaskSize(bh.task)

## Use 1/3 of the observations for training
train.set = sample(n, size = n/3)

## Train the learner
mod = train("regr.lm", bh.task, subset = train.set)
mod
#> Model for learner.id=regr.lm; learner.class=regr.lm
#> Trained on: task.id = BostonHousing-example; obs = 168; features = 13
#> Hyperparameters:


## Calculate the observation weights
target = getTaskTargets(bc.task)
tab = as.numeric(table(target))
w = 1/tab[target]
train("classif.rpart", task = bc.task, weights = w)
#> Model for learner.id=classif.rpart; learner.class=classif.rpart
#> Trained on: task.id = BreastCancer-example; obs = 683; features = 9
#> Hyperparameters: xval=0


n = nrow(iris)
iris.train = iris[seq(1, n, by = 2), -5]
iris.test = iris[seq(2, n, by = 2), -5]
task = makeClusterTask(data = iris.train)
mod = train("cluster.kmeans", task)

newdata.pred = predict(mod, newdata = iris.test)
newdata.pred
plotLearnerPrediction("regr.lm", features = "lstat", task = bh.task)
plotLearnerPrediction("regr.lm", features = c("lstat", "rm"), task = bh.task)
plotLearnerPrediction(regr.lrn, features = c("lstat", "rm"), task = bh.task)


## Performance measure suitable for the iris classification task
listMeasures(iris.task)

n = getTaskSize(bh.task)
lrn = makeLearner("regr.gbm", n.trees = 1000)
mod = train(lrn, task = bh.task, subset = seq(1, n, 2))
pred = predict(mod, task = bh.task, subset = seq(2, n, 2))
performance(pred)
performance(pred, measures = medse)
performance(pred, measures = list(mse, medse, mlr::mae))
performance(pred, measures = timeboth, model = mod)
## 3-fold cross-validation
rdesc = makeResampleDesc("CV", iters = 3)
rdesc
hout
#> Resample description: holdout with 0.67 split rate.
#> Predict: test
#> Stratification: FALSE
cv3
#> Resample description: cross-validation with 3 iterations.
#> Predict: test
#> Stratification: FALSE
rdesc = makeResampleDesc("CV", iters = 3)
rin = makeResampleInstance(rdesc, task = iris.task)

## Calculate the performance of two learners based on the same resample instance
r.lda = resample("classif.lda", iris.task, rin, show.info = FALSE)
r.rpart = resample("classif.rpart", iris.task, rin, show.info = FALSE)
r.lda$aggr
#> mmce.test.mean 
#>           0.02

r.rpart$aggr
#> mmce.test.mean 
#>     0.05333333

#Have a look at on.learner.error in configureMlr as well as the examples given in section Configure mlr of this tutorial.
base.learners = list(
  makeLearner("classif.ksvm"),
  makeLearner("classif.randomForest")
)
lrn = makeModelMultiplexer(base.learners)
ps = makeModelMultiplexerParamSet(lrn#,
                                  #makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x),
                                  #makeIntegerParam("ntree", lower = 1L, upper = 500L)
)
print(ps)
#>                                Type len Def
#> selected.learner           discrete   -   -
#> classif.ksvm.sigma          numeric   -   -
#> classif.randomForest.ntree  integer   -   -
#>                                                       Constr Req Tunable
#> selected.learner           classif.ksvm,classif.randomForest   -    TRUE
#> classif.ksvm.sigma                                 -12 to 12   Y    TRUE
#> classif.randomForest.ntree                          1 to 500   Y    TRUE
#>                            Trafo
#> selected.learner               -
#> classif.ksvm.sigma             Y
#> classif.randomForest.ntree     -

rdesc = makeResampleDesc("CV", iters = 2L)
ctrl = makeTuneControlIrace(maxExperiments = 200L)
res = tuneParams(lrn, iris.task, rdesc, par.set = ps, control = ctrl, show.info = FALSE)
print(head(as.data.frame(res$opt.path)))
