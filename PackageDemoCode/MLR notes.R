for(allmodel in allmodels){#just before all models define d.f and reduce it
  write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
  bad.models=c("DENFIS","neuralnet","partDSA","blackboost","bstSm","bstTree","penalized","brnn","gamLoess","ANFIS","FIR.DM","FS.HGD","nodeHarvest","mlpWeightDecayML","monmlp","mlp","mlpWeightDecay","mlpSGD","rbf","rbfDDA","rfRules","GFS.FR.MOGUL","mlpML","HYFIS","GFS.THRIFT" ,"GFS.LT.RS")
  #too slow neuralnet# dnfis useless and just stops on huge datasets
  if(allmodel %in% bad.models) {next()} #gamLoess crashes. the capitals are slow and terrible
  library(caret) #mlp...s creat some bizzare problem that breaks train ##nodeHarvest is SLOW ##"rbf"crash R "rbfDDA" crash train and really bad #rfRules is REALLY slow.##"pythonKnnReg",pythonKnnReg can not install
  #penalized slow then fails
  slow.models=c("leapSeq","glmStepAIC","ppr","qrnn")#,"cubist","plsRglm","WM","gamboost")#cubist, plsRglm,WM,gamboost  only sometimes
  if(allmodel %in% slow.models && datasource=="needles in haystack"){next()}#too slow for many columns
  if(allmodel %in% slow.models && datasource=="needles hay noise"){next()}#too slow for many columns
  slow.models=c("qrnn")
  #if(allmodel %in% slow.models){next()}#too slow for much cv
  noNA.models=c("kknn")#leapSeq
  if(allmodel %in% noNA.models && datasource=="sparsity NA"){next()}#too slow for many columns
  
  
  seed.var=seed.var+1
  if(length(df.previous.calcs[,1])>0){
    if(check.redundant(df=df.previous.calcs,norming=norming,trans.y=trans.y,withextra=withextra,missingdata=missingdata,datasource=datasource ,column.to.predict=column.to.predict,allmodel=allmodel)){next}}
  
  
  # unloading the NS 'object'
  pkgs = names(sessionInfo()$otherPkgs) 
  pkgs = paste('package:', pkgs, sep = "")#detach
  lapply(pkgs,  detach, character.only = TRUE, unload = TRUE)
  library(caret)
  #library(caretEnsemble)
  library(MLmetrics)
  gc()
  
  when<-proc.time()            
  list.of.packages <-getModelInfo(allmodel)[[1]]$library
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, dep = TRUE)
  if(length(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])){
    write.table(paste("Fail","Fail","Fail","Fail","PackageFail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,round(proc.time()[3]-when[3]),  sep = ","),
                file = "gen test out.csv", append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    next()}
  
  
  not.failed=0
  set.seed(seed.var)
  try({trainedmodel <- train(x=data.frame(training[,2:length(training[1,])]),
                             y = df.toprocess[inTrain,1],
                             method = allmodel,
                             trControl = adaptControl,
                             tuneLength = tuneLength)
  
  predicted.outcomes<-predict(trainedmodel, newdata=(testing))
  p <- data.frame(predicted.outcomes,testing)
  #Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
  Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],mean(p[,2], na.rm = T))
  #mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
  mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],mean(p[,2], na.rm = T))
  p<- data.frame(predict(loess.model,predicted.outcomes),y.untransformed[-inTrain])
  #RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
  RMSE=RMSE(p[,1],p[,2])
  #RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
  RMSE.mean=RMSE(p[,2],mean(p[,2], na.rm = T))
  #MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)
  MMAAEE=MAE(p[,1],p[,2])
  
  wut=print(trainedmodel,selectCol=TRUE)
  overRMSE=-1
  try({overRMSE=as.numeric(wut[wut[,length(wut[1,])]=="*","RMSE"])})#length(wut[1,])-3]
  replace.overRMSE=1
  try({if(is.numeric(overRMSE)){replace.overRMSE=0}})
  if(replace.overRMSE==1){overRMSE=-1}
  if(length(overRMSE)<1){overRMSE=-1}
  
  #print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
  write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),
                round(overRMSE,digits = 3),round(RMSE,digits = 3),round(MMAAEE,digits = 3),
                date(),allmodel,column.to.predict,trans.y,datasource,missingdata,
                withextra,norming,RMSE.mean,adaptControl$search,seed.var,round(proc.time()[3]-when[3]),
                adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,
                adaptControl$adaptive$min,trainedmodel$bestTune),
              file = "gen test out.csv", append =TRUE, quote = F, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = F,
              col.names = F, qmethod = "double")
  print(date())
  not.failed=1
  })
  
  if(not.failed==0) {
    try({trainedmodel <- train(x=data.frame(training[,2:length(training[1,])]),
                               y =  df.toprocess[inTrain,1],
                               method = allmodel,
                               trControl = simpleControl,
                               tuneLength = tuneLength2)
    
    predicted.outcomes<-predict(trainedmodel, newdata=(testing))
    p <- data.frame(predicted.outcomes,testing)
    #Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
    Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],mean(p[,2], na.rm = T))
    #mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
    mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],mean(p[,2], na.rm = T))
    p<- data.frame(predict(loess.model,predicted.outcomes),y.untransformed[-inTrain])
    #RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
    RMSE=RMSE(p[,1],p[,2])
    #RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
    RMSE.mean=RMSE(p[,2],mean(p[,2], na.rm = T))
    #MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)
    MMAAEE=MAE(p[,1],p[,2])
    print(confusionMatrix(p[,1],p[,2]))
    
    overRMSE=-1
    wut=print(trainedmodel,selectCol=TRUE)
    try({overRMSE=as.numeric(wut[wut[,length(wut[1,])]=="*","RMSE"])})#length(wut[1,])-
    replace.overRMSE=1
    try({if(is.numeric(overRMSE)){replace.overRMSE=0}})
    if(replace.overRMSE==1){overRMSE=-1}
    if(length(overRMSE)<1){overRMSE=-1}
    
    #print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
    write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),round(overRMSE,digits = 3),
                  round(RMSE,digits = 3),round(MMAAEE,digits = 3),date(),allmodel,column.to.predict,
                  trans.y,datasource,missingdata,withextra,norming,RMSE.mean,simpleControl$search,
                  seed.var,round(proc.time()[3]-when[3]),simpleControl$method,tuneLength2,
                  simpleControl$number,"no rep","no min",trainedmodel$bestTune),
                file = "gen test out.csv", append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    print(date())
    not.failed=1
    })
    
  }
  if(not.failed==0) {
    print(c("failed","failed",date(),datasource,missingdata,withextra,norming,allmodel))
    write.table(paste("Fail","Fail","Fail","Fail","Fail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,round(proc.time()[3]-when[3]),  sep = ","),
                file = "gen test out.csv", append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    write.table(paste("Fail",date(),allmodel,  sep = ", "),
                file = "backup.csv", append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
  }
  if(not.failed==1) {
    write.table(paste("Succ",date(),allmodel,  sep = ", "),
                file = "backup.csv", append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    fail.try=T
    try({
      #noVarImp.models=c("parRF")#var imp crashes with these models
      #if(allmodel %in% noVarImp.models){next()}#
      if(mean.improvement<0){mean.improvement=0}
      varimportant<-varImp(trainedmodel)
      write.table(paste(allmodel,date(),round(mean.improvement,digits=3),datasource,round(varimportant$importance,digits=1),  sep = ", "),
                  file = "importance.csv", append =TRUE, quote = F, sep = ",",
                  eol = "\n", na = "NA", dec = ".", row.names = F,
                  col.names = F, qmethod = "double")
      fail.try=F
    })
    if(fail.try==T){
      write.table(paste(allmodel,date(),round(mean.improvement,digits=3),datasource,"Failed",  sep = ", "),
                  file = "importance.csv", append =TRUE, quote = F, sep = ",",
                  eol = "\n", na = "NA", dec = ".", row.names = F,
                  col.names = F, qmethod = "double")
    }
  }
  
}
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
