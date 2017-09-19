R.utils::gcDLLs()
list.of.packages <- c("ParamHelpers","devtools")
#list.of.packages <- c("caretEnsemble","logicFS"," RWeka","ordinalNet","xgboost","mlr","caret","MLmetrics","bartMachine","spikeslab","party","rqPen","monomvn","foba","logicFS","rPython","qrnn","randomGLM","msaenet","Rborist","relaxo","ordinalNet","rrf","frbs","extraTrees","ipred","elasticnet","bst","brnn","Boruta","arm","elmNN","evtree","extraTrees","deepnet","kknn","KRLS","RSNNS","partDSA","plsRglm","quantregForest","ranger","inTrees")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dep = TRUE)

#devtools::install_github("berndbischl/ParamHelpers") # version >= 1.11 needed.
#devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)
library(mlr)
library(mlbench)
configureMlr(on.learner.error = "warn")
regr.task = makeRegrTask(id = "recc", data = training, target = "V1")

hyper.control<-makeHyperControl(mlr.control = makeTuneControlRandom(maxit = 20),
                                resampling = cv3,
                                measures = mse)
library(mlrHyperopt)
res = hyperopt(regr.task, learner = "regr.svm", hyper.control =hyper.control)
res

mlrallmodels<-listLearners("regr")
######for all mlr models########
for(allmodel in mlrallmodels[[1]]){#just before all models define d.f and reduce it
  write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
  bad.models=c("regr.GPfit","neuralnet","partDSA","blackboost","bstSm","bstTree","penalized","brnn","gamLoess","ANFIS","FIR.DM","FS.HGD","nodeHarvest","mlpWeightDecayML","monmlp","mlp","mlpWeightDecay","mlpSGD","rbf","rbfDDA","rfRules","GFS.FR.MOGUL","mlpML","HYFIS","GFS.THRIFT" ,"GFS.LT.RS")
  #too slow neuralnet# dnfis useless and just stops on huge datasets
  if(allmodel %in% bad.models) {next()} #gamLoess crashes. the capitals are slow and terrible
  library(caret) #mlp...s creat some bizzare problem that breaks caret::train ##nodeHarvest is SLOW ##"rbf"crash R "rbfDDA" crash train and really bad #rfRules is REALLY slow.##"pythonKnnReg",pythonKnnReg can not install
  #penalized slow then fails
  slow.models=c("leapSeq","glmStepAIC","ppr","qrnn")#leapSeq
  if(allmodel %in% slow.models && datasource=="needles in haystack"){next()}#too slow for many columns
  if(allmodel %in% slow.models && datasource=="needles hay noise"){next()}#too slow for many columns
  slow.models=c("qrnn")
  if(allmodel %in% slow.models){next()}#too slow for much cv
  noNA.models=c("kknn")#leapSeq
  if(allmodel %in% noNA.models && datasource=="sparsity NA"){next()}#too slow for many columns
  
  seed.var=seed.var+1
  

  error.pack=0
  try({list.of.packages <-getLearnerPackages(allmodel)
  error.pack=1})
  if(error.pack==0){next()}
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, dep = TRUE)
  if(length(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])){
    write.table(paste("Fail","Fail","Fail","Fail","PackageFail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,round(proc.time()[3]-when[3]),  sep = ","),
                file = "gen test out.csv", append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    next()}

  
  when<-proc.time()
  if(length(df.previous.calcs[,1])>0){
    if(check.redundant(df=df.previous.calcs,norming=norming,trans.y=trans.y,withextra=withextra,missingdata=missingdata,datasource=datasource ,column.to.predict=column.to.predict,allmodel=allmodel)){next}}
  not.failed=0
  set.seed(seed.var)
  try({mod<-train(allmodel, regr.task)#hyperopt(regr.task, learner = allmodel, hyper.control =hyper.control)

  
  predicted.outcomes<-predict(mod, newdata=(testing))
  p <- data.frame(predicted.outcomes$data[,2],testing[,1])
  #Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
  Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],mean(p[,2], na.rm = T))
  #mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
  mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],mean(p[,2], na.rm = T))
  p<- data.frame(predict(loess.model,predicted.outcomes$data[,2]),y.untransformed[-inTrain])
  #RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
  RMSE=RMSE(p[,1],p[,2])
  #RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
  RMSE.mean=RMSE(p[,2],mean(p[,2], na.rm = T))
  #MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)
  MMAAEE=MAE(p[,1],p[,2])
  
  
  overRMSE=-1
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
                col.names = F, qmethod = "double")}
}
