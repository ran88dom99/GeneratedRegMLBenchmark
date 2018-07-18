#svm with Radial Basis Function is the most recommended learner.
#Get hypertopt's outputs for the  5 models pls
#pkgs = names(sessionInfo()$otherPkgs)
#if(length(pkgs)>0){
#  print(data())
#  pkgs = paste('package:', pkgs, sep = "")
#  lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
#}
library(e1071)
library(ParamHelpers)
library(mlr)
library(mlbench)
library(mlrHyperopt)
library(MLmetrics)

setwd(cpout.folder)

imp = impute(training, classes = list(numeric = 0),
             dummy.classes = "numeric", dummy.type = "numeric")
training<-imp$data
imp = impute(testing, classes = list(numeric = 0),
             dummy.classes = "numeric", dummy.type = "numeric")
testing<-imp$data

configureMlr(on.learner.error = "warn")
regr.task = makeRegrTask(id = "recc", data = training, target = "V1")
mlrallmodels<-listLearners("regr")

resampler<-makeResampleDesc("CV",iters=mlr.iters)
hyper.control<-makeHyperControl(mlr.control = makeTuneControlIrace(maxExperiments=5L),#makeTuneControlRandom(maxit=tuneLength)
                                resampling = resampler,
                                measures = rmse)
hyper.control.rand<-makeHyperControl(mlr.control = makeTuneControlRandom(maxit=tuneLengthMLR),
                                     resampling = resampler,
                                     measures = rmse)

#res = hyperopt(regr.task, learner = "regr.svm", hyper.control =hyper.control)
#res

#mlrallmodels<-"regr.bartMachine"


try({
  methods = c("mrmr","randomForestSRC.rfsrc", "univariate.model.score")
fv = generateFilterValuesData(regr.task,
                              method = c("mrmr","randomForestSRC.rfsrc", "univariate.model.score"),
                              nselect<-10)#,,"permutation.importance","randomForestSRC.var.select"
plotFilterValues(fv)#issues errors"cforest.importance",,more.args = list(imp.learner<-"regr.cubist")
restr.df<-data.frame()
for(i in 1:length(fv$data[,1])){
  restr.df[i,1]<-paste(fv$data[i,1], signif(fv$data[i,3],digits = 3),sep = ",")
  restr.df[i,2]<-paste(fv$data[i,1], signif(fv$data[i,4],digits = 3),sep = ",")
  restr.df[i,3]<-paste(fv$data[i,1], signif(fv$data[i,5],digits = 3),sep = ",")
}

for(n in 1:3){
for(i in 2:length(restr.df[,n])){
  restr.df[1,n]<-paste(restr.df[1,n],restr.df[i,n],sep=",")
}
  
  Rseed<-.Random.seed[1]
  Cseed<-.Random.seed[2]
  
write.table(paste("mlr",methods[n],date(),"",trans.y,datasource,missingdata,withextra,norming,
                  which.computer,task.subject,FN,high.fold,
                  Rseed,Cseed,seed.var,"",restr.df[1,n],  sep = ","),
            file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
}
})


######for all mlr models########
for(allmodel in mlrallmodels[[1]]){#just before all models define d.f and reduce it

  when<-proc.time()
  write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
  write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)

   bad.models=union(bad.models,c("regr.bgpllm","regr.btg","regr.bgp","regr.btgp","regr.btgpllm", "regr.GPfit",
                                 "neuralnet","partDSA","blackboost","bstSm","bstTree","penalized","brnn","gamLoess",
                                 "ANFIS","FIR.DM","FS.HGD","nodeHarvest","mlpWeightDecayML","monmlp","mlp","mlpWeightDecay","mlpSGD","rbf","rbfDDA","rfRules","GFS.FR.MOGUL","mlpML","HYFIS","GFS.THRIFT" ,"GFS.LT.RS"))
#   bad.models=union(bad.models,c("regr.nodeHarvest"  ,            
#                   "regr.penalized"     ,   "regr.plsr"        ,     "regr.randomForest"  ,  
#                                  "regr.randomForestSRC",  "regr.ranger" ,          "regr.rknn"   ,         
 #                                         "regr.RRF"          ,    "regr.slim"     , "regr.svm"   ,  "regr.xgboost" )) #skip model
   #too slow neuralnet# dnfis useless and just stops on huge datasets
  if(allmodel %in% bad.models) {next()} #gamLoess crashes. the capitals are slow and terrible
  library(caret) #mlp...s creat some bizzare problem that breaks caret::train ##nodeHarvest is SLOW ##"rbf"crash R "rbfDDA" crash train and really bad #rfRules is REALLY slow.##"pythonKnnReg",pythonKnnReg can not install
  pass.sometimes<-c("regr.LiblineaRL2L2SVR")
  if((allmodel %in% pass.sometimes) &&  ("expoTrans"==norming)) {next()}
  #penalized slow then fails"Boston Housing"==datasource &&
  slow.models=c("leapSeq","glmStepAIC","ppr","qrnn")#leapSeq
  if(allmodel %in% slow.models && datasource=="needles in haystack"){next()}#too slow for many columns
  if(allmodel %in% slow.models && datasource=="needles hay noise"){next()}#too slow for many columns
  slow.models=c("qrnn")
  if(allmodel %in% slow.models){next()}#too slow for much cv
  noNA.models=c("kknn")#leapSeq
  if(allmodel %in% noNA.models && datasource=="sparsity NA"){next()}#too slow for many columns
  print(allmodel)

  #seed.var=seed.var+1

  if(length(df.previous.calcs[,1])>0){
    if(check.redundant(df=df.previous.calcs,norming=norming,trans.y=trans.y,withextra=withextra,missingdata=missingdata,datasource=datasource ,column.to.predict=column.to.predict,allmodel=allmodel,FN=FN))
      {next}}



  error.pack=0
  try({list.of.packages <-getLearnerPackages(allmodel)
  error.pack=1})
  if(error.pack==0){
    write.table(paste("Fail","Fail","Fail","Fail","PrePackageFail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,round(proc.time()[3]-when[3]),  sep = ","),
                file = out.file, append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    next()}
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, dep = TRUE)
  if(length(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])){
    write.table(paste("Fail","Fail","Fail","Fail","PackageFail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,round(proc.time()[3]-when[3]),  sep = ","),
                file = out.file, append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    next()}


  when<-proc.time()

  not.failed=0
  try({
    mod<-hyperopt(regr.task, learner = allmodel, hyper.control =hyper.control.rand)
    lrn = setHyperPars(makeLearner(allmodel), par.vals = mod$x)
    m = train(lrn, regr.task)

  #keep rmse but train new model on mod$x's parameters
  
  predicted.outcomes<-predict(m, newdata=(testing[,-1]))
  #predicted.outcomes<-predict(mod, newdata=(testing[,-1]))
  
  
  overRMSE=-1
  overRMSE<-mod$y ####WHY MOD NOT M!!!
  #if(replace.overRMSE==1){overRMSE=-1}
  if(length(overRMSE)<1){overRMSE=-1}
  
  printPredMets(predicted.outcomes=predicted.outcomes$data[,1],overRMSE=overRMSE,hypercount="full",libpack="mlr")
  
  not.failed=1
  })

  #if hyperopt failed just use no hypering
  try({if(not.failed==0) {
    mod<-  train(allmodel, regr.task)
    
    predicted.outcomes<-predict(mod, newdata=(testing[,-1]))
    train.outcomes<-predict(mod, newdata=(training[,-1]))
  
  overRMSE=-1
  try({overRMSE<-mod$learner.model$rmse_train 
  if(length(overRMSE)<1){overRMSE=-1}
  if( overRMSE==-1) {
  if(trans.y==2){
    overRMSE<-RMSE(train.outcomes$data[,1],y.untransformed[-foldTrain[[FN]]])
  }else{
    overRMSE<-RMSE(predict(loess.model,train.outcomes$data[,1]),y.untransformed[-foldTrain[[FN]]])
  }
  }})
  #if(replace.overRMSE==1){overRMSE=-1}
  if(length(overRMSE)<1){overRMSE=-1}
  
  printPredMets(predicted.outcomes=predicted.outcomes$data[,1],overRMSE=overRMSE,hypercount="none",libpack="mlr")
  
  m<-mod
    not.failed=1
  }})
  if(not.failed==1){
    try({ 
      custom_predict <- function(object, newdata) {
        pred <- predict(object, newdata=newdata)$data 
        return(pred)
      }
      varimperm(custom_predict=custom_predict, modeltp=m,
                X=testing[,-1], Y=testing[,1], metpack = "mlr_hold")
      varimperm(custom_predict=custom_predict, modeltp=m,
                X=training[,-1], Y=training[,1], metpack = "mlr_train")
    })
  }
  if(not.failed==0) {
  failfail()
  }
 
}
setwd(base.folder)
