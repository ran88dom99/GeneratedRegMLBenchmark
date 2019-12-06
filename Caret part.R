setwd(cpout.folder)
###########for all models#################
for(allmodel in allmodels){#just before all models define d.f and reduce it

  write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
  write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)

  bad.models=union(bad.models,c("randomGLM","DENFIS","neuralnet","partDSA","blackboost","bstSm","bstTree","penalized","brnn",
                                "gamLoess","ANFIS","FIR.DM","FS.HGD","nodeHarvest","mlpWeightDecayML","monmlp","mlp","mlpWeightDecay",
                                "mlpSGD","rbf","rbfDDA","rfRules","GFS.FR.MOGUL","mlpML","HYFIS","GFS.THRIFT" ,"GFS.LT.RS","svmSpectrumString","svmExpoString","svmBoundrangeString"))
  #randomGLM "bartMachine","extraTrees",temp#too slow neuralnet# dnfis useless and just stops on huge datasets
  #svmSpectrumString svmExpoString svmBoundrangeString are only for strings? bag 
  if(allmodel %in% bad.models) {next()} #gamLoess crashes. the capitals are slow and terrible
  library(caret) #mlp...s creat some bizzare problem that breaks train ##nodeHarvest is SLOW ##"rbf"crash R "rbfDDA" crash train and really bad #rfRules is REALLY slow.##"pythonKnnReg",pythonKnnReg can not install
  #penalized slow then fails
  slow.models=c("gamSpline","leapSeq","glmStepAIC","ppr","qrnn","WM","plsRglm")#,"cubist","plsRglm","gamboost")#cubist, plsRglm,WM,gamboost  only sometimes
  if(allmodel %in% slow.models && datasource=="needles in haystack"){next()}#too slow for many columns
  if(allmodel %in% slow.models && datasource=="needles hay noise"){next()}#too slow for many columns
  slow.models=c("qrnn")
  #if(allmodel %in% slow.models){next()}#too slow for much cv
  noNA.models=c("kknn")#leapSeq
  if(allmodel %in% noNA.models && datasource=="sparsity NA"){next()}#too slow for many columns
  print(allmodel)

  #seed.var=seed.var+1
  if(length(df.previous.calcs[,1])>0){
    if(check.redundant(df=df.previous.calcs,norming=norming,trans.y=trans.y,withextra=withextra,missingdata=missingdata,datasource=datasource ,column.to.predict=column.to.predict,allmodel=allmodel,FN=FN))
      {next}}

  if(F){
  # unloading the NS 'object'
  pkgs = names(sessionInfo()$otherPkgs)
  pkgs = paste('package:', pkgs, sep = "")#detach
  lapply(pkgs,  detach, character.only = TRUE, unload = TRUE)
  library(caret)
  #library(caretEnsemble)
  library(MLmetrics)
  }

  gc()
  when<-proc.time()
  list.of.packages <-getModelInfo(allmodel)[[1]]$library
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, dep = TRUE)
  if(length(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])){
    write.table(paste("Fail","Fail","Fail","Fail","PackageFail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1],.Random.seed[2],seed.var,round(proc.time()[3]-when[3]),  sep = ","),
                file = out.file, append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    next()}


  not.failed=0
 
  try({trainedmodel <- train(x=data.frame(training[,2:length(training[1,])]),
                             y = df.toprocess[-foldTrain[[FN]],1],
                             method = allmodel,
                             trControl = adaptControl,
                             tuneLength = tuneLength)

  predicted.outcomes<-predict(trainedmodel, newdata=(testing[,-1]))
  if(predictNDCG) NDCGpredics<-predict(trainedmodel, newdata=( df.forNDCG[,-1]))
  
  #get trainer function's metrics
  wut=print(trainedmodel,selectCol=TRUE)
  overRMSE=-1
  try({overRMSE=as.numeric(wut[wut[,length(wut[1,])]=="*","RMSE"])})#length(wut[1,])-3]
  replace.overRMSE=1
  try({if(is.numeric(overRMSE)){replace.overRMSE=0}})
  if(replace.overRMSE==1){overRMSE=-1}
  if(length(overRMSE)<1){overRMSE=-1}
  
  printPredMets(predicted.outcomes=predicted.outcomes,overRMSE=overRMSE,hypercount="full",RANKSforNDCG=NDCGpredics)
  
  not.failed=1
  })

  if(not.failed==0) {
    try({trainedmodel <- train(x=data.frame(training[,2:length(training[1,])]),
                               y =  df.toprocess[-foldTrain[[FN]],1],
                               method = allmodel,
                               trControl = simpleControl,
                               tuneLength = tuneLength2)

    
    predicted.outcomes<-predict(trainedmodel, newdata=(testing[,-1]))
    if(predictNDCG) NDCGpredics<-predict(trainedmodel, newdata=( df.forNDCG[,-1]))
    
    overRMSE=-1
    wut=print(trainedmodel,selectCol=TRUE)
    try({overRMSE=as.numeric(wut[wut[,length(wut[1,])]=="*","RMSE"])})#length(wut[1,])-
    replace.overRMSE=1
    try({if(is.numeric(overRMSE)){replace.overRMSE=0}})
    if(replace.overRMSE==1){overRMSE=-1}
    if(length(overRMSE)<1){overRMSE=-1}
 
    printPredMets(predicted.outcomes=predicted.outcomes,overRMSE=overRMSE,hypercount="part",RANKSforNDCG=NDCGpredics)
    
    not.failed=1
    })
  }
  ########no hp&cv  goes here
  if(not.failed==0) {
    try({trainedmodel <- train(x=data.frame(training[,2:length(training[1,])]),
                               y =  df.toprocess[-foldTrain[[FN]],1],
                               method = allmodel)
    
    predicted.outcomes<-predict(trainedmodel, newdata=(testing[,-1]))
    if(predictNDCG) NDCGpredics<-predict(trainedmodel, newdata=( df.forNDCG[,-1]))
    
    overRMSE=-1
    wut=print(trainedmodel,selectCol=TRUE)
    try({wut=print(trainedmodel,selectCol=TRUE)
    overRMSE=as.numeric(wut[wut[,length(wut[1,])]=="*","RMSE"])})#length(wut[1,])-
    replace.overRMSE=1
    try({if(is.numeric(overRMSE)){replace.overRMSE=0}})
    if(replace.overRMSE==1){overRMSE=-1}
    if(length(overRMSE)<1){overRMSE=-1}
    
    printPredMets(predicted.outcomes=predicted.outcomes,overRMSE=overRMSE,hypercount="none",RANKSforNDCG=NDCGpredics)
    
    not.failed=1
    })
  }
  if(not.failed==0) {
failfail()
  }
  if(not.failed==1) {
    fail.try=T
    ########VARIEBLE IMPORTANCE
    try({ 
      #noVarImp.models=c("parRF")#var imp crashes with these models
      #if(allmodel %in% noVarImp.models){next()}#
      if(mean.improvement<0){mean.improvement=0}
      Rseed<-.Random.seed[1]
      Cseed<-.Random.seed[2]
      varimportant<-varImp(trainedmodel)
      colNms<-row.names.data.frame(as.data.frame(varimportant$importance))
      colImpor<-signif(varimportant$importance$Overall,digits = 3)
      varImpMix<-""#varImpMix<-vector(mode="character",length = length(colNms)*2)
      for(i in 1:length(colNms)){
        #varImpMix[i*2]<-colNms[i] ; varImpMix[i*2+1]<-colImpor[i]
        varImpMix<-paste(varImpMix,colNms[i],colImpor[i], sep = ",")
      }
       write.table(paste("caret",allmodel,date(),round(mean.improvement,digits=3),trans.y,
                         datasource,missingdata,withextra,norming,which.computer,task.subject,
                         FN,high.fold,Rseed,Cseed,seed.var,
                         varImpMix,  sep = ","),
                  file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
                  eol = "\n", na = "NA", dec = ".", row.names = F,
                  col.names = F, qmethod = "double")
      fail.try=F
    })
    if(fail.try==F){
      try({
        #object=sl_lasso; newdata=X_holdout
        custom_predict <- function(object, newdata) {
          pred <- predict(object, newdata) 
          return(pred)
        }
        varimperm(custom_predict=custom_predict, modeltp=trainedmodel,
                  X=testing[,-1], Y=testing[,1], metpack = "caret_hold")
        varimperm(custom_predict=custom_predict, modeltp=trainedmodel,
                  X=training[,-1], Y=training[,1], metpack = "caret_train")
      })
    }
    if(fail.try==T){
      write.table(paste("caret",allmodel,date(),round(mean.improvement,digits=3),datasource,"Failed",  sep = ","),
                  file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
                  eol = "\n", na = "NA", dec = ".", row.names = F,
                  col.names = F, qmethod = "double")
    }
  }

}
setwd(base.folder)
