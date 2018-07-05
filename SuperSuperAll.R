#superlearner' for allmodel's actual superlearner instead of just SL's models
#does not seem to produce exact same results for same seeds
#no automatic hyperparams 

#cv.iters<-11
#every auto package-library R script must automate 
#installation
#
#func(check for existence of crash or prevent repeat
#   set timer and write to task&algo crash files)
#
#randomseed in sometimes
#tune cv seed in
# 
#write to out results hyperparams also  oveRmse, !!prediction cycled
#write to out on fail
#varimp, func(internal, fail,)  


list.of.packages<-c("SuperLearner","RhpcBLASctl","biglasso","dbarts","sva","LogicReg","speedglm","KernelKnn")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dep = TRUE)
#For XGBoost we need to tweak the install command a bit; Windows users may need to install Rtools first.
#install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")
library(SuperLearner)

setwd(cpout.folder)

############################
# Setup example dataset.

set.seed(seed = seed.var)

# Identify predictors and response
y <- names(testing)[1]
x <- setdiff(names(training), y)

# X is our training sample.
X_train = training[,x]

# Create a holdout set for evaluating model performance.
# Note: cross-validation is even better than a single holdout sample.
X_holdout = testing[,x]

Y_train = training[,y]
Y_holdout = testing[,y]

problemms <- c("SL.template","SL.qda","SL.mean", "SL.lda","SL.knn","SL.leekasso","SL.logreg","SL.qda","SL.dbarts","SL.gbm")
super <- (SuperLearner::listWrappers())[69:110]
super <- setdiff(super,problemms)
methodsz <- c("method.NNLS"  , "method.NNLS2", "method.NNloglik", "method.CC_LS", "method.CC_nloglik")
######

for(itr in methodsz){
  # Review the outcome variable distribution.
  table(Y_train, useNA = "ifany")
  allmodel <- itr
  if(CrashNRep(allmodel)) {next()}

  fail.try.main<-T 
  try({
    # Set the seed for reproducibility.
    set.seed(seed = seed.var)
    
    #Let’s fit 2 separate models: lasso (sparse, penalized OLS) and randomForest. We specify family = binomial() because we are predicting a binary outcome, aka classification. With a continuous outcome we would specify family = gaussian().
    # Fit lasso model.
    when<-proc.time()
    
    if(itr=="method.NNLS"){
      fit_nnls <- SuperLearner(Y = Y_train, X = X_train, SL.library = super, 
                             verbose = TRUE, method = "method.NNLS",cvControl = list(V = cv.iters))
    } else {
      fit_nnls<- recombineSL(fit_nnls, Y = Y_train, method = itr)
    }
    
    summary(fit_nnls)
    print(fit_nnls)
    fit_nnls$coef
 
    predics<- predict(fit_nnls, X_holdout, onlySL = T)$pred
    trainpred<- predict(fit_nnls, X_train, onlySL = T)$pred 
    
    printPredMets(predicted.outcomes=predics,trainpred =trainpred ,hypercount="none")
    fail.try.main<-F  
  })
  
  if(fail.try.main){    
    print(c("failed","failed",date(),datasource,missingdata,withextra,norming,which.computer,task.subject,allmodel))
    write.table(paste("Fail","Fail","Fail","Fail","Fail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1],.Random.seed[2],seed.var,round(proc.time()[3]-when[3]),  sep = ","),
                file = out.file, append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
  }
}

