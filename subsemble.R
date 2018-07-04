#THIS IS SLOW 
#subset * models ^  2

#every auto package-library R script must automate 
#installation
#write to task&algo crash files
#func(check for existence of crash or prevent repeat
#tune cv seed in
#write to out on fail
#write to out results including  oveRmse, hyperparams
#varimp, func(internal, fail,) prediction cycled 
#set timer

#install.packages("subsemble")


list.of.packages<-c("subsemble","SuperLearner","RhpcBLASctl","biglasso","dbarts","sva","LogicReg","speedglm","KernelKnn")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dep = TRUE)
#For XGBoost we need to tweak the install command a bit; Windows users may need to install Rtools first.
#install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")
library(SuperLearner)
library(subsemble)


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

problemms <- c("SL.template","SL.qda","SL.mean", "SL.lda","SL.knn","SL.leekasso","SL.logreg","SL.qda","SL.dbarts")
super <- (SuperLearner::listWrappers())[69:110]
super <- setdiff(super,problemms)
learner <- setdiff(super,problemms)

#if(F){learnControl$multiType="divisor";learnControl = list(multiType="divisor")}

cv <- cv.iters

######
for(subsets in c(1,2,5)){
for(itr in super){
  # Review the outcome variable distribution.
  table(Y_train, useNA = "ifany")
  allmodel<-paste0(itr,subsets,"subs")
  if(CrashNRep(allmodel)) {next()}
  write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
  write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)
  
  fail.try.main<-T
  try({
    # Set the seed for reproducibility.
    set.seed(seed = seed.var)
    
    #Let’s fit 2 separate models: lasso (sparse, penalized OLS) and randomForest. We specify family = binomial() because we are predicting a binary outcome, aka classification. With a continuous outcome we would specify family = gaussian().
    # Fit lasso model.
    when<-proc.time()
    

    metalearner <- itr
    fit <- subsemble(x=X_train, y=Y_train, 
                     learner = learner, metalearner = metalearner,
                     subsets = subsets, seed = seed.var, cvControl = list(V=cv))
    
    

    # Review the elements in the SuperLearner object.
    names(fit)
    
    # Here is the risk of the best model (discrete SuperLearner winner).
    #pred <- predict(fit, X_holdout)
    ## SL.glmnet_All 
    ##     0.1330516
    predics <- predict(fit, X_holdout, onlySL = T)$pred
    overRMSE <- RMSE(predict(fit, X_train, onlySL = T)$pred, Y_train)
    # Here is the raw glmnet result object:
    
    printPredMets(predicted.outcomes=predics,overRMSE=overRMSE,hypercount="none")
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
}
