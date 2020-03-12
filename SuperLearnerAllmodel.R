#superlearner for allmodel
#no varimp #own, later
#no hyperparams 
#superlearner has special ensembling methods ? but must save models for it. 


list.of.packages <- c("SuperLearner","RhpcBLASctl","biglasso","dbarts","sva","LogicReg","speedglm","KernelKnn")
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

problemms<-c("SL.template","SL.qda","SL.mean", "SL.lda","SL.knn")
super<-(SuperLearner::listWrappers())[69:110]
#super<-setdiff(super,problemms)

######

for(itr in super){
# Review the outcome variable distribution.
table(Y_train, useNA = "ifany")
allmodel<-itr
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

sl_lasso = SuperLearner(Y = Y_train, X = X_train, family = gaussian(),
                        SL.library = itr,cvControl = list(V = cv.iters))

print(sl_lasso)
# Review the elements in the SuperLearner object.
names(sl_lasso)

# Here is the risk of the best model (discrete SuperLearner winner).
overRMSE<-sl_lasso$cvRisk[which.min(sl_lasso$cvRisk)]

## SL.glmnet_All 
##     0.1330516
predics <- predict(sl_lasso, X_holdout, onlySL = T)$pred
bigpredict <- proc.time()
if(predictNDCG) NDCGpredics <- predict(sl_lasso, df.forNDCG[,x], onlySL = T)$pred
print(proc.time()-bigpredict)
# Here is the raw glmnet result object:

printPredMets(predicted.outcomes=predics,overRMSE=overRMSE,hypercount="none",RANKSforNDCG=NDCGpredics)
fail.try.main<-F  
})
if(!fail.try.main){
  #object=sl_lasso; newdata=X_holdout
  custom_predict <- function(object, newdata) {
    pred <- predict(object, newdata, onlySL = T)$pred
    return(pred)
  }
  varimperm(custom_predict=custom_predict, modeltp=sl_lasso,
            X=X_holdout, Y=Y_holdout,R=training[,-1], metpack = "SL1_hold")
  varimperm(custom_predict=custom_predict, modeltp=sl_lasso,
            X=X_train, Y=Y_train,R=training[,-1], metpack = "SL1_train")
  }

  if(fail.try.main){    
failfail()
  }
}
