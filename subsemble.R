#THIS IS SLOW 
#subset * models ^  2
#also broken. 2 or more subsets cause crashing possibly cause too few rows for only one algo

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
for(subsets in c(1)){ #,2,5
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
    fit <- subsemble(x=X_train, y=Y_train, verbose=T,
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
  if(!fail.try.main){
    custom_predict <- function(object, newdata) {
      pred <- predict(object, newdata, onlySL = T)$pred
      return(pred)
    }
    varimperm(custom_predict=custom_predict, modeltp=fit,
              X=X_holdout, Y=Y_holdout, metpack = "subsem_hold")
    varimperm(custom_predict=custom_predict, modeltp=fit,
              X=X_train, Y=Y_train, metpack = "subsem_train")
    }
  
  if(fail.try.main){    
failfail()
  }
}
}
