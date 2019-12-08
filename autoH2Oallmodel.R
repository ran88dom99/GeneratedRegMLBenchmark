#autoH2O for all auto models
#worrying: conversion to h2o data frame drops row names 
#put winning models as hyperparams of model 
#fail printout
#
setwd(cpout.folder)
library(h2o)


fail.try=F
for(itr in c(.1,1)){#,30

  
try({
  when<-proc.time()

# For binary classification, response should be a factor
#train[,y] <- as.factor(train[,y])
#test[,y] <- as.factor(test[,y])

  maxrun<-itr*tuneLength
  allmodel<-paste("h2oAutoml",as.character(maxrun),sep = " ")
  write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
  write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)

  if(!CrashNRep(allmodel)) {
    fail.try=T
    h2o.init()
    
    # Import a sample binary outcome train/test set into H2O
    train <- as.h2o(training)
    test <- as.h2o(testing)
    
    # Identify predictors and response
    y <- "V1"
    x <- setdiff(names(train), y)
    
    
    
aml <- h2o.automl( x=x, y = y,
                   max_runtime_secs = maxrun*60,
                   nfolds=cv.iters, seed=seed.var,
                  training_frame = train)

 #                  max_runtime_secs = 0,
#                  max_models = 20,max_models = 200,
#                  )

# View the AutoML Leaderboard, keep_cross_validation_models=FALSE
lb <- aml@leaderboard

lbdf<-as.data.frame(lb)
print(lbdf)

aml@leader

lbdf[1,3]
lbdf[1,1]
# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

pred <- h2o.predict(aml, test)  # predict(aml, test) also works
pred <- h2o.predict(aml@leader, test)
preddf<-as.data.frame(pred) #452434
row.names(preddf) <- row.names(testing)
print(preddf)

RANKSforNDCG<-NULL
if(predictNDCG){
NDCGtest<- as.h2o(df.forNDCG)
RANKSforNDCG <- h2o.predict(aml, NDCGtest)  # predict(aml, NDCGtest) also works
RANKSforNDCG <- h2o.predict(aml@leader, NDCGtest)
#row.names(RANKSforNDCG) <- row.names(df.forNDCG)
RANKSforNDCGdf <- data.frame(RANKSforNDCG,1)
RANKSforNDCG<-RANKSforNDCGdf[,1]
}
overRMSE<-lbdf[1,3]
printPredMets(predicted.outcomes=preddf,overRMSE=overRMSE,hypercount="full",libpack="autoH2O",RANKSforNDCG=RANKSforNDCG)


varimportant<-as.data.frame(h2o.varimp(aml@leader))
print(varimportant)
colNms<-as.vector(varimportant$names)
colImpor<-signif(varimportant$coefficients,digits = 3)
fail.try=F

if(fail.try){    
failfail()
} else {
  try({varimprint(metpack="h2oa",colNms=colNms,colImpor=colImpor)})
    try({ 
      custom_predict <- function(object, newdata) {
        test <- as.h2o(newdata)
        pred <- h2o.predict(object, test)
        preddf<-as.data.frame(pred) 
        return(preddf)
      }
      varimperm(custom_predict=custom_predict, modeltp=aml,
               X=testing[,-1], Y=testing[,1], metpack = "h2oa_hold",n_sample = 1000)
      varimperm(custom_predict=custom_predict, modeltp=aml,
                X=training[,-1], Y=training[,1], metpack = "h2oa_train",n_sample = 1000)
          })
}
}

})


}
try({h2o.shutdown(prompt = F)})
