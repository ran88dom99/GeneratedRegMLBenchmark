#autoH2O for all auto models
setwd(cpout.folder)
library(h2o)

h2o.init()

# Import a sample binary outcome train/test set into H2O
train <- as.h2o(training)
test <- as.h2o(testing)

# Identify predictors and response
y <- "V1"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
#train[,y] <- as.factor(train[,y])
#test[,y] <- as.factor(test[,y])

fail.try=T
try({
  allmodel<-"h2oAutoml"
  if(!CrashNRep(allmodel)) {
aml <- h2o.automl( x=x, y = y,
                   max_runtime_secs = 300,
                  training_frame = train)

#                  max_runtime_secs = 0,
#                  max_models = 20,
#                  nfolds=cv.iters, seed=seed.var)

# View the AutoML Leaderboard, keep_cross_validation_models=FALSE
lb <- aml@leaderboard

lbdf<-as.data.frame(lb)
print(lbdf)
aml@leader

# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

pred <- h2o.predict(aml, test)  # predict(aml, test) also works
preddf<-as.data.frame(pred)
# or:
#pred <- h2o.predict(aml@leader, test)
overRMSE<-(-1)#greedy_ensemble$error$RMSE
printPredMets(predicted.outcomes=preddf,overRMSE=overRMSE,hypercount="full")
}
fail.try=F
})
