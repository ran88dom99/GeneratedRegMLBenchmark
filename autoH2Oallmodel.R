#autoH2O for all auto models
#conversion to h2o data frame drops row names and this is worrying
#
setwd(cpout.folder)
library(h2o)

for(itr in c(.1,1,10,30)){
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
  maxrun<-itr*tuneLength
  allmodel<-paste("h2oAutoml",as.character(maxrun),sep = " ")
  if(!CrashNRep(allmodel)) {
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
# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

pred <- h2o.predict(aml, test)  # predict(aml, test) also works
pred <- h2o.predict(aml@leader, test)
preddf<-as.data.frame(pred) #452434
row.names(preddf) <- row.names(testing)
print(preddf)

overRMSE<-lbdf[1,3]
printPredMets(predicted.outcomes=preddf,overRMSE=overRMSE,hypercount="full")
}
fail.try=F
})
}