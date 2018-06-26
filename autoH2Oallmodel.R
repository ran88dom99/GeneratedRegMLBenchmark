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

########VARIEBLE IMPORTANCE
fail.try=T
try({ 
  #noVarImp.models=c("parRF")#var imp crashes with these models
  #if(allmodel %in% noVarImp.models){next()}#
  if(mean.improvement<0){mean.improvement=0}
  Rseed<-.Random.seed[1]
  Cseed<-.Random.seed[2]
  varimportant<-as.data.frame(h2o.varimp(aml@leader))
  print(varimportant)
  colNms<-as.vector(varimportant$variable)
  colImpor<-signif(varimportant$scaled_importance,digits = 3)
  varImpMix<-""#varImpMix<-vector(mode="character",length = length(colNms)*2)
  for(i in 1:length(colNms)){
    #varImpMix[i*2]<-colNms[i] ; varImpMix[i*2+1]<-colImpor[i]
    varImpMix<-paste(varImpMix,colNms[i],colImpor[i], sep = ",")
  }
  write.table(paste("h2oa",allmodel,date(),round(mean.improvement,digits=3),trans.y,
                    datasource,missingdata,withextra,norming,which.computer,task.subject,
                    FN,high.fold,Rseed,Cseed,seed.var,
                    varImpMix,  sep = ","),
              file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = F,
              col.names = F, qmethod = "double")
  fail.try=F
})
if (fail.try==T) {
  write.table(paste("h2oa",allmodel,date(),"FAIL",  sep = ","),
              file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = F,
              col.names = F, qmethod = "double")
}
}
fail.try=F
})
}
h2o.shutdown(prompt = F)
