#### THIS SCRIPT IS FOR TESTING MODELS. WHICH CAUSE caretEnsemble TO FAIL?
#devtools::install_github("zachmayer/caretEnsemble")
#update.packages(oldPkgs="caret", ask=FALSE)
sessionInfo()
adaptControl <-trainControl(method = "cv", number = 10,  search = "random")
library("caTools")
library("caretEnsemble")#"extraTrees","gbm",
stackmodels<-c( "rpart","glm","cubist","earth","bagEarth",
              "lasso","Rborist","rlm","nnet","pcaNNet","avNNet","pcr","ppr",
"enet",	"blassoAveraged",	"leapBackward","BstLm","gamboost","xgbTree","svmLinear2")
#
for (o in stackmodels) {
  for (p in stackmodels) {
    twomodels<-vector(mode = "character",length = 2)
    twomodels[1]<-o;twomodels[2]<-p;
    print(twomodels)
    writeout<- paste(c(twomodels),sep = ",")
    for(i in 2:length(writeout)){
      writeout[1]<-paste(writeout[1],writeout[i],sep=",")}
if(o==p) next()


    try({
set.seed(222)
model_list <- caretList(
  x=training[,-1],
  y=training[,1],
  trControl=adaptControl,
  methodList=twomodels
)
z <- as.data.frame(predict(model_list, newdata=head(testing[,-1])))
print(z)
#xyplot(resamples(model_list))
#modelCor(resamples(model_list))
    })
    failed<-1
try({
greedy_ensemble <- caretEnsemble(
  model_list
  )
summary(greedy_ensemble)

model_preds <- lapply(model_list, predict, newdata=testing[,-1], type="raw")
#model_preds <- lapply(model_preds, function(x) x[,"M"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=testing[,-1], type="raw")
model_preds$ensemble <- ens_preds
model_preds

varImp(greedy_ensemble)

overRMSE<-(-1)#greedy_ensemble$error$RMSE
allmodel<-"caretEnsGreedyGlm"
printPredMets(predicted.outcomes=ens_preds,overRMSE=overRMSE,hypercount="full")
failed<-0
})
if(failed==1) write.table(paste(writeout[1],"greedy",sep = ","),file = "carensfails.csv",  quote = F, sep = ",", row.names = F,col.names = F,append = T)
    
#caTools::colAUC(model_preds, testing$Class)

for (i in twomodels) {
  failed<-1
try({
stack_ensemble <- caretStack(
  model_list,
  method=i, 
  tuneLength=tuneLength,
  trControl=adaptControl
)
#$ens_model$finalModel
ens_preds <- predict(stack_ensemble, newdata=testing[,-1], type="raw")
overRMSE<-(-1)#min(stack_ensemble$error$RMSE, na.rm = T)
allmodel<-paste("caretEnstk",i,sep = " ")
printPredMets(predicted.outcomes=ens_preds,overRMSE=overRMSE,hypercount="full")
failed<-0
})
}
if(failed==1) write.table(paste(writeout[1],sep = ","),file = "carensfails.csv",  quote = F, sep = ",", row.names = F,col.names = F,append = T)
  }
}
