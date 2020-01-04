#caretEnsamble script for allmodel tester
#which models stack better?
#little varimp
#ensembles out only; main models not


#devtools::install_github("zachmayer/caretEnsemble")
#update.packages(oldPkgs="caret", ask=FALSE)
#sessionInfo()
#adaptControl <-trainControl(method = "cv", number = 10,  search = "random")
setwd(cpout.folder)

library("caTools")
library("caretEnsemble")#"extraTrees","gbm",
stackmodels<-c( "rpart","glm","cubist","earth","bagEarth",
                "lasso","Rborist","rlm","nnet","pcaNNet","avNNet","pcr","ppr",
                "enet",	"blassoAveraged",	"leapBackward","BstLm","gamboost","xgbTree",
                "svmLinear2","kknn","knn")

allmodel<-"caretPreEns"
write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)
print("carens first")
for(dummy in 1){
fail.try=T
    try({
      
      set.seed(seed.var)
      model_list <- caretList(
        x=training[,-1],
        y=training[,1],
        trControl=adaptControl,
        methodList=stackmodels
      )
      fail.try=F
      z <- as.data.frame(predict(model_list, newdata=head(testing[,-1])))
      print(z)
      #xyplot(resamples(model_list))
      #modelCor(resamples(model_list))
    })
    print("carens sec")
    if(fail.try) {
      allmodel<-"MAJOR FAIL in CARET.ENS"
      print(c("failed","failed",date(),datasource,missingdata,withextra,norming,which.computer,task.subject,allmodel))
      write.table(paste("Fail","Fail","Fail","Fail","Fail","Fail","Fail","Fail","Fail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1],.Random.seed[2],seed.var,round(proc.time()[3]-when[3]),  sep = ","),
                  file = out.file, append =TRUE, quote = F, sep = ",",
                  eol = "\n", na = "NA", dec = ".", row.names = F,
                  col.names = F, qmethod = "double") 
      print(allmodel)
      next()
      }
 
    failed<-1
    try({
      allmodel<-"caretEnsGreedyGlm"
      if(!CrashNRep(allmodel)) { #does next() exit try({})
        when<-proc.time()
        
      greedy_ensemble <- caretEnsemble(
        model_list
      )
      summary(greedy_ensemble)
      
      #model_preds <- lapply(model_list, predict, newdata=testing[,-1], type="raw")
      #model_preds <- data.frame(model_preds)
      ens_preds <- predict(greedy_ensemble, newdata=testing[,-1], type="raw")
      if(predictNDCG) NDCGpredics<-predict(greedy_ensemble, newdata=df.forNDCG[,-1], type="raw")
      
      #model_preds$ensemble <- ens_preds
      #model_preds
      overRMSE<-(-1)#greedy_ensemble$error$RMSE
      printPredMets(predicted.outcomes=ens_preds,overRMSE=overRMSE,hypercount="full",RANKSforNDCG=NDCGpredics)

      varimportant<-varImp(greedy_ensemble)
      print(varimportant)
      colNms<-row.names.data.frame(as.data.frame(varimportant))
      colImpor<-signif(varimportant$overall,digits = 3)
      varimprint(metpack="caretEns",colNms=colNms,colImpor=colImpor)
      }
      failed<-0
    })
    if(failed==1) {
        print(c("failed","failed",date(),datasource,missingdata,withextra,norming,which.computer,task.subject,allmodel))
        write.table(paste("Fail","Fail","Fail","Fail","Fail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1],.Random.seed[2],seed.var,round(proc.time()[3]-when[3]),  sep = ","),
                    file = out.file, append =TRUE, quote = F, sep = ",",
                    eol = "\n", na = "NA", dec = ".", row.names = F,
                    col.names = F, qmethod = "double")    
        }  
    print("carens 3")
    
    #caTools::colAUC(model_preds, testing$Class)
    
    for (i in stackmodels) {
      allmodel<-paste("caretEnstk",i,sep = " ")
      print(i)
       if(CrashNRep(allmodel)) {next()}
 
      failed<-1
      try({
        when<-proc.time()
        
        stack_ensemble <- caretStack(
          model_list, method=i, 
          tuneLength=tuneLength,  trControl=adaptControl
        )
        
        #$ens_model$finalModel
        ens_preds <- predict(stack_ensemble, newdata=testing[,-1], type="raw")
        if(predictNDCG) NDCGpredics<-predict(stack_ensemble, newdata=df.forNDCG[,-1], type="raw")
        overRMSE<-(-1)#min(stack_ensemble$error$RMSE, na.rm = T)
        printPredMets(predicted.outcomes=ens_preds,overRMSE=overRMSE,hypercount="full",RANKSforNDCG=NDCGpredics)
        failed<-0
      })
      if(failed==0){
        try({ 
        custom_predict <- function(object, newdata) {
          pred <- predict(object, newdata, type="raw") 
          return(pred)
        }
        varimperm(custom_predict=custom_predict, modeltp=stack_ensemble,
                  X=testing[,-1], Y=testing[,1], metpack = "carEns_hold")
        varimperm(custom_predict=custom_predict, modeltp=stack_ensemble,
                  X=training[,-1], Y=training[,1], metpack = "carEns_train")
        
          })
      }
      
      if(failed==1) {
 failfail()   
      }
    }
}