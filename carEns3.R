#caretEnsamble script for allmodel tester
#devtools::install_github("zachmayer/caretEnsemble")
#update.packages(oldPkgs="caret", ask=FALSE)
#sessionInfo()
#adaptControl <-trainControl(method = "cv", number = 10,  search = "random")
#todo varimp
library("caTools")
library("caretEnsemble")#"extraTrees","gbm",
stackmodels<-c( "rpart","glm","cubist","earth","bagEarth",
                "lasso","Rborist","rlm","nnet","pcaNNet","avNNet","pcr","ppr",
                "enet",	"blassoAveraged",	"leapBackward","BstLm","gamboost","xgbTree","svmLinear2")

allmodel<-"caretPreEns"
write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)

    try({
      set.seed(222)
      model_list <- caretList(
        x=training[,-1],
        y=training[,1],
        trControl=adaptControl,
        methodList=stackmodels
      )
      z <- as.data.frame(predict(model_list, newdata=head(testing[,-1])))
      print(z)
      #xyplot(resamples(model_list))
      #modelCor(resamples(model_list))
    })
    
    allmodel<-"caretEnsGreedyGlm"
    write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
    write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)

    failed<-1
    try({
      if(allmodel %in% bad.models) {next()} #does next() exit try({})
      greedy_ensemble <- caretEnsemble(
        model_list
      )
      summary(greedy_ensemble)
      
      #model_preds <- lapply(model_list, predict, newdata=testing[,-1], type="raw")
      #model_preds <- data.frame(model_preds)
      ens_preds <- predict(greedy_ensemble, newdata=testing[,-1], type="raw")
      #model_preds$ensemble <- ens_preds
      #model_preds
      
      varImp(greedy_ensemble)
      
      overRMSE<-(-1)#greedy_ensemble$error$RMSE
      allmodel<-"caretEnsGreedyGlm"
      printPredMets(predicted.outcomes=ens_preds,overRMSE=overRMSE,hypercount="full")
      failed<-0
    })
    if(failed==1) write.table(paste("greedy",sep = ","),file = "carensfails.csv",  quote = F, sep = ",", row.names = F,col.names = F,append = T)
    
    #caTools::colAUC(model_preds, testing$Class)
    
    for (i in stackmodels) {
      allmodel<-paste("caretEnstk",i,sep = " ")
      write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
      write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)
      if(allmodel %in% bad.models) {next()}
      
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
        printPredMets(predicted.outcomes=ens_preds,overRMSE=overRMSE,hypercount="full")
        failed<-0
      })
    }
    if(failed==1) write.table(paste(i,sep = ","),file = "carensfails.csv",  quote = F, sep = ",", row.names = F,col.names = F,append = T)

