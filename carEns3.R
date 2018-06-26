#caretEnsamble script for allmodel tester
#which models stack better?


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
fail.try=T
    try({
      
      set.seed(222)
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
    if(fail.try) {print("MAJOR FAIL in CARET.ENS")}
 
    failed<-1
    try({
      allmodel<-"caretEnsGreedyGlm"
      if(!CrashNRep(allmodel)) { #does next() exit try({})
      
      greedy_ensemble <- caretEnsemble(
        model_list
      )
      summary(greedy_ensemble)
      
      #model_preds <- lapply(model_list, predict, newdata=testing[,-1], type="raw")
      #model_preds <- data.frame(model_preds)
      ens_preds <- predict(greedy_ensemble, newdata=testing[,-1], type="raw")
      #model_preds$ensemble <- ens_preds
      #model_preds
      overRMSE<-(-1)#greedy_ensemble$error$RMSE
      printPredMets(predicted.outcomes=ens_preds,overRMSE=overRMSE,hypercount="full")
      failed<-0

      ########VARIEBLE IMPORTANCE
      fail.try=T
      try({ 
        #noVarImp.models=c("parRF")#var imp crashes with these models
        #if(allmodel %in% noVarImp.models){next()}#
        if(mean.improvement<0){mean.improvement=0}
        Rseed<-.Random.seed[1]
        Cseed<-.Random.seed[2]
        varimportant<-varImp(greedy_ensemble)
        print(varimportant)
        colNms<-row.names.data.frame(as.data.frame(varimportant))
        colImpor<-signif(varimportant$overall,digits = 3)
        varImpMix<-""#varImpMix<-vector(mode="character",length = length(colNms)*2)
        for(i in 1:length(colNms)){
          #varImpMix[i*2]<-colNms[i] ; varImpMix[i*2+1]<-colImpor[i]
          varImpMix<-paste(varImpMix,colNms[i],colImpor[i], sep = ",")
        }
        write.table(paste("caretEns",allmodel,date(),round(mean.improvement,digits=3),trans.y,
                          datasource,missingdata,withextra,norming,which.computer,task.subject,
                          FN,high.fold,Rseed,Cseed,seed.var,
                          varImpMix,  sep = ","),
                    file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
                    eol = "\n", na = "NA", dec = ".", row.names = F,
                    col.names = F, qmethod = "double")
        fail.try=F
      })
      if (fail.try==T) {
        write.table(paste("caretEns",allmodel,date(),"FAIL",  sep = ","),
                                           file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
                                           eol = "\n", na = "NA", dec = ".", row.names = F,
                                           col.names = F, qmethod = "double")
      }
      }

    })
    if(failed==1) write.table(paste("greedy",sep = ","),file = "carensfails.csv",  quote = F, sep = ",", row.names = F,col.names = F,append = T)
    print("carens 3")
    
    #caTools::colAUC(model_preds, testing$Class)
    
    for (i in stackmodels) {
      allmodel<-paste("caretEnstk",i,sep = " ")
      print(i)
       if(CrashNRep(allmodel)) {next()}
 
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