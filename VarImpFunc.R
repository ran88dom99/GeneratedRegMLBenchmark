#ExplDALEX for allmodel
library("DALEX")

varimperm<-function(custom_predict, modeltp, X, Y, n_sample = 200, metpack = "unk")
  {
  ###VARIEBLE IMPORTANCE USING FRIEDMANS PERMUTE
  #n_sample : time to spend 
  #metpack is just an addition to name
  fail.try.vif=T
  if((mean.improvement<.05) && (Rsqd<.05)){return(NULL)}
  try({ 
    when<-proc.time()
    set.seed(seed=seed.var)
    expl_reg <- DALEX::explain(modeltp, data=X, y=Y,
                                    predict_function = custom_predict)
    
    vi_reg <- variable_importance(expl_reg, loss_function = loss_root_mean_square,
                                      type = "ratio", n_sample = n_sample)

    #noVarImp.models=c("parRF")#var imp crashes with these models
    #if(allmodel %in% noVarImp.models){next()}#

    Rseed <- .Random.seed[1]
    Cseed <- .Random.seed[2]
    metpack <- paste(metpack,"permute",sep = "_")
    
    varImpMix <- as.character(round(proc.time()[3]-when[3]))#varImpMix<-vector(mode="character",length = length(colNms)*2)
    for(i in 2:(length(vi_reg[,2])-1)){
      #varImpMix[i*2]<-colNms[i] ; varImpMix[i*2+1]<-colImpor[i]
      varImpMix<-paste(varImpMix,vi_reg[i,1],signif(vi_reg[i,2],digits = 4), sep = ",")
    }
    write.table(paste(metpack,allmodel,date(),round(mean.improvement,digits=3),trans.y,
                      datasource,missingdata,withextra,norming,which.computer,task.subject,
                      FN,high.fold,Rseed,Cseed,seed.var,
                      varImpMix,  sep = ","),
                file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    fail.try.vif=F
  })
  if (fail.try.vif==T) {
    write.table(paste(metpack,allmodel,date(),"FAIL",  sep = ","),
                file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
  }
}

varimprint<-function(metpack="unk",colNms=colNms,colImpor=colImpor)
  {
  ###VARIEBLE IMPORTANCE
  #combines 2 vectors and writes to varieable imprtance file
  #metpack is just an addition to name
  fail.try.vif=T
  try({ 
    #noVarImp.models=c("parRF")#var imp crashes with these models
    #if(allmodel %in% noVarImp.models){next()}#
    if(mean.improvement<0){mean.improvement=0}
    Rseed<-.Random.seed[1]
    Cseed<-.Random.seed[2]
    
    varImpMix<-""#varImpMix<-vector(mode="character",length = length(colNms)*2)
    for(i in 1:length(colNms)){
      #varImpMix[i*2]<-colNms[i] ; varImpMix[i*2+1]<-colImpor[i]
      varImpMix<-paste(varImpMix,colNms[i],colImpor[i], sep = ",")
    }
    write.table(paste(metpack,allmodel,date(),round(mean.improvement,digits=3),trans.y,
                      datasource,missingdata,withextra,norming,which.computer,task.subject,
                      FN,high.fold,Rseed,Cseed,seed.var,
                      varImpMix,  sep = ","),
                file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
    fail.try.vif=F
  })
  if (fail.try.vif==T) {
    write.table(paste(metpack,allmodel,date(),"FAIL",  sep = ","),
                file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")
  }
}

if(F){
  #object=sl_lasso; newdata=X_holdout
  custom_predict <- function(object, newdata) {
    pred <- predict(object, newdata, onlySL = T)$pred
    return(pred)
  }
  #pereds<-custom_predict(sl_lasso,X_holdout)
  
  explainer_reg_slasso <- DALEX::explain(sl_lasso, data=X_holdout, y=Y_holdout,
                                         predict_function = custom_predict, label="suprelpoorn")
  
  
  vi_regr_rf <- variable_importance(explainer_reg_slasso, loss_function = loss_root_mean_square, type = "ratio", n_sample = 10)
  print(vi_regr_rf)
  vi_regr_rf$variable
  str(vi_regr_rf)
  vig <- vi_regr_rf
  vig[9,2]
  plot( vi_regr_rf )
  
  
  mp_regr_rf <- model_performance(explainer_reg_slasso)
  plot(mp_regr_rf)
  
  modeltp<-sl_lasso
  X=X_holdout; Y=Y_holdout
}