#ExplDALEX for allmodel
require("DALEX")

#varimperm(custom_predict=custom_predict, modeltp=trainedmodel,X=testing[,-1], Y=testing[,1], metpack = "caret_hold")
#varimperm(custom_predict=custom_predict, modeltp=trainedmodel, X=training[,-1], Y=training[,1], metpack = "caret_train")

#lots of issues; could just use the stable median or test all costants at cost to time?
#use data from outside dataset to compute the constants? would stableize median-mean?
#multiple lossfunctions?
#bounding resulting statistic (why did dalex give negative numbers and huge numbers?)

#check for spearman!! not "mean.improvement"

varimperm <- function(custom_predict, modeltp, X, Y, R, n_sample = 20, metpack = "unk", lossfunction="pearson")
{
  fail.try.vif=T
  
  #permutation causes extra variability; more error than expected.
  #eg variable with few great spikes translate into double the "empty" error
  #if mean or mode is used the actual missing variance should be found?
  if(lossfunction=="rmse"){
    lossf<-function(trg,modl,ndta){
      return(sqrt(mean((custom_predict(modl, ndta) - trg)^2)))
    }
    baseline<-RMSE(Y,train.based.mean)
    vif<-function(org,solv,baseline){
      solv<-min(solv)
      if(baseline<=org) print("mean base is somehow less errored than model")
      return((solv-org)/(baseline-org))
    }
  }
  
  if(lossfunction=="pearson"){
    lossf<-function(trg,modl,ndta){
      return(cor(custom_predict(modl, ndta),trg)^2)
    }
    baseline<-0
    vif<-function(org,solv,baseline){
      solv<-max(solv,na.rm = T)
      if(baseline>org) print("model worse than completely random?")
      return((org-solv))
    }
  }
  org <- lossf(Y,modeltp, X) 
  if((org<.05)){return(NULL)}
  try({ 
    #for each column
    #set it to each (mean , median , mode, 0, multiple permutations)
    #calculate rmse for them and compare via newrmse /oldrmse ?
    #minimal is the correct change because I am looking for a variable  
    #that is empty of info but disturbs the system least
    #a sensitive vriable given lots of extra noise will appear to be more important
    #
    #but then it turns out pearson practicaly does not change unless permutation
    #since relationship is linear no change between constants
    #if extremely non linear and only some numbers cause change then ignore
    #again no information is better than random causing errors
    #unfortunately could land on wrong or right number!
    #say pick the more correct number for this testing set bc more 
    #of this data actualy would have had 
    
    varImpMix <- as.character(round(proc.time()[3]-when[3]))
    i<-4
    #sumsolv<-0
    #tempDF<-data.frame()
    for(i in 1:dim(X)[2]){
      A <- X
      #smy<-summary(R[,i])
      A[,i] <- mean(R[,i],na.rm=T)#smy[3]
      nMnE <- lossf(Y,modeltp, A)
      if(F){
      A[,i] <- smy[4]
      nMdE <- lossf(Y,modeltp, A)
      comn  <- as.numeric(names(sort(table(R[,i]),decreasing=TRUE)[1:5]))
      comn[is.na(comn)]<-comn[2]
      A[,i] <- comn[1]
      nMoE <- lossf(Y,modeltp, A)
      A[,i] <- comn[3]
      nM3E <- lossf(Y,modeltp, A) 
      A[,i] <- comn[5]
      nM5E <- lossf(Y,modeltp, A)
      
      A[,i] <- 0
      nZeE <- lossf(Y,modeltp, A)
      
      
      A <- X
      nPerE<-vector()
      for(ittr in 1:n_sample){
        A[,i] <- A[sample.int(dim(A)[1]),i]
        (nPerE <- c(nPerE,lossf(Y,modeltp, A)))
      }
      nPerE<-mean(nPerE)
      
      tempDF <<- rbind(tempDF,data.frame(paste0(allmodel,datasource,names(A)[i]),org,nMnE,nMdE,nZeE,nMoE,nM3E,nM5E,nPerE))
      tomin <- c(nMnE,nMdE,nZeE,nMoE)
      }
      tomin <- nMnE
      
      #,nPerE
      #nPerE;nMnE;nMdE;nZeE;nMoE;nM2E;nM3E;
      #print(which.min(tomin))
      
      solv <- vif(org,tomin,baseline)
      #sumsolv <- sumsolv + solv
      varImpMix <- paste(varImpMix,names(X)[i],signif(solv,digits = 4), sep = ",")
    }
    fail.try.vif=F
    metpack <- paste(metpack,"JustMean",sep = "_")
  })
  
  Rseed <- .Random.seed[1]
  Cseed <- .Random.seed[2]
  
  write.table(paste(metpack,allmodel,date(),round(mean.improvement,digits=3),trans.y,
                    datasource,missingdata,withextra,norming,which.computer,task.subject,
                    FN,high.fold,Rseed,Cseed,seed.var,
                    varImpMix,  sep = ","),
              file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = F,
              col.names = F, qmethod = "double")
  
  
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


varimpermDEP<-function(custom_predict, modeltp, X, Y, n_sample = 5, metpack = "unk")
{
  
  fail.try.vif=T
  if((mean.improvement<.05) && (Rsqd<.05)){return(NULL)}
  
  ###VARIEBLE IMPORTANCE USING FRIEDMANS PERMUTE
  #n_sample : time to spend 
  #metpack is just an addition to name
  
  if(F){try({ 
    when<-proc.time()
    set.seed(seed=seed.var)
    expl_reg <- DALEX::explain(modeltp, data=X, y=Y,
                               predict_function = custom_predict)
    
    vi_reg <- variable_importance(expl_reg, loss_function = loss_root_mean_square,
                                  type = "ratio", n_sample = n_sample)
    
    #noVarImp.models=c("parRF")#var imp crashes with these models
    #if(allmodel %in% noVarImp.models){next()}#
    varImpMix <- as.character(round(proc.time()[3]-when[3]))#varImpMix<-vector(mode="character",length = length(colNms)*2)
    for(i in 2:(length(vi_reg[,2])-1)){
      #varImpMix[i*2]<-colNms[i] ; varImpMix[i*2+1]<-colImpor[i]
      varImpMix<-paste(varImpMix,vi_reg[i,1],signif(vi_reg[i,2],digits = 4), sep = ",")
    }
    fail.try.vif=F
    metpack <- paste(metpack,"permute",sep = "_")
  })}
  Rseed <- .Random.seed[1]
  Cseed <- .Random.seed[2]
  
  
  
  write.table(paste(metpack,allmodel,date(),round(mean.improvement,digits=3),trans.y,
                    datasource,missingdata,withextra,norming,which.computer,task.subject,
                    FN,high.fold,Rseed,Cseed,seed.var,
                    varImpMix,  sep = ","),
              file = paste(importance.file,".csv",sep=""), append =TRUE, quote = F, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = F,
              col.names = F, qmethod = "double")
  
  
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