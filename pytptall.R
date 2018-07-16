library(reticulate)
tpot <- import("tpot")
#http://proceedings.mlr.press/v64/olson_tpot_2016.pdf
adContBack<-adaptControl
tuneLengthBack<-tuneLength
setwd(cpout.folder)
for(retpop in c(25,75)){
  for(offsprig in c(50,300)){
    for(itr in c(3,4,5,6,8,10,12,15,18,21,25,29,33,38)){#c(3,4,5,6,8,10,12,15,18,21,25,29,33,38),100,300)
  earlystop<-10
  #onepipmin<-40
  
  adaptControl$search <- "genetic"
  tuneLength <- retpop
  adaptControl$repeats <- offsprig
  adaptControl$adaptive$min <- earlystop
  adaptControl$number <- itr
  #<-onepipmin

  try({
    when <- proc.time()
    
    allmodel<-paste("TPOT",as.character(itr),sep = " ")
    write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
    write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)


    
    seed.var
    if(T){
      retainpopulation = r_to_py(as.integer(retpop))
      offspring_size = r_to_py(as.integer(offsprig))
      cv = r_to_py(as.integer(itr))
      random_state = r_to_py(as.integer(seed.var))
      generationcount = r_to_py(as.integer(300))
      early_stop = r_to_py(as.integer(earlystop))
      mins_onapipe = r_to_py(as.integer(40))
      checkpoint_folder = r_to_py("tpot")
      pipefile = r_to_py(paste("tpot","pipe",".py",sep = ""))
    }
    
    if(!CrashNRep(allmodel)) {
      print(date())
      fail.try=T

      
      ztpot <- tpot$TPOTRegressor(generations=generationcount, population_size=retainpopulation,cv=cv,
                                offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                                periodic_checkpoint_folder=checkpoint_folder,random_state =random_state,verbosity=2)
      
      
      X_train <- r_to_py(training[,-1])
      Y_train <- r_to_py(training[,1])
      ztpot$fit(X_train, Y_train)
      prde <- ztpot$predict(X_train)
      oveRMSE <- RMSE(prde,py_to_r(Y_train)) 
      print(oveRMSE)
      
      X_holdout <- r_to_py(testing[,-1])
      Y_holdout <- r_to_py(testing[,1])
      predictions <- ztpot$predict(X_holdout)
      predicttt <- RMSE(predictions,py_to_r(Y_holdout))
      print(predicttt)
      
      fail.grep<-T
      try({
      ztpot$export(pipefile)
      movethepot <- paste0("tpot/",round((predicttt)*100),datasource,".py")
      file.copy(as.character(pipefile),as.character(movethepot[1]))
      

      library(stringr)
      gDat <- read.delim(file = py_to_r(pipefile),stringsAsFactors = F)
      start<-grep("exported.*make_p", (gDat[,1]), value = F)
      end<-grep("^)$", (gDat[,1]), value = F)
      
      hyparams <- vector(mode = "character")
      hyparams <- gDat[(start+1):(end),1]
      hyparams <- gsub("StackingEstimator.estimator=", "Stacking", hyparams)
      hyparams <- gsub("\\(.*$", "", hyparams)
      hyparams <- gsub("\\).*$", "", hyparams)
      hyparams <- gsub("[ ]+", "", hyparams)
      #hyparams <- gsub("make_pipeline", "", hyparams);hyparams <- gsub("make_union", "", hyparams)
      for(itt in length(hyparams)) {
        if(hyparams[itt]=="") { hyparams<-hyparams[-itt] }
      }
      fail.grep<-F
      })
      
      if(fail.grep==F){
        printPredMets(predicted.outcomes=predictions,overRMSE=oveRMSE,hypercount="full",libpack="tpot")
      } else {
        printPredMets(predicted.outcomes=predictions,overRMSE=oveRMSE,hypercount="full")
      }
      
      fail.try<-F
    }
  })
  
  if(fail.try==T) {
    print(c("failed","failed",date(),datasource,missingdata,withextra,norming,which.computer,task.subject,allmodel))
    write.table(paste("Fail","Fail","Fail","Fail","Fail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1],.Random.seed[2],seed.var,round(proc.time()[3]-when[3]),  sep = ","),
                file = out.file, append =TRUE, quote = F, sep = ",",
                eol = "\n", na = "NA", dec = ".", row.names = F,
                col.names = F, qmethod = "double")    
  }

  }
 }
}
adaptControl <- adContBack
tuneLengthBack -> tuneLength