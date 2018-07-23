library(reticulate)
tpot <- import("tpot")
#http://proceedings.mlr.press/v64/olson_tpot_2016.pdf
adContBack<-adaptControl
tuneLengthBack<-tuneLength
setwd(cpout.folder)
oveRMSE<-100
oveRMSE.cou<-0
predicttt<-100
predicttt.cou<- 0

earlystop<-7
onepipmin<-40
max.generations<-300
varimp.every<-3
gensperitr<-2
#not set up yet, waiting for black opt
Xover.rt<-.1
mutation.rt<-.9

for(retpop in c(25,1000)){
  for(offsprig in c(50,300)){
    for(itr in c(5,11,21,41)){#5,8,12,c(3,4,5,6,8,10,12,15,18,21,25,29,33,38),100,300)

  
  adaptControl$search <- "genetic"
  tuneLength <- retpop
  adaptControl$repeats <- offsprig
  adaptControl$adaptive$min <- earlystop
  adaptControl$number <- itr
  adaptControl$method <- onepipmin
  fail.try=T
  
  try({
    when <- proc.time()
    
    timechecksum<-((retpop+offsprig*earlystop)*itr*onepipmin)/((25+25*2)*3*5)
    timechecksum<-round(timechecksum)
    allmodel<-paste(as.character(timechecksum),"TPOT",sep = "_")
    write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
    write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)
    if(CrashNRep(allmodel)) {next()}
    
    if(T){
      retainpopulation = r_to_py(as.integer(retpop))
      offspring_size = r_to_py(as.integer(offsprig))
      cv = r_to_py(as.integer(itr))
      random_state = r_to_py(as.integer(seed.var))
      generationcount = r_to_py(as.integer(gensperitr))#)early_stop=early_stop,
      early_stop = r_to_py(as.integer(earlystop))
      mins_onapipe = r_to_py(as.integer(onepipmin))
      checkpoint_folder = r_to_py("tpot")
      pipefile = r_to_py(paste("tpot","pipe",".py",sep = ""))
      memfile =  r_to_py(paste("/tpot","memcache/",sep = ""))
      memfile = r_to_py("auto")
      warm = r_to_py("True")
    }

      print(date())
      
      for(itr.genr in 1:max.generations){
        try({
        #I want RMSE of every generation for reaserch
      #set tpot to use previous calcs to print each generation
      if(itr.genr==2){
      if(which.computer!="ALTA"){ 
        ztpot <- tpot$TPOTRegressor(generations=generationcount, population_size=retainpopulation,cv=cv,
                                offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                                periodic_checkpoint_folder=checkpoint_folder,random_state =random_state,verbosity=2,memory=memfile, warm_start=warm)
      } else {
        ztpot <- tpot$TPOTRegressor(generations=generationcount, population_size=retainpopulation,cv=cv,
                                    offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                                    periodic_checkpoint_folder=checkpoint_folder,random_state =random_state,verbosity=2, warm_start=warm)
      }
      }
      #cold start
      if(itr.genr==1){
        if(which.computer!="ALTA"){ 
          ztpot <- tpot$TPOTRegressor(generations=generationcount, population_size=retainpopulation,cv=cv,
                                                                offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                                                                periodic_checkpoint_folder=checkpoint_folder,random_state =random_state,verbosity=2,memory=memfile)
        } else {
          ztpot <- tpot$TPOTRegressor(generations=generationcount, population_size=retainpopulation,cv=cv,
                                      offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                                      periodic_checkpoint_folder=checkpoint_folder,random_state =random_state,verbosity=2)
        }
      }
      
      print(ztpot)
      
      X_train <- r_to_py(training[,-1])
      Y_train <- r_to_py(training[,1])
      ztpot$fit(X_train, Y_train)
      prde <- ztpot$predict(X_train)
      old.oveRMSE<-oveRMSE
      oveRMSE <- RMSE(prde,py_to_r(Y_train)) 
      print(oveRMSE)
      if(old.oveRMSE==oveRMSE) {
        oveRMSE.cou=oveRMSE.cou+1
      } else { oveRMSE.cou=0}
      

      X_holdout <- r_to_py(testing[,-1])
      Y_holdout <- r_to_py(testing[,1])
      predictions <- ztpot$predict(X_holdout)
      old.predicttt<-predicttt
      predicttt <- RMSE(predictions,py_to_r(Y_holdout))
      if(old.predicttt==predicttt) {
        predicttt.cou=predicttt.cou+1
      } else { predicttt.cou=0}
      
      print(predicttt)
      print(prde)
      print(predictions)
      
      fail.grep<-T
      try({
      ztpot$export(pipefile)
      movethepot <- paste("tpot/",datasource,round((predicttt)*100),timechecksum,".py",sep = "_")
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
        printPredMets(predicted.outcomes=predictions,overRMSE=oveRMSE,hypercount="full",libpack="emptpot")
      }
      
      if( ((itr.genr-1) %% varimp.every)==0  ) {
        try({ 
          custom_predict <- function(object, newdata) {
            newdata2 <- r_to_py(newdata)
            pred <- object$predict(newdata2) 
            return(pred)
          }
          varimperm(custom_predict=custom_predict, modeltp=ztpot,
                    X=testing[,-1], Y=testing[,1], metpack = paste0("TPOT_hold_",itr.genr),
                    n_sample = 10000)
          varimperm(custom_predict=custom_predict, modeltp=ztpot,
                    X=training[,-1], Y=training[,1], metpack = paste0("TPOT_train_",itr.genr),
                    n_sample = 1000)
        })
      }
        })
      if(predicttt.cou >= earlystop && oveRMSE.cou >= earlystop) {break()}
      
      }
      fail.try<-F
  })
  try({ 
    custom_predict <- function(object, newdata) {
      newdata2 <- r_to_py(newdata)
      pred <- object$predict(newdata2) 
      return(pred)
    }
    varimperm(custom_predict=custom_predict, modeltp=ztpot,
              X=testing[,-1], Y=testing[,1], metpack = ("TPOT_hold_fin"),
              n_sample = 10000)
    varimperm(custom_predict=custom_predict, modeltp=ztpot,
              X=training[,-1], Y=training[,1], metpack = "TPOT_train_fin",
              n_sample = 1000)
  })
  if(fail.try==T) {
    failfail() 
  }

  }
 }
}
adaptControl <- adContBack
tuneLength <- tuneLengthBack
