#http://proceedings.mlr.press/v64/olson_tpot_2016.pdf

setwd(cpout.folder)
for(itr in c(4:30)){
  fail.try=T
  
  try({
when <- proc.time()
library(reticulate)

allmodel<-paste("TPOT",as.character(itr),sep = " ")
write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)
adaptControl$number<-itr
seed.var
if(T){
  retainpopulation = r_to_py(as.integer(50))
  offspring_size = r_to_py(as.integer(100))
  cv = r_to_py(as.integer(cv.iters))
  random_state = r_to_py(as.integer(seed.var))
  generationcount = r_to_py(as.integer(2))
  early_stop = r_to_py(as.integer(5))
  mins_onapipe = r_to_py(as.integer(40))
  checkpoint_folder = r_to_py("tpot")
  pipefile = r_to_py(paste("tpot","pipe",".py",sep = ""))
}


if(!CrashNRep(allmodel)) {

tpot <- import("tpot")
ztpot<-tpot$TPOTRegressor(generations=generationcount, population_size=retainpopulation,
                          offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                          periodic_checkpoint_folder=checkpoint_folder,verbosity=2)


X_train<-r_to_py(training[,-1])
Y_train<-r_to_py(training[,1])
ztpot$fit(X_train, Y_train)
prde<-ztpot$predict(X_train)
oveRMSE <- RMSE(prde,py_to_r(Y_train))
print(oveRMSE)

X_holdout<-r_to_py(testing[,-1])
Y_holdout<-r_to_py(testing[,1])
predictions<-ztpot$predict(X_holdout)
predicttt <- RMSE(predictions,py_to_r(Y_holdout))
print(predicttt)


ztpot$export(pipefile)

library(stringr)
gDat <- read.delim(file = py_to_r(pipefile),stringsAsFactors = F)
start<-grep("exported.*make_p", (gDat[,1]), value = F)
end<-grep("^)$", (gDat[,1]), value = F)

hyparams <- vector(mode = "character")
hyparams <- gDat[(start+1):(end-1),1]
hyparams <- gsub("StackingEstimator.estimator=", "Stacking", hyparams)
hyparams <- gsub("\\(.*$", "", hyparams)
hyparams <- gsub("[ ]+", "", hyparams)

printPredMets(predicted.outcomes=predictions,overRMSE=oveRMSE,hypercount="full",libpack="tpot")

    }
  })
  adaptControl$number<-cv.iters
}
 