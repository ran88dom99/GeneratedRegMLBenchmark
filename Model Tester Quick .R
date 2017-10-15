## capture messages and errors to a file.
#zz <- file("all.Rout", open="wt")
#sink(zz, type="message")

#try(log("a"))


########packages install check######
library(caret)
list.of.packages <- c("caret","caretEnsemble","mlr","MLmetrics","tgp")
#list.of.packages <- c("caretEnsemble","logicFS"," RWeka","ordinalNet","xgboost","mlr","caret","MLmetrics","bartMachine","spikeslab","party","rqPen","monomvn","foba","logicFS","rPython","qrnn","randomGLM","msaenet","Rborist","relaxo","ordinalNet","rrf","frbs","extraTrees","ipred","elasticnet","bst","brnn","Boruta","arm","elmNN","evtree","extraTrees","deepnet","kknn","KRLS","RSNNS","partDSA","plsRglm","quantregForest","ranger","inTrees")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dep = TRUE)


#install.packages("mlr", dependencies = c("Depends", "Suggests"))
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("SuperLearner", dependencies = c("Depends", "Suggests"))
#install.packages("rattle", dependencies = c("Depends", "Suggests"))

# Load libraries
#library(mlbench)

library(caret)
#library(caretEnsemble)
library(MLmetrics)

try({
before.last.alg<-as.matrix(read.csv("beforelast algorithm.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
last.alg<-as.matrix(read.csv("last algorithm tried.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
if(last.alg==before.last.alg){print("algorithm may be broken")}
write.table(last.alg,file = "beforelast algorithm.csv",  quote = F, row.names = F,col.names = F)
})

#######not to redo a test function#####
check.redundant<-function(df=df.previous.calcs,norming="asis",trans.y=1,withextra="missing",missingdata="leaveempty",datasource="mean" ,column.to.predict=200,allmodel="ctree")
{
  for(intern in 1:length(df[,1])){
    if((any(df[intern,] == norming, na.rm=T))&&
       (any(df[intern,] == withextra, na.rm=T))&&
       (any(df[intern,] == missingdata, na.rm=T))&&
       (any(df[intern,] == datasource, na.rm=T))&&
       (any(df[intern,] == column.to.predict, na.rm=T))&&
       (any(df[intern,] == allmodel, na.rm=T))&&
       (  (df[intern,9] == trans.y)))
    {return(TRUE)}
  }
  return(FALSE)
}
#####caret init#####
allmodels <- c("avNNet", "bagEarth", "bagEarthGCV",
               "bayesglm", "bdk", "blackboost", "Boruta", "brnn", "BstLm" ,
               "bstTree", "cforest", "ctree", "ctree2", "cubist", "DENFIS",
               "dnn", "earth", "elm", "enet",   "evtree",
               "extraTrees",  "gamLoess",  "gaussprLinear", "gaussprPoly", "gaussprRadial",
               "gcvEarth","glm", "glmboost",  "icr", "kernelpls",
               "kknn", "knn",  "krlsRadial", "lars" , "lasso",
               "leapBackward", "leapForward", "leapSeq", "lm", "M5", "M5Rules",
               "mlpWeightDecay", "neuralnet" , "partDSA",
               "pcaNNet", "pcr", "penalized", "pls", "plsRglm", "ppr",
               "qrf" , "ranger",  "rf")
allmodels <- c("rlm", "rpart", "rpart2",
               "RRF", "RRFglobal",  "simpls",
               "svmLinear", "svmPoly", "svmRadial", "svmRadialCost",
               "widekernelpls",  "xgbLinear",
               "xgbTree")
allmodels <- c("avNNet","BstLm","bstTree","cforest","ctree","ctree2",
               "cubist","earth","enet","evtree","glmboost",
               "icr","kernelpls","kknn","lasso","pcaNNet",
               "pcr","pls","qrf","ranger","rf")

allmodels <- c("BstLm")#"","enet","lasso",
#allmodels <- c("rf")"rqlasso",, "xyf" "rvmPoly", "rvmRadial",    "spls", "superpc" ,   "treebag",  "svmLinear2",  "SBC",
#allmodels <- c("bartMachine", "xgbLinear", "pcaNNet","svmLinear","glmnet","cforest","cubist","rf","ranger")"glmnet",
#wow rfRules is really slow "rfRules","WM", takes 50min
# brak everythig "rbfDDA","ridge","rqnc",
# use "rf" to test all
allmodels <- unique(modelLookup()[modelLookup()$forReg,c(1)])
#allmodels<- c("svmLinear","svmPoly","svmRadial")
#library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
allmodels<-c("bartMachine","extraTrees")#,"randomGLM"


adaptControl <- trainControl(method = "adaptive_cv",
                             number = 7, repeats = 5,
                             adaptive = list(min = 4, alpha = 0.05,
                                             method = "gls", complete = FALSE),
                             search = "random")
adaptControl <-trainControl(method = "cv", number = 3,
                            search = "random")
simpleControl <- trainControl(method = "cv",
                              number = 3,
                              search = "random")
tuneLength=32
tuneLength2=32
#########MLR init######
#R.utils::gcDLLs()
#list.of.packages <- c("ParamHelpers","devtools","mlrMBO","RJSONIO","plot3D","plotly")
#install.packages("mlrMBO", dependencies = c("Depends", "Suggests"))
list.of.packages <- c("caretEnsemble","logicFS"," RWeka","ordinalNet","xgboost","mlr","caret","MLmetrics","bartMachine","spikeslab","party","rqPen","monomvn","foba","logicFS","rPython","qrnn","randomGLM","msaenet","Rborist","relaxo","ordinalNet","rrf","frbs","extraTrees","ipred","elasticnet","bst","brnn","Boruta","arm","elmNN","evtree","extraTrees","deepnet","kknn","KRLS","RSNNS","partDSA","plsRglm","quantregForest","ranger","inTrees")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dep = TRUE)

#devtools::install_github("berndbischl/ParamHelpers") # version >= 1.11 needed.
#devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)

tuneLengthMLR=32
mlr.iters<-3
#######data read process start#####
seed.const=222+round(runif(1,min=0,max=100))
seed.var=seed.const
column.to.predict=1
print(date());

if(!exists("gen.count")){gen.count=40}
gens.names<-as.matrix(read.table("gens names.csv", sep = ",",header = FALSE,row.names=1,fill=TRUE, quote="",dec="."))
for(gend.data in 12:40){
  data.source<-as.matrix(read.csv(paste(gens.names[gend.data],".csv", sep = ""), sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
  datasource<-gens.names[gend.data]
  missingdatas=c("ignore")
  for(missingdata in missingdatas){
    withextras=c("none")
    for(withextra in withextras){
      ################data wrestling###############
      
      dependant.selection=complete.cases(data.source[,column.to.predict])
      df.previous.calcs=as.data.frame(read.csv(file="gen test out.csv", header = FALSE, sep = ",", quote = "",
                                               dec = ".", fill = TRUE, comment.char = ""))
      unimportant.computations<-vector(mode = "logical",length=length(df.previous.calcs[,1])  )
      for(intern in 1:length(df.previous.calcs[,1])){
        if((any(df.previous.calcs[intern,] == withextra, na.rm=T))&&
           (any(df.previous.calcs[intern,] == missingdata, na.rm=T))&&
           (any(df.previous.calcs[intern,] == datasource, na.rm=T))&&
           (any(df.previous.calcs[intern,] == column.to.predict, na.rm=T)))
        {unimportant.computations[intern]<-T}}
      
      df.previous.calcs<-df.previous.calcs[unimportant.computations,]
      
      
      
      #data.source=data.frame( data.source[,column.to.predict],data.source[,1:2], data.source[,4:(column.to.predict-1)], data.source[,(column.to.predict+1):length( data.source[1,])])
      
      
      normings=c("range01","asis")#,"expoTrans","quantile","centernscale","YeoJohnson"
      for(norming in normings) {
        for(trans.y in 1:1) {
          df.toprocess=data.source
          y.untransformed<-df.toprocess[,1]
          
          if(norming=="centernscale"){
            preProcValues= preProcess(df.toprocess[,trans.y:length(df.toprocess[1,])],method = c("center", "scale"))
            df.toprocess[,trans.y:length(df.toprocess[1,])]<- predict(preProcValues, df.toprocess[,trans.y:length(df.toprocess[1,])])}
          if(norming=="range01"){
            preProcValues= preProcess(df.toprocess[,trans.y:length(df.toprocess[1,])],method = c("range"))
            df.toprocess[,trans.y:length(df.toprocess[1,])]<- predict(preProcValues, df.toprocess[,trans.y:length(df.toprocess[1,])])}
          if(norming=="expoTrans"){
            preProcValues= preProcess(df.toprocess[,trans.y:length(df.toprocess[1,])],method = c("expoTrans"))
            df.toprocess[,trans.y:length(df.toprocess[1,])]<- predict(preProcValues, df.toprocess[,trans.y:length(df.toprocess[1,])])}
          if(norming=="YeoJohnson"){
            preProcValues= preProcess(df.toprocess[,trans.y:length(df.toprocess[1,])],method = c("YeoJohnson"))#"center", "scale",
            df.toprocess[,trans.y:length(df.toprocess[1,])]<- predict(preProcValues, df.toprocess[,trans.y:length(df.toprocess[1,])])}
          
          if((norming=="asis")&&(trans.y==2)){next}
          
          
          ################preprocess###########
          df.toprocess=data.frame(df.toprocess[dependant.selection,])
          y.untransformed=y.untransformed[dependant.selection]
          if(norming=="quantile"){
            for(Clol in trans.y:length(data.source[1,])){
              df.toprocess[,Clol]<- (rank(df.toprocess[,Clol],na.last = "keep",ties.method = "average")-1) }
            preProcValues= preProcess(df.toprocess[,trans.y:length(df.toprocess[1,])],method = c("range"))
            df.toprocess[,trans.y:length(df.toprocess[1,])]<- predict(preProcValues, df.toprocess[,trans.y:length(df.toprocess[1,])])}
          
          
          loess.model<-loess(y.untransformed~ df.toprocess[,1],span = 0.21, degree = 1)
          
          
          #df.toprocess = data.frame(df.toprocess,)
          nzv <- nearZeroVar(df.toprocess[,])#, saveMetrics= TRUE
          #nzv[nzv$nzv,][1:10,]
          if(length(nzv)>1){
          df.toprocess = (df.toprocess[, -nzv])}
          
          set.seed(seed.var)
          inTrain <- createDataPartition(y = df.toprocess[,1],
                                         p = .75,
                                         list = FALSE)
          training <- df.toprocess[ inTrain,]
          testing  <- df.toprocess[-inTrain,]
          write.table(df.toprocess,file = "sanity check 1.csv",  quote = F, row.names = F,col.names = F)
          

          
###########for all models#################
          
          source("Caret part.R")
          #source("MLR part.R")
        }
        }
      }
    }
  
}
##########end#########
#stopCluster(cl)
## reset message sink and close the file connection
sink(type="message")
close(zz)

### Display the log file
readLines("all.Rout")