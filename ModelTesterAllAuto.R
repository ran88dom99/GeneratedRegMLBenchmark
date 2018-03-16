
options(repos=structure(c(CRAN="https://rweb.crmda.ku.edu/cran/")))
## capture messages and errors to a file.https://rweb.crmda.ku.edu/cran/
#zz <- file("all.Rout", open="wt")https://cran.cnr.berkeley.edu
#sink(zz, type="message") edit for rebaseless
#chek for R package updates
#try(log("a")) ## test --no-edit
#devtools::install_github("berndbischl/ParamHelpers") # version >= 1.11 needed.
#devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)
memory.limit()
task.subject<-"14th20hp3cv"
pc.mlr<-c("ACE")#"ALTA","HOPPER"
which.computer<-Sys.info()[['nodename']]
out.file<-paste("out",task.subject,which.computer,.Platform$OS.type,.Platform$r_arch,".csv",sep="")
importance.file<-paste("importance",task.subject,which.computer,.Platform$OS.type,.Platform$r_arch,sep="")

#if(exists("base.folder")){setwd(base.folder)}
base.folder<-getwd()
cpout.folder<-paste(base.folder,"/",which.computer,sep = "")
setwd(cpout.folder)

if(length(which(list.files() == out.file))<1){
  write.table("pMAE,pRMSE,ocvRMSE,RMSEutrans,MAEutrans,date,algomodel,trgCol,transTarg,task,missing,append,transform,pc,expirament,RMSEofMean,hpGen,randomseed,time,validmethod,tuneLength,cvcount,ignrepeats,adaptivemin,bestTune,,,,,,,,,,,," ,file =,out.file,  quote = F, sep = ",", row.names = F,col.names = F)
  write.table("0.01,0.01,100,100,100,Wed Aug 02 16:37:25 2017,dummy,8,1,bac latent features,ignore,none,asis,1.127,random,333,53,adaptive_cv,16,5,2,2,19,0.0107744822639878,FALSE,,,,,,,,,," ,file =,out.file,append=T,quote = F, sep = ",", row.names = F,col.names = F)
  }
if(length(which(list.files() == paste(importance.file,".csv",sep="")))<1) write.table( ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,," ,file = paste(importance.file,".csv",sep=""),  quote = F, sep = ",", row.names = F,col.names = F)
if(length(which(list.files() == paste(importance.file,"mlr.csv",sep="")))<1) write.table( ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,," ,file = paste(importance.file,"mlr.csv",sep=""),  quote = F, sep = ",", row.names = F,col.names = F)

cv.iters=3
tuneLength=20
tuneLength2=8
normings=c("asis","centernscale","expoTrans","range01","quantile","YeoJohnson","all","PCA","ICA")#,"centernscale"

gensTTesto<-c(56,53,4,12,13,14,15,20,45,54,55, 44,3,1,52,57)#,  51,c(4)#c(1:40)#c(5,10,11,13,14,15,16,17,18,19,20,21,24,28,38,39,40)
gensTTest<-vector()
write.table( t(gensTTesto),file = "initial tasks to test.csv",  quote = F, sep = ",", row.names = F,col.names = F)
try({
  gensTTest<-t(read.csv("tasks to test.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
  gensTTest<-as.vector(gensTTest)
})
if(!exists("gensTTest")) gensTTest<-c(gensTTesto)#reversion[length(reversion):1]
gensTTesto<-c(gensTTesto[length(gensTTesto):1])
if(length(gensTTest)<1) gensTTest<-c(gensTTesto)#reversion[length(reversion):1]


########packages install check######

#list.of.packages <- c("caret","caretEnsemble","mlr","MLmetrics","tgp")
#list.of.packages <- c("gower","dimRed","DEoptimR","caretEnsemble","logicFS"," RWeka","ordinalNet","xgboost","mlr","caret","MLmetrics","bartMachine","spikeslab","party","rqPen","monomvn","foba","logicFS","rPython","qrnn","randomGLM","msaenet","Rborist","relaxo","ordinalNet","rrf","frbs","extraTrees","ipred","elasticnet","bst","brnn","Boruta","arm","elmNN","evtree","extraTrees","deepnet","kknn","KRLS","RSNNS","partDSA","plsRglm","quantregForest","ranger","inTrees")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages, dep = TRUE)


#install.packages("mlr", dependencies = c("Depends", "Suggests"))
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("caret",repos = "http://cran.r-project.org",dependencies = c("Depends", "Imports", "Suggests"))
#install.packages("SuperLearner", dependencies = c("Depends", "Suggests"))
#install.packages("rattle", dependencies = c("Depends", "Suggests"))

# Load libraries
#library(mlbench)

library(caret)
#library(caretEnsemble)
library(MLmetrics)

########error no repeat#########


try({
  before.last.alg<-as.matrix(read.csv("beforelast algorithm.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
  last.alg<-as.matrix(read.csv("last algorithm tried.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
  #write.table(paste(date(), last.alg,.Platform$OS.type,.Platform$r_arch,which.computer,sep=" "),file = "algos after which reset.csv",  quote = F, row.names = F,col.names = F,append = T)
  if(last.alg==before.last.alg){print("algorithm may be broken")}
  write.table(last.alg,file = "beforelast algorithm.csv",  quote = F, row.names = F,col.names = F)
})
try({
  before.last.tsk<-as.matrix(read.csv("beforelast task.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
  last.tsk<-as.matrix(read.csv("last task tried.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
  write.table(paste(date(),last.alg, last.tsk,cv.iters,tuneLength,.Platform$OS.type,.Platform$r_arch,which.computer,sep=","),file = "test after which reset.csv",  quote = F, row.names = F,col.names = F,append = T)
  if(last.tsk==before.last.tsk){print("task may be broken")}
  write.table(last.tsk,file = "beforelast task.csv",  quote = F, row.names = F,col.names = F)
})
bad.models=c("spaccceeee")
previous.fails<-(read.csv("test after which reset.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
previous.fails<-previous.fails[previous.fails[,8]==which.computer,]
lgf<-length(previous.fails[,2])
for(lt in 2:lgf)  {
  if(previous.fails[lt,2]==previous.fails[lt-1,2])  {
    bad.models=union(bad.models,c(paste(previous.fails[lt,2])))  }}

#source(functionsallautotest.R)
#######not to redo a test function in functions source#####
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
best.ranged <- c("avNNet", "nnet", "pcaNNet", "glm.nb")
best.asis <- c("svmLinear3", "relaxo", "superpc", "xgbTree")
best.cns <- c("gam", "bam", "svmLinear2", "msaenet", "BstLm", "gbm")

cv6hp5 <- c( "BstLm", "qrnn")#earth
cv3hp32 <- c("Rborist", "pcaNNet", "SBC")
cv7x5hp32 <- c("gbm", "krlsPoly", "kknn", "xgbLinear","RRF", "cubist", "rlm" )
cv6hp5.avoid <- c("pcaNNet")
cv3hp32.avoid <- c("glm.nb", "gamboost", "ctree2","glmboost", "leapSeq","ctree","svmLinear2")
cv7x5hp32.avoid <- c("SBC","bagearthgcv","gcvearth","lmStepAIC","glmStepAIC","bridge","lm","glm","bayesglm","blassoAveraged","treebag","rpart1SE")

#wow rfRules is really slow "rfRules","WM", takes 50min
# brak everythig "rbfDDA","ridge","rqnc",
# use "rf" to test all
library(caret)
allmodels <- unique(modelLookup()[modelLookup()$forReg,c(1)])
#allmodels <-c("avNNet", "nnet", "pcaNNet",  "glm.nb", "gam" ,
#              "bam","msaenet", "svmLinear2","svmLinear3",
#              "relaxo",  "superpc", "xgbTree", "BstLm")
#allmodels<- c("svmLinear","svmPoly","svmRadial")
#library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)
#allmodels<-c("bartMachine","extraTrees")#,"randomGLM"


adaptControl <- trainControl(method = "adaptive_cv",
                             number = 7, repeats = 5,
                             adaptive = list(min = 4, alpha = 0.05,
                                             method = "gls", complete = FALSE),
                             search = "random")
adaptControl <-trainControl(method = "cv", number = cv.iters,  search = "random")
simpleControl <- trainControl(method = "cv",
                              number = cv.iters,
                              search = "random")


#########MLR init######
#R.utils::gcDLLs()
#list.of.packages <- c("ParamHelpers","devtools","mlrMBO","RJSONIO","plot3D","plotly")
#install.packages("mlrMBO", dependencies = c("Depends", "Suggests"))
list.of.packages <- c("caretEnsemble","logicFS"," RWeka","ordinalNet","xgboost","mlr","caret","MLmetrics","bartMachine","spikeslab","party","rqPen","monomvn","foba","logicFS","rPython","qrnn","randomGLM","msaenet","Rborist","relaxo","ordinalNet","rrf","frbs","extraTrees","ipred","elasticnet","bst","brnn","Boruta","arm","elmNN","evtree","extraTrees","deepnet","kknn","KRLS","RSNNS","partDSA","plsRglm","quantregForest","ranger","inTrees")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dep = TRUE)

#devtools::install_github("berndbischl/ParamHelpers") # version >= 1.11 needed.
#devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)

tuneLengthMLR<-tuneLength
mlr.iters<-cv.iters
#######data read process start#####
seed.var =222+round(runif(1,min=0,max=100))
column.to.predict=1
print(date());

setwd(base.folder)
if(!exists("gen.count")){gen.count=56}
gens.names<-as.matrix(read.table("gens names.csv", sep = ",",header = FALSE,row.names=1,fill=TRUE, quote="",dec="."))
count.toy.data.passed<-1
for(gend.data in gensTTest){
  count.toy.data.passed<-count.toy.data.passed+1
  setwd(base.folder)
  data.source<-as.matrix(read.csv(paste("Generats/",gens.names[gend.data],".csv", sep = ""), sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
  datasource<-gens.names[gend.data,1]
  setwd(cpout.folder)
  missingdatas=c("ignore")
  for(missingdata in missingdatas){
    withextras=c("none")
    for(withextra in withextras){
      ################data wrestling###############

      dependant.selection=complete.cases(data.source[,column.to.predict])
      df.previous.calcs=as.data.frame(read.csv(file=out.file, header = TRUE, sep = ",", quote = "",
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

        for(norming in normings) {
        for(trans.y in 2) {#1:
          df.toprocess=data.source

          #df.toprocess = data.frame(df.toprocess,)
          nzv <- nearZeroVar(df.toprocess[,])#, saveMetrics= TRUE
          #nzv[nzv$nzv,][1:10,]
          if(length(nzv)>1){
            df.toprocess <- df.toprocess[, -nzv]
            }
          if((norming=="asis")&&(trans.y==1)){next}
          if((norming=="all")&&(trans.y==1)){next}
          if((norming=="ICA")&&(trans.y==1)){next}
          if((norming=="PCA")&&(trans.y==1)){next}
          y.untransformed<-df.toprocess[,1]
          l.df.tp<-length(df.toprocess[1,])

          if(norming=="centernscale"){
            preProcValues= preProcess(df.toprocess[,trans.y:l.df.tp],method = c("center", "scale"))
            df.toprocess[,trans.y:l.df.tp]<- predict(preProcValues, df.toprocess[,trans.y:l.df.tp])}
          if(norming=="range01"){
            preProcValues= preProcess(df.toprocess[,trans.y:l.df.tp],method = c("range"))
            df.toprocess[,trans.y:l.df.tp]<- predict(preProcValues, df.toprocess[,trans.y:l.df.tp])}
          if(norming=="expoTrans"){
            preProcValues= preProcess(df.toprocess[,trans.y:l.df.tp],method = c("expoTrans"))
            df.toprocess[,trans.y:l.df.tp]<- predict(preProcValues, df.toprocess[,trans.y:l.df.tp])}
          if(norming=="YeoJohnson"){
            preProcValues= preProcess(df.toprocess[,trans.y:l.df.tp],method = c("YeoJohnson"))#"center", "scale",
            df.toprocess[,trans.y:l.df.tp]<- predict(preProcValues, df.toprocess[,trans.y:l.df.tp])}
          if(norming=="all"){
            df.toprocessFA<-df.toprocess[,2:l.df.tp]
            preProcValueYJ= preProcess(df.toprocess[,2:l.df.tp],method = c("YeoJohnson"))
            preProcValueET= preProcess(df.toprocess[,2:l.df.tp],method = c("expoTrans"))
            preProcValue01= preProcess(df.toprocess[,2:l.df.tp],method = c("range"))
            preProcValuecns= preProcess(df.toprocess[,2:l.df.tp],method = c("center", "scale"))

            Q_Fail<-T #I expect ICA to crash horribly so testing for fail and NA
            try({
              preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("ica"),n.comp=3)#"center", "scale",
              df.ica<-predict(preProcValues, df.toprocess[,2:l.df.tp])
              Q_Fail<-F
            })
            if(Q_Fail || anyNA(df.ica)){
              print("icafail")}
            else{
              df.toprocess<-cbind(df.toprocess,df.ica)}
            Q_Fail<-T #I expect PCA to crash horribly so testing for fail and NA
            try({
              preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("pca"))#"center", "scale",
              df.pca<-predict(preProcValues, df.toprocess[,2:l.df.tp])
              Q_Fail<-F
            })
            if(Q_Fail || anyNA(df.pca)){
              print("pcafail")}
            else{
              df.toprocess<-cbind(df.toprocess,df.pca)}

            df.toprocess<-cbind( df.toprocess,
                                 predict(preProcValueYJ, df.toprocess[,2:l.df.tp]),
                                 predict(preProcValueET, df.toprocess[,2:l.df.tp]),
                                 predict(preProcValue01, df.toprocess[,2:l.df.tp]),
                                 predict(preProcValuecns, df.toprocess[,2:l.df.tp]),
                                 deparse.level = 1)
          }

          Q_Fail<-T #I expect ICA to crash horribly so testing for fail and NA
          try({
            if(norming=="ICA"){
              preProcValues= preProcess(df.toprocess[,trans.y:l.df.tp],method = c("ica"),n.comp=3)#"center", "scale",
              df.toprocess<-cbind(df.toprocess,predict(preProcValues, df.toprocess[,trans.y:l.df.tp]))}
            Q_Fail<-F
          })
          if(norming=="ICA"){
            if(Q_Fail || anyNA(df.toprocess)){next;print("icafail")}}

          Q_Fail<-T #I expect PCA to crash horribly so testing for fail and NA
          try({
            if(norming=="PCA"){
              preProcValues= preProcess(df.toprocess[,trans.y:l.df.tp],method = c("pca"))#"center", "scale",
              df.toprocess <-cbind(df.toprocess,predict(preProcValues, df.toprocess[,trans.y:l.df.tp]))
              }
            Q_Fail<-F
          })
          if(norming=="PCA"){
            if(Q_Fail || anyNA(df.toprocess)){next}}


          ################preprocess###########
          df.toprocess=data.frame(df.toprocess[dependant.selection,])
          y.untransformed=y.untransformed[dependant.selection]
          if(norming=="quantile"){
            for(Clol in trans.y:l.df.tp){
              df.toprocess[,Clol]<- (rank(df.toprocess[,Clol],na.last = "keep",ties.method = "average")-1)
              }
            preProcValues= preProcess(df.toprocess[,trans.y:l.df.tp],method = c("range"))
            df.toprocess[,trans.y:l.df.tp]<- predict(preProcValues, df.toprocess[,trans.y:l.df.tp])
            }

          if(norming=="all"){
            df.toprocessFA=data.frame(df.toprocessFA[dependant.selection,])
            l.df.tpfa<-length(df.toprocessFA[1,])
            for(Clol in 1:(l.df.tpfa)){
              df.toprocessFA[,Clol]<- (rank(df.toprocessFA[,Clol],na.last = "keep",ties.method = "average")-1)
              }
            preProcValues= preProcess(df.toprocessFA[,1:l.df.tpfa],method = c("range"))
            df.toprocessFA[,1:l.df.tpfa]<- predict(preProcValues, df.toprocessFA[,1:l.df.tpfa])
            df.toprocess<-cbind( df.toprocess,df.toprocessFA,deparse.level = 1)
          }

          df.toprocess = signif(df.toprocess,digits = 3)
          #df.toprocess = data.frame(df.toprocess,)
          nzv <- nearZeroVar(df.toprocess[,])#, saveMetrics= TRUE
          #nzv[nzv$nzv,][1:10,]
          if(length(nzv)>1){
            df.toprocess = (df.toprocess[, -nzv])}

          loess.model<-loess(y.untransformed~ df.toprocess[,1],span = 0.21, degree = 1)

          seed.var =222+round(runif(1,min=0,max=100))
          set.seed(seed.var)#spliting after transform is not ok?
          inTrain <- createDataPartition(y = df.toprocess[,1],
                                         p = .75,
                                         list = FALSE)
          training <- df.toprocess[ inTrain,]
          testing  <- df.toprocess[-inTrain,]
          write.table(df.toprocess,file = "sanity check 1.csv",  quote = F, row.names = F, col.names = T)

          ###########for all models#################
          gc()
          
          setwd(base.folder)
          if(max(which.computer==pc.mlr)>0)
            source("MLR part.R")
          else
            source("Caret part.R")

         setwd(cpout.folder)
          if(norming == normings[length(normings)]){
            if(count.toy.data.passed>length(gensTTest)){gensTTest<-c(gensTTesto)}
            write.table( t(gensTTest[count.toy.data.passed:length(gensTTest)]),file = "tasks to test.csv",  quote = F, sep = ",", row.names = F,col.names = F)

            }

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
