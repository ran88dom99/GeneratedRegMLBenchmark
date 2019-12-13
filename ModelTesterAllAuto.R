#try({library(lintr)})
options(repos=structure(c(CRAN="https://rweb.crmda.ku.edu/cran/")))
## capture messages and errors to a file.https://rweb.crmda.ku.edu/cran/
#zz <- file("all.Rout", open="wt")https://cran.cnr.berkeley.edu
#sink(zz, type="message") edit for rebaseless
#chek for R package updates
#try(log("a")) ## test --no-edit
#WHEN INSTALLING RTOOLS MAKE SURE TO "Select Additional Tasks" dialog box I checked "Edit the system PATH. ...".
#install.packages("devtools")+++++
##devtools::install_github("r-lib/devtools")
#
#devtools::install_github("berndbischl/ParamHelpers") # version >= 1.11 needed.
#devtools::install_github("jakob-r/mlrHyperopt", dependencies = TRUE)
memory.limit()

########packages install check######

#list.of.packages <- c("caret","caretEnsemble","mlr","MLmetrics","tgp")
list.of.packages <- c("DALEX","ddalpha","dplyr","gtools","reticulate","AlgDesign","LearnBayes","httpuv","gower","dimRed","DEoptimR","caretEnsemble","logicFS",
                      " RWeka","ordinalNet","xgboost","mlr","caret","MLmetrics","bartMachine","spikeslab","party","rqPen","monomvn",
                      "foba","logicFS","rPython","qrnn","randomGLM","msaenet","Rborist","relaxo","ordinalNet","rrf","frbs","extraTrees","ipred",
                      "elasticnet","bst","brnn","Boruta","arm","elmNN","evtree","extraTrees","deepnet","kknn","KRLS","RSNNS","partDSA","plsRglm",
                      "quantregForest","ranger","inTrees","fda.usc","FDboost","LiblineaR","questionr","import")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dep = TRUE)


#install.packages("mlr", dependencies = c("Depends", "Suggests"))
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("caret",repos = "http://cran.r-project.org",dependencies = c("Depends", "Imports", "Suggests"))
#install.packages("SuperLearner", dependencies = c("Depends", "Suggests"))
#install.packages("rattle", dependencies = c("Depends", "Suggests"))

fail.try<-T
try({
which.computer<-names(read.csv("thispc.txt"))
fail.try<-F
})
 if(fail.try==T){
which.computer<-Sys.info()[['nodename']]
write(which.computer,"thispc.txt")
}

task.subject<-"QSlink3"#"carEnstest3"#
# regeneration including same 100, reselection to testrun  
pc.tpot=F
pc.caret=T
pc.mlr<-c("ACEREBOUTt","HOPPER","ALTA")#T,"HOPPER"
pc.smallR<-c("HOPPER","ALTAt","ACEREBOUT")
if(which.computer=="ALTA") #.libPaths("D:/R library/3.4");
  {pc.tpot=F;pc.caret=F}#;task.subject<-"carEnstest4"
if(which.computer=="ACEREBOUT") 
  {pc.tpot=F;pc.caret=F; }#task.subject<-"hffoldreccTPOT";
if(which.computer=="HOPPER"){pc.tpot=F;pc.caret=F}
if(which.computer=="LAPTOPBBQ"){pc.tpot=F;pc.caret=F}

  
out.file<-paste("out",task.subject,which.computer,.Platform$OS.type,.Platform$r_arch,".csv",sep="")
importance.file<-paste("importance",task.subject,which.computer,.Platform$OS.type,.Platform$r_arch,sep="")

source("predEncapFu.R")
source("VarImpFunc.R")
#if(exists("base.folder")){setwd(base.folder)}
base.folder<-getwd()
cpout.folder<-paste(base.folder,"/",which.computer,sep = "")
setwd(cpout.folder)

if(length(which(list.files() == out.file))<1){
  write.table("pq9TargRank,gainin30,spearman2,pearson2,pMAE,pRMSE,ocvRMSE,RMSEutrans,MAEutrans,date,algomodel,trgCol,transTarg,task,missing,append,transform,pc,expirament,fold,maxfold,seed,seedit,foldseed,RMSEmean,RMSEmeantrain,hpGen,time,validmethod,tuneLength,cvcount,ignrepeats,adaptivemin,bestTuneparams,btp1,btp2,btp3,btp4,btp5,btp6,btp7,btp8,btp9,btp10,btp11,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,",file =,out.file,  quote = F, sep = ",", row.names = F,col.names = F)
  write.table("0,NA,0,0,0.01,0.01,100,100,100,Wed Aug 02 16:37:25 2017,dummy,8,1,bac latent features,ignore,none,asis,CLOUD,10hp10cv,1,5,403,624,222,0.9,0.9284,random,434,cv,10,10,NA,5,15,1.3312352844514,0.968602964049205,1.30087468028069,1.36411243351176,,," ,file =,out.file,append=T,quote = F, sep = ",", row.names = F,col.names = F)
  }
if(length(which(list.files() == paste(importance.file,".csv",sep="")))<1) write.table( ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,," ,file = paste(importance.file,".csv",sep=""),  quote = F, sep = ",", row.names = F,col.names = F)
#if(length(which(list.files() == paste(importance.file,"mlr.csv",sep="")))<1) write.table( ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,," ,file = paste(importance.file,"mlr.csv",sep=""),  quote = F, sep = ",", row.names = F,col.names = F

high.fold=5
min.high.fold=2
cv.iters=5
tuneLength=5
tuneLength2=3
normings=c("asis","range01","centernscale","all","YeoJohnson","quantile","PCA","ICA","expoTrans")#,"centernscale"
predictNDCG<-T
#if(which.computer=="ACEREBOUT") {cv.iters<-20;min.high.fold=20;high.fold=20}
#if(which.computer=="HOPPER"){high.fold=5}

pram.cycle<-T
gensTTesto<-c(79:84)#,  51,c(4)#c(1:40)#c(5,10,11,13,14,15,16,17,18,19,20,21,24,28,38,39,40)
gensTTest<-vector()
#write.table( t(gensTTesto),file = "initial tasks to test.csv",  quote = F, sep = ",", row.names = F,col.names = F)
try({
  gensTTest<-t(read.csv("tasks to test.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
  gensTTest<-as.vector(gensTTest)
})
if(!exists("gensTTest")) gensTTest<-c(gensTTesto)#reversion[length(reversion):1]
gensTTesto<-c(gensTTesto[length(gensTTesto):1])
if(length(gensTTest)<1) gensTTest<-c(gensTTesto)#reversion[length(reversion):1]

#if item not in last.parameters then next()
#every full for loop write down last.parameters
#if last.parameters does not exist, write first parameters
#first.pram<-c("56","ignore","none","asis","1","1")#preve.pram<-first.pram
try({
  preve.pram<-t(read.csv("previous parameters.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
  preve.pram<-as.vector(preve.pram)
})
if(!exists("preve.pram")) pram.cycle<-F


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
seed.var =222 #folds must be kept the same so no +round(runif(1,min=0,max=100)) for main
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
  #for(i in 1:dim(data.source)[2]){
  #  data.source[,i] <- as.numeric(as.character(data.source[,i]))
  #}
  #data.source <- as.matrix(data.source)
  print(head(data.source))
  datasource<-gens.names[gend.data,1]
  setwd(cpout.folder)
  missingdatas=c("ignore")
  for(missingdata in missingdatas){
    withextras=c("none")
    for(withextra in withextras){
      ################data wrestling###############

      dependant.selection=complete.cases(data.source[,column.to.predict])
      
      
      df.previous.calcs=as.data.frame(read.csv(file=out.file, header = T, sep = ",", quote = "",   dec = ".", fill = TRUE, comment.char = ""))
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
        for(trans.y in 1:2) {#1:
          
          if(pram.cycle){
            ppsum<-0
            ppsum<-ppsum+(trans.y %in% preve.pram)
            ppsum<-ppsum+(norming %in% preve.pram)
            ppsum<-ppsum+(withextra %in% preve.pram)
            ppsum<-ppsum+(missingdata %in% preve.pram)
          if(ppsum<4) next()
            }
          
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
          #the three below are all TODOs
          if((norming=="quantile")&&(trans.y==1)){next}
          if((norming=="YeoJohnson")&&(trans.y==1)){next}
          if((norming=="expoTrans")&&(trans.y==1)){next}
          y.untransformed<-df.toprocess[dependant.selection,1]
          l.df.tp<-length(df.toprocess[1,])

          if(norming=="centernscale"){
            preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("center", "scale"))
            df.toprocess[,2:l.df.tp]<- predict(preProcValues, df.toprocess[,2:l.df.tp])}
          if(norming=="range01"){
            preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("range"))
            df.toprocess[,2:l.df.tp]<- predict(preProcValues, df.toprocess[,2:l.df.tp])}
          if(norming=="expoTrans"){
            preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("expoTrans"))
            df.toprocess[,2:l.df.tp]<- predict(preProcValues, df.toprocess[,2:l.df.tp])}
          if(norming=="YeoJohnson"){
            preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("YeoJohnson"))#"center", "scale",
            df.toprocess[,2:l.df.tp]<- predict(preProcValues, df.toprocess[,2:l.df.tp])}
          Q_Fail<-T #I expect ICA to crash horribly so testing for fail and NA
          try({
            if(norming=="ICA"){
              preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("ica"),n.comp=8)#"center", "scale",
              df.toprocess<-cbind(df.toprocess,predict(preProcValues, df.toprocess[,2:l.df.tp]))}
            Q_Fail<-F
          })
          if(norming=="ICA"){
            if(Q_Fail || anyNA(df.toprocess)){next;print("icafail")}}
          
          Q_Fail<-T #I expect PCA to crash horribly so testing for fail and NA
          try({
            if(norming=="PCA"){
              preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("pca"))#"center", "scale",
              df.toprocess <-cbind(df.toprocess,predict(preProcValues, df.toprocess[,2:l.df.tp]))
            }
            Q_Fail<-F
          })
          if(norming=="PCA"){
            if(Q_Fail || anyNA(df.toprocess)){next}}
          
          ###### ALL
          if(norming=="all"){
            df.toprocessFA<-df.toprocess[,2:l.df.tp]
            preProcValueYJ= preProcess(df.toprocess[,2:l.df.tp],method = c("YeoJohnson"))
            preProcValueET= preProcess(df.toprocess[,2:l.df.tp],method = c("expoTrans"))
            preProcValue01= preProcess(df.toprocess[,2:l.df.tp],method = c("range"))
            preProcValuecns= preProcess(df.toprocess[,2:l.df.tp],method = c("center", "scale"))
            Q_Fail<-T #I expect ICA to crash horribly so testing for fail and NA
            try({
              preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("ica"),n.comp=10)#"center", "scale",
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

          if(norming=="quantile"){
            for(Clol in 2:l.df.tp){
              df.toprocess[,Clol]<- (rank(df.toprocess[,Clol],na.last = "keep",ties.method = "average")-1)
            }
            preProcValues= preProcess(df.toprocess[,2:l.df.tp],method = c("range"))
            df.toprocess[,2:l.df.tp]<- predict(preProcValues, df.toprocess[,2:l.df.tp])
          } 

          ################preprocess###########
          df.forNDCG=data.frame(df.toprocess[-dependant.selection,])
          df.toprocess=data.frame(df.toprocess[dependant.selection,])
          if(!identical(df.forNDCG,df.toprocess)){warning("some na in rows? because difference betwix complete.cases() df[-dependant.selection,] and df[dependant.selection,]")}
          #quantile takes a while so it runs after dataset is reduced to usable y values
          # no. quantile is too error prone so its not used in "all" or as y transformer. 
          #But is used regularily and with ALL data on explanatories like the others. 
          #There is just not enough time to do this properly
          if(norming=="all" && F){
            df.toprocessFA=data.frame(df.toprocessFA[dependant.selection,])
            l.df.tpfa<-length(df.toprocessFA[1,])
            for(Clol in 1:(l.df.tpfa)){
              df.toprocessFA[,Clol]<- (rank(df.toprocessFA[,Clol],na.last = "keep",ties.method = "average")-1)
            }
            preProcValues= preProcess(df.toprocessFA[,1:l.df.tpfa],method = c("range"))
            df.toprocessFA[,1:l.df.tpfa]<- predict(preProcValues, df.toprocessFA[,1:l.df.tpfa])
            df.toprocess<-cbind( df.toprocess,df.toprocessFA,deparse.level = 1)
          }

      
          

          #####second folding
          set.seed(seed.var)#spliting after transform is not ok?
          foldTrain<-createFolds(y = df.toprocess[,1], k = high.fold, list = TRUE, returnTrain = FALSE)
          #inTrain <- createDataPartition(y = df.toprocess[,1],p = .75,list = FALSE)
          for(FN in 1:high.fold){
            if(min.high.fold<FN){next()}
            training <- df.toprocess[-foldTrain[[FN]],]
            
            if(pram.cycle){
              ppsum<-0
              ppsum<-ppsum+(trans.y %in% preve.pram)
              ppsum<-ppsum+(norming %in% preve.pram)
              ppsum<-ppsum+(withextra %in% preve.pram)
              ppsum<-ppsum+(missingdata %in% preve.pram)
              ppsum<-ppsum+(FN %in% preve.pram[3:length(preve.pram)])
              if(ppsum<5) next()
            }
            
            pram.cycle<-F
            preve.pram<-c(trans.y,norming,withextra,missingdata,FN)
            (write.table(c(preve.pram),file="previous parameters.csv", quote = F,row.names = F))
            
            if(trans.y==1){

            if(norming=="quantile" && trans.y==1){ #quantilization leaves holes for algos to detect
              training[,1]<- (rank(training[,1],na.last = "keep",ties.method = "average")-1)
              mintrain<-min(training[,1],na.rm =F);maxtrain<-max(training[,1],na.rm =F);rangtrain<-maxtrain-mintrain
              training[,1]<-(training[,1]-mintrain)/rangtrain
              df.toprocess[,1]<- (rank(df.toprocess[,1],na.last = "keep",ties.method = "average")-1)
              df.toprocess[,1]<-(df.toprocess[,1]-mintrain)/rangtrain
            }
            
              library(data.table)
          if(norming=="centernscale"){
            preProcValues= preProcess(training[,1:2],method = c("center", "scale"))
            df.toprocess[,1]<- predict(preProcValues, df.toprocess[,1:2])[,1]}
          if(norming=="range01"){
            preProcValues= preProcess(training[,1:2],method = c("range"))
            df.toprocess[,1]<- predict(preProcValues, df.toprocess[,1:2])[,1]}
          if(norming=="expoTrans"){
            preProcValues= preProcess(training[,1:2],method = c("expoTrans"))
            df.toprocess[,1]<- predict(preProcValues, df.toprocess[,1:2])[,1]}
          if(norming=="YeoJohnson"){
            preProcValues= preProcess((training[,1:2]),method = c("YeoJohnson"))#"center", "scale",
            df.toprocess[,1]<- predict(preProcValues, df.toprocess[,1:2])[,1]
            }
            }
            
            df.toprocess = signif(df.toprocess,digits = 3)
            #df.toprocess = data.frame(df.toprocess,)
            nzv <- nearZeroVar(df.toprocess[,])#, saveMetrics= TRUE
            #nzv[nzv$nzv,][1:10,]
            if(length(nzv)>1){
              df.toprocess = (df.toprocess[, -nzv])}
            
            loess.model<-stats::loess(y.untransformed~ df.toprocess[,1],span = 0.21, degree = 1)
            
            training <- df.toprocess[-foldTrain[[FN]],]            
            testing  <- df.toprocess[foldTrain[[FN]],]
            
            train.based.mean<-signif(mean(training[,1], na.rm = T), digits = 4)
            test.based.mean<-signif(mean(testing[,1], na.rm = T), digits = 4)
            train.based.med<-signif(median(training[,1], na.rm = T), digits = 4)
            test.based.med<-signif(median(testing[,1], na.rm = T), digits = 4)
            
              un.train.based.mean<-signif(mean(y.untransformed[-foldTrain[[FN]]], na.rm = T), digits = 4)
              un.test.based.mean<-signif(mean(y.untransformed[foldTrain[[FN]]], na.rm = T), digits = 4)
              un.train.based.med<-signif(median(y.untransformed[-foldTrain[[FN]]], na.rm = T), digits = 4)
              un.test.based.med<-signif(median(y.untransformed[foldTrain[[FN]]], na.rm = T), digits = 4)

              RMSE.mean=signif(RMSE(y.untransformed[foldTrain[[FN]]],un.train.based.mean), digits = 4)
              RMSE.mean.train=signif(RMSE(training[,1],train.based.mean), digits = 4)
              #RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
            
          write.table(df.toprocess,file = "sanity check 1.csv",  quote = F, row.names = F, col.names = T)
          allmodel<-"perfect"; when<-0
          printPredMets(predicted.outcomes= testing[,1],overRMSE=-1,hypercount="none",libpack="ignore")
          
          ###########for all models#################
          gc()

          setwd(base.folder)
          if((pc.tpot==T)){
            source("pytptall.R")
            setwd(base.folder)
          }
          #stop()
          if(max(which.computer==pc.mlr)>0){
            source("MLR part.R")
            setwd(base.folder)
            
          } else {
          if(max(which.computer==pc.smallR)>0){
            #source("autoH2Oallmodel.R")
            setwd(base.folder)
            source("SuperLearnerAllmodel.R")
            setwd(base.folder)
            #source("subsemble.R")
            setwd(base.folder)
            source("SuperSuperAll.R")
            setwd(base.folder)
          } else {
            if(pc.caret){
             source("carEns3.R")
             setwd(base.folder)
             source("Caret part.R")
             setwd(base.folder)
            }
            }
          }
          

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

}
##########end#########
#stopCluster(cl)
## reset message sink and close the file connection
sink(type="message")
close(zz)

### Display the log file
readLines("all.Rout")
