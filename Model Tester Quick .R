## capture messages and errors to a file.
#zz <- file("all.Rout", open="wt")
#sink(zz, type="message")

#try(log("a"))


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


before.last.alg<-as.matrix(read.csv("beforelast algorithm.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
last.alg<-as.matrix(read.csv("last algorithm tried.csv", sep = ",",fill=TRUE, header = FALSE,quote="",dec="."))
if(last.alg==before.last.alg){print("algorithm may be broken")}
write.table(last.alg,file = "beforelast algorithm.csv",  quote = F, row.names = F,col.names = F)


#make sure not to redo a test
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
               "pcr","pls","qrf","ranger","rf"
)
#allmodels <- c("ranger")#"BstLm","enet","lasso",
#allmodels <- c("rf")"rqlasso",, "xyf" "rvmPoly", "rvmRadial",    "spls", "superpc" ,   "treebag",  "svmLinear2",  "SBC",
#allmodels <- c("bartMachine", "xgbLinear", "pcaNNet","svmLinear","glmnet","cforest","cubist","rf","ranger")"glmnet",
#wow rfRules is really slow "rfRules","WM", takes 50min
# brak everythig "rbfDDA","ridge","rqnc",
# use "rf" to test all
allmodels <- unique(modelLookup()[modelLookup()$forReg,c(1)])

#library(doParallel); cl <- makeCluster(detectCores()); registerDoParallel(cl)


adaptControl <- trainControl(method = "adaptive_cv",
                             number = 8, repeats = 5,
                             adaptive = list(min = 4, alpha = 0.05,
                                             method = "gls", complete = FALSE),
                             search = "random")
adaptControl <-trainControl(method = "cv",
                            number = 6,
                            search = "random")
simpleControl <- trainControl(method = "cv",
                              number = 6,
                              search = "random")


seed.const=222
seed.var=seed.const
column.to.predict=1

print(date());

if(!exists("gen.count")){gen.count=17}
gens.names<-as.matrix(read.table("gens names.csv", sep = ",",header = FALSE,row.names=1,fill=TRUE, quote="",dec="."))
for(gend.data in 1:gen.count){
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
          df.toprocess = (df.toprocess[, -nzv])
          
          tuneLength=5
          tuneLength2=4
          
          set.seed(seed.var)
          inTrain <- createDataPartition(y = df.toprocess[,1],
                                         p = .75,
                                         list = FALSE)
          training <- df.toprocess[ inTrain,]
          testing  <- df.toprocess[-inTrain,]
          write.table(df.toprocess,file = "sanity check 1.csv",  quote = F, row.names = F,col.names = F)
          
          
          ######for all models########
          for(allmodel in allmodels){#just before all models define d.f and reduce it
            write.table(allmodel,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
            bad.models=c("neuralnet","partDSA","blackboost","bstSm","bstTree","penalized","brnn","gamLoess","ANFIS","FIR.DM","FS.HGD","nodeHarvest","mlpWeightDecayML","monmlp","mlp","mlpWeightDecay","mlpSGD","rbf","rbfDDA","rfRules","GFS.FR.MOGUL","mlpML","HYFIS","GFS.THRIFT" ,"GFS.LT.RS")
            #too slow neuralnet
            if(allmodel %in% bad.models) {next()} #gamLoess crashes. the capitals are slow and terrible
            library(caret) #mlp...s creat some bizzare problem that breaks caret::train ##nodeHarvest is SLOW ##"rbf"crash R "rbfDDA" crash train and really bad #rfRules is REALLY slow.##"pythonKnnReg",pythonKnnReg can not install
            #penalized slow hen fails
            seed.var=seed.var+1
            list.of.packages <-getModelInfo(allmodel)[[1]]$library
            new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
            if(length(new.packages)) install.packages(new.packages, dep = TRUE)
            if(length(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])){
              write.table(paste("Fail","Fail","Fail","Fail","PackageFail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,round(proc.time()[3]-when[3]),  sep = ","),
                          file = "gen test out.csv", append =TRUE, quote = F, sep = ",",
                          eol = "\n", na = "NA", dec = ".", row.names = F,
                          col.names = F, qmethod = "double")
              next()}
            when<-proc.time()
            
            if(length(df.previous.calcs[,1])>0){
              if(check.redundant(df=df.previous.calcs,norming=norming,trans.y=trans.y,withextra=withextra,missingdata=missingdata,datasource=datasource ,column.to.predict=column.to.predict,allmodel=allmodel)){next}}
            not.failed=0
            set.seed(seed.var)
            try({trainedmodel <- train(x=data.frame(training[,2:length(training[1,])]),
                                       y = df.toprocess[inTrain,1],
                                       method = allmodel,
                                       trControl = adaptControl,
                                       tuneLength = tuneLength)
            #SVMS crash cause mean needs na.rm to = T #
            predicted.outcomes<-predict(trainedmodel, newdata=(testing))
            p <- data.frame(predicted.outcomes,testing)
            #Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
            Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],mean(p[,2], na.rm = T))
            #mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
            mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],mean(p[,2], na.rm = T))
            p<- data.frame(predict(loess.model,predicted.outcomes),y.untransformed[-inTrain])
            #RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
            RMSE=RMSE(p[,1],p[,2])
            #RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
            RMSE.mean=RMSE(p[,2],mean(p[,2], na.rm = T))
            #mae=mean(abs(p[,2]-p[,1]), na.rm = T)
            mae=MAE(p[,1],p[,2])
            
            wut=print(trainedmodel,selectCol=TRUE)
            overRMSE=as.numeric(wut[wut[,length(wut[1,])]=="*",length(wut[1,])-3])
            replace.overRMSE=1
            try({if(is.numeric(overRMSE)){replace.overRMSE=0}})
            if(replace.overRMSE==1){overRMSE=-1}
            if(length(overRMSE)<1){overRMSE=-1}
            
            #print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
            write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),round(overRMSE,digits = 3),round(RMSE,digits = 3),round(mae,digits = 3),date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,RMSE.mean,adaptControl$search,seed.var,round(proc.time()[3]-when[3]),adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune),
                        file = "gen test out.csv", append =TRUE, quote = F, sep = ",",
                        eol = "\n", na = "NA", dec = ".", row.names = F,
                        col.names = F, qmethod = "double")
            print(date())
            not.failed=1
            })
            
            if(not.failed==0) {
              try({trainedmodel <- train(x=data.frame(training[,2:length(training[1,])]),
                                         y =  df.toprocess[inTrain,1],
                                         method = allmodel,
                                         trControl = simpleControl,
                                         tuneLength = tuneLength2)
              
              predicted.outcomes<-predict(trainedmodel, newdata=(testing))
              p <- data.frame(predicted.outcomes,testing)
              #Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
              Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],mean(p[,2], na.rm = T))
              #mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
              mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],mean(p[,2], na.rm = T))
              p<- data.frame(predict(loess.model,predicted.outcomes),y.untransformed[-inTrain])
              #RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
              RMSE=RMSE(p[,1],p[,2])
              #RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
              RMSE.mean=RMSE(p[,2],mean(p[,2], na.rm = T))
              #mae=mean(abs(p[,2]-p[,1]), na.rm = T)
              mae=MAE(p[,1],p[,2])
              print(confusionMatrix(p[,1],p[,2]))
              
              
              wut=print(trainedmodel,selectCol=TRUE)
              overRMSE=as.numeric(wut[wut[,length(wut[1,])]=="*",length(wut[1,])-3])
              replace.overRMSE=1
              try({if(is.numeric(overRMSE)){replace.overRMSE=0}})
              if(replace.overRMSE==1){overRMSE=-1}
              if(length(overRMSE)<1){overRMSE=-1}
              
              #print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
              write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),round(overRMSE,digits = 3),round(RMSE,digits = 3),round(mae,digits = 3),date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,RMSE.mean,simpleControl$search,seed.var,round(proc.time()[3]-when[3]),simpleControl$method,tuneLength2,simpleControl$number,"no rep","no min",trainedmodel$bestTune),
                          file = "gen test out.csv", append =TRUE, quote = F, sep = ",",
                          eol = "\n", na = "NA", dec = ".", row.names = F,
                          col.names = F, qmethod = "double")
              print(date())
              not.failed=1
              })
              
            }
            if(not.failed==0) {
              print(c("failed","failed",date(),datasource,missingdata,withextra,norming,allmodel))
              write.table(paste("Fail","Fail","Fail","Fail","Fail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,round(proc.time()[3]-when[3]),  sep = ","),
                          file = "gen test out.csv", append =TRUE, quote = F, sep = ",",
                          eol = "\n", na = "NA", dec = ".", row.names = F,
                          col.names = F, qmethod = "double")
              write.table(paste("Fail",date(),allmodel,  sep = ", "),
                          file = "backup.csv", append =TRUE, quote = F, sep = ",",
                          eol = "\n", na = "NA", dec = ".", row.names = F,
                          col.names = F, qmethod = "double")
            }
            if(not.failed==1) {
              write.table(paste("Succ",date(),allmodel,  sep = ", "),
                          file = "backup.csv", append =TRUE, quote = F, sep = ",",
                          eol = "\n", na = "NA", dec = ".", row.names = F,
                          col.names = F, qmethod = "double")}
          }}}}}}
#stopCluster(cl)
## reset message sink and close the file connection
sink(type="message")
close(zz)

### Display the log file
readLines("all.Rout")