
#source(functionsallautotest.R)
#######not to redo a test function in functions source#####
check.redundant<-function(df=df.previous.calcs,norming="asis",trans.y=1,withextra="missing",missingdata="leaveempty",datasource="mean" ,column.to.predict=200,allmodel="ctree",FN=1)
{
  for(intern in 1:length(df[,1])){
    if((any(df[intern,11:16] == norming, na.rm=T))&&
       (any(df[intern,10:16] == withextra, na.rm=T))&&
       (any(df[intern,10:16] == missingdata, na.rm=T))&&
       (any(df[intern,9:16] == datasource, na.rm=T))&&
       (any(df[intern,6:9] == column.to.predict, na.rm=T))&&
       (any(df[intern,5:9] == allmodel, na.rm=T))&&
       (any(df[intern,15:17] == FN, na.rm=T))&&
       (  (df[intern,9] == trans.y)))
    {return(TRUE)}
  }
  return(FALSE)
}

#The discussion in this section is somewhat more technical than in other parts of this document.
#However, it details one of the major differences between S-Plus and R.
#The symbols which occur in the body of a function can be divided into three classes; formal
#parameters, local variables and free variables. The formal parameters of a function are those
#occurring in the argument list of the function. Their values are determined by the process of
#binding the actual function arguments to the formal parameters. Local variables are those whose
#values are determined by the evaluation of expressions in the body of the functions. Variables
#which are not formal parameters or local variables are called free variables. Free variables become
#local variables if they are assigned to. Consider the following function definition.
#f <- function(x) {
#  y <- 2*x
#  print(x)
#  print(y)
#  print(z)
#}
#In this function, x is a formal parameter, y is a local variable and z is a free variable.
#In R the free variable bindings are resolved by first looking in the environment in which the
#function was created. This is called lexical scope. First we define a function called cube.
#cube <- function(n) {
#  sq <- function() n*n
#  n*sq()
#}
#The variable n in the function sq is not an argument to that function. Therefore it is a free
#variable and the scoping rules must be used to ascertain the value that is to be associated with
#it. Under static scope (S-Plus) the value is that associated with a global variable named n.
#Under lexical scope (R) it is the parameter to the function cube since that is the active binding
#for the variable n at the time the function sq was defined. The difference between evaluation
#in R and evaluation in S-Plus is that S-Plus looks for a global variable called n while R first
#looks for a variable called n in the environment created when cube was invoked.

CrashNRep<-function(allmodeli=allmodel){
#check if model crashes, or model with these params been done, 
#first write crash 
  write.table(allmodeli,file = "last algorithm tried.csv",  quote = F, row.names = F,col.names = F)
write.table(gens.names[gend.data],file = "last task tried.csv",  quote = F, row.names = F,col.names = F)
if(allmodeli %in% bad.models) {return(T)}
if(length(df.previous.calcs[,1])>0){
  if(check.redundant(df=df.previous.calcs,norming=norming,trans.y=trans.y,withextra=withextra,missingdata=missingdata,datasource=datasource ,column.to.predict=column.to.predict,allmodel=allmodeli,FN=FN))
  {return(T)}
}
return(F)
}

#Input: predictions,  overrmse, hyperparams or not
#Input thats just envirnoment: Many precalculated scores, y.untrans, loess.model, foldtrain & FN
#only output is printing


printPredMets<-function(predicted.outcomes=predicted.outcomes,overRMSE=overRMSE,hypercount="none",libpack="notune")
{
  #hypercount=c("full","part","none")
  p <- data.frame(predicted.outcomes,testing[,1])
  #Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
  Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],train.based.mean)
  #mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
  mean.improvement<<-1-MAE(p[,1],p[,2])/MAE(p[,2],train.based.med)

  if(is.data.frame(predicted.outcomes))
    predicted.outcomes<-as.vector(predicted.outcomes[,1])  
  testIndex<-foldTrain[[FN]]
  
  if(trans.y==2){
    p<- data.frame(predicted.outcomes,y.untransformed[testIndex])
  }else{
    p<- data.frame(predict(loess.model,predicted.outcomes),y.untransformed[testIndex])
  }
  #RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
  RMSEp=RMSE(p[,1],p[,2])
  MMAAEE=MAE(p[,1],p[,2])
  #MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)  
  #RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
  #RMSE.mean=signif(RMSE(p[,2],mean(p[,2], na.rm = T)), digits = 4)
  #RMSE.mean.train=signif(RMSE(training[,1],mean(training[,1], na.rm = T)), digits = 4)
  #MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)

Rseed<-.Random.seed[1]
Cseed<-.Random.seed[2]

outCtrl<-adaptControl

for(i in 1:5){outCtrl$bestune[i]<-""}
if(libpack=="caret"){
  for(i in 1:5){
    if(length(trainedmodel$bestTune)==(i-1)){break}
    try({outCtrl$bestune[i]<-signif(trainedmodel$bestTune[i],digits = 3)})
} } 
if(libpack=="mlr"){
  for(i in 1:5){
    if(length(mod$x)==(i-1)){break}
    try({outCtrl$bestune[i]<-signif(as.numeric(mod$x[i]),digits = 3)})
 } }

if(hypercount=="full")
{
  outCtrl$search<-adaptControl$search
  outCtrl$method<-adaptControl$method
  outCtrl$tuneLength<-tuneLength
  outCtrl$number<-adaptControl$number
  outCtrl$repeats<-adaptControl$repeats
  outCtrl$adaptivemin<-adaptControl$adaptive$min
}
if(hypercount=="part")
{
  outCtrl$search<-simpleControl$search
  outCtrl$method<-simpleControl$method
  outCtrl$tuneLength<-tuneLength2
  outCtrl$number<-simpleControl$number
  outCtrl$repeats<-"no rep"
  outCtrl$adaptivemin<-"no min"
}
if(hypercount=="none")
{
  outCtrl$search<-simpleControl$search
  outCtrl$method<-"nohyperparameters"
  outCtrl$tuneLength<-1
  outCtrl$number<-simpleControl$number
  outCtrl$repeats<-"no rep"
  outCtrl$adaptivemin<-"no min"
}

if(length(testIndex)!= length(as.vector(predicted.outcomes))){
  warning("test index and length of predictions do not match", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
        domain = NULL)
} 
InxdPred<-vector(mode="double",length = length(testIndex)*2)
for(i in 1:length(testIndex)){
  InxdPred[i*2]<-testIndex[i] 
  InxdPred[i*2+1]<-signif(predicted.outcomes[i],digits = 3)
}



writeout<- paste(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),signif(overRMSE,digits = 3),
                   signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),date(),allmodel,column.to.predict,
                   trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,
                   Rseed,Cseed,seed.var,RMSE.mean,RMSE.mean.train,outCtrl$search,
                   round(proc.time()[3]-when[3]),outCtrl$method,outCtrl$tuneLength,
                   outCtrl$number,outCtrl$repeats,outCtrl$adaptivemin,
                   outCtrl$bestune[1:5],InxdPred))
for(i in 2:length(writeout)){
  writeout[1]<-paste(writeout[1],writeout[i],sep=",")}

#print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
write.table( writeout[1],
            file = out.file, append =TRUE, quote = F, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
print(date())
}


############bunch of scraps kept just in case########
if(F){
  libpack="mlr"
  hypercount="none"
  pipin<-predicted.outcomes$data[,2]
  predicted.outcomes<-pipin
 writeout<- paste(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),signif(overRMSE,digits = 3),
    signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),date(),allmodel,column.to.predict,
    trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,
    Rseed,Cseed,seed.var,RMSE.mean,RMSE.mean.train,outCtrl$search,
    round(proc.time()[3]-when[3]),outCtrl$method,outCtrl$tuneLength,
    outCtrl$number,outCtrl$repeats,outCtrl$adaptivemin,
    outCtrl$bestune[1:5],signif(predicted.outcomes,digits = 3)))
 for(i in 2:length(writeout)){
   writeout[1]<-paste(writeout[1],writeout[i],sep=",")}

 ?paste
 paste(writeout[1:40],sep=",",collapse = T)
  
write.table(paste(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),
                  signif(overRMSE,digits = 3),signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),
                  date(),allmodel,column.to.predict,trans.y,datasource,missingdata,
                  withextra,norming,which.computer,task.subject,FN,high.fold,
                  Rseed,Cseed,seed.var,RMSE.mean,RMSE.mean.train,
                  NoHyper,round(proc.time()[3]-when[3]),
                  adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,
                  adaptControl$adaptive$min,mod$x, sep = ","),
            file = out.file, append =TRUE, quote = F, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),
              signif(overRMSE,digits = 3),signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),
              date(),allmodel,column.to.predict,trans.y,datasource,missingdata,
              withextra,norming,which.computer,task.subject,FN,high.fold,
              .Random.seed[1:2],seed.var,RMSE.mean,RMSE.mean.train,adaptControl$search,round(proc.time()[3]-when[3]),
              adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,
              adaptControl$adaptive$min,mod$x),
            file = out.file, append =TRUE, quote = F, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
}

if(F)
{
  overRMSE=-1
  overRMSE<-mod$y
  #if(replace.overRMSE==1){overRMSE=-1}
  if(length(overRMSE)<1){overRMSE=-1}
  
  predicted.outcomes$data[,2]
  NoAp<-"NoAp"
  NoHyper<-"nohyperparam"
}