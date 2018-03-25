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
###########single common output function########
p <- data.frame(predicted.outcomes$data[,2],testing[,1])
#Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],mean(p[,2], na.rm = T))
#mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],median(p[,2], na.rm = T))
p<- data.frame(predict(loess.model,predicted.outcomes$data[,2]),y.untransformed[-inTrain])
#RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
RMSEp=RMSE(p[,1],p[,2])
#RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
RMSE.mean=signif(RMSE(p[,2],mean(p[,2], na.rm = T)), digits = 4)
#MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)
MMAAEE=MAE(p[,1],p[,2])


overRMSE=-1
overRMSE<-mod$y
#if(replace.overRMSE==1){overRMSE=-1}
if(length(overRMSE)<1){overRMSE=-1}

#print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),
              signif(overRMSE,digits = 3),signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),
              date(),allmodel,column.to.predict,trans.y,datasource,missingdata,
              withextra,norming,which.computer,task.subject,RMSE.mean,adaptControl$search,seed.var,round(proc.time()[3]-when[3]),
              adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,
              adaptControl$adaptive$min,mod$x),
            file = out.file, append =TRUE, quote = F, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = "double")


p <- data.frame(predicted.outcomes$data[,2],testing[,1])
#Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],mean(p[,2], na.rm = T))
#mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],median(p[,2], na.rm = T))
p<- data.frame(predict(loess.model,predicted.outcomes$data[,2]),y.untransformed[-inTrain])
#RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
RMSEp=RMSE(p[,1],p[,2])
#RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
RMSE.mean=signif(RMSE(p[,2],mean(p[,2], na.rm = T)), digits = 4)
#MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)
MMAAEE=MAE(p[,1],p[,2])


overRMSE=-1
#if(replace.overRMSE==1){overRMSE=-1}
if(length(overRMSE)<1){overRMSE=-1}
NoAp<-"NoAp"
NoHyper<-"nohyperparam"

#print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
write.table(paste(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),
                  signif(overRMSE,digits = 3),signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),
                  date(),allmodel,column.to.predict,trans.y,datasource,missingdata,
                  withextra,norming,which.computer,task.subject,RMSE.mean,NoHyper,seed.var,round(proc.time()[3]-when[3]),
                  adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,
                  adaptControl$adaptive$min, sep = ","),
            file = out.file, append =TRUE, quote = F, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = "double")


##############common output fail################



