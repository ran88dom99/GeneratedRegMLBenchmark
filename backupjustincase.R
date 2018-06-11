p <- data.frame(predicted.outcomes,testing[,1])
#Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],train.based.mean)
#mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],train.based.med)

if(trans.y==2){
  p<- data.frame(predicted.outcomes,y.untransformed[foldTrain[[FN]]])
}else{
  p<- data.frame(predict(loess.model,predicted.outcomes),y.untransformed[foldTrain[[FN]]])
}
#RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
RMSEp=RMSE(p[,1],p[,2])
MMAAEE=MAE(p[,1],p[,2])
#MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)  



#print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),
              signif(overRMSE,digits = 3),signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),
              date(),allmodel,column.to.predict,trans.y,datasource,missingdata,
              withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1:2],seed.var,RMSE.mean,RMSE.mean.train,adaptControl$search,round(proc.time()[3]-when[3]),
              adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,
              adaptControl$adaptive$min,trainedmodel$bestTune),
            file = out.file, append =TRUE, quote = F, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
print(date())


p <- data.frame(predicted.outcomes,testing[,1])
#Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],train.based.mean)
#mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],train.based.med)

if(trans.y==2){
  p<- data.frame(predicted.outcomes,y.untransformed[foldTrain[[FN]]])
}else{
  p<- data.frame(predict(loess.model,predicted.outcomes),y.untransformed[foldTrain[[FN]]])
}
#RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
RMSEp=RMSE(p[,1],p[,2])
MMAAEE=MAE(p[,1],p[,2])
#MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)

overRMSE=-1
wut=print(trainedmodel,selectCol=TRUE)
try({overRMSE=as.numeric(wut[wut[,length(wut[1,])]=="*","RMSE"])})#length(wut[1,])-
replace.overRMSE=1
try({if(is.numeric(overRMSE)){replace.overRMSE=0}})
if(replace.overRMSE==1){overRMSE=-1}
if(length(overRMSE)<1){overRMSE=-1}

#print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),signif(overRMSE,digits = 3),
              signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),date(),allmodel,column.to.predict,
              trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1:2],seed.var,RMSE.mean,RMSE.mean.train,simpleControl$search,
              round(proc.time()[3]-when[3]),simpleControl$method,tuneLength2,
              simpleControl$number,"no rep","no min",trainedmodel$bestTune),
            file = out.file, append =TRUE, quote = F, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
print(date())


p <- data.frame(predicted.outcomes,testing[,1])
#Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],train.based.mean)
#mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],train.based.med)

if(trans.y==2){
  p<- data.frame(predicted.outcomes,y.untransformed[foldTrain[[FN]]])
}else{
  p<- data.frame(predict(loess.model,predicted.outcomes),y.untransformed[foldTrain[[FN]]])
}
#RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
RMSEp=RMSE(p[,1],p[,2])
MMAAEE=MAE(p[,1],p[,2])
#MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T) 


overRMSE=-1
wut=print(trainedmodel,selectCol=TRUE)
try({wut=print(trainedmodel,selectCol=TRUE)
overRMSE=as.numeric(wut[wut[,length(wut[1,])]=="*","RMSE"])})#length(wut[1,])-
replace.overRMSE=1
try({if(is.numeric(overRMSE)){replace.overRMSE=0}})
if(replace.overRMSE==1){overRMSE=-1}
if(length(overRMSE)<1){overRMSE=-1}

#print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),signif(overRMSE,digits = 3),
              signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),date(),allmodel,column.to.predict,
              trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1:2],seed.var,RMSE.mean,RMSE.mean.train,simpleControl$search,
              round(proc.time()[3]-when[3]),"nohyperparameters",tuneLength2,
              simpleControl$number,"no rep","no min",trainedmodel$bestTune),
            file = out.file, append =TRUE, quote = F, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
print(date())



  p <- data.frame(predicted.outcomes$data[,2],testing[,1])
  #Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))

  Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],train.based.mean)
  #mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
  mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],train.based.med)
  if(trans.y==2){
    p<- data.frame(predicted.outcomes$data[,2],y.untransformed[foldTrain[[FN]]])
  }else{
    p<- data.frame(predict(loess.model,predicted.outcomes$data[,2]),y.untransformed[foldTrain[[FN]]])
  }
  #RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
  RMSEp=RMSE(p[,1],p[,2])
  MMAAEE=MAE(p[,1],p[,2])
  #RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
  #RMSE.mean=signif(RMSE(p[,2],mean(p[,2], na.rm = T)), digits = 4)
  #RMSE.mean.train=signif(RMSE(training[,1],mean(training[,1], na.rm = T)), digits = 4)
  #MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)

  #print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
  write.table(c(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),
                signif(overRMSE,digits = 3),signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),
                date(),allmodel,column.to.predict,trans.y,datasource,missingdata,
                withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1:2],seed.var,RMSE.mean,RMSE.mean.train,adaptControl$search,round(proc.time()[3]-when[3]),
                adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,
                adaptControl$adaptive$min,mod$x),
              file = out.file, append =TRUE, quote = F, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = F,
              col.names = F, qmethod = "double")
  print(date())
  
  p <- data.frame(predicted.outcomes$data[,2],testing[,1])
  #Rsqd =(1-sum((p[,2]-p[,1])^2, na.rm = T)/sum((p[,2]-mean(p[,2]))^2, na.rm = T))
  
  Rsqd=1-RMSE(p[,1],p[,2])/RMSE(p[,2],train.based.mean)
  #mean.improvement=1-mean(abs(p[,2]-p[,1]), na.rm = T)/mean(abs(p[,2]-median(p[,2])), na.rm = T)
  mean.improvement=1-MAE(p[,1],p[,2])/MAE(p[,2],train.based.med)
  if(trans.y==2){
    p<- data.frame(predicted.outcomes$data[,2],y.untransformed[foldTrain[[FN]]])
  }else{
    p<- data.frame(predict(loess.model,predicted.outcomes$data[,2]),y.untransformed[foldTrain[[FN]]])
  }
  #RMSE=(sqrt(mean((p[,1]-p[,2])^2, na.rm = T)))
  RMSEp=RMSE(p[,1],p[,2])
  MMAAEE=MAE(p[,1],p[,2])
  #RMSE.mean=(sqrt(mean((p[,2]-mean(p[,2]))^2, na.rm = T)))
  #RMSE.mean=signif(RMSE(p[,2],mean(p[,2], na.rm = T)), digits = 4)
  #RMSE.mean.train=signif(RMSE(training[,1],mean(training[,1], na.rm = T)), digits = 4)
  #MMAAEE=mean(abs(p[,2]-p[,1]), na.rm = T)

  NoAp<-"NoAp"
  NoHyper<-"nohyperparam"
  Rseed<-.Random.seed[1]
  Cseed<-.Random.seed[2]
  
  #print(c(Rsqd,RMSE,overRMSE,date(),allmodel,column.to.predict,datasource,missingdata,withextra,norming,adaptControl$search,seed.const,adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,adaptControl$adaptive$min,trainedmodel$bestTune))
  write.table(paste(round(mean.improvement,digits = 3),round(Rsqd,digits = 3),
                    signif(overRMSE,digits = 3),signif(RMSEp,digits = 3),signif(MMAAEE,digits = 3),
                    date(),allmodel,column.to.predict,trans.y,datasource,missingdata,
                    withextra,norming,which.computer,task.subject,FN,high.fold,Rseed,Cseed,seed.var,RMSE.mean,RMSE.mean.train,NoHyper,round(proc.time()[3]-when[3]),
                    adaptControl$method,tuneLength,adaptControl$number,adaptControl$repeats,
                    adaptControl$adaptive$min,mod$x, sep = ","),
              file = out.file, append =TRUE, quote = F, sep = ",",
              eol = "\n", na = "NA", dec = ".", row.names = F,
              col.names = F, qmethod = "double")
  
  print(date())