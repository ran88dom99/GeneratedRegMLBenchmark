#ggplot: how  label, print to file
expiramentresults<-read.csv("Generated data output first complete run.csv", sep = ",",fill=TRUE, header = F,quote="",dec=".")
exp.name<-"First 332v100dp5n3n16"
mainDir<-getwd()
subDir<-exp.name
#dir.create(file.path(mainDir, subDir))
#setwd(file.path(mainDir, subDir))
#######percent failed######
fails.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,7]))
{
  countr=countr+1
  fails.df[countr,1]<-algo
  fails.df[countr,2]<-as.numeric(sum(expiramentresults[,7]==algo))
  fails.df[countr,3]<-as.numeric(sum((expiramentresults[,1]=="Fail")*(expiramentresults[,7]==algo), na.rm = T))
  fails.df[countr,4]<-as.numeric(sum((is.na(expiramentresults[,1]))*(expiramentresults[,7]==algo)))
  fails.df[countr,5]<-(as.numeric(fails.df[countr,4])+as.numeric(fails.df[countr,3]))/as.numeric(fails.df[countr,2])
}
iti <- order(fails.df[,5],-fails.df[,4])
fails.df<-rbind(fails.df)[iti,]
write.table(fails.df,
            file = paste(exp.name,"Fails.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
not.interesting<-(fails.df[,5]==1) + (fails.df[,5]<.01)
I.fails.df<-fails.df[!not.interesting,]
library(ggplot2)

z<-ggplot(I.fails.df, aes(y = I.fails.df[,4], x = reorder(I.fails.df[,1], I.fails.df[,5]))) + geom_point()+ coord_flip()
z+ geom_point(colour="red",aes(I.fails.df[,1] ,I.fails.df[,3]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.fails.df[,1] ,I.fails.df[,2]))+
  geom_point(colour="green",aes(I.fails.df[,1] ,(I.fails.df[,5]*.5*max(I.fails.df[,2]))))
ggsave(paste(exp.name,"Fails.png", sep = ""),plot = last_plot(),scale = 3)

#####time taken#####
time.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,7]))
{
  countr=countr+1
  only.algo<-(expiramentresults[,7]==algo)
  time.df[countr,1]<-algo
  time.df[countr,2]<-min(expiramentresults[only.algo,17],na.rm = T)
  time.df[countr,3]<-median(expiramentresults[only.algo,17],na.rm = T)
  time.df[countr,4]<-mean(expiramentresults[only.algo,17],na.rm = T)
  time.df[countr,5]<-max(expiramentresults[only.algo,17],na.rm = T)
}
iti <- order(time.df[,3],time.df[,4])
time.df<-rbind(time.df)[iti,]
write.table(time.df,
            file = paste(exp.name,"Time.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
not.interesting<-(time.df[,3]<40) 
I.time.df<-time.df[!not.interesting,]

z<-ggplot(I.time.df, aes(y = I.time.df[,3], x = reorder(I.time.df[,1], I.time.df[,3]))) + geom_point()+ coord_flip()
z+   geom_point(colour="blue",aes(I.time.df[,1] ,I.time.df[,2]))#+
  geom_point(colour="red",aes(I.time.df[,1] ,I.time.df[,4]),na.rm = TRUE)
#+
  #geom_point(colour="green",aes(I.time.df[,1] ,(I.time.df[,6]/max(I.time.df[,6]))))
  ggsave(paste(exp.name,"Time.png", sep = ""),plot = last_plot(),scale = 3)
  
######power to detect#####
exp.res.noF<-expiramentresults[,1]
for(countr in 1:length(exp.res.noF)){
  if(is.na(exp.res.noF[countr])){exp.res.noF[countr]<--.09}
  if(exp.res.noF[countr]=="Fail"){exp.res.noF[countr]<--.09}
}
exp.res.noF<-as.numeric(levels(exp.res.noF))[exp.res.noF]
power.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,7]))
{
  countr=countr+1
  only.algo<-as.logical((expiramentresults[,7]==algo)*(expiramentresults[,1]!="Fail"))
  only.algo[is.na(only.algo)]<-F
  if(sum(only.algo, na.rm=T)<1) {
    countr=countr-1    
    next()}
  power.df[countr,1]<-algo
  power.df[countr,2]<-min(exp.res.noF[only.algo],na.rm = T)
  power.df[countr,3]<-median(exp.res.noF[only.algo],na.rm = T)
  power.df[countr,4]<-round(mean(exp.res.noF[only.algo],na.rm = T),digits = 3)
  power.df[countr,5]<-max(exp.res.noF[only.algo],na.rm = T)
  power.df[countr,6]<-sum(only.algo, na.rm=T)
  power.df[countr,7]<-round(quantile(exp.res.noF[only.algo], probs = .8, na.rm =T),digits = 3)
}

iti <- order(power.df[,4],power.df[,3])
power.df<-rbind(power.df)[iti,]
write.table(power.df,
            file = paste(exp.name,"Power.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#power.df<-data.frame(power.df, row.names = 1:length(power.df[,1]))
not.interesting<-(power.df[,6]<17)+(power.df[,4]<.2)+ is.na(power.df[,6]) 
I.power.df<-power.df[!not.interesting,]

z<-ggplot(I.power.df, aes(y = I.power.df[,4], x = reorder(I.power.df[,1], I.power.df[,4]))) + geom_point()+ coord_flip()
z+ geom_point(colour="red",aes(I.power.df[,1] ,I.power.df[,7]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.power.df[,1] ,I.power.df[,3]))+
  geom_point(colour="green",aes(I.power.df[,1] ,(I.power.df[,6]/max(I.power.df[,6]))))
ggsave(paste(exp.name,"Power.png", sep = ""),plot = last_plot(),scale = 3)
#some algos are better at some category of tasks and these tasks are overepresented
#some tasks are harder and thus improvement in them should count for more
#####success by task#### 
fails.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,10]))
{
  countr=countr+1
  fails.df[countr,1]<-algo
  fails.df[countr,2]<-as.numeric(sum(expiramentresults[,10]==algo))
  fails.df[countr,3]<-as.numeric(sum((expiramentresults[,1]=="Fail")*(expiramentresults[,10]==algo), na.rm = T))
  fails.df[countr,4]<-as.numeric(sum((is.na(expiramentresults[,1]))*(expiramentresults[,10]==algo)))
  fails.df[countr,5]<-(as.numeric(fails.df[countr,4])+as.numeric(fails.df[countr,3]))/as.numeric(fails.df[countr,2])
}
iti <- order(fails.df[,5],-fails.df[,4])
fails.df<-rbind(fails.df)[iti,]
write.table(fails.df,
            file = paste(exp.name,"FailsTask.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
not.interesting<-(fails.df[,5]==1) + (fails.df[,5]<.01)
I.fails.df<-fails.df#[!not.interesting,]


z<-ggplot(I.fails.df, aes(y = I.fails.df[,4], x = reorder(I.fails.df[,1], I.fails.df[,5]))) + geom_point()+ coord_flip()
z+ geom_point(colour="green",aes(I.fails.df[,1] ,I.fails.df[,3]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.fails.df[,1] ,I.fails.df[,2]*170/max(I.fails.df[,2])))+
  geom_point(colour="red",aes(I.fails.df[,1] ,(I.fails.df[,5]*.5*max(I.fails.df[,2]))))
ggsave(paste(exp.name,"FailTask.png", sep = ""),plot = last_plot(),scale = 3)

#####time taken by task#####
time.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,10]))
{
  countr=countr+1
  only.algo<-(expiramentresults[,10]==algo)
  time.df[countr,1]<-algo
  time.df[countr,2]<-min(expiramentresults[only.algo,17],na.rm = T)
  time.df[countr,3]<-median(expiramentresults[only.algo,17],na.rm = T)
  time.df[countr,4]<-mean(expiramentresults[only.algo,17],na.rm = T)
  time.df[countr,5]<-max(expiramentresults[only.algo,17],na.rm = T)
}
iti <- order(time.df[,3],time.df[,4])
time.df<-rbind(time.df)[iti,]
write.table(time.df,
            file = paste(exp.name,"TimeTask.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
not.interesting<-(time.df[,3]<40) 
I.time.df<-time.df#[!not.interesting,]

z<-ggplot(I.time.df, aes(y = I.time.df[,3], x = reorder(I.time.df[,1], I.time.df[,3]))) + geom_point()+ coord_flip()
z+   geom_point(colour="blue",aes(I.time.df[,1] ,I.time.df[,2]))#+
ggsave(paste(exp.name,"TimeTask.png", sep = ""),plot = last_plot(),scale = 3)

#geom_point(colour="red",aes(I.time.df[,1] ,I.time.df[,4]),na.rm = TRUE)
#+
#geom_point(colour="green",aes(I.time.df[,1] ,(I.time.df[,6]/max(I.time.df[,6]))))

######power to detect by task#####
exp.res.noF<-expiramentresults[,1]
for(countr in 1:length(exp.res.noF)){
  if(is.na(exp.res.noF[countr])){exp.res.noF[countr]<--.09}
  if(exp.res.noF[countr]=="Fail"){exp.res.noF[countr]<--.09}
}
exp.res.noF<-as.numeric(levels(exp.res.noF))[exp.res.noF]
power.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,10]))
{
  countr=countr+1
  only.algo<-as.logical((expiramentresults[,10]==algo)*(expiramentresults[,1]!="Fail"))
  only.algo[is.na(only.algo)]<-F
  if(sum(only.algo, na.rm=T)<1) {
    countr=countr-1    
    next()}
  power.df[countr,1]<-algo
  power.df[countr,2]<-min(exp.res.noF[only.algo],na.rm = T)
  power.df[countr,3]<-median(exp.res.noF[only.algo],na.rm = T)
  power.df[countr,4]<-round(mean(exp.res.noF[only.algo],na.rm = T),digits = 3)
  power.df[countr,5]<-max(exp.res.noF[only.algo],na.rm = T)
  power.df[countr,6]<-sum(only.algo, na.rm=T)
  power.df[countr,7]<-round(quantile(exp.res.noF[only.algo], probs = .85, na.rm =T),digits = 3)
}

iti <- order(power.df[,4],power.df[,3])
power.df<-rbind(power.df)[iti,]
write.table(power.df,
            file = paste(exp.name,"PowerTask.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#power.df<-data.frame(power.df, row.names = 1:length(power.df[,1]))
not.interesting<-(power.df[,6]<10)+ is.na(power.df[,6]) 
I.power.df<-power.df[!not.interesting,]

z<-ggplot(I.power.df, aes(y = I.power.df[,4], x = reorder(I.power.df[,1], I.power.df[,4]))) + geom_point()+ coord_flip()
z+ geom_point(colour="red",aes(I.power.df[,1] ,I.power.df[,5]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.power.df[,1] ,I.power.df[,3]))+
  geom_point(colour="green",aes(I.power.df[,1] ,(I.power.df[,6]/max(I.power.df[,6]))))
ggsave(paste(exp.name,"PowerTask.png", sep = ""),plot = last_plot(),scale = 3)
#some algos are better at some category of tasks and missing them could affect statistic metric index
########difference between %MAE & %RMSE by task####
exp.res.noF2<-expiramentresults[,2]
for(countr in 1:length(exp.res.noF2)){
  if(is.na(exp.res.noF2[countr])){exp.res.noF2[countr]<--.09}
  if(exp.res.noF2[countr]=="Fail"){exp.res.noF2[countr]<--.09}
}
exp.res.noF2<-as.numeric(levels(exp.res.noF2))[exp.res.noF2]
Results.Defactor<-data.frame(exp.res.noF,exp.res.noF2,expiramentresults[,3:length(expiramentresults[1,])])

exp.res.noFZ<-exp.res.noF;exp.res.noFZ2<-exp.res.noF2
for(countr in 1:length(exp.res.noFZ))
{if(exp.res.noFZ[countr]<0) exp.res.noFZ[countr]<-0
  if(exp.res.noFZ2[countr]<0) exp.res.noFZ2[countr]<-0 }
rel.res.diff<-(round((exp.res.noFZ-exp.res.noFZ2)/max((exp.res.noFZ)+(exp.res.noFZ2)),digits = 3))
res.diff<-(round((exp.res.noFZ-exp.res.noFZ2),digits = 3))


diff.df<-data.frame()
countr=0
for(algo in unique(Results.Defactor[,10]))
{
  countr=countr+1
  only.algo<-as.logical((Results.Defactor[,10]==algo)*(expiramentresults[,1]!="Fail"))
  only.algo[is.na(only.algo)]<-F
  if(sum(only.algo, na.rm=T)<1) {
    countr=countr-1    
    next()}
  diff.df[countr,1]<-algo
  diff.df[countr,2]<-sum(only.algo, na.rm=T)  
  fivenumber<-round(fivenum(res.diff[only.algo],na.rm = T),digits = 3)
  diff.df[countr,3]<-(fivenumber[1]);diff.df[countr,4]<-(fivenumber[2])
  diff.df[countr,5]<-(fivenumber[3]);diff.df[countr,6]<-(fivenumber[4]);diff.df[countr,7]<-(fivenumber[5])
  diff.df[countr,8]<-round(mean(res.diff[only.algo],na.rm = T),digits = 3)
  fivenumber<-round(fivenum(rel.res.diff[only.algo],na.rm = T),digits = 3)
  diff.df[countr,9]<-(fivenumber[1]);diff.df[countr,10]<-(fivenumber[2])
  diff.df[countr,11]<-(fivenumber[3]);diff.df[countr,12]<-(fivenumber[4]);diff.df[countr,13]<-(fivenumber[5])
  
  diff.df[countr,14]<-round(mean(rel.res.diff[only.algo],na.rm = T),digits = 3)
}

iti <- order(diff.df[,5],diff.df[,11])
diff.df<-rbind(diff.df)[iti,]
write.table(diff.df,
            file = paste(exp.name,"DiffTask.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#diff.df<-data.frame(diff.df, row.names = 1:length(diff.df[,1]))
not.interesting<-(diff.df[,6]<10)+ is.na(diff.df[,6]) 
I.diff.df<-diff.df#[!not.interesting,]

z<-ggplot(I.diff.df, aes(y = I.diff.df[,5], x = reorder(I.diff.df[,1], I.diff.df[,5]))) + geom_point()+ coord_flip()
z+ geom_point(colour="red",aes(I.diff.df[,1] ,I.diff.df[,8]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.diff.df[,1] ,I.diff.df[,12]))+
  geom_point(colour="green",aes(I.diff.df[,1] ,(I.diff.df[,14])))
ggsave(paste(exp.name,"DiffTask.png", sep = ""),plot = last_plot(),scale = 3)

#########acceptable minimum######
acceptablePloss<-.05
lowestFindScore<-.1
acceptAlgo.df<-data.frame()
acceptAlgo.df[1,1]<-NA
acceptAlgo.df[,c(1:220)]<-NA
countr=0
for(algo in unique(expiramentresults[,10]))
{
  countr=countr+1
  only.algo<-as.logical((expiramentresults[,10]==algo)*(expiramentresults[,1]!="Fail"))
  only.algo[is.na(only.algo)]<-F
  if(sum(only.algo, na.rm=T)<1) {
    countr=countr-1    
    next()}
  taskSeek=(power.df[,1]==algo)
  if(power.df[taskSeek,5]<lowestFindScore) {countr=countr-1 ; next()}
  minAccept<-power.df[taskSeek,5]*(1-acceptablePloss)
  acceptAlgo.df[countr,1]<-algo
  #ranks<-rank(exp.res.noF[only.algo])
  acceptAlgo<-as.logical((exp.res.noF>minAccept)*only.algo)
  acceptAlgo.df[countr,2]<-sum(acceptAlgo)
  acceptAlgo.df[countr,3]<-minAccept
  acceptAlgo.df[countr,4:(sum(acceptAlgo)+3)]<-as.character(expiramentresults[acceptAlgo,7])
 }


write.table(acceptAlgo.df,
            file = paste(exp.name,acceptablePloss,"MinNecessary.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")



