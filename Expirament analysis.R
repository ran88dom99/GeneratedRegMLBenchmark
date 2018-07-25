#ggplot: how  label, print to file
exp.name<-"acefoldbadpca"
mainDir<-getwd()
subDir<-exp.name
expiramentresults<-read.csv("acefoldbadpca.csv", sep = ",",fill=TRUE, header =T,quote="",dec=".")
gene.expect<-matrix(data = NA, nrow = 100, ncol = 100, byrow = FALSE,
                    dimnames = NULL)#gene.expect<-read.csv("gens names.csv", sep = ",",fill=TRUE, header = F,quote="",dec=".")
dir.create(file.path(mainDir, subDir))
setwd(file.path(mainDir, subDir))

#for highfold
expiramentresults[,10] <- paste(expiramentresults[,10],expiramentresults[,16],sep="_")
expiramentresults<-expiramentresults[(expiramentresults[,7]!="perfect"),]

library(ggplot2)    
library(reshape2) 
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
not.interesting<-(fails.df[,5]==1) + (fails.df[,5]<.001)
I.fails.df<-fails.df[!not.interesting,]
library(ggplot2)

z<-ggplot(I.fails.df, aes(y = I.fails.df[,4], x = reorder(I.fails.df[,1], I.fails.df[,5]))) + geom_point()+ coord_flip()
z+ geom_point(colour="red",aes(I.fails.df[,1] ,I.fails.df[,3]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.fails.df[,1] ,I.fails.df[,2]))+
  geom_point(colour="green",aes(I.fails.df[,1] ,(I.fails.df[,5]*.5*max(I.fails.df[,2]))))+
  ylab(paste("Failures, R %fail/NA, Bu Attempts, Bk NA, G Fails")) + xlab("")
ggsave(paste(exp.name,"Fails.png", sep = ""),plot = last_plot(),scale = 3)

#####time taken#####
time.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,7]))
{
  countr=countr+1
  only.algo<-(expiramentresults[,7]==algo)
  time.df[countr,1]<-algo
  time.df[countr,2]<-min(expiramentresults[only.algo,21],na.rm = T)
  time.df[countr,3]<-median(expiramentresults[only.algo,21],na.rm = T)
  time.df[countr,4]<-mean(expiramentresults[only.algo,21],na.rm = T)
  time.df[countr,5]<-max(expiramentresults[only.algo,21],na.rm = T)
}
iti <- order(time.df[,3],time.df[,4])
time.df<-rbind(time.df)[iti,]
write.table(time.df,
            file = paste(exp.name,"Time.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
not.interesting<-(time.df[,3]<10) 
I.time.df<-time.df[!not.interesting,]
time.df[,2]<-log(time.df[,2])
time.df[,3]<-log(time.df[,3])

z<-ggplot(I.time.df, aes(y = I.time.df[,3], x = reorder(I.time.df[,1], I.time.df[,3]))) + geom_point()+ coord_flip()
z+   geom_point(colour="blue",aes(I.time.df[,1] ,I.time.df[,2]))+
  ylab(paste("time taken in log seconds, min and Median")) + xlab("")


  #geom_point(colour="green",aes(I.time.df[,1] ,(I.time.df[,6]/max(I.time.df[,6]))))
  ggsave(paste(exp.name,"Time.png", sep = ""),plot = last_plot(),scale = 3)
  
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
  geom_point(colour="red",aes(I.fails.df[,1] ,(I.fails.df[,5]*.5*max(I.fails.df[,2]))))+
  ylab(paste("Failures, R %fail/NA, Bu Attempts, Bk NA, G Fails")) + xlab("")

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
z+   geom_point(colour="blue",aes(I.time.df[,1] ,I.time.df[,2]))+
  ylab(paste("time taken in seconds, min and Median")) + xlab("")
ggsave(paste(exp.name,"TimeTask.png", sep = ""),plot = last_plot(),scale = 3)

#geom_point(colour="red",aes(I.time.df[,1] ,I.time.df[,4]),na.rm = TRUE)
#+
#geom_point(colour="green",aes(I.time.df[,1] ,(I.time.df[,6]/max(I.time.df[,6]))))

######power to detect by task#####
exp.res.noF<-expiramentresults[,1]
exp.res.noF<-as.numeric(levels(exp.res.noF))[exp.res.noF]
exp.res.noF[is.na(exp.res.noF)]<-0
exp.res.noF[exp.res.noF<(-3)]<-(-3)
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
  power.df[countr,7]<-round(quantile(exp.res.noF[only.algo], probs = .95, na.rm =T),digits = 3)
  power.df[countr,8]<-round(quantile(exp.res.noF[only.algo], probs = .75, na.rm =T),digits = 3)
  
}

iti <- order(power.df[,5],power.df[,7])
power.df<-rbind(power.df)[iti,]
write.table(power.df,
            file = paste(exp.name,"PowerTask.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#power.df<-data.frame(power.df, row.names = 1:length(power.df[,1]))
not.interesting<-(power.df[,6]<10)+ is.na(power.df[,6]) 
I.power.df<-power.df[!not.interesting,]

z<-ggplot(I.power.df, aes(y = I.power.df[,7], x = reorder(I.power.df[,1], I.power.df[,5]))) + geom_point()+ coord_flip()
z+ geom_point(colour="red",aes(I.power.df[,1] ,I.power.df[,5]),na.rm = TRUE)+scale_x_discrete(position = "top")+
  geom_point(colour="blue",aes(I.power.df[,1] ,I.power.df[,3]))+
  #geom_point(colour="purple",aes(I.power.df[,1] ,I.power.df[,3]))+
  geom_point(colour="green",aes(I.power.df[,1] ,(I.power.df[,6]/max(I.power.df[,6])-.35)))+
  ylab(paste("%MAE, Bk 95th quantile, R max, Bu median, G Suc.Attempts")) + xlab("")
ggsave(paste(exp.name,"PowerTask.png", sep = ""),plot = last_plot(),scale = 3)

ttdd<-data.frame()
ttdz<-data.frame()
iti <- order(power.df[,4],power.df[,3])
power.df<-rbind(power.df)[iti,]
orderedlevels<-factor(expiramentresults[,10], power.df[,1],  ordered = T)
ttdz<-data.frame(exp.res.noF,orderedlevels)
ttdd<-ttdz[expiramentresults[,1]!="Fail",]

p <- ggplot(as.data.frame(ttdd), aes(x=melt(ttdd)[,1],y=melt(ttdd)[,3]))
p  +geom_boxplot(width=0.4) + stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
  theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")+
  xlab(paste("task in",exp.name)) + ylab("detectability MAE, boxplot and mean red dot") + coord_flip()

ggsave(paste(exp.name,"PowerTaskMean.png", sep = ""),plot = last_plot(),scale = 3)
#some algos are better at some category of tasks and missing them could affect statistic metric index
########difference between %MAE & %RMSE by task####
exp.res.noF<-expiramentresults[,1]
exp.res.noF<-as.numeric(levels(exp.res.noF))[exp.res.noF]
exp.res.noF[is.na(exp.res.noF)]<-0

exp.res.noF2<-expiramentresults[,2]
exp.res.noF2<-as.numeric(levels(exp.res.noF2))[exp.res.noF2]
exp.res.noF2[is.na(exp.res.noF2)]<-0
Results.Defactor<-data.frame(exp.res.noF,exp.res.noF2,expiramentresults[,3:length(expiramentresults[1,])])

exp.res.noFZ<-exp.res.noF;exp.res.noFZ2<-exp.res.noF2
for(countr in 1:length(exp.res.noFZ))
{if(exp.res.noFZ[countr]<(-3)) exp.res.noFZ[countr]<-(-3)
  if(exp.res.noFZ2[countr]<(-3)) exp.res.noFZ2[countr]<-(-3) }
res.diff<-round((exp.res.noFZ-exp.res.noFZ2),digits = 3)
rel.res.diff<-(round((exp.res.noFZ-exp.res.noFZ2)/(exp.res.noFZ+exp.res.noFZ2),digits = 3))
rel.res.diff[is.na(rel.res.diff)]<-0

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

z<-ggplot(I.diff.df, aes(y = I.diff.df[,4], x = reorder(I.diff.df[,1], I.diff.df[,8]))) + geom_point()+ coord_flip()
z+ geom_point(colour="red",aes(I.diff.df[,1] ,I.diff.df[,8]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.diff.df[,1] ,I.diff.df[,6]))+
  geom_point(colour="green",aes(I.diff.df[,1] ,(I.diff.df[,7])))+
  ylab(paste("%MAE - %RMSE, Bk L.Quartile, R mean, Bu U.Quart, G max")) + xlab("")
ggsave(paste(exp.name,"DiffTask.png", sep = ""),plot = last_plot(),scale = 3)

#########acceptable minimum ######
acceptablePloss<-.05
lowestFindScore<-.01
acceptAlgo.df<-data.frame()
acceptAlgo.df[1,1]<-NA
acceptAlgo.df[,c(1:400)]<-NA
acceptAlgo.Rdf<-data.frame()
acceptAlgo.Rdf[1,1]<-NA
acceptAlgo.Rdf[,c(1:400)]<-NA
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
  acceptAlgo<-as.logical((exp.res.noF>minAccept)*only.algo)
  acceptAlgo.df[countr,2]<-sum(acceptAlgo)
  acceptAlgo.df[countr,3]<-power.df[taskSeek,5]
  acceptAlgo.df[countr,4:(sum(acceptAlgo)+3)]<-as.character(expiramentresults[acceptAlgo,7])
  acceptAlgo.Rdf[countr,4:(sum(acceptAlgo)+3)]<-as.numeric(as.character(expiramentresults[acceptAlgo,1]))
 }

for(countr in 1:length(acceptAlgo.df[,3]))
{
  iti <- order(as.numeric(acceptAlgo.Rdf[countr,4:(acceptAlgo.df[countr,2]+3)]), decreasing = T)
  acceptAlgo.df[countr,4:(acceptAlgo.df[countr,2]+3)]<-(acceptAlgo.df[countr,4:(acceptAlgo.df[countr,2]+3)])[iti]
}

iti <- order(acceptAlgo.df[,3],acceptAlgo.df[,2])
#acceptAlgo.df<-rbind(acceptAlgo.df)[iti,]
gene.expect[,3]<-1#,gene.expect[length(acceptAlgo.df[,1]),3]
out<-data.frame(acceptAlgo.df[,1:2],acceptAlgo.df[,3:length(acceptAlgo.df[1,])])
write.table(out,
            file = paste(exp.name,acceptablePloss,"MinNecessary.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")



######power to detect#####
if(F){ 
exp.res.noF<-expiramentresults[,1]
exp.res.noF<-as.numeric(levels(exp.res.noF))[exp.res.noF]
exp.res.noF[is.na(exp.res.noF)]<-0
exp.res.noF[exp.res.noF<(-3)]<-(-3)

algPower.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,7]))
{
  countr=countr+1
  only.algo<-as.logical((expiramentresults[,7]==algo)*(expiramentresults[,1]!="Fail"))
  only.algo[is.na(only.algo)]<-F
  if(sum(only.algo, na.rm=T)<1) {
    countr=countr-1    
    next()}
  algPower.df[countr,1]<-algo
  algPower.df[countr,2]<-round(mean(exp.res.noF[only.algo],na.rm = T),digits = 3)

}

algo.max<-vector()
for(countr in 1:length(exp.res.noF)){
  for(task in 1:length(power.df[,1])){
    if(power.df[task,1]==expiramentresults[countr,10]){
      algo.max[countr]<-power.df[task,5]}}}

algo.max[algo.max<.2]<-.2
exp.res.noF<-exp.res.noF/algo.max
#exp.res.noF<-exp.res.noF+1-algo.max

#algPower.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,7]))
{
  countr=countr+1
  only.algo<-as.logical((expiramentresults[,7]==algo)*(expiramentresults[,1]!="Fail"))
  only.algo[is.na(only.algo)]<-F
  if(sum(only.algo, na.rm=T)<1) {
    countr=countr-1    
    next()}
  #algPower.df[countr,1]<-algo
  algPower.df[countr,3]<-min(exp.res.noF[only.algo],na.rm = T)
  algPower.df[countr,4]<-median(exp.res.noF[only.algo],na.rm = T)
  #algPower.df[countr,4]<-round(mean(exp.res.noF[only.algo],na.rm = T),digits = 3)
  algPower.df[countr,5]<-max(exp.res.noF[only.algo],na.rm = T)
  algPower.df[countr,6]<-sum(only.algo, na.rm=T)
  algPower.df[countr,7]<-round(quantile(exp.res.noF[only.algo], probs = c(.7), na.rm =T),digits = 3)
  algPower.df[countr,8]<-round(quantile(exp.res.noF[only.algo], probs = c(.8), na.rm =T),digits = 3)
  algPower.df[countr,9]<-round(quantile(exp.res.noF[only.algo], probs = c(.9), na.rm =T),digits = 3)
}

iti <- order(algPower.df[,4],algPower.df[,8])
algPower.df<-rbind(algPower.df)[iti,]
write.table(algPower.df,
            file = paste(exp.name,"Power.csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#algPower.df<-data.frame(algPower.df, row.names = 1:length(algPower.df[,1]))
I.power.df<-algPower.df[!not.interesting,]

z<-ggplot(I.power.df, aes(y = I.power.df[,2], x = reorder(I.power.df[,1], I.power.df[,2]))) + 
  geom_point()+#+##+coord_flip()
  theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")
z+ geom_point(colour="red",aes(I.power.df[,1] ,I.power.df[,8]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.power.df[,1] ,I.power.df[,5]))+
  xlab("") + ylab("%MAE, ord. by mean, G Succ.Attempts, Bk mean, Bu P R Max 90 80 quant of max for task") +
  geom_point(colour="purple",aes(I.power.df[,1] ,I.power.df[,9]))+
  geom_point(colour="green",aes(I.power.df[,1] ,(I.power.df[,6]/max(I.power.df[,6])-.45)))
ggsave(paste(exp.name,"Power.png", sep = ""),plot = last_plot(),scale = 3)

#some tasks are harder and thus improvement in them should count for more
#that affects the black mean but not red-blue bc 1-0 is % of max score on that task
#some algos are better at some category of tasks and these tasks are overepresented
#that affects the black mean but less red-blue bc as only method's best 20% matter
z<-ggplot(I.power.df, aes(y = I.power.df[,2], x = reorder(I.power.df[,1], I.power.df[,8]))) + 
  geom_point()+#+##+coord_flip()
  theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")
z+ geom_point(colour="red",aes(I.power.df[,1] ,I.power.df[,8]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.power.df[,1] ,I.power.df[,5]))+
  xlab("") + ylab("%MAE, ord. by 80%, G Succ.Attempts, Bk mean, Bu P R Max 90 80 quant of max for task") +
  geom_point(colour="purple",aes(I.power.df[,1] ,I.power.df[,9]))+
  geom_point(colour="green",aes(I.power.df[,1] ,(I.power.df[,6]/max(I.power.df[,6])-.45)))
ggsave(paste(exp.name,"PowerReorder.png", sep = ""),plot = last_plot(),scale = 3)
z<-ggplot(I.power.df, aes(y = I.power.df[,2], x = reorder(I.power.df[,1], I.power.df[,9]))) + 
  geom_point()+#+##+coord_flip()
  theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")
z+ geom_point(colour="red",aes(I.power.df[,1] ,I.power.df[,8]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.power.df[,1] ,I.power.df[,5]))+
  xlab("") + ylab("%MAE, ord. by 90%, G Succ.Attempts, Bk mean, Bu P R Max 90 80 quant of max for task") +
  geom_point(colour="purple",aes(I.power.df[,1] ,I.power.df[,9]))+
  geom_point(colour="green",aes(I.power.df[,1] ,(I.power.df[,6]/max(I.power.df[,6])-.45)))
ggsave(paste(exp.name,"PowerReorder2.png", sep = ""),plot = last_plot(),scale = 3)
}
######power to detect by pipe#####
for (itr1 in c("algonly","pipe")) {
  if(itr1=="pipe") expiramentresults[,7] <- paste(expiramentresults[,7],expiramentresults[,13],sep = "_") 
for (itr2 in c("abs","relmax")) {
    
  out.file.name<-paste0("Power",itr1,itr2)
exp.res.noF<-expiramentresults[,1]
exp.res.noF<-as.numeric(levels(exp.res.noF))[exp.res.noF]
exp.res.noF[is.na(exp.res.noF)]<-0
exp.res.noF[exp.res.noF<(-3)]<-(-3)

algPower.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,7]))
{
  countr=countr+1
  only.algo<-as.logical((expiramentresults[,7]==algo)*(expiramentresults[,1]!="Fail"))
  only.algo[is.na(only.algo)]<-F
  if(sum(only.algo, na.rm=T)<1) {
    countr=countr-1    
    next()}
  algPower.df[countr,1]<-algo
  algPower.df[countr,2]<-round(mean(exp.res.noF[only.algo],na.rm = T),digits = 3)
  
}

algo.max<-vector()
for(countr in 1:length(exp.res.noF)){
  for(task in 1:length(power.df[,1])){
    if(power.df[task,1]==expiramentresults[countr,10]){
      algo.max[countr]<-power.df[task,5]}}}

algo.max[algo.max<.2]<-.2
if(itr2=="relmax")
exp.res.noF<-exp.res.noF/algo.max
#exp.res.noF<-exp.res.noF+1-algo.max

#algPower.df<-data.frame()
countr=0
for(algo in unique(expiramentresults[,7]))
{
  countr=countr+1
  only.algo<-as.logical((expiramentresults[,7]==algo)*(expiramentresults[,1]!="Fail"))
  only.algo[is.na(only.algo)]<-F
  if(sum(only.algo, na.rm=T)<1) {
    countr=countr-1    
    next()}
  #algPower.df[countr,1]<-algo
  algPower.df[countr,3]<-round((min(exp.res.noF[only.algo],na.rm = T)),digits = 3)
  algPower.df[countr,4]<-round((median(exp.res.noF[only.algo],na.rm = T)),digits = 3)
  #algPower.df[countr,4]<-round(mean(exp.res.noF[only.algo],na.rm = T),digits = 3)
  algPower.df[countr,5]<-round((max(exp.res.noF[only.algo],na.rm = T)),digits = 3)
  algPower.df[countr,6]<-sum(only.algo, na.rm=T)
  algPower.df[countr,7]<-round(quantile(exp.res.noF[only.algo], probs = c(.7), na.rm =T),digits = 3)
  algPower.df[countr,8]<-round(quantile(exp.res.noF[only.algo], probs = c(.8), na.rm =T),digits = 3)
  algPower.df[countr,9]<-round(quantile(exp.res.noF[only.algo], probs = c(.9), na.rm =T),digits = 3)
}

iti <- order(algPower.df[,4],algPower.df[,8])
algPower.df<-rbind(algPower.df)[iti,]
colnames(algPower.df)<-c("name","mean","min","median","max","count","70%","80%","90%")
write.table(algPower.df,
            file = paste(exp.name,out.file.name,".csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = T, qmethod = "double")
#algPower.df<-data.frame(algPower.df, row.names = 1:length(algPower.df[,1]))
interesting<-(algPower.df[,4]>=(0))+(algPower.df[,8]>=(.25))+(algPower.df[,9]>=(.5))
if(itr2=="relmax" && itr1=="pipe")
not.interesting<-(algPower.df[,6]<5)+(algPower.df[,4]<=(-.7))+ (algPower.df[,8]<=(.07))+ is.na(algPower.df[,6])+(interesting<1)
if(itr2=="relmax" && itr1=="algonly")
not.interesting<-(algPower.df[,6]<5)+(algPower.df[,4]<=(-2))+ is.na(algPower.df[,6]) 
if(itr2=="abs" && itr1=="algonly")
  not.interesting<-(algPower.df[,6]<5)+(algPower.df[,4]<=(-.6))+ is.na(algPower.df[,6]) 
interesting<-(algPower.df[,4]>=(0))+(algPower.df[,8]>=(.2))+(algPower.df[,9]>=(.25))
if(itr2=="abs" && itr1=="pipe")
  not.interesting<-(algPower.df[,6]<5)+(algPower.df[,4]<=(-.7))+ (algPower.df[,8]<=(.07))+ is.na(algPower.df[,6])+(interesting<1)


I.power.df<-algPower.df[!not.interesting,]

z<-ggplot(I.power.df, aes(y = I.power.df[,2], x = reorder(I.power.df[,1], I.power.df[,2]))) + 
  geom_point()+#+##+coord_flip()
  theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")
z+ geom_point(colour="red",aes(I.power.df[,1] ,I.power.df[,8]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.power.df[,1] ,I.power.df[,5]))+
  xlab("") + ylab("%MAE, ord. by mean, G Succ.Attempts, Bk mean, Bu P R Max 90 80 quant of max for task") +
  geom_point(colour="purple",aes(I.power.df[,1] ,I.power.df[,9]))+
  geom_point(colour="green",aes(I.power.df[,1] ,(I.power.df[,6]/max(I.power.df[,6])-.45)))
ggsave(paste(exp.name,out.file.name,".png", sep = ""),plot = last_plot(),scale = 3)

#some tasks are harder and thus improvement in them should count for more
#that affects the black mean but not red-blue bc 1-0 is % of max score on that task
#some algos are better at some category of tasks and these tasks are overepresented
#that affects the black mean but less red-blue bc as only method's best 20% matter
z<-ggplot(I.power.df, aes(y = I.power.df[,2], x = reorder(I.power.df[,1], I.power.df[,8]))) + 
  geom_point()+#+##+coord_flip()
  theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")
z+ geom_point(colour="red",aes(I.power.df[,1] ,I.power.df[,8]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.power.df[,1] ,I.power.df[,5]))+
  xlab("") + ylab("%MAE, ord. by 80%, G Succ.Attempts, Bk mean, Bu P R Max 90 80 quant of max for task") +
  geom_point(colour="purple",aes(I.power.df[,1] ,I.power.df[,9]))+
  geom_point(colour="green",aes(I.power.df[,1] ,(I.power.df[,6]/max(I.power.df[,6])-.45)))
ggsave(paste(exp.name,out.file.name,"Reorder.png", sep = ""),plot = last_plot(),scale = 3)
z<-ggplot(I.power.df, aes(y = I.power.df[,2], x = reorder(I.power.df[,1], I.power.df[,9]))) + 
  geom_point()+#+##+coord_flip()
  theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")
z+ geom_point(colour="red",aes(I.power.df[,1] ,I.power.df[,8]),na.rm = TRUE)+
  geom_point(colour="blue",aes(I.power.df[,1] ,I.power.df[,5]))+
  xlab("") + ylab("%MAE, ord. by 90%, G Succ.Attempts, Bk mean, Bu P R Max 90 80 quant of max for task") +
  geom_point(colour="purple",aes(I.power.df[,1] ,I.power.df[,9]))+
  geom_point(colour="green",aes(I.power.df[,1] ,(I.power.df[,6]/max(I.power.df[,6])-.45)))
ggsave(paste(exp.name,out.file.name,"Reorder2.png", sep = ""),plot = last_plot(),scale = 3)
    }
  }
################end#######
setwd(file.path(mainDir))
