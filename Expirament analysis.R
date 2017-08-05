#ggplot: how to sort, label, print to file
expiramentresults<-read.csv("Generated data output first complete run.csv", sep = ",",fill=TRUE, header = F,quote="",dec=".")
exp.name<-"First 332v100dp5n3n16"
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
not.interesting<-(fails.df[,5]==1) + (fails.df[,5]<.09)
I.fails.df<-fails.df[!not.interesting,]
library(ggplot2)
p<-ggplot(I.fails.df)
p+   geom_point(aes(I.fails.df[,5] ,I.fails.df[,1]))

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
not.interesting<-(time.df[,3]<50) 
I.time.df<-time.df[!not.interesting,]

p<-ggplot(I.time.df)
p+   geom_point(aes(I.time.df[,3] ,I.time.df[,1]))+   
  geom_point(aes(I.time.df[,2] ,I.time.df[,1]))

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
  if(sum(only.algo, na.rm=T)<1) next()
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

not.interesting<-(power.df[,6]<10)+(power.df[,4]<.2)  
I.power.df<-power.df[!not.interesting,]
p<-ggplot(I.power.df)
p+   geom_point(aes(I.power.df[,3] ,I.power.df[,1]))+
  geom_point(colour="green",aes(I.power.df[,7] ,I.power.df[,1]))+
  geom_point(colour="blue",aes(I.power.df[,4] ,I.power.df[,1]))
#####time and success by algorithm####


