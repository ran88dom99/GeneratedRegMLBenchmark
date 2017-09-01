#load data
exp.name<-"7th range01 vs asis"
mainDir<-getwd()
subDir<-exp.name
expiramentresults<-data.frame()
expiramentresults<-(read.csv("7th range01 vs asis.csv", sep = ",",fill=TRUE, header = F,quote="",dec=".",stringsAsFactors=F))
dir.create(file.path(mainDir, subDir))
setwd(file.path(mainDir, subDir))

#why do negative numbers ever matter? and de-factoring
exp.res.noF<-expiramentresults[,1]
exp.res.noF<-as.numeric((exp.res.noF))
exp.res.noF[exp.res.noF<0]<-0
exp.res.noF[is.na(exp.res.noF)]<--.01
exp.res.noF->expiramentresults[,1]



#list of good comparisons of tasks
runsTcompare<-data.frame(c("asis"))
#runsTcompare[,1]<-
runsTcompare[,2]<-c("range01")
runsTcompare[,3]<-c("asis vs range01")

u.learns<-unique(expiramentresults[,8])
u.gens<-unique(expiramentresults[,11])
runcomp<-array(2, c(length(runsTcompare[,1]), length(u.learns), length(u.gens)))
failruncomp<-array(0, c(length(runsTcompare[,1]), length(u.learns), length(u.gens)))
maxruncomp<-array(1.1, c(length(runsTcompare[,1]), length(u.learns), length(u.gens)))
comboruncomp<-array(2, c(length(runsTcompare[,1]), length(u.learns), length(u.gens)))

task.max<-vector()
for(task in 1:length(u.gens))
  {
  task.max[task]<-0
  task.max[task]<-max(exp.res.noF[u.gens[task]==expiramentresults[,11]],na.rm = T)
  }  
task.max[task.max<.3]<-.3



count.comp<-0
count.learn<-0
count.gen<-0

#for every run
for(comp.run in 1:length(runsTcompare[,1])){
  count.comp<-1+count.comp
  #new df of relevant rows
  selExpResL<-as.logical(as.character((expiramentresults[,2]))==as.character(runsTcompare[comp.run,1]))+
    (as.character(expiramentresults[,2])==as.character(runsTcompare[comp.run,2]))
  selExpResL<-as.logical(selExpResL)
  selExpRes<-expiramentresults[selExpResL,]

  #for every unique model
  count.learn<-0
  for(ev.learn in u.learns){
    count.learn<-1+count.learn
    selselExpRes<-selExpRes[as.logical(selExpRes[,8]==ev.learn),]
    #for every generator
    count.gen<-0
    for(ev.gen in u.gens){
      sssExpRes<-selselExpRes[as.logical(selselExpRes[,11]==ev.gen),]
      count.gen<-1+count.gen
      #Test for fails
      if(length(sssExpRes[,1])<2) {next()}
      #do not assume.be specific
      maxone<-max(sssExpRes[sssExpRes[,2]==runsTcompare[count.comp,1],1])
      maxtwo<-max(sssExpRes[sssExpRes[,2]==runsTcompare[count.comp,2],1])
      if(maxone==-.01 && maxtwo==-.01) {next()}
      if(maxone==-.01 || maxtwo==-.01){
        #fill in data points in success and fail dataframes
        failruncomp[count.comp,count.learn,count.gen]<-maxone-maxtwo
      }else{
        runcomp[count.comp,count.learn,count.gen]<-maxone-maxtwo  
      }
      comboruncomp[count.comp,count.learn,count.gen]<-maxone-maxtwo
      maxruncomp[count.comp,count.learn,count.gen]<-task.max[count.gen]
    }}}
head(runcomp)
head(failruncomp)
#######basic compaison######### 
# for every ModelA make violin graphs per each Run
library(ggplot2)    
library(reshape2) 
#plot(runcomp[1,1,runcomp[1,1,]<1.5])
m=1
for(m in c(2,3,7)){
ttdd<-data.frame(t(runcomp[,m,]))
ttdd[ttdd==2]<-NA
maxttdd<-data.frame(t(maxruncomp[,m,]))
colnames(ttdd)<-runsTcompare[,3]
#p <- ggplot(melt(ttdd), aes(x=melt(maxttdd)[,2],y=melt(ttdd)[,2],colour = melt(ttdd)[,1]))
#p+geom_point()
exp.name<-u.learns[m]
p <- ggplot(melt(ttdd), aes(x=melt(ttdd)[,1],y=melt(ttdd)[,2]))
p + geom_violin() + geom_boxplot(width=0.05)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
  xlab(paste("runs of",exp.name)) + ylab("diff") 
ggsave(paste(exp.name," and violins per Run.png", sep = ""),plot = last_plot(),scale = 3)
}

#for every Run boxplot of every ModelAl
for(r in 1:length(runsTcompare[,1])){
ttdd<-data.frame((runcomp[r,,]))
ttdd[ttdd==2]<-NA
maxttdd<-data.frame((maxruncomp[r,,]))
rownames(ttdd)<-u.learns
#p <- ggplot(melt(ttdd), aes(x=melt(maxttdd)[,2],y=melt(ttdd)[,2],colour = melt(ttdd)[,1]))
#p+geom_point(), x = reorder(, I.power.df[,8])
means<-vector()
for(m in 1:length(u.learns)){
  means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
ordos<- order(means,ttdd[,1])
ttdd<-rbind(ttdd)[ordos,]
for(m in 1:length(u.learns)){
  means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
ttdd<-ttdd[!is.nan(means),]
ttdd<-t(ttdd)

exp.name<-runsTcompare[r,3]
p <- ggplot(melt(ttdd), aes(x=(melt(ttdd)[,2]),y=melt((ttdd))[,3]))
p  + geom_boxplot(width=0.4)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
 theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")+
   xlab(paste("learners in",exp.name)) + ylab("diff") 
ggsave(paste(exp.name," and boxes per learnr.png", sep = ""),plot = last_plot(),scale = 3)
}

for(r in 1:length(runsTcompare[,1])){ 
  ttdd<-data.frame((runcomp[r,,]))
  ttdd[ttdd==2]<-NA
  maxttdd<-data.frame((maxruncomp[r,,]))
  colnames(ttdd)<-u.gens
  ttdd<-t(ttdd)

  means<-vector()
  for(m in 1:length(u.gens)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ordos<- order(means,ttdd[,1])
  ttdd<-rbind(ttdd)[ordos,]
  for(m in 1:length(u.gens)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ttdd<-ttdd[,!is.nan(means)]
  ttdd<-t(ttdd)
  
  exp.name<-runsTcompare[r,3]
  p <- ggplot(melt(ttdd), aes(x=(melt(ttdd)[,2]),y=melt((ttdd))[,3]))
  p  + geom_boxplot(width=0.4)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    theme(axis.text.x=element_text(size=7, angle=270,hjust=1,vjust=0.3))+scale_x_discrete(position = "top")+
    xlab(paste("gens in",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and boxes per generator.png", sep = ""),plot = last_plot(),scale = 3)
}

##########Now repeat with percent adjusted#########
# for every ModelA make violin graphs per each Run
library(ggplot2)    
library(reshape2) 
#plot(runcomp[1,1,runcomp[1,1,]<1.5])
m=1
for(m in c(2,3,7)){
  ttdd<-data.frame(t(runcomp[,m,]))
  ttdd[ttdd==2]<-NA
  maxttdd<-data.frame(t(maxruncomp[,m,]))
  maxttdd[maxttdd>1]<-NA
  maxttdd[maxttdd<.3]<-.3
  ttdd<-ttdd/maxttdd
  colnames(ttdd)<-runsTcompare[,3]
  #p <- ggplot(melt(ttdd), aes(x=melt(maxttdd)[,2],y=melt(ttdd)[,2],colour = melt(ttdd)[,1]))
  #p+geom_point()
  exp.name<-u.learns[m]
  p <- ggplot(melt(ttdd), aes(x=melt(ttdd)[,1],y=melt(ttdd)[,2]))
  p + geom_violin() + geom_boxplot(width=0.05)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    xlab(paste("percent runs of",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and violins per Run percent.png", sep = ""),plot = last_plot(),scale = 3)
}

#for every Run boxplot of every ModelAl
for(r in 1:length(runsTcompare[,1])){
  ttdd<-data.frame((runcomp[r,,]))
  ttdd[ttdd==2]<-NA
  maxttdd<-data.frame((maxruncomp[r,,]))
  maxttdd[maxttdd>1]<-NA
  maxttdd[maxttdd<.3]<-.3
  ttdd<-ttdd/maxttdd
  rownames(ttdd)<-u.learns
  #p <- ggplot(melt(ttdd), aes(x=melt(maxttdd)[,2],y=melt(ttdd)[,2],colour = melt(ttdd)[,1]))
  #p+geom_point(), x = reorder(, I.power.df[,8])
  means<-vector()
  for(m in 1:length(u.learns)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ordos<- order(means,ttdd[,1])
  ttdd<-rbind(ttdd)[ordos,]
  for(m in 1:length(u.learns)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ttdd<-ttdd[!is.nan(means),]
  ttdd<-t(ttdd)
  
  exp.name<-runsTcompare[r,3]
  p <- ggplot(melt(ttdd), aes(x=(melt(ttdd)[,2]),y=melt((ttdd))[,3]))
  p  + geom_boxplot(width=0.4)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")+
    xlab(paste("percent learners in",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and boxes per learnr percent.png", sep = ""),plot = last_plot(),scale = 3)
}

for(r in 1:length(runsTcompare[,1])){ 
  ttdd<-data.frame((runcomp[r,,]))
  ttdd[ttdd==2]<-NA
  maxttdd<-data.frame((maxruncomp[r,,]))
  maxttdd[maxttdd>1]<-NA
  maxttdd[maxttdd<.3]<-.3
  ttdd<-ttdd/maxttdd
  colnames(ttdd)<-u.gens
  ttdd<-t(ttdd)
  
  means<-vector()
  for(m in 1:length(u.gens)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ordos<- order(means,ttdd[,1])
  ttdd<-rbind(ttdd)[ordos,]
  for(m in 1:length(u.gens)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ttdd<-ttdd[,!is.nan(means)]
  ttdd<-t(ttdd)
  
  exp.name<-runsTcompare[r,3]
  p <- ggplot(melt(ttdd), aes(x=(melt(ttdd)[,2]),y=melt((ttdd))[,3]))
  p  + geom_boxplot(width=0.4)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    theme(axis.text.x=element_text(size=7, angle=270,hjust=1,vjust=0.3))+scale_x_discrete(position = "top")+
    xlab(paste("percent gens in",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and boxes per generator percent.png", sep = ""),plot = last_plot(),scale = 3)
}

##########Now missed seperately#########
# for every ModelA make violin graphs per each Run
library(ggplot2)    
library(reshape2) 
#plot(runcomp[1,1,runcomp[1,1,]<1.5])
m=1
for(m in c(2,3,7)){
  ttdd<-data.frame(t(failruncomp[,m,]))
  ttdd[ttdd==2]<-NA#does nothing! 
  maxttdd<-data.frame(t(maxruncomp[,m,]))
  maxttdd[maxttdd>1]<-NA
  maxttdd[maxttdd<.3]<-.3
  ttdd<-ttdd/maxttdd
  colnames(ttdd)<-runsTcompare[,3]
  #p <- ggplot(melt(ttdd), aes(x=melt(maxttdd)[,2],y=melt(ttdd)[,2],colour = melt(ttdd)[,1]))
  #p+geom_point()
  exp.name<-u.learns[m]
  p <- ggplot(melt(ttdd), aes(x=melt(ttdd)[,1],y=melt(ttdd)[,2]))
  p + geom_violin() + geom_boxplot(width=0.05)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    xlab(paste("fail runs of",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and violins per Run fail.png", sep = ""),plot = last_plot(),scale = 3)
}

#for every Run boxplot of every ModelAl
for(r in 1:length(runsTcompare[,1])){
  ttdd<-data.frame((failruncomp[r,,]))
  ttdd[ttdd==2]<-NA
  maxttdd<-data.frame((maxruncomp[r,,]))
  maxttdd[maxttdd>1]<-NA
  maxttdd[maxttdd<.3]<-.3
  ttdd<-ttdd/maxttdd
  rownames(ttdd)<-u.learns
  #p <- ggplot(melt(ttdd), aes(x=melt(maxttdd)[,2],y=melt(ttdd)[,2],colour = melt(ttdd)[,1]))
  #p+geom_point(), x = reorder(, I.power.df[,8])
  means<-vector()
  for(m in 1:length(u.learns)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ordos<- order(means,ttdd[,1])
  ttdd<-rbind(ttdd)[ordos,]
  for(m in 1:length(u.learns)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ttdd<-ttdd[!is.nan(means),]
  ttdd<-t(ttdd)
  
  exp.name<-runsTcompare[r,3]
  p <- ggplot(melt(ttdd), aes(x=(melt(ttdd)[,2]),y=melt((ttdd))[,3]))
  p  + geom_boxplot(width=0.4)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")+
    xlab(paste("fail learners in",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and boxes per learnr fail.png", sep = ""),plot = last_plot(),scale = 3)
}

for(r in 1:length(runsTcompare[,1])){ 
  ttdd<-data.frame((failruncomp[r,,]))
  ttdd[ttdd==2]<-NA
  maxttdd<-data.frame((maxruncomp[r,,]))
  maxttdd[maxttdd>1]<-NA
  maxttdd[maxttdd<.3]<-.3
  ttdd<-ttdd/maxttdd
  colnames(ttdd)<-u.gens
  ttdd<-t(ttdd)
  
  means<-vector()
  for(m in 1:length(u.gens)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ordos<- order(means,ttdd[,1])
  ttdd<-rbind(ttdd)[ordos,]
  for(m in 1:length(u.gens)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ttdd<-ttdd[,!is.nan(means)]
  ttdd<-t(ttdd)
  
  exp.name<-runsTcompare[r,3]
  p <- ggplot(melt(ttdd), aes(x=(melt(ttdd)[,2]),y=melt((ttdd))[,3]))
  p  + geom_boxplot(width=0.4)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    theme(axis.text.x=element_text(size=7, angle=270,hjust=1,vjust=0.3))+scale_x_discrete(position = "top")+
    xlab(paste("fail gens in",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and boxes per generator fail.png", sep = ""),plot = last_plot(),scale = 3)
}

##########Now missed with percent#########
# for every ModelA make violin graphs per each Run
#plot(runcomp[1,1,runcomp[1,1,]<1.5])

m=1
for(m in c(2,3,7)){
  ttdd<-data.frame(t(comboruncomp[,m,]))
  ttdd[ttdd==2]<-NA
  maxttdd<-data.frame(t(maxruncomp[,m,]))
  maxttdd[maxttdd>1]<-NA
  maxttdd[maxttdd<.3]<-.3
  ttdd<-ttdd/maxttdd
  colnames(ttdd)<-runsTcompare[,3]
  #p <- ggplot(melt(ttdd), aes(x=melt(maxttdd)[,2],y=melt(ttdd)[,2],colour = melt(ttdd)[,1]))
  #p+geom_point()
  exp.name<-u.learns[m]
  p <- ggplot(melt(ttdd), aes(x=melt(ttdd)[,1],y=melt(ttdd)[,2]))
  p + geom_violin() + geom_boxplot(width=0.05)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    xlab(paste("both runs of",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and violins per Run combo.png", sep = ""),plot = last_plot(),scale = 3)
}

#for every Run boxplot of every ModelAl
for(r in 1:length(runsTcompare[,1])){
  ttdd<-data.frame((comboruncomp[r,,]))
  ttdd[ttdd==2]<-NA
  maxttdd<-data.frame((maxruncomp[r,,]))
  maxttdd[maxttdd>1]<-NA
  maxttdd[maxttdd<.3]<-.3
  ttdd<-ttdd/maxttdd
  rownames(ttdd)<-u.learns
  #p <- ggplot(melt(ttdd), aes(x=melt(maxttdd)[,2],y=melt(ttdd)[,2],colour = melt(ttdd)[,1]))
  #p+geom_point(), x = reorder(, I.power.df[,8])
  means<-vector()
  for(m in 1:length(u.learns)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ordos<- order(means,ttdd[,1])
  ttdd<-rbind(ttdd)[ordos,]
  for(m in 1:length(u.learns)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ttdd<-ttdd[!is.nan(means),]
  ttdd<-t(ttdd)
  
  exp.name<-runsTcompare[r,3]
  p <- ggplot(melt(ttdd), aes(x=(melt(ttdd)[,2]),y=melt((ttdd))[,3]))
  p  + geom_boxplot(width=0.4)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2))+scale_x_discrete(position = "top")+
    xlab(paste("fail n succ learners in",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and boxes per learnr combo.png", sep = ""),plot = last_plot(),scale = 3)
}

for(r in 1:length(runsTcompare[,1])){ 
  ttdd<-data.frame((comboruncomp[r,,]))
  ttdd[ttdd==2]<-NA
  maxttdd<-data.frame((maxruncomp[r,,]))
  maxttdd[maxttdd>1]<-NA
  maxttdd[maxttdd<.3]<-.3
  ttdd<-ttdd/maxttdd
  colnames(ttdd)<-u.gens
  ttdd<-t(ttdd)
  
  means<-vector()
  for(m in 1:length(u.gens)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ordos<- order(means,ttdd[,1])
  ttdd<-rbind(ttdd)[ordos,]
  for(m in 1:length(u.gens)){
    means[m]<-mean(as.numeric(ttdd[m,]),na.rm = T)}
  ttdd<-ttdd[,!is.nan(means)]
  ttdd<-t(ttdd)
  
  exp.name<-runsTcompare[r,3]
  p <- ggplot(melt(ttdd), aes(x=(melt(ttdd)[,2]),y=melt((ttdd))[,3]))
  p  + geom_boxplot(width=0.4)+ stat_summary(fun.y = "mean", colour = "red", size = 1, geom = "point")+
    theme(axis.text.x=element_text(size=7, angle=270,hjust=1,vjust=0.3))+scale_x_discrete(position = "top")+
    xlab(paste("fail n succ gens in",exp.name)) + ylab("diff") 
  ggsave(paste(exp.name," and boxes per generator combo.png", sep = ""),plot = last_plot(),scale = 3)
}
