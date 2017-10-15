#options : spearman or person? mean or 
#percent of all intersections above a set amount
#diagnol is rangeing data vs asis
S.pear<-"pearson"#"kendall","spearman"
use.mean<-F
min.corr<-.93#only if use.mean is false

#read file
VarImpResults<-data.frame()
VarImpResults<-(read.csv("importance2.csv", sep = ",",fill=TRUE, header = F,quote="",dec=".",stringsAsFactors=F))
 
u.learns<-unique(VarImpResults[,1])
u.gens<-unique(VarImpResults[,4])
#make 140x140x40 DF/array
pearson.array<-array(NA, c(length(u.learns), length(u.learns),
                      length(u.gens)))


#check redundancy 
#for every generator and model
count.gen<-0
c.gen<-u.gens[9]
for(c.gen in (u.gens)){
  count.gen<-1+count.gen
  #new df of relevant rows
  selVIRL<-as.logical(as.character(VarImpResults[,4])==c.gen)
  selVIRL<-as.logical(selVIRL)
  selVIR<-VarImpResults[selVIRL,5:13]

  
  #######array fill########
  
  #for every unique model
  count.learn<-0
  for(ev.learn in u.learns){
    count.learn<-1+count.learn
    selselVIR<-selVIR[as.logical(VarImpResults[selVIRL,1]==ev.learn),]
    #for every other model
    countt.learn<-0
    for(ev2.learn in u.learns){
      sssVIR<-selVIR[as.logical(VarImpResults[selVIRL,1]==ev2.learn),]
      countt.learn<-1+countt.learn
      #Test for fails
      if(length(sssVIR[,])<1 || length(sssVIR[,1])<1) {next()}
      if(length(selselVIR[,])<1 || length(selselVIR[,1])<1) {next()}
      Lsss<-length(sssVIR[,1]);Lselsel<-length(selselVIR[,1])
      #do not assume.be always 1vs1
      sum.of.cors<-0
      last.count<-0
      if(ev.learn==ev2.learn){
      for(n in 1:Lselsel){
        for(g in 1:Lsss){
          if(n==g){next()}
          if(is.na(sssVIR[g,])||is.na(selselVIR[n,])){next()}
          last.count<-last.count+1
          sum.of.cors<-cor(as.numeric(sssVIR[g,]),as.numeric(selselVIR[n,]),method =S.pear)+sum.of.cors
        }}
      pearson.array[count.learn,countt.learn,count.gen]<-sum.of.cors/last.count
      }else{
        if(Lselsel==Lsss){
        for(n in 1:Lselsel){
          if(is.na(sssVIR[n,])||is.na(selselVIR[n,])){next()}
          last.count<-last.count+1
          sum.of.cors<-cor(as.numeric(sssVIR[n,]),as.numeric(selselVIR[n,]),method =S.pear)+sum.of.cors
        }
          pearson.array[count.learn,countt.learn,count.gen]<-sum.of.cors/last.count
          
          }else{
          every.intersection<-vector(mode = "numeric", length = Lselsel * Lsss)
          for(n in 1:Lselsel){
            for(g in 1:Lsss){            
              every.intersection[g+Lsss*(n-1)]<-cor(as.numeric(sssVIR[g,]),as.numeric(selselVIR[n,]),method =S.pear)
              }
            }  
          rank.inters<-rank(every.intersection)
          min.accept<-Lselsel * Lsss-min(Lselsel,Lsss)
          pearson.array[count.learn,countt.learn,count.gen]<-mean(every.intersection[rank.inters>min.accept],na.rm = T)
          
        }
        
        
      }
    }}}
  #write  pearson to pearson array

#make 140x140DF p.a.1
out.array<-array(0, c(length(u.learns), length(u.learns)))
out.array.mean<-array(0, c(length(u.learns), length(u.learns)))
not.na.array<-array(0, c(length(u.learns), length(u.learns)))

#how often is each intersection true?
#for every model model
for(n in 1:length(u.learns)){ 
  for(c in 1:length(u.learns)){
    out.array.mean[n,c]=mean(pearson.array[n,c,], na.rm = T) 
    #for every generator

    for(g in 1:length(u.gens)){
    #sum Ts into 140xxdf
      if(!is.na(pearson.array[n,c,g])){
        not.na.array[n,c]=not.na.array[n,c]+1
        if(pearson.array[n,c,g]>=min.corr)
          {out.array[n,c]=out.array[n,c]+1}
        }
        
    }}}
colnames(out.array) <- u.learns
rownames(out.array) <- u.learns
colnames(out.array.mean) <- u.learns
rownames(out.array.mean) <- u.learns
#outr.array<-out.array/max(out.array)
#corrplot(out.array, method = "circle")
#stickyball selfsticing map
#clustering in other words
#############corrmat##########

if(T){
  col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white",
                             "cyan", "#007FFF", "blue","#00007F"))
  col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                             "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
  col3 <- colorRampPalette(c("red", "white", "blue"))
  col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F",
                             "cyan", "#007FFF", "blue","#00007F"))
  if(use.mean){
  M<-out.array.mean}else{
    M<-out.array/not.na.array}
  
  library('corrplot') #package corrplot
  #corrplot(M, method = "circle") #plot matrix
  
  M[is.na(M)]<-0
  (order.AOE <- corrMatOrder(M,  order = "AOE"))
  (order.FPC <- corrMatOrder(M, order = "FPC"))
  (order.hc <- corrMatOrder(M, order = "hclust"))
  (order.hc2 <- corrMatOrder(M, order = "hclust", hclust.method = "ward.D2"))
  
  M.AOE <- M[order.AOE,order.AOE]
  M.FPC <- M[order.FPC,order.FPC]
  M.hc  <- M[order.hc, order.hc]
  M.hc2 <- M[order.hc2,order.hc2]
  
  par(ask = F)
  #corrplot(M)
  #corrplot(M.AOE, is.corr=FALSE,addrect = 2, method="ellipse", col=col1(200), addCoef.col = "black")
  
  corrplot(M.FPC, is.corr=FALSE,addrect = 2, method="ellipse", col=col1(200), addCoef.col = "black")
  
  #corrplot(M.hc, is.corr=FALSE,addrect = 2, method="ellipse", col=col1(200), addCoef.col = "black")
  
  #corrplot(M.hc2, is.corr=FALSE,addrect = 2, method="ellipse", col=col1(200), addCoef.col = "black")
  }