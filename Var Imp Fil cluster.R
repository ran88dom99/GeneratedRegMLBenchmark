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

  #############corrmat##########
  if(F){
  M <- cor(t(selVIR)) # get correlations do i need to t()
  colnames(M) <- VarImpResults[selVIRL,1]
  rownames(M) <- VarImpResults[selVIRL,1]
  library('corrplot') #package corrplot
  corrplot(M, method = "circle") #plot matrix
  
  M[is.na(M)]<-0
  (order.AOE <- corrMatOrder(M, order = "AOE"))
  (order.FPC <- corrMatOrder(M, order = "FPC"))
  (order.hc <- corrMatOrder(M, order = "hclust"))
  (order.hc2 <- corrMatOrder(M, order = "hclust", hclust.method = "ward.D2"))
  
  M.AOE <- M[order.AOE,order.AOE]
  M.FPC <- M[order.FPC,order.FPC]
  M.hc  <- M[order.hc, order.hc]
  M.hc2 <- M[order.hc2,order.hc2]
  
  par(ask = F)
  corrplot(M)
  corrplot(M.AOE)
  
  corrplot(M.FPC)
  
  corrplot(M.hc)
  
  corrplot(M.hc2)
  
  pearson.array[,,count.gen]<-M}
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
      remov<-vector(length = length(selselVIR[,1]))
      remov[]<-T
      n<-1
      for(n in 1:(length(selselVIR[,1])))
      {
        if(is.na(selselVIR[n,])){remov[n]<-F}
      }
      selselVIR<-selselVIR[remov,]
      remov<-vector(length = length(sssVIR[,1]))
      remov[]<-T
      n<-1
      for(n in 1:(length(sssVIR[,1])))
      {
        if(is.na(sssVIR[n,])){remov[n]<-F}
      }
      sssVIR<-sssVIR[remov,]

      if(length(sssVIR[,])<1 || length(sssVIR[,1])<1) {next()}
      if(length(selselVIR[,])<1 || length(selselVIR[,1])<1) {next()}
      #do not assume.be always 1vs1
      sum.of.cors<-0
      last.count<-0
      for(n in 1:length(selselVIR[,1])){
        for(g in 1:length(sssVIR[,1])){
          last.count<-last.count+1
          sum.of.cors<-cor(as.numeric(sssVIR[g,]),as.numeric(selselVIR[n,]))+sum.of.cors
        }}
      pearson.array[count.learn,countt.learn,count.gen]<-sum.of.cors/last.count
    }}}
  #write  pearson to pearson array

#make 140x140DF p.a.1
out.array<-array(0, c(length(u.learns), length(u.learns)))
#how often is each intersection true?
min.corr<-.96
#for every model model
for(n in 1:length(u.learns)){ 
  for(c in 1:length(u.learns)){ 
    #for every generator
    for(g in 1:length(u.gens)){
    #sum Ts into 140xxdf
      if(!is.na(pearson.array[n,c,g])){
      if(pearson.array[n,c,g]>=min.corr)
        {out.array[n,c]=out.array[n,c]+1}}
    }}}
colnames(out.array) <- u.learns
rownames(out.array) <- u.learns
outr.array<-out.array/max(out.array)
corrplot(outr.array, method = "circle")
#stickyball selfsticing map
#clustering in other words