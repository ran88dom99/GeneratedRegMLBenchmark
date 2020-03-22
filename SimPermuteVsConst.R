##### make a single function to try everything#####
slt<-function(x,r,v){
  t<-vector(mode = "numeric",length = length(x))
  for(i in 1:length(x)){
    if(x[i]<=0) {
      t[i]<-r[i]
    }
    if(x[i]>0) {
      t[i]<-v[i]
    }
  }
  return(t)
} #tree branch split pick between r and v depending on x
makemean<-function(x){
  t<-x
  for(i in 1:length(x)){
    t[i]<-mean(x)
  }
  return(t)
} # just makes a vector of means
xnd<-function(q,r,v){
  return(slt(q,r,v))#*r
}#EDIT interaction for x Note "ideal" (yi) must still be edited by hand
metr<-function(g,h){ 
  cor(g,h)
} #EDIT metric measuring agreement and error between two variables
#edit ideal at line 155 1 for correlation and 0 for rmse
#do not forget to change save file name 
#asume competnt feature selection and remove unused featurs from dataframe
##set all file name including htis one

require(caret)
allmodels <- unique(modelLookup()[modelLookup()$forReg,c(1)])
bad.models=c("randomGLM","DENFIS","neuralnet","partDSA","blackboost","bstSm","bstTree","penalized","brnn",
             "gamLoess","ANFIS","FIR.DM","FS.HGD","nodeHarvest","mlpWeightDecayML","monmlp","mlp","mlpWeightDecay",
             "mlpSGD","rbf","rbfDDA","rfRules","GFS.FR.MOGUL","mlpML","HYFIS","GFS.THRIFT" ,"GFS.LT.RS",
             "svmSpectrumString","svmExpoString","svmBoundrangeString",
             "bagEarthGCV","bam","mxnet","mlpKerasDecay","mlpKerasDropout",
             "qrnn","mxnet")

allmodels <- setdiff(allmodels,bad.models)
if(exists("recoutr2")){
allmodels <- setdiff(allmodels,rownames(recoutr2))
} else {
recoutr <- data.frame()
recoutr2 <- data.frame()
justmeans <- data.frame(nams=c("ideal_corrs_w_y", "man_X_meaned",
                               "man_perm_mean_metric",
                               "man_perm_metric_mean", "man_ideal","modl_w_x",
                               "modl_wo_x", "modl_mean_x",
                               "y_perm","modl_x_perm",
                               "modl_wo_x_and_modl_mean_x",
                               "modl_wo_x_and_modl_perm_x"))
finished <- vector()
}

iter <- 40
leng <- 1000
ix <- 1:(leng/2+1) #testing partition
ux <- (max(ix):leng) #training partition


for(allmodel in allmodels){
  mdle<-function(daata){
    return(
      train(x = data.frame(daata[,2:length(daata[1,])]),
            y = daata[,1],
            method = allmodel,
            tuneLength = 1)
      )
    #(y~.,daata),
  }
  print(paste(allmodel,date()))
  record <- data.frame()
try({
for(i in 1:iter){
  E<-rnorm(leng)*2 #error
  x<-rnorm(leng) #varieble to remove
  z<-rnorm(leng) #not iteractive, unconditional other detected variable
  r<-rnorm(leng) #usualy unused but maybe interactive used in xnd()
  v<-rnorm(leng) #usualy unused but maybe interactive used in xnd() 
  q<-rnorm(leng) #usualy unused but maybe interactive used in xnd() 
  thon<-seq.int(by=0,length.out = length(v))
  y=xnd(x,r,slt(v,q,thon))+E+z #actual model with error
  
  
  if(F){
    print(paste(
      metr(y,E),
      metr(y,x),
      metr(y,z),
      metr(y,r),
      metr(y,v),
      metr(y,xnd(x,r,slt(v,q,thon)))))
    print(paste(
      metr(y,E)^2,
      metr(y,x)^2, 
      metr(y,z)^2,
      metr(y,r)^2,
      metr(y,v)^2,
      metr(y,xnd(x,r,slt(v,q,thon)))^2))
  }
  
  #each variable signal should not be clean except E
  x<-x+rnorm(leng)/4 
  z<-z+rnorm(leng)/4 
  r<-r+rnorm(leng)/4 
  v<-v+rnorm(leng)/4 
  q<-q+rnorm(leng)/4 
  
  d<-data.frame(y,E,x,z,r)#,v,q)
  
  xh<-makemean(x)

  if(F){
    ymx=xnd(xh,r,slt(v,q,thon))+E+z #model if x meaned with error
  print(paste(
    metr(y,ymx)^2,
    metr(y,xnd(xh,r,slt(v,q,thon)))^2,
    metr(y,E)^2))
  }
  
  #yr<-r+z
  yo=xnd(xh,r,slt(v,q,thon))+z  #model if x meaned 
  #metr(yo,yr)
  
  #why is this diffferent than mean!!???
  tb=data.frame(y) #build up for permutation mean then metric
  tb<-tb[,-1]
  for(i in 1:30){
    xp<-sample(x,size = length(x))
    yp=xnd(xp,r,slt(v,q,thon))+z
    tb<-cbind(tb,yp)
  }
  yp<-apply(tb,1,mean) #yp (permute) final
  
  tb=vector() #build up for permutation metric then mean 
  for(i in 1:30){
    xp<-sample(x,size = length(x))
    yp=xnd(xp,r,slt(v,q,thon))+z
    tb<-cbind(tb,metr(yp,y))
  }
  ypl<-mean(tb) #yp (permute) final
  
  f<-apply(data.frame(r,v),1,mean)
  yi=z#+1*r #what the result should have been?
  #f#
  
  dt <- d[ix,]
  d <- d[ux,]
  
  lmd<-mdle(d[,c(-2,-3)]) #daata<-d[,c(-2,-3)]
  ylx=predict(lmd,newdata=dt[,c(-1,-2,-3)]) #based on linear model withOUT x
  lmd<-mdle(d[,-2])#
  yl=predict(lmd,newdata=dt[,c(-1,-2)]) #based on linear model with x
  f<-dt
  f$x<-xh[ix]
  ylm=predict(lmd,newdata=f[,c(-1,-2)]) #based on linear model with x as mean
  
  
  #based on linear model with x as permutation
  if(F){
  tb=data.frame(y[ix]) #build up for permutation mean then metric
  tb<-tb[,-1]
  for(i in 1:30){
    f<-dt
    f$x<-sample(x[ix],size = length(x[ix]))
    ylp=predict(lmd,newdata=f[,c(-1,-2)]) 
    tb<-cbind(tb,ylp)
  }
  ylp<-apply(tb,1,mean) #yp (permute) final
  }
  ylp<-sample(y[ix],size = length(y[ix]))
  
  tb=vector() #build up for permutation metric then mean 
  tbx=vector() #build up for permutation metric then mean 
  for(i in 1:30){
    f$x<-sample(x[ix],size = length(x[ix]))
    ylpp=predict(lmd,newdata=f[,c(-1,-2)]) 
    tb<-cbind(tb,metr(ylpp,y[ix]))
    tbx<-cbind(tb,metr(ylpp,ylx))
  }
  ylpm<-mean(tb) #yp (permute) final
  ylxpm<-mean(tbx)
  
  record<-rbind(record,data.frame(1,metr(y,yo),metr(y,yp),ypl,metr(y,yi),
                                  metr(y[ix],yl),metr(y[ix],ylx),metr(y[ix],ylm),
                                  metr(y[ix],ylp),ylpm,metr(ylx,ylm),ylxpm))
}

names(record)<-c("ideal_corrs_w_y", "man_X_meaned",
                 "man_perm_mean_metric",
                 "man_perm_metric_mean", "man_ideal","modl_w_x",
                  "modl_wo_x", "modl_mean_x",
                 "y_perm","modl_x_perm",
                 "modl_wo_x_and_modl_mean_x",
                 "modl_wo_x_and_modl_perm_x")

print(record)
for(i in 1:dim(record)[2]){
  record[is.na(record[,i]),i] <- mean(record[,i])
}
dts<-dist(t(record), method = "manh",upper=T)
out<-round((as.matrix(dts)[,c(1,5,7)])/iter,digits = 3)
#not reached? crashes at dist with NAs?
rownames(out)[1] <- allmodel
print(out)
recoutr<-rbind(recoutr,out)
save(recoutr,file = "routSplitr_vq1Corr.Rdata")
dts<-dist(t(record), method = "eucl",upper=T)
out<-round((as.matrix(dts)[,c(1,5,7)])/iter,digits = 3)
#not reached? crashes at dist with NAs?
rownames(out)[1] <- allmodel
print(out)
recoutr2<-rbind(recoutr2,out)
save(recoutr2,file = "routSplitr_vq1L2Corr.Rdata")


avc<-vector()
for(i in 1:dim(record)[2]){
  avc<-c(avc,mean(record[,i]))
}
justmeans<-cbind(justmeans,data.frame(avc))
names(justmeans)[dim(justmeans)[2]]<-allmodel
save(justmeans,file = "routSplitr_vq1.Rdata")

finished<-c(finished,allmodel)
})
}
recoutr;finished
#cor(record)
#cor(record,method = "sp")
#record[,]<-record[,]^2
#cor(record)
#cor(record,method = "sp")

