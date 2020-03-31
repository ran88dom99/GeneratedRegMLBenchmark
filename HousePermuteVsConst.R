##### make a single function to try everything#####

makemean<-function(x){
  t<-x
  for(i in 1:length(x)){
    t[i]<-mean(x)
  }
  return(t)
} # just makes a vector of means

metr<-function(g,h){ 
  cor(g,h)
} #EDIT metric measuring agreement and error between two variables
#edit ideal at line 155 1 for correlation and 0 for rmse
#do not forget to change save file name 
#DIDNT use competnt feature selection and remove unused featurs from dataframe
##set all file name including htis one

require(caret)
allmodels <- unique(modelLookup()[modelLookup()$forReg,c(1)])
bad.models=c("randomGLM","DENFIS","neuralnet","partDSA","blackboost","bstSm","bstTree","penalized","brnn",
             "gamLoess","ANFIS","FIR.DM","FS.HGD","nodeHarvest","mlpWeightDecayML","monmlp","mlp","mlpWeightDecay",
             "mlpSGD","rbf","rbfDDA","rfRules","GFS.FR.MOGUL","mlpML","HYFIS","GFS.THRIFT" ,"GFS.LT.RS",
             "svmSpectrumString","svmExpoString","svmBoundrangeString",
             "bagEarthGCV","bam","mxnet","mlpKerasDecay","mlpKerasDropout",
             "qrnn","mxnet","mxnetAdam","WM")

allmodels <- setdiff(allmodels,bad.models)
if(exists("recoutr2")){
  allmodels <- setdiff(allmodels,rownames(recoutr2))
} else {
  recoutr <- data.frame()
  recoutr2 <- data.frame()
  justmeans <- data.frame(nams=c("ideal_corrs_w_y", 
                                 "modl_w_x",
                                 "modl_wo_x",
                                 "modl_mean_x",
                                 "modl_x_perm_mean",
                                 "modl_x_quant_mean",
                                 "modl_x_perm_met",
                                 "modl_x_perm_met_sd",
                                 "wo_x_n_modl_mean_x",
                                 "wo_x_n_modl_perm_mean",
                                 "wo_x_n_modl_x_quant_mean",
                                 "wo_x_n_modl_x_perm_met",
                                 "wo_x_n_modl_x_perm_met_sd"))
  finished <- vector()
}

library(MASS)
bs<-Boston


iter <- 3
leng <- dim(bs)[1]

bs<-bs[,c(14,1,2,3,4,5,6,7,8,9,10,11,12,13)]
names(bs)[1]<-"y"
str(bs)

for(col in 2:14){ #col<-14
for(allmodel in allmodels){#allmodel<-allmodels[3]
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
    for(i in 1:iter){#i<-1
      ex <- sample(1:leng,leng/3) #testing partition
      rx <- setdiff(c(1:leng),c(ex))#training partition
      y <- bs$y
      
      dt <- bs[ex,]
      d <- bs[rx,]
      lE<-length(ex)
      
      lmd <- mdle(d[,c(-col)]) #based on model withOUT x
      ylx = predict(lmd,newdata=dt[,c(-1,-col)]) 
      lmd <- mdle(d[,]) #based on linear model with x
      yl = predict(lmd,newdata=dt[,c(-1)]) #based on model with x
      f <- dt
      f[,col] <- makemean(d[,col])[1:lE]
      ylm = predict(lmd,newdata=f[,c(-1)]) #based on model with predict x as mean
      
      #based on linear model with x as permutation
      try({
        tb = data.frame(y[ex]) #build up for permutation mean then metric
        tb <- tb[,-1]
        for(i in 1:20){
          f <- dt
          f[,col] <-sample(d[,col],size = length(f[,col]))
          ylp = predict(lmd,newdata=f[,c(-1)]) 
          tb <- cbind(tb,ylp)
        }
        ylp <- apply(tb,1,mean)
      })
      
      #x as quantiles
      try({
        tb = data.frame(y[ex]) #build up for permutation mean then metric
        tb <- tb[,-1]
        tq <- vector(mode = "numeric",length = dim(f)[1])
        for(i in quantile(d[,col],seq(.1,.9,.2))){#i<-quantile(f[,col])[1]
          g <- dt
          tq[] <- i
          tq[] <- f[which.min(abs(f[,col]-tq)),col]
          g[,col] <- tq
          ylq = predict(lmd,newdata=g[,c(-1)]) 
          tb <- cbind(tb,ylq)
        }
        ylq <- apply(tb,1,mean)
      })
      
      try({
      tb = vector() #build up for permutation metric then mean 
      tbx = vector() #build up for permutation metric then mean 
      for(i in 1:20){
        f[,col] <- sample(d[,col],size = length(f[,col]))
        ylpp = predict(lmd,newdata=f[,c(-1)]) 
        tb <- rbind(tb,metr(ylpp,y[ex]))
        tbx <- rbind(tbx,metr(ylpp,ylx))
      }
      ylpm <- mean(tb) #yp (permute) final
      ylxpm <- mean(tbx)
      ylpsd <- sd(tb) #yp (permute) final
      ylxpsd <- sd(tbx)
      })
      
      record <- rbind(record,data.frame(1, metr(y[ex],yl),metr(y[ex],ylx),metr(y[ex],ylm),
                                      metr(y[ex],ylp),metr(y[ex],ylq),ylpm,ylpsd,
                                      metr(ylx,ylm),metr(ylx,ylp),metr(ylx,ylq),ylxpm,ylxpsd))
    }
    
    names(record)<-c("ideal_corrs_w_y", 
                     "modl_w_x",
                     "modl_wo_x",
                     "modl_mean_x",
                     "modl_x_perm_mean",
                     "modl_x_quant_mean",
                     "modl_x_perm_met",
                     "modl_x_perm_met_sd",
                     "wo_x_n_modl_mean_x",
                     "wo_x_n_modl_perm_mean",
                     "wo_x_n_modl_x_quant_mean",
                     "wo_x_n_modl_x_perm_met",
                     "wo_x_n_modl_x_perm_met_sd")
    
    print(record)
    if(F){
    for(i in 1:dim(record)[2]){
      record[is.na(record[,i]),i] <- mean(record[,i])
    }
    
    dts<-dist(t(record), method = "manh",upper=T)
    out<-round((as.matrix(dts)[,c(1,5,7)])/iter,digits = 3)
    #not reached? crashes at dist with NAs?
    rownames(out)[1] <- allmodel
    print(out)
    recoutr<-rbind(recoutr,out)
    save(recoutr,file = "routHouseCorr.Rdata")
    dts<-dist(t(record), method = "eucl",upper=T)
    out<-round((as.matrix(dts)[,c(1,5,7)])/iter,digits = 3)
    #not reached? crashes at dist with NAs?
    rownames(out)[1] <- allmodel
    print(out)
    recoutr2<-rbind(recoutr2,out)
    save(recoutr2,file = "routHouseL2Corr.Rdata")
    }
    
    avc<-vector()
    for(i in 1:dim(record)[2]){
      avc<-c(avc,mean(record[,i],na.rm = T))
    }
    justmeans<-cbind(justmeans,data.frame(avc))
    names(justmeans)[dim(justmeans)[2]]<-paste0(allmodel,"_",names(bs)[col])
    save(justmeans,file = "routHouse.Rdata")
    
    finished<-c(finished,allmodel)
  })
}
}
recoutr;finished
#cor(record)
#cor(record,method = "sp")
#record[,]<-record[,]^2
#cor(record)
#cor(record,method = "sp")

##### IMPORTANT TODO #####
#where do we get the data for permuting? Training OFC! DONE. but not in first run
#quantiles represent edges of cuts not centers; fix this.
#mean before or after proximity metric, lets see what shows up in aggregate anylis
#what about Standard deviation? also after anyl


