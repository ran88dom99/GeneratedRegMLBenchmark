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
load("~/GitHub/GeneratedRegMLBenchmark/routHouse.Rdata")
if(!exists("justmeans")){
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
  
} 
  finished <- vector()
  recoutr <- data.frame()
  recoutr2 <- data.frame()


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
  #record <- data.frame()
  try({
    for(izzyx in 1:iter){#i<-1
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
        qit <- 5
        a <- (1/qit)/2; b <- (1-(1/qit)/2); c <- (((b-a)/qit))
        qunt <- quantile(d[,col],seq(a,b,c))
        for(i in qunt){#i<-qunt[1]
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
      
      record1 <- data.frame(allmodel,colnam=names(bs)[col],
                             ideal_corrs_w_y=1,
                            modl_w_x=metr(y[ex],yl),
                            modl_wo_x=metr(y[ex],ylx),
                            modl_mean_x=metr(y[ex],ylm),
                            modl_x_perm_mean=metr(y[ex],ylp),
                            modl_x_quant_mean=metr(y[ex],ylq),
                            modl_x_perm_met=ylpm,
                            modl_x_perm_met_sd=ylpsd,
                            wo_x_n_modl_mean_x=metr(ylx,ylm),
                            wo_x_n_modl_perm_mean=metr(ylx,ylp),
                            wo_x_n_modl_x_quant_mean=metr(ylx,ylq),
                            wo_x_n_modl_x_perm_met=ylxpm,
                            wo_x_n_modl_x_perm_met_sd=ylxpsd)
      
      if (!exists("record")) {
        record <- record1
      } else {
      record <- rbind(record,record1)
      }
      
    }
    save(record,file = "rouRecHouse.Rdata")
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
    if(F){
    avc<-vector()
    for(i in 1:dim(record)[2]){
      avc<-c(avc,mean(record[,i],na.rm = T))
    }
    justmeans<-cbind(justmeans,data.frame(avc))
    names(justmeans)[dim(justmeans)[2]]<-paste0(allmodel,"_",names(bs)[col])
      save(justmeans,file = "routHouse.Rdata")
    }
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
# hopefully sources for X substitutes are from the training set now
#quantiles represent edges of cuts not centers; fix this. DONE
#mean before or after proximity metric, lets see what shows up in aggregate anylis
#what about Standard deviation? also after anyl
# record every time run not just mean and at that time ignore sd
#no more mxnet no more WM
# I cannnot concentrate enough to tell what is causing quant to create identical scores to mean
# its the correlation; the permuttion changes order but quant does not