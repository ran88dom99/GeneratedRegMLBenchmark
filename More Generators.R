library(caret)

######mean subtraction byrow####
gen.count=gen.count+1
gens.names[gen.count]="mean subtraction in each row"
max.out[gen.count]=1#1 err.sqd
varim=c(1,1,1,1,1,1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
  me.sim.sub=mean(simScores[Row,1:10],na.rm=T)
  for(Col in 1:10){
    simScores[Row,Col]=simScores[Row,Col]-me.sim.sub
  }}

write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######median subtraction byrow######
gen.count=gen.count+1
gens.names[gen.count]="median subtraction in each row"
max.out[gen.count]=.29#.43 err.sqd
varim=c(1,1,1,1,1,1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
  me.sim.sub=median(simScores[Row,1:10],na.rm=T)
  for(Col in 1:10){
    simScores[Row,Col]=simScores[Row,Col]-me.sim.sub
  }}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######rescale 4 poly ######
gen.count=gen.count+1
gens.names[gen.count]="rescale 4 poly"
max.out[gen.count]=0#0 err.sqd
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]+simScores[Row,4]
}
for(Col in 1:10){
  A<-rnorm(1, mean = 0, sd = 1)
  B<-rnorm(1, mean = 0, sd = 1)
  C<-rnorm(1, mean = 0, sd = 1)
  D<-rnorm(1, mean = 0, sd = 1)
  for(Row in 1:Rows){
    X<-simScores[Row,Col]
    simScores[Row,1]=A+X*B+X*X*C+X*X*X*D
    }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######rescale iso addition########
gen.count=gen.count+1
gens.names[gen.count]="rescale isotonic addition"
max.out[gen.count]=.5#.8 err.sqd
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]+simScores[Row,4]
}
for(Col in 1:10){
  A<-abs(rnorm(1, mean = 0, sd = .2))
  B<-abs(rnorm(1, mean = 0, sd = .2))
  C<-abs(rnorm(1, mean = 0, sd = .2))
  D<-abs(rnorm(1, mean = 0, sd = .2))
  for(Row in 1:Rows){
    x<-simScores[Row,Col]
    if(x>-1) x=x+A
    if(x>-.3) x=x+B
    if(x>.1) x=x+C
    if(x>1) x=x+D
    x->simScores[Row,Col]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######rescale iso add rand bin######
gen.count=gen.count+1
gens.names[gen.count]="rescale isotonic addition ran bin"
max.out[gen.count]=.5#.8 err.sqd
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]+simScores[Row,4]
}
for(Col in 1:10){
  q<-rnorm(1, mean = 0, sd = 1)
  w<-rnorm(1, mean = 0, sd = 1)
  p<-rnorm(1, mean = 0, sd = 1)
  o<-rnorm(1, mean = 0, sd = 1)
  A<-abs(rnorm(1, mean = 0, sd = .2))
  B<-abs(rnorm(1, mean = 0, sd = .2))
  C<-abs(rnorm(1, mean = 0, sd = .2))
  D<-abs(rnorm(1, mean = 0, sd = .2))
  for(Row in 1:Rows){
    x<-simScores[Row,Col]
    if(x>q) x=x+A
    if(x>w) x=x+B
    if(x>p) x=x+C
    if(x>o) x=x+D
    x->simScores[Row,Col]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######noise on predictors what if users missvote?####
gen.count=gen.count+1
gens.names[gen.count]="noise in predictors"
max.out[gen.count]=.7#.91 err.sqd
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=(simScores[Row,4])+(-simScores[Row,2])+(simScores[Row,3])
}
for(Row in 1:Rows){
  for(Col in 2:7){
  simScores[Row,Col]=simScores[Row,Col]+rnorm(1, mean = 0, sd = .3)}
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######outliers ######
gen.count=gen.count+1
gens.names[gen.count]="outliers"
max.out[gen.count]=.6#.24 err.sqd
varim=c(1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(rnorm(1, mean = 0, sd = 1)>1.5){###error used to be posssible to switch
    simScores[Row,1]=rnorm(1, mean = 0, sd = 7)
  }else{
    simScores[Row,1]=simScores[Row,2]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######colinerity unnecessary#####
gen.count=gen.count+1
gens.names[gen.count]="colinerity unnecessary"
max.out[gen.count]=1
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=(simScores[Row,4])+(-simScores[Row,2])+(simScores[Row,3])
}
for(Row in 1:Rows){
  simScores[Row,5:7]=simScores[Row,2:4]
  for(Col in 5:7){#noise is applied only to doubles
    simScores[Row,Col]<-simScores[Row,Col]+rnorm(1, mean = 0, sd = .1)}
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######colinerity middle is correct#####
gen.count=gen.count+1#almost identical to "noise in predictors"
gens.names[gen.count]="colinerity middle is correct"
max.out[gen.count]=.8#.96 err.sqd
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=(simScores[Row,4])+(-simScores[Row,2])+(simScores[Row,3])
}
for(Row in 1:Rows){
  simScores[Row,5:7]=simScores[Row,2:4]
  for(Col in 2:7){
  simScores[Row,Col]=simScores[Row,Col]+rnorm(1, mean = 0, sd = .3)}
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######Spread thinly#######
gen.count=gen.count+1
gens.names[gen.count]="Spread thinly"
max.out[gen.count]=1
varim=c(1,1,1,1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=.1*simScores[Row,8]+.1*simScores[Row,2]+.1*simScores[Row,3]+.1*simScores[Row,4]+.1*simScores[Row,5]+.1*simScores[Row,6]+.1*simScores[Row,7]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######needles in haystack######
gen.count=gen.count+1
gens.names[gen.count]="needles in haystack"
max.out[gen.count]=1
simScores<-matrix(data = 0, nrow = Rows, ncol = 1000, byrow = FALSE,dimnames = NULL);
for(Row in 1:Rows){
  simScores[Row,1:1000]=rnorm(1000, mean = 0, sd = 1)
}
a<-round(runif(10, min = 0, max = 1000))
for(Row in 1:Rows){
  simScores[Row,1]=.1*simScores[Row,a[1]]+.1*simScores[Row,a[2]]+.1*simScores[Row,a[3]]+.1*simScores[Row,a[4]]+.1*simScores[Row,a[5]]+.1*simScores[Row,a[6]]+.1*simScores[Row,a[7]]+.1*simScores[Row,a[8]]+.1*simScores[Row,a[9]]
}
varim=vector(mode ="numeric",length = 999)
varim[a]<-1
varimport[gen.count,1:length(varim)]=varim #EXPECT ERRORS HERE
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
simScores<-matrix(data = 0, nrow = Rows, ncol = 12, byrow = FALSE,dimnames = NULL);
simScores[,11]<-1
######haystack + noise######
gen.count=gen.count+1
gens.names[gen.count]="needles hay noise"
simScores<-matrix(data = 0, nrow = Rows, ncol = 1000, byrow = FALSE,dimnames = NULL);
max.out[gen.count]=.5#.5 actualy I forgot.....
for(Row in 1:Rows){
  simScores[Row,1:1000]=rnorm(1000, mean = 0, sd = 1)
}
a<-round(runif(10, min = 0, max = 1000))
for(Row in 1:Rows){
  simScores[Row,1]=.1*simScores[Row,a[1]]+.1*simScores[Row,a[2]]+.1*simScores[Row,a[3]]+.1*simScores[Row,a[4]]+.1*simScores[Row,a[5]]+.1*simScores[Row,a[6]]+.1*simScores[Row,a[7]]+.1*simScores[Row,a[8]]+.1*simScores[Row,a[9]]
}
for(Row in 1:Rows){
  simScores[Row,2:1000]=simScores[Row,2:1000]+rnorm(999, mean = 0, sd = .3)
}
varim=vector(mode ="numeric",length = 999)
varim[a]<-1
varimport[gen.count,1:length(varim)]=varim #EXPECT ERRORS HERE
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
simScores<-matrix(data = 0, nrow = Rows, ncol = 12, byrow = FALSE,dimnames = NULL);
simScores[,11]<-1


######sparsity NA#########
gen.count=gen.count+1
gens.names[gen.count]="sparsity NA"
max.out[gen.count]=.44#.62 me^2
varim=c(1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]+simScores[Row,4]+simScores[Row,5]
}
for(Row in 1:Rows){
  for(Col in 2:10){
    if(runif(1, min = 0, max = 1)>.7) simScores[Row,Col]<-NA
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######sparsity 0 offcenter#########
gen.count=gen.count+1
gens.names[gen.count]="sparsity 0 offcenter"
max.out[gen.count]=.54#.7 me^2
varim=c(1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 2, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]+simScores[Row,4]+simScores[Row,5]
}
for(Row in 1:Rows){
  for(Col in 2:10){
    if(runif(1, min = 0, max = 1)>.7) simScores[Row,Col]<-0
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######sparsity 2 row center#########
gen.count=gen.count+1
gens.names[gen.count]="sparsity 2 row center"
max.out[gen.count]=.54#.7 me^2
varim=c(1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 2, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]+simScores[Row,4]+simScores[Row,5]
}
for(Row in 1:Rows){
  for(Col in 2:10){
    if(runif(1, min = 0, max = 1)>.7) simScores[Row,Col]<-2
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######sparsity col center#########
gen.count=gen.count+1
gens.names[gen.count]="sparsity 2 col center"
max.out[gen.count]=.54#.7 me^2
varim=c(1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 2, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]+simScores[Row,4]+simScores[Row,5]
}
for(Col in 2:10){
  mean.col<-mean(simScores[,Col], na.rm = T)
  for(Row in 1:Rows){
    if(runif(1, min = 0, max = 1)>.7) simScores[Row,Col]<-mean.col
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######sparsity 20#########
gen.count=gen.count+1
gens.names[gen.count]="sparsity 20"
max.out[gen.count]=.5#.65 me^2
varim=c(1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]+simScores[Row,4]+simScores[Row,5]
}
for(Row in 1:Rows){
  for(Col in 2:10){
    if(runif(1, min = 0, max = 1)>.7) simScores[Row,Col]<-20
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######quantilization#######
gen.count=gen.count+1
gens.names[gen.count]="quantilization"
max.out[gen.count]=.1#.1 err.sqd
varim=c(1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
simScores[,2]<- (rank(simScores[,1],na.last = "keep",ties.method = "average")-1) 
#preProcValues<- preProcess(simScores[,2],method = c("range"))
#simScores[,2]<- predict(preProcValues, simScores[,2])
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######composite####
gen.count=gen.count+1
gens.names[gen.count]="composite 1"
max.out[gen.count]=.21#.42
varim=c(1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]+simScores[Row,4]+simScores[Row,5]
}
simScores1<-simScores[,c(1,2,3,6,7)]
simScores2<-simScores[,c(1,4,5,8,9)]
write.table(round(simScores1,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
gen.count=gen.count+1
gens.names[gen.count]="composite 2"
max.out[gen.count]=.21#.42
varim=c(1,1)
varimport[gen.count,1:length(varim)]=varim
write.table(round(simScores2,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
gen.count=gen.count+1
gens.names[gen.count]="composite complete"
max.out[gen.count]=1
varim=c(1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######generated by pros#########
gen.count=gen.count+1
gens.names[gen.count]="OpenML mv"
max.out[gen.count]=1#1 err.sqd
gen.count=gen.count+1
gens.names[gen.count]="OpenML 2dplanes"
max.out[gen.count]=1#1 err.sqd
#OpenML mv.csv
#OpenML 2dplanes.csv

#######mlbench Boston Friedman#############
library(mlbench)
data("BostonHousing")
summary("BostonHousing")
is.data.frame(BostonHousing)
BostonHow<-data.frame(BostonHousing[,14],BostonHousing[,1:13])
gen.count=gen.count+1
gens.names[gen.count]="Boston Housing"
max.out[gen.count]=1
write.table(round(BostonHow,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

F1<-mlbench.friedman1(Rows, sd=1)
FF1<-data.frame(F1[["y"]],F1[[1]])
gen.count=gen.count+1
gens.names[gen.count]="Friedman's 1st"
max.out[gen.count]=1
write.table(round(FF1,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

gen.count=gen.count+1
gens.names[gen.count]="kaggle housing"
max.out[gen.count]=.75
#friedman.1.data(n=Rows) #tgp package#
#Friedman 1/80 generated for validation of MARS https://artax.karlin.mff.cuni.cz/r-help/library/tgp/html/friedman.1.data.html
#kaggle MAL
#######simulate reccomendation problem######
gen.count=gen.count+1
gens.names[gen.count]="Recc sim 1"
max.out[gen.count]=1

simPubs<-matrix(data = 0, nrow = 100, ncol = 10, byrow = FALSE,dimnames = NULL)
simGames<-matrix(data = 0, nrow = Rows, ncol = 10, byrow = FALSE,dimnames = NULL)
#simScores<-matrix(data = 0, nrow = Rows, ncol = 11, byrow = FALSE,dimnames = NULL)
#generate based on latency, then missvote, then isotonic reg, then finaly sparsity 0

for(Col in 1:100)
{simPubs[Col,1:10]=rnorm(10, mean = 0, sd = 1)}
for(Row in 1:Rows)
{simGames[Row,1:10]=rnorm(10, mean = 0, sd = 1)}
for(Col in 1:100){
  for(Row in 1:Rows){
    simScores[Row,Col]=sum(simPubs[Col,1]*simGames[Row,1],simPubs[Col,2]*simGames[Row,2],simPubs[Col,3]*simGames[Row,3])
  }}

for(Row in 1:Rows){
  for(Col in 2:100){
    simScores[Row,Col]=simScores[Row,Col]+rnorm(1, mean = 0, sd = .3)}
}

for(Col in 2:100){
  q<-rnorm(1, mean = 0, sd = 1)
  w<-rnorm(1, mean = 0, sd = 1)
  p<-rnorm(1, mean = 0, sd = 1)
  o<-rnorm(1, mean = 0, sd = 1)
  A<-abs(rnorm(1, mean = 0, sd = .2))
  B<-abs(rnorm(1, mean = 0, sd = .2))
  C<-abs(rnorm(1, mean = 0, sd = .2))
  D<-abs(rnorm(1, mean = 0, sd = .2))
  for(Row in 1:Rows){
    x<-simScores[Row,Col]
    if(x>q) x=x+A
    if(x>w) x=x+B
    if(x>p) x=x+C
    if(x>o) x=x+D
    x->simScores[Row,Col]
  }
}

for(Row in 1:Rows){
  for(Col in 2:100){
    if(runif(1, min = 0, max = 1)>.4) simScores[Row,Col]<-0
  }
}

write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")


#######some users just vote everything they didnt hate at 9###
#####nonuniform generating distributions###
######simple lack of data###
#####if c1 c2 c3 agree its a geat movie###
#######maximum possible accuracy####
#since data is generated, maximum attainable is determinable
#####write alg names to file; last######
out<-data.frame(gens.names,max.out)
write.table(out,
            file = "gens names.csv", append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = T,
            col.names = F, qmethod = "double")
write.table(varimport,
            file = "gens names.csv", append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
