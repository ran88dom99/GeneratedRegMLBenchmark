Rows=100
simScores<-matrix(data = 0, nrow = Rows, ncol = 12, byrow = FALSE,dimnames = NULL);
simScores[,11]<-1;###!!!!!!! this may be necessary for many algorithms
#vector of strings to keep names of each project, increase maximum
gens.names=vector(length = 100)
max.out=vector(length = 100)
varimport=matrix(data=0, nrow = 100, ncol = 1000)
#single scalar to keep count
gen.count=0

######basic sum C1 + C2 ########
gen.count=gen.count+1
gens.names[gen.count]="basic sum C1 + C2"
max.out[gen.count]=1
varim=c(1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]+simScores[Row,3]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######basic latent features######
gen.count=gen.count+1
gens.names[gen.count]="basic latent features"
max.out[gen.count]=1
varim=c(1,1,1,1,1,1,1,1,1,1)
varimport[gen.count,1:length(varim)]=varim

simPubs<-matrix(data = 0, nrow = 10, ncol = 3, byrow = FALSE,dimnames = NULL)
simGames<-matrix(data = 0, nrow = Rows, ncol = 3, byrow = FALSE,dimnames = NULL)
#simScores<-matrix(data = 0, nrow = Rows, ncol = 11, byrow = FALSE,dimnames = NULL)

for(Col in 1:10)
{simPubs[Col,1:3]=rnorm(3, mean = 0, sd = 1)}
for(Row in 1:Rows)
{simGames[Row,1:3]=rnorm(3, mean = 0, sd = 1)}

for(Col in 1:10){
  for(Row in 1:Rows){
    simScores[Row,Col]=sum(simPubs[Col,1]*simGames[Row,1],simPubs[Col,2]*simGames[Row,2],simPubs[Col,3]*simGames[Row,3])
  }}

write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv", sep = ""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")


######just random#########
gen.count=gen.count+1
gens.names[gen.count]="random"
max.out[gen.count]=0
varim=c(0,0,0,0,0,0,0,0,0,0)
varimport[gen.count,1:length(varim)]=varim

for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######bm C1 ^ 2#######
gen.count=gen.count+1
gens.names[gen.count]="bm C1 ^ 2"
max.out[gen.count]=1
varim=c(1,0,0,0,0,0,0,0,0,0)
varimport[gen.count,1:length(varim)]=varim

for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]*simScores[Row,2]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######bm sqr00t(C1)#######
gen.count=gen.count+1
gens.names[gen.count]="bm sqroot(C1)"
max.out[gen.count]=1
varim=c(1,0,0,0,0,0,0,0,0,0)
varimport[gen.count,1:length(varim)]=varim

for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=sqrt(simScores[Row,2])
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######bm C1 ^ .5#######
gen.count=gen.count+1
gens.names[gen.count]="bm C1 ^ .5"
max.out[gen.count]=1#many values undefined
varim=c(1,0,0,0,0,0,0,0,0,0)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]^.5
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######bm 1dC1#######
gen.count=gen.count+1
gens.names[gen.count]="bm 1dC1"
max.out[gen.count]=1
varim=c(1,0)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=1/(simScores[Row,2])
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######bm C1 ^ -2#######
gen.count=gen.count+1
gens.names[gen.count]="bm C1 ^ -2"
max.out[gen.count]=1
varim=c(1,0,0,0,0,0,0,0,0,0)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]^-2
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######bm log(C1) natural log#######
gen.count=gen.count+1
gens.names[gen.count]="bm log(C1)"
max.out[gen.count]=1#plenty undefined
varim=c(1,0)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=log(simScores[Row,2])
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######bm sin(C2)#####
gen.count=gen.count+1
gens.names[gen.count]="bm sin(C2)"
max.out[gen.count]=1#many values undefined
varim=c(1,0)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 3)
}
for(Row in 1:Rows){
  simScores[Row,1]=sin(simScores[Row,2])
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######bm sin(13 * C2 ^ -.2)#####
gen.count=gen.count+1
gens.names[gen.count]="bm sin(13 * C2 ^ -.2)"
max.out[gen.count]=1#many values undefined
varim=c(1,0)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 3)
}
for(Row in 1:Rows){
  simScores[Row,1]=sin(13*simScores[Row,2]^-.2)
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######poly aC1^3 + bC1^2 + dC1 ########
gen.count=gen.count+1
gens.names[gen.count]="poly aC1^3 + bC1^2 + dC1"
max.out[gen.count]=1
varim=c(1,0)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
coef1<-rnorm(1, mean = 0, sd = 1);coef2<-rnorm(1, mean = 0, sd = 1);
coef3<-rnorm(1, mean = 0, sd = 1);

for(Row in 1:Rows){
  simScores[Row,1]=coef1*simScores[Row,2]^3+coef2*simScores[Row,2]^2+coef3*simScores[Row,2]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######poly aC1^1.3 + bC1^2.1 + dC1^.7 ########
gen.count=gen.count+1
gens.names[gen.count]="poly aC1^1.3 + bC1^2.1 + dC1^.7"
max.out[gen.count]=1
varim=c(1,0)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
coef1<-rnorm(1, mean = 0, sd = 1);coef2<-rnorm(1, mean = 0, sd = 1);
coef3<-rnorm(1, mean = 0, sd = 1);

for(Row in 1:Rows){
  simScores[Row,1]=coef1*simScores[Row,2]^1.3+coef2*simScores[Row,2]^2.1+coef3*simScores[Row,2]^.7
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######poly C1 t C2 ########
gen.count=gen.count+1
gens.names[gen.count]="poly C1 t C2"
max.out[gen.count]=1
varim=c(1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]*simScores[Row,3]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######poly C1 ^ C2######
gen.count=gen.count+1
gens.names[gen.count]="poly C1 ^ C2"
max.out[gen.count]=1#many values undefined
varim=c(1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,3]^simScores[Row,2]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######poly C1 t C2 t C3#######
gen.count=gen.count+1
gens.names[gen.count]="poly C1 t C2 t C3"
max.out[gen.count]=1
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]*simScores[Row,4]*simScores[Row,3]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######poly C1 t C2 t C3^1d3######
gen.count=gen.count+1
gens.names[gen.count]="poly C1 t C2 t C3^1d3"
max.out[gen.count]=1#many values undefined
varim=c(1,1,.3)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]*simScores[Row,4]*simScores[Row,3]^(1/3)
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######poly C1 t C2 ^ C3######
gen.count=gen.count+1
gens.names[gen.count]="poly C1 t C2 ^ C3"
max.out[gen.count]=1#many values undefined
varim=c(1,1,.3)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=simScores[Row,2]*simScores[Row,4]^simScores[Row,3]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######ifs 1 & -1 based on C1######
gen.count=gen.count+1
gens.names[gen.count]="ifs 1 & -1 on C1"
max.out[gen.count]=1
varim=c(1,0)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,2]>.5){
    simScores[Row,1]=-1
  }else{
    simScores[Row,1]=1
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######ifs C2 & -C2 based on C1####
gen.count=gen.count+1
gens.names[gen.count]="ifs C2 & -C2 on C1"
max.out[gen.count]=1
varim=c(1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,3]>.5){
    simScores[Row,1]=-simScores[Row,2]
  }else{
    simScores[Row,1]=simScores[Row,2]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######ifs C2+C3 & C4+C5 on C1#########
gen.count=gen.count+1
gens.names[gen.count]="ifs C2+C3 & C4+C5 on C1"
max.out[gen.count]=1
varim=c(1,1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,4]>.5){
    simScores[Row,1]=simScores[Row,2]+simScores[Row,3]
  }else{
    simScores[Row,1]=simScores[Row,5]+simScores[Row,6]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######ifs smooth -C2-C3&C2+C3 onC1#####
gen.count=gen.count+1
gens.names[gen.count]="ifs smooth -C2-C3&C2+C3 onC1"
max.out[gen.count]=1
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim

for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,4]>.5){
    simScores[Row,1]=-simScores[Row,2]-simScores[Row,3]
  }else{
    simScores[Row,1]=simScores[Row,2]+simScores[Row,3]
  }
  if((simScores[Row,4]>.0)&&(simScores[Row,1]<.5)){
    simScores[Row,1]=-simScores[Row,2]*.1-simScores[Row,3]*.1
  }
  if((simScores[Row,4]>-.5)&&(simScores[Row,1]<.0)){
    simScores[Row,1]=simScores[Row,2]*.1+simScores[Row,3]*.1
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######ifs -C2-C3 & C2+C3 on C1####
gen.count=gen.count+1
gens.names[gen.count]="ifs -C2-C3 & C2+C3 on C1"
max.out[gen.count]=1
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,4]>.5){
    simScores[Row,1]=-simScores[Row,2]-simScores[Row,3]
  }else{
    simScores[Row,1]=simScores[Row,2]+simScores[Row,3]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######ifs C1 & C2 based on C3#####
gen.count=gen.count+1
gens.names[gen.count]="ifs C1 & C2 on C1"
max.out[gen.count]=1
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,2]>.5){
    simScores[Row,1]=simScores[Row,3]
  }else{
    simScores[Row,1]=simScores[Row,4]#####error was here
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######ifs C2 & C3t2 on C4g1.5#####
gen.count=gen.count+1
gens.names[gen.count]="ifs C2 & C3t2 on C4g1.5"
max.out[gen.count]=1
varim=c(1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,4]>1.5){
    simScores[Row,1]=simScores[Row,3]*2
  }else{
    simScores[Row,1]=simScores[Row,2]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######ifs C1&C2 on random#####
gen.count=gen.count+1
gens.names[gen.count]="ifs C1&C2 on random"
max.out[gen.count]=.6#.24 err.sqd 
varim=c(1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(rnorm(1, mean = 0, sd = 1)>.5){
    simScores[Row,1]=simScores[Row,3]
  }else{
    simScores[Row,1]=simScores[Row,2]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

######ifs nested 2 layer######
gen.count=gen.count+1
gens.names[gen.count]="ifs nested 2 layer"
max.out[gen.count]=1
varim=c(1,1,1,1,1)
varimport[gen.count,1:length(varim)]=varim
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,3]>.3){
    if(simScores[Row,2]>.3){
      simScores[Row,1]=-simScores[Row,6]
    }else{
      simScores[Row,1]=simScores[Row,6]
    }
  }else{
    if(simScores[Row,4]>.3){
      simScores[Row,1]=-simScores[Row,5]
    }else{
      simScores[Row,1]=simScores[Row,5]
    }
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")



#####write alg names to file; last######
out<-data.frame(gens.names,max.out)
write.table(out,
            file = "gens names.csv", append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = T,
            col.names = F, qmethod = "double")
source("More Generators.R")
