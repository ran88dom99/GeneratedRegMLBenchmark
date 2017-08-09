Rows=300
simScores<-matrix(data = 0, nrow = Rows, ncol = 12, byrow = FALSE,dimnames = NULL);
simScores[,11]<-1;###!!!!!!! this may be necessary for many algorithms
#vector of strings to keep names of each project, increase maximum
gens.names=vector(length = Rows)
#single scalar to keep count
gen.count=0

############basic latent features######
gen.count=gen.count+1
gens.names[gen.count]="basic latent features"

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


#######just random#########
gen.count=gen.count+1
gens.names[gen.count]="just random"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")



########polynomial C1 ^ 2#######
gen.count=gen.count+1
gens.names[gen.count]="polynomial C1 ^ 2"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,8]=simScores[Row,1]*simScores[Row,1]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
########polynomial C1 ^ .5#######
gen.count=gen.count+1
gens.names[gen.count]="polynomial C1 ^ .5"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,8]=simScores[Row,1]^.5
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
########polynomial C1 ^ -2#######
gen.count=gen.count+1
gens.names[gen.count]="polynomial C1 ^ -2"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,8]=simScores[Row,1]^-2
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

########polynomial C1 t C2 ########
gen.count=gen.count+1
gens.names[gen.count]="polynomial C1 t C2"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,8]=simScores[Row,2]*simScores[Row,1]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
########polynomial C1 ^ C2######
gen.count=gen.count+1
gens.names[gen.count]="polynomial C1 ^ C2"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,8]=simScores[Row,1]^simScores[Row,2]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
########polynomial C1 t C2 t C3#######
gen.count=gen.count+1
gens.names[gen.count]="polynomial C1 t C2 t C3"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,8]=simScores[Row,2]*simScores[Row,1]*simScores[Row,3]
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
########polynomial C1 t C2 t C3^1d3######
gen.count=gen.count+1
gens.names[gen.count]="polynomial C1 t C2 t C3^1d3"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,8]=simScores[Row,2]*simScores[Row,1]*simScores[Row,3]^(1/3)
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#####mean subtraction in each row####
gen.count=gen.count+1
gens.names[gen.count]="mean subtraction in each row"
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
#######median subtraction in each row######
gen.count=gen.count+1
gens.names[gen.count]="median subtraction in each row"
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

#####switches between 1 & -1 based on C1######
gen.count=gen.count+1
gens.names[gen.count]="switches between 1 & -1 based on C1"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,1]>.5){
    simScores[Row,8]=-1
  }else{
    simScores[Row,8]=1
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

#####switches between C2 & -C2 based on C1####
gen.count=gen.count+1
gens.names[gen.count]="switches between C2 & -C2 based on C1"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,1]>.5){
    simScores[Row,8]=-simScores[Row,2]
  }else{
    simScores[Row,8]=simScores[Row,2]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#####switches between C2+C3 & C4+C5 based on C1#########
gen.count=gen.count+1
gens.names[gen.count]="switches between C2+C3 & C4+C5 based on C1"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,1]>.5){
    simScores[Row,8]=simScores[Row,2]+simScores[Row,3]
  }else{
    simScores[Row,8]=simScores[Row,5]+simScores[Row,6]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#####smoothed switches between -C2-C3 & C2+C3 based on C1#####
gen.count=gen.count+1
gens.names[gen.count]="smoothed switches between -C2-C3 & C2+C3 based on C1"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,1]>.5){
    simScores[Row,8]=-simScores[Row,2]-simScores[Row,3]
  }else{
    simScores[Row,8]=simScores[Row,2]+simScores[Row,3]
  }
  if((simScores[Row,1]>.0)&&(simScores[Row,1]<.5)){
    simScores[Row,8]=-simScores[Row,2]*.1-simScores[Row,3]*.1
  }
  if((simScores[Row,1]>-.5)&&(simScores[Row,1]<.0)){
    simScores[Row,8]=simScores[Row,2]*.1+simScores[Row,3]*.1
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#####switches between -C2-C3 & C2+C3 based on C1####
gen.count=gen.count+1
gens.names[gen.count]="switches between -C2-C3 & C2+C3 based on C1"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,1]>.5){
    simScores[Row,8]=-simScores[Row,2]-simScores[Row,3]
  }else{
    simScores[Row,8]=simScores[Row,2]+simScores[Row,3]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#####randomly switches between C1 & C2#####
gen.count=gen.count+1
gens.names[gen.count]="randomly switches between C1 & C2"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simGames[Row,1]>.5){
    simScores[Row,8]=simScores[Row,1]
  }else{
    simScores[Row,8]=simScores[Row,2]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
##########needles in haystack######

########Spread thinly#######

########log(C1)#######

#######C1^-C2######

#######1/C1#######

#######haystack + noise######

#######bizzare rescales######

#######what if users missvote? that the noise is in predictors####

######outliers ######

######colinerity#####

#######partial friedman sin()#####


#######generated by pros######
#Friedman 1/80 generated for validation of MARS https://artax.karlin.mff.cuni.cz/r-help/library/tgp/html/friedman.1.data.html
#https://www.openml.org/d/344
#https://www.openml.org/d/215
#######maximum possible accuracy test####3
#since data is generated, maximum attainable is determinable

#####nonuniform generating distributions#####


#####if c1 c2 c3 agree its a geat movie#####

#####write alg names to file; last######
write.table(gens.names,
            file = "gens names.csv", append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = T,
            col.names = F, qmethod = "double")
