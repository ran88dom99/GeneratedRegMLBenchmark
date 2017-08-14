
########Spread thinly#######
gen.count=gen.count+1
gens.names[gen.count]="Spread thinly"
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
########log(C1)#######
gen.count=gen.count+1
gens.names[gen.count]="log(C1)"
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
#######C1^-C2######
gen.count=gen.count+1
gens.names[gen.count]="C1^-C2"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=(simScores[Row,3])^(-simScores[Row,2])
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
#######1dC1#######
gen.count=gen.count+1
gens.names[gen.count]="1dC1"
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

#######bizzare rescales ######

#######some users just vote everything they didnt hate at 9#######

#######what if users missvote? that the noise is in predictors####
gen.count=gen.count+1
gens.names[gen.count]="noise in predictors"
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
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  if(simScores[Row,4]>1.5){
    simScores[Row,1]=rnorm(1, mean = 0, sd = 7)
  }else{
    simScores[Row,1]=simScores[Row,2]
  }
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")

#####switch C2 & C3 based on C4>1.5#####
gen.count=gen.count+1
gens.names[gen.count]="switches between C3 & C2 based on C4>1.5"
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

######colinerity unnecessary#####
gen.count=gen.count+1
gens.names[gen.count]="colinerity unnecessary"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=(simScores[Row,4])+(-simScores[Row,2])+(simScores[Row,3])
}
for(Row in 1:Rows){
  simScores[Row,5:7]=simScores[Row,2:4]
  for(COl in 5:7){
    simScores[Row,Col]<-simScores[Row,Col]+rnorm(1, mean = 0, sd = .3)}
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
######colinerity middle is correct#####
gen.count=gen.count+1
gens.names[gen.count]="colinerity middle is correct"
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
#######partial friedman sin()#####
gen.count=gen.count+1
gens.names[gen.count]="partial friedman sin(C2)"
for(Row in 1:Rows){
  simScores[Row,1:10]=rnorm(10, mean = 0, sd = 1)
}
for(Row in 1:Rows){
  simScores[Row,1]=sin(simScores[Row,2])
}
write.table(round(simScores,digits  = 3),
            file = paste(gens.names[gen.count],".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
##########needles in haystack######

#######haystack + noise######

######simple lack of data######

######sparsity NA#########

######sparsity 0#########

######sparsity 20#########

######nested if C2 if C3 C4 e C5 e if C5 C6 e C7######

#####randomly switches between C1 & C2#####
gen.count=gen.count+1
gens.names[gen.count]="randomly switches between C1 & C2"
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

#######generated by pros######
#friedman.1.data(n=Rows) #tgp package#
#Friedman 1/80 generated for validation of MARS https://artax.karlin.mff.cuni.cz/r-help/library/tgp/html/friedman.1.data.html
#OpenML mv.csv
#OpenML 2dplanes.csv

#####nonuniform generating distributions#####


#####if c1 c2 c3 agree its a geat movie#####

#####each of the scalings in preprocessing#######

#######maximum possible accuracy####
#since data is generated, maximum attainable is determinable

#####write alg names to file; last######
write.table(gens.names,
            file = "gens names.csv", append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = T,
            col.names = F, qmethod = "double")
