#set cover solution
#25 people 5 boards.
#lines represent sets and pass through and come out the other side.
#so five items nearly touch
#must be 179* otherwise repeats. Say all positive X directions.

#go through each dot (item) chack if all its directions are covered 
#else create new set
# saydirection must go five squares out from dot one square in. 
#subtract distance to edge then one more and start from opposite side

# does not work for numbers of sides without median being on top of a dot ie odd

#can probaby do non squares so long as odd

# maybe programming was a better idea after all IT IS A MUCH MORE GENERAL CHOICE

NumDots <- 25
rootNumDots <- ceiling(sqrt(NumDots))
DistToEdg <- ceiling(rootNumDots/2)-1
df <- data.frame(row.names = (1:NumDots))
temp.df <- data.frame(row.names = (1:NumDots))
numDirections <- ceiling(NumDots/rootNumDots)+1 # (NumDots-1)/(rootNumDots-1)

for(itr in 1:NumDots){
  df[itr,1] <- 1+((itr-1) %% rootNumDots)
  df[itr,2] <- ceiling((itr) / (rootNumDots))
}

for(itr2 in 1:NumDots){
    if((df[itr2,1]-1 <= DistToEdg) && (df[itr2,2]-1 <= DistToEdg)){
      temp.df[itr2,1] <- T
    } else {
      temp.df[itr2,1] <- F 
      }
}
temp.df[1,1] <- F
targets<-df[temp.df[,1],1:2]
lengTargets<-sum(temp.df[,1])
targets[,1:2]<-targets[,1:2]-1
targets[,3]<-targets[,2]/targets[,1]
directions<-unique(targets[,3])
directions

for (itr in directions) {
  for (itr2 in 1:rootNumDots) {
    for (itr3 in 1:rootNumDots) {
      if (df[itr2,2]-1 ) {
      
      }
    }
  }
}
