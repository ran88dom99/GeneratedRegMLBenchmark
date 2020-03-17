require(data.table)
tempDF<-as.data.table(tempDF)
tempDF<-unique(tempDF)
save(tempDF,file = "routWhichConstVarImp.Rdata")
load(file = "routWhichConstVarImp.Rdata")
str(tempDF)
for(i in names(tempDF)[-1]){
  tmhold<-round(tempDF[,i,with=F],digits = 4)
  tempDF[,(i):=tmhold]
}
summary(tempDF)
lastminz<-function(x){
  return(any(x[length(x)] > x))
}
identii<-function(x){
  return(all(x[1] == x))
}
for(i in names(tempDF)[3:9]){
  tmhold<-round(abs(tempDF[,i,with=F]-tempDF[,2,with=F]),digits = 4)
  tempDF[,(paste0(i,"err")):=tmhold]
}
tempDF[,eachrow:=1:dim(tempDF)[1]]

tempDF[,anyerroratmost:=sum(c(nMnEerr,nMdEerr,nZeEerr,nMoEerr,nPerEerr)),by=eachrow]
tempDF[,anyerroratmosttf:=anyerroratmost>0,by=eachrow]
tD<-tempDF
tD<-tD[(anyerroratmosttf)]

tD[,permutenotmin:=lastminz(c(nMnE,nMdE,nZeE,nMoE,nPerE)),by=eachrow]
tD[,absolutelyidenical:=identii(c(nMnE,nMdE,nZeE,nMoE,nM3E,nM5E,nPerE)),by=eachrow]
tD[,idenicalexceptpermute:=identii(c(nMnE,nMdE,nZeE,nMoE,nM3E,nM5E)),by=eachrow]
tD[,idenicalexceptextremes:=identii(c(nMnE,nMdE,nZeE,nMoE)),by=eachrow]
tD[,interactionshere:= (idenicalexceptpermute==F & idenicalexceptextremes==T), by=eachrow]
tD[,permutebigger:=lastminz(c(nMnEisqrerr,nMdEisqrerr,nZeEisqrerr,nMoEisqrerr,nPerEisqrerr)),by=eachrow]
summary(tD)
anyerror<-function(x){ return(sum((x>0.00001)))}
tD[,anyerroratall:=anyerror(c(nMnEisqrerr,nMdEisqrerr,nZeEisqrerr,nMoEisqrerr,nPerEisqrerr)),by=eachrow]

cor(tD$permutebigger,tD$permutenotmin,use = "pair")
#cor(tD$anyerroratmosttf,tD$absolutelyidenical,use = "pair")
require(corrplot)
M
corrplot(M)

View(tD)
tD[(permutenotmin<1)]
,by=org


reckeep<-vector()
for(i in 1:4000){
orig<-round(runif(100)*10)
new<-sample(orig,size = length(orig))
reckeep<-c(reckeep,cor(orig,new))
}
mean(reckeep)


reckeep<-vector()
for(i in 1:4000){
  orig<-round(runif(100)*10)
  new<-sample(orig,size = length(orig)) ^ 2
  reckeep<-c(reckeep,cor(orig,new))
}
mean(reckeep)

reckeep<-vector()
for(i in 1:4000){
  orig<-round(rnorm(100)*10)
  new<-sample(orig,size = length(orig)) 
  other<-rnorm(length(orig))
  orig<-orig*other
  reckeep<-c(reckeep,cor(orig,new))
}
mean(reckeep)

orig <- round(runif(100)*10)
new <- (orig -4)^2
cor(orig,new)

#### try out permutation vs mean and analysis of variance ####
# i do not know why this did not work just before

E<-rnorm(100)*5
a<-3
b<-1
x<-rnorm(100)
z<-rnorm(100)
y=a*x+b*z+E
cor(y,E) +
cor(y,x) +
cor(y,z)
cor(y,E)^2 +
cor(y,x)^2 +
cor(y,z)^2

xh<-mean(x)
ymx=a*xh+b*z+E
cor(y,ymx)

yo=a*xh+b*z
cor(y,yo)
xp<-sample(x,size = length(x))
yp=a*xp+b*z
cor(y,yp)
ym=b*z
cor(y,ym)

#### same thing with interactive variables ####


E<-rnorm(100)*5
a<-3
x<-rnorm(100)
z<-rnorm(100)
v<-rnorm(100)
y=a*x*z+E+v
print(paste(
cor(y,E),
cor(y,x),
cor(y,z),
cor(y,a*x*z)))
print(paste(
cor(y,E)^2,
cor(y,x)^2,
cor(y,z)^2,
cor(y,a*x*z)^2))

xh<-mean(x)
ymx=a*xh*z+E+v
print(paste(
cor(y,ymx)^2,
cor(y,a*xh*z)^2,
cor(y,E)^2))

yo=a*xh*z +v
cor(y,yo)
xp<-sample(x,size = length(x))
yp=a*xp*z +v
cor(y,yp)
ym=v
cor(y,ym)
##### next test; I have been using pearson instead of RMSE####

