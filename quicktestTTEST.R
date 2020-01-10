np<-rnorm(1000,0,1)
ep<-rexp(1000)
cp<-rchisq(1000,3)
lp<-rlnorm(1000)
wp<-rweibull(1000,3) 

trg<-ep
popmean<-mean(trg)

for(cl in c(.95,.9,.7,.5)){
for(i in 2:40){#i<-2
  act<-vector(mode="logical")
  for(r in 1:10000){
    tt<-t.test(sample(trg,i),conf.level = cl)
    if(tt$conf.int[1]<popmean & tt$conf.int[2]>popmean){
      act[r]<-T
    } else {
      act[r]<-F
    }
  }
  percent<-sum(act)/length(act)
  print(paste(i,cl,percent))
}
}

#wierd effect of lowered variance drasticaly dropping interval 
##do not use strict conf intervals for this reason 
##no more than .9 
.3^3

t.test(c(1.1,3),conf.level = .9)
t.test(c(2.1,3),conf.level = .9)
t.test(c(3.1,3),conf.level = .9)

t.test(c(0.2,0,4),conf.level = .9)$co
t.test(c(2,0,4),conf.level = .9)$co
t.test(c(3.8,0,4),conf.level = .9)$co

t.test(c(0.2,0.2,0,4),conf.level = .9)$co
t.test(c(2,2,0,4),conf.level = .9)$co
t.test(c(3.8,3.8,0,4),conf.level = .9)$co

t.test(c(3,2,1),conf.level = .9)
t.test(c(,2,1),conf.level = .9)

a <- .9
g <- (-1)
t.test(c(2,g),conf.level = a)$co
t.test(c(2,2,g),conf.level = a)$co
t.test(c(2,2,2,g),conf.level = a)$co
t.test(c(2,2,2,2,g),conf.level = a)$co
t.test(c(2,2,2,2,2,g),conf.level = a)$co
t.test(c(2,2,2,2,2,2,g),conf.level = a)$co
t.test(c(2,2,2,2,2,2,2,g),conf.level = a)$co

h <- 6
t.test(c(2,h),conf.level = a)$co
t.test(c(2,2,h),conf.level = a)$co
t.test(c(2,2,2,h),conf.level = a)$co
t.test(c(2,2,2,2,h),conf.level = a)$co
t.test(c(2,2,2,2,2,h),conf.level = a)$co
t.test(c(2,2,2,2,2,2,h),conf.level = a)$co
t.test(c(2,2,2,2,2,2,2,h),conf.level = a)$co

ttm(c(2,h),2,cl = a)
ttm(c(2,2,h),2,cl = a)
ttm(c(2,2,2,h),2,cl = a)
ttm(c(2,2,2,2,h),2,cl = a)
ttm(c(2,2,2,2,2,h),2,cl = a)
ttm(c(2,2,2,2,2,2,h),2,cl = a)
ttm(c(2,2,2,2,2,2,2,h),2,cl = a)

x<-c(2,2,2,6)
psd<-3

ttm <- function(x,psd,cl=.95,u_l="u",dsda=10){
  if(cl>=1 | cl<=0) warning("confidence level out of bounds")
  if(!is.vector(x)) warning("x not a vector")
  n <- length(x)
  if( n < 1 ) warning("x has length 0")
  nsd <- sd(x) * min( n / dsda , 1) + psd * max((1 - n / dsda) , 0)
  ttt <- qt((1-(1-cl)/2),df = (n-1))
  entrvl <- ttt * nsd / sqrt(n)
  if(u_l=="u"){  return( mean(x) + entrvl) }
  if(u_l=="l"){  return( mean(x) - entrvl) }
}

uDF[,paste0(i,"u") := tt1(get(i),o=xo[1],coi = cl)$conf.int[2],  by=keyOneRecipe]
uDF[,paste0(i,"l") := tt1(get(i),o=xo[2],coi = cl)$conf.int[1],  by=keyOneRecipe] 

cl<-.5
h<-c(.5)
a<-1;b<-3

if(T){
df<-data.frame()
for(a in -1:7){
  for(b in 1:24){
  tto<-t.test(c(sample(a,b,replace = T),h),conf.level = cl)$co
  df<-rbind(df,data.frame(mode=as.numeric(a),reps=as.numeric(b),lower=tto[1],upper=tto[2]))
  }
}

ggplot(df, aes(mode, reps, fill= lower)) + 
  geom_tile() + theme_dark()  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_gradient2(midpoint = 2, high = "blue")
}

require(ggraptR)
#ggraptR()
a<-.95;n<-195
 percentile of the t distribution with N - 1 degrees of freedom

 eh<-(1-(1-a)/2)
 eh<-.975


t1-0.025,N-1                 =   1.9723

ttm <- function(x,a=.95,u_l="u"){
n <- length(x)
ttt <- qt((1-(1-a)/2),df = (n-1))
entrvl <- ttt * sqrt(n) * sd(x)
mean(x) +entrvl
}



##### and here we test MCMD multi criteria decision making #####
require(FuzzyMCDM)
d <- matrix(c(0.63,0.42,0.63,0.67,0.8,0.59,0.8,0.84,0.92,0.75,0.92,0.92,0.29,0.71,0.75,
              0.42,0.46,0.88,0.92,0.59,0.63,1,1,0.71,0.75,0.59,0.42,0.42,0.92,0.75,0.58,0.59,1,0.88,
              0.76,0.75,0.59,0.71,0.42,0.33,0.75,0.88,0.58,0.51,0.88,0.96,0.71,0.67,0.5,0.67,0.67,
              0.67,0.67,0.84,0.84,0.84,0.84,0.92,0.96,0.96,0.67,0.54,0.54,0.25,0.84,0.71,0.71,0.42,
              0.96,0.88,0.88,0.59,0.67,0.71,0.42,0.25,0.84,0.88,0.59,0.42,0.96,0.96,0.75,0.58,0.54,
              0.625,0.625,0.295,0.705,0.79,0.795,0.46,0.88,0.92,0.875,0.62),nrow=4,ncol=24)
w <- c(1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24,
       1/24,1/24,1/24,1/24,1/24,1/24,1/24,1/24)
cb <- c('max','max','max','max','max','max','max','max')
FuzzyMMOORA(d,w,cb)
https://arxiv.org/ftp/arxiv/papers/1401/1401.4590.pdf
https://stats.stackexchange.com/questions/154888/combining-multiple-metrics-to-provide-comparisons-ranking-of-k-objects-question