require(reprex)

require(caret)
mdle<-function(daata){
  return(
    train(x = data.frame(daata[,2:length(daata[1,])]),
          y = daata[,1],
          method = "lm",
          tuneLength = 1)
  )
}
bse<-rnorm(100)
d<-data.frame(a=bse,b=bse+rnorm(100))
lmd <- mdle(d[,c(1,2)]) #model with only x
yox = predict(lmd,newdata=d[c(2)]) 


reprex()
