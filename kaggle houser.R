data.source<-as.data.frame(read.csv("kaggle train.csv", sep = ",",fill=TRUE, header = T,quote="",dec="."))
dummies <- dummyVars(SalePrice ~ ., data = data.source)
data.source2<-data.frame(data.source,predict(dummies, newdata = data.source))
head(data.source2)
data.source3<-data.frame(data.source2[,81:370])
data.source3[,1]<-log(data.source3[,1]) 
meannie<-mean(data.source3[,1])
RMSE.mean=RMSE(data.source3[,1],meannie)
1-.1/RMSE.mean
write.table(data.source3,
            file = paste("kaggle housing",".csv",sep=""), append =F, quote = F, sep = ",",
            eol = "\n", na = "", dec = ".", row.names = F,
            col.names = F, qmethod = "double")
