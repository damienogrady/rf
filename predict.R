library(foreign)
library(randomForest)

grass<-read.dbf('bigtrees.dbf')
use<-grass[,c(8,9,17,18,26,27,35,36,44,45,53,54,62,63,71,72,80,81)]
class<-predict(rf,use)
out<-data.frame(cat=grass$cat,class=as.integer(as.character(class)))
write.dbf(out,'class.dbf')
