library(foreign)
library(randomForest)
grass<-read.dbf('NessVegClass.dbf')
use<-grass[!is.na(grass$bl_n),]
classes<-as.factor(use$Non_GDE)
predictors<-use[,c(10,11,19,20,28,29,37,38,46,47,55,56,64,65,73,74,82,83)]
rf<-randomForest(predictors,classes,proximity = T)
mp<-barplot(importance(rf),beside=T,axisnames=F,las=2)
axis(1, at=mp, labels=row.names(importance(rf)),las=2)

#The following came from Torsten Hothorn - http://r.789695.n4.nabble.com/Re-Fwd-Re-Party-extract-BinaryTree-from-cforest-td3878100.html

#**************************
#return the rules of a tree
#**************************
getConds<-function(tree){
  #store all conditions into a list
  conds<-list()
  #start by the terminal nodes and find previous conditions
  id.leafs<-which(tree$status==-1)
  j<-0
  for(i in id.leafs){
    j<-j+1
    prevConds<-prevCond(tree,i)
    conds[[j]]<-prevConds$cond
    while(prevConds$id>1){
      prevConds<-prevCond(tree,prevConds$id)
      conds[[j]]<-paste(conds[[j]]," & ",prevConds$cond)
      if(prevConds$id==1){
        conds[[j]]<-paste(conds[[j]]," => ",tree$prediction[i])
        break()
      }
    }
    
  }
  
  return(conds)
}

#**************************
#find the previous conditions in the tree
#**************************
prevCond<-function(tree,i){
  if(i %in% tree$right_daughter){
    id<-which(tree$right_daughter==i)
    cond<-paste(tree$split_var[id],">",tree$split_point[id])
  }
  if(i %in% tree$left_daughter){
    id<-which(tree$left_daughter==i)
    cond<-paste(tree$split_var[id],"<",tree$split_point[id])
  }
  
  return(list(cond=cond,id=id))
}

#remove spaces in a word
collapse<-function(x){
  x<-sub(" ","_",x)
  
  return(x)
}

tree<-getTree(rf, k=1, labelVar=TRUE)
#rename the name of the column
colnames(tree)<-sapply(colnames(tree),collapse)
rules<-getConds(tree)
print(rules)

library(party)
py<-cforest(classes ~ ., data = predictors)

pt <- party:::prettytree(py@ensemble[[1]], names(py@data@get("input"))) 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- py@data 
nt@responses <- py@responses 
plot(nt) 
