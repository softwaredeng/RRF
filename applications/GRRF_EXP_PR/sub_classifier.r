testF=function(trainX,trainY,testX,testY,ntree)
{
errThis=NULL;colNThis=NULL
rf=randomForest(trainX, as.factor(trainY),ntree=1000)
pred=predict(rf,testX);pred=as.character(pred)
errRF=1-sum(pred==testY)/length(pred);
errThis=c(errThis,errRF);
colNThis=c(colNThis,paste("RF",sep=""))

#result = list(colNThis=colNThis,errThis=errThis)
#return(result)

#svm1 <- svm(trainX,as.factor(trainY),type='C',kernel='linear')
#pred=predict(svm1,testX);pred=as.character(pred)
#errSVM=1-sum(pred==testY)/length(pred);
#errThis=c(errThis,errSVM);
#colNThis=c(colNThis,paste("SVM",sep=""))

trainX1 = data.frame(trainX)
trainY1 = data.frame(trainY)
trainXY1= cbind(trainX1,trainY1)
colnames(trainXY1)=c(paste("",1:ncol(trainX1),sep=""),"Y")

testX1 = data.frame(testX)
colnames(testX1)=c(paste("",1:ncol(testX1),sep=""))

T <- J48(Y ~ ., data = trainXY1)
pred=predict(T,testX1);pred=as.character(pred)
errTree=1-sum(pred==testY)/length(pred);
errThis=c(errThis,errTree);
colNThis=c(colNThis,paste("Tree",sep=""))

#if(length(unique(trainY))>2)fit1=glmnet(trainX,as.factor(trainY),family="multinomial",lambda=0)
#if(length(unique(trainY))==2)fit1=glmnet(trainX,as.factor(trainY),family="binomial",lambda=0)


result = list(colNThis=colNThis,errThis=errThis)
return(result)
}


testRF=function(trainX,trainY,testX,testY,ntree)
{
errThis=NULL;colNThis=NULL
rf=randomForest(trainX, as.factor(trainY),ntree=ntree)
pred=predict(rf,testX);pred=as.character(pred)
errRF=1-sum(pred==testY)/length(pred);
errThis=c(errThis,errRF);
colNThis=c(colNThis,paste("RF",sep=""))

result = list(colNThis=colNThis,errThis=errThis)
return(result)
}

