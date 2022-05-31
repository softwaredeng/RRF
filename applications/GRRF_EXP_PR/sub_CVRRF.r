#---GRRF
cvGRRFOOB=function(trainX,trainY,nFold,ntree,coefImpV)
{


rf = randomForest(trainX, as.factor(trainY),ntree=ntree)
imp=rf$importance;imp=imp[,"MeanDecreaseGini"];
# --- delete attributes with zero scores
FS = which(imp>0);trainX=trainX[,FS,drop=FALSE];
imp = imp[FS];#imp = log2(imp)
impRF=(imp-min(imp))/(max(imp)-min(imp));

err=NULL
for(ii in 1:length(coefImpV))
{
 coefReg=(1-coefImpV[ii])*1+coefImpV[ii]*impRF #weighted average
 rf = RRF(trainX, as.factor(trainY),flagReg=1,replace=FALSE,coefReg=coefReg,ntree=ntree)
 #imp=rf$importance;
 #imp1=imp[,"MeanDecreaseGini"];FS=which(imp1>0); #here use Gini to get the variables used in the tree; if use Accuracy, some variables are not used approriatly, so might have negative accuracy sum
 #rf=randomForest(trainX[,FS,drop=FALSE], as.factor(trainY),ntree=ntree)
 errThis = rf$err.rate; errThis=mean(errThis[,"OOB"]); 
 err = c(err,errThis)
 #err =c(err ,1-length(which(rf$predicted==trainY))/length(trainY));
}
minE = min(err)
ix = which(err==minE)
bestImpCoef = max(coefImpV[ix])

#print(coefImpV)
#print(err)

result = list(bestImpCoef=bestImpCoef)
return(result)
}


#---GRRF
cvRRF=function(trainX,trainY,nFold,ntree,coefImpV)
{
err=NULL
rowN = nrow(trainX)
allIx=sample(nrow(trainX),nrow(trainX),replace=FALSE)
trainX=trainX[allIx,];trainY=trainY[allIx];

for(i in 0:(nFold-1))
{
ratio = 2/3
N = length(trainY)
Yunique = unique(unlist(trainY))
trainIxRep = NULL; testIxRep = NULL
for(yI in 1:length(Yunique))
{
 thisY = Yunique[yI]
 ix = which(unlist(trainY)==thisY)
 L = ceiling(length(ix)*ratio)
 thisIx = sample(length(ix),L,replace=FALSE) 
 ixThisTrain = ix[thisIx]
 ixThisTest  = ix[-thisIx]
 trainIxRep = c(trainIxRep,ixThisTrain)
 testIxRep = c(testIxRep,ixThisTest) 
}

#tempIx=(ceiling(rowN/nFold*i)+1):ceiling(rowN/nFold*(i+1))
thisTestX = trainX[testIxRep,,drop=FALSE];thisTrainX = trainX[-testIxRep,,drop=FALSE]
thisTestY = trainY[testIxRep];thisTrainY = trainY[-testIxRep]

rf = randomForest(thisTrainX, as.factor(thisTrainY),ntree=ntree)
imp=rf$importance;imp=imp[,"MeanDecreaseGini"];
# --- delete attributes with zero scores
FS = which(imp>0);thisTrainX =thisTrainX[,FS,drop=FALSE];thisTestX= thisTestX[,FS,drop=FALSE]
imp = imp[FS];imp = log2(imp)
impRF=(imp-min(imp))/(max(imp)-min(imp));

errThisFold = NULL
for(ii in 1:length(coefImpV))
{
 coefReg=(1-coefImpV[ii])*1+coefImpV[ii]*impRF #weighted average
 rf = RRF(thisTrainX, as.factor(thisTrainY),flagReg=1,replace=FALSE,coefReg=coefReg,ntree=ntree)
 imp=rf$importance;
 imp1=imp[,"MeanDecreaseGini"];FS=which(imp1>0); #here use Gini to get the variables used in the tree; if use Accuracy, some variables are not used approriatly, so might have negative accuracy sum
 resF=testRF(thisTrainX[,FS,drop=FALSE],thisTrainY,thisTestX[,c(FS),drop=FALSE],thisTestY,ntree)
 errThisFold=c(errThisFold,resF$errThis);
}
err = rbind(err,errThisFold)
}

meanE = apply(err,2,mean)
minE = min(meanE)
ix = which(meanE==minE)
bestImpCoef = max(coefImpV[ix])

#print(coefImpV)
#print(err)

result = list(bestImpCoef=bestImpCoef)
return(result)
}




#RRF
cvRRF0=function(trainX,trainY,nFold,ntree,coefRegV)
{
err=NULL
rowN = nrow(trainX)
allIx=sample(nrow(trainX),nrow(trainX),replace=FALSE)
trainX=trainX[allIx,];trainY=trainY[allIx];
for(i in 0:(nFold-1))
{

tempIx=(ceiling(rowN/nFold*i)+1):ceiling(rowN/nFold*(i+1))
thisTestX = trainX[tempIx,,drop=FALSE];thisTrainX = trainX[-tempIx,,drop=FALSE]
thisTestY = trainY[tempIx];thisTrainY = trainY[-tempIx]

rf = randomForest(thisTrainX, as.factor(thisTrainY),ntree=ntree)
imp=rf$importance;imp=imp[,"MeanDecreaseGini"];
impRF=imp/(max(imp));

errThisFold = NULL
for(ii in 1:length(coefRegV))
{
 coefReg=coefRegV[ii] #weighted average
 rf = RRF(thisTrainX, as.factor(thisTrainY),flagReg=1,replace=FALSE,coefReg=coefReg,ntree=ntree)
 imp=rf$importance;
 imp1=imp[,"MeanDecreaseGini"];FS=which(imp1>0); #here use Gini to get the variables used in the tree; if use Accuracy, some variables are not used approriatly, so might have negative accuracy sum
 resF=testRF(thisTrainX[,FS,drop=FALSE],thisTrainY,thisTestX[,c(FS),drop=FALSE],thisTestY,ntree)
 errThisFold=c(errThisFold,resF$errThis);
}

err = rbind(err,errThisFold)
}

meanE = apply(err,2,mean)
minE = min(meanE)
ix = which(meanE==minE)
bestBaseCoef = max(coefRegV[ix])

#print(coefImpV)
#print(err)

result = list(bestBaseCoef=bestBaseCoef)
return(result)
}
