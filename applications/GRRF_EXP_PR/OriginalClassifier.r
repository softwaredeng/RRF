rm(list=ls(all=TRUE))
#set.seed(82)
library(RWeka);library(randomForest)
library(varSelRF)
graphics.off()
library('e1071')
set.seed(1)
source('sub_classifier.r')

nRep=100
ntree=1000

dimData=NULL;nCls=NULL;
colN=NULL;err=NULL;fN=NULL;time=NULL;count=1;

dataV=c("colon.data","leukemia_train_38x7129.data","prostate.data")
dataV=c("adenocarcinoma","brain","breast.2.class",
"breast.3.class","colon","leukemia","lymphoma","nci","prostate",
"srbct")

#dataV=c("srbct")

for(dataI in 1:length(dataV)){
thisData=dataV[dataI]

print(thisData);print(Sys.time())

path=paste(getwd(), "/data2/",thisData,".class.txt",sep=""); #musk vehicle is good austra
Y=read.table(path,header=FALSE,sep = "	",na.strings = c("?"))
path=paste(getwd(), "/data2/",thisData,".data.txt",sep=""); #musk vehicle is good austra
X=read.table(path,header=FALSE,skip=1,row.names=1,sep = "	",na.strings = c("?"))

X=t(X);Y=t(Y);

print(dim(X));print(length(Y))
#if(nrow(X)!=length(Y))stop("stop")

cR = nrow(X)
cN = ncol(X)+1
dimData=rbind(dimData,dim(X))
nCls=rbind(nCls,length(unique(Y)))

pathWrite=paste(getwd(), "/Sample","Rep",nRep,"",sep=""); #musk vehicle is good austra
pathIxTrain = paste(pathWrite,"/", thisData,"Train",sep="")
pathIxTest = paste(pathWrite,"/", thisData,"Test",sep="")

ixTrain <- read.table (pathIxTrain,header=FALSE,sep="," );
ixTest <- read.table (pathIxTest,header=FALSE,sep=",");


for(foldI in 1:nRep)#nRep
{
errTempData=NULL
err1=NULL

trainIxThis = as.numeric(ixTrain[foldI,])
testIxThis = as.numeric(ixTest[foldI,])

testX = X[testIxThis,];trainX = X[trainIxThis,]
testY = Y[testIxThis];trainY = Y[trainIxThis]

trainY = paste("class",trainY,sep="")
testY = paste("class",testY,sep="")

resF=testF(trainX,trainY,testX,testY,ntree)

err=c(err,resF$errThis);
colN=c(colN,paste(resF$colNThis,"_","sample",foldI,"_",thisData,sep=""))
fN=c(fN,matrix(ncol(trainX),1,length(resF$errThis)));

}
}


timeStamp=gsub(" ", "_", Sys.time())   #(or can use Sys.Date())
timeStamp=gsub(":", "-", timeStamp)
timeStamp=paste("OriginalClassifier","_Rep",nRep,"_ntree",ntree,"_",timeStamp, ".RData",sep="");
save.image(timeStamp)

#----write 
all = cbind(err,fN)
rownames(all)=colN
pathOut=paste(getwd(), "/result/","Rep",nRep,"Original",sep="");
write.table(all, file=pathOut,quote=FALSE,row.names=TRUE,col.names=TRUE,sep=",")


#------------

a=paste("Tree",sep="")
c=regexpr(a,colN)
d=which(c>0)
colN[d]
err[d]
fN[d]
mean(err[d])


Mat = NULL;MatRN=NULL

for(thisD in dataV)
{
print(thisD)
for(k in 1:length(sdV)){
kSd=sdV[k]
for(repI in 1:nRep){
V=NULL;matColN=NULL
thisRegImpRep=paste("sd",kSd,"_rep",repI,"_",sep="")

print(thisRegImpRep)

thisRegImpRepIx=regexpr(thisRegImpRep,colN)
thisDIx=regexpr(thisD,colN);

MatRN=c(MatRN,paste(thisD,"_",thisRegImpRep,sep=""))

temp=which(thisRegImpRepIx>0&thisDIx>0);
mean(err[temp]);
V=c(V,mean(err[temp]),mean(fN[temp]),mean(time[temp]));
matColN=c(matColN,"errRFE","feaRFE","timeRFE")

Mat=rbind(Mat,V)
}}}

colnames(Mat)=matColN
rownames(Mat)=MatRN
ix=order(matColN)
Mat=Mat[,ix]

save.image(paste(timeStamp, "ProcessVer1.RData",sep=""))

#-------------

#only use the following parameter for showing the paper
showCoefRegV = c(0.80,0.85,0.9,1.00)
showCoefImp = c(0.05,0,0.05,0.10)

#----std and mean of CV

#CV 
rowV = rownames(Mat)
newRow=NULL
MatM=NULL;MatS=NULL
for(thisD in dataV){
thisDIx=regexpr(thisD,rowV);
for(k in 1:length(showCoefRegV)){
kReg=showCoefRegV[k]
jImp=showCoefImp[k]
thisRep=paste("Reg",kReg,"_Imp",jImp,"_",sep="")
newRow=c(newRow,paste(thisD,thisRep,sep=""))
thisRepIx=regexpr(thisRep,rowV);
temp=which(thisDIx>0&thisRepIx>0)
tempD = Mat[temp,]
MatM=rbind(MatM,apply(tempD,2,mean))
MatS=rbind(MatS,(apply(tempD,2,sd))/sqrt(nRep) )
}}
rownames(MatS)=newRow
rownames(MatM)=newRow

#---delete unwanted columns for show
delCol = c("All")
delIx = which(regexpr(delCol, colnames(MatM))>0)
MatM = MatM[,-delIx];MatS = MatS[,-delIx]

# this transfer different parameters from multiple rows to one row for each data
rowV = rownames(MatM)
MatM1 = NULL;MatS1 = NULL
for(thisD in dataV)
{
 thisDIx=regexpr(thisD,rowV);
 thisM = NULL;thisS = NULL
 newColName=NULL
 for(k in 1:length(showCoefRegV)){
  kReg=showCoefRegV[k]
  jImp=showCoefImp[k]
  thisRep=paste("Reg",kReg,"_Imp",jImp,"_",sep="")
  newColName = c(newColName,paste(colnames(MatM),thisRep,sep="")) 
  thisRepIx=regexpr(thisRep,rowV); #only the parameters of interest
  temp=which(thisDIx>0&thisRepIx>0)
  thisM=c(thisM,MatM[temp,])
  thisS=c(thisS,MatS[temp,])
 }
 MatM1=rbind(MatM1,thisM)
 MatS1=rbind(MatS1,thisS)
}
rownames(MatM1)=dataV;colnames(MatM1)=newColName 
rownames(MatS1)=dataV;colnames(MatS1)=newColName 
ix = order(newColName)
MatM1=MatM1[,ix];MatS1=MatS1[,ix]

# delete the columns which replicate because RFE, Orig are the same for different coefficient
delCol = NULL
newColName=colnames(MatM1)
dupStr = c("Orig","RFE")
typeStr = c("err","fea","time")
for(strI in dupStr)
{
 for(typeI in typeStr)
 {
  temp1=regexpr(strI,newColName);
  temp2=regexpr(typeI,newColName);
  temp=which(temp1>0&temp2>0)
  temp = temp[-1]
  if(length(temp)>0)delCol=c(delCol,temp)  
 }
}
MatM2=MatM1[,-delCol]
MatS2=MatS1[,-delCol]

apply(MatM2,2,mean)

write.table(MatM2, file="tempR.txt",quote=FALSE,row.names=TRUE,col.names=TRUE,sep="\t")
write.table(MatS2, file="tempS.txt",quote=FALSE,row.names=TRUE,col.names=TRUE,sep="\t")


#transformed to the form "mean(std)"
startIx = 6
MatM2[,1:startIx]=round(MatM2[,1:startIx],3);MatS2[,1:startIx]=round(MatS2[,1:startIx],4)
MatM2[,(startIx+1):ncol(MatM2)]=round(MatM2[,(startIx+1):ncol(MatM2)],1);MatS2[,(startIx+1):ncol(MatS2)]=round(MatS2[,(startIx+1):ncol(MatS2)],2)
output = matrix("",nrow(MatM2),ncol(MatM2))
for(ii in 1:nrow(MatM2))
{
for(iii in 1:ncol(MatM2))
{
 output[ii,iii]=paste(MatM2[ii,iii],"(",MatS2[ii,iii],")",sep="")
}
}
output=data.frame(output)
rownames(output)=dataV
colnames(output)=colnames(MatM2)

write.table(output, file="tempR1.txt",quote=FALSE,row.names=TRUE,col.names=TRUE,sep="\t")



#------------work above
# below is not used


newColName=colnames(MatM2)
typeStr = c("err","fea","time")
typeStr = "time"
thisM = MatM2
for(typeI in typeStr)
 {
  temp2=regexpr(typeI,newColName);
  temp=which(temp2>0)
  thisM1 =thisM[,temp]
 }


MatErrAll = MatM[,"errAll"]
MatErrAll = matrix(MatErrAll,ncol=length(coefRegV))
colnames(MatErrAll)=coefRegV;rownames(MatErrAll)=coefImp;
x11();plot(coefRegV,MatErrAll[1,],type="o",ylim=c(0,0.5),pch=pch[1],lty=lty[1],col=col[1],
,xlab="coefficient of regularization",ylab="error rate",cex.lab=sz,cex.axis=sz,cex=sz)
for(i in 2:length(coefImp))lines(coefRegV,MatErrAll[i,],type="o",pch=pch[i],lty=lty[i],col=col[i],cex=sz)
legend("topright",legend=leg, col=col, pch=pch,lty=lty, merge=TRUE,cex=sz)


rowV = rownames(Mat)
MatM=NULL;MatStd=NULL
for(thisD in dataV)
{
 temp=regexpr(thisD,rowV);temp=which(temp>0)
 tempD = Mat[temp,]
 MatM=rbind(MatM,apply(tempD,2,mean))
 MatStd=rbind(MatStd,(apply(tempD,2,sd))/sqrt(nRep) )
}
rownames(MatStd)=dataV
rownames(MatM)=dataV

apply(MatM,2,mean)




