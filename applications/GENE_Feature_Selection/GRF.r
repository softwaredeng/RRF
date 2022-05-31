# Technical details can be found in the following papers
# Houtao Deng, "Guided Random Forest in the RRF Package", arXiv:1306.0237, 2013.
# Houtao Deng, George Runger, "Gene Selection with Guided Regularized Random Forest", Pattern Recognition, 46.12 (2013): 3483-3489
# Houtao Deng, George Runger, "Feature Selection via Regularized Trees", The 2012 International Joint Conference on Neural Networks (IJCNN), IEEE, 2012. 

rm(list=ls(all=TRUE))
library(RRF);
set.seed(1)

nRep <- 100
nTree <- 1000

dataV <- c("adenocarcinoma","brain","breast.2.class",
"breast.3.class","colon","leukemia","lymphoma","nci","prostate",
"srbct")

eAll <- NULL; fAll <- NULL

for(dataI in 1:length(dataV)){
thisData <- dataV[dataI]

print(thisData); print(Sys.time())

# data
path <- paste(getwd(), "/data/",thisData,".class.txt",sep="");
Y<-read.table(path,header=FALSE,sep = "	",na.strings = c("?"))
path <- paste(getwd(), "/data/",thisData,".data.txt",sep="");
X <- read.table(path,header=FALSE,skip=1,row.names=1,sep = "	",na.strings = c("?"))

X <- t(X);Y <- t(Y);

# index of training and testing data. 2/3 for training and 1/3 for testing. 100 replicates 
pathWrite <- paste(getwd(), "/ixRep100",sep=""); #musk vehicle is good austra
pathIxTrain <- paste(pathWrite,"/", thisData,"Train",sep="")
pathIxTest <- paste(pathWrite,"/", thisData,"Test",sep="")

ixTrain <- read.table (pathIxTrain,header=FALSE,sep="," );
ixTest <- read.table (pathIxTest,header=FALSE,sep=",");

for(foldI in 1:nRep) 
{
trainIxThis <- as.numeric(ixTrain[foldI,])
testIxThis <- as.numeric(ixTest[foldI,])

testX <- X[testIxThis,];trainX <- X[trainIxThis,]
testY <- Y[testIxThis];trainY <- Y[trainIxThis]

trainY <- paste("class",trainY,sep="")
testY <- paste("class",testY,sep="")

#Orig RF and calculate the importance
rf <- RRF(trainX, as.factor(trainY),flagReg=0,ntree=nTree) 
imp <- rf$importance;
imp <- imp[,"MeanDecreaseGini"]; 
impNORM <- imp/(max(imp)) #normalize the importance scores into [0,1]
fsRF <- length(rf$feaSet)

pred <- predict(rf,testX);pred=as.character(pred)
errRF <- 1-sum(pred==testY)/length(pred);

# --- Guided Regularized Random Forest (GRRF) with gamma = 0.1
gamma <- 0.1
coefReg <- (1-gamma) + gamma*impNORM
GRRF <- RRF(trainX, as.factor(trainY), flagReg = 1,ntree=nTree,coefReg=coefReg) 
imp <- GRRF$importance;imp=imp[,"MeanDecreaseGini"] 
pred <- as.character(predict(GRRF,testX));
errGRRF <- 1-sum(pred==testY)/length(pred); # error of GRRF
fsGRRF  <- length(GRRF$feaSet);

# apply RF to the feature subset selected by GRRF
rf <- RRF(trainX[,GRRF$feaSet,drop=FALSE],flagReg=0,as.factor(trainY),ntree=nTree) 
pred <- as.character(predict(rf,testX[,GRRF$feaSet,drop=FALSE]));
errGRRF_RF<-1-sum(pred==testY)/length(pred);

# --- Guided Random Forest (GRF) with gamma = 1, therefore coefReg is equivalent to the normalized score impNORM
GRF <- RRF(trainX, as.factor(trainY), flagReg = 0,ntree=nTree,coefReg=impNORM) 
pred <- as.character(predict(GRF,testX));
errGRF <- 1-sum(pred==testY)/length(pred);
fsGRF <-length(GRF$feaSet);

# apply RF to the feature subset selected by GRF
rf <- RRF(trainX[,GRF$feaSet,drop=FALSE],flagReg=0,as.factor(trainY),ntree=nTree) 
pred <- as.character(predict(rf,testX[,GRF$feaSet,drop=FALSE]));
errGRF_RF<-1-sum(pred==testY)/length(pred);

eAll <- rbind(eAll,c(errGRF,errGRF_RF, errRF, errGRRF,errGRRF_RF))
fAll <- rbind(fAll,c(fsGRF, fsRF, fsGRRF))
}
}

colnames(eAll) <- c("GRF","GRF_RF","RF","GRRF", "GRRF_RF")
colnames(fAll) <- c("GRF","RF", "GRRF")


# average error/#features for each data set
meanErrMat <- NULL
meanFeaMat <- NULL
nRep <- nrow(eAll)/length(dataV)
for(I in 1:length(dataV))
{
thisE <- eAll[((I-1)*nRep+1):(I*nRep),]
thisF <- fAll[((I-1)*nRep+1):(I*nRep),]
meanErrMat <- rbind(meanErrMat,apply(thisE,2,mean))
meanFeaMat <- rbind(meanFeaMat,apply(thisF,2,mean))
}
rownames(meanErrMat) <- dataV
rownames(meanFeaMat) <- dataV

print(meanErrMat)
print(meanFeaMat)

