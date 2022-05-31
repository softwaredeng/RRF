rm(list=ls(all=TRUE))
#set.seed(82)
library(RRF);library(randomForest)
library(varSelRF)
source('sub_classifier.r')
graphics.off()
library(RWeka);

set.seed(1)

nRep=100
ntree=1000

listFea = list(c("a","b"));listFea=listFea[-1]

coefImp= c(0.7,0.6,0.5,0.4,0.3,0.2,0.1,0)
coefRegV=c(1)

dimData=NULL;nCls=NULL;
colN=NULL;err=NULL;fN=NULL;time=NULL;count=1;

dataV=c("adenocarcinoma","brain","breast.2.class",
"breast.3.class","colon","leukemia","lymphoma","nci","prostate",
"srbct")

FFCM = NULL;FFM=NULL; FCM=NULL

for(dataI in 1:length(dataV)){
thisData=dataV[dataI]

print(thisData);print(Sys.time())

path=paste(getwd(), "/data2/",thisData,".class.txt",sep=""); #musk vehicle is good austra
Y=read.table(path,header=FALSE,sep = "	",na.strings = c("?"))
path=paste(getwd(), "/data2/",thisData,".data.txt",sep=""); #musk vehicle is good austra
X=read.table(path,header=FALSE,skip=1,row.names=1,sep = "	",na.strings = c("?"))

X=t(X);Y=t(Y);

print(table(Y))

print(dim(X));print(length(Y))
if(nrow(X)!=length(Y))stop("stop")

cR = nrow(X)
cN = ncol(X)+1
dimData=rbind(dimData,dim(X))
nCls=rbind(nCls,length(unique(Y)))


#Orig RF and calculate the importance
rf = randomForest(X, as.factor(Y),ntree=ntree)
imp=rf$importance;imp=imp[,"MeanDecreaseGini"]; #accuracy is a better important score

FS=which(imp>0);
impRF=imp/(max(imp));

corV = NULL
FF=NULL; FC=NULL; FFC = NULL
for(k in 1:length(coefRegV)){
kReg=coefRegV[k]
for(j in 1:length(coefImp)){#---<1>
jImp=coefImp[j] #change k to j, if crossproduct; currently map 1-1

coefReg=(1-jImp)*kReg+jImp*impRF #weighted average
baseNameThis = paste("Reg",kReg,"_Imp",jImp,"_",thisData,sep="")

#RRF use importance
rf = RRF(X, as.factor(Y),flagReg=1,replace=FALSE,coefReg=coefReg,ntree=ntree)

imp=rf$importance;
imp1=imp[,"MeanDecreaseGini"];FS=which(imp1>0); #here use Gini to get the variables used in the tree; if use Accuracy, some variables are not used approriatly, so might have negative accuracy sum

Y1 = as.numeric(unlist(data.frame(Y)))
Y1[Y1==1]=-1;Y1[Y1==2]=1;
tmp = cbind(X[,FS,drop=FALSE])
corFF = abs(cor(tmp))
corFF = corFF - diag(ncol(corFF))
#corFFAve = mean(corFF)
top10 = NULL
for(iii in 1:ncol(corFF))
{
 tmp = corFF[,iii]; tmp1 = order(tmp,decreasing=TRUE)
 tmp1=tmp1[1:5]; 
 top10 = c(top10,tmp[tmp1])
}
corFFAve = mean(top10)
FF = c(FF,corFFAve)

tmp = cbind(X[,FS,drop=FALSE])
corFC = abs(cor(tmp,Y1))
corFCAve = mean(corFC)

#corV=c(corV,mean(corFS))
#corV=c(corV,mean(apply(corFS,1,max)))

#FF = c(FF,corFFAve)
FC = c(FC,corFCAve)

#K = length(FS)
#FFC = c(FFC, K*corFCAve/sqrt(K+K*(K-1)*corFFAve)  )


#colN=c(colN,paste(baseNameThis,"_",sep=""))

}
}

thisX = X
for(iii in ncol(thisX):1)
{
if( length(unique(thisX[,iii]))==1)
{ thisX = thisX[,-iii];}
}

corX = abs(cor(thisX))
corX = corX - diag(ncol(corX))
top10 = NULL
for(iii in 1:ncol(corX))
{
 tmp = corX[,iii]; tmp1 = order(tmp,decreasing=TRUE)
 tmp1=tmp1[1:5]; 
 top10 = c(top10,tmp[tmp1])
}
corFFAve = mean(top10)
FF = c(FF,corFFAve)

#corFC = abs(cor(thisX,Y1))
#corFCAve = mean(corFC)
#FC = c(FC,corFCAve)

#K = ncol(thisX)
#FFC = c(FFC, K*corFCAve/sqrt(K+K*(K-1)*corFFAve))

FFM = rbind(FFM,FF)
#FCM = rbind(FCM,FC)
#FFCM = rbind(FFCM,FFC)

print(FFM)
#print(FCM)
#print(FFCM)
}

plot(FFM[1,],type="l",lwd=2,col="red",xaxt='n',
xlab="gamma",ylab="average correlation of 5 most similar features",
main="correlation related to GRRF parameter: gamma",cex=2,
cex.lab=1.5)
for(ii in 1:4)
{
 lines(FFM[ii,],type="l",lwd=2,col="red")
}
axis(side=1,at=1:(length(coefImp)+1),labels=paste(c(coefImp,"All Fea")))


timeStamp=gsub(" ", "_", Sys.time())   #(or can use Sys.Date())
timeStamp=gsub(":", "-", timeStamp)
timeStamp=paste("Correlation_Analysis","_ntree",ntree,"_",timeStamp, ".RData",sep="");
save.image(timeStamp)

#------------

#----write 
#all = cbind(err,fN)
#rownames(all)=colN
#pathOut=paste(getwd(), "/result/","Correlation_Analysis",sep="");
#write.table(all, file=pathOut,quote=FALSE,row.names=TRUE,col.names=TRUE,sep=",")
#------------------------

