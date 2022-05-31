rm(list=ls(all=TRUE))
#set.seed(82)
library(RRF);library(randomForest)
library(varSelRF)
source('sub_classifier.r')
graphics.off()
library(RWeka);

set.seed(1)

ntree=1000

dimData=NULL;nCls=NULL;
colN=NULL;err=NULL;fN=NULL;time=NULL;count=1;

dataV=c("adenocarcinoma","brain","breast.2.class",
"breast.3.class","colon","leukemia","lymphoma","nci","prostate",
"srbct")

thisTimeM = NULL

for(dataI in 1:length(dataV)){
thisData=dataV[dataI]

thisTimeV = NULL

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

ptm <- proc.time()
rf = randomForest(X, as.factor(Y),ntree=ntree)
tempTime = proc.time() - ptm; 
thisTimeV =c(thisTimeV,tempTime[3])

imp=rf$importance;imp=imp[,"MeanDecreaseGini"]; #accuracy is a better important score
impRF=imp/(max(imp));

bestImpCoef = 0.2
coefReg=(1-bestImpCoef)+bestImpCoef*impRF#weighted average
grrf <- RRF(X,as.factor(Y),flagReg=1,replace=FALSE,coefReg=coefReg,ntree=ntree)

tempTime = proc.time() - ptm; 
thisTimeV =c(thisTimeV,tempTime[3])


ptm <- proc.time()
RFE=varSelRF(X, as.factor(Y), c.sd = 1, mtryFactor = 1, ntree = ntree,
ntreeIterat = 2000, vars.drop.num = NULL, vars.drop.frac = 0.2,
whole.range = TRUE, recompute.var.imp = FALSE, verbose = FALSE,
returnFirstForest = TRUE, fitted.rf = NULL, keep.forest = FALSE)

tempTime = proc.time() - ptm; 
thisTimeV =c(thisTimeV,tempTime[3])

thisTimeM = rbind(thisTimeM,thisTimeV)

print(thisTimeM)
}

L = NULL
for(i in 1:length(dataV))L=c(L,"",dataV[i],"")

rownames(thisTimeM)=  dataV 
# Graph autos with adjacent bars using rainbow colors
myplot=barplot(as.matrix(t(thisTimeM)), main="", ylab= "Time(sec.)",
   beside=TRUE, col=rainbow(3),xaxt='n',cex.lab=1.5)
text(myplot, par("usr")[3], labels=L, srt=45, offset=3, adj=1, xpd=TRUE,cex=1) 
legend("top", c("RF","GRRF","varSelRF"), cex=1.2, 
   bty="n", fill=rainbow(3));




timeStamp=gsub(" ", "_", Sys.time())   #(or can use Sys.Date())
timeStamp=gsub(":", "-", timeStamp)
timeStamp=paste("Time_Analysis","_ntree",ntree,"_",timeStamp, ".RData",sep="");
save.image(timeStamp)

#------------

#----write 
all = cbind(err,fN)
rownames(all)=colN
pathOut=paste(getwd(), "/result/","Time_Analysis",sep="");
write.table(all, file=pathOut,quote=FALSE,row.names=TRUE,col.names=TRUE,sep=",")
#------------------------

