pkgname <- "RRF"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "RRF-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('RRF')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("MDSplot")
### * MDSplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MDSplot
### Title: Multi-dimensional Scaling Plot of Proximity matrix from RRF
### Aliases: MDSplot
### Keywords: classif tree

### ** Examples

set.seed(1)
data(iris)
iris.rf <- RRF(Species ~ ., iris, proximity=TRUE,
                        keep.forest=FALSE)
MDSplot(iris.rf, iris$Species)
## Using different symbols for the classes:
MDSplot(iris.rf, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MDSplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RRF")
### * RRF

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RRF
### Title: Feature Selection with Regularized Random Forest
### Aliases: RRF RRF.formula RRF.default print.RRF

### ** Examples

#-----Example 1 -----
library(RRF);set.seed(1)

#only the first feature and last feature are truly useful
X <- matrix(runif(50*50), ncol=50)
class <- (X[,1])^2 + (X[,50])^2  
class[class>median(class)] <- 1;
class[class<=median(class)] <- 0

#ordinary random forest. 
rf <- RRF(X,as.factor(class), flagReg = 0)
impRF <- rf$importance
impRF <- impRF[,"MeanDecreaseGini"]
rf$feaSet

#regularized random forest
rrf <- RRF(X,as.factor(class), flagReg = 1)
rrf$feaSet

#guided regularized random forest
imp <- impRF/(max(impRF))#normalize the importance score
gamma <- 0.5
coefReg <- (1-gamma)+gamma*imp #weighted average
grrf <- RRF(X,as.factor(class),coefReg=coefReg, flagReg=1)
grrf$feaSet

#guided random forest
gamma <- 1
coefReg <- (1-gamma)+gamma*imp 
grf <- RRF(X,as.factor(class),coefReg=coefReg, flagReg=0)
grf$feaSet

#-----Example 2 XOR learning-----
#only the first 3 features are needed
#and each individual feature is not useful
#bSample <- sample(0:1,20000,replace=TRUE)
#X <- matrix(bSample,ncol=40)
#class <- xor(xor(X[,1],X[,2]),X[,3])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RRF", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("classCenter")
### * classCenter

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: classCenter
### Title: Prototypes of groups.
### Aliases: classCenter
### Keywords: classif

### ** Examples

data(iris)
iris.rf <- RRF(iris[,-5], iris[,5], prox=TRUE)
iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox)
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
     main="Iris Data with Prototypes")
points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("classCenter", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("combine")
### * combine

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: combine
### Title: Combine Ensembles of Trees
### Aliases: combine
### Keywords: regression classif

### ** Examples

data(iris)
rf1 <- RRF(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf2 <- RRF(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf3 <- RRF(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf.all <- combine(rf1, rf2, rf3)
print(rf.all)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("combine", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getTree")
### * getTree

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getTree
### Title: Extract a single tree from a forest.
### Aliases: getTree
### Keywords: tree

### ** Examples

data(iris)
## Look at the third trees in the forest.
getTree(RRF(iris[,-5], iris[,5], ntree=10), 3, labelVar=TRUE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getTree", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("grow")
### * grow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: grow
### Title: Add trees to an ensemble
### Aliases: grow grow.default grow.RRF
### Keywords: regression classif

### ** Examples

data(iris)
iris.rf <- RRF(Species ~ ., iris, ntree=50, norm.votes=FALSE)
iris.rf <- grow(iris.rf, 50)
print(iris.rf)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("grow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("importance")
### * importance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: importance
### Title: Extract variable importance measure
### Aliases: importance importance.default importance.RRF
### Keywords: regression classif tree

### ** Examples

set.seed(4543)
data(mtcars)
mtcars.rf <- RRF(mpg ~ ., data=mtcars, ntree=1000,
                          keep.forest=FALSE, importance=TRUE)
importance(mtcars.rf)
importance(mtcars.rf, type=1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("importance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("imports85")
### * imports85

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: imports85
### Title: The Automobile Data
### Aliases: imports85
### Keywords: datasets

### ** Examples

data(imports85)
imp85 <- imports85[,-2]  # Too many NAs in normalizedLosses.
imp85 <- imp85[complete.cases(imp85), ]
## Drop empty levels for factors.
imp85[] <- lapply(imp85, function(x) if (is.factor(x)) x[, drop=TRUE] else x)

stopifnot(require(RRF))
price.rf <- RRF(price ~ ., imp85, do.trace=10, ntree=100)
print(price.rf)
numDoors.rf <- RRF(numOfDoors ~ ., imp85, do.trace=10, ntree=100)
print(numDoors.rf)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("imports85", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("margin")
### * margin

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: margin
### Title: Margins of RRF Classifier
### Aliases: margin margin.default margin.RRF plot.margin
### Keywords: classif

### ** Examples

set.seed(1)
data(iris)
iris.rf <- RRF(Species ~ ., iris, keep.forest=FALSE)
plot(margin(iris.rf))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("margin", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("na.roughfix")
### * na.roughfix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: na.roughfix
### Title: Rough Imputation of Missing Values
### Aliases: na.roughfix na.roughfix.default na.roughfix.data.frame
### Keywords: NA

### ** Examples

data(iris)
iris.na <- iris
set.seed(111)
## artificially drop some data values.
for (i in 1:4) iris.na[sample(150, sample(20)), i] <- NA
iris.roughfix <- na.roughfix(iris.na)
iris.narf <- RRF(Species ~ ., iris.na, na.action=na.roughfix)
print(iris.narf)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("na.roughfix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("outlier")
### * outlier

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: outlier
### Title: Compute outlying measures
### Aliases: outlier outlier.RRF outlier.default
### Keywords: classif

### ** Examples

set.seed(1)
iris.rf <- RRF(iris[,-5], iris[,5], proximity=TRUE)
plot(outlier(iris.rf), type="h",
     col=c("red", "green", "blue")[as.numeric(iris$Species)])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("outlier", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("partialPlot")
### * partialPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: partialPlot
### Title: Partial dependence plot
### Aliases: partialPlot partialPlot.default partialPlot.RRF
### Keywords: classif regression tree

### ** Examples

data(airquality)
airquality <- na.omit(airquality)
set.seed(131)
ozone.rf <- RRF(Ozone ~ ., airquality)
partialPlot(ozone.rf, airquality, Temp)

data(iris)
set.seed(543)
iris.rf <- RRF(Species~., iris)
partialPlot(iris.rf, iris, Petal.Width, "versicolor")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("partialPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.RRF")
### * plot.RRF

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.RRF
### Title: Plot method for RRF objects
### Aliases: plot.RRF
### Keywords: classif regression tree

### ** Examples

data(mtcars)
plot(RRF(mpg ~ ., mtcars, keep.forest=FALSE, ntree=100), log="y")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.RRF", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict.RRF")
### * predict.RRF

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict.RRF
### Title: predict method for random forest objects
### Aliases: predict.RRF
### Keywords: classif regression

### ** Examples

data(iris)
set.seed(111)
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8, 0.2))
iris.rf <- RRF(Species ~ ., data=iris[ind == 1,])
iris.pred <- predict(iris.rf, iris[ind == 2,])
table(observed = iris[ind==2, "Species"], predicted = iris.pred)
## Get prediction for all trees.
predict(iris.rf, iris[ind == 2,], predict.all=TRUE)
## Proximities.
predict(iris.rf, iris[ind == 2,], proximity=TRUE)
## Nodes matrix.
str(attr(predict(iris.rf, iris[ind == 2,], nodes=TRUE), "nodes"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict.RRF", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rrfImpute")
### * rrfImpute

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rrfImpute
### Title: Missing Value Imputations by RRF
### Aliases: rrfImpute rrfImpute.formula rrfImpute.default
### Keywords: regression classif tree

### ** Examples

data(iris)
iris.na <- iris
set.seed(111)
## artificially drop some data values.
for (i in 1:4) iris.na[sample(150, sample(20)), i] <- NA
set.seed(222)
iris.imputed <- rrfImpute(Species ~ ., iris.na)
set.seed(333)
iris.rf <- RRF(Species ~ ., iris.imputed)
print(iris.rf)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rrfImpute", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rrfcv")
### * rrfcv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rrfcv
### Title: Random Forest Cross-Valdidation for feature selection
### Aliases: rrfcv
### Keywords: classif regression

### ** Examples



## The following can take a while to run, so if you really want to try
## it, copy and paste the code into R.

## Not run: 
##D set.seed(647)
##D myiris <- cbind(iris[1:4], matrix(runif(508 * nrow(iris)), nrow(iris), 508))
##D result <- rrfcv(myiris, iris$Species)
##D with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))
##D 
##D result <- replicate(5, rrfcv(myiris, iris$Species), simplify=FALSE)
##D error.cv <- sapply(result, "[[", "error.cv")
##D matplot(result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
##D         lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
##D         xlab="Number of variables", ylab="CV Error")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rrfcv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("treesize")
### * treesize

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: treesize
### Title: Size of trees in an ensemble
### Aliases: treesize
### Keywords: regression classif

### ** Examples

data(iris)
iris.rf <- RRF(Species ~ ., iris)
hist(treesize(iris.rf))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("treesize", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tuneRRF")
### * tuneRRF

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tuneRRF
### Title: Tune RRF for the optimal mtry parameter
### Aliases: tuneRRF
### Keywords: classif tree

### ** Examples

data(fgl, package="MASS")
fgl.res <- tuneRRF(fgl[,-10], fgl[,10], stepFactor=1.5)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tuneRRF", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("varImpPlot")
### * varImpPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: varImpPlot
### Title: Variable Importance Plot
### Aliases: varImpPlot
### Keywords: regression classif tree

### ** Examples

set.seed(4543)
data(mtcars)
mtcars.rf <- RRF(mpg ~ ., data=mtcars, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)
varImpPlot(mtcars.rf)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("varImpPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("varUsed")
### * varUsed

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: varUsed
### Title: Variables used in a random forest
### Aliases: varUsed
### Keywords: tree

### ** Examples

data(iris)
set.seed(17)
varUsed(RRF(Species~., iris, ntree=100))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("varUsed", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
