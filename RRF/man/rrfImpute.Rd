\name{rrfImpute}
\alias{rrfImpute}
\alias{rrfImpute.formula}
\alias{rrfImpute.default}
\title{Missing Value Imputations by RRF}
\description{
  Impute missing values in predictor data using proximity from RRF.
}
\usage{
\method{rrfImpute}{default}(x, y, iter=5, ntree=300, ...)
\method{rrfImpute}{formula}(x, data, ..., subset)
}
\arguments{
  \item{x}{A data frame or matrix of predictors, some containing
    \code{NA}s, or a formula.}
  \item{y}{Response vector (\code{NA}'s not allowed).}
  \item{data}{A data frame containing the predictors and response.}
  \item{iter}{Number of iterations to run the imputation.}
  \item{ntree}{Number of trees to grow in each iteration of
    RRF.}
  \item{...}{Other arguments to be passed to
    \code{\link{RRF}}.}
  \item{subset}{A logical vector indicating which observations to use.}
}
\value{
  A data frame or matrix containing the completed data matrix, where
  \code{NA}s are imputed using proximity from RRF.  The first
  column contains the response.
}
\details{
  The algorithm starts by imputing \code{NA}s using
  \code{\link{na.roughfix}}.  Then \code{\link{RRF}} is called
  with the completed data.  The proximity matrix from the RRF
  is used to update the imputation of the \code{NA}s.  For continuous
  predictors, the imputed value is the weighted average of the
  non-missing obervations, where the weights are the proximities.  For
  categorical predictors, the imputed value is the category with the
  largest average proximity.  This process is iterated \code{iter}
  times.

  Note: Imputation has not (yet) been implemented for the unsupervised
  case.  Also, Breiman (2003) notes that the OOB estimate of error from
  RRF tend to be optimistic when run on the data matrix with
  imputed values.
}
\references{
  Leo Breiman (2003).  Manual for Setting Up, Using, and Understanding
  Random Forest V4.0.
  \url{http://oz.berkeley.edu/users/breiman/Using_random_forests_v4.0.pdf}
}
\seealso{
  \code{\link{na.roughfix}}.
}
\examples{
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
}
\author{Andy Liaw}
\keyword{regression}
\keyword{classif}
\keyword{tree}
