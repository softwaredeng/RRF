\name{grow}
\alias{grow}
\alias{grow.default}
\alias{grow.RRF}

\title{Add trees to an ensemble}

\description{
  Add additional trees to an existing ensemble of trees.
}

\usage{
\method{grow}{RRF}(x, how.many, ...)
}

\arguments{
  \item{x}{an object of class \code{RRF}, which contains a
    \code{forest} component.}
  \item{how.many}{number of trees to add to the \code{RRF}
    object.}
  \item{...}{currently ignored.}
}

\value{
An object of class \code{RRF}, containing \code{how.many}
additional trees. 
}

\note{
The \code{confusion}, \code{err.rate}, \code{mse} and \code{rsq}
components (as well as the corresponding components in the \code{test}
compnent, if exist) of the combined object will be \code{NULL}.  
}

\seealso{\code{\link{combine}}, \code{\link{RRF}}}

\author{Andy Liaw \email{andy\_liaw@merck.com}}

\examples{
data(iris)
iris.rf <- RRF(Species ~ ., iris, ntree=50, norm.votes=FALSE)
iris.rf <- grow(iris.rf, 50)
print(iris.rf)
}

\keyword{regression}
\keyword{classif}
