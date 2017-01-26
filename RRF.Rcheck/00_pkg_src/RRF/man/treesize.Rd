\name{treesize}
\alias{treesize}

\title{Size of trees in an ensemble}

\description{
Size of trees (number of nodes) in and ensemble.
}

\usage{
treesize(x, terminal=TRUE)
}

\arguments{
  \item{x}{an object of class \code{RRF}, which contains a
    \code{forest} component.}
  \item{terminal}{count terminal nodes only (\code{TRUE}) or all nodes
    (\code{FALSE}}
}

\value{
A vector containing number of nodes for the trees in the
\code{RRF} object.
}

\note{
  The \code{RRF} object must contain the \code{forest}
  component; i.e., created with \code{RRF(...,
    keep.forest=TRUE)}. 
}

\seealso{\code{\link{RRF}}}

\author{Andy Liaw \email{andy\_liaw@merck.com}}

\examples{
data(iris)
iris.rf <- RRF(Species ~ ., iris)
hist(treesize(iris.rf))
}

\keyword{regression}
\keyword{classif}
