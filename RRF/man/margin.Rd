\name{margin}
\alias{margin}
\alias{margin.default}
\alias{margin.RRF}
\alias{plot.margin}
\title{Margins of RRF Classifier}
\description{
  Compute or plot the margin of predictions from a RRF classifier.
}
\usage{
\method{margin}{RRF}(x, ...)
\method{margin}{default}(x, observed, ...)
\method{plot}{margin}(x, sort=TRUE, ...)
}
\arguments{
  \item{x}{an object of class \code{\link{RRF}}, whose
    \code{type} is not \code{regression}, or a matrix of predicted
	probabilities, one column per class and one row per observation.
	For the \code{plot} method, \code{x} should be an object returned by
	\code{margin}.}
  \item{observed}{the true response corresponding to the data in \code{x}.}
  \item{sort}{Should the data be sorted by their class labels?}
  \item{...}{other graphical parameters to be passed to \code{plot.default}.}
}
\value{
  For \code{margin}, the \emph{margin} of observations from the
  \code{\link{RRF}} classifier (or whatever classifier that
  produced the predicted probability matrix given to \code{margin}).
  The margin of a data point is defined as the proportion of votes for
  the correct class minus maximum proportion of votes for the other
  classes.  Thus under majority votes, positive margin means correct
  classification, and vice versa.
}
\seealso{
  \code{\link{RRF}}
}
\examples{
set.seed(1)
data(iris)
iris.rf <- RRF(Species ~ ., iris, keep.forest=FALSE)
plot(margin(iris.rf))
}
\author{Robert Gentlemen, with slight modifications by Andy Liaw}
\keyword{classif}
