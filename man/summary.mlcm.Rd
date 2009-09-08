\name{summary.mlcm}
\alias{summary.mlcm}
\alias{print.summary.mlcm}

\title{ Summary Method for mlcm objects }
\description{
 Method functions for \code{mlcm} and \code{summary.glm} objects.}
\usage{
\method{summary}{mlcm}(object, digits = max(3, getOption("digits") - 4), ...)

\method{print}{summary.mlcm}(x, digits = max(3, getOption("digits") - 4), ...)
}

\arguments{
  \item{object}{ an object of class \dQuote{mlcm}, a result of a call to \code{mlcm} }
  \item{x}{an object of class \dQuote{summary.mlcm}, usually a call to \code{summary.mlcm}}
  \item{digits}{ the number of significant digits to use when printing }
  \item{\dots}{ further arguments passed to or from other methods }
}
\details{
 Displays summary information from a \dQuote{mlcm} object.
}
\value{
  A list of 5 elements
  \item{pscale }{A named vector or matrix indicting the estimated scale values.}
  \item{sigma }{The estimate of the scale parameter, currently always set to 1.}
  \item{logLik}{The logarithm of the likelihood.}
  \item{link}{The link used for the binomial family.}
  \item{model}{A character string giving the name of the model fit.}
  
Normally, \code{print.summary.mlcm} is not meant to be called directly by the user.
}
\references{  Ho, Y. H., Landy. M. S.  and Maloney, L. T. (2008). Conjoint measurement of gloss and surface texture. \emph{Psychological Science}, \bold{19}, 196--204.}
\author{Kenneth Knoblauch}
\seealso{  \code{\link{mlcm}}}
\examples{
summary(mlcm(BumpyGlossy))
}

\keyword{ methods }
\keyword{ print }
