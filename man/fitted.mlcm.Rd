\name{fitted.mlcm}
\alias{fitted.mlcm}

\title{ Fitted Responses for a Conjoint Measurement Scale }
\description{
  \code{fitted.mlcm} returns the fitted responses from an estimated conjoing measurement scale obtained by \code{mlcm}.
}
\usage{
\method{fitted}{mlcm}(object, ...)
}


\arguments{
  \item{object}{object of class \sQuote{mlcm}, typically obtained from the output of \code{mlcm}. }
  \item{\dots}{currently ignored}
}

\value{
 A numeric vector contained the fitted probabilities to the responses of the observer for each pair of stimuli.
}
\references{Ho, Y. H., Landy. M. S.  and Maloney, L. T. (2008). Conjoint measurement of gloss and surface texture. \emph{Psychological Science}, \bold{19}, 196--204. }
\author{Kenneth Knoblauch}
\seealso{\code{\link{mlcm}}}
\examples{
data(BumpyGlossy)
fitted(mlcm(BumpyGlossy))
}

\keyword{ methods }
