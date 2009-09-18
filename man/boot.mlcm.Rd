\name{boot.mlcm}
\alias{boot.mlcm}

\title{ Resampling of an Estimated Conjoint Measurement Scale }
\description{
 Using the \code{fitted} responses (probabilities) to the conjoint measurement scale, new responses are generated which permit new bootstrap replications of estimated scales to be generated. The mean scale is useful for evaluating bias and the standard deviation for estimating standard errors of the scale values. 
}
\usage{
boot.mlcm(x, nsim, ...)
}
\arguments{
  \item{x}{ an object of class \sQuote{mlcm}}
  \item{nsim}{ an integer, the number of simulations. }
  \item{\dots}{ Additional options passed along to the function \code{mlcm}. }
}
\details{
  The scale values (from \sQuote{glm} method) permit the fitted probabilities to be estimated.  These are used to generate new responses to the stimulus pairs using \code{rbinom}.  The new responses are then used with \code{mlcm} to estimate a bootstrapped scale.  This is repeated \eqn{N} times and stored in the output with the mean and standard deviation of the bootstrapped scales.
}
\value{
A list of 4 elements:  
  \item{boot.samp }{A \eqn{p} x \eqn{N} matrix of the bootstrap samples of the scale, where p is the number of scale values and N is the number of simulations.  }
  \item{bt.mean }{A vector of length \eqn{p} giving the mean of the bootstrap scales.}
  \item{bt.sd }{A vector of length \eqn{p} giving the standard deviation of the boostrap scales. }
  \item{N }{The number of bootstrap simulations.}
  
}
\references{ 
Ho, Y. H., Landy. M. S.  and Maloney, L. T. (2008). Conjoint measurement of gloss and surface texture. \emph{Psychological Science}, \bold{19}, 196--204.
}
\author{Kenneth Knoblauch and Laurence T. Maloney}

\seealso{\code{\link{mlcm}}}
\examples{
	data(BumpyGlossy)
	bg.mlcm <- mlcm(BumpyGlossy)
	#nsim should be near 10,000 for stability,
	# but this will take a little time
	boot.mlcm(bg.mlcm, 100)
}
\keyword{ datagen }
\keyword{ misc }
