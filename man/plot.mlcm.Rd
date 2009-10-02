\name{plot.mlcm}
\alias{plot.mlcm}
\alias{lines.mlcm}
\alias{points.mlcm}

\title{ Plot an mlcm Object }
\description{
  Plots the conjoint measurement scale(s) as a function of stimulus level.
}
\usage{
\method{plot}{mlcm}(x, standard.scale = FALSE, transpose = FALSE, ...)
\method{lines}{mlcm}(x, standard.scale = FALSE, transpose = FALSE, ...)
\method{points}{mlcm}(x, standard.scale = FALSE, transpose = FALSE, ...)
}

\arguments{
  \item{x}{ \code{mlcm} object, typically result of \code{mlcm} }
  \item{standard.scale}{ logical indicating whether the plotted scales should be normalized so that the maximum scale value is 1 }
  \item{transpose}{logical, indicating whether to transpose the matrix of the perceptual scale, when the full model is fit. Not defined if there are more than 2 dimensions.}
  \item{\dots}{ other parameters to be passed through to the plotting function }
}
\details{
  These functions use \code{matplot}, \code{matlines} and \code{matpoints} so their help page should be examined for information on additional parameters that can be specified.
}


\author{ Kenneth Knoblauch }

\seealso{ \code{\link{matplot}}}
\examples{
plot(mlcm(BumpyGlossy), type = "b")

bg.full <- mlcm(BumpyGlossy, model = "full")
opar <- par(mfrow = c(1, 2), pty = "s")
plot(bg.full, type = "b", 
	xlab = "Gloss Level",
	ylab = "Bumpiness Model Estimates")
plot(bg.full, transpose = TRUE, type = "b",
	xlab = "Bumpiness Level",
	ylab = "Glossiness Model Estimates")
par(opar)
}

\keyword{ hplot }
