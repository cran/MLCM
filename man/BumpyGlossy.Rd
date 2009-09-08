\name{BumpyGlossy}
\alias{BumpyGlossy}
\alias{GlossyBumpy}

\docType{data}
\title{ Conjoint Measurement Data for Bumpiness and Glossiness}
\description{
  Data sets from two observers who were asked to judge which of two samples appeared bumpier (\code{BumpyGlossy}) or glossier (\code{GlossyBumpy}) when the two attributes were covaried simultaneously.
}
\usage{
	data(BumpyGlossy)
	data(GlossyBumpy)
	}
\format{
  A data frame with 975 observations on the following 5 variables.
  \describe{
    \item{\code{Resp}}{a factor with levels \code{0} \code{1} indicating whether the first or second sample was judged to be bumpier (glossier).}
    \item{\code{G1}}{a numeric vector indicating the indexed level of glossiness of the first sample.}
    \item{\code{G2}}{a numeric vector indicating the indexed level of glossiness of the second sample.}
    \item{\code{B1}}{a numeric vector indicating the indexed level of bumpiness of the first sample.}
    \item{\code{B2}}{a numeric vector indicating the indexed level of bumpiness of the second sample.}
  }
}
\details{
  Synthetic images of textures composed of random arrays of overlapping balls were created with varying degress of bumpiness and glossiness of the surfaces.  In separate experiments observers judged which of a pair of images appeared bumpier or glossier.  These data permit evaluating whether the level of glossiness (bumpiness) affects judgments of bumpiness (glossiness).  The data are from the observers (RK and FC) indicated in Figure 4C of the Ho et al (2008) paper.  Each data set contains three replications of the same stimuli with each session being 325 trials long.
}
\source{
  Ho, Y.-H., Landy. M. S., Maloney, L. T. (2008), Conjoint measurement of gloss and surface texture. \emph{Psychological Science}, \bold{19(2)}, 196--204.}
\references{
  Ho, Y.-H., Landy. M. S., Maloney, L. T. (2008), Conjoint measurement of gloss and surface texture. \emph{Psychological Science}, \bold{19(2)}, 196--204.
}
\examples{
data(BumpyGlossy)
data(GlossyBumpy)
}
\keyword{datasets}
