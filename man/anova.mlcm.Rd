\name{anova.mlcm}
\alias{anova.mlcm}

\title{ Analysis of Deviance for Maximum Likelihood Conjoint Measurement Model Fits }
\description{
 Compute an analysis of deviance table for one or more maximum likelihood conjoint measurement model fits.
}
\usage{
\method{anova}{mlcm}(object, ..., dispersion = NULL, test = NULL)
}
\arguments{
  \item{object, \dots}{objects of class \code{mlcm}, typically the result of a call to \code{mlcm}}
  \item{dispersion}{the dispersion parameter for the fitting family.  By default, it is obtained from the object(s)}
  \item{test}{a character string (partially) matching one of "Chisq", "F", or "Cp".  See \code{\link{stat.anova}}.  Normally, "Chisq" is the appropriate value, here. }
}
\details{
 See \code{\link{anova.glm}} for details.  In brief, specifying a single object, results in the display of a sequential analysis of deviance table for that model.  Specifying several objects, a table indicating the results of the likelihood ratio tests between successive models is displayed.  The models must be nested and fit to the same data set.  One can mix a \code{formula} method model with a \code{glm} model, but not more than one comparison between a pair of such models at a time.
}
\value{
An object of class "anova" inheriting from class "data.frame". }
\references{  Ho, Y. H., Landy. M. S.  and Maloney, L. T. (2008). Conjoint measurement of gloss and surface texture. \emph{Psychological Science}, \bold{19}, 196--204.}
\author{Kenneth Knoblauch}
\section{Warning }{see section Warnings in \code{\link{anova}} for warnings.} 

\seealso{\code{\link{anova.glm}}, \code{\link{anova}},  \code{\link{glm}} }
\examples{
bg.add <- mlcm(BumpyGlossy)
bg.ind <- mlcm(BumpyGlossy, model = "ind", whichdim = 2)
bg.full <- mlcm(BumpyGlossy, model = "full")

anova(bg.ind, bg.add, bg.full, test = "Chisq")
}
\keyword{methods}
\keyword{models}
