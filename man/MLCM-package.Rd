\name{MLCM-package}
\Rdversion{1.1}
\alias{MLCM-package}
\alias{MLCM}
\docType{package}
\title{
Maximum Likelihood Conjoint Measurement
}
\description{
Estimate perceptual scales from data collected in a conjoint measurement experiment by maximum likelihood. 
 Data for conjoint measurement are typically collected using a psychophysical procedure. The stimuli vary along \eqn{n \ge 2}{n >= 2} dimensions.  The observer views pairs of stimuli and judges which stimulus of each pair is higher on a specified dimension.  For example, stimuli may be goods baskets containing amounts of milk and honey (dimensions) and the subject may order each pair of baskets by subjective desirability.
  This package contains functions to estimate the additive contribution of the \eqn{n} scales to the judgment by a maximum likelihood method under several hypotheses of how the perceptual dimensions interact.
}
\details{
\tabular{ll}{
Package: \tab MLCM\cr
Type: \tab Package\cr
Version: \tab 0.4.3\cr
Date: \tab 2020-01-11\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
}

Index:
\preformatted{
BumpyGlossy		Dataset: Conjoint Measurement for Bumpiness and Glossiness (Ho et al. 2008)

Texture			Dataset: 3-way conjoint Measurement for Texture (Sun et. al, 2021)

MLCM-package		Estimate perceptual scales from a conjoint measurement experiment by maximum likelihood

anova.mlcm		Likelihood ratio tests for Maximum Likelihood Conjoint Measurement models

logLik.mlcm		Calculate log likelihood for Conjoint Measurement models 

make.wide		Create data frame for Fitting Conjoint Measurement Scale by glm

mlcm			Fit Conjoint Measurement Models by Maximum Likelihood 

plot.mlcm		plot method for Maximum Likelihood Conjoint Measurement models

print.mlcm		print method for Maximum Likelihood Conjoint Measurement models

print.summary.mlcm	print  method for summary of Maximum Likelihood Conjoint Measurement models

summary.mlcm		summary method for Maximum Likelihood Conjoint Measurement models
}
}
\author{
Kenneth Knoblauch

Maintainers: Guillermo Aguilar <guillermo.aguilar@mail.tu-berlin.de>, Ken Knoblauch <ken.knoblauch@inserm.fr>
}
\references{
Luce, R. D., and Tukey, J. W. (1964). Simultaneous conjoint measurement. 
\emph{Journal of Mathematical Psychology}, \bold{1}, 
1--27.	
	
Krantz, D. H., Luce, R. D., Suppes, P., and Tversky, A. (1971).
\emph{Foundations of Measurement, Vol. 1: Additive and Polynomial Representations}.
New York: Academic Press.	
	
 Ho, Y. H., Landy. M. S.  and Maloney, L. T. (2008). Conjoint measurement of gloss and surface texture. \emph{Psychological Science}, \bold{19}, 196--204.
}

\keyword{ package }
\keyword{ models }
\seealso{
\code{\link{glm}}
}
\examples{
bg.acm <- mlcm(BumpyGlossy)
plot(bg.acm, pch = 21:22, bg = c("blue", "red"), col = "black",
	ylab = "Contributions to Perceived Bumpiness")
}
