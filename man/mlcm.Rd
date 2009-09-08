\name{mlcm}
\alias{mlcm}
\alias{mlcm.default}
\alias{print.mlcm}

\title{Fit Conjoint Measurement Models by Maximum Likelihood}
\description{Generic function \code{mlcm} uses different methods to fit the results of a conjoint measurement experiment using \code{glm} (Generalized Linear Model).  The default method (currently, the only one) permits fitting the data with a choice of 3 different models. 
}
\usage{
mlcm(x, ...)

\method{mlcm}{default}(x, model = "add", whichdim = NULL, lnk = "probit", 
	control = glm.control(maxit = 50000, epsilon = 1e-14), ...
	)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a data frame of an odd number of columns (at least 5).  The first is logical or a 2-level factor named \code{Resp} indicating the response of the observer.  The next columns give the indices in pairs along each dimension for each of the two stimuli being compared. }
  \item{model}{character indicating which of three conjoint measurement models to fit to the data:  \dQuote{add}, for additive (default), \dQuote{ind}, for independence or \dQuote{full}, for including a dependence with the levels of each dimension with the others.}
  \item{whichdim}{integer indicating which dimension of the data set to fit when the independence model is chosen}
  \item{lnk}{character indicating the link function to use with the binomial family.  Current default is the probit link.}
  \item{control}{information to control the fit.  See \code{glm} and \code{glm.control}.}
  \item{\dots}{additional arguments passed to \code{glm}. }
}
\details{
In a conjoint measurement experiment, observers are presented with pairs of stimuli that vary along 2 or more dimensions.  The observer's task is to choose which stimulus of the pair is greater along one of the dimensions.  Over a large number of trials, \code{mlcm} estimates numbers, 

\deqn{\psi_1, ..., \psi_p, \psi'_1, ..., \psi'_q, ...}, 

by maximum likelihood using \code{glm} that best predict the observer's judgments.  

The function permits the estimation of 3 different models, independent, additive (the default) and full,
by specifying the \code{model} argument.  The independent model fits the data along only 1 dimension, specified by the \code{whichdim} argument.  The additive model fits all dimensions with each fixed at 0 at the lowest level on each dimension.  Thus, if there are \eqn{n} dimensions each with \eqn{p_i} levels, \code{mlcm} estimates \eqn{\sum p_i - n} coefficients. 

Specifying the full model will fit a saturated model in which an estimate will be made for each combination of the scale values except the lowest (0 on all scales).  Currently, this option only allows 2 dimensions to be fit.
}
\value{
  a list of class \sQuote{mlcm} with the following components:
  
  \item{pscale }{a vector or matrix giving the perceptual scale value estimates}
  \item{sigma }{numeric indicating judgment \eqn{\sigma}, currently always set to 1}
  \item{NumDim}{numeric indicating number of stimulus dimensions in data set}
  \item{NumLev}{numeric indicating the number of levels along both dimensions, currently assumed to be the same}
  \item{model}{character indicating which of the 3 models were fit}
  \item{link}{character indicating the link used for the binomial family with \code{glm}}
  \item{obj}{the \sQuote{glm} object}
}
\references{ Luce, R. D., and Tukey, J. W. (1964). Simultaneous conjoint measurement. 
\emph{Journal of Mathematical Psychology}, \bold{1}, 
1--27.	
	
Krantz, D. H., Luce, R. D., Suppes, P., and Tversky, A. (1971).
\emph{Foundations of Measurement, Vol. 1: Additive and Polynomial Representations}.
New York: Academic Press.	
	
 Ho, Y. H., Landy. M. S.  and Maloney, L. T. (2008). Conjoint measurement of gloss and surface texture. \emph{Psychological Science}, \bold{19}, 196--204.}
\author{Ken Knoblauch }

\seealso{  \code{\link{glm}}}
\examples{

# Additive model
bg.add <- mlcm(BumpyGlossy)
plot(bg.add, type = "b")

# Independence model for Bumpiness
bg.ind <- mlcm(BumpyGlossy, model = "ind", whichdim = 2)

anova(bg.ind, bg.add, test = "Chisq")

# Full model
bg.full <- mlcm(BumpyGlossy, model = "full")

anova(bg.add, bg.full, test = "Chisq")

# Compare additive and full model graphically
plot(bg.full, standard.scale = TRUE, type = "b", 
	lty = 2, ylim = c(0, 1.05),
	xlab = "Gloss Level",
	ylab = "Bumpiness Model Estimates")
# additive prediction
bg.pr <- with(bg.add, outer(pscale[, 1], pscale[, 2], "+"))
# predictions are same for arbitrary scaling,
#  so we adjust additive predictions to best fit
#  those from the full model by a scale factor.
cf <- coef(lm(as.vector(bg.full$pscale/bg.full$pscale[5, 5]) ~ 
	as.vector(bg.pr) - 1))
matplot(cf * bg.pr, type = "b", add = TRUE, lty = 1)
}

\keyword{models}
\keyword{regression}