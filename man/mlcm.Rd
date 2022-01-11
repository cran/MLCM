\name{mlcm}
\alias{mlcm}
\alias{mlcm.default}
\alias{mlcm.formula}
\alias{print.mlcm}

\title{Fit Conjoint Measurement Models by Maximum Likelihood}
\description{Generic function \code{mlcm} uses different methods to fit the results of a conjoint measurement experiment using \code{glm} (Generalized Linear Model).  The default method permits fitting the data with a choice of 3 different models. The formula method permits fitting the data with a parametric model.
}
\usage{
mlcm(x, ...)

\method{mlcm}{default}(x, model = "add", whichdim = NULL, lnk = "probit", 
	control = glm.control(maxit = 50000, epsilon = 1e-14), ...
	)
\method{mlcm}{formula}(x, p, data, 
	model = "add", whichdim = NULL,
	lnk = "probit", opt.meth = "BFGS",
	control = list(maxit = 50000, reltol = 1e-14), ...)
}
\arguments{
  \item{x}{ a data frame of an odd number of columns (at least 5) or a formula object.  In the case of a data frame, the first should be logical or a 2-level factor named \code{Resp} indicating the response of the observer.  The next columns give the indices in pairs along each dimension for each of the two stimuli being compared. }
  \item{p}{ numeric indicating initial values of parameters for the formula method.}
  \item{data}{data frame of class \sQuote{mlcm.df} for the formula method.}
  \item{model}{character indicating which of three conjoint measurement models to fit to the data:  \dQuote{add}, for additive (default), \dQuote{ind}, for independence or \dQuote{full}, for including a dependence with the levels of each dimension with the others. The \dQuote{full} is not applicable for the formula method.}
  \item{whichdim}{integer indicating which dimension of the data set to fit when the independence model is chosen}
  \item{lnk}{character indicating the link function to use with the binomial family.  Current default is the probit link.}
  \item{control}{information to control the fit.  See \code{glm} and \code{glm.control} or \code{optim} for the formula method.}
  \item{opt.meth}{character indicating optimization method (default: \dQuote{BFGS}) for \code{optim} with the formula method.}
  \item{\dots}{additional arguments passed to \code{glm} or \code{optim}. }
}
\details{
In a conjoint measurement experiment, observers are presented with pairs of stimuli that vary along 2 or more dimensions.  The observer's task is to choose which stimulus of the pair is greater along one of the dimensions.  Over a large number of trials, \code{mlcm} estimates numbers, 

\deqn{\psi_1, ..., \psi_p, \psi'_1, ..., \psi'_q, ...}, 

by maximum likelihood using \code{glm} that best predict the observer's judgments.  

The function permits the estimation of 3 different models, independent, additive (the default) and full,
by specifying the \code{model} argument.  The independent model fits the data along only 1 dimension, specified by the \code{whichdim} argument.  The additive model fits all dimensions with each fixed at 0 at the lowest level on each dimension.  Thus, if there are \eqn{n} dimensions each with \eqn{p_i} levels, \code{mlcm} estimates \eqn{\sum p_i - n} coefficients. 

Specifying the full model will fit a saturated model in which an estimate will be made for each combination of the scale values except the lowest (0 on all scales). This option, now, allows any number of dimensions to be fit.
}
\value{
  a list of class \sQuote{mlcm} that will include some of the following components depending on whether the default or formual method is used:
  
  \item{pscale }{a vector or matrix giving the perceptual scale value estimates}
  \item{stimulus }{numeric indicating the scale values along each dimension}
  \item{sigma }{numeric indicating judgment \eqn{\sigma}, currently always set to 1}
  \item{par }{numeric indicating the fitted parameter values when the formula method is used}
  \item{logLik }{log likelihood returned with the formula method}
  \item{hess }{Hessian matrix returned with the formual method}
  \item{method }{character indicating whether the model was fit by \code{glm} or with the formula method}
  \item{se }{standard errors returned with the formula method}
  \item{NumDim}{numeric indicating number of stimulus dimensions in data set}
  \item{NumLev}{numeric indicating the number of levels along both dimensions, currently assumed to be the same}
  \item{model}{character indicating which of the 3 models were fit}
  \item{link}{character indicating the link used for the binomial family with \code{glm}}
  \item{obj}{the \sQuote{glm} object}
  \item{data }{the \sQuote{mlcm} data frame}
  \item{conv }{numeric indicating whether convergence was reached in the case of the formula method}
  \item{formula }{formula object from argument to formula method}
  \item{func }{function constructed from formula object}
  \item{whichdim}{numeric indicating which dimension was fit in the case of the \dQuote{ind} model}
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

opar <- par(mfrow = c(1, 2), pty = "s")
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

#### Now make image of residuals between 2 models
bg.full.sc <- bg.full$pscale/bg.full$pscale[5, 5]
bg.add.adj <- cf * bg.pr
bg.res <- (bg.add.adj - bg.full.sc) + 0.5
image(1:5, 1:5, bg.res, 
	col = grey.colors(100, min(bg.res), max(bg.res)),
	xlab = "Gloss Level", ylab = "Bumpiness Level" 
	)
	
#### Example with formula
# additive model
bg.frm <- mlcm(~ p[1] * (x - 1)^p[2] + p[3] * (y - 1)^p[4], 
   p = c(0.1, 1.3, 1.6, 0.8), data = BumpyGlossy)
summary(bg.frm)
# independence model
bg.frm1 <- mlcm(~ p[1] * (x - 1)^p[2], p = c(1.6, 0.8),
	data = BumpyGlossy, model = "ind", whichdim = 2)
summary(bg.frm1)

### Test additive against independent fits
ddev <- -2 * (logLik(bg.frm1) - logLik(bg.frm))
df <- attr(logLik(bg.frm), "df") - attr(logLik(bg.frm1), "df")
pchisq(as.vector(ddev), df, lower = FALSE)

# Compare additive power law and nonparametric models 
xx <- seq(1, 5, len = 100)
par(mfrow = c(1, 1))
plot(bg.add, pch = 21, bg = c("red", "blue"))
lines(xx, predict(bg.frm, newdata = xx)[seq_along(xx)])
lines(xx, predict(bg.frm, newdata = xx)[-seq_along(xx)])
AIC(bg.frm, bg.add)
par(opar)


#### Analysis of 3-way MLCM data set
# additive model
T.mlcm <- mlcm(Texture)
summary(T.mlcm)
plot(T.mlcm, type = "b")
# independent models
lapply(seq(1, 3), function(wh){ 
	m0 <- mlcm(Texture, model = "ind", which = wh)
	anova(m0, T.mlcm, test = "Chisq")
	})
	
# Deviance differences for 2-way interactions vs 2-way additive models

mlcm(Texture[, -c(4, 5)])$obj$deviance - 
		mlcm(Texture[, -c(4, 5)], model = "full")$obj$deviance
mlcm(Texture[, -c(2, 3)])$obj$deviance - 
		mlcm(Texture[, -c(2, 3)], model = "full")$obj$deviance
mlcm(Texture[, -c(6, 7)])$obj$deviance - 
		mlcm(Texture[, -c(6, 7)], model = "full")$obj$deviance

# deviance differences for 3-way interaction tested against 3 2-way interactions
T3way.mlcm <-  mlcm(Texture, model = "full")  
## construct model matrix from 3 2-way interactions
T3_2way.mf <- cbind(model.frame(mlcm(Texture[, -c(4, 5)], model = "full")$obj), 
		model.matrix(mlcm(Texture[, -c(2, 3)], model = "full")$obj),
		model.matrix(mlcm(Texture[, -c(6, 7)], model = "full")$obj)
	)
T3_2way.mlcm <- glm(Resp ~ . + 0, family = binomial(probit), data = T3_2way.mf) 
Chi2 <- T3_2way.mlcm$deviance -  T3way.mlcm$obj$deviance
degfr <- T3_2way.mlcm$df.residual -  T3way.mlcm$obj$df.residual
pchisq(Chi2, degfr, lower.tail = FALSE)
}
\keyword{models}
\keyword{regression}
