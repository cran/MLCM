\name{predict.mlcm}
\alias{predict.mlcm}
\title{
Predict Method for MLCM Objects
}
\description{
Predict values based on conjoint measurment scale fit.
}
\usage{
\method{predict}{mlcm}(object, newdata = NULL, type = "link", ...)
}
\arguments{
  \item{object}{object of class \sQuote{mlcm} usually created from running \code{mlcm}.
}
  \item{newdata}{numeric vector of new data for which to predict scale values.  Only relevant when the \code{formula} method is used.
}
  \item{type}{
character indicating whether the predicted value should be on the \dQuote{link} or \dQuote{response} scale. Only relevant when the \code{glm} method is used.
}
  \item{\dots}{
Other parameters passed along to the \code{predict} method of \code{glm} when the \code{glm} method is used.
}
}
\details{
For data sets fit with the \code{glm} method, the predicted values are returned either on the \dQuote{link} or \dQuote{response} scale.  For the \code{formula} method, predicted values are returned on the \dQuote{link} scale.  The \dQuote{newdata} argument is there for this case.
}
\value{
Numeric vector of predicted values.
}
\author{
Kenneth Knoblauch
}
\seealso{
\code{\link{mlcm}}, \code{\link{fitted.mlcm}}
}
\examples{
bg.add <- mlcm(BumpyGlossy)
bg.frm <- mlcm(~ p[1] * (x - 1)^p[2] + p[3] * (y - 1)^p[4],
	p = c(0.1, 1.3, 1.6, 0.8), data = BumpyGlossy)
xx <- seq(1, 5, len = 100)
plot(bg.add)
lines(xx, predict(bg.frm, newdata = xx)[seq_along(xx)])
lines(xx, predict(bg.frm, newdata = xx)[-seq_along(xx)])
}
\keyword{methods}
