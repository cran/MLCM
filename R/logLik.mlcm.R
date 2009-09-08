`logLik.mlcm` <-
function(object, ...) {
	val <- logLik(object$obj)
	attr(val, "df") <- length(coef(object$obj))
	class(val) <- "logLik"
	val
	}

