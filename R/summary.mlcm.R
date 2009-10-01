`summary.mlcm` <-
function(object, 
	digits = max(3, getOption("digits") - 4), ...){
  z <- object
  ans <- list()
  ans$pscale <- z$pscale	
  ans$sigma <- z$sigma
  ans$logLik <- logLik(z)[1]	
  ans$link <- z$link
  ans$method <- z$method
  ans$par <- z$par
  ans$model <- z$model
  ans$formula <- z$formula
  class(ans) <- "summary.mlcm"
  ans
}

