`summary.mlcm` <-
function(object, 
	digits = max(3, getOption("digits") - 4), ...){
  z <- object
  ans <- list()
  ans$pscale <- z$pscale	
  ans$sigma <- z$sigma
  ans$logLik <- logLik(z$obj)[1]	
  ans$link <- z$link
  ans$model <- z$model
  class(ans) <- "summary.mlcm"
  ans
}

