`print.summary.mlcm` <-
function(x, 
	digits = max(3, getOption("digits") - 4), ...){
  cat("\n Maximum Likelihood Conjoint Measurement\n")
  cat("\nLink:\t")
  cat(x$link)
  cat("\t\tModel:\t")
  cat(x$model)	
  cat("\n\nPerceptual Scale:\n")
    print.default(format(x$pscale, digits = digits), 
    	quote = FALSE, ...)
  cat("\nsigma:\t")
    cat(format(x$sigma, digits = digits))
    cat("\t\t\tlogLik:\t")
    cat(format(x$logLik, digits = digits))
    cat("\n\n")	
    if(x$method == "formula"){
    	cat("formula:\t", as.character(x$formula))
    	cat("\n\np:\t", x$par)
    	}
    cat("\n")
    invisible(x)
}

