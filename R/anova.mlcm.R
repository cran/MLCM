anova.mlcm <- function(object, ..., dispersion = NULL, test = NULL){
	dotargs <- list(...)
	m1 <- object$obj
	m2 <- if (length(dotargs) > 0) lapply(dotargs, "[[", "obj")
	if (length(m2) > 0) return(anova.glmlist(c(list(m1), m2), dispersion = dispersion, test = test)) else
		return(anova(m1, dispersion = dispersion, test = test))
}

