`mlcm.default` <-
function (x, model = "add", whichdim = NULL, lnk = "probit", 
    control = glm.control(maxit = 50000, epsilon = 1e-14), ...) 
{
    if (!(model %in% c("add", "ind", "full"))) 
        stop("\nNot a legitimate value for model!\n")
    if ((model == "ind") && is.null(whichdim)) 
        stop("\nIndependence model requires you to choose \n\t\twhich dimension (whichdim) to fit!\n")
    d <- x
    dsInc.mat <- if (model != "full") {
       do.call(cbind, lapply(seq(1, length(d) - 1, 2), 
        function(x, d) make.wide(d[, x:(x + 1)]), d = d[, -1]))
    } else {
      make.wide.full(d[, -1])	
    }
    dsInc.df <- data.frame(resp = d[, 1], X = dsInc.mat)
    names(dsInc.df) <- c("resp", colnames(dsInc.mat))
    psc.glm <- if (model == "ind"){
    	 dmnms <- unique(substring(names(d[, -1]), 1, nchar(names(d[, 
            -1])) - 1))
        dm <- dmnms[whichdim]
        nl <- max(d[, -1])
        f <- paste(dm, seq_len(nl)[-1], sep = "", collapse = " + ")
        f <- as.formula(paste("resp ~ ", f, " - 1"))
        glm(f, family = binomial(link = lnk), data = dsInc.df, 
            control = control, ...)} else { 
        glm(resp ~ . - 1, family = binomial(link = lnk), 
        	data = dsInc.df, control = control, ...)
       }
    psc.glm$call$family[[2]] <- lnk
    psc.glm$call$control <- control
    nd <- (length(d) - 1)/2
    nl <- (length(dsInc.df) - 1)/nd
    switch(model, add = {
        pscale <- sapply(seq_len(nd), function(ix) {
            tmp <- c(0, coef(psc.glm)[seq((ix - 1) * nl + 1, 
                ix * nl)])
            names(tmp) <- paste("Lev", seq_len(nl + 1), sep = "")
            tmp
        })
        colnames(pscale) <- unique(substring(names(d[, -1]), 
            1, nchar(names(d[, -1])) - 1))
    }, ind = {
        pscale <- c(0, coef(psc.glm))
        pscale <- matrix(pscale, ncol = 1)
        rownames(pscale) <- paste(dmnms[whichdim], seq_len(nl + 
            1), sep = "")
    }, full = {
		 nlp <- (2 * nl + 1)^(1/nd)
        pscale <- matrix(c(0, coef(psc.glm)), ncol = nlp)
        dmn <- unique(substring(names(d[, -1]), 1, nchar(names(d[, 
            -1])) - 1))
        colnames(pscale) <- paste(dmn[2], seq_len(nlp), sep = "")
        rownames(pscale) <- paste(dmn[1], seq_len(nlp), sep = "")
    })
    psc.lst <- list(pscale = pscale, sigma = 1, 
    	method = "glm",
    	NumDim = nd, NumLev = nl + 1, 
    	model = model, link = lnk, 
    	obj = psc.glm)
    class(psc.lst) <- "mlcm"
    psc.lst
}
	