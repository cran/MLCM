`plot.mlcm` <-
function(x, standard.scale = FALSE, transpose = FALSE, ...){
   par(ask = FALSE)
   m <- if (transpose) t(x$pscale) else x$pscale
   if (standard.scale){
   	dm <- dim(m)
   	mx <- max(m[dm[1], ])
   	matplot(m/mx, ...)
   	} else
   	matplot(m, ...)
}

