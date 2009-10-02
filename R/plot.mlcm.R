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

`lines.mlcm` <- function(x, standard.scale = FALSE,
	transpose = FALSE, ...){
	m <- if (transpose) t(x$pscale) else x$pscale
   if (standard.scale){
   	dm <- dim(m)
   	mx <- max(m[dm[1], ])
   	matlines(m/mx, ...)
   	} else
   	matlines(m, ...)		
}

`points.mlcm` <- function(x, standard.scale = FALSE,
	transpose = FALSE, ...){
	m <- if (transpose) t(x$pscale) else x$pscale
   if (standard.scale){
   	dm <- dim(m)
   	mx <- max(m[dm[1], ])
   	matpoints(m/mx, ...)
   	} else
   	matpoints(m, ...)		
}
