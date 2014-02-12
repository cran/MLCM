\name{as.mlcm.df}
\alias{as.mlcm.df}

\title{
Coerce data frame to mlcm.df
}
\description{
Coerce a data frame from an MLCM experiment to an object of class \code{mlcm.df}. 
}
\usage{
as.mlcm.df(d, ...)
}
\arguments{
  \item{d}{
object of class \code{data.frame} typically obtained from an MLCM experiment.  It should have an odd number of columns and no less than 5.
}
  \item{\dots}{
Currently unused.
}
}
\details{
The first column should be named \code{Resp}.  Subsequent columns contain integer indices to the stimulus levels of the stimuli presented for each trial.  As there are two stimuli per trial and at least 2 dimensions tested per experiment, the minimun number of total columns will be 5.
}
\value{
Returns a data frame of class \code{mlcm.df}.
}

\author{
Kenneth Knoblauch
}


\seealso{
See Also as \code{\link{mlcm}},
}

\keyword{manip}
