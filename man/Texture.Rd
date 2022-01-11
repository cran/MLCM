\name{Texture}
\alias{Texture}
\docType{data}
\title{
Three-way Conjoint Measurement Data for Texture Regularity.
}
\description{
Data from one subject, S1, for a 3-way MLCM experiment in which the element spacing, element size and  inter-element jitter of dot patterns were systematically varied.  Pairs of stimuli were presented with the 3 attributes varied independently, and the subject judged which of the pair appeared more regular.
}
\usage{data("Texture")}
\format{
  A data frame with 4140 observations on the following 7 variables.
  \describe{
    \item{\code{Resp}}{a numeric vector taking values 0/1 depending on whether the subject chose the first or second stimulus as more regular.}
    \item{\code{S1}}{a numeric vector indicating which of 3 levels of average element spacing for the first stimulus.}
    \item{\code{S2}}{a numeric vector indicating which of 3 levels of average element spacing for the second stimulus.}
    \item{\code{Z1}}{a numeric vector indicating which of 3 levels of element size for the first stimulus.}
    \item{\code{Z2}}{a numeric vector indicating which of 3 levels of element size for the second stimulus.}
    \item{\code{J1}}{a numeric vector indicating which of 5 levels of element jitter for the first stimulus.}
    \item{\code{J2}}{a numeric vector indicating which of 5 levels of element jitter for the second stimulus.}
  }
}
\details{
The authors describe the data sets as follows.  Each participant completed 4140 experimental trials, as shown in the 4140 rows: Column 1: response value. 0: participant chose stimulus 1 of a pair as more regular. 1: participant chose stimulus 2 as more regular. Column 2: element spacing level (1--3) for stimulus 1. Column 3: element spacing level (1--3) for stimulus 2. Column 4: element size level (1--3) for stimulus 1. Column 5: element size level (1--3) for stimulus 2. Column 6: element jitter level (1--5) for stimulus 1. Column 7: element jitter level (1--5) for stimulus 2. Note, the trial order was randomized during the experiment.

Additional data files for the other 5 participants in the study can be found in csv format files at
\doi{10.1371/journal.pcbi.1008802}.  With respect to the original file, the current data set was modified to include column names.

A fuller analysis of this data set can be found in the examples at \code{\link{mlcm}}.
}
\source{
Sun H.-C., St-Amand D., Baker C. L. Jr, Kingdom F. A. A. (2021), Visual perception of texture regularity: Conjoint measurements and a wavelet response-distribution model. \emph{PLoS Computational Biology} \bold{17(10)}, e1008802. \doi{10.1371/journal.pcbi.1008802}.
}
\references{
Knoblauch K., Maloney L. T. (2012) Modeling Psychophysical Data in R, Springer Science \& Business Media, \doi{10.1007/978-1-4614-4475-6}.
}
\examples{
data(Texture)
# additive model fit
Texture.mlcm <- mlcm(Texture)
summary(Texture.mlcm)
plot(Texture.mlcm, type = "b")
}
\keyword{datasets}
