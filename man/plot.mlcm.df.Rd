\name{plot.mlcm.df}
\Rdversion{1.1}
\alias{plot.mlcm.df}

\title{
Create Conjoint Proportion Plot from mlcm.df Object
}
\description{
Creates a conjoint proportions plot as in Ho et al. (2008) in which the proportion of responses of one type are indicated as a function of the stimulus pairs used in the experiment.
}
\usage{
\method{plot}{mlcm.df}(x, clr = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object of class \sQuote{mlcm.df} typically the result of a conjoint measurement experiment of 2 dimensions.
}
  \item{clr}{a palette for the color scale in the plot. If none specified, then a grey level palette will be calculated based on the maximum number of repetitions.
}
  \item{\dots}{additional graphical parameters passed to \code{image} used for generating the plot.
}
}
\details{
The input should be a data frame of class \sQuote{mlcm.df} that contains 5 columns.  The first column contains the responses of the observer.  The next two correspond to the indices of the stimuli along the first dimensions and the last two the indices of the two stimuli along the second dimension.  The plot shows a color-coded (grey-level by default) map of the proportion of responses for each combination of indices in one stimulus with respect to the combinations of the indices in the other.  There should be several replications of each pairing for the plot to make some sense.
}
\value{Currently, nothing is returned.  Used for its side-effect of producing a plot.
}
\references{
 Ho, Y. H., Landy. M. S.  and Maloney, L. T. (2008). Conjoint measurement of gloss and surface texture. \emph{Psychological Science}, \bold{19}, 196--204.
}
\author{Ken Knoblauch}
\note{
Will not work on experiments using more than 2 dimensions.
}

\seealso{\code{\link{image}}}
\examples{
	data(BumpyGlossy)
	plot(BumpyGlossy, 
		xlab = expression(paste("Surface ", S[ij], 
		" Gloss Level (i) and Bump level (j)")),
		ylab = expression(paste("Surface ", S[kl], 
		" Gloss Level (k) and Bump level (l)")) 
	)
}

\keyword{hplot}
