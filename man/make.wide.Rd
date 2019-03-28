\name{make.wide}
\alias{make.wide}
\alias{make.wide.full}

\title{ Create data frame for Fitting Conjoint Measurment Models by glm }
\description{
  \code{make.wide} and \code{make.wide.full} generate a \eqn{n} x \eqn{q - 1} matrix from an \eqn{n} x \eqn{2} column subset of a data frame storing the results of a conjoint measurement experiment, where \eqn{n} is the number of trials and \eqn{q} is the number of levels per dimension in the stimulus set tested. Currently, \code{make.wide.full} is limited to data sets with only 2 stimulus dimensions. The columns code covariates for all but the first stimulus level, which is constrained to be 0, along each dimension.  These columns take the value 0 unless one of the stimuli in the trial corresponded to a level along that dimension, in which case it takes a 1 or a -1, depending on which of the two stimuli represented that level. If both stimuli represent the same level for a dimension, then they cancel out and the column contains a 0.  This function is used for each dimension along which the stimuli vary to create a design matrix for each dimension.  The final design matrix is constructed inside the \code{mlcm} method by putting together the design matrices from each dimension.
}
\usage{
make.wide(d)

make.wide.full(d)
}
\arguments{
  \item{d}{a \eqn{n x 2} column data frame.  The columns give the indices of the levels of the dimensions along which the two stimuli presented in a trial vary.}
}
\details{
 This is a helper function, normally used inside \code{mlcm}, and not typically exploited by the casual user.  
}
\value{
	A data frame with \eqn{n} rows and \eqn{q - 1} columns
	\item{D2--Dq}{For each dimension along which the stimulus can vary, there are \eqn{q - 1} columns coding the absence or presence of that level of the dimension in the stimulus.  If the level is present, then the value is -1 or 1 as a function of which of the two stimuli contained that level, unless both do, in which case it is, also, 0. }
 
 }
\author{Kenneth Knoblauch }

\keyword{manip}
