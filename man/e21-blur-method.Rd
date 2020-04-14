\name{blur-method}
\alias{blur}
\alias{blur-method}
\alias{blur,ANY-method}
\docType{methods}
\title{Method "blur"}
\description{
  \code{blur} is a generic function used to add noise to a signal as defined
  by various objects. The generic function invokes different \code{\link{methods}}
  which depend on the \code{\link{class}} of the first argument.
}
\usage{
\S4method{blur}{ANY}(object, x, \dots)
}
\arguments{
  \item{object}{an object from which adding noise to its signal is desired}
  \item{x}{matrix containing signal to be affected}
  \item{\dots}{additional arguments affecting the noise addition}
}
\value{
  The form of the value returned by \code{blur} depends on the
  class of its argument. See the documentation of the particular methods
  for details of what is produced by that method.
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
}
\keyword{methods}

