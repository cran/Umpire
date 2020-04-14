\name{rand-method}
\alias{rand}
\alias{rand-method}
\alias{rand,ANY-method}
\docType{methods}
\title{Method "rand"}
\description{
  \code{rand} is a generic function used to produce random vectors from the
  distribution defined by various objects. The generic function invokes particular
  \code{\link{methods}} which depend on the \code{\link{class}} of the first
  argument.
}
\usage{
\S4method{rand}{ANY}(object, n, \dots)
}
\arguments{
  \item{object}{an object from which random numbers from a distribution is
    desired}
  \item{n}{numeric scalar specifying quantity of random numbers}
  \item{\dots}{additional arguments affecting the random numbers produced}
}
\value{
  The form of the value returned by \code{rand} depends on the
  class of its argument. See the documentation of the particular methods
  for details of what is produced by that method.
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
}
\keyword{methods}

