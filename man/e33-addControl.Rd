\name{addControl-method}
\alias{addControl}
\alias{addControl-method}
\alias{addControl,ANY-method}
\alias{addControl,CancerModel-method}
\alias{addControl,CancerEngine-method}
\docType{methods}
\title{Method "addControl"}
\description{
  \code{addControl} is a generic function used to add a control group to
  a simulated patient cohort. Implementations exists for a
  \code{CancerModel} and for a \code{CancerEngine}.
}
\usage{
\S4method{addControl}{ANY}(object, fraction = 0.5, \dots)
\S4method{addControl}{CancerModel}(object, fraction = 0.5, \dots)
\S4method{addControl}{CancerEngine}(object, fraction = 0.5, \dots)
}
\arguments{
  \item{object}{an object to which adding a contrl group is desired}
  \item{fraction}{a real number between zero oand one; the fraction of
    the final cohort that shoudl consist of controls}
  \item{\dots}{additional arguments; not yet used}
}
\value{
  Returns a new object of the same class as its first argument.
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
}
\keyword{methods}

