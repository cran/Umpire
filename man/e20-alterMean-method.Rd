\name{alterMean-method}
\alias{alterMean}
\alias{alterMean-method}
\alias{alterMean,ANY-method}
\alias{alterSD}
\alias{alterSD-method}
\alias{alterSD,ANY-method}
\docType{methods}
\title{Methods "alterMean" and "alterSD"}
\description{
  \code{alterMean} and \code{alterSD} are generic functions used to alter
  means or standard deviations, respectively, based on the input object.
  Each generic functions invokes different
  \code{\link{methods}} which depend on the \code{\link{class}} of the
  first argument.
}
\usage{
\S4method{alterMean}{ANY}(object, TRANSFORM, \dots)
\S4method{alterSD}{ANY}(object, TRANSFORM, \dots)
}
\arguments{
  \item{object}{an object for which altering mean or standard deviation
    is desired}
  \item{TRANSFORM}{function that returns its transformed input}
  \item{\dots}{additional arguments affecting the specific transformation
    performed}
}
\value{
  The form of the value returned by \code{alterMean} or \code{alterSD}
  depends on the class of its argument. See the documentation of the
  particular methods for details of what is produced by that method. 
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
  Jiexin Zhang \email{jiexinzhang@mdanderson.org},
}
\keyword{methods}

