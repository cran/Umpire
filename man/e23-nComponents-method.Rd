\name{nComponents-method}
\alias{nComponents}
\alias{nComponents-method}
\alias{nComponents,ANY-method}
\docType{methods}
\title{Method "nComponents"}
\description{
  \code{nComponents} is a generic function, analogous to 'nrow' or
  'ncol', that reports the number of components of an ``engine``.
}
\usage{
\S4method{nComponents}{ANY}(object, \dots)
}
\arguments{
  \item{object}{an object from which the number of components is desired}
  \item{\dots}{additional arguments affecting the number of components produced}
}
\value{
  Returns a nonnegative integer (scalar).
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
  Caitlin E. Coombes \email{caitlin.coombes@osumc.edu}
}
\keyword{methods}

