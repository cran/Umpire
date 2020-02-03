\name{transforms}
\alias{normalOffset}
\alias{invGammaMultiple}
\title{Transform functions}
\description{
  \code{normalOffset} is a function that can be used as the
  \code{TRANSFORM} argument in an \code{alterMean} operation, which adds
  an offset to each value in the mean where the offset is chosen from a
  normal distribution.

  \code{invGammaMultiple} is a function that can be used as the
  \code{TRANSFORM} argument in an \code{alterSD} operation, which multiplies
  each standard deviation by a positive value chosen from an inverse gamma
  distribution with parameters \code{shape} and \code{scale}.
}
\usage{
normalOffset(x, delta, sigma)
invGammaMultiple(x, shape, rate)
}
\arguments{
  \item{x}{numeric vector of mean expression or standard deviation
    defined in the object}
  \item{delta, sigma}{numeric vector used as \code{mean} and/or \code{sd}
    parameters specifying the normal distribution}
  \item{shape, rate}{numeric vector used as \code{shape} and/or \code{rate}
    parameters specifying the gamma distribution. Must be positive.}
}
\value{
  \code{normalOffset} returns a new vector, each element of which is added
  by a offset chosen from a normal distribution with parameters \code{mean}
  and \code{sd}.

  \code{invGammaMultiple} returns a new vector, each element of which is
  multiplied by a positive value chosen from an inverse gamma distribution
  with parameters \code{shape} and \code{scale}.
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
  Jiexin Zhang \email{jiexinzhang@mdanderson.org},
}
\seealso{
  \code{\link{alterMean}},
  \code{\link{alterSD}},
  \code{\link[base]{RNGkind}}
}
\examples{
nComp <- 10
nGenes <- 100
comp <- list()
for (i in 1:nComp) {
  comp[[i]] <- IndependentNormal(rnorm(nGenes/nComp, 6, 1.5),
                                 1/rgamma(nGenes/nComp, 44, 28))
}
myEngine <- Engine(comp)
nrow(myEngine)
nComponents(myEngine)
summary(myEngine)
myData <- rand(myEngine, 5)
dim(myData)
summary(myData)
MEAN <- 2
SD <- 2
myEngine.alterMean <- alterMean(myEngine,
                                function(x) normalOffset(x, MEAN, SD))
myData.alterMean <- rand(myEngine.alterMean, 5)
summary(myData.alterMean)
RATE <- 1
SHAPE <- 2
myEngine.alterSD <- alterSD(myEngine,
                            function(x) invGammaMultiple(x, SHAPE, RATE))
myData.alterSD <- rand(myEngine.alterSD, 5)
summary(myData.alterSD)
}
\keyword{datagen}
\keyword{distribution}

