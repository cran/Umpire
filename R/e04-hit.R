# Copyright (C) Kevin R. Coombes, 2007-2012

###############################################################
# A HIT is a modification to one of the components of an engine
# or abstract engine. All modifications must be expressible as
# adjustments to the parameters of the component. In mathematical
# terms, a hit is a function whose input is the set of parameter
# values of a component and whose output is a new set of
# parameter values.
#
# Before implementing objects to represent hits, we start by
# defining two generic kinds of effects that a hit can have.  In
# the multivariate normal context, we can alter the mean or we can
# alter the standard deviation.

####################
# The 'object' of an 'alterMean' operation should be an engine or
# a component of an engine. The 'TRANSFORM' function for each
# object should take as its input a vector of mean expression and
# return a transformed mean vector that can be used to alter the
# object.

# The 'object' of an 'alterSD' operation should be an engine or
# a component of an engine. The 'TRANSFORM' function for each
# object should take as its input a vector of standard deviations
# and return a transformed vector that can be used to alter the
# object.

# alterMean and alterSD for an Engine just loop over the
# components

setMethod("alterMean", "Engine", function(object, TRANSFORM, ...) {
  new("Engine",
      components=lapply(object@components,
        alterMean, TRANSFORM, ...))
})

## Loop over the components
setMethod("alterSD", "Engine", function(object, TRANSFORM, ...) {
  new("Engine",
      components=lapply(object@components,
        alterSD, TRANSFORM, ...))
})

####################
# alterMean and alterSD for an 'IndependentNormal' simply replace
# the appropriate slot by the transformed vector
setMethod("alterMean", "IndependentNormal", function(object, TRANSFORM, ...) {
  new("IndependentNormal",
      mu=TRANSFORM(object@mu, ...),
      sigma=object@sigma)
})

## Replace the appropriate slot by the transformed vector
setMethod("alterSD", "IndependentNormal", function(object, TRANSFORM, ...) {
  new("IndependentNormal",
      mu=object@mu,
      sigma=TRANSFORM(object@sigma, ...))
})

####################
## Replace the appropriate slot by the transformed vector
setMethod("alterMean", "IndependentLogNormal", function(object, TRANSFORM, ...) {
  new("IndependentLogNormal",
      logmu = TRANSFORM(object@logmu), # maybe add option for working on normal scale
      logsigma = object@logsigma)
})

## Replace the appropriate slot by the transformed vector
setMethod("alterSD", "IndependentLogNormal", function(object, TRANSFORM, ...) {
  new("IndependentLogNormal",
      logmu = object@logmu,
      logsigma = TRANSFORM(object@logsigma, ...)) # option for normal scale?
})


####################
# alterMean for an 'MVN' simply replaces the appropriate slot by
# the transformed vector
setMethod("alterMean", "MVN", function(object, TRANSFORM, ...) {
  new("MVN",
      mu=TRANSFORM(object@mu, ...),
      lambda=object@lambda,
      half=object@half)
})

# alterSD for an MVN is trickier, because of the way the data is stored.
# In order to have some hope of getting this correct, we work in the space of
# the covariance matrix, Sigma. If we let R denote the correlation matrix and
# let Delta be the diagonal matrix whose entries are the individual standard
# deviations, then  Sigma = Delta %*% R %*% Delta. So, we can change the
# standard deviations by replacing Delta in this product.  We then construct
# a new 'MVN' object with the old mean vector and the new covariance matrix.
setMethod("alterSD", "MVN", function(object, TRANSFORM, ...) {
  Y <- covar(object)
  sigma <- sqrt(diag(Y)) # old standard deviations
  newsig <- TRANSFORM(sigma, ...) # new standard deviations
  D <- diag(newsig/sigma)
  Sigma <- D %*% Y %*% D
  MVN(object@mu, Sigma)
})

# Here is a possible 'TRANSFORM' to be used in an 'alterMean' operation.
# Each value in the mean is changed by adding an offset, where the offset is
# chosen from a normal distribution. Similar transforms can be defined using
# other distributions. Constant (non-random) offsets can be obtained using
# the transform "function(x){x+OFFSET}".
normalOffset <- function(x, delta=0, sigma=1) {
  x + rnorm(length(x), delta, sigma)
}

# Here is a possible 'TRANSFORM' for an 'alterSD' operation, which multiplies
# each standard deviation by a positive value chosen from an inverse gamma
# distribution. Constant multiples can be obtained using the transform
# "function(x){x*SCALE}".
invGammaMultiple <- function(x, shape, rate) {
  x / rgamma(length(x), shape=shape, rate=rate)
}

