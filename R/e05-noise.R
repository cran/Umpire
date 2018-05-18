# Copyright (C) Kevin R. Coombes, 2007-2012

###############################################################
# A NOISE MODEL represents the additional machine noise that is layered
# on top of any biological variabilty when measuring the gene expression
# in a set of samples. We model both additive and multiplicative noise,
# so that the observed expression of gene g in sample i is given by
#    Y_gi = S_gi exp(H_gi) + E_gi
# where
#    Y_gi = observed expression
#    S_gi = true bilogical signal
#    H_gi ~ N(0, phi) defines the multiplicative noise
#    E_gi ~ N(nu, tau) defines the additive noise
# Note that we allow a systematic offset/bias in the additive noise model.

setClass("NoiseModel",
         slots = c(additiveOffset="numeric",
                   additiveScale="numeric",
                   multiplicativeScale="numeric"))

setValidity("NoiseModel", function(object) {
  msg <- NULL
  if (any(object@additiveScale < 0)) {
    msg <- c(msg, "additiveScale must be non-negative")
  }
  if (any(object@multiplicativeScale < 0)) {
    msg <- c(msg, "multiplicativeScale must be non-negative")
  }
  if (is.null(msg)) { # pass
    msg <- TRUE
  }
  msg
})

NoiseModel <- function(nu, tau, phi) {
  ## :TODO: sadly, no parameter checking was done...
  # KRC: Tough. Leave it alone.
  new("NoiseModel",
      additiveOffset=nu,
      additiveScale=tau,
      multiplicativeScale=phi)
}

# The main operation is given by blur(noiseModel, dataMatrix), which adds
# and multiplies random noise to the dataMatrix containing the true signal.
setMethod("blur", "NoiseModel", function(object, x, ...) {
  ## :PLR: This seems to be defined backwards as 'x' is the thing being
  ##       operated on --> giving 'blur(what, noise)' instead...
  ## :NOTE: This should have been two methods, using 'x' in signature
  # KRC: Leave it alone.
  if (inherits(x, "matrix")) {
    h <- matrix(rnorm(nrow(x)*ncol(x), 0, object@multiplicativeScale),
                nrow=nrow(x))
    e <- matrix(rnorm(nrow(x)*ncol(x), object@additiveOffset,
                      object@additiveScale),
                nrow=nrow(x))
  } else {
    h <- rnorm(length(x), 0, object@multiplicativeScale)
    e <- rnorm(length(x), object@additiveOffset, object@additiveScale)
  }
  exp(h)*x + e
})

