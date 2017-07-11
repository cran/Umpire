# Copyright (C) Kevin R. Coombes, 2007-2012

###############################################################
# In practice, we use components that are multivariate normal.
# The case of independent normals is treated separately, but
# blocks of independent normals are components in the sense
# described above. Note that a component can be viewed as a
# special case of an engine that only has one component, so
# most methods of engines must also be defined for individual
# components.

setClass("IndependentNormal",
         representation=list(
           mu="numeric",
           sigma="numeric"))

## Generates an IndependentNormal object.
IndependentNormal <- function(mu, sigma) {
  if (length(sigma) != 1 & length(sigma) != length(mu))
    stop("'mu' and 'sigma' have different lengths")
  new("IndependentNormal", mu=mu, sigma=sigma)
}

setValidity("IndependentNormal", function(object) {
  msg <- NULL
  if (length(object@sigma) != 1) {
    if (length(object@sigma) != length(object@mu)) {
      msg <- c(msg, "lengths of sigma and mu differ")
    }
  }
  if (any(object@sigma < 0)) {
    msg <- c(msg, "sigma must be non-negative")
  }
  if (is.null(msg)) { # pass
    msg <- TRUE
  }
  msg
})

setMethod("summary", "IndependentNormal", function(object, ...) {
  cat("An IndependentNormal object, representing a vector\n")
  cat(paste("of length", length(object@mu),
            "of independent normal random variables.\n"))
})

setMethod("rand", "IndependentNormal", function(object, n, ...) {
  p <- length(object@mu)
  matrix(rnorm(n*p, object@mu, object@sigma), ncol=n)
})

setMethod("nrow", "IndependentNormal", function(x) {
  length(x@mu)
})

