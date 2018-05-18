# Copyright (C) Kevin R. Coombes, 2007-2012

###############################################################
# Now we create the general MVN class. The implementation is
# designed for efficiency when generating new samples, since
# we expect to do this several times.  Basically, this class
# separates the 'mvrnorm' function from the 'MASS' library into
# several steps.  The computationally expensive step (when the
# dimension is large) is the eigenvector decomposition of the
# covariance matrix. This step is performed at construction and
# the pieces are stored in the object.

setClass("MVN",
         slots = c(mu="numeric",
                   lambda="numeric",
                   half="matrix"))

## Generates an MVN object.
MVN <- function(mu, Sigma, tol=1e-06) {
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) 
    stop("incompatible arguments")
  ## :PLR: manpage recommends EISPACK=FALSE
  ## KRC: And it also warns that the eigenvalues may differ
  ## between platforms if you do that.
  eS <- eigen(Sigma, symmetric = TRUE, EISPACK = TRUE)
  ev <- eS$values
# JX: why not just all(ev>0)?
# KRC: because an eigenvalue of 10^-15 is essentially zero. Without
# this, numeric roundoff error causes problems.
  if (!all(ev >= -tol * abs(ev[1]))) 
    stop("'Sigma' is not positive definite")
  new("MVN",
      mu=mu,
      lambda=sqrt(pmax(ev, 0)),
      half=eS$vectors)
}

# The 'rand' method for 'MVN' objects contains the second half
# of the 'mvrnorm' function from the 'MASS' library.

# JX: do I really understand this?
setMethod("rand", "MVN", function(object, n, ...) {
  p <- length(object@mu)
# JX: n is the number of samples. X is generated as n*p. Should it be p*n?
  X <- matrix(rnorm(p * n), n)
  drop(object@mu) + object@half %*%
    diag(object@lambda, p) %*%  t(X)
})

setMethod("nrow", "MVN", function(x) {
  length(x@mu)
})

setMethod("summary", "MVN", function(object, ...) {
  cat("An MVN object, representing a vector\n")
  cat(paste("of length", length(object@mu),
              "of multivariate normal random variables.\n"))
})

# KRC: I suppose the 'covar' and 'correl' functions should be generic
# versions of 'cov' and 'cor'. But after a while I get tired of
# retrofitting S4 generic functions for everything.

# Assertion 1: This should return the same matrix that was used
# in the function call to construct the MVN object.
# Assertion 2: After applying an "alterMean" function (see e04-hit),
# the covariance matrix is unchanged.
covar <- function(object) {
  if (!inherits(object, "MVN"))
    stop("'object' must be derived from class 'MVN'.")
  Y <- diag(object@lambda) %*% object@half
  t(Y) %*% Y
}

# This is the correlation matrix that underlies the covariance
# matrix.
# Assertion 1: The diagonal consists of all ones.
# Assertion 2: After applying an "alterMean" or an "alterSD"
# function (see e04-hit), the correlation matrix is unchanged.
correl <- function(object) {
  Y <- covar(object)
  D <- diag(1/sqrt(diag(Y)))
  D %*% Y %*% D
}

