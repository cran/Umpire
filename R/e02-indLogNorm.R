# Copyright (C) Kevin R. Coombes, 2007-2012

###############################################################
# In addition to the Independent Normal class, we also have a
# class for independent log normal, because the true expression
# value follows a log normal distribution in our model.

setClass("IndependentLogNormal",
         representation = list(
           logmu="numeric",
           logsigma="numeric"))

## Generates an IndependentLogNormal object.
IndependentLogNormal <- function(logmu, logsigma) {
  if (length(logsigma) != 1 & length(logsigma) != length(logmu))
    stop("'logmu' and 'logsigma' have different lengths")
  new("IndependentLogNormal", logmu=logmu, logsigma=logsigma)
}

setValidity("IndependentLogNormal", function(object) {
  msg <- NULL
  if (length(object@logsigma) != 1) {
    if (length(object@logsigma) != length(object@logmu)) {
      msg <- c(msg, "lengths of logsigma and logmu differ")
    }
  }
  if (any(object@logsigma < 0)) {
    msg <- c(msg, "all logsigma values must be non-negative")
  }
  if (is.null(msg)) { # pass
    msg <- TRUE
  }
  msg
})

setMethod("summary", "IndependentLogNormal", function(object, ...) {
  cat("An IndependentLogNormal object, representing a vector\n")
  cat(paste("of length", length(object@logmu),
              "of independent log normal random variables.\n"))
})

setMethod("rand", "IndependentLogNormal", function(object, n, ...) {
  p <- length(object@logmu)
  matrix(rlnorm(n*p, object@logmu, object@logsigma), ncol=n)
})

setMethod("nrow", "IndependentLogNormal", function(x) {
  length(x@logmu)
})

