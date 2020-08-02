# Copyright (C) Kevin R. Coombes, 2007-2012

###############################################################
# A CANCER MODEL contains a number of pieces of information
# representing an abstract, heterogeneous collection of cancer
# patients.  First, there is a binary matrix of "hit patterns".
# Here a "hit" is the same thing described above; namely, an
# alteration to a component of an engine. Each column of the
# matrix represents a different subtype of the cancer; each
# row represents one component.  A '1' entry means that the
# component is altered in that subtype; a '0' entry means that
# it is not altered.
#
# Each hit is assumed to have an effect on the survival of
# any individual in which it occurs; this effect will be
# used as a parameter in a standard Cox proportional hazards
# model to simulate survival data.
#
# Each hit is also assumed to have an effect on a binary
# outcome (which can be thought of as 'good' or 'bad' or can
# be thought of as 'responder' or 'non-responder'), modeled
# as a parameter in a logistic model for the probability of
# one of the outcomes.
#
# Finally, each cancer subtype is assumed to occur with
# some prevalence.

setClass("CancerModel",
         slots = c(name="character",
                   hitPattern="matrix",
                   survivalBeta="numeric",
                   outcomeBeta="numeric",
                   prevalence="numeric",
                   survivalModel="SurvivalModel",
                   call="call"))

# We define a general-purpose generator for cancer models.
#   nPossible = number of possible hits = number of rows in
#      the matrix of hit patterns
#   nPattern = number of cancer subtypes = number of columns
#      in the matrix of hit patterns
#   HIT  = generator (r-function) of a discrete distribution
#      supported on the positive integers
#   SURV = generator (r-function) of a continuous distribution
#   OUT  = generator (r-function) of a continuous distribution
#   survivalModel = object encapsulating the parameters needed
#      to simulate survival times.
#   prevalence = optional vector of relative prevalences of
#      the cancer subtypes.
CancerModel <- function(name, nPossible, nPattern,
                        HIT=function(n) 5,
                        SURV=function(n) rnorm(n, 0, 0.3),
                        OUT=function(n) rnorm(n, 0, 0.3),
                        survivalModel=NULL,
                        prevalence=NULL) {
  call <- match.call()
  if (is.null(survivalModel)) {
    survivalModel = SurvivalModel()
  }
  if (is.null(prevalence) | length(prevalence) == 1) { # equally likely
    prevalence <- rep(1/nPattern, nPattern)
  }
  if (length(prevalence) > 1) {
    if (length(prevalence) != nPattern) {
      stop("number of patterns must equal the number of prevalences.")
    }
    prevalence <- prevalence/sum(prevalence)
  }
  if (any(prevalence < 0)) {
    stop("prevalences must be non-negative")
  }
  
  hp <- matrix(0, nPossible, nPattern)
  for (i in 1:nPattern) {
    # JX: for each subtype, there are 5 hits? then nHitsPerPattern
    # return vector containing all 5? 
    # KRC: Only in the default.  You can replace HIT with a function
    # that generates random integers (say, discrete uniform between 3
    # and 10) so there are different numbers of hits per pattern.
    hp[sample(nPossible, HIT(nPossible)), i] <- 1
  }
  s <- SURV(nPossible)
  o <- OUT(nPossible) # this leaves outcome uncorrelated with survival
#  o <- sort(o)[rank(s)] # force outcome to correlate with survival
# KRC: which do we really want to use? Should this be user-selectable?
  scramble <- sample(nPossible)
  s <- c(s[s > 0], s[s <= 0])[scramble]
  o <- c(o[o > 0], o[o <= 0])[scramble]
  new("CancerModel",
      name=name,
      hitPattern=hp,
      survivalBeta=s,
      outcomeBeta=o,
      prevalence=prevalence,
      survivalModel=survivalModel,
      call=call)
}

setMethod("ncol", "CancerModel", function(x) {
  ncol(x@hitPattern)
})

setMethod("nrow", "CancerModel", function(x) {
  nrow(x@hitPattern)
})

nPatterns <- function(object) {
  if(!inherits(object, "CancerModel"))
    stop(paste("Class of 'object' should be CancerModel, not", class(object)))
  ncol(object)
}

nPossibleHits <- function(object) {
  if(!inherits(object, "CancerModel"))
    stop(paste("Class of 'object' should be CancerModel, not", class(object)))
  nrow(object)
}

# Since one can construct models where the number of hits differs from one
# pattern to another, this function returns a vector of length equal to the
# number of patterns, with values indicating how many hits occur in each
# of the patterns.
nHitsPerPattern <- function(object) {
  if(!inherits(object, "CancerModel"))
    stop(paste("Class of 'object' should be CancerModel, not", class(object)))
  apply(object@hitPattern, 2, sum)
}

survivalCoefficients <- function(object) {
  if(!inherits(object, "CancerModel"))
    stop(paste("Class of 'object' should be CancerModel, not", class(object)))
  object@survivalBeta
}

outcomeCoefficients <- function(object) {
  if(!inherits(object, "CancerModel"))
    stop(paste("Class of 'object' should be CancerModel, not", class(object)))
  object@outcomeBeta
}

setMethod("summary", "CancerModel", function(object, ...) {
  cat(paste(object@name, ", a CancerModel object constructed via:\n", sep=""))
  cat(paste("  ", strwrap(as.character(list(object@call))), "\n"))
  cat("\nPattern prevalences:\n")
  print(object@prevalence)
  cat("\nSurvival effects:\n")
  print(summary(object@survivalBeta))
  cat("\nOutcome effects:\n")
  print(summary(object@outcomeBeta))
})

# JX: what's hc? sample(1:nPossible)?
# KRC: check its usage below in the 'rand' method for a CancerModel
# hc represents the cancer subtype or Hit-pattern Class (HC).
.realizeOutcome <- function(object, hc) {
  if(!inherits(object, "CancerModel"))
    stop("First argument must be a 'CancerModel' object")
  temp <- as.vector(matrix(object@outcomeBeta, nrow=1) %*% object@hitPattern)
  probs <- exp(temp) / (1+exp(temp))
  outclass <- c("Good", "Bad")
  factor(outclass[1+rbinom(length(hc), 1, probs[hc])])
}

.realizeSurvival <- function(object, hc) {
  if(!inherits(object, "CancerModel"))
    stop("First argument must be a 'CancerModel' object")
  sm <- object@survivalModel
  temp <- as.vector(matrix(object@survivalBeta, nrow=1) %*% object@hitPattern)
  rand(sm, length(hc), beta=temp[hc])
}

## Here we generate a phenoData object
setMethod("rand", "CancerModel", function(object,  n, balance=FALSE, ...) {
  if (balance) {
    m <- ncol(object@hitPattern)
    # Maybe we should at least give a warning if n is not multiple of m. 
    # otherwise, the number of samples returned won't be whatever the user expected.
    count <- round(n/m)
    hc <- rep(1:m, each=count)
  } else {
    cp <- cumsum(object@prevalence)
    ru <- runif(n)
    hc <- unlist(lapply(ru, function(x, cp) {
      1 + length(cp) - sum(cp > x)
    }, cp)) # picks out a class by sampling from an explicit
            # discrete distribution
  }
  outcome <- .realizeOutcome(object, hc)
  survival <- .realizeSurvival(object, hc)
  data.frame(CancerSubType=hc,
             Outcome=outcome,
             survival)
})

 
