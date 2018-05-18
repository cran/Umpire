# Copyright (C) Kevin R. Coombes, 2007-2012

###############################################################
# This is tricky.  A CANCER ENGINE is supposed to combine the
# CANCER MODEL (which defines hit patterns and keeps track of
# how each hit is a latent variable linking to survival or
# outcome phenotype data) with two gene expression ENGINE
# objects (that can be used to generate data depending on
# the presence or absence of individual hits).  The tricky part
# arises because we may want to apply this class with either
# an ENGINE or an ENGINE WITH ACTIVITY, so it is hard to
# define the class representation.  The first attempt used a
# poor man's reference scheme, by storing character strings with
# the names of objects that could be retrieved by an
# eval(as.name(...)) construction.  This idea fails if you try
# to produce a CANCER ENGINE inside a function, where R fails
# to locate the objects.  So, we replaced that with a local
# environment that stores the actual ENGINEs.

setClass("CancerEngine",
         slots = c(cm="CancerModel",
                   base="character",
                   altered="character",
                   localenv="environment"))

## Generates a CancerEngine object.
CancerEngine <- function(cm, base, altered) {
  localenv <- new.env()
  if (is.character(base)) {
    x <- try(eval(as.name(base), parent.frame()))
    if (inherits(x, "try-error")) {
      stop(paste("unable to locate base engine via", base))
    }
    if (!inherits(x, "Engine")) {
      stop(paste("base argument (", base, ") must evaluate to an Engine"))
    }
    assign(base, x, envir=localenv)
  } else {
    assign("base", base, envir=localenv)
    base <- "base"
  }
  if (is.character(altered)) {
    x <- try(eval(as.name(altered), parent.frame()))
    if (inherits(x, "try-error")) {
      stop(paste("unable to locate altered engine via", altered))
    }
    if(!inherits(x, "Engine")) {
      stop(paste("altered argument (", altered, ") must evaluate to an Engine"))
    }
    assign(altered, x, envir=localenv)
  } else {
    assign("altered", altered, envir=localenv)
    altered <- "altered"
  }
  new("CancerEngine", cm=cm, base=base, altered=altered, localenv=localenv)
}

setMethod("summary", "CancerEngine", function(object, ...) {
  cat(paste("A 'CancerEngine' using the cancer model:\n--------------\n"))
  summary(object@cm)
  cat("--------------\n\nBase expression given by:\n")
  summary(get(object@base, object@localenv))
  cat("\nAltered expression given by:\n")
  summary(get(object@altered, object@localenv))  
})

setMethod("rand", "CancerEngine", function(object, n, ...) {
  # first generate the clinical data
  clinical <- rand(object@cm, n, ...)
  hitlist <- clinical$CancerSubType       # remember the subtypes
  B <- get(object@base, envir=object@localenv)    # base Engine
  A <- get(object@altered, envir=object@localenv) # altered Engine
  b <- rand(B, n)       # base simulation
  a <- rand(A, n)       # altered values
  temp <- object@cm@hitPattern
  # there ought to be a better way to do this
  # idea is to expand the "hit pattern" for each patient to include
  # all genes in a correlated block
  U <- unlist(lapply(B@components, nrow))      # size of each component
  ends <- cumsum(U)
  starts <- (1+c(0, ends))[1:length(U)]
  isHit <- matrix(0, nrow=sum(U), ncol=n)
  for (i in 1:nrow(temp)) {
    for (j in 1:n) {
      if (temp[i, hitlist[j]]==1) {
        isHit[starts[i]:ends[i], j] <- 1
      }
    }
  }
  # now pick either 'base' or 'altered' based on absence or presence of hits
  foo <- b*(1-isHit) + a*(isHit)
  # note that the expression data does not include any noise....
  list(clinical=clinical, data=foo)
})
