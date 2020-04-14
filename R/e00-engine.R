# Copyright (C) Kevin R. Coombes, 2007-2012

###############################################################
# An ENGINE is a tool (ie. an algorithm) for simulating vectors 
# of gene expression from some underlying distribution.  As
# functional objects, every engine takes a single parameter
# that specifies the number of sample vectors to be generated.
# The default is to generate one sample at a time.
#
## KRC: The following description no longer applies to the
##      actual class structure.
# In most cases, an engine is an instantiation of a more general
# family or class that we call an ABSTRACT ENGINE. An abstract
# engine can also be thought of as an engine with parameters.
# We instantiate an engine by binding all the free parameters
# of an abstract engine to actual values. Note that partial
# binding (of a subset of the free parameters) produces another
# abstract engine.
#
# NOTE: We interpret an ENGINE as the gene expression generator
# for a homogenous population; effects of cancer on gene
# expression are modeled at a higher level.

setClass("Engine", slots = c(components="list"))

Engine <- function(components) {
  new("Engine", components=components)
}

setMethod("summary", "Engine", function(object, ...) {
  cat(paste("An Engine with", length(object@components), "components.\n"))
})

setMethod("rand", "Engine", function(object, n, ...) {
  do.call(rbind, lapply(object@components, rand, n=n))
})

# Every engine must know the number of genes (i.e, the length of the vector)
# it generates.
setMethod("nrow", "Engine", function(x) {
  do.call(sum, lapply(x@components, nrow))
})

# Every abstract engine is an ordered list of components. For
# all practical purposes, a COMPONENT should be viewed as an
# irreducible abstract engine, which generates a contiguous
# subvector of the total vector of gene expression.
#
# Every component must have an IDENTIFIER that is unique within
# the context of its enclosing abstract engine.  The identifer
# may be implicitly taken to be the position of the component
# in the ordered list.

setMethod("nComponents", "Engine", function(object, ...) {
  length(object@components)
})

