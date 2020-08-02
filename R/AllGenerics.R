# Copyright (C) Kevin R. Coombes, 2007-2012

###
### ALLGENERICS.R
###


if (!isGeneric("alterMean"))
  setGeneric("alterMean",
     function(object, TRANSFORM, ...) standardGeneric("alterMean"))

if (!isGeneric("alterSD"))
  setGeneric("alterSD",
     function(object, TRANSFORM, ...) standardGeneric("alterSD"))

if (!isGeneric("rand"))
  setGeneric("rand",
     function(object, n, ...) standardGeneric("rand"))

if (!isGeneric("nrow"))
  setGeneric("nrow",
     function(x) standardGeneric("nrow"))

if (!isGeneric("ncol"))
  setGeneric("ncol",
     function(x) standardGeneric("ncol"))

if (!isGeneric("nComponents"))
  setGeneric("nComponents",
     function(object, ...) standardGeneric("nComponents"))

if (!isGeneric("blur"))
  setGeneric("blur",
     function(object, x, ...) standardGeneric("blur"))

if (!isGeneric("addControl"))
  setGeneric("addControl",
     function(object, fraction = 0.5, ...) standardGeneric("addControl"))

