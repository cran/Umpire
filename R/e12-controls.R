## Copyright (C) Kevin R. Coombes and Caitlin E. Coombes, 2020

setMethod("addControl", "CancerModel", function(object, fraction=0.5, ...) {
  hp <- cbind(0, object@hitPattern)
  prev <- c(fraction, object@prevalence*(1-fraction))
  newCM <- new("CancerModel",
               name = paste(object@name, "plus control"),
               hitPattern = hp,
               survivalBeta = object@survivalBeta,
               outcomeBeta = object@outcomeBeta,
               prevalence = prev,
               survivalModel = object@survivalModel,
               call = object@call)
  newCM
})

setMethod("addControl", "CancerEngine", function(object, fraction=0.5, ...) {
  object@cm <- addControl(object@cm, fraction)
  object
})
