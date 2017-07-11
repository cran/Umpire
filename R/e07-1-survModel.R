# Copyright (C) Kevin R. Coombes, 2007-2012

###
### SURVMODEL.R
###

setClass("SurvivalModel",
         representation=list(
           baseHazard="numeric",
           accrual="numeric",
           followUp="numeric",
           units="numeric",
           unitName="character"))
## JZ question: can baseHazard be an underlying hazard function?
## KRC: Not at present.  Might want to think about how to do this
##      in the future.

## Generates a SurvivalModel object.
SurvivalModel <- function(baseHazard=1/5,
                          accrual=5,  # years
                          followUp=1, # years
                          units=12, unitName="months") {
  new("SurvivalModel", baseHazard=baseHazard, accrual=accrual,
      followUp=followUp, units=units, unitName=unitName)
}

## Generate survival data.
setMethod("rand", "SurvivalModel", function(object, n, beta=NULL, ...) {
  if (is.null(beta)) {
    beta <- rep(0, n)
  }
  if (length(beta) != n) {
    stop("must supply 'beta' parameters for all patients")
  }
  hazard <- object@baseHazard*exp(beta)
  survival <- rexp(n, hazard)               # theoretical survival
  censor <- object@followUp + runif(n, 0, object@accrual) # real time
  cen <- trunc(censor*object@units)         # months, which is more likely ...
  sur <- trunc(survival*object@units)
  lfu <- ((sur + cen) - abs(sur-cen))/2     # vectorized minimum
  event <- (survival <= censor)             # did we observe the event?
  data.frame(LFU=lfu, Event=event)
})

