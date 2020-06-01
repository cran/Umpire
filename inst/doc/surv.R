## ----packages-----------------------------------------------------------------
library(Umpire)
library(survival)

## ----sm, fig.width=4, fig.height=4--------------------------------------------
sm <- SurvivalModel(baseHazard = 1/5, # default 1/5 inverse years
                    accrual = 5,      # default 5 years
                    followUp = 1,     # default 1 years
                    units = 12, unitName = "months")
R <- rand(sm, 200)
summary(R)

## ----km, fig.cap="Base Kaplan-Meier survival curve."--------------------------
baseModel <- survfit(Surv(LFU, Event) ~ 1, R)
plot(baseModel)

## ----cm, fig.cap="Two group survival curves (original).", fig.width=4, fig.height=4----
for (ignore in 1:5) {
  for (jgnore in 1:4) {
    cm <- CancerModel("survtester", nPossible=20, nPattern=2,
                       SURV = function(n) rnorm(n, 0, 2), # old default; too large
     		       survivalModel = sm)
    S <- rand(cm, 200)
    model <- survfit(Surv(LFU, Event) ~ CancerSubType, S)
    print(model)
    plot(model)
  }
}

## ----cmnew, fig.cap="Two group survival curves (improved).", fig.width=4, fig.height=4----
for (ignore in 1:5) {
  for (jgnore in 1:4) {
    cm <- CancerModel("survtester", nPossible=20, nPattern=2,
                       SURV = function(n) rnorm(n, 0, 0.3),
     		       survivalModel = sm)
    S <- rand(cm, 200)
    model <- survfit(Surv(LFU, Event) ~ CancerSubType, S)
    print(model)
    plot(model)
  }
}

## -----------------------------------------------------------------------------
for (nPos in c(5, 10, 15)) {
  for (jgnore in 1:4) {
    cm <- CancerModel("survtester", nPossible=nPos, nPattern=2,
                       SURV = function(n) rnorm(n, 0, 0.3),
     		       survivalModel = sm)
    S <- rand(cm, 200)
    model <- survfit(Surv(LFU, Event) ~ CancerSubType, S)
    print(model)
    plot(model)
  }
}


## -----------------------------------------------------------------------------
sessionInfo()

