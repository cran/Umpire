library(Umpire)
# set seed to ensure reproducibility
set.seed(63582)

sm <- SurvivalModel(baseHazard = 1/5, # default 1/5 inverse years
                    accrual = 5,      # default 5 years
                    followUp = 1,     # default 1 years
                    units = 12, unitName = "months")

sdata <- rand(sm, 100)
class(sdata) == "data.frame"
all( dim(sdata) == c(100, 2) )
summary(sdata) # median raw survival == 29 months, 44 events out of 100 patients
if(require(survival)) {
  model <- survfit(Surv(LFU, Event) ~ 1, sdata)
  print(model)
  plot(model)
}

sm <- SurvivalModel(1/2, 5, 2, 12, "months")
sdata <- rand(sm, 100)
class(sdata) == "data.frame"
all( dim(sdata) == c(100, 2) )
summary(sdata) # median raw survival == 29 months, 44 events out of 100 patients
if(require(survival)) {
  model <- survfit(Surv(LFU, Event) ~ 1, sdata)
  print(model)
  plot(model)
}

## Now test how SurvivalModel works in the CancerModel
## return to defaults
sm <- SurvivalModel(baseHazard = 1/5, # default 1/5 inverse years
                    accrual = 5,      # default 5 years
                    followUp = 1,     # default 1 years
                    units = 12, unitName = "months")
R <- rand(sm, 200)
summary(R)

cm <- CancerModel("survtester", nPossible=20, nPattern=2,
                  SURV = function(n) rnorm(n, 0, 0.3),
                  survivalModel = sm)
S <- rand(cm, 200)
summary(S)
attach(S)
table(CancerSubType, Outcome)
detach()
if(require(survival)) {
  model <- survfit(Surv(LFU, Event) ~ CancerSubType, S)
  print(model)
  plot(model, col=2:3)
}

## Now test how SurvivalModel works in the ClinicalEngine

ce <- ClinicalEngine(20, 4, TRUE, survivalModel = sm)
summary(ce)
X <-  rand(ce, 200)
summary(X$clinical)
