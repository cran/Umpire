## ------------------------------------------------------------------------
library(Umpire)

## ----seed----------------------------------------------------------------
set.seed(84503)

## ------------------------------------------------------------------------
ce <- ClinicalEngine(20, 4, isWeighted = TRUE)
summary(ce)

## ----nrow----------------------------------------------------------------
nrow(ce)
nComponents(ce)

## ------------------------------------------------------------------------
dset <- rand(ce, 300)

## ------------------------------------------------------------------------
class(dset)
names(dset)
summary(dset$clinical)

## ------------------------------------------------------------------------
class(dset$data)
dim(dset$data)

## ------------------------------------------------------------------------
cnm <- ClinicalNoiseModel(nrow(ce@localenv$eng), shape = 1.02, scale = 0.05)
summary(cnm)
noisy <- blur(cnm, dset$data)

## ------------------------------------------------------------------------
dt <- setDataTypes(dset$data,
                   pCont = 1/3, pBin = 1/3, pCat = 1/3,
                   pNominal = 0.5, range = 3:9,
                   inputRowsAreFeatures = TRUE)
names(dt)

## ------------------------------------------------------------------------
class(dt$binned)
dim(dt$binned)
summary(dt$binned)

## ------------------------------------------------------------------------
dt$cutpoints[[1]]
dt$cutpoints[[5]]

## ------------------------------------------------------------------------
cp <- dt$cutpoints
type <- sapply(cp, function(X) { X$Type })
table(type)

## ------------------------------------------------------------------------
mte <- MixedTypeEngine(ce,
                       noise = cnm,
                       cutpoints = dt$cutpoints)
summary(mte)

## ------------------------------------------------------------------------
dset2 <- rand(mte, 20)
class(dset2)
summary(dset2)

## ------------------------------------------------------------------------
dset3 <- rand(mte, 25, keepall = TRUE)
class(dset3)
names(dset3)

## ----raw-----------------------------------------------------------------
dim(dset3$raw)
summary(t(dset3$raw))
dim(t(dset3$noisy))
summary(dset3$noisy)

## ---- fig.cap="Raw and noisy data."--------------------------------------
plot(dset3$raw[5,], dset3$noisy[5,], xlab = "Raw", ylab = "Noisy", pch=16)

## ----fig.cap = "Noisy and binned data."----------------------------------
dim(dset3$binned)
summary(dset3$binned)
plot(dset3$binned[,5], dset3$noisy[5,], xlab = "Binned", ylab = "Noisy")

