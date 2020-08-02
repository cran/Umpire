library(Umpire)
set.seed(97531)


## all in one
sm  <-  SurvivalModel(baseHazard = 1/3)
mte <- MixedTypeEngine(ce = list(nFeatures = 40,
                                 nClusters = 4,
                                 isWeighted = FALSE,
                                 survivalModel = sm),
                       noise = list(nFeatures = 40,
                                    shape = 1.02,
                                    scale = 0.05/1.02),
                       cutpoints = list(N = 200,
                                        pCont = 0.6,
                                        pBin = 0.2,
                                        pCat = 0.2,
                                        pNominal = 0.5))

## use all defaults
mte <- MixedTypeEngine(ce = list(nFeatures = 40,
                                 nClusters = 4,
                                 isWeighted = FALSE),
                       noise = list(nFeatures = 40),
                       cutpoints = list(N = 200,
                                        pCont = 0.6,
                                        pBin = 0.2,
                                        pCat = 0.2))

## precompute everything except cutpoints
ce <- ClinicalEngine(nFeatures = 40,
                     nClusters = 4,
                     isWeighted = FALSE)
noise <- ClinicalNoiseModel(nFeatures = 40)
mte <- MixedTypeEngine(ce, noise,
                       cutpoints = list(N = 200,
                                        pCont = 0.6,
                                        pBin = 0.2,
                                        pCat = 0.2))
