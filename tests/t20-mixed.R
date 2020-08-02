library(Umpire)
set.seed(97531)

## Need to generate a gene expression data set to get started.
## So, we must start with a clinical version of a cancer engine

## the logical 'isWeighted' argument determines if prevalences are equal or not
ce <- ClinicalEngine(20, 4, FALSE)
summary(ce)                # prevalences should be the same
round(ce@cm@prevalence, 2) # good
ce <- ClinicalEngine(20, 4, TRUE)
summary(ce)                # prevalences should be varied
round(ce@cm@prevalence, 2) # good

nComponents(ce)
N <- nrow(ce) # fixed!
N # should equal 20, as requested by the user

## Now generate a data set
dset <- rand(ce, 300)
class(dset)
names(dset)
summary(dset$clinical)
dim(dset$data) # 20 features, 300 samples

## Must add noise before making a mixed-type engine
cnm <- ClinicalNoiseModel(N) # default shape and scale
noisy <- blur(cnm, dset$data)

## Now we set the data types
dt <- makeDataTypes(dset$data, 1/3, 1/3, 1/3, 0.3, range = c(3, 9))
cp <- dt$cutpoints
type <- sapply(cp, function(X) { X$Type })
table(type)
sum(is.na(type))
length(type)
class(dt$binned)
dim(dt$binned)
summary(dt$binned)

## Use the pieces from above to create an MTE.
mte <- MixedTypeEngine(ce, noise = cnm, cutpoints = dt$cutpoints)
# and generate some data
R <- rand(mte, 20)
summary(R)
