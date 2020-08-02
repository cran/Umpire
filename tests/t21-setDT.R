library(Umpire)
set.seed(97531)

ce <- ClinicalEngine(666, 4, FALSE)
N <- nrow(ce)
dset <- rand(ce, 300)
cnm <- ClinicalNoiseModel(N) # default shape and scale
noisy <- blur(cnm, dset$data)
# next line used to throw a subtle rounding error
dt <- makeDataTypes(dset$data, 1/3, 1/3, 1/3, 0.3,
                   range = c(3, 9), exact = FALSE)


testfun <- function(NF, exact) {
  ce <- ClinicalEngine(NF, 4, FALSE)
  N <- nrow(ce)
  dset <- rand(ce, 300)
  cnm <- ClinicalNoiseModel(N) # default shape and scale
  noisy <- blur(cnm, dset$data)
  dt <- makeDataTypes(dset$data, 1/3, 1/3, 1/3, 0.3,
                     range = c(3, 9), exact = exact)
  invisible(dt)
}

dt <- testfun(27, exact = FALSE)
dim(dt$binned)
table( sapply(dt$cutpoints, function(x) x$Type) )

dt <- testfun(27, exact = TRUE)
dim(dt$binned)
table( sapply(dt$cutpoints, function(x) x$Type) )

dt <- testfun(81, exact = TRUE)
dim(dt$binned)
table( sapply(dt$cutpoints, function(x) x$Type) )

dt <- testfun(28, exact = TRUE)
dim(dt$binned)
table( sapply(dt$cutpoints, function(x) x$Type) )

dt <- testfun(29, exact = TRUE) # can only get 28 since all blocks are equal size
dim(dt$binned)
table( sapply(dt$cutpoints, function(x) x$Type) )

dt <- testfun(500, exact = FALSE)
dim(dt$binned)
table( sapply(dt$cutpoints, function(x) x$Type) )


