## Copyright (C) Kevin R. Coombes and Caitlin E. Coombes, 2020

## CRITICAL NOTE: The original (1.0) version of Umpire generates
## gene expression data in the form of numeric matrices with rows
## as features=genes and columns as samples. For mixed-type clinical
## data, we need to transpose the results and use data frames
## instead of matrices. This change has numerous potential
## side-effects on things like "blur" and "rand".

####################################################
### Step 1: Create a Clinical Engine
##
## Key point: Need to figure out reasonable default parameters
## based only on the desired number of clusters and the desired
## number (NF) of features. This ioncludes the number of hits per
## cluster (set at 2, 3, or 5), the total number of possible
## hits (set at NF, NF/2, NF/3, or 20), and the prevalences (either
## set as equal or selected from a Dirichlet diostribution. Finally,
## need to figure out sensible block sizes.
##
## Original version from CEC master's thesis.
## Arguments:
##    nFeatures = number of features
##    nClusters = number of clusters/subtypes
##    weighted = logical value used to define prevalence of subtypes
##    bHyp = object of class BlockHyperParameters, if not NULL
ClinicalEngine <- function(nFeatures, nClusters, isWeighted, bHyp = NULL){
  hitfn <- ifelse(nFeatures < 15, # small feature size
                  function(n) 2,
                  ifelse(nFeatures < 45,
                         function(n) 3, # medium feature size
                         function(n) 5 # large feature size
                         )
                  )
  ## nPossible = number of possible 'hits' based on multi-hit model
  ## of cancer, where smaller numbers are well separated
  nPos <- function(nF) {
    ifelse(nF < 12,
           nF,   # no correlation; each feature will be a seaprate hit
           ifelse (nF < 50,
                   round(nF/3),
                   ifelse(nF < 100,
                          round(nF/5),
                          20)
                   )
           )
  }
  NP <- nPos(nFeatures)
  mod <- CancerModel(name="Clinical Simulation Model (Raw)",
                     nPossible = NP, # number of possible hits
                     nPattern = nClusters,        # number of subtypes
                     HIT = hitfn,
                     prevalence = Prevalence(isWeighted, nClusters)
                     )
  if (is.null(bHyp)) {
    blockInfo <- computeBlocks(nFeatures, NP)
    bHyp <- ClinicalBHP(nExtraBlocks = blockInfo$nExtra,
                        meanBlockSize = blockInfo$blockSize,
                        minBlockSize = blockInfo$blockSize)
  }
  eng <- makeBlockStructure(mod, bHyp)
}

### Auxiliary routines ###
##
## This code wass taken from Simulations_More.Rmd, which was used to
## create simulation's for CEC's master's thesis.
Prevalence <- function(weighted, k){
  if(!weighted){ # so equal balance
    a = 100
    prev <- rdirichlet(1, rep(a, k))
    as.numeric(prev)
  } else { # weighted means unequal balance
    if(k<=8){
      a = 10
      prev <- rdirichlet(1, rep(a, k))
      as.numeric(prev)
    } else if (k>=16){
      my.min = -1
      w = 5
      while(my.min<0.01){
      alphas <- c(rep(w*1,k/4), rep(w*2,k/4), rep(w*4,k/4), rep(w*8,k/4))
      prev <- rdirichlet(1, c(alphas))
      my.min <- min(prev)
    } # end while loop
      as.numeric(prev)
    } # end if/else cluster size for unequal weights
  } # end if/else statement
} # end function

computeBlocks <- function(NF, NP) { # Features, Possible hits
  quotient <- NF %/% NP
  remainder <- NF %% NP
  if (quotient == 0) {
    stop("Unable to achieve desired feature and possibhle hit parameters.")
  } else if(quotient < 3) { # no choices left
    blockSize  <-  quotient
  } else if (quotient < 6) {
    blockSize  <-  quotient - 2
  } else {
    blockSize <- quotient - 4
  }
  nExtra <- (NF - NP*blockSize) %/% blockSize
  list(blockSize = blockSize, nExtra = nExtra)
}


ClinicalBHP <- function(nExtraBlocks = NULL,
                        meanBlockSize = NULL,
                        sigmaBlockSize =  0,
                        minBlockSize =  NULL,
                        mu0 = 6,
                        sigma0 = 1.5,
                        rate = 28.11,
                        shape = 44.25,
                        p.cor = 0.6,
                        wt.cor = 5) {
  ## Shouldn't happen unless someone peeks around inside the code
  ## and calls this unexported function directly. And then they
  ## get what they deserve.
  if (is.null(nExtraBlocks)) {
    warning("Using NULL defaults")
    nExtraBlocks <-  3
    meanBlockSize <- 5
    minBlockSize <- 5
  }
  BlockHyperParameters(nExtraBlocks,
                       meanBlockSize,
                       sigmaBlockSize,
                       minBlockSize,
                       mu0,
                       sigma0,
                       rate,
                       shape,
                       p.cor,
                       wt.cor
                       )
}

####################################################
### Step 2: USe the generic 'rand' function on the ClinicalEngine
### to genrate a data set

####################################################
### Step 3: Add noise
##
## Default settings for low levels of noise in clinical data. Users can
## if they wish, go back to the original gene-expression based noise model.
## Note that this version adds _different_ levels of noise to each feature
ClinicalNoiseModel <- function(nFeatures, shape = 1.02, scale = 0.05/shape) {
  N <- NoiseModel(0, 0, 0)
  NoiseModel(nu = 0,
             tau = rgamma(nFeatures, shape=shape, scale=scale),
             phi = 0)
}

####################################################
### Step 4: Convert continous data to various data types
##
## Given the desired percentages or fractions of each data type
## and a "training" data set, this function decides which features
## to make binary or categorical. It also records the cutpoints,
## associated labels, and type of each feature. The result can be
## used to create a "MixedTypeEngine" both for the ability to
## report parameters and to generate later test sets from the same
## multivariate distribution.
setDataTypes <- function(dataset, pCont, pBin, pCat,
                         pNominal = 0.5, range = c(3, 9),
                         inputRowsAreFeatures = TRUE) {
  if (inputRowsAreFeatures) { # always want output columns as features
    dataset = t(dataset)
  }
  nFeats <- ncol(dataset)
  cutpoints <- list() # prepare to fill a list
  probs <- c(pCont, pBin, pCat)
  if (any(probs < 0)) stop("All type probabilities must be nonnegative.")
  if (sum(probs) != 1) {
    warning("Probabilities are being rescaled so they add to 1.")
    probs <- probs/sum(probs)
  }
  Type <- sample(c("continuous", "binary", "categorical"),
                 size = nFeats, replace = TRUE, prob = probs)
  isCont <- Type == "continuous"
  for (W in which(isCont)) {
    L <- list(breaks = NULL, labels = NULL, Type = "continuous")
    cutpoints[[W]] <- L
  }
  binned <- as.data.frame(dataset)
  if (any(isbin <- Type == "binary")) {
    X <- makeBinary(binned[, isbin, drop = FALSE])
    binned[, isbin] <- X$dataset
    cutpoints[isbin] <- X$cuts
  }
  if (any(iscat <- Type == "categorical")) {
    DD <- binned[, iscat, drop = FALSE]
    X <- makeCategories(DD, pNominal)
    binned[, iscat] <- X$dataset
    cutpoints[iscat] <- X$cuts
  }
  list(binned = binned, cutpoints=cutpoints)
}

# columns of `dataset` are features
makeCategories <- function(dataset, pNominal = 0.5, range = c(3,9)) {
  range <- min(range, na.rm=TRUE):max(range, na.rm=TRUE)
  nPts <- nrow(dataset)
  cutpoints <- list() # prepare to fill a list
  ## loop through columns of dataset
  for (I in 1:ncol(dataset)) {
    ## decide if variable is ordinal or nominal
    catType <- sample(c("ordinal", "nominal"), 1,
                      prob = c(1 - pNominal, pNominal))
    ## Sample a number of categories between 3 and 9.
    cc <- sample(range, 1, replace = TRUE)
    ## Create a numbered list of bin identities.
    if (catType == "ordinal") {
      id <- LETTERS[1:cc] # ordinals labeled in A, ..., I
    } else { # nominal labeled in R. ..., Z
      id <- LETTERS[18:26][sample(1:cc, cc, replace = FALSE)]
    }
#    cat("id =", id, "\n", file = stderr())
    ## Sample bin sizes from the Dirichlet distribution
    r <- rdirichlet(1, rep(20, cc)) # magic=20 determined empirically
    ## Construct a vector of percentage cutoffs
    pcuts <- c(0, cumsum(r))
    qcuts <- quantile(dataset[, I], pcuts)
#    cat(I, "before:", length(qcuts), length(id), "\n", file=stderr())
    while (any(duplicated(qcuts))) {
      W <- which(duplicated(qcuts))[1] - 1 # since qcuts are nondecreasing
      N <- which(qcuts > qcuts[W])
      if (length(N) == 0) {
        qcuts[(W+1):length(qcuts)] <- qcuts[W] + (1:(length(qcuts) - W))
      } else {
        N <- N[1]
        qcuts[W:N] <- seq(qcuts[W], qcuts[N], length = N - W + 1)
      }
    }
    ## move the extremes out to avoid problems on future data
    qcuts[1] <- -Inf
    qcuts[length(qcuts)] <- Inf
#    cat(I, "after:", length(qcuts), length(id), "\n", file=stderr())
    ## bin everything
    M <- cut(dataset[, I], breaks=qcuts, labels=id, include.lowest=TRUE)
    cutpoints[[I]] <- list(breaks  = qcuts,
                           labels = id,
                           Type = catType)
    dataset[, I] <- factor(M, levels = sort(id))
#    cat(summary(dataset[,I]), "\n", file = stderr())
  } # end loop through rows of dataet
  list(dataset = dataset, cuts = cutpoints)
} # end function

# columns of `dataset` are features
makeBinary <- function(dataset) {
  ## KRC: May need to check that BI works on single-row data matrix
  ## if it looks bimodal, split at the midpoint of modes
  bi <- bimodalIndex(t(dataset)) # use the bimodal index (Wang et al)
  my.bi <- 1.1                # and cutoff from the paper
  cutpoints <- list()
  for (I in 1:ncol(dataset)) {
    if(bi[I, "BI"] > my.bi){
      bound <- (bi[I,"mu1"] + bi[I,"mu2"])/2
    } else {
      perc <- sample(5:35, 1) # arbitrary split point
      perc <- perc/100        # converted to fraction
      # decide whether to keep lower or upper chunk
      low.cutoff  <- quantile(dataset[, I], perc)
      high.cutoff <- quantile(dataset[, I], 1 - perc)
      bound <-  ifelse(bi[I,"pi"] < 0.5, low.cutoff, high.cutoff)
    }
    bks <- c(-Inf, bound, Inf)
    lbls <- sample(c(0,1)) # randomly assign 0 or 1 to highvalues.
    X <- cut(dataset[, I], breaks = bks, labels = lbls)
    X <- as.numeric(as.character(X))
    type <- ifelse(mean(X) < 0.10, "asymmetric binary", "symmetric binary")
    cutpoints[[I]] <- list(breaks = bks, labels = lbls, Type = type)
    dataset[, I] <- X
  }
  list(dataset = dataset, cuts = cutpoints)
}

####################################################
### Step 5: Create your MixedTypeEngine
##
## Objects of this class remember the entire procedure to
## (a) simulate raw (continuous) data,
## (b) add a particular kind of noise to it, and
## (c) discretize some features.
setClass("MixedTypeEngine",
         contains = "CancerEngine",
         slots = c(noise = "NoiseModel",
                   cutpoints = "list"))

MixedTypeEngine <- function (ce, noise, cutpoints) {
  new("MixedTypeEngine",
      ce,
      noise = noise,
      cutpoints = cutpoints)
}

## It's probably worth some time to figure out all the properties
## that an MTE should really have, and put all those checks here.
setValidity("MixedTypeEngine", function(object) {
  msg <- NULL
  rightSize <- length(object@cutpoints) == nrow(object@localenv$eng)
  if (!rightSize) {
    msg <- c(msg, "Sizes of cut points and engine do not match.")
  }
  ## make sure cutpoints are sensible
  listCheck <- all (sapply(object@cutpoints, is.list))
  if (!listCheck) {
    msg <- c(msg, "All elements of 'cutpoints' must themselves be lists.")
  } else {
    hasBreaks <- all (sapply(object@cutpoints, function(X) {
      "breaks" %in% names(X)
    }))
    hasLabels <- all (sapply(object@cutpoints, function(X) {
      "labels" %in% names(X)
    }))
    hasType <- all (sapply(object@cutpoints, function(X) {
      "Type" %in% names(X)
    }))
    if (!hasBreaks) {
      msg <- c(msg, "Every element of 'cutpoints' must include 'breaks'.")
    }
    if (!hasLabels) {
      msg <- c(msg, "Every element of 'cutpoints' must include 'labels'.")
    }
    if (!hasType) {
      msg <- c(msg, "Every element of 'cutpoints' must include the 'Type' of the feature.")
    }
    if (hasBreaks & hasLabels & hasType) {
      nullBreaks <- sapply(object@cutpoints, function(OC) is.null(OC$breaks))
      nullLabels <- sapply(object@cutpoints, function(OC) is.null(OC$labels))
      isCont <- sapply(object@cutpoints, function(OC) OC$Type == "continuous")
      if (!all(isCont == nullBreaks)) {
        msg <- c(msg, "Null break list does not agree with 'continuous' type value.")
      }
      if (!all(nullBreaks == nullLabels)) {
        msg <- c(msg, "Null breaks does not match null labels.")
      }
    }
  }
  if (is.null(msg)) msg <- TRUE
  msg
})

setMethod("summary", "MixedTypeEngine", function(object, ...) {
  cat(paste("A 'MixedTypeEngine' (MTE) based on:\n"))
  callNextMethod()
  cat("\n---------------\nThe MTE uses the following noise model:\n")
  cat(summary(object@noise))
  cat("---------------\nThe MTE simulates clinical data of these types:\n")
  table(sapply(object@cutpoints, function(x) x$Type))
})



####################################################
### Step 6: Simulate new data from the same engine
##
## 'rand' generates random-vectors from a given joint distibution. This includes
## adding noise and discretizing consistently. Mostly used for test data, since
## the construction of a realistic MTE requires generating a training data set
## to get started.
setMethod("rand", "MixedTypeEngine", function(object, n,
                                              keepall = FALSE, ...) {
  ## 'chop' applies pre-existing cutpoints and factor labels to
  ## appropriate data feature-rows
  chop <- function(mtengine, hazed) {
    OC <- mtengine@cutpoints
    for (I in 1:length(OC)) {
      if (is.null(OC[[I]]$breaks) | "continuous" == OC[[I]]$Type) {
        next # do nothing for continuous data
      }
      ## othwerise, discretize following the rules
      X <-  cut(hazed[,I],
                breaks = OC[[I]]$breaks,
                labels = OC[[I]]$labels)
      hazed[,I] <- factor(X, levels=sort(OC[[I]]$labels))
    }
    hazed
  }

  ## 1. Generate continuous data
  eng <- as(object, "CancerEngine")
  dataset <- rand(eng, n)
  ## 2. add noise
  hazy <- blur(object@noise, dataset$data)
  ## 3. convert from continuous to mixed-type
  binned <- chop(object, as.data.frame(t(hazy)))
  if (keepall) {
    binned <- list(raw = dataset$data, clinical = dataset$clinical,
                   noisy = hazy, binned = binned)
  }
  binned
})





