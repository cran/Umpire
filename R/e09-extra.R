# Copyright (C) Kevin R. Coombes, 2007-2012

setClass("BlockHyperParameters",
         slots = c(
           nExtraBlocks="numeric",    # block correlation
           meanBlockSize="numeric",   # block correlation
           sigmaBlockSize="numeric",  # block correlation
           minBlockSize="numeric",    # block correlation
           mu0="numeric",             # hyperp mean log gene expression
           sigma0="numeric",          # hyperp SD of mean log gene expression
           rate="numeric",            # gamma param for SD of gene expression
           shape="numeric",           # gamma param for SD of gene expression
           p.cor="numeric",           # beta param for within-block correlation
           wt.cor="numeric"))         # beta param for within-block correlation

BlockHyperParameters <- function(nExtraBlocks=100,
                                 meanBlockSize=100,
                                 sigmaBlockSize=30,
                                 minBlockSize=5,
                                 mu0=6,
                                 sigma0=1.5,
                                 rate=28.11,
                                 shape=44.25,
                                 p.cor=0.6,
                                 wt.cor=5) {
  new("BlockHyperParameters",
      nExtraBlocks=round(nExtraBlocks),
      meanBlockSize=meanBlockSize,
      sigmaBlockSize=sigmaBlockSize,
      minBlockSize=round(minBlockSize),
      mu0=mu0,
      sigma0=sigma0,
      rate=rate,
      shape=shape,
      p.cor=p.cor,
      wt.cor=wt.cor)
}

makeBlockStructure <- function(cm, hyperp, xform=normalOffset, ...) {
  if (!inherits(cm, "CancerModel"))
    stop("'cm' must be a CancerModel")
  if (!inherits(hyperp, "BlockHyperParameters"))
    stop("'hyperp' must define the BlockHyperParameters")
  nBlocks <- nrow(cm@hitPattern)
  # number of networks/pathways
  nTotalBlocks <- nBlocks + hyperp@nExtraBlocks
  # block size
  blockSize <- round(rnorm(nTotalBlocks,
                           hyperp@meanBlockSize,
                           hyperp@sigmaBlockSize))
  blockSize[blockSize < hyperp@minBlockSize] <- hyperp@minBlockSize
  # block corr
  p <- hyperp@p.cor
  w <- hyperp@wt.cor
  # set up the baseline Engine
  rho <- rbeta(nTotalBlocks, p*w, (1-p)*w)
  base <- lapply(1:nTotalBlocks, function(i, hyperp, rho) {
    bs <- blockSize[i]
    co <- matrix(rho[i], nrow=bs, ncol=bs)
    diag(co) <- 1
    mu <- rnorm(bs, hyperp@mu0, hyperp@sigma0)
    sigma <- matrix(1/rgamma(bs, rate=hyperp@rate, shape=hyperp@shape), nrow=1)
    covo <- co *(t(sigma) %*% sigma)
    MVN(mu, covo)
  }, hyperp=hyperp, rho=rho)
  eng <- Engine(base)
  # alter the means if there is a hit
  altered <- alterMean(eng, xform, ...)
  # return the CancerEngine object
  CancerEngine(cm=cm, base="eng", altered="altered")
}

