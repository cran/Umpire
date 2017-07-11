\name{CancerEngine-class}
\alias{CancerEngine-class}
\alias{CancerEngine}
\alias{rand,CancerEngine-method}
\alias{summary,CancerEngine-method}
\docType{class}
\title{The "CancerEngine" Class}
\description{
  A CancerEngine combines a CancerModel (which defines the combinatorics
  of hits that produce cancer subtypes) with a pair of gene expression
  Engines that can be used to simulate microarray data depending on the
  presence or absence of different hits.
}
\usage{
CancerEngine(cm, base, altered)
\S4method{summary}{CancerEngine}(object, \dots)
}
\arguments{
  \item{cm}{object of class \code{CancerModel}}
  \item{base}{character string giving the name of an \code{\link{Engine}}
    or \code{\link{EngineWithActivity}}, or an object of class \code{Engine}.
    Represents the expected gene expression in the absence of any hits.}
  \item{altered}{character string giving the name of an \code{\link{Engine}}
    or \code{\link{EngineWithActivity}}, or an object of class \code{Engine}.
    Represents the expected gene expression in the presence of hits.}
  \item{object}{object of class \code{CancerEngine}}
  \item{\dots}{extra arguments for generic routines}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{CancerEngine} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{cm}:}{A \code{\link{CancerModel}} object.}
    \item{\code{base}:}{Object of class "character" giving the name of
      an \code{\link{Engine}} or \code{\link{EngineWithActivity}}.
      Represents the expected gene expression in the absence of any hits.}
    \item{\code{altered}:}{Object of class "character" giving the name of
      an \code{\link{Engine}} or \code{\link{EngineWithActivity}}.
      Represents the expected gene expression in the presence of hits.}
    \item{\code{localenv}:}{Object of class \code{"environment"}; used
      in the internal implementation. }
  }
}
\section{Methods}{
  \describe{
    \item{rand}{\code{signature(object = "CancerEngine")}: :TODO: }
    \item{summary}{\code{signature(object = "CancerEngine")}: :TODO: }  }
}
\references{
  Zhang J, Coombes KR.\cr
  \emph{Sources of variation in false discovery rate estimation include
  sample size, correlation, and inherent differences between groups.}\cr
  BMC Bioinformatics. 2012; 13 Suppl 13:S1. 
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
}
\seealso{
  \code{\linkS4class{CancerModel}}
}
\examples{
showClass("CancerEngine")
set.seed(391629)
## Set up survival outcome; baseline is exponential
sm <- SurvivalModel(baseHazard=1/5, accrual=5, followUp=1)
## Build a CancerModel with 6 subtypes
nBlocks <- 20    # number of possible hits
cm <- CancerModel(name="cansim",
                  nPossible=nBlocks,
                  nPattern=6,
                  OUT = function(n) rnorm(n, 0, 1), 
                  SURV= function(n) rnorm(n, 0, 1),
                  survivalModel=sm)
## Include 100 blocks/pathways that are not hit by cancer
nTotalBlocks <- nBlocks + 100
## Assign values to hyperparameters
## block size
blockSize <- round(rnorm(nTotalBlocks, 100, 30))
## log normal mean hypers
mu0    <- 6
sigma0 <- 1.5
## log normal sigma hypers
rate   <- 28.11
shape  <- 44.25
## block corr
p <- 0.6
w <- 5
## Set up the baseline Engine
rho <- rbeta(nTotalBlocks, p*w, (1-p)*w)
base <- lapply(1:nTotalBlocks,
               function(i) {
                 bs <- blockSize[i]
                 co <- matrix(rho[i], nrow=bs, ncol=bs)
                 diag(co) <- 1
                 mu <- rnorm(bs, mu0, sigma0)
                 sigma <- matrix(1/rgamma(bs, rate=rate, shape=shape), nrow=1)
                 covo <- co *(t(sigma) \%*\% sigma)
                 MVN(mu, covo)
               })
eng <- Engine(base)
## Alter the means if there is a hit
altered <- alterMean(eng, normalOffset, delta=0, sigma=1)
## Build the CancerEngine using character strings
object <- CancerEngine(cm, "eng", "altered")
## Or build it using the actual Engine components
ob <- CancerEngine(cm, eng, altered)
summary(object)
summary(ob)
## Simulate the data
dset <- rand(object, 20)
summary(dset$clinical)
summary(dset$data[, 1:3])
}
\keyword{classes}
\keyword{datagen}

