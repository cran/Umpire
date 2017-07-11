\name{NormalVsCancer}
\alias{NormalVsCancerModel}
\alias{NormalVsCancerEngine}

\title{Simulating Cancer Versus Normal Datasets}
\description{
  These functions are useful for simulating data that compares a
  homogeneous "cancer" group to a homogeneous "normal" group of
  samples.
}
\usage{
NormalVsCancerModel(nBlocks, survivalModel=NULL, name="NormalVsCancer")
NormalVsCancerEngine(nBlocks, hyperp) 
}
\arguments{
  \item{nBlocks}{scalar integer representing number of correlated blocks
    that are differentially expressed between cancer and normal}
  \item{survivalModel}{a \code{SurvivalModel} object}
  \item{name}{character string specifying name of the object being created}
  \item{hyperp}{object of class \code{BlockHyperParameters} that describes
    the block correlation structure.}
}
\details{
  The simplest simulation model assumes that we are comparing two
  homogeneous groups.
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
  Jiexin Zhang \email{jiexinzhang@mdanderson.org},
}
\seealso{
  \code{\linkS4class{BlockHyperParameters}},
  \code{\linkS4class{CancerEngine}},
  \code{\linkS4class{CancerModel}}
}
\examples{
nvc <- NormalVsCancerModel(10)
summary(nvc)
rand(nvc, 10)
rand(nvc, 10, balance=TRUE)
engine <- NormalVsCancerEngine(10)
dset <- rand(engine, 10, balance=TRUE)
}

