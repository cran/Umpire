\name{BlockHyperParameters-class}
\alias{BlockHyperParameters-class}
\alias{BlockHyperParameters}
\alias{makeBlockStructure}
\docType{class}
\title{The "BlockHyperParameters" Class}
\description{
  Provides tools to create a CancerEngine with block correlation structure.
  Also makes it possible to simulate paired clinical and gene expression data
  with this block structure.
}
\usage{
BlockHyperParameters(nExtraBlocks = 100,
                     meanBlockSize = 100,
                     sigmaBlockSize = 30,
                     minBlockSize = 5,
                     mu0 = 6,
                     sigma0 = 1.5,
                     rate = 28.11,
                     shape = 44.25,
                     p.cor = 0.6,
                     wt.cor = 5)
makeBlockStructure(cm, hyperp, xform = normalOffset, ...)
}
\arguments{
  \item{cm}{object of class \code{CancerModel}}
  \item{hyperp}{object of class \code{BlockHypeParameters}}
  \item{nExtraBlocks}{integer scalar specifying number of blocks not
    involved in the "hit" structure defined by the \code{CancerModel}}
  \item{meanBlockSize}{numeric scalar specifying mean number of genes in a
    correlated block}
  \item{sigmaBlockSize}{numeric scalar specifying standard deviation of the
    number of genes in a correlated block}
  \item{minBlockSize}{integer scalar specifying minimal number of genes in a
    correlated block}
  \item{mu0}{numeric scalar specifying expected mean expression level of a
    gene on the log scale}
  \item{sigma0}{numeric scalar specifying standard deviation of the mean
    expression level of a gene on the log scale}
  \item{rate}{numeric scalar specifying one of the gamma parameters}
  \item{shape}{numeric scalar specifying one of the gamma parameters}
  \item{p.cor}{numeric scalar specifying expected correlation within each
    block}
  \item{wt.cor}{numeric scalar specifying weight given to the expected block
    correlation}
  \item{xform}{A function that will be passed to the \code{alterMean}
    method}
  \item{\dots}{extra arguments that wil be passed back to the
    \code{xform} function}
}
\details{
  Our standard model for gene expression in a homogeneous sample assumes
  that the overall correlation matrix is block diagonal.  Correlation
  between genes in different blocks is assumed to be zero.  Correlation
  for genes in the same block is assumed to be a constant, but different
  correlation constants can be used in different blocks.  The actual
  correlations are assumed to arise from a beta distribution of the form
  Beta(pw, (1-p)w), where p=\code{p.cor} and w=\code{wt.cor} are two of the
  hyperparameters.

  The number of blocks is determined jointly by the \code{\link{CancerModel}},
  \code{cm}, and the hyperparameter \code{nExtraBlocks}.  The size of a
  block is assumed to arise from a normal distribution with mean given
  by \code{meanBlockSize} and standard deviaion given by
  \code{sigmaBlockSize}.  To avoid accidentally assigning non-postive
  block sizes, this distribution is truncated below by
  \code{minBlockSize}.

  The expression of each gene is assumed to come from a log-normal
  distribution with parameters describing the per-gene mean (\eqn{\mu_g})
  and standard deviation (\eqn{\sigma_g}) n the log scale.  These
  parameters, in turn, are assumed to come from hyperdistributions.
  Specifically, we assume that \eqn{\mu_g} comes from a normal distribution
  with  mean \code{mu0} and standard deviation \code{sigma0}.  We also
  assume that \eqn{\sigma_g} comes from an inverse gamma distribution with
  parameters \code{rate} and \code{shape}.

  The \code{BlockHyperParameters} class simply bundles the parameters for
  this model into a single structure. The default values are consistent
  with data we have seen from several Affymetrix microarray studies.

  The \code{makeBlockStructure} function takes a \code{CancerModel} and
  a \code{BlockHyperParameters} object as arguments and produces a
  \code{\link{CancerEngine}} object. The \code{\link{rand}} method for
  this class can be used to generate matched clinical data (with the
  structure defined by the \code{CancerModel} object) and gene
  expression data with the specified block correlation structure.
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{BlockHyperParameters} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{nExtraBlocks}:}{An integer; the number of blocks not involved in
      the "hit" structure defined by the \code{CancerModel}.}
    \item{\code{meanBlockSize}:}{A real number; the mean number of genes in a
      correlated block.} 
    \item{\code{sigmaBlockSize}:}{A real number; standard deviation of the number of
      genes in a correlated block.}
    \item{\code{minBlockSize}:}{An integer; the minimal number of genes in a
      correlated block.}
    \item{\code{mu0}:}{A real number; the expected mean expression level of a gene
      on the log scale.}
    \item{\code{sigma0}:}{A real number; the standard deviation of the mean
      expression level of a gene on the log scale.}
    \item{\code{rate}:}{Gamma parameter; see details.}
    \item{\code{shape}:}{Gamma parameter; see details.}
    \item{\code{p.cor}:}{A real number; the expected correlation within each block.}
    \item{\code{wt.cor}:}{A real number; the weight given to the
      expected block correlation.} 
  }
}
\section{Methods}{
  There are no special methods defind for this class.
}
\value{
  The \code{BlockHyperParameters} generator returns an object of class
  \code{BlockHyperParameters}.

  The function \code{makeBlockStructure} returns an object of the
  \code{\link{CancerEngine}} class.  
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
}
\seealso{
  \code{\linkS4class{CancerModel}},
  \code{\linkS4class{CancerEngine}}
}
\examples{
showClass("BlockHyperParameters")
sm <- SurvivalModel(baseHazard = 1/3, units = 52, unitName = "weeks")
cm <- CancerModel("myModel", nPossible = 10, nPattern = 5,
                  survivalModel = sm)
hyper <- BlockHyperParameters()
engine <- makeBlockStructure(cm, hyper)
outcome <- rand(engine, 100)
summary(outcome$clinical)
dim(outcome$data)
}
\keyword{classes}
\keyword{datagen}

