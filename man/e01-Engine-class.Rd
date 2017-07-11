\name{Engine-class}
\alias{Engine-class}
\alias{Engine}
\alias{alterMean,Engine-method}
\alias{alterSD,Engine-method}
\alias{nrow,Engine-method}
\alias{rand,Engine-method}
\alias{summary,Engine-method}
\alias{nComponents}
\docType{class}
\title{The "Engine" Class}
\description{
  The \code{Engine} class is a tool (i.e., an algorithm) used to simulate
  vectors of gene expression from some underlying distribution.
}
\usage{
Engine(components)
nComponents(object)
\S4method{alterMean}{Engine}(object, TRANSFORM, \dots)
\S4method{alterSD}{Engine}(object, TRANSFORM, \dots)
\S4method{nrow}{Engine}(x)
\S4method{rand}{Engine}(object, n, \dots)
\S4method{summary}{Engine}(object, \dots)
}
\arguments{
  \item{components}{object of class \code{list}, each element of which
    contains the parameters for the underlying distribution that the
    gene expression follows. A component can be viewed as a special case
    of an engine that only has one component.}
  \item{object, x}{object of class \code{Engine}}
  \item{TRANSFORM}{function takes as its input a vector of mean expression
    or standard deviation and returns a transformed vector that can be used
    to alter the appropriate slot of the object.}
  \item{n}{numeric scalar representing number of samples to be simulated}
  \item{\dots}{extra arguments for generic or plotting routines}
}
\details{
  In most cases, an engine object is an instantiation of a more general
  family or class that we call an ABSTRACT ENGINE. Every abstract engine
  is an ordered list of components, which can also be thought of as an
  engine with parameters. We instantiate an engine by binding all the
  free parameters of an abstract engine to actual values. Note that
  partial binding (of a subset of the free parameters) produces another
  abstract engine.
  
  For all practical purposes, a COMPONENT should be viewed as an
  irreducible abstract engine. Every component must have an IDENTIFIER
  that is unique within the context of its enclosing abstract engine.
  The identifer may be implicitly taken to be the position of the
  component in the ordered list.
  
  We interpret an \code{Engine} as the gene expression generator for a
  homogenous population; effects of cancer on gene expression are modeled
  at a higher level.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Engine",
    components=components)}, or use the \code{Engine} generator function.
  Every engine is an ordered list of components, which generates a contiguous
  subvector of the total vector of gene expression.
}
\section{Methods}{
  \describe{
    \item{alterMean(object, TRANSFORM, \dots)}{Takes an object of class
      \code{Engine}, loops over the components in the \code{Engine}, alters
      the mean as defined by \code{TRANSFORM} function, and returns a modified
      object of class \code{Engine}.}
    \item{alterSD(object, TRANSFORM, \dots)}{Takes an object of class
      \code{Engine}, loops over the components in the \code{Engine}, alters
      the standard deviation as defined by \code{TRANSFORM} function, and
      returns a modified object of class \code{Engine}.}
    \item{nrow(x)}{Counts the total number of genes (i.e, the
      length of the vector the \code{Engine} will generate).}
    \item{rand(object, n, \dots)}{Generates \eqn{nrow(Engine)*n} matrix
      representing gene expressions of \code{n} samples following the
      underlying distribution captured in the object of \code{Engine}.}
    \item{summary(object, \dots)}{Prints out the number of components included
      in the object of \code{Engine}.}
  }
}
\value{
  The \code{Engine} generator returns an object of class \code{Engine}.
  
  The \code{alterMean} method returns an object of class \code{Engine} with
  altered mean.
  
  The \code{alterSD} method returns an object of class \code{Engine} with
  altered standard deviation.
  
  The \code{nrow} method returns the number of genes (i.e, the length of the
  vector) the \code{Engine} object will generate.
  
  The \code{rand} method returns \eqn{nrow(Engine)*n} matrix representing the
  expressions of \code{nrow(Engine)} genes and \code{n} samples.
  
  The \code{summary} method prints out the number of components included
  in the \code{Engine} object.
  
  The \code{nComponents} method returns the number of components in the
  \code{Engine} object.
}
\references{
  Zhang J, Coombes KR.\cr
  \emph{Sources of variation in false discovery rate estimation include
  sample size, correlation, and inherent differences between groups.}\cr
  BMC Bioinformatics. 2012; 13 Suppl 13:S1. 
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
  Jiexin Zhang \email{jiexinzhang@mdanderson.org},
}
\seealso{
  \code{\linkS4class{EngineWithActivity}}
}
\examples{
showClass("Engine")
nComp <- 10
nGenes <- 100
comp <- list()
for (i in 1:nComp) {
  comp[[i]] <- IndependentNormal(rnorm(nGenes/nComp, 6, 1.5),
                                 1/rgamma(nGenes/nComp, 44, 28))
}
myEngine <- Engine(comp)
nrow(myEngine)
nComponents(myEngine)
summary(myEngine)
myData <- rand(myEngine, 5)
dim(myData)
summary(myData)
OFFSET <- 2
myEngine.alterMean <- alterMean(myEngine, function(x){x+OFFSET})
myData.alterMean <- rand(myEngine.alterMean, 5)
summary(myData.alterMean)
SCALE <- 2
myEngine.alterSD <- alterSD(myEngine, function(x){x*SCALE})
myData.alterSD <- rand(myEngine.alterSD, 5)
summary(myData.alterSD)
}
\keyword{datagen}
\keyword{classes}

