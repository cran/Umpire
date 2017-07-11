\name{IndependentLogNormal-class}
\alias{IndependentLogNormal-class}
\alias{IndependentLogNormal}
\alias{nrow,IndependentLogNormal-method}
\alias{rand,IndependentLogNormal-method}
\alias{summary,IndependentLogNormal-method}
\docType{class}
\title{The "IndependentLogNormal" Class}
\description{
  The \code{IndependentLogNormal} class is a tool used to generate gene
  expressions that follow log normal distribution, because the true expression
  value follows log normal in our model.
}
\usage{
IndependentLogNormal(logmu,logsigma)
\S4method{nrow}{IndependentLogNormal}(x)
\S4method{rand}{IndependentLogNormal}(object, n, \dots)
\S4method{summary}{IndependentLogNormal}(object, \dots)
}
\arguments{
  \item{logmu}{numeric vector specifying the mean expression values on the
    logarithmic scale.}
  \item{logsigma}{numeric vector specifying the standard deviation of the
    gene expression values on the logarithmic scale}
  \item{object, x}{object of class \code{IndependentLogNormal}}
  \item{n}{numeric scalar specifying number of samples to be simulated}
  \item{\dots}{extra arguments for generic or plotting routines}
}

\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{IndependentLogNormal} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{logmu}:}{numeric vector containing the mean expression
      values on the logarithmic scale}
    \item{\code{logsigma}:}{numeric vector containing the standard deviation
      of the gene expression values on the logarithmic scale}
  }
}
\section{Methods}{
  \describe{
    \item{nrow(x)}{Returns the number of genes (i.e, the length of the
      \code{logmu} vector).}
    \item{rand(object, n, \dots)}{Generates nrow(IndependentLogNormal)*n matrix
      representing gene expressions of n samples following log normal
      distribution captured in the object of \code{IndependentLogNormal}.}
    \item{summary(object, \dots)}{Prints out the number of independent log
      normal random variables in the object of \code{IndependentLogNormal}.}
  }
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
  Jiexin Zhang \email{jiexinzhang@mdanderson.org},
}
\seealso{
  \code{\linkS4class{Engine}},
  \code{\linkS4class{IndependentNormal}},
  \code{\linkS4class{MVN}}
}
\examples{
showClass("IndependentLogNormal")
nGenes <- 20
logmu <- rnorm(nGenes, 6, 1)
logsigma <- 1/rgamma(nGenes, rate=14, shape=6)
ln <- IndependentLogNormal(logmu, logsigma)
nrow(ln)
summary(ln)
if (any(logmu - ln@logmu)) {
  print('means do not match')
} else {
  print('means verified')
}
if (any(logsigma - ln@logsigma)) {
  print('standard deviations do not match')
} else {
  print('sd verified')
}
x <- rand(ln, 1000)
print(dim(x))
 
print(paste("'ln' should be valid:", validObject(ln)))
ln@logsigma <- 1:3 # now we break it
print(paste("'ln' should not be valid:", validObject(ln, test=TRUE)))
tmp.sd <- sqrt(apply(log(x), 1, var))
plot(tmp.sd, logsigma)
tmp.mu <- apply(log(x), 1, mean)
plot(tmp.mu, logmu)
rm(nGenes, logmu, logsigma, ln, x, tmp.mu, tmp.sd)
}
\keyword{datagen}
\keyword{classes}
\keyword{distribution}

