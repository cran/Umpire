\name{MVN-class}
\alias{MVN-class}
\alias{MVN}
\alias{alterMean,MVN-method}
\alias{alterSD,MVN-method}
\alias{nrow,MVN-method}
\alias{rand,MVN-method}
\alias{summary,MVN-method}
\alias{covar}
\alias{correl}
\docType{class}
\title{The "MVN" Class}
\description{
  The \code{MVN} class is a tool used to generate gene expressions that
  follow multivariate normal distribution.
}
\usage{
MVN(mu, Sigma, tol = 1e-06)
covar(object)
correl(object)
\S4method{alterMean}{MVN}(object, TRANSFORM, \dots)
\S4method{alterSD}{MVN}(object, TRANSFORM, \dots)
\S4method{nrow}{MVN}(x)
\S4method{rand}{MVN}(object, n, \dots)
\S4method{summary}{MVN}(object, \dots)
}
\arguments{
  \item{mu}{numeric vector representing \var{k}-dimensional means}
  \item{Sigma}{numeric \var{k}-by-\var{k} covariance matrix containing the
    measurement of the linear coupling between every pair of random vectors}
  \item{tol}{numeric scalar roundoff error that will be tolerated when
    assessing the singularity of the covariance matrix}
  \item{object, x}{object of class \code{MVN}}
  \item{TRANSFORM}{function that takes a vector of mean expression or
    standard deviation and returns a transformed vector that can be used
    to alter the appropriate slot of the object.}
  \item{n}{numeric scalar representing number of samples to be simulated}
  \item{\dots}{extra arguments for generic or plotting routines}
}
\details{
  The implementation of \code{MVN} class is designed for efficiency when
  generating new samples, since we expect to do this several times.
  Basically, this class separates the \code{mvrnorm} function from the
  \pkg{MASS} package into several steps. The computationally expensive step
  (when the dimension is large) is the eigenvector decomposition of the
  covariance matrix. This step is performed at construction and the
  pieces are stored in the object. The \code{rand} method for \code{MVN}
  objects contains the second half of the \code{mvrnorm} function.

  Note that we typically work on expression values after taking the
  logarithm to some appropriate base. That is, the multivariate normal
  should be used on the logarithmic scale in order to contruct an engine.

  The \code{alterMean} method for an \code{MVN} simply replaces the appropriate slot by
  the transformed vector. The \code{alterSD} method for an \code{MVN} is trickier,
  because of the way the data is stored. In order to have some hope of getting
  this correct, we work in the space of the covariance matrix, \var{Sigma}.
  If we let \var{R} denote the correlation matrix and let \var{Delta} be the
  diagonal matrix whose entries are the individual standard deviations, then
               \eqn{Sigma = Delta \%*\% R \%*\% Delta}.
  So, we can change the standard deviations by replacing \var{Delta} in this
  product. We then construct a new \code{MVN} object with the old mean vector
  and the new covariance matrix.

  The \code{covar} and \code{correl} functions, respectively, calculate
  the covariance matrix and correlation matrix that underly the
  covariance matrix for the objects of class \code{MVN}. We have four
  assertions as shown below, which are tested in the examples section:
  \enumerate{
    \item \code{covar} should return the same matrix that was used
      in the function call to construct the \code{MVN} object.
    \item After applying an \code{alterMean} method, the
      covariance matrix is unchanged.
    \item The diagonal of the correlation matrix consists of all ones.
    \item After applying an \code{alterMean} or an \code{alterSD} method, the
      correlation matrix is unchanged.
  }
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{MVN} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{mu}:}{numeric vector containing the k-dimensional means}
    \item{\code{lambda}:}{numeric vector containing the square roots of
      the eigenvalues of the covariance matrix}
    \item{\code{half}:}{numeric matrix with \eqn{k*k} dimensions whose
      columns contain the eigenvectors of the covariance matrix}
  }
}
\section{Methods}{
  \describe{
    \item{alterMean(object, TRANSFORM, \dots)}{Takes an object of class
      \code{MVN}, loops over the \code{mu} slot, alters the mean as defined
      by TRANSFORM function, and returns an object of class \code{MVN} with
      altered \code{mu}.}
    \item{alterSD(object, TRANSFORM, \dots)}{Takes an object of class
      \code{MVN}, works on the diagonal matrix of the covariance matrix, alters
      the standard deviation as defined by TRANSFORM function, and reconstructs
      an object of class \code{MVN} with the old \code{mu} and
      reconstructed covariance matrix.}
    \item{nrow(x)}{Returns the number of genes (i.e, the length of the
      \code{mu} vector).}
    \item{rand(object, n, \dots)}{Generates \eqn{nrow(MVN)*n} matrix
      representing gene expressions of \code{n} samples following the
      multivariate normal distribution captured in the object of \code{MVN}.}
    \item{summary(object, \dots)}{Prints out the number of
      multivariate normal random variables in the object of \code{MVN}.}
    \item{covar(object)}{Returns the covariance matrix of the object of
      class \code{MVN}.}
    \item{correl(object)}{Returns the correlation matrix of the object of
      class \code{MVN}.}
  }
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
  Jiexin Zhang \email{jiexinzhang@mdanderson.org},
}
\seealso{
  \code{\linkS4class{Engine}},
  \code{\linkS4class{IndependentNormal}}
}
\examples{
showClass("MVN")
  \dontrun{
tolerance <- 1e-10
## Create a random orthogonal 2x2 matrix
a <- runif(1)
b <- sqrt(1-a^2)
X <- matrix(c(a, b, -b, a), 2, 2)
## Now choose random positive squared-eigenvalues
Lambda2 <- diag(rev(sort(rexp(2))), 2)
## Construct a covariance matrix
Y <- t(X) %*% Lambda2 %*% X
## Use the MVN constructor
marvin <- MVN(c(0,0), Y)
## Check the four assertions
print(paste('Tolerance for assertion checking:', tolerance))
print(paste('Covar  assertion 1:',
            all(abs(covar(marvin) - Y) < tolerance)))
mar2 <- alterMean(marvin, normalOffset, delta=3)
print(paste('Covar  assertion 2:',
            all(abs(covar(marvin) - covar(mar2)) < tolerance)))
print(paste('Correl assertion 1:',
            all(abs(diag(correl(marvin)) - 1) < tolerance)))
mar3 <- alterSD(marvin, function(x) 2*x)
print(paste('Correl assertion 1:',
            all(abs(correl(marvin) - correl(mar2)) < tolerance)))
rm(a, b, X, Lambda2, Y, marvin, mar2, mar3)
  }
}
\keyword{datagen}
\keyword{classes}
\keyword{distribution}

