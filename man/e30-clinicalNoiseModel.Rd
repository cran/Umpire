\name{ClinicalNoiseModel}
\alias{ClinicalNoiseModel}
\title{
A Noise Model for Clinical Data
}
\description{
  A \code{ClinicalNoiseModel} represents the additional human and measurement 
  noise that is layered on top of any biological variabilty when measuring 
  clinical variables.
}
\usage{
ClinicalNoiseModel(nFeatures, shape = 1.02, scale = 0.05/shape)
}
\arguments{
  \item{nFeatures}{An integer; the number of additive scale parameters
    to sample from the gamma distribution.
  }
  \item{shape}{
      The \code{shape} gamma hyperparameter describing the standard 
      deviation of additive noise.
  }
  \item{scale}{
      The \code{scale} gamma hyperparameter describing the standard 
      deviation of additive noise.
  }
}
\details{
  We model both additive and multiplicative noise, so that the observed
  expression of clinical variable \var{c} in sample \var{i} is given by:
  \eqn{Y_ci = S_ci + E_ci}, where \var{Y_ci} = observed expression,
  \var{S_ci} = true biological signal.
  In the ClinicalNoiseModel (as opposed to the \code{\link{NoiseModel}}), 
  we model the additive noise as \eqn{E_ci ~ N(0,tau)}, 
  without multiplicative noise or an additive bias/offset in the clinical model. 
  The standard deviation hyperparameters of the additive noise \var{tau} 
  is modeled by the gamma distribution \eqn{tau ~ Gamma(shape, scale)}
}
\value{
An object of class \code{\link{NoiseModel}}.
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
  Caitlin E. Coombes \email{caitlin.coombes@osumc.edu}
}
\seealso{
\code{\link{NoiseModel-class}},
}
\examples{
showClass("NoiseModel")

## generate a ClinicalEngine with 20 features and 4 clusters
ce <- ClinicalEngine(20, 4, TRUE)
## generate 300 simulated patients
set.seed(194718)
dset <- rand(ce, 300)

cnm <- ClinicalNoiseModel(nrow(ce@localenv$eng), shape=2, scale=0.1)
cnm

noisy <- blur(cnm, dset$data)
hist(noisy)
}
\keyword{classes}
\keyword{datagen}
