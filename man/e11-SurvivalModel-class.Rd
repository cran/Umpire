\name{SurvivalModel-class}
\alias{SurvivalModel-class}
\alias{SurvivalModel}
\alias{rand,SurvivalModel-method}
\docType{class}
\title{The "SurvivalModel" Class}
\description{
  A \code{SurvivalModel} class represents the information for simulating
  survival times for each patient.
}
\usage{
SurvivalModel(baseHazard = 1/5,
              accrual = 5,
              followUp = 1,
              units = 12,
              unitName = "months")
\S4method{rand}{SurvivalModel}(object, n, beta = NULL, \dots) 
}
\arguments{
  \item{baseHazard}{numeric scalar describing the underlying hazard rate at
    baseline levels of covariates}
  \item{accrual}{numeric scalar representing number of patient accrual years}
  \item{followUp}{numeric scalar representing length of follow up, in years}
  \item{units}{numeric scalar representing number of units per year where
    units are specified by \code{unitName}}
  \item{unitName}{character string representing the \code{unit} argument type}
  \item{object}{object of class \code{SurvivalModel}}
  \item{n}{numeric scalar specifying quantity of random numbers}
  \item{beta}{numeric vector specifying beta parameters for patients}
  \item{\dots}{extra arguments for generic routines}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{SurvivalModel} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{baseHazard}:}{see corresponding argument above}
    \item{\code{accrual}:}{see corresponding argument above}
    \item{\code{followUp}:}{see corresponding argument above}
    \item{\code{units}:}{see corresponding argument above}
    \item{\code{unitName}:}{see corresponding argument above}
  }
}
\section{Methods}{
  \describe{
    \item{rand(object, n, beta, \dots)}{Simulate survival data for \code{n}
      patients given \code{beta}.}
  }
}
\value{
  The \code{SurvivalModel} generator returns an object of class
  \code{SurvivalModel}.

  The \code{rand} method returns a data.frame with components:
    \item{LFU}{time to event}
    \item{Event}{whether the data is censored}
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
  \code{\linkS4class{CancerModel}}
}
\examples{
showClass("SurvivalModel")
sm <- SurvivalModel()
## Generate data from base model
outcome <- rand(sm, 100)
summary(outcome)
## Generate data from five classes with different beta parameters
beta <- rep(rnorm(5, 0, 2), each = 20)
outcome <- rand(sm, 100, beta = beta)
summary(outcome)
}
\keyword{classes}
\keyword{datagen}

