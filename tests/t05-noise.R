library(Umpire)
# set seed to ensure reproducibility
suppressWarnings( RNGversion("3.5.3") )
set.seed(823458)
# create a noise model
noise <- NoiseModel(0, 1, 0.1)
# set parameters from independent normal hyperparameters
nGenes <- 5
for (i in 1:2) {
  mu <- rnorm(nGenes, 6, 1)
  sigma <- 1/rgamma(nGenes, shape=14, rate=5)
  temp <- IndependentNormal(mu, sigma)
  assign(paste('ind', i, sep=''), temp, 1)
}
# create a correlation matrix
a <- runif(1)
b <- sqrt(1-a^2)
X <- matrix(c(a, b, -b, a), 2, 2)
Lambda2 <- diag(rev(sort(rexp(2))), 2)
Y <- t(X) %*% Lambda2 %*% X
# create  multivariate normal generator
marvin <- MVN(c(0,0), Y)
# create a  multi-component engine
#old: engine <- Engine(ind1, ind2, marvin)
engine <- Engine(list(ind1, ind2, marvin))
# generate data from the engine
x <- rand(engine, 10)
# add noise
y <- blur(noise, x)
print(summary(as.vector(x-y)))
# cleanup
rm(ind1, ind2, mu, sigma, temp, i, nGenes,
   a, b, Lambda2, X, Y, marvin, engine,
   x, y, noise)

  

