library(Rcpp)
library(ggplot2)

# ESTIMATION OF A MEAN
data(iris)

# BAYESIAN APPROACH

# Suppose some researchers believes from previous studies
# that the mean and variance must be more less behaves as follows
# mu ~ NORMAL(mean=6, sd=4) 
# sigma ~ Gamma(rate=5, shape=1)  
prior.mean <- function(x) dnorm(x, 3, .2)
prior.sd <- function(x) dgamma(x, 5, 40)


prior.b0 <- function(x) dnorm(x, 3, .2)
prior.b1 <- function(x) dnorm(x, 3, .2)
prior.b2 <- function(x) dnorm(x, 3, .2)
prior.b3 <- function(x) dnorm(x, 3, .2)


# OUR DATA WILL BE IN MATRIX FORM!!! MANDATORY!!!! IF YOU DON'T LIKE IT
# YOU CAN USE A WRAPPER FUNCTION IN R THAT CALLS THE C FUNCTIONS AND TRANSFORMS
# A VECTOR OR A DATAFRAME INTO A NUMERICAL MATRIX. BUT THIS WAY WE CAN 
# GENERALISE THIS EXAMPLE IT TO REGRESSIONS AND LARGE DATA!
data <- matrix(iris$Sepal.Width, ncol=1)

# WE WILL USE AND MCMC METHOD.
# NEED 
# 1: A objective density: 2) a proposal density
# Recall obj ~~ L(theta|X)prior(X)
# But as we want logarithms, we have log(obj) = log(L) + log(prior)

# 1)
cppFunction('
  double objdens(NumericMatrix data, NumericVector theta){
    double lkh, logprior;
    int m=data.nrow();
    NumericVector X(m);
    X = data(_,0); // In this example is redundant but helps to generalise
    // Compute loglikelihood
    lkh=0;
    for (int i=0; i<m; i++){
      lkh += -.5*pow((X[i]-theta[0])/theta[1],2)-log(theta[1]);
    }
    // Compute logprior
    logprior = R::dnorm(theta[0], 3.0,.5, true) +  R::dgamma(theta[1], 5.0, 1.0/40.0, true);
    // Log of target density
    return lkh + logprior;
}')
objdens(data, c(2,3))

# 2) Proposal: random walk in the same dimension as the number of parameters
cppFunction('
  NumericVector proposal(NumericVector theta){
    int nparam = theta.size();
    double jump = .1; 
    NumericVector newtheta(nparam);
    for (int i=0; i<nparam; i++){
      newtheta[i] = R::rnorm(theta[i], jump);
    }
    return newtheta;
}')
proposal(c(1,2))


# 3) METROPOLIS

source("BayesianMH.cpp")

nsim <- 1000
init <- c(3, 1)
MHBayes(20, init, objdens, proposal, data)
mh.samp <- MHBayes(nsim, init, objdens, proposal, data)
estims <- mh.samp$theta

#  SOME DIAGNOSTIC IMPORTANT STUFF
#  Exploration graph:
library(calibrate)
pts <- seq(1,100,by=5)
plot(estims[pts, ], type="l", xlab="mean", ylab="sd")
textxy(estims[pts,1], estims[pts,2], pts)
cor(estims)
### 1) REJECTION RATES
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
rej.rate <- cumsum(rejections)/cumsum(trials)
plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
plot(trials[-1], type="l", main="Number of trials")
### 2) AUTOCORRELATION
acf(estims[ , 1])
acf(estims[ , 2]) # WARNING HERE!
# burnin and subsampling
burnin <- 100
estims <- estims[-(1:burnin), ]
thinning <- 2 
# OBS: thinning is rarely usefull!!!! check that nothing changes
# sub <- sample.int(nsim-burnin, size=round(thinning*nsim))
# estims <- estims[sub, ]
# acf(estims[ , 1])
# acf(estims[ , 2]) 


# LET'S COMPARE PRIORS AND POSTERIORS AND DO INFERENCE

hist(estims[ ,1], prob=TRUE, xlim=c(2.5,3.5), breaks=20, col="lightgreen",
     main="Histogram and Posterior(blue) vs Prior(red) of the Mean") # posterior distribution of mean
plot(prior.mean, xlim=c(2.5,3.5), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,1]), col="darkblue", lwd="2")

hist(estims[ ,2], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of the s.d.") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,2]), col="darkblue", lwd="2")

mean(estims[ ,1]) # approx. mean-value of the posterior of mean
mean(estims[ ,2]) # approx. mean-value of the posterior of standard deviation

# CERTAINTY INTERVALS
intervals3 <- quantile(estims[ ,1], c(alpha/2, 1-alpha/2))
intervals3
quantile(estims[ ,2], c(alpha/2, 1-alpha/2)) # ALSO FOR SSSDDDD


