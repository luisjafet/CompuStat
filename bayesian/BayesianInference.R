library(Rcpp)
library(ggplot2)

data(iris)
X <- as.matrix(cbind(1, iris[, 2:4]))
Y <- iris[, 1]

prior.sd <- function(x) dgamma(x, 5, 40)
prior.b0 <- function(x) dnorm(x, 3, .2)
prior.b1 <- function(x) dnorm(x, 3, .2)
prior.b2 <- function(x) dnorm(x, 3, .2)
prior.b3 <- function(x) dnorm(x, 3, .2)

cppFunction('
  double objdens(NumericMatrix X, NumericVector Y, NumericVector thetas){
    double lkh, logprior;
    
    NumericVector betas(4);
    betas[0]=thetas[0];
    betas[1]=thetas[1];
    betas[2]=thetas[2];
    betas[3]=thetas[3];
    aux = X * betas;

    int m = X.nrow();
    lkh=0;

    for (int i=0; i<m; i++){
      lkh += -.5 * pow((Y[i]-aux(i,_))/(2*thetas[4]), 2) - log(thetas[4]);
    }
    logprior = R::dnorm(thetas[0], 3.0,.5, true) + R::dnorm(thetas[1], 3.0,.5, true) + R::dnorm(thetas[2], 3.0,.5, true) + R::dnorm(thetas[3], 3.0,.5, true) + R::dgamma(thetas[4], 5.0, 1.0/40.0, true);
    return lkh + logprior;
}')
objdens(X, Y, c(1,2,3,4,5))

cppFunction('
  NumericVector proposal(NumericVector thetas){
    int nparam = thetas.size();
    double jump = .05; 
    NumericVector newtheta(nparam);
    for (int i=0; i<nparam; i++){
      newtheta[i] = R::rnorm(thetas[i], jump);
    }
    return newtheta;
}')
proposal(c(1,2,3,4,5))

source("BayesianMH.cpp")

nsim <- 1000
init <- c(1,2,3,4,5)
MHBayes(20, init, objdens, proposal, X, Y)
mh.samp <- MHBayes(nsim, init, objdens, proposal, X, Y)
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


