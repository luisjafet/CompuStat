require(plyr)

mc.intervals <- function(Phi, N, X.dens = runif, alpha = 0.05){
  #N: is a vector which contains different sample sizes for our estimate
  #alpha: determines the confidence intervals of level 1-alpha
  #X.dens: must be a function form which to draw N trials of X
  #Phi: is used sum
  
  results.list <- lapply(N, function(nsim){
    #MonteCarlo step
    X <- sapply(nsim, FUN = X.dens)
    PhiX <- sapply(X, Phi)
    estim <- mean(PhiX)
    S2 <- var(PhiX)
    quant <- qnorm(alpha/2, lower.tail = FALSE)
    int.upper <- estim + sqrt(S2/nsim) * quant
    int.lower <- estim - sqrt(S2/nsim) * quant
    return (data.frame(N = nsim, Estimate = estim, LI = int.lower, UI = int.upper))
  })
  
  results.table <- ldply(results.list)
  return(results.table)
  
}


set.seed(110104)
Phi <- function(x) 2*sqrt(4-x^2)
X.dens <- function(nsim) runif(nsim, 0 , 2)
N <- seq(from=1000, to=10000, by=1000)
data <- mc.intervals(Phi=Phi, N=N, X.dens=X.dens)
print(data)



