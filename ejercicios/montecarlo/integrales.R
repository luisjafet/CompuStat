montecarlo <- function(nsim, alpha=0.975){
  x <- runif(nsim, 0, 2)
  phi <- 2*sqrt(4-x^2)
  theta <- mean(phi)
  z <- qnorm(alpha/2, lower.tail = FALSE)
  s <- var(phi)
  limsup <- theta + z * sqrt(s/nsim)
  liminf <- theta - z * sqrt(s/nsim)
  return(data.frame(est=phi, limsup=limsup, liminf=liminf))
}

data <- data.frame()
n <- 1:100
for(i in n){
  data <- rbind(data, montecarlo(i))
}

#plot(data[,1], type="l", col="white")
#lines(data[,2], col="red")
#lines(data[,3], col="green")

trapecio <- function(n, fun, a, b){
  pnts <- seq(a, b, (b-a)/n)
  integral <- 0
  for (x in 1:n){
    integral <- integral + (fun(pnts[x]) + fun(pnts[x])) * (b-a/2*n) 
  }
  return(integral)
}

trapecio(1000, function(x){4*sqrt(1-x^2)},0,1)



