#a = 0
#b = 1000
#nsim = 10000

#crudo
#U = runif(nsim, a, b)
#estim1 = mean((b-a)*dnorm(U))

#prioritario
#U = rexp(nsim)
#estim2 = mean(dnorm(U)/dexp(U))
#a = 0
#b = 1000
#nsim = 10000

a = 0
b = 2
nsim = 100
M = 1
phi = function(x){M * exp(-M * x)}

#crudo
U = runif(nsim, a, b)
estim1 = mean((b-a)*phi(U))

#prioritario
U = rexp(nsim, 1)
estim2 = mean(phi(U)/(1-exp(-2)))


