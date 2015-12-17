library(shiny)

LCG <- function(nsim,M=(2**32),a=1664525,b=1013904223,x0=1){
  X = c(x0,numeric(nsim-1))
  for(i in 1:(nsim-1)){
    X[i+1]<-(a*X[i]+b) %% M
  }
  return (X/M)
}

shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    
    n<-seq(10,10000,by=10)
    
    montecarlo<-function(n,alpha=0.05){
      x<-runif(n,0,2)
      theta<-mean(2*sqrt(4-x**2))
      z<-qnorm(alpha/2,lower.tail = F)
      varianza<-var(x)
      limsup<-theta+z*sqrt(varianza/n)
      liminf<-theta-z*sqrt(varianza/n)
      return (list(est=theta,limsup=limsup,liminf=liminf))
    }
    
    sal<-t(sapply(n,montecarlo))
    
    plot(as.numeric(sal[,1]),type = "l",xlab="nsim",ylab="result",main="Function: 2*sqrt(4-x**2)")
    lines(as.numeric(sal[,2]),col="green")
    lines(as.numeric(sal[,3]),col="red")
    abline(h=pi)
    
  })
  
  output$plot2 <- renderPlot({
    
    n<-seq(100,10000,by=10)
    
    montecarlo<-function(n,alpha=0.05){
      x<-runif(n,0,1)
      theta<-mean(4/(1+x**2))
      z<-qnorm(alpha/2,lower.tail = F)
      varianza<-var(x)
      limsup<-theta+z*sqrt(varianza/n)
      liminf<-theta-z*sqrt(varianza/n)
      return (list(est=theta,limsup=limsup,liminf=liminf))
    }
    
    sal<-t(sapply(n,montecarlo))
    
    plot(as.numeric(sal[,1]),type = "l",xlab="nsim",ylab="result",main="Function: 4/(1+x**2)")
    lines(as.numeric(sal[,2]),col="green")
    lines(as.numeric(sal[,3]),col="red")
    abline(h=pi)
    
  })
  output$plot3 <- renderPlot({
    
    n<-seq(100,10000,by=10)
    
    montecarlo<-function(n,alpha=0.05){
      x<-runif(n,0,1)
      theta<-mean(6/sqrt(4-x**2))
      z<-qnorm(alpha/2,lower.tail = F)
      varianza<-var(x)
      limsup<-theta+z*sqrt(varianza/n)
      liminf<-theta-z*sqrt(varianza/n)
      return (list(est=theta,limsup=limsup,liminf=liminf))
    }
    
    sal<-t(sapply(n,montecarlo))
    
    plot(as.numeric(sal[,1]),type = "l",xlab="nsim",ylab="result",main="Function: 6/sqrt(4-x**2)")
    lines(as.numeric(sal[,2]),col="green")
    lines(as.numeric(sal[,3]),col="red")
    abline(h=pi)
    
  })
})
