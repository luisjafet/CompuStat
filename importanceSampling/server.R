library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(parallel)

crudo<-function(x){
  runif(x,0,2)
}

aux <- function (p, N, X.dens=runif, alpha=0.05){
  result.list <- lapply(N,function(nsim){
    X<-runif(N,0,1)
    quant <- qnorm(alpha/2,lower.tail=FALSE)
    int.up <- mean(sapply(X,p)) + sqrt(var(sapply(X,p))/nsim)*quant
    int.low <- mean(sapply(X,p)) - sqrt(var(sapply(X,p))/nsim)*quant
    return(data.frame(N = nsim, Estimate =  mean(sapply(X,p)), LI = int.low, UI = int.up))
  })
  return (ldply(result.list) %>% mutate(i = row_number()))
}

aux.is <- function (p, N, X.dens=runif,g=dunif, alpha=0.05){
  result.list <- lapply(N,function(nsim){
    X<- X.dens(N)
    quant <- qnorm(alpha/2,lower.tail=FALSE)
    int.upper <- mean(sapply(X,p)/sapply(X,g)) + sqrt(var(sapply(X,p))/nsim)*quant
    int.lower <- mean(sapply(X,p)/sapply(X,g)) - sqrt(var(sapply(X,p))/nsim)*quant
    return(data.frame(N = nsim, Estimate = mean(sapply(X,p)/sapply(X,g)), LI = int.lower, UI = int.upper))
  })
  results.table <- ldply(result.list)%>% mutate(i = row_number())
  return (results.table)
}

shinyServer(function(input, output) {
  
  
  N <- reactive(seq(100,input$N,by=10))
  
  l<- reactive({
    p <- function(x){
      input$m*exp(-(input$m)*x)
    }
    exp.is<-function(x,l){ X <- -(1/l)*log(1 - (1 - exp(-2*(l)))*runif(x, 0, 1) )   }
    g=function(x,l) dexp(x,rate=l)/(1-exp(-2*(l))) 
    l.v<-seq(.1,5*input$m,by=0.1)
    result<-lapply(l.v,function(y){
      X<- exp.is(input$N,y)
      return(mean(sapply(X,p)/g(X,y)))
    })
    results.table <- ldply(result) %>% mutate(l = l.v,Estim=V1)
  })
  
  crudo.d<-reactive({
    p <- function(x){
      input$m*exp(-(input$m)*x)
    } 
    aux(p,N())   
  })
  
  
  is.b<-reactive({
    p <- function(x){
      input$m*exp(-(input$m)*x)
    }
    
    beta.is<-function(x){
      2*rbeta(x,1,3)
    }
    
    g=function(x){
      (.5)*dbeta(.5*x,1,3)
    }
    aux.is(p,N(),X.dens=beta.is,g=g)
  })
  
  is.e<-reactive({
    p <- function(x){
      input$m*exp(-(input$m)*x)
    }
    exp.is<-function(x){
      U <- runif(x, 0, 1) 
      X <- -(1/input$l)*log(1 - (1 - exp(-2*(input$l)))*U)
    }
    g=function(x){
      dexp(x,rate=input$l)/(1-exp(-2*(input$l)))
    }
    aux.is(p,N(),X.dens=exp.is,g=g)
    
  })
  
  output$crudo <- renderPlot({
    dat <- crudo.d() %>% mutate(i = row_number(), method='crudo')
    ggplot(dat) +
      geom_ribbon(aes(i,ymin=LI,ymax=UI), alpha=0.5,,colour="green") +
      geom_line(aes(i,Estimate)) +
      facet_wrap(~method, nrow = 2) + geom_hline(yintercept=1-exp(-2*input$m))
  })
  
  output$exponencial <- renderPlot({
    dat <- is.e() %>% mutate(i = row_number(), method='exponencial')
    ggplot(dat) +
      geom_ribbon(aes(i,ymin=LI,ymax=UI), alpha=0.5,,colour="green") +
      geom_line(aes(i,Estimate)) +
      facet_wrap(~method, nrow = 2) + geom_hline(yintercept=1-exp(-2*input$m))
  })
  
  output$beta <- renderPlot({
    dat <- is.b() %>% mutate(i = row_number(), method='beta')
    ggplot(dat) +
      geom_ribbon(aes(i,ymin=LI,ymax=UI), alpha=0.5,colour="green") +
      geom_line(aes(i,Estimate)) +
      facet_wrap(~method, nrow = 2) + geom_hline(yintercept=1-exp(-2*input$m))
  })
  
  output$lam<-renderPlot({
    dat<- l() %>% mutate(method='lambda')
    ggplot(dat) + 
      geom_line(aes(l,Estim)) + 
      facet_wrap(~method, nrow = 2) +
      geom_hline(yintercept=1-exp(-2*input$m)) 
    
  })
  
})