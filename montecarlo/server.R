
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

exp_fun <- function(nsim, l=.5){
  X <- c(1, numeric(nsim)-1)
  for(i in 1:(nsim)){
    u <- runif(1)
    X[i+1] <- -1 * (log(1-u)/l)
  }
  return (X)
}


shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    x    <- exp_fun(input$simulaciones, input$lambda)
    bins <- seq(min(x), max(x), length.out = input$simulaciones)
    hist(x, col = "darkgreen", border = "white")
    table_created <- t(t(sort(x, decreasing=FALSE))) 
    output$table <- renderTable(table_created)
  })

})
