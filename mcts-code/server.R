library(shiny)
source("connect-game-code.R")
source("mcts.R")




shinyServer(function(input, output) {
  
  
  
  
 
  
  
  
  
  
  ntext <- eventReactive(input$start, {
    selection
    cat(selection)
    cat("\n")
  })
  
  output$nText <- renderText({
    ntext()
  })
  
  
  output$table = renderDataTable({
    data.frame(board)
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  
})


