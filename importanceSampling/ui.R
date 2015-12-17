library(shiny)

shinyUI(fluidPage(
  
  titlePanel("ImportanceSampling"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput('N', 'Simulaciones',
                   value = 500),
      sliderInput('m', 'm',
                  value = 2.5, min = 0.1, max = 5, step = .1),
      sliderInput('l', 'lambda',
                  value = 3, min = 0.1, max = 15, step = .1)
    ),
    
    mainPanel(
      plotOutput('crudo'),
      plotOutput('exponencial'),
      plotOutput('beta'),
      plotOutput('lam')
    )
    
  )
)
)