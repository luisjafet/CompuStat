# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  headerPanel("Integrales"),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3")
    
  )
  
))