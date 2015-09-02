
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  titlePanel("Box Muller"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Tamaño",  min = 100, max = 100000, value = 1000)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
