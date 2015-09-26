# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  titlePanel("Función Inversa"),

  sidebarLayout( sidebarPanel(sliderInput("simulaciones", "Número de simulaciones", min = 500, max = 1000, value = 500),
  numericInput("lambda", "Parámetro lambda", value = .5),
  tableOutput("table")
    ),

    mainPanel(
      plotOutput("distPlot")
    )
  )
))
