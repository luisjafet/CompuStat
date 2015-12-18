library(shiny)

shinyUI(pageWithSidebar(
  headerPanel('Conecta 4'),
  sidebarPanel(
    actionButton("start", "Iniciar"),
    actionButton("button1", "1"),
    actionButton("button2", "2"),
    actionButton("button3", "3"),
    actionButton("button4", "4")
  ),
  mainPanel(
    verbatimTextOutput("nText"),
    tabsetPanel(
      tabPanel('table',
               dataTableOutput("table"))
    )
  )
))
