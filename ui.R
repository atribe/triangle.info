
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Triangle Information"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "side.a",
                          label = "Side A Length (Black):",
                          value = 3,
                          min = 1,
                          step = 1),
      numericInput(inputId = "side.b",
                          label = "Side B Length (Blue):",
                          value = 4,
                          min = 1,
                          step = 1),
      numericInput(inputId = "side.c",
                          label = "Side C Length (Green):",
                          value = 5,
                          min = 1,
                          step = 1)
      ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("triangle.info"),
      plotOutput("triangle.plot")
    )
  )
))
