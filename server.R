
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
source('functions/triangle.geometry.R')

shinyServer(function(input, output) {
  
  side.a <- reactive({
    # cache side a from the ui
    validate(
      need(input$side.a, "Side A must be a number")
    )
    
    return(input$side.a)
  })
  
  side.b <- reactive({
    # cache side b from the ui
    validate(
      need(input$side.b, "Side B must be a number")
    )
    
    return(input$side.b)
  })
  
  side.c <- reactive({
    # cache side c from the ui
    validate(
      need(input$side.c, "Side C must be a number")
    )
    
    return(input$side.c)
  })
  
  output$triangle.info <- renderText({
    a <- side.a()
    b <- side.b()
    c <- side.c()
    
    info.text <- if(!is.valid.triangle(a, b, c)) {
      "These side lengths are not a valid triangle. Make the short sides longer or the long side shorter."
    } else if(is.right.triangle(a, b, c)) {
      "These side lengths produce a valid right triangle."
    } else if(a == b & a == c) {
      "These side lengths produce an valid equilateral triangle."
    } else if(a == b | a == c | c == b) {
      "These side lengths produce an valid isosceles triangle."
    } else {
      "These side lengths produce a valid triangle."
    }
    
    return(info.text)
  })
  
  output$triangle.plot <- renderPlot({
    a <- side.a()
    b <- side.b()
    c <- side.c()
    
    validate(
      need(is.valid.triangle(a, b, c), "Cannot plot an invalid triangle.")
    )
    
    gamma <- law.of.cosines(a, b, c)
    
    side.b.xend <- x.pos(gamma, b)
    side.b.yend <- y.pos(gamma, b)
    
    theta <- law.of.cosines(a, c, b)
    side.c.xend <- x.pos(theta, c)
    side.c.yend <- y.pos(theta, c)
      
    ggplot() +
      geom_segment(aes(x=0, y=0, xend=a, yend=0), size=2) + # side A
      geom_segment(aes(x=0, y=0, xend=side.b.xend, yend=side.b.yend), color="blue", size=2) + # side B
      geom_segment(aes(x=a, y=0, xend=a-side.c.xend, yend=side.c.yend), color="green", size=2) + # side C
      theme_minimal() +
      xlab("") +
      ylab("")
  })
})