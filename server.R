
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  side.a <- reactive({
    validate(
      need(input$side.a, "Side A must be a number")
    )
    
    return(input$side.a)
  })
  
  side.b <- reactive({
    validate(
      need(input$side.b, "Side B must be a number")
    )
    
    return(input$side.b)
  })
  
  side.c <- reactive({
    validate(
      need(input$side.c, "Side C must be a number")
    )
    
    return(input$side.c)
  })
  
  output$triangle.info <- renderText({
    ordered.vector <- sort(c(side.a(),side.b(),side.c()), decreasing = TRUE)
    
    info.text <- if(!valid.triangle(side.a(),side.b(),side.c())) {
      "These side lengths are not a valid triangle. Make the short sides longer or the long side shorter."
    } else if(ordered.vector[2]^2 + ordered.vector[3]^2 == ordered.vector[1]^2) {
      "These side lengths produce a valid right triangle."
    } else if(side.a() == side.b() & side.a() == side.c()) {
      "These side lengths produce an valid equilateral triangle."
    } else if(side.a() == side.b() | side.a() == side.c() | side.c() == side.b()) {
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
      need(valid.triangle(a, b, c), "Cannot plot an invalid triangle.")
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
      geom_segment(aes(x=a, y=0, xend=a-side.c.xend, yend=side.c.yend), color="green", size=2) # side C
  })
})

valid.triangle <- function(a, b, c) {
  side.vector <- c(a, b, c)
  return(sum(side.vector) - max(side.vector) > max(side.vector))
}

law.of.cosines <- function(a, b, c) {
  gamma <- acos((a^2 + b^2 - c^2)/(2*a*b))
  return(gamma)
}

x.pos <- function(gamma, h) {
  x <- h * sin(pi/2 - gamma)
  return(x)
}

y.pos <- function(gamma, h) {
  y <- h * sin(gamma)
  return(y)
}