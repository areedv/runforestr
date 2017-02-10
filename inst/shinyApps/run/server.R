function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$n))
  })
  output$plot <- renderPlot({
    plot(rnorm(input$n))
  })
}
