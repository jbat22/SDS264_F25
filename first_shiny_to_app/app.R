library(tidyverse)

ui <- fluidPage(
  titlePanel("Number of Bins"),
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput("n_breaks", label = "Number of bins:",
                              choices = c(10, 20, 35, 50), selected = 20),
                  
                  sliderInput("bw_adjust", label = "Bandwidth adjustment:",
                              min = 0.2, max = 2, value = 1, step = 0.2),
                  checkboxInput("type_check2","Tidyverse?",value=FALSE)
                ),
                mainPanel(
                  plotOutput(outputId = "histogram")
                )
  )
)

server <- function(input, output) {
  
  output$histogram <- renderPlot({
    if (input$type_check2) 
      hist(faithful$eruptions, probability = TRUE, 
           breaks = as.numeric(input$n_breaks),
           xlab = "Duration (minutes)", 
           main = "Geyser eruption duration")
    
      dens <- density(faithful$eruptions, adjust = input$bw_adjust)
      lines(dens, col = "blue") 
    else if (!input$type_check2)      
      ggplot(faithful, aes(x = eruptions)) +
      geom_histogram(aes(y = ..density..), 
                     bins = as.numeric(input$n_breaks2),
                     fill = "white", colour = "black") +
      geom_density(adjust = input$bw_adjust2, colour = "blue") +
      labs(x = "Duration (minutes)", 
           title = "Geyser eruption duration")
  })
}  


shinyApp(ui = ui, server = server)