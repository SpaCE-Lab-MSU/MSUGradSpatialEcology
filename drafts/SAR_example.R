#rm(list=ls())

###Load packages
require(shiny)
require(ggplot2)

### Define user interface
ui = fluidPage(
  titlePanel("Species-Area Relationship - Power Model"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("c", "Value of c:", min = 0.1, max = 10, value = 1), #bar to define values for c
      
      sliderInput("z", "Value of z:", min = 0.1, max = 2, value = 0.5),#bar to define values for z
      numericInput("area", "Area:", value = 1000, min = 1),#define range for area
      numericInput("num_species", "Number of Species:", value = 50, min = 1, max = 1000),#define rangefor species richness
      checkboxInput("loglog", "Log-Log Axes"),
      width = 3
    ),
    
    mainPanel(
      plotOutput("species_area_plot")
    )
  )
)

### Define the server
server = function(input, output) {
  output$species_area_plot <- renderPlot({
    
    ### Generate data
    area <- seq(1, input$area, length.out = 1000)
    species <- input$c * area^input$z
    
    ### Create plot
    p = ggplot() +
      geom_line(aes(x = area, y = species), color = "blue") +
      geom_hline(yintercept = input$num_species, linetype = "dashed", color = "red") +
      labs(x = "Area", y = "Species") +
      theme_minimal()
    
    # Log-Log Axes
    if (input$loglog) {
      p <- p + scale_x_log10() + scale_y_log10()
    }
    
    print(p)
  })
}

### Execute app
shinyApp(ui = ui, server = server)