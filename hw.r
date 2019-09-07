library(shiny)
library(ggplot2)
library(DT)
#install.packages("ggalt")
#library(ggalt)
getwd()

#Set working directory 
#setwd("C:/Users/shrey/Desktop/CMU/Sem3/rShiny/hw1-shreyapr")

#Load Data into R
crime <- read.csv(file = 'data/data.csv')
#head(comp)
#View(comp)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("State", "homicideRate2017", "firearmDeathRate", "firearmDeaths", "Pop"),
                  selected = "Pop"),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                   label = "X-axis",
                   choices = c("State", "homicideRate2017", "firearmDeathRate", "firearmDeaths", "Pop"),
                   selected = "State"),
      
      #Select alpha level for plots
      sliderInput(inputId= "alpha", 
                  label = "Select alpha level:", 
                  min = 0, 
                  max = 1, 
                  value = 0.4), 
      
      # Show data table 
      checkboxInput(inputId = "show_data",
                    label = "Show data table :",
                    value = TRUE), 
      
      
      # Button
      downloadButton("downloadData", "Download")
    ),
    
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      
      plotOutput(outputId = "bar"),
      
      plotOutput("dot"),
      
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "crime")
      
    )
  )
)


# Define server function required to create the scatterplot
server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
    ggplot(data = crime, aes_string(x = input$x, y = input$y, col = input$x)) +
      geom_point(alpha = input$alpha) + coord_flip()
    
   
  })
  
  
  output$bar <- renderPlot({
    
    
   ggplot(data = crime, horiz = True, aes_string(x =input$x, y =input$y)) + geom_bar(stat = "identity", fill = "#FF6666")+coord_flip()
   
    
  })
  
  
  
  output$dot <- renderPlot({
    
    # Simple Dotplot
    
    ggplot(crime) +
      geom_point(aes_string(x = input$y, y = input$x), alpha = input$alpha, color='darkred')
    
  })

  
  
  # Print data table if checked -------------------------------------
  output$crime <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = crime[, 1:5], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
  
    }
)
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(crime, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(crime, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
