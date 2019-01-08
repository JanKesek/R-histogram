#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
# Define UI for app that draws a histogram ----
library(shiny)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Histogram of top 50 currencies on Coinmarket"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "inputSlider",
                  label = "Number of bins:",
                  min = 2,
                  max = 50,
                  value = 40),
      selectInput("variable", "Choose Currency to plot:",
                  list("Currency Symbol For Boxplot" = fetchjson$symbol[1:5]),
                  selected = "XRP")
    
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "bPlot"),
      tags$p("Summary of prices in certain frequency (use slider to change)"),
      verbatimTextOutput("sum")
      
    )
  )
)
