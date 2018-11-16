library(shiny)
library(jsonlite)
fetchjson <- fromJSON("https://api.coinmarketcap.com/v1/ticker/?limit=50")



# Define server logic required to draw a histogram

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- as.double( fetchjson$`24h_volume_usd`)
    
    #bins <- seq(1000000, max(x) + 5 , by = 1000000      )
    #bins <- as.integer( seq(1000000, 1000000000 , by = 53000000      ))
    #bins <- as.double( seq(1000000, 1000000000 , by = 1000      ))
    #bins <- seq(1000000, maxim , by = 5605103877      )
    #bins <- seq(1000000, 290000000000 , by = 290000      )
    
    #binslength <- (max(x)-min(x)) / (1 + (log10(length(x)) / log10(2) ))
    binslength <- ceiling(1+log2(length(x)))
    bins <- seq(floor(min(x)), ceiling(max(x)), length.out = binslength)
    
    #barplot(table(fetchjson$total_supply)) 
    freq = input$inputSlider
    x2 <- fetchjson$`24h_volume_usd`[1:freq]
    options(scipen=5)
    hist(as.double(fetchjson$`24h_volume_usd`[1:freq]), 
         xlab = "Closing price of currency" ,
         breaks = bins, axes = FALSE) 
    axis(side =1, at = bins, labels = TRUE, cex.axis=1)
    axis(side =2, at = 1:freq, labels = TRUE, cex.axis=1)
    #filtered <-
   #  filter(fetchjson$id == input$inputSlider)
    #ggplot(filtered, aes(Alcohol_Content)) +
    #  geom_histogram()
  #})
    #col = "#75AADB", border = "white",
    #xlab = "Waiting time to next eruption (in mins)",
    #main = "Histogram of waiting times"
  
  }) 
}

