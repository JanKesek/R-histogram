library(shiny)
library(jsonlite)
library(curl)
fetchjson <- fromJSON("https://api.coinmarketcap.com/v1/ticker/?limit=50")

#df <- newdata$ticker$markets
#volume <- as.double(vectorofCallsNamed[[as.character("BTC")]]$markets$volume)
#price <- as.double(vectorofCallsNamed[[as.character("BTC")]]$markets$price)
#curr <- "BTC"
vectorofCallsNamedPrice2 <- list()
vectorofCallsNamedVolume2 <- list()


BTC<- fromJSON("https://api.cryptonator.com/api/full/BTC-usd")
ETH<- fromJSON("https://api.cryptonator.com/api/full/ETH-usd")
XRP <- fromJSON("https://api.cryptonator.com/api/full/XRP-usd")
BCH <- fromJSON("https://api.cryptonator.com/api/full/BCH-usd")
XLM <- fromJSON("https://api.cryptonator.com/api/full/XLM-usd")

vectorofCallsNamedPrice2[["BTC"]] <- BTC$ticker$markets$price
vectorofCallsNamedPrice2[["ETH"]] <- ETH$ticker$markets$price
vectorofCallsNamedPrice2[["XRP"]] <- XRP$ticker$markets$price
vectorofCallsNamedPrice2[["BCH"]] <- BCH$ticker$markets$price
vectorofCallsNamedPrice2[["XLM"]] <- XLM$ticker$markets$price

vectorofCallsNamedVolume2[["BTC"]] <- BTC$ticker$markets$volume
vectorofCallsNamedVolume2[["ETH"]] <- ETH$ticker$markets$volume
vectorofCallsNamedVolume2[["XRP"]] <- XRP$ticker$markets$volume
vectorofCallsNamedVolume2[["BCH"]] <- BCH$ticker$markets$volume
vectorofCallsNamedVolume2[["XLM"]] <- XLM$ticker$markets$volume

price <-vectorofCallsNamedPrice2[["BTC"]]

volume <- vectorofCallsNamedVolume2[["BTC"]]

# Define server logic required to draw a histogram

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$distPlot <- renderPlot({
    
    x    <- as.double( fetchjson$`24h_volume_usd`)
    #Sturges formula for calculating  bins length
    binslength <- ceiling(1+log2(length(x)))
    bins <- seq(floor(min(x)), ceiling(max(x)), length.out = binslength)
    
    freq = input$inputSlider
      hist(as.double(fetchjson$`24h_volume_usd`[1:freq]),
           main = "Number of currencies in certain price (use sliderInput to set frequency)",
         xlab = "Closing price of currency" ,
         breaks = bins, axes = FALSE)
    axis(side =1, at = bins, labels = TRUE, cex.axis=1)
    axis(side =2, at = 1:freq, labels = TRUE, cex.axis=1)
}) 

  
  output$bPlot <- renderPlot({

    
    curr <- input$variable
    a<- vectorofCallsNamedPrice2[[curr]]
    b<- vectorofCallsNamedVolume2[[curr]]
    plot(a, b, main = "Price to value, use selectInput to choose currency",
        xlab = "Price", ylab = "Volume", pch = 16, cex = .9, col = "red")
   # plot(price, volume, main = "Price to value, use selectInput to choose currency",
    #        xlab = "Price", ylab = "Volume", pch = 16, cex = .9, col = "red")
    })
  output$sum <- renderPrint({
    freq = input$inputSlider
    
    summary(as.numeric(fetchjson$`24h_volume_usd`[1:freq]))
  })
  
}

