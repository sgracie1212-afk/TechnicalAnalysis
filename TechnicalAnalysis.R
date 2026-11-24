# BDA400 Assignment 2 - Technical Analysis
# Technical Analysis using R
# Load required packages
library(quantmod)
library(TTR)

# Read stock symbols from portfolio file
portfolio <- readLines("portfolio.txt")

# Display stock symbols
print(portfolio)
# Function to load stock data
load_stock_data <- function() {
  symbols <- readLines("portfolio.txt")
  stock_data <- list()
  
  for (symbol in symbols) {
    data <- getSymbols(symbol, src = "yahoo",
                       from = "2025-01-01",
                       auto.assign = FALSE)
    stock_data[[symbol]] <- as.data.frame(data)
  }
  
  return(stock_data)
}

# Load all stock data
stocks <- load_stock_data()

# Display loaded stock names
print(names(stocks))
# Preview Apple stock data
print(head(stocks[["AAPL"]]))
# Function to calculate basic statistics
calculate_statistics <- function(data) {
  closing <- na.omit(Cl(data))
  values <- unique(closing)
  
  list(
    Moving_Average = SMA(closing, n = 20),
    Mean = mean(closing),
    Mode = values[which.max(tabulate(match(closing, values)))],
    Median = median(closing),
    Standard_Deviation = sd(closing)
  )
}

# Calculate statistics for all five stocks
statistics <- lapply(stocks, calculate_statistics)
# Display calculated statistics for all stocks
for (symbol in names(statistics)) {
  cat("\nStock:", symbol, "\n")
  cat("Mean:", statistics[[symbol]]$Mean, "\n")
  cat("Mode:", statistics[[symbol]]$Mode, "\n")
  cat("Median:", statistics[[symbol]]$Median, "\n")
  cat("Standard Deviation:",
      statistics[[symbol]]$Standard_Deviation, "\n")
  cat("Latest 20-Day Moving Average:",
      tail(statistics[[symbol]]$Moving_Average, 1), "\n")
}
# Function to display stock data
display_stock_data <- function(stock_data) {
  for (symbol in names(stock_data)) {
    cat("\nStock Data:", symbol, "\n")
    print(head(stock_data[[symbol]]))
  }
}

# Display all five stocks
display_stock_data(stocks)

# Plot Apple stock closing prices
plot(as.Date(rownames(stocks[["AAPL"]])),
     Cl(stocks[["AAPL"]]),
     type = "l",
     col = "blue",
     main = "Apple Stock Closing Prices",
     xlab = "Date",
     ylab = "Closing Price (USD)")
# Create closing price charts for all stocks
for (symbol in names(stocks)) {
  plot(as.Date(rownames(stocks[[symbol]])),
       Cl(stocks[[symbol]]),
       type = "l",
       col = "blue",
       main = paste(symbol, "Closing Prices"),
       xlab = "Date",
       ylab = "Closing Price (USD)")
  # Plot closing prices with moving averages
  for (symbol in names(stocks)) {
    dates <- as.Date(rownames(stocks[[symbol]]))
    closing <- as.numeric(Cl(stocks[[symbol]]))
    moving_average <- SMA(closing, n = 20)
    
    plot(dates, closing,
         type = "l",
         col = "blue",
         main = paste(symbol, "Technical Analysis"),
         xlab = "Date",
         ylab = "Price (USD)")
    
    lines(dates, moving_average, col = "red", lwd = 2)
    
    legend("topleft",
           legend = c("Closing Price", "20-Day Moving Average"),
           col = c("blue", "red"),
           lty = 1)
  }
  
}



