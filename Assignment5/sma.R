# BDA400 Assignment 5
# Simple Moving Average (SMA)

sma <- function(data, period) {
  
  # Check the data length
  if (length(data) < period) {
    stop("Data length must be at least the period")
  }
  
  # Initialize the SMA vector
  sma_values <- numeric(length(data) - period + 1)
  
  # Calculate the moving average
  for (i in 1:length(sma_values)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }
  
  return(sma_values)
}