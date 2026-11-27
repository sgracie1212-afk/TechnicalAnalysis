# BDA400 Assignment 5
# Exponential Moving Average (EMA)

ema <- function(data, period) {
  
  # Calculate the multiplier
  multiplier <- 2 / (period + 1)
  
  # Initialize the EMA vector
  ema_values <- numeric(length(data))
  
  # Calculate EMA values
  for (i in seq_along(data)) {
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      ema_values[i] <- (data[i] - ema_values[i - 1]) *
        multiplier + ema_values[i - 1]
    }
  }
  
  return(ema_values)
}