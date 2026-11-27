# BDA400 Assignment 5
# Relative Strength Index (RSI)

rsi <- function(data, period) {
  
  diff_values <- diff(data)
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)
  
  rsi_values <- rep(NA_real_, length(data))
  
  if (length(data) <= period) {
    return(rsi_values)
  }
  
  # Calculate initial averages
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])
  
  # Calculate RSI using Wilder's smoothing
  for (i in (period + 1):length(data)) {
    
    if (i > period + 1) {
      avg_gain <- (avg_gain * (period - 1) +
                     gains[i - 1]) / period
      
      avg_loss <- (avg_loss * (period - 1) +
                     losses[i - 1]) / period
    }
    
    if (avg_gain == 0 && avg_loss == 0) {
      rsi_values[i] <- 50
    } else if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  
  return(rsi_values)
}