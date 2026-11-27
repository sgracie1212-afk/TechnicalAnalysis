# BDA400 Assignment 5
# Stochastic RSI (StochRSI)

stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Calculate RSI
  rsi_values <- rsi(data, period)
  
  # Remove initial missing values
  rsi_values <- rsi_values[!is.na(rsi_values)]
  
  # Check for sufficient data
  if (length(rsi_values) < k_period + d_period - 1) {
    stop("Insufficient data for Stochastic RSI")
  }
  
  # Calculate minimum and maximum RSI
  min_rsi <- min(rsi_values)
  max_rsi <- max(rsi_values)
  
  # Normalize RSI values
  if (max_rsi == min_rsi) {
    k_values <- rep(NA_real_, length(rsi_values))
  } else {
    k_values <- (rsi_values - min_rsi) /
      (max_rsi - min_rsi)
  }
  
  # Calculate the %K line
  k_line <- sma(k_values, k_period)
  
  # Calculate the %D line
  d_line <- sma(k_line, d_period)
  
  # Return results
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}