# BDA400 Assignment 5
# Standard Deviation (stdev)

stdev <- function(data) {
  
  # Calculate the mean
  mean_value <- sum(data) / length(data)
  
  # Calculate the differences
  diff_values <- data - mean_value
  
  # Calculate the squared differences
  squared_diff <- diff_values^2
  
  # Calculate the variance
  variance <- sum(squared_diff) / length(data)
  
  # Calculate the standard deviation
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}