# BDA400 Assignment 5
# Linear Regression (linreg)

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  
  n <- length(regressionSource)
  
  # Check the regression length
  if (regressionLength > n) {
    stop("Regression length cannot exceed data length")
  }
  
  # Check the regression offset
  if (regressionOffset >= regressionLength) {
    stop("Offset must be less than regression length")
  }
  
  # Calculate starting and ending positions
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  # Extract the relevant data
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)
  
  # Calculate the means
  mean_index <- mean(index_values)
  mean_source <- mean(source_subset)
  
  # Calculate numerator and denominator
  numerator <- sum((index_values - mean_index) *
                     (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  
  # Calculate slope and intercept
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  
  # Calculate predicted values
  predicted_values <- slope * index_values + intercept
  
  # Return the results
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}