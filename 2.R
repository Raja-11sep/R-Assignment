# Question 2

# First we define the function f(x) = x^3 - x - 1
f <- function(x) {
  return(x^3 - x - 1)
}

# Use Bisection method to find the root with accuracy up to 3 decimal digit.
bisection <- function(f, a, b, accuracy) {
  # Initialize variables
  iter <- 0
  root <- NULL
  
  # Check if the initial interval [a, b] is valid
  if (f(a) * f(b) >= 0) {
    stop("Initial interval [a, b] is not valid. f(a) * f(b) must be negative.")
  }
  
  # Iterate until the desired accuracy is reached
  while ((b - a) >= accuracy) {
    iter <- iter + 1
    # Calculate the midpoint
    x0 <- (a + b) / 2
    
    # Check if x0 is a root (f(x0) is very close to 0)
    if (abs(f(x0)) < accuracy) {
      root <- x0
      break
    }
    
    # Update the interval [a, b] based on the sign of f(x0)
    if (f(a) * f(x0) < 0) {
      b <- x0
    } else {
      a <- x0
    }
  }
  
  if (is.null(root)) {
    # If no exact root is found, return the approximate root
    root <- (a + b) / 2
  }
  
  return(list(root = root, iterations = iter))
}

# Set the initial interval [a, b] and desired accuracy
a <- 1  # Initial lower bound
b <- 2  # Initial upper bound
# Desired accuracy to 3 decimal places
accuracy <- 0.001  

# Find the root using the Bisection method
result <- bisection(f, a, b, accuracy)

# Display the result
cat("Approximate Root of f(x):", result$root, "\n")
cat("Number of Iterations:", result$iterations, "\n")