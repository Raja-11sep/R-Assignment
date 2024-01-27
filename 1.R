# Question 1

# Install some useful library.

install.packages("tidyverse")
library(tidyverse)

# First define the function f(x)
f <- function(x) x / (1 + x)

# Calculate the Simpson's 1/3 rule result
simpson13 <- function(a, b, n) {
  h <- (b - a) / n
  x <- seq(a, b, by = h)
  y <- f(x)
  s <- y[1] + y[length(y)]
  for (i in 2:(n - 1)) {
    if (i %% 2 == 0) {
      s <- s + 2 * y[i]
    } else {
      s <- s + 4 * y[i]
    }
  }
  s <- s * h / 3
  return(s)
}

# Calculate the Trapezoidal rule result
trapezoidal <- function(a, b, n) {
  h <- (b - a) / n
  x <- seq(a, b, by = h)
  y <- f(x)
  s <- y[1] + y[length(y)]
  for (i in 2:(n - 1)) {
    s <- s + 2 * y[i]
  }
  s <- s * h / 2
  return(s)
}

# Calculate the true value of the integral with limit 0 to 5
true_value <- 5-log(6)

# Calculate the true error and absolute relative true error with the help of true value
true_error <- simpson13(0, 5, 10) - true_value
absolute_relative_true_error <- abs(true_error / true_value)

# Print the results
print(paste0("integration of f(x) by using Simpson's 1/3 rule:", simpson13(0, 5, 10)))
print(paste0("integration of f(x) by using Trapezoidal rule:", trapezoidal(0, 5, 10)))
print(paste0("True value of the integral:", true_value))
print(paste0("True error of the integral:", true_error))
print(paste0("Absolute relative true error of the integral:", absolute_relative_true_error))