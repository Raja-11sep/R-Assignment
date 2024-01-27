# Question 5

# First we define the function f(x, y)
f <- function(x, y) {
  return(2 * x^2 + 2 * y^2)
}

# Create a grid of x and y values
x <- seq(-20, 20, length.out = 50)
y <- seq(-20, 20, length.out = 50)

# Generate a grid of z values using the function f(x, y)
z <- outer(x, y, FUN = function(x, y) f(x, y))

# Create a contour plot
contour(x, y, z, 
        main = "Contour Plot of f(x, y) = 2x^2 + 2y^2",  
        xlab = "x-axis", ylab = "y-axis", 
        col = terrain.colors(20)  
)

# Add a pattern color legend
legend("topright", legend = "f(x, y)", fill = terrain.colors(20), density = 20)
