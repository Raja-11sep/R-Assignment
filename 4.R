# Question 4

# Load the required library which we use
library(plotly)

# Now create a data frame with state names and AQI values
data <- data.frame(
  State = c("Andhra", "Arunachal", "Bihar", "Chandigarh", "Delhi", "Kerala", "Punjab", "UP"),
  AQI = c(59, 79, 110, 168, 92, 85, 113, 102)
)
print(data)

# Now create a 3D pie chart for given AQI data
pie_chart <- plot_ly(
  data,
  labels = ~State,
  values = ~AQI,
  type = "pie",
  # Show percentage and label on slices
  textinfo = "percent+label",
  # Position the text inside the slices
  textposition = "inside",
  # Adjust the hole size for a 3D effect
  hole = 0.4, 
  # Add a border around slices
  marker = list(line = list(width = 2))  
)
# print pie chart without layout and title
print(pie_chart) 

# Customize the layout and title
pie_chart <- pie_chart %>% layout(
  title = "AQI of Indian States on Sep 13, 2023",
  showlegend = TRUE,          
  legend = list(x = 0.9, y = 0.5)  
)

# Display the pie chart with layout and title
print(pie_chart)