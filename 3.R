

heatmap_plot <- ggplot(data = bestseller, aes(x = Genre, y = Year, fill = User_Rating)) + 
  geom_tile() + 
  scale_fill_gradient(low = "green", high = "red") + 
  labs(
    title = "Bestseller Heatmap", 
    x = "Genre", 
    y = "Year", 
    fill = "User Rating"
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Display the heatmap plot
print(heatmap_plot)