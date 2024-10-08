# Create the bar plot for win probabilities with reduced bar height (adjusting width)
bar_plot <- ggplot(win_prob_by_genre, aes(x = reorder(genre, avg_win_prob), y = avg_win_prob)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +  # Adjust width to reduce bar height
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Show percentages
  labs(y = "Win Awards Probability", x = NULL) +
  coord_flip() +
  theme_minimal()+
  theme(axis.title.y = element_blank(),  # Remove y-axis label
        axis.text.y = element_text(size = 10))  # Set the size of y-axis text
        #plot.margin = margin(t = 10, r = -10, b = 10, l = 10))  # Adjust the plot margins

# Save the plot
ggsave("Charts/bar_plot.jpg", plot = bar_plot, width = 4, height = 8)

# Create the Ridgeline plot (without the y-axis text)
ridgeline_plot <- ggplot(data, aes(x = imdb_rating, y = reorder(genre, median_rating), fill = median_rating)) +
  geom_density_ridges(scale = 1.5, bandwidth = 0.4) +
  scale_fill_gradient(low = "white", high = "blue", name = "Median IMDb Rating") +
  labs(y = NULL, x = "IMDb Rating") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        #plot.margin = margin(t = 10, r = -10, b = 10, l = 7),  # Adjust the plot margins
        legend.position = "right")

# Save the plot
ggsave("Charts/ridgeline_plot.jpg", plot = ridgeline_plot, width = 8, height = 8)





# Align the plots using cowplot to create dual y-axes effect
combined_plot <- plot_grid(bar_plot, ridgeline_plot, nrow = 1, align = "h", rel_widths = c(0.3, 0.7))

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave("Charts/combined_dual_axis_chart.jpg", plot = combined_plot, width = 14, height = 8)
