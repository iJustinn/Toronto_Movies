# Load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggridges")) install.packages("ggridges")
if (!require("dplyr")) install.packages("dplyr")
library(ggplot2)
library(ggridges)
library(dplyr)

# Load the dataset
data <- read.csv("data/raw_data.csv", stringsAsFactors = FALSE)

# Convert IMDb rating to numeric and filter non-finite values
data$imdb_rating <- as.numeric(data$imdb_rating)
data <- data %>% filter(!is.na(imdb_rating) & is.finite(imdb_rating))

# Split the genre column (assuming it's comma-separated) and unnest it
data$genre <- strsplit(data$genre, ", ")
data <- tidyr::unnest(data, genre)

# Calculate the median IMDb rating by genre
genre_median_ratings <- data %>%
  group_by(genre) %>%
  summarize(median_rating = median(imdb_rating, na.rm = TRUE))  # Calculate the median rating

# Merge the median ratings back into the dataset
data <- data %>%
  left_join(genre_median_ratings, by = "genre")  # Add the median_rating column to the data

# Create the Ridgeline plot (without the y-axis text)
ridgeline_plot <- ggplot(data, aes(x = imdb_rating, y = reorder(genre, median_rating), fill = median_rating)) +
  geom_density_ridges(scale = 1.5, bandwidth = 0.4) +
  scale_fill_gradient(low = "white", high = "blue", name = "Median IMDb Rating") +
  labs(y = NULL, x = "IMDb Rating") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right")

# Show the plot
show(ridgeline_plot)

# Save the plot
ggsave("Charts/ridgeline_plot.jpg", plot = ridgeline_plot, width = 8, height = 10)


