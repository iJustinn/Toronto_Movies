# Install necessary libraries if not installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")

# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)

# Load the dataset (assuming the file is in CSV format)
data <- read.csv("data/raw_data.csv", stringsAsFactors = FALSE)

# Convert 'imdb_rating' to numeric and 'genre' to a list-like structure
data$imdb_rating <- as.numeric(data$imdb_rating)

# Split genres where multiple genres are listed in one entry
data$genre <- strsplit(data$genre, ", ")

# Unnest the data (equivalent to explode in Python)
data <- tidyr::unnest(data, genre)

# Group by genre and calculate median IMDb rating
genre_median_ratings <- data %>%
  group_by(genre) %>%
  summarize(median_rating = median(imdb_rating, na.rm = TRUE)) %>%
  arrange(median_rating)

# Normalize the median ratings to a 0-1 scale for color mapping
genre_median_ratings$normalized_rating <- rescale(genre_median_ratings$median_rating)

# Merge median ratings back into the original dataset
data <- data %>%
  left_join(genre_median_ratings, by = "genre")

# Generate the plot
draft_plot <- ggplot(data, aes(x = imdb_rating, y = reorder(genre, median_rating), fill = median_rating)) +
  geom_boxplot(outlier.shape = NA) + # Remove outliers from the plot
  scale_fill_gradient(low = "white", high = "red", name = "Median IMDb Rating") +
  labs(title = "IMDb Ratings Distribution by Genre",
       x = "IMDb Rating", y = "Genre") +
  theme_minimal() +
  theme(legend.position = "right")

# Show the plot
show(draft_plot)

# Saving the plot
ggsave("Charts/draft.jpg", plot = draft_plot)
