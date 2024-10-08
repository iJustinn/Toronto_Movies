# Load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the dataset
data <- read.csv("data/raw_data.csv", stringsAsFactors = FALSE)

# Extract wins and nominations from the awards column
extract_awards <- function(awards_text) {
  wins <- ifelse(grepl("win", awards_text), as.numeric(sub(" .*", "", awards_text)), 0)
  noms <- ifelse(grepl("nomination", awards_text), 
                 as.numeric(sub(".*& ([0-9]+) nomination.*", "\\1", awards_text)), 0)
  return(data.frame(wins = wins, noms = noms))
}

# Apply the function to extract wins and nominations
award_data <- do.call(rbind, lapply(data$awards, extract_awards))

# Merge the wins and nominations back into the dataset
data <- cbind(data, award_data)

# Calculate win probability
data$win_prob <- ifelse(data$wins + data$noms > 0, data$wins / (data$wins + data$noms), NA)

# Remove rows with NA win probabilities
data <- data %>% filter(!is.na(win_prob))

# Split the genre column (assuming it's comma-separated) and unnest it
data$genre <- strsplit(data$genre, ", ")
data <- tidyr::unnest(data, genre)

# Calculate the average win probability by genre
win_prob_by_genre <- data %>%
  group_by(genre) %>%
  summarize(avg_win_prob = mean(win_prob, na.rm = TRUE))

# Ensure all genres from the ordered list are included, even if there's no data for them
ordered_genres <- c(
  "Animation", "Music", "Family", "Sport", "Fantasy", "Horror", 
  "Comedy", "Romance", "Action", "Thriller", "Adventure", "Sci-Fi", 
  "Mystery", "Musical", "Crime", "Drama", "War", "History", 
  "Documentary", "Biography"
)

# Create a complete dataset to include missing genres with NA values
win_prob_by_genre <- win_prob_by_genre %>%
  complete(genre = ordered_genres, fill = list(avg_win_prob = 0))  # Fill missing values with 0

# Convert the genre column to a factor and set the levels based on ordered_genres
win_prob_by_genre$genre <- factor(win_prob_by_genre$genre, levels = ordered_genres)

# Create the bar plot for win probabilities with a gradient color scale
bar_plot <- ggplot(win_prob_by_genre, aes(y = genre, x = avg_win_prob, fill = avg_win_prob)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +  # Add black borders and color fill
  scale_fill_gradient(low = "white", high = "red", name = "Probability of Winning Awards") +  # Gradient fill for win probability
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +  # Show percentages
  scale_x_reverse() +  # Reverse the x-axis to start the bars from the right
  labs(x = NULL, y = NULL) +  # Remove titles for x and y axes
  scale_y_discrete(position = "right") +  # Move y-axis labels to the right
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12),  # Adjust x-axis title
        axis.text.y.right = element_text(size = 10),  # Move y-axis text to the right
        axis.ticks.y.right = element_line(),  # Add y-axis ticks on the right
        axis.line.y.right = element_line(),  # Add the y-axis line on the right
        legend.position = "left")  # Move the legend to the left

# Show the plot
print(bar_plot)

# Save the plot
ggsave("Charts/bar_plot.jpg", plot = bar_plot, width = 8, height = 10)


