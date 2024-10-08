# Load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
library(ggplot2)
library(dplyr)

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

# Create the bar plot for win probabilities with bars starting from the right and black outlines
bar_plot <- ggplot(win_prob_by_genre, aes(y = reorder(genre, avg_win_prob), x = avg_win_prob)) +
  geom_bar(stat = "identity", fill = "white", color = "black", width = 0.5) +  # Add black borders using color
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +  # Show percentages
  scale_x_reverse() +  # Reverse the x-axis to start the bars from the right
  labs(x = "Probability of Winning Awards", y = NULL) +  # Add title for x-axis
  scale_y_discrete(position = "right") +  # Move y-axis labels to the right
  theme_minimal() +
  theme(axis.title.x = element_text(size = 12),  # Adjust x-axis title
        axis.text.y.right = element_text(size = 10),  # Move y-axis text to the right
        axis.ticks.y.right = element_line(),  # Add y-axis ticks on the right
        axis.line.y.right = element_line())  # Add the y-axis line on the right

# Show the plot
show(bar_plot)

# Save the plot
ggsave("Charts/bar_plot.jpg", plot = bar_plot, width = 4, height = 8)



