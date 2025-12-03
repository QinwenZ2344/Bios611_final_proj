library(tidyverse)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(ggplot2))

# Define file paths
# WTA
wta_rankings_path <- "data/WTA/wta_rankings_20s.csv"
wta_players_path <- "data/WTA/wta_players.csv"
# ATP
atp_rankings_path <- "data/ATP/atp_rankings_20s.csv"
atp_players_path <- "data/ATP/atp_players.csv"

# Function to read and tag
read_data <- function(rank_path, player_path, sex_label) {
  # Read rankings
  rankings <- read_csv(rank_path, show_col_types = FALSE) %>%
    mutate(sex = sex_label)
    
  # Read players
  players <- read_csv(player_path, show_col_types = FALSE)
  
  # Join
  # Note: ATP/WTA player files might have slightly different columns or formats, 
  # but standard ones usually share player_id, name_first, name_last.
  # We should check if player_id column name matches in rankings.
  
  # Usually rankings have 'player' column for ID.
  
  # Get most recent ranking date for this dataset
  latest_date <- max(rankings$ranking_date)
  
  # Filter for latest top 15 (to get top 30 combined approx)
  top_rankings <- rankings %>%
    filter(ranking_date == latest_date) %>%
    arrange(rank) %>%
    head(15)
  
  # Join with player info
  # Ensure player_id is correct type for join
  top_players <- top_rankings %>%
    mutate(player = as.character(player)) %>%
    left_join(players %>% mutate(player_id = as.character(player_id)), 
              by = c("player" = "player_id")) %>%
    mutate(full_name = paste(name_first, name_last))
    
  return(list(data = top_players, date = latest_date))
}

# Process both
wta_res <- read_data(wta_rankings_path, wta_players_path, "WTA")
atp_res <- read_data(atp_rankings_path, atp_players_path, "ATP")

combined_top <- bind_rows(wta_res$data, atp_res$data) %>%
  arrange(desc(points))

# Create the bar plot
# We'll color by Sex and facet
p <- ggplot(combined_top, aes(x = reorder(full_name, -points), y = points, fill = sex)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("ATP" = "blue", "WTA" = "deeppink")) +
  facet_wrap(~ sex, scales = "free_x") +
  labs(
    x = "Player Name",
    y = "Points"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" # Hide legend as panels are labeled
  )

# Display the plot
print(p)

# Save the plot
if (!dir.exists("plots")) {
  dir.create("plots")
}
plot_output_file <- "plots/combined_top_players_rank.png"
ggsave(plot_output_file, plot = p, width = 14, height = 8)
cat(paste("Plot saved to", plot_output_file, "\n"))

# Save Text Summary to documents/
if (!dir.exists("documents")) {
  dir.create("documents")
}
txt_output_file <- "documents/player_rank_summary.txt"
sink(txt_output_file)

cat("Top Players Summary (Ranked by Points)\n")
cat("======================================\n\n")
cat(paste("Date of Rankings (ATP):", atp_res$date, "\n"))
cat(paste("Date of Rankings (WTA):", wta_res$date, "\n\n"))

cat("Top Players List:\n")
print(combined_top %>% select(sex, rank, full_name, points))

sink()
cat(paste("Summary saved to", txt_output_file, "\n"))
