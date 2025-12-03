# Load necessary libraries
if (!require("tidyverse")) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)

# Define file paths
data_dir <- "data"
processed_file <- file.path(data_dir, "processed_data", "player_pca_clusters.csv")

# Create documents directory if it doesn't exist
if (!dir.exists("documents")) {
  dir.create("documents")
}
output_file <- "documents/miscluster_analysis_findings.txt"

# 1. Load Data
if (!file.exists(processed_file)) {
  stop("Processed data file not found. Please run scripts/player_cluster.r first.")
}

player_data <- read_csv(processed_file) %>%
  mutate(player_id = as.character(player_id))
message("Loaded player data with ", nrow(player_data), " players.")

# Ensure cluster is a factor if it isn't
player_data$cluster <- as.factor(player_data$cluster)

# Remove existing classification columns if they exist to avoid duplication
player_data <- player_data %>%
  select(-any_of(c("dominant_sex", "mis_clustered_type", "classification", "group_label")))

# 2. Identify Mis-clustered Players
# Re-confirm dominant sex per cluster (in case logic differs or to be safe)
cluster_dominance <- player_data %>%
  count(cluster, sex) %>%
  group_by(cluster) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(cluster, dominant_sex = sex)

player_data <- player_data %>%
  left_join(cluster_dominance, by = "cluster")

# Debugging: Check columns
message("Columns after join: ", paste(colnames(player_data), collapse = ", "))

player_data <- player_data %>%
  mutate(
    classification = case_when(
      sex == dominant_sex ~ "Correct",
      TRUE ~ "Mis-clustered"
    ),
    group_label = case_when(
      classification == "Correct" ~ paste(sex, "(Correct)"),
      sex == "WTA" & classification == "Mis-clustered" ~ "WTA in ATP Cluster",
      sex == "ATP" & classification == "Mis-clustered" ~ "ATP in WTA Cluster"
    )
  )

# --- NEW: Load Rankings to get Points ---
message("Loading Ranking Data...")
# Helper to get average points
get_avg_points <- function(path, sex_label) {
  if(!file.exists(path)) return(NULL)
  df <- read_csv(path)
  
  # Calculate average points per player
  df_summ <- df %>%
    group_by(player) %>%
    summarise(avg_points = mean(points, na.rm = TRUE), .groups = "drop") %>%
    mutate(player_id = as.character(player), sex = sex_label) %>%
    select(player_id, sex, avg_points)
  
  return(df_summ)
}

wta_points <- get_avg_points(file.path(data_dir, "WTA", "wta_rankings_20s.csv"), "WTA")
atp_points <- get_avg_points(file.path(data_dir, "ATP", "atp_rankings_20s.csv"), "ATP")
all_points <- bind_rows(wta_points, atp_points)

# Join points to player_data
player_data <- player_data %>%
  left_join(all_points, by = c("player_id", "sex"))

# Start Analysis Output
sink(output_file)
cat("Mis-clustered Player Analysis\n")
cat("===========================\n\n")

cat("1. Classification Summary:\n")
print(table(player_data$sex, player_data$classification))
cat("\n")
print(table(player_data$group_label))
cat("\n")

# 3. Analyze Service & Power Stats
# Comparing mis-clustered groups to the cluster they ended up in vs their own gender
# Key stats: avg_ace, avg_svpt (volume), avg_first_won, avg_df

stats_of_interest <- c("avg_ace", "avg_df", "avg_svpt", "avg_first_won", "avg_bp_saved", "avg_bp_faced")

cat("2. Service and Power Metrics Comparison\n")
cat("---------------------------------------\n")
cat("Question: Do WTA in the 'ATP cluster' look like ATP players in terms of serve volume/aces?\n")
cat("Question: Do ATP in the 'WTA cluster' have more 'WTA-like' patterns?\n\n")

summary_stats <- player_data %>%
  group_by(group_label) %>%
  summarise(
    count = n(),
    mean_ace = mean(avg_ace, na.rm = TRUE),
    mean_df = mean(avg_df, na.rm = TRUE),
    mean_svpt = mean(avg_svpt, na.rm = TRUE),
    mean_1st_won = mean(avg_first_won, na.rm = TRUE),
    mean_bp_faced = mean(avg_bp_faced, na.rm = TRUE),
    mean_points = mean(avg_points, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)
cat("\nInterpretation:\n")
cat("- Compare 'WTA in ATP Cluster' values to 'ATP (Correct)'.\n")
cat("- Compare 'ATP in WTA Cluster' values to 'WTA (Correct)'.\n\n")

# T-tests or significance checks (optional depth)
# Test if WTA in ATP cluster have significantly higher aces than Correct WTA
wta_mis <- player_data %>% filter(group_label == "WTA in ATP Cluster")
wta_corr <- player_data %>% filter(group_label == "WTA (Correct)")

if (nrow(wta_mis) > 2 && nrow(wta_corr) > 2) {
  t_ace <- t.test(wta_mis$avg_ace, wta_corr$avg_ace)
  cat("T-test: Aces - WTA (Mis-clustered) vs WTA (Correct)\n")
  cat("p-value: ", t_ace$p.value, "\n")
  cat("Mean Mis: ", t_ace$estimate[1], " Mean Correct: ", t_ace$estimate[2], "\n\n")
}

# 4. Win Rate Analysis
cat("3. Win Rate Analysis\n")
cat("--------------------\n")
cat("Question: Do mis-clustered players have higher or lower win_rate than correctly clustered ones?\n\n")

win_rate_stats <- player_data %>%
  group_by(sex, classification) %>%
  summarise(
    avg_win_rate = mean(win_rate, na.rm = TRUE),
    median_win_rate = median(win_rate, na.rm = TRUE),
    sd_win_rate = sd(win_rate, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

print(win_rate_stats)
cat("\n")

# Check statistical difference in win rate
cat("Statistical comparison of Win Rates (Correct vs Mis-clustered by Sex):\n")
for (s in c("ATP", "WTA")) {
  sub <- player_data %>% filter(sex == s)
  correct <- sub %>% filter(classification == "Correct") %>% pull(win_rate)
  mis <- sub %>% filter(classification == "Mis-clustered") %>% pull(win_rate)
  
  if (length(mis) > 2) {
    res <- t.test(correct, mis)
    cat(paste0(s, ": p-value = ", format(res$p.value, digits=4), "\n"))
    cat(paste0("   Mean Correct: ", round(res$estimate[1], 3), "\n"))
    cat(paste0("   Mean Mis-clustered: ", round(res$estimate[2], 3), "\n"))
  } else {
    cat(paste0(s, ": Not enough mis-clustered players for t-test.\n"))
  }
}
cat("\n")

# 5. Rank/Points Analysis
cat("4. Rank and Points Analysis\n")
cat("---------------------------\n")
cat("Are mis-clustered players generally higher or lower ranked (based on points)?\n\n")

# Points stats
points_stats <- player_data %>%
  group_by(sex, classification) %>%
  summarise(
    avg_points = mean(avg_points, na.rm = TRUE),
    median_points = median(avg_points, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )
print(points_stats)

cat("\nStatistical comparison of Average Points:\n")
for (s in c("ATP", "WTA")) {
  sub <- player_data %>% filter(sex == s)
  # Ensure no NAs
  sub <- sub %>% filter(!is.na(avg_points))
  
  correct <- sub %>% filter(classification == "Correct") %>% pull(avg_points)
  mis <- sub %>% filter(classification == "Mis-clustered") %>% pull(avg_points)
  
  if (length(mis) > 2 && length(correct) > 2) {
    res <- t.test(correct, mis)
    cat(paste0(s, ": p-value = ", format(res$p.value, digits=4), "\n"))
    cat(paste0("   Mean Correct: ", round(res$estimate[1], 0), "\n"))
    cat(paste0("   Mean Mis-clustered: ", round(res$estimate[2], 0), "\n"))
  } else {
    cat(paste0(s, ": Not enough data/players for t-test.\n"))
  }
}

# 6. PCA Space Distance
cat("\n5. PCA Space Location\n")
cat("---------------------\n")
cat("Distance from cluster centers.\n")

# Re-calculate centers (approximate based on current data)
centers <- player_data %>%
  group_by(cluster) %>%
  summarise(
    cen_PC1 = mean(PC1),
    cen_PC2 = mean(PC2),
    .groups = "drop"
  )

player_data_dist <- player_data %>%
  left_join(centers, by = "cluster") %>%
  mutate(
    dist_to_center = sqrt((PC1 - cen_PC1)^2 + (PC2 - cen_PC2)^2)
  )

dist_stats <- player_data_dist %>%
  group_by(group_label) %>%
  summarise(
    avg_dist_to_own_cluster_center = mean(dist_to_center),
    .groups = "drop"
  )

print(dist_stats)
cat("\nInterpretation: Higher distance suggests players are on the periphery of the cluster.\n")

sink()
message("Analysis complete. Findings saved to ", output_file)

# ---------------- PLOTTING ----------------
# Generate plots to compare avg points (Aces and First Serve Won)

# Define theme to match report style
common_theme <- theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none", # Hide legend as labels are descriptive enough
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )

# 1. Aces Boxplot
p_box <- ggplot(player_data, aes(x = group_label, y = avg_ace, fill = sex)) +
  geom_boxplot() +
  common_theme +
  labs(
    title = "A. Average Aces per Match",
    x = "Group",
    y = "Aces"
  )

# 2. First Serve Won Boxplot
p_sv_won <- ggplot(player_data, aes(x = group_label, y = avg_first_won, fill = sex)) +
  geom_boxplot() +
  common_theme +
  labs(
    title = "B. First Serve Points Won",
    x = "Group",
    y = "Avg Points Won"
  )

# 3. NEW: Ranking Points Boxplot
# Log scale might be better for points as they are highly skewed
p_points <- ggplot(player_data, aes(x = group_label, y = avg_points, fill = sex)) +
  geom_boxplot() +
  scale_y_log10() + # Log scale for points to handle large range
  common_theme +
  labs(
    title = "Average Ranking Points",
    x = "Group",
    y = "Avg Ranking Points"
  )

# Combine plots into one panel (A and B only) using grid
output_plot_file <- "plots/miscluster_combined_panel.png"

# Use png device directly to combine plots with grid
png(output_plot_file, width = 12, height = 6, units = "in", res = 300)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
print(p_box, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_sv_won, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
invisible(dev.off())

message("Saved combined panel plot to ", output_plot_file)

# Save Points plot (C) separately
points_plot_file <- "plots/miscluster_points_boxplot.png"
ggsave(points_plot_file, p_points, width = 8, height = 6)
message("Saved points plot to ", points_plot_file)
