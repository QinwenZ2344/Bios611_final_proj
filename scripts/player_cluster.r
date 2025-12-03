# Load necessary libraries
# We will use tidyverse for data manipulation and visualization if available.
# If you run this and get errors about missing packages, please install them via install.packages("tidyverse")
if (!require("tidyverse")) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(cluster)

# Define file paths
data_dir <- "/Users/homelab/Desktop/BIOS 611/data"

# WTA Files
wta_files <- c(
  file.path(data_dir, "WTA", "wta_matches_2020.csv"),
  file.path(data_dir, "WTA", "wta_matches_2021.csv"),
  file.path(data_dir, "WTA", "wta_matches_2022.csv"),
  file.path(data_dir, "WTA", "wta_matches_2023.csv"),
  file.path(data_dir, "WTA", "wta_matches_2024.csv")
)

# ATP Files
atp_files <- c(
  file.path(data_dir, "ATP", "atp_matches_2020.csv"),
  file.path(data_dir, "ATP", "atp_matches_2021.csv"),
  file.path(data_dir, "ATP", "atp_matches_2022.csv"),
  file.path(data_dir, "ATP", "atp_matches_2023.csv"),
  file.path(data_dir, "ATP", "atp_matches_2024.csv")
)

# 1. Read and Combine Data
message("Reading data files...")

read_files_with_sex <- function(files, sex) {
  lapply(files, function(f) {
    if(file.exists(f)) {
        # Read all columns as character to avoid type mismatches
        df <- read_csv(f, col_types = cols(.default = "c"), show_col_types = FALSE)
        df$sex <- sex
        return(df)
    } else {
        warning(paste("File not found:", f))
        return(NULL)
    }
  })
}

wta_list <- read_files_with_sex(wta_files, "WTA")
atp_list <- read_files_with_sex(atp_files, "ATP")

# Remove NULLs if any files were missing
wta_list <- wta_list[!sapply(wta_list, is.null)]
atp_list <- atp_list[!sapply(atp_list, is.null)]

all_matches <- bind_rows(c(wta_list, atp_list))

message("Total matches loaded: ", nrow(all_matches))

# Convert relevant stats columns to numeric
# We need to extract stats for both winners and losers and treat them as player performances
# Relevant columns based on inspection and data dictionary:
# ace, df (double faults), svpt (serve points), 1stIn, 1stWon, 2ndWon, SvGms, bpSaved, bpFaced

# List of columns to convert to numeric
num_cols <- c("w_ace", "w_df", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon", "w_SvGms", "w_bpSaved", "w_bpFaced",
              "l_ace", "l_df", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_SvGms", "l_bpSaved", "l_bpFaced")

all_matches <- all_matches %>%
  mutate(across(all_of(num_cols), as.numeric))

# Prepare Winner Data
winners <- all_matches %>%
  select(
    player_id = winner_id,
    player_name = winner_name,
    sex,
    ace = w_ace,
    df = w_df,
    svpt = w_svpt,
    first_in = w_1stIn,
    first_won = w_1stWon,
    second_won = w_2ndWon,
    sv_gms = w_SvGms,
    bp_saved = w_bpSaved,
    bp_faced = w_bpFaced
  ) %>%
  mutate(won = 1)

# Prepare Loser Data
losers <- all_matches %>%
  select(
    player_id = loser_id,
    player_name = loser_name,
    sex,
    ace = l_ace,
    df = l_df,
    svpt = l_svpt,
    first_in = l_1stIn,
    first_won = l_1stWon,
    second_won = l_2ndWon,
    sv_gms = l_SvGms,
    bp_saved = l_bpSaved,
    bp_faced = l_bpFaced
  ) %>%
  mutate(won = 0)

# Combine
player_matches <- bind_rows(winners, losers)

# 3. Filter and Average Stats
# Remove players with < 5 games in 3 years
# Calculate average of game stats
player_stats <- player_matches %>%
  group_by(player_id, player_name, sex) %>%
  summarise(
    n_games = n(),
    win_rate = mean(won),
    avg_ace = mean(ace, na.rm = TRUE),
    avg_df = mean(df, na.rm = TRUE),
    avg_svpt = mean(svpt, na.rm = TRUE),
    avg_first_in = mean(first_in, na.rm = TRUE),
    avg_first_won = mean(first_won, na.rm = TRUE),
    avg_second_won = mean(second_won, na.rm = TRUE),
    avg_sv_gms = mean(sv_gms, na.rm = TRUE),
    avg_bp_saved = mean(bp_saved, na.rm = TRUE),
    avg_bp_faced = mean(bp_faced, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_games >= 5) %>%
  na.omit() # Remove rows with NA in stats

message("Number of players after filtering (<5 games) and removing NAs: ", nrow(player_stats))

# 4. PCA
# Use average game stats for PCA. Exclude ID, Name, Sex, n_games, win_rate
pca_cols <- player_stats %>%
  select(starts_with("avg_"))

# Perform PCA (scale. = TRUE is important for stats with different units/magnitudes)
pca_res <- prcomp(pca_cols, scale. = TRUE)

# 4b. Scree Plot
# Calculate variance explained
var_explained <- pca_res$sdev^2
prop_var <- var_explained / sum(var_explained)
scree_data <- data.frame(
  PC = 1:length(prop_var),
  Variance = prop_var
)

# Plot
p_scree <- ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_line(color = "black", group = 1) +
  geom_point(color = "black") +
  geom_text(aes(label = scales::percent(Variance, accuracy = 0.1)), vjust = -0.5, size = 3) +
  scale_x_continuous(breaks = 1:length(prop_var)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()

ggsave("plots/wta_atp_pca_scree_plot.png", p_scree, width = 8, height = 6)
message("Saved scree plot to plots/wta_atp_pca_scree_plot.png")

# 4c. PC Loadings Plot
loadings <- as.data.frame(pca_res$rotation)
loadings$Variable <- rownames(loadings)

# Define dictionary
var_dict <- c(
  "avg_ace" = "Average Aces",
  "avg_df" = "Average Double Faults",
  "avg_svpt" = "Average Serve Points",
  "avg_first_in" = "Average 1st Serves In",
  "avg_first_won" = "Average 1st Serve Pts Won",
  "avg_second_won" = "Average 2nd Serve Pts Won",
  "avg_sv_gms" = "Average Serve Games",
  "avg_bp_saved" = "Average Break Points Saved",
  "avg_bp_faced" = "Average Break Points Faced"
)

# Map descriptions
loadings$Description <- var_dict[loadings$Variable]
# Fill NA if any variable is missing from dict (safety)
loadings$Description <- gsub("Average ", "", loadings$Description)
# Plot Loadings
p_loadings <- ggplot(loadings, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Description), size = 3) +
  geom_text(aes(label = Variable), vjust = -0.7, fontface = "bold", size = 3.5) +
  labs(
    x = paste0("PC1 Loadings (", round(prop_var[1]*100, 1), "% Variance)"),
    y = paste0("PC2 Loadings (", round(prop_var[2]*100, 1), "% Variance)"),
    color = "Variable Dictionary"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave("plots/pca_loadings_PC1_PC2.png", p_loadings, width = 10, height = 7)
message("Saved PCA loadings plot to plots/pca_loadings_PC1_PC2.png")

# 5. Clustering
# Cluster players by their PC stats.
# Using first few PCs that explain most variance.
# Using K=2 as requested to check for gender separation.
pca_scores <- as.data.frame(pca_res$x)
clustering_data <- pca_scores[, 1:4] # Using top 4 PCs for clustering logic still

set.seed(42)
k_clusters <- 2
km <- kmeans(clustering_data, centers = k_clusters, nstart = 25)

# Add results back to dataframe
player_stats$cluster <- as.factor(km$cluster)
# Add all top 4 PCs for plotting
player_stats <- bind_cols(player_stats, pca_scores[, 1:4]) 

# Check clustering vs sex
gender_cluster_table <- table(player_stats$sex, player_stats$cluster)
message("Cluster distribution by Sex:")
print(gender_cluster_table)

# --- NEW: Rank Level Analysis ---
message("Adding Rank Level Analysis...")

# Helper to load rankings
get_player_ranks <- function(path) {
  if(!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) %>%
    group_by(player) %>%
    summarise(best_rank = min(rank, na.rm = TRUE)) %>%
    mutate(player_id = as.character(player)) %>%
    select(player_id, best_rank)
}

wta_ranks <- get_player_ranks(file.path(data_dir, "WTA", "wta_rankings_20s.csv")) %>% mutate(sex = "WTA")
atp_ranks <- get_player_ranks(file.path(data_dir, "ATP", "atp_rankings_20s.csv")) %>% mutate(sex = "ATP")
all_ranks <- bind_rows(wta_ranks, atp_ranks)

player_stats <- player_stats %>%
  left_join(all_ranks, by = c("player_id", "sex")) %>%
  mutate(best_rank = replace_na(best_rank, 99999)) %>% # Handle unranked
  group_by(sex) %>%
  mutate(rank_level = ntile(best_rank, 30)) %>%
  ungroup() %>%
  mutate(rank_level = as.factor(rank_level))

# Get variance for labels (MOVED UP for plot use)
pc_vars <- summary(pca_res)$importance[2, ]

# Plot Win Rate (replacing Rank Level)
p_rank <- ggplot(player_stats, aes(x = PC1, y = PC2, color = win_rate)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_viridis_c(option = "plasma", name = "Win Rate", direction = -1) +
  facet_wrap(~sex, ncol = 1) +
  theme_minimal() +
  labs(
    x = paste0("PC1 (", round(pc_vars[1] * 100, 1), "% Var)"),
    y = paste0("PC2 (", round(pc_vars[2] * 100, 1), "% Var)")
  ) +
  coord_fixed() # Optional: helps maintain aspect ratio if desired, but unified scales are default in facet_wrap

ggsave("plots/combined_cluster_win_rate_panels.png", p_rank, width = 6, height = 9)
message("Saved win rate panel plot to plots/combined_cluster_win_rate_panels.png")

# --- NEW: Mis-clustered Analysis ---
# Determine dominant sex for each cluster
cluster_dominance <- player_stats %>%
  count(cluster, sex) %>%
  group_by(cluster) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(cluster, dominant_sex = sex)

player_stats <- player_stats %>%
  left_join(cluster_dominance, by = "cluster") %>%
  mutate(
    mis_clustered_type = case_when(
      sex != dominant_sex ~ paste("Mis-clustered", sex),
      TRUE ~ "Correct"
    )
  )

# Plot Mis-clustered
# Define colors manually to ensure Grey for Correct
p_mis <- ggplot(player_stats, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = mis_clustered_type), alpha = 0.7, size = 2) +
  scale_color_manual(values = c("Correct" = "grey85", 
                                "Mis-clustered WTA" = "#F8766D", 
                                "Mis-clustered ATP" = "#00BFC4")) +
  theme_minimal() +
  labs(
    color = "Status"
  )
ggsave("plots/combined_cluster_misclustered.png", p_mis, width = 10, height = 7)
message("Saved mis-clustered plot to plots/combined_cluster_misclustered.png")

# --- Identify Top Players for Labeling ---
# Function to get top N players IDs from rankings
get_top_ids <- function(rank_path, n=5) {
  rankings <- read_csv(rank_path, show_col_types = FALSE)
  latest_date <- max(rankings$ranking_date)
  rankings %>%
    filter(ranking_date == latest_date) %>%
    arrange(rank) %>%
    head(n) %>%
    pull(player) %>%
    as.character()
}

top_wta_ids <- get_top_ids(file.path(data_dir, "WTA", "wta_rankings_20s.csv"), 5)
top_atp_ids <- get_top_ids(file.path(data_dir, "ATP", "atp_rankings_20s.csv"), 5)

# Filter player_stats for these top players
top_players_data <- player_stats %>%
  filter(player_id %in% c(top_wta_ids, top_atp_ids)) %>%
  mutate(label = paste(player_name, paste0("(", sex, ")")))

message("Found ", nrow(top_players_data), " top ranked players in the analysis dataset.")

# 6. Plot PC Contrasts with Clusters and Gender
# Function to generate PC plot
create_pc_plot <- function(data, x_pc, y_pc, x_var, y_var, top_data) {
  p <- ggplot(data, aes(x = .data[[x_pc]], y = .data[[y_pc]], color = cluster, shape = sex)) +
    geom_point(alpha = 0.6) +
    # Add labels for top players
    geom_text(data = filter(top_data, sex == "WTA"),
              aes(label = player_name),
              color = "black", size = 3, vjust = -1, show.legend = FALSE) +
    geom_point(data = filter(top_data, sex == "WTA"), color = "black", shape = 1, size = 3, show.legend = FALSE) +
    geom_text(data = filter(top_data, sex == "ATP"),
              aes(label = player_name),
              color = "blue", size = 3, vjust = -1, show.legend = FALSE) +
    geom_point(data = filter(top_data, sex == "ATP"), color = "red", shape = 1, size = 3, show.legend = FALSE) +
    theme_minimal() +
    labs(
      x = paste0(x_pc, " (", round(x_var * 100, 1), "% Var)"),
      y = paste0(y_pc, " (", round(y_var * 100, 1), "% Var)")
    )
  return(p)
}

# Get variance for labels
pc_vars <- summary(pca_res)$importance[2, ]

# Generate plots for combinations
# 1 vs 2
p12 <- create_pc_plot(player_stats, "PC1", "PC2", pc_vars[1], pc_vars[2], top_players_data)
ggsave("plots/combined_cluster_PC1_vs_PC2.png", p12, width = 10, height = 7)

message("Saved cluster plots for PC combinations.")

# 7. Regression Model: Win Rate ~ PCs
reg_data <- bind_cols(player_stats %>% select(win_rate, sex), clustering_data)
# Including sex in regression to see effect
lm_model <- lm(win_rate ~ PC1 + PC2 + PC3 + PC4 + sex, data = reg_data)

# 8. Correlation Analysis (Win Rate vs PCs)
test_pc1 <- cor.test(player_stats$win_rate, player_stats$PC1)
test_pc2 <- cor.test(player_stats$win_rate, player_stats$PC2)
test_pc3 <- cor.test(player_stats$win_rate, player_stats$PC3)

# 9. Save Processed Data
processed_dir <- file.path(data_dir, "processed_data")
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}
write_csv(player_stats, file.path(processed_dir, "player_pca_clusters.csv"))
message("Saved processed player data to data/processed_data/player_pca_clusters.csv")

# 9. Document Findings
if (!dir.exists("documents")) {
  dir.create("documents")
}
output_file <- "documents/combined_analysis_findings.txt"
sink(output_file)

cat("Analysis of WTA & ATP Matches 2020-2024\n")
cat("=======================================\n\n")

cat("1. Data Processing\n")
cat("   - Combined WTA and ATP data.\n")
cat("   - Filtered players with fewer than 5 matches.\n")
cat("   - Total Players Analyzed: ", nrow(player_stats), "\n")
cat("   - Breakdown by Sex:\n")
print(table(player_stats$sex))
cat("\n")

cat("2. PCA Results\n")
cat("   - Summary:\n")
print(summary(pca_res))
cat("\n   - Loadings (Variable contributions to PCs):\n")
print(pca_res$rotation[, 1:min(5, ncol(pca_res$rotation))])
cat("\n")

cat("3. Clustering (K=2)\n")
cat("   - Goal: Check if clustering separates gender.\n")
cat("   - Contingency Table (Sex vs Cluster):\n")
print(gender_cluster_table)
cat("\n")
cat("   - Interpretation: Check if one cluster is predominantly WTA and the other ATP.\n")
cat("   - See 'combined_cluster_*.png' files for visualizations.\n\n")

cat("4. Regression Analysis: Win Rate ~ PCs + Sex\n")
cat("   - Model: win_rate ~ PC1 + PC2 + PC3 + PC4 + sex\n")
cat("   - Summary:\n")
print(summary(lm_model))
cat("\n")

cat("5. Correlation Analysis (Win Rate vs PCs)\n")
cat(sprintf("   - Win Rate vs PC1: r = %.4f, p-value = %.4g\n", test_pc1$estimate, test_pc1$p.value))
cat(sprintf("   - Win Rate vs PC2: r = %.4f, p-value = %.4g\n", test_pc2$estimate, test_pc2$p.value))
cat(sprintf("   - Win Rate vs PC3: r = %.4f, p-value = %.4g\n", test_pc3$estimate, test_pc3$p.value))

sink()
message("Saved findings to ", output_file)
