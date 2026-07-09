library(tidyverse)

# 1. Get paths to all CSV files in the folder
file_paths <- list.files(path = "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research/skillcorner_CMSAC2026/dynamic_events", 
                         pattern = "\\.csv$", full.names = TRUE)

# 2. Read and stack them into a single data frame
#all_matches <- read_csv(file_paths, id = "source_file")

# Only look at off-ball runs
all_matches_offball <- all_matches |>
  filter(event_type == "off_ball_run") |>
  filter(is_player_possession_start_matched == TRUE & is_player_possession_end_matched == TRUE)

# 0.7% of xthreat values are zeros
sum(all_matches_offball$xthreat == 0, na.rm = TRUE) / length(na.omit(all_matches_offball$xthreat))

# Distribution of xthreat
all_matches_offball |>
  ggplot(aes(x=xthreat)) +
  geom_histogram()

# Bar chart xthreat by run types
all_matches_offball |>
  group_by(event_subtype) |>
  summarize(mean_xthreat_rate = mean(xthreat, na.rm = TRUE), .groups = 'drop') |>
  ggplot(aes(x= fct_reorder(event_subtype, desc(mean_xthreat_rate)), y = mean_xthreat_rate)) +
  geom_col() +
  labs(x = "", y = "Mean Xthreat Rate") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
