#Correlation plot for numeric variables
#Chaely Raynor
#7 July 2026

library(tidyverse)

all_offball <- read_csv("C:\\Users\\chael\\OneDrive - Saint Vincent College\\CMSACamp\\Capstone Project\\allmatches_off_ball_only.csv")

offball_fewervars <- all_offball |>
  filter((is_player_possession_start_matched == TRUE) & 
           (is_player_possession_end_matched==TRUE))|>
  select(event_id, index, match_id, frame_start, frame_end, duration, event_subtype, 
         player_position, x_start, y_start, x_end, y_end,
         game_state, team_in_possession_phase_type, team_out_of_possession_phase_type,
         lead_to_shot, targeted, received, received_in_space, distance_covered, 
         trajectory_direction, speed_avg, location_to_player_in_possession_start,
         location_to_player_in_possession_end, inside_defensive_shape_start, inside_defensive_shape_end,
         separation_gain, n_simultaneous_runs, give_and_go, intended_run_behind, push_defensive_line,
         break_defensive_line, passing_option_at_start, n_opponents_overtaken, xthreat)

offball_subset <- offball_fewervars|>
  filter(xthreat != is.na(xthreat) & speed_avg != is.na(speed_avg))


#Numeric variables subset
offball_numeric <- offball_subset |>
  select(xthreat, speed_avg, duration, x_start, y_start, x_end, y_end, n_simultaneous_runs,
         separation_gain, n_opponents_overtaken, distance_covered)

#correlation plot on these numeric variables
library(ggcorrplot)
corr <- cor(offball_numeric, use = "pairwise.complete.obs")
ggcorrplot(corr, type = "lower", lab = TRUE)

