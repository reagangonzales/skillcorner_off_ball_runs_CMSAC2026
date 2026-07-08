#Initial Beta Regression
#Chaely Raynor
#7 July 2026

library(tidyverse)
filepath = "C:\\Users\\chael\\OneDrive - Saint Vincent College\\CMSACamp\\Capstone Project\\"
data = read_csv(paste0(filepath, "1832186_dynamic_events.csv"))

sample_offball <- data |>
  filter(event_type == "off_ball_run")

library(betareg)

offball_logit <- betareg(xthreat ~ + x_end  + y_end + trajectory_direction + 
                           event_subtype + speed_avg + separation_gain + 
                           delta_to_last_defensive_line_gain +
                           n_simultaneous_runs +
                           distance_to_player_in_possession_end + 
                           location_to_player_in_possession_start, data = sample_offball)
summary(offball_logit)

library(broom)
print(tidy(offball_logit), n=24)


