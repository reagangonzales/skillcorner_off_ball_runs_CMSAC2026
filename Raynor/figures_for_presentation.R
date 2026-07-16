#Figures for Presentation
#Chaely Raynor
#15 July 2026

#Loading in the event data of all offball runs
library(tidyverse)
library(gt)

#loading in events data
all_offball <- read_csv("C:\\Users\\chael\\OneDrive - Saint Vincent College\\CMSACamp\\Capstone Project\\allmatches_off_ball_only.csv")


#creating run_type variable and dropping nas
offball_clean <- all_offball |>
  filter(!is.na(xthreat) & !is.na(speed_avg))|>
  mutate(run_type = case_when(event_subtype %in% c( "pulling_wide","overlap", 
                                                    "underlap", 
                                                    "cross_receiver") ~ "width",
                              event_subtype %in% c("coming_short", 
                                                   "dropping_off", 
                                                   "support")  ~ "depth_dropping",
                              event_subtype %in% c("run_ahead_of_the_ball", 
                                                   "behind") ~ "penetrating",
                              event_subtype == "pulling_half_space" ~ "half_space"),
         run_type = factor(run_type, 
                           levels = c("penetrating", "width", 
                                      "depth_dropping", "half_space")))


#table for data slide
offball_clean|>
  select("Match ID" = match_id, "Player Name" = player_name,
         "Team Name" = team_shortname, "X End" = x_end, "Y End" = y_end, "Run Type" = run_type,
         "Avg Speed" = speed_avg, "Dist to Player in Poss" = distance_to_player_in_possession_end,
         "N Simultaneous Runs" = n_simultaneous_runs, "Game State" = game_state)|>
  slice_sample(n =10)|>
  gt()



#xthreat histogram
# Distribution of xthreat
offball_clean |>
  ggplot(aes(x=(xthreat))) +
  geom_histogram(fill= "#00a7d1", color = "grey5",
                 bins = 30)+
  theme_bw()+
  labs(x="Threat", y="Count")

#
