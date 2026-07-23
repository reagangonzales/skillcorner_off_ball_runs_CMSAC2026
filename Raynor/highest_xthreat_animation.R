#highest threat run animiation
library(tidyverse)
library(jsonlite)
library(sportyR)
library(gganimate)

filepath = "C:\\Users\\chael\\OneDrive - Saint Vincent College\\CMSACamp\\Capstone Project\\"
tracking <- fromJSON(paste0(filepath, "match_1105572_tracking.json"))
all_matches_offball <- read_csv("C:\\Users\\chael\\OneDrive - Saint Vincent College\\CMSACamp\\Capstone Project\\allmatches_off_ball_only.csv")
# Only look at off-ball runs
all_matches_offball <- all_matches_offball |>
  filter(event_type == "off_ball_run")

# Mutate 0 xthreat values to 0.005
all_matches_offball <- all_matches_offball |>
  mutate(xthreat = if_else(xthreat == 0, 0.005, xthreat))

# Drop missing data
all_matches_offball <- all_matches_offball |> drop_na(xthreat)
all_matches_offball <- all_matches_offball |> drop_na(speed_avg)

# Dropped off ball runs with xthreat - 0 (0.7% of data)
# Condensed off ball run types from 10 types to 4
# Made penetrating offball run baseline
# Adjust x and y scale relative to distance to attacking goal
offball_clean <- all_matches_offball |>
  filter(x_end >= 0) |>
  mutate(
    x_to_goal = 54.5 - x_end,   # 0 = at goal line, larger = further from goal
    y_to_goal = abs(y_end),      # 0 = central, larger = wider
    
    # Cap at n_sim_runs 5+ but keep as factor
    n_simultaneous_runs = factor(
      if_else(n_simultaneous_runs >= 5, 5L,
              as.integer(n_simultaneous_runs)),
      levels = c(0, 1, 2, 3, 4, 5),
      labels = c("0", "1", "2", "3", "4", "5+")),
    
    run_type = case_when(event_subtype %in% c("dropping_off","coming_short") ~ "support_to_receive",
                         event_subtype %in% c("pulling_wide", "pulling_half_space") ~ "width_creation",
                         event_subtype %in% c("support", "run_ahead_of_the_ball") ~ "progressive_support",
                         event_subtype %in% c("overlap", "underlap", "behind") ~ "penetrative_runs",
                         event_subtype %in% c("cross_receiver") ~ "cross_receiver"),
    run_type = factor(run_type,
                      levels = c("cross_receiver", "support_to_receive",
                                 "width_creation", "progressive_support",
                                 "penetrative_runs"))) |>
  mutate(x_to_goal = ifelse(x_to_goal < 0, 0, x_to_goal))

max_xthreat_event <- offball_clean|>
  arrange(desc(xthreat))|>
  slice_head(n=1)

# Extract event info
start_f <- max_xthreat_event$frame_start
end_f   <- max_xthreat_event$frame_end
receiver_id <- max_xthreat_event$player_id
carrier_id  <- max_xthreat_event$player_in_possession_id

# Filter tracking frames
frames <- tracking |>
  filter(frame >= start_f & frame <= end_f)

# Unnest players + ball
frames <- frames |>
  unnest(player_data) |>
  unnest(ball_data, names_sep = "_ball") |>
  mutate(
    is_receiver = player_id == receiver_id,
    is_carrier  = player_id == carrier_id
  )

# Build animation on MLS pitch
p <- geom_soccer(league = "mls") +
  
  geom_point(data = frames,
             aes(x = x, y = y),
             color = "gray6", size = 3, alpha = 0.8) +
  
  geom_point(data = filter(frames, is_receiver),
             aes(x = x, y = y),
             color = "red", size = 6) +
  
  geom_point(data = filter(frames, is_carrier),
             aes(x = x, y = y),
             color = "blue", size = 5) +
  
  geom_point(data = frames,
             aes(x = ball_data_ballx, y = ball_data_bally),
             color = "white", size = 4) +
  
  coord_fixed() +
  transition_time(frame) +
  labs(title = paste0(max_xthreat_event$event_subtype, " — Frame {frame}"))

# Return animation object
animate(p, fps = 20, width = 800, height = 500,
        renderer = gifski_renderer())
