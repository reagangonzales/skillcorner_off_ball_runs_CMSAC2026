#Animations of off ball runs
#Chaely Raynor
#15 July 2026

#load in one game
library(tidyverse)
library(jsonlite)
library(sportyR)
library(gganimate)
filepath = "C:\\Users\\chael\\OneDrive - Saint Vincent College\\CMSACamp\\Capstone Project\\"
events = read_csv(paste0(filepath, "1832186_dynamic_events.csv"))
tracking <- stream_in(file(paste0(filepath,
                                  "1832186_tracking_extrapolated.jsonl")))

unique(events$event_subtype)

dir.create("animations", showWarnings = FALSE)
anim_save("animations/cross_receiver_pitch.gif", animation = anim)

####Function for animating off ball runs
animate_off_ball_run <- function(event_row, tracking_df) {
  
  # Extract event info
  start_f <- event_row$frame_start
  end_f   <- event_row$frame_end
  receiver_id <- event_row$player_id
  carrier_id  <- event_row$player_in_possession_id
  
  # Filter tracking frames
  frames <- tracking_df |>
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
    labs(title = paste0(event_row$event_subtype, " — Frame {frame}"))
  
  # Return animation object
  animate(p, fps = 20, width = 800, height = 500,
          renderer = gifski_renderer())
}

#cross receiver run animation
cross_event <- events |>
  filter(event_subtype == "cross_receiver") |>
  slice_sample(n=1)

cross_receiver_anim <- animate_off_ball_run(cross_event, tracking)

#dropping off run animation
droppingoff_event <- events |>
  filter(event_subtype == "dropping_off") |>
  slice_sample(n=1)

dropping_off_anim <- animate_off_ball_run(droppingoff_event, tracking)

#underlap run animation
underlap_event <- events |>
  filter(event_subtype == "underlap") |>
  slice_sample(n=1)

underlap_anim <- animate_off_ball_run(underlap_event, tracking)

#pulling wide run animation
pullingwide_event <- events |>
  filter(event_subtype == "pulling_wide") |>
  slice_sample(n=1)

pulling_wide_anim <- animate_off_ball_run(pullingwide_event, tracking)

#run_ahead_of_the_ball off run animation
ahead_event <- events |>
  filter(event_subtype == "run_ahead_of_the_ball") |>
  slice_sample(n=1)

ahead_anim <- animate_off_ball_run(ahead_event, tracking)

#coming short run animation
comingshort_event <- events |>
  filter(event_subtype == "coming_short") |>
  slice_sample(n=1)

coming_short_anim <- animate_off_ball_run(comingshort_event, tracking)

#pulling half space run animation
pullinghs_event <- events |>
  filter(event_subtype == "pulling_half_space") |>
  slice_sample(n=1)

pullinghs_anim <- animate_off_ball_run(pullinghs_event, tracking)

#support run animation
support_event <- events |>
  filter(event_subtype == "support") |>
  slice_sample(n=1)

support_anim <- animate_off_ball_run(support_event, tracking)

#behind run animation
behind_event <- events |>
  filter(event_subtype == "behind") |>
  slice_sample(n=1)

behind_anim <- animate_off_ball_run(behind_event, tracking)

#overlap run animation
overlap_event <- events |>
  filter(event_subtype == "overlap") |>
  slice_sample(n=1)

overlap_anim <- animate_off_ball_run(overlap_event, tracking)
