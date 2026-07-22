#Figures for Presentation
#Chaely Raynor
#15 July 2026

#Loading in the event data of all offball runs
library(tidyverse)
library(gt)
library(NatParksPalettes)
library(sportyR)

#loading in events data
all_offball <- read_csv("C:\\Users\\chael\\OneDrive - Saint Vincent College\\CMSACamp\\Capstone Project\\allmatches_off_ball_only.csv")

all_offball <- all_offball |>
  mutate(xthreat = if_else(xthreat == 0, 0.005, xthreat))
#creating run_type variable and dropping nas
offball_clean <- all_offball |>
  filter(!is.na(xthreat) & !is.na(speed_avg))|>
  mutate(run_type = case_when(event_subtype %in% c( "pulling_wide","pulling_half_space") ~ "width",
                              event_subtype %in% c("coming_short", 
                                                   "dropping_off") ~ "support to receive",
                              event_subtype %in% c("run_ahead_of_the_ball", 
                                                   "support") ~ "progressive support",
                              event_subtype %in% c("underlap", "overlap", "behind") ~ "penetrative runs",
                              event_subtype == "cross_receiver" ~ "cross receiver"),
         run_type = factor(run_type, 
                           levels = c("cross receiver", "penetrative runs", 
                                      "progressive support","width", "support to receive")))
offball_clean <- offball_clean|>
  filter(x_end >= 0)

#table for data slide
offball_clean|>
  select("Match ID" = match_id, "Player Name" = player_name,
         "Team Name" = team_shortname, "X End" = x_end, "Y End" = y_end, "Run Type" = run_type,
         "Avg Speed" = speed_avg, "Dist to Player in Poss" = distance_to_player_in_possession_end,
         "N Simultaneous Runs" = n_simultaneous_runs, "Game State" = game_state)|>
  slice_sample(n =5)|>
  gt()



#xthreat histogram
# Distribution of xthreat
offball_clean |>
  ggplot(aes(x=(xthreat))) +
  geom_histogram(fill= "#00a7d1", color = "grey5",
                 bins = 30)+
  theme_bw()+
  labs(x="Threat", y="Count")

#log transform histogram
offball_clean |>
  ggplot(aes(x=log(xthreat))) +
  geom_histogram(fill= "#00a7d1", color = "grey5",
                 bins = 30)+
  theme_bw()+
  labs(x="Log(Threat)", y="Count")


#mean xthreat by run type

offball_clean |> 
  group_by(event_subtype, run_type) |> 
  summarize(mean_xthreat_rate = mean(xthreat, na.rm = TRUE), .groups = 'drop') |> 
  mutate(order_val = as.numeric(run_type) * -1000 + mean_xthreat_rate) |> 
  ggplot(aes(
    x = reorder(event_subtype, desc(order_val)),
    y = mean_xthreat_rate,
    fill = run_type
  )) +
  geom_col(color = "grey5") +
  labs(x = "", y = "Mean Xthreat Rate", fill = "Run Type") +
  scale_fill_manual(values=natparks.pals("Yellowstone", 5))+
  scale_x_discrete(labels = c("cross receiver", "behind", "underlap", "overlap",
                              "support", "run ahead of ball", "pulling half space",
                              "pulling wide", "coming short", "dropping off"))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


## beta distribution for reference
#define range
p = seq(0,1, length=100)

#create plot of Beta distribution with shape parameters 2 and 10
plot(p, dbeta(p, 2, 10), type='l')

#hexbin plot of field location and xthreat
geom_soccer(league = "mls") +
  stat_bin_hex(data = offball_clean, aes(x=x_end, y=y_end, z= xthreat),
               binwidth = c(2,2),
               fun=mean, color = "grey5",
               alpha = 0.9) +
  scale_fill_gradient(low = "midnightblue", high = "goldenrod", name = "Average Threat") +
  labs(x="", y="")+
  theme_classic()+
  theme(legend.position)


ggplot(data = offball_clean, 
            aes(x=x_end, y=y_end, z=xthreat))+
  stat_summary_hex(binwidth = c(2, 2), fun = mean, 
                   color = "black", linewidth = 0.05)

#sample normal distribution
x <- seq(-4, 4, length = 100)
y <- dnorm(x, mean = 0, sd = 1)
plot(x, y, type = "l", main = "Normal Distribution Density")
  
