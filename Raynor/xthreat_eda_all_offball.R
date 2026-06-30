#off ball runs
#xthreat visualizations with whole season data
#Chaely Raynor
#29 June 2026

#libraries
library(tidyverse)
library(NatParksPalettes)

#set theme
set_theme(theme_bw())

#loading in events data
all_offball <- read_csv("C:\\Users\\chael\\OneDrive - Saint Vincent College\\CMSACamp\\Capstone Project\\allmatches_off_ball_only.csv")
all_offball<- all_offball[-1] #removing id column from when I saved the data



ggplot(all_offball, aes(y=event_subtype, x=xthreat, fill= event_subtype))+
  geom_boxplot()+
  theme_bw()+
  labs(x="Threat", y="Off Ball Run Type")+
  scale_fill_manual(values=natparks.pals("Yellowstone", 10))+
  theme(legend.position = "none")

#plots
ggplot(all_offball, aes(distance_covered, xthreat))+
  geom_point()+
  theme_bw()+
  labs(x="Distance Covered (meters)",
       y="Threat")

ggplot(all_offball, aes(separation_gain, xthreat))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  labs(x="Separation Gain", y="Threat")

ggplot(all_offball, aes(speed_avg, xthreat))+
  geom_point()+
  theme_bw()+
  labs(x="Average Speed", y="Threat")

ggplot(all_offball, aes(player_position, xthreat, fill= player_position))+
  geom_boxplot()+
  theme_bw()+
  labs(x="Player Position", y="Threat")+
  theme(legend.position = "none")+
  scale_fill_manual(values=natparks.pals("Banff", 10))


ggplot(all_offball, aes(y=event_subtype, x=xthreat, fill= event_subtype))+
  geom_boxplot()+
  theme_bw()+
  labs(x="Threat", y="Off Ball Run Type")+
  scale_fill_manual(values=natparks.pals("Yellowstone", 10))+
  theme(legend.position = "none")

#players with most threatening off ball runs
all_offball|>
  group_by(player_name, team_shortname)|>
  summarize(n_runs = n(),
            avg_threat = mean(xthreat),)|>
  ungroup()|>
  arrange(desc(avg_threat))|>
  head(5)

