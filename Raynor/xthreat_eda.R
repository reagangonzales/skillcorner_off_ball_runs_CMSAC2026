#EDA on xthreat variable in the dynamic events
#Chaely Raynor
# 26 June 2026

#libraries
library(tidyverse)
library(NatParksPalettes)

#data
filepath = "C:\\Users\\chael\\OneDrive - Saint Vincent College\\CMSACamp\\Capstone Project\\"
events = read_csv(paste0(filepath, "1832186_dynamic_events.csv"))

#subset of just off ball runs
off_ball <- events|> filter(event_type == "off_ball_run")


#plots
ggplot(off_ball, aes(distance_covered, xthreat))+
  geom_point()+
  theme_bw()+
  labs(x="Distance Covered (meters)",
       y="Threat")

ggplot(off_ball, aes(separation_gain, xthreat))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  labs(x="Separation Gain", y="Threat")

ggplot(off_ball, aes(speed_avg, xthreat))+
  geom_point()+
  theme_bw()+
  labs(x="Average Speed", y="Threat")

ggplot(off_ball, aes(player_position, xthreat, fill= player_position))+
  geom_boxplot()+
  theme_bw()+
  labs(x="Player Position", y="Threat")+
  theme(legend.position = "none")+
  scale_fill_manual(values=natparks.pals("Banff", 10))


ggplot(off_ball, aes(y=event_subtype, x=xthreat, fill= event_subtype))+
  geom_boxplot()+
  theme_bw()+
  labs(x="Threat", y="Off Ball Run Type")+
  scale_fill_manual(values=natparks.pals("Yellowstone", 10))+
  theme(legend.position = "none")

#players with most threatening off ball runs
events|>
  filter(event_type == "off_ball_run")|>
  group_by(player_name, team_shortname)|>
  summarize(n_runs = n(),
            avg_threat = mean(xthreat),)|>
  ungroup()|>
  arrange(desc(avg_threat))|>
  head(5)|>
  gt()

events|>
  filter(event_type == "off_ball_run" & player_name == "P. Agyemang")|>
  count(event_subtype)
#the most threatening player mostly has cross_receiver runs