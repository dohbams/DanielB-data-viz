# load libraries
library(tidyverse)
library(ggsoccer)
library(ggtext)
library(StatsBombR)
library(extrafont)

# get data

# First, we need to look at all competitions that StatsBomb provides. Here is the code for doing that
comp <- FreeCompetitions()

# Let’s filter the data to the competition we want by running this code below:
ucl <- comp |> filter(season_id == 37, country_name == 'Europe')

# Next, we retrieve all matches that correspond to that competition/league and season. Here is the code for doing that:
matches <- FreeMatches(ucl)

# After we get all matches, we can retrieve the event data by running this line of code:
events <- get.matchFree(matches)

# And lastly, we clean the data. Here is the code for doing that:
clean_df <- allclean(events)

# Subset the data for all shots taken by AC Milan in the match
ac_shot <- clean_df |> filter(team.name == 'AC Milan') |> filter(type.name == 'Shot') |> 
  drop_na(shot.outcome.name) |> mutate(team.name = as.factor(team.name)) |> 
  mutate(shot.outcome.name = str_replace(shot.outcome.name, 'Off T', 'Off Target')) |> 
  select(player.name, location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg, shot.outcome.name)

# Subset the data for all shots taken by Liverpool in the match
liv_shot <- clean_df |> filter(team.name == 'Liverpool') |> filter(type.name == 'Shot') |> 
  drop_na(shot.outcome.name) |> mutate(team.name = as.factor(team.name)) |> 
  mutate(shot.outcome.name = str_replace(shot.outcome.name, 'Off T', 'Off Target')) |> 
  select(player.name, location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg, shot.outcome.name)

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour='#6594C0', fill='#011021') +
  geom_point(data=ac_shot, aes(x=location.x, y=location.y, size=shot.statsbomb_xg
                               ), color="white") +
  geom_point(data=liv_shot, aes(x=120-location.x, y=location.y, size=shot.statsbomb_xg
                                ), color="#b30000") +
  labs(
    title="<span style='color:white;'>AC Milan</span> <span style='color: #6594C0;'>vs</span> <span style='color: #b30000;'>Liverpool<span>",
    subtitle = "Shots Map | UEFA Champions League Final 2004/2005. <br> <b> The <span style='color:white;'>white</span> circles represent AC Milan while the <span style='color:#b30000;'>red</span> circles represent Liverpool",
    
    caption="Data Source: StatsBomb"
  ) + 
  labs(color = 'Teams') +
  facet_wrap(~shot.outcome.name) +
  theme(
    plot.background = element_rect(fill='#011021', color='#011021'),
    panel.background = element_rect(fill='#011021', color='#011021'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family= "Tw Cen MT", color='white'),
    plot.title = element_markdown(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_markdown(hjust=0.5, vjust=0, size=9, colour = '#6594C0'),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none",
    legend.background = element_rect(fill='#011021', color='#011021'),
    legend.text = element_text(family= "Tw Cen MT", color='black'),
    strip.text = element_text(size = 12, family = "Tw Cen MT", hjust = 0.5, color = 'white'),
    strip.background = element_rect(fill='#011021', color='#011021')
  )

# save the plot
