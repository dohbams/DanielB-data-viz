# ----- Load libraries ---- #
library(tidyverse)
library(RColorBrewer)

# ----- This section prepares and cleans our dataframe ---- #

mw = read.csv('Minimum Wage.csv', header = T)
mw = mw %>%  
  mutate(hourly_usd = as.numeric(as.character(hourly_usd)), 
         annual_usd = as.numeric(as.character(annual_usd)), 
         workweek_hours = as.numeric(as.character(workweek_hours)))

clean_mw = mw %>% 
  drop_na(hourly_usd) %>% 
  arrange(desc(hourly_usd)) %>% 
  mutate(country = factor(country, country))

data = clean_mw[1:20,] %>%
  select(country, annual_usd, hourly_usd) %>%
  arrange(desc(annual_usd))

df = data  %>% select(country, annual_usd)  

df = df[-c(11:20),]

# ----- This section prepares our dataframe for labels ---- #

len = 4

df2 = data.frame(country = letters[1:len], 
                 annual_usd = rep(0,len), 
                 country2 = rep("", len))

df$country2 = paste0(df$country, " - ", "$ ", df$annual_usd, " " )

df = rbind(df, df2)

df$country = factor(df$country, levels = rev(df$country))

# ----- This section sets the colors for our viz ---- #
colr = c("#1f005c", "#5b0060", "#870160", "#ac255e", "#ca485c", 
         "#e16b5c", "#f39060", "#ffb56b", "#f99e43", "#cd731a", "#814407")

# ----- This section constructs the layers of our viz ---- #

ggplot(df, aes(x = country, y = annual_usd,
               fill = country2)) + 
  # create a bar plot
  geom_bar(width = 0.3, stat="identity") + 
  # set a manual color palette
  scale_fill_manual(values = colr) +
  # curve the bar chart 
  coord_polar(theta = "y") +
  # remove the x and y axis labels
  xlab("") + ylab("") +
  # let the "circumference" of the racetrack/radial bars
  ylim(c(0,35100)) +
  # add a title at the top of the chart
  ggtitle("Top 10 Countries by Annual Minimum Wage in USD") +
  # add a subtitle  and caption
  labs(subtitle = "\nThe viz below summarises the top 10 countries by annual nominal minimum wage in USD as at 2022.",
       caption = "Viz by : Daniel Bamidele\nData source : Wikipedia") +
  # add the legend text
  geom_text(data = df, hjust = 1, size = 3,
            aes(x = country, y = 0, label = country2)) +
  # use the minimal theme to make it pretty
  theme_minimal() +
  # remove the external legend, format text elements and remove all the extra elements
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        plot.caption = element_text(size = 10, face = "italic", hjust = 0, vjust = 10 ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  theme(plot.background = element_rect(fill = "#FFF7E4"))

# save the plot

ggsave("ggfy_min_wage.png" , width = 10, height = 12, dpi = 500)