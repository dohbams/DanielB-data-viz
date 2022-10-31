# ----- Load libraries ---- #
library(tidyverse)
library(RColorBrewer)
library(extrafont)

# ----- This section prepares and cleans our dataframe ---- #
db = read.csv("Dumbbell.csv" , header = T)

# ----- This section cleans and prepares our dataframe for labels ---- #
db = db %>% mutate(commodity = Commodity) %>% 
            select(commodity, August.2021, August.2022) %>% 
            mutate(shift = round(August.2022 - August.2021, 1),
                   mn = round(((August.2022 + August.2021) / 2), 1)) %>% 
            arrange(desc(shift)) %>% 
            mutate(lbl_2022 = paste0("-N- ", August.2022), 
                   lbl_2021 = paste0("-N- ", August.2021))
db2 = db %>% slice_min(shift, n = 15) %>%  pivot_longer(2:3, names_to = "year", values_to = "price") %>% mutate(paired = rep(1:(n()/2),each=2), year=factor(year))


# ----- This section constructs the layers of our viz ---- #

ggplot( data = db2,aes(x= price, y= reorder(commodity,-shift))) +
  # create a line plot
  geom_line(aes(group = paired),color="grey")+
  # create a point plot
  geom_point(aes(color=year), size=6) +
  #  Add labels to the center of each dumbbell
  geom_text(aes(label = commodity, x = mn, family = "Tw Cen MT"), nudge_y = 0.3) + 
  #  Add labels to the left of each dumbbell
  geom_text(data = db, color= "black", size = 3, hjust = 3, aes(x=August.2021, label= lbl_2021, family = "Tw Cen MT"))+
  #  Add labels to the right of each dumbbell
  geom_text(data = db, color= "black", size = 3, hjust = -2,aes(x=August.2022, label= lbl_2022, family = "Tw Cen MT")) +
  # Add a title and caption
  labs(title = "Average Price of Petroleum Products Between August 2021 and August 2022 in Nigeria", 
       caption = " Data source : NBS | tidyverse R package | @doh_bams ") + 
  # Limit the x axis
  xlim(c(0,1000)) +
  # use the minimal theme to make it pretty
  theme_minimal()+
  # Set font style, size and color. Remove grid and y axis label. Format legend. Format plot.
  theme(plot.title = element_text(size = 20, family = "Tw Cen MT", hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic", hjust = 0.5, vjust = 0,  family = "Tw Cen MT"),
        plot.background = element_rect(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(2,2,2,2, "cm"),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, family = "Tw Cen MT", vjust = 25),
        axis.ticks = element_blank()) +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12,family = "Tw Cen MT")) 

# save the plot

ggsave("db-chart.png" , width = 12, height = 12, dpi = 1000)
