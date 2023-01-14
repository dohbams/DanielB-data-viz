# Load libraries
library(tidyverse)
library(ggbump)
library(plotly)
library(extrafont)
library(ggimage)
library(sysfonts)
library(ggtext)
library(showtext)
library(RColorBrewer)
library(grid)
library(patchwork)

# load data for the bump chart
data = read.csv('pl_all.csv', header = T)

# add a color for each team
data = data |> mutate(colr2 = case_when(Club == "Arsenal"  ~ '#B72824',
                                        Club == "Man City"  ~ '#92C6E7',
                                        Club == "Newcastle"  ~ '#424449',
                                        Club == "Man Utd"  ~ '#E70306',
                                        Club == "Spurs"  ~ '#EFEDEF',
                                        Club == "Liverpool"  ~ '#EE293B',
                                        Club == "Fulham"  ~ '#D5D4DC',
                                        Club == "Brighton"  ~ '#0E7BE5',
                                        Club == "Brentford"  ~ '#DA0C20',
                                        Club == "Chelsea"  ~ '#054D8F',
                                        Club == "Aston Villa"  ~ '#7E2032',
                                        Club == "Crystal Palace"  ~ '#EFEDEF',
                                        Club == "Leicester"  ~ '#133ABD',
                                        Club == "Leeds"  ~ '#DCDADD',
                                        Club == "Nottm Forest"  ~ '#E9EA38',
                                        Club == "Bournemouth"  ~ '#E8403D',
                                        Club == "West Ham"  ~ '#87201F',
                                        Club == "Everton"  ~ '#1554AB',
                                        Club == "Wolves"  ~ '#F69001',
                                        Club == "Southampton"  ~ '#F01C32'))


# start the plot for the bump chart
p1 = ggplot(data, aes(x = Matchday, y = desc(Position), group = Club)) +
  geom_bump(size = 1,smooth = 12, aes(color = colr2), lineend = 'round') + 
  geom_point(data = data |> filter(Matchday == 19), size = 2, aes(color = colr2)) +
  geom_text(data = data |>  filter(Matchday == max(Matchday)),
            aes(x = Matchday + 0.3, label = Club, family = 'Tw Cen MT'),
            size = 2.5, hjust = 0, fontface = 'bold' 
            ) +
  labs(title = '
                How has team ranking changed since matchday 1?
      ') +
  scale_x_continuous(breaks = c(0,5,10,15,19))+
  scale_color_identity() +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(2,2,2,2),
        text = element_text(family= "Tw Cen MT", color='#37003C'),
        plot.title=element_text(hjust=0.5, vjust=0, size=16, face = 'bold.italic' ),
        axis.text.x = element_text(size = 10, vjust = 6),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

# preview plot                
#p1

# load data for current table
mw_19 = read.csv('mw_19.csv', header = T)

mw_19 = mw_19[,-1]

# set spacing for the table elements (a lot of trial and error here)
spacing= seq(from=3, to=8, length.out=8)
spacing = spacing + 0.2
spacing[4] = spacing[4] + 0.125

#df for table header
headers = data.frame(
  text = c("Rank/Club", "Played","W","D","L","GF","GA","GD","Pts"),
  xpos = c(0.7, spacing)
)
text_size=2.4

df_pl <-mw_19 |>
  pivot_longer(cols=c(Played, W,D,L,GF,GA,GD,Pts))|>
  mutate(xpos = case_when(name=="Played" ~ spacing[1],
                          name=="W" ~ spacing[2], 
                          name=="D" ~ spacing[3],
                          name=="L" ~spacing[4], 
                          name=="GF" ~spacing[5], 
                          name=="GA" ~spacing[6],
                          name=="GD" ~spacing[7],
                          name== "Pts" ~ spacing[8]),
         colr2 = case_when(Club == "Arsenal"  ~ '#B72824',
                           Club == "Man City"  ~ '#92C6E7',
                           Club == "Newcastle"  ~ '#424449',
                           Club == "Man Utd"  ~ '#E70306',
                           Club == "Spurs"  ~ '#EFEDEF',
                           Club == "Liverpool"  ~ '#EE293B',
                           Club == "Fulham"  ~ '#D5D4DC',
                           Club == "Brighton"  ~ '#0E7BE5',
                           Club == "Brentford"  ~ '#DA0C20',
                           Club == "Chelsea"  ~ '#054D8F',
                           Club == "Aston Villa"  ~ '#7E2032',
                           Club == "Crystal Palace"  ~ '#EFEDEF',
                           Club == "Leicester"  ~ '#133ABD',
                           Club == "Leeds"  ~ '#DCDADD',
                           Club == "Nottm Forest"  ~ '#E9EA38',
                           Club == "Bournemouth"  ~ '#E8403D',
                           Club == "West Ham"  ~ '#87201F',
                           Club == "Everton"  ~ '#1554AB',
                           Club == "Wolves"  ~ '#F69001',
                           Club == "Southampton"  ~ '#F01C32')
         )
# start the plot for the table

p2 = ggplot(data)+
  #headers
  geom_text(data=headers, mapping=aes(x=xpos, y=0.2, label=text), size=2.8, family="Tw Cen MT" )+
  #team
  geom_text(df_pl,  mapping=aes(x=1.5, y=Position, label = Club), hjust=0, size=text_size, family="Tw Cen MT")+
  #rank
  geom_text(mapping=aes(x=0.25, y=Position, label=Position), hjust=0, size=text_size, family="Tw Cen MT" )+
  #table lines
  geom_segment(mapping=aes(x=0, xend=9, y=Position-0.5, yend=Position-0.5), color="#DADADA")+
  #points data  
  geom_text(data=df_pl, mapping=aes(x=xpos, y=Position, label=value), size=text_size, family="Tw Cen MT" )+
  #adjust scales for axis and coordinates
  scale_y_reverse(limits=c(20.5,0), expand=c(0,0))+
  labs(title = 'Standings as at matchday 19') + 
  theme_minimal()+
  theme(
    axis.text=element_blank(),
    axis.title=element_blank(),
    panel.grid = element_blank(),
    panel.spacing.x = unit(1.5,"lines"),
    text = element_text(family= "Tw Cen MT", color='#37003C'),
    plot.title=element_text(hjust=0.5, vjust=0, size=16, face = 'bold.italic'),
    plot.margin = margin(b=1)) +
  coord_equal()

p = p1+ p2 + plot_annotation(title = '
                             The Premier League so far...',
                             theme = theme(plot.title = element_text(size = 45, face = 'bold', hjust = 0.5, color = '#37003C')),
                             caption = 'Data : Transfermarkt | @Doh_Bams') & 
  theme(text = element_text('Tw Cen MT'))


#save plot
ggsave(plot = p, "pl-tab.png", bg="white", height=11, width=19, units='in', dpi = 500)
  




