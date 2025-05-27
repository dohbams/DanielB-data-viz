# Load required libraries
library(tidyverse)
library(ggbump)
library(extrafont)
library(ggtext)
library(grid)
library(patchwork)
library(magick)
library(cowplot)

# Load and prepare bump chart data
data = read.csv('pl_data.csv', header = T)

# Assign team colors
data = data |> mutate(colr2 = case_when(
  Club == "Arsenal" ~ '#B72824',
  Club == "Man City" ~ '#92C6E7',
  Club == "Newcastle" ~ '#3b3b3b',
  Club == "Man Utd" ~ '#E70306',
  Club == "Tottenham" ~ '#1f5452',
  Club == "Liverpool" ~ '#EE293B',
  Club == "Fulham" ~ '#D5D4DC',
  Club == "Brighton" ~ '#0E7BE5',
  Club == "Brentford" ~ '#e79bb7',
  Club == "Chelsea" ~ '#054D8F',
  Club == "Aston Villa" ~ '#7E2032',
  Club == "Crystal Palace" ~ 'purple',
  Club == "Southampton" ~ '#b12138',
  Club == "Ipswich" ~ '#de2c37',
  Club == "Forest" ~ '#1f2c65',
  Club == "Bournemouth" ~ '#177b92',
  Club == "West Ham" ~ '#87201F',
  Club == "Everton" ~ '#1554AB',
  Club == "Wolves" ~ '#F69001',
  Club == "Leicester" ~ '#0053a0'))

# Create bump chart
p1 = ggplot(data, aes(x = Matchday, y = desc(Position), group = Club)) +
  geom_bump(linewidth = 4, smooth = 20, color = "gray60", alpha = 0.15) +
  geom_bump(linewidth = 2, smooth = 20, aes(color = colr2)) +
  geom_point(data = data |> filter(Matchday == min(Matchday)), 
             aes(color = colr2), size = 2.5) +
  geom_point(data = data |> filter(Matchday == max(Matchday)), 
             aes(color = colr2), size = 2.5, shape = 16) +
  geom_text(data = data |> filter(Matchday == max(Matchday)), 
            aes(x = Matchday + 0.5, label = Club), 
            hjust = 0, fontface='bold', size = 2.25, family ='Tw Cen MT') +
  scale_color_identity() +
  scale_x_continuous(breaks = c(seq(0, 30, by = 10), 38)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family= "Tw Cen MT", color='#37003C'),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

# Load and prepare league table data
mw_38 = read.csv('mw_38.csv', header = T)

# Set table column spacing
spacing = seq(from=3, to=8, length.out=8) + 0.2
spacing[4] = spacing[4] + 0.125

# Create table headers
headers = data.frame(
  text = c("Rank/Club", "Played","W","D","L","GF","GA","GD","Pts"),
  xpos = c(0.7, spacing)
)

# Reshape table data
df_pl <- mw_38 |>
  pivot_longer(cols=c(Played, W,D,L,GF,GA,GD,Pts)) |>
  mutate(xpos = case_when(
    name=="Played" ~ spacing[1],
    name=="W" ~ spacing[2], 
    name=="D" ~ spacing[3],
    name=="L" ~ spacing[4], 
    name=="GF" ~ spacing[5], 
    name=="GA" ~ spacing[6],
    name=="GD" ~ spacing[7],
    name== "Pts" ~ spacing[8]),
    colr2 = data$colr2[match(Club, data$Club)]
  )

# Create league table visualization
p2 = ggplot(data) +
  geom_text(data=headers, aes(x=xpos, y=0.2, label=text), size=2.8, family="Tw Cen MT") +
  geom_text(df_pl, aes(x=1.5, y=Position, label = Club), hjust=0, size=2.2, family="Tw Cen MT") +
  geom_text(aes(x=0.25, y=Position, label=Position), hjust=0, size=2.2, family="Tw Cen MT") +
  geom_segment(aes(x=0, xend=9, y=Position-0.5, yend=Position-0.5), color="#DADADA") +
  geom_text(data=df_pl, aes(x=xpos, y=Position, label=value), size=2.2, family="Tw Cen MT") +
  scale_y_reverse(limits=c(20.5,0), expand=c(0,0)) +
  labs(title = 'Standings after the final matchday') + 
  theme_minimal() +
  theme(
    axis.text=element_blank(),
    panel.grid = element_blank(),
    text = element_text(family= "Tw Cen MT", color='#37003C')) +
  coord_equal()

# Combine visualizations
combined_plot <- cowplot::plot_grid(
  p1, p2,
  ncol = 2,
  rel_widths = c(3, 0.9),
  align = "hv",
  axis = "tblr")

# Add titles and save
final_plot <- ggdraw() +
  draw_plot(combined_plot, scale = 0.85) +
  draw_label("Premier League 2024/25 Season Review", 
             x = 0.5, y = 0.95, size = 20, 
             fontfamily = "Tw Cen MT", fontface = "bold", color = "#37003C") +
  draw_label("Tracking team positions throughout the season and final league standings", 
             x = 0.5, y = 0.91, size = 11, 
             fontfamily = "Tw Cen MT", color = "#37003C") +
  draw_label("Data : Transfermarkt", 
             x = 0.5, y = 0.02, size = 8, 
             fontfamily = "Tw Cen MT", color = "#37003C")

ggsave("2025_pl38.png", plot = final_plot, 
       width = 18, height = 9, dpi = 300, bg = "white")
