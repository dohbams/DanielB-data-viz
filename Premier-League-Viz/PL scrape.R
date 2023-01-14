# load libraries
library(rvest)
library(tidyverse)

pages = seq(1,19,1) # I used 19 because we are at matchday 19

for (i in pages) {
  url = paste0('https://www.transfermarkt.com/premier-league/spieltagtabelle/wettbewerb/GB1?saison_id=2022&spieltag=',i)
  
  pl = read_html(url)

  tab = html_table(pl)

  table = tab[[3]] 
  # remove unwanted columns
  table = table[,-2] 
  # rename columns
  names(table)[1] = 'Position'
  names(table)[3] = 'Played'
  names(table)[8] = 'GD'
  table = table |>  mutate(Matchday = i)
  table = table |> separate(Goals, c('GF','GA'), sep = ':')
  table = table |> mutate(GF = as.numeric(GF), GA = as.numeric(GA))
  #save each table to a new variable
  assign(paste0('pl_mw', i), table)
  
}

# combine all tables 
pl_all = bind_rows(pl_mw1,
                   pl_mw2,
                   pl_mw3,
                   pl_mw4,
                   pl_mw5,
                   pl_mw6,
                   pl_mw7,
                   pl_mw8,
                   pl_mw9,
                   pl_mw10,
                   pl_mw11,
                   pl_mw12,
                   pl_mw13,
                   pl_mw14,
                   pl_mw15,
                   pl_mw16,
                   pl_mw17,
                   pl_mw18,
                   pl_mw19)

# save the combined table as csv
write.csv(pl_all, 'pl_all.csv')

# save the latest table as csv
write.csv(pl_mw19, 'mw_19.csv')
