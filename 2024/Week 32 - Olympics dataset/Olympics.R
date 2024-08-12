library(tidyverse)
library(plotly)
library(tidytuesdayR)

tuesdata <- tt_load(2024, week = 32)
oly <- tuesdata$olympics
theme_set(theme_minimal())


df <- oly %>% 
  select(id, name, noc, year, season, city) %>% 
  group_by(id, name) %>% 
  unique()


vis <- function(data, seas = c("Summer", "Winter")){
  top_noc <- data %>% 
    filter(season == seas) %>% 
    group_by(noc) %>% 
    summarise(total_count = n()) %>% 
    ungroup() %>% 
    top_n(5) %>% 
    pull(noc)
  
  df <- data %>% 
    filter(noc %in% top_noc) %>% 
    group_by(year, noc) %>% 
    summarise(count = n(), .groups = 'drop') %>% 
    ungroup() %>% 
    group_by(noc) %>% 
    ungroup() %>% 
    arrange(year, noc)
  
  
  fig <- plot_ly(data = df, 
                 x =~year, 
                 y = ~count, 
                 type = 'scatter', 
                 color = ~noc, 
                 mode = 'line', 
                 stackgroup = 'one',
                 hoverinfo = 'text',
                 hovertext = paste0("Country: ", df$noc,
                                    "<br>Count: ", df$count) ) %>% 
    layout(title = paste0("Top 5 country participation - ", seas, " Olympics"),
           yaxis = list(title = "Total Athletes"),
           xaxis = list(title = "Year"),
           legend = list(title = list(text = '<b>Country</b>')))
  
  return(fig)
  
}

vis(oly, 'Winter')
vis(oly, 'Summer')
