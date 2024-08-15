library(tidyverse)
library(tidytuesdayR)
library(plotly)

tuesdata <- tt_load(2024, week = 32)
oly <- tuesdata$olympics


df <- oly %>% 
  select(id, name, noc, year, season, city) %>% 
  group_by(id, name) %>% 
  unique()


vis <- function(data, seas = c("Summer", "Winter")){
  top_noc <- data %>% 
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
    mutate(label = ifelse(row_number() == n(), paste0(noc, " - ", count), NA)) %>% 
    ungroup() %>% 
    arrange(year, noc) %>% 
    ggplot(aes(x = year, y = count, color = noc, label = label))+
    geom_line(alpha = 0.6, linewidth = 1) +
    geom_text(nudge_x = 3, size = 4)+
    labs(title = paste0("Top 5 Country Participation - ", seas, " Olympics"))+
    ylab("Number of athletes")+
    theme_bw()+
    #scale_fill_manual(values = met.brewer('Degas'))+
    scale_colour_manual(values = met.brewer('Degas'))+
    theme(legend.position = 'none',
          legend.title = element_blank())
  
  return(df)
}

vis(oly, 'Winter')


#=================
top_noc <- oly %>% 
  filter(season == "Winter") %>% 
  group_by(noc) %>% 
  summarise(total_count = n()) %>% 
  ungroup() %>% 
  top_n(5) %>% 
  pull(noc)

df <- oly %>% 
  filter(noc %in% top_noc) %>% 
  group_by(year, noc) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(noc) %>% 
  #mutate(label = ifelse(row_number() == n(), paste0(noc, " - ", count), NA)) %>% 
  ungroup() %>% 
  arrange(year, noc)

plot <- ggplot(df, aes(x = year, y = count, fill = noc) )+
  geom_area()+
  #geom_line(alpha = 0.6, linewidth = 1) +
  #geom_text(nudge_x = 3, size = 4)+
  labs(title = paste0("Top 5 Country Participation - ", seas, " Olympics"))+
  ylab("Total athletes")+
  theme_bw()+
  theme(legend.position = 'bottom',
        legend.title = element_blank())



fig <- plot_ly(df, x =~year, 
               y = ~count, 
               type = 'scatter', 
               color = ~noc, 
               mode = 'line', 
               stackgroup = 'one',
               hoverinfo = 'text',
               hovertext = paste0("Country: ", df$noc,
                                  "<br>Count: ", df$count) ) %>% 
  layout(title = paste0("Top 5 country participation, Winter Olympics"),
         yaxis = list(title = "Total Athletes"),
         xaxis = list(title = "Year"),
         legend = list(title = list(text = '<b>Country</b>')))
fig

