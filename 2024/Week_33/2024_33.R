
## 1. LOAD PACKAGES & SETUP ----  
pacman::p_load(
  tidyverse, plotly, ggplot2, showtext, janitor, skimr, scales, lubridate, ggtext, MetBrewer,
  ggridges
)

### 1.1 SET FIG SIZE ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 16,
  height = 9,
  units  = "in",
  dpi    = 320)

### 1.2 RESOLUTION ----
showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA ----
tt <- tidytuesdayR::tt_load(x = base::as.double("2024"),
              week = base::as.double("33")) 

fairs <- tt$worlds_fairs %>% clean_names() %>% glimpse()

tidytuesdayR::readme(tt) 
rm(tt)  


## 3. EXAMINING THE DATA ----
skim(fairs)
glimpse(fairs)
colnames(fairs) %>% sort()


## 4. TIDYDATA ----

df_filtered <- fairs %>% select(name_of_exposition, category, visitors) %>% 
  filter(!is.na(visitors),
         visitors >=10 ) %>% 
  arrange(desc(visitors))

# 5. VISUALIZATION ---- 

### 5.1 PLOT AESTHETICS ---- 
bkg_colour      <- colorspace::lighten("gray", .8) 
title_colour    <- "gray10"          
subtitle_colour <- "gray10"  
caption_colour  <- "gray30"  
text_colour     <- "gray20"   


### |-  5.2 TITLES AND CAPTIONS ----
title_text    <- str_glue("Number of Visitors across world fairs (>= 10M)") 
caption_text  <- str_glue("#TidyTuesday: { 2024 } Week { 33 } &bull; Source: List of world expositions (Wikipedia)")  


### |-  5.3 FONTS ----
font_add('fa6-brands', 'fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf') 
font_add_google("Roboto", family = "title")                            
font_add_google("Roboto", family = "subtitle")   
font_add_google("Roboto", family = "text")  
font_add_google("Roboto", family = "caption")
showtext_auto(enable = TRUE)  

### |-  5.4 PLOT THEME ----
theme_set(theme_minimal(base_size = 12, base_family = "text"))                

theme_update(
  #Format titles and axis titles
  plot.title.position   = "plot",
  plot.caption.position = "panel",
  plot.title           = element_markdown(
    size               = rel(2), 
    family             = 'title',
    color              = title_colour,
    face               = "bold",
    lineheight         = 0.85,
    hjust = 0.5,
    margin             = margin(t = 5, b = 10)),
  
  plot.subtitle        = element_markdown(
    size               = rel(.8), 
    family             = 'subtitle',
    color              = title_colour,
    lineheight         = 0.8, 
    margin             = margin(t = 5, b = 20)),
  
  plot.caption         = element_markdown(
    size               = rel(1),
    family             = 'caption',
    color              = caption_colour,
    lineheight         = 0.1,
    hjust              = -1.5,
    halign             = 0.5,
    margin             = margin(t = 1, b = 1)),
  axis.title.x          = element_text(margin = margin(3, 0, 0, 0), size = rel(1), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.text             = element_text(size = rel(0.8), color = text_colour, family = "text"),
  
  #Format background colours
  panel.background      = element_rect(fill = bkg_colour, color = bkg_colour),
  plot.background       = element_rect(fill = bkg_colour, color = bkg_colour),
  
  #Format the grid
  panel.grid.minor.x    = element_blank(),
  panel.grid.major.x    = element_line(linetype = "dotted", linewidth = 0.1, color = 'gray40'),
  panel.grid.minor.y    = element_blank(),
  panel.grid.major.y    = element_blank(),
  
  #Format legend
  legend.position       = 'top',
  legend.background = element_rect(fill=bkg_colour, color=bkg_colour),
  legend.title = element_text(family = "serif",
                              color = text_colour,
                              size = 20, face = "bold", hjust = 0.5),


)

### 5.5 FINAL PLOT ----  

df_filtered %>% 
  ggplot(aes(x = reorder(name_of_exposition, visitors), y = visitors, fill = as.factor(category)))+
  # Geoms
  geom_bar(stat = 'identity', width = 0.7)+
  coord_flip()+
  # Scales
  scale_fill_manual(values = met.brewer("Navajo"))+
  scale_y_continuous(breaks = seq(0,80,by=10), limits = c(0,80))+
  # Labels
  labs(title = title_text,
       x = "",
       y = "Number of Visitors (millions)",
       fill = element_blank(), 
       caption = caption_text)
