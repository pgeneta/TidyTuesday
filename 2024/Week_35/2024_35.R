## 1. LOAD PACKAGES & SETUP ===================================================
pacman::p_load(
  tidyverse, plotly, ggplot2, showtext, janitor, skimr, scales, lubridate,
  ggtext, MetBrewer
)

### 1.1 FIGURE SIZE ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 5,
  height = 5,
  units  = "in",
  dpi    = 320)

## 2. READ IN THE DATA =========================================================
tt <- tidytuesdayR::tt_load(
  x = base::as.double("2024"),
  week = base::as.double("35")
)

episodes <- tt$power_rangers_episodes |>
  clean_names() |>
  glimpse()

seasons <- tt$power_rangers_seasons |>
  clean_names() |>
  glimpse()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA =======================================================
skim(episodes)
glimpse(episodes)
colnames(episodes) |> sort()


## 4. TIDYDATA ================================================================

seasons <- seasons |> 
  mutate(air_date_last_ep = recode(air_date_last_ep, "2022" = "2022-09-29"))

seasons$air_date_first_ep <- ymd(seasons$air_date_first_ep)
seasons$air_date_last_ep <- ymd(seasons$air_date_last_ep)

full <- episodes |> 
  left_join(seasons,
            by = "season_title") |> 
  select(-desc, -imdb_rating.y, -producer)



# 5. VISUALIZATION ============================================================

### 5.1 PLOT AESTHETICS ----
bkg_colour <- "#D0DDD7"
title_colour <- "#856084"
subtitle_colour <- "#856084"
caption_colour <- "#856084"
text_colour <- "#0C1618"

# col_palette  <- c("" = "", "" = "")


### 5.2 TITLES AND CAPTION ----
tt <- str_glue("#TidyTuesday: { 2024 } Week { 35 } <br>")
linkedin <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span> paul-geneta")
github <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span> pgeneta")

title_text <- str_glue("Power Rangers IMDB Ratings")
subtitle_text <- str_glue("Season 1 — Season 28")
caption_text <- str_glue("{tt} {linkedin} &bull; {github}")
source_text <- str_glue("Source: Kaggle: Power Rangers Season & Episodes")

### 5.3 FONTS ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Roboto", family = "title")
font_add_google("Roboto", family = "subtitle")
font_add_google("Roboto", family = "text")
font_add_google("Roboto", family = "caption")
showtext_auto(enable = TRUE)

### 5.4 PLOT THEME ----
theme_set(theme_minimal(base_size = 15, base_family = "text"))

theme_update(
  # Format titles and axis titles----------------
  plot.title = element_markdown(
    size               = rel(2.5),
    family             = "title",
    color              = title_colour,
    face               = "bold",
    lineheight         = 0.85,
    hjust              = 0.5,
    margin             = margin(t = 5, b = 5)
  ),
  plot.subtitle = element_markdown(
    size               = rel(1.5),
    family             = "subtitle",
    color              = title_colour,
    lineheight         = 0.8,
    hjust              = 0.5,
    margin             = margin(t = 0, b = 10)
  ),
  plot.caption = element_markdown(
    size               = rel(1.1),
    family             = "caption",
    color              = caption_colour,
    lineheight         = 0.3,
    hjust              = c(-.1,-2.1),
    halign             = c(0,0),
    margin             = margin(t = 5, b = 0)
  ),
  axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = rel(1.5), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.title.y = element_text(margin = margin(0, 10, 0, 0), size = rel(1.5), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.text = element_text(size = rel(1.2), color = text_colour, family = "text"),
  axis.line.x = element_line(color = "#d7d7d8", linewidth = .2),

  # Format background colours------------
  panel.background = element_rect(fill = bkg_colour, color = bkg_colour),
  plot.background = element_rect(fill = bkg_colour, color = bkg_colour),

  # Format the grid------------------------
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.1, color = "gray40"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),

  # Format legend----------------------
  legend.position = "plot",
  legend.background = element_rect(fill = bkg_colour, color = bkg_colour),
  legend.title = element_text(
    family = "serif",
    color = text_colour,
    size = 20, face = "bold"
  ),
)

### 5.6 FINAL PLOT =============================================================

full |>
  ggplot(aes(x = year(air_date), y = imdb_rating.x)) +
  

# GEOMS
  geom_boxplot(aes(group = factor(year(air_date))),
               color = "#856084") +
  geom_smooth(method = "loess",
              formula = "y~x",
              se = FALSE,
              color = '#755C1B',
              linewidth = 0.7)+
  geom_vline(xintercept = 2002, linetype = "dashed", color = "gray40")+
  geom_vline(xintercept = 2011, linetype = "dashed", color = "gray40")+
  annotate("text", 
           x = 1997, 
           y = 10, 
           label = "Producer: Saban",
           color = "#755C1B", 
           family = 'sans',
           size = 8)+
  annotate("text", 
           x = 2006.5, 
           y = 10, 
           label = "Producer: Disney",
           color = "#755C1B", 
           family = 'sans',
           size = 8)+
  annotate("text", 
           x = 2016, 
           y = 10, 
           label = "Producer: Saban Brands",
           color = "#755C1B", 
           family = 'sans',
           size = 8)+
scale_y_continuous(limits = c(5,10.1),
                   breaks = c(5:10))+
# LABELS
labs(x = "Year released",
     y = "IMDB rating",
     title = title_text,
     subtitle = subtitle_text,
     caption = c(caption_text, source_text)
)     
