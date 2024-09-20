## 1. LOAD PACKAGES & SETUP ===================================================
pacman::p_load(
  tidyverse, 
  plotly, 
  ggplot2, 
  showtext, 
  janitor, 
  skimr, 
  scales, 
  lubridate,
  ggtext, 
  MetBrewer
)

### 1.1 FIGURE SIZE ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 14,
  height = 10,
  units  = "in",
  dpi    = 320)


## 2. READ IN THE DATA =========================================================
tt <- tidytuesdayR::tt_load(
  x = base::as.double("2024"),
  week = base::as.double("38")
)

macbeth <- tt$macbeth |>
  clean_names() |>
  glimpse()

hamlet <- tt$hamlet |> 
  clean_names() |> 
  glimpse()

romeo_juliet <- tt$romeo_juliet |> 
  clean_names() |> 
  glimpse()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA =======================================================
skim(var)
glimpse(var)
colnames(var) |> sort()


## 4. TIDYDATA ================================================================

macbeth_top <- macbeth |> 
  filter(character != "[stage direction]") |> 
  group_by(character) |> 
  count() |> 
  ungroup() |> 
  top_n(5,n) |> 
  arrange(n) |> 
  mutate(book = "Macbeth")

hamlet_top <- hamlet |> 
  filter(character != "[stage direction]") |> 
  group_by(character) |> 
  count() |> 
  ungroup() |> 
  top_n(5,n) |> 
  arrange(n) |> 
  mutate(book = "Hamlet")

romeo_juliet_top <- romeo_juliet |> 
  filter(character != "[stage direction]") |> 
  group_by(character) |> 
  count() |> 
  ungroup() |> 
  top_n(5,n) |> 
  arrange(n) |> 
  mutate(book = "Romeo and Juliet")

plot_data <- rbind(macbeth_top, hamlet_top, romeo_juliet_top)


# 5. VISUALIZATION ============================================================

### 5.1 PLOT AESTHETICS ----
bkg_colour <- "#F5F3F5"
title_colour <- "#1B264F"
subtitle_colour <- "#576CA8"
caption_colour <- "#576CA8"
text_colour <- "#576CA8"


### 5.2 TITLES AND CAPTION ----
tt <- str_glue("#TidyTuesday: { 2024 } Week { 38 } &bull; ")
linkedin <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span> paul-geneta")
github <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span> pgeneta")

title_text <- str_glue("Who spoke the most in Shakespeare's plays?")
subtitle_text <-str_glue("Counting dialogue in Hamlet, Macbeth, and Romeo and Juliet")
caption_text <- str_glue("{tt} {linkedin} &bull; {github}")
source_text <- str_glue("Source: Shakespeare.mit.edu")

### 5.3 FONTS ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Roboto", family = "title")
font_add_google("Roboto", family = "subtitle")
font_add_google("Roboto", family = "text")
font_add_google("Roboto", family = "caption")
showtext_auto(enable = TRUE)

### 5.6 FINAL PLOT =============================================================

plot_data |>
  ggplot(aes(x = character, 
             y = n,
             fill = book)) +

# GEOMS
  geom_bar(stat = "identity", show.legend = FALSE)+
  coord_flip()+
  
# SCALES
  scale_fill_manual(values = met.brewer("Cassatt2"))+
  
# LABELS
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = c(caption_text,source_text),
    x = "",
    y = "Word Count"
  )+
  
# FACETS (if applicable)
  facet_wrap(~book, scales = "free", nrow = 3)+
  
# THEME
  theme_minimal(base_size = 12, base_family = "text")+
  theme(
  # Format titles and axis titles----------------
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_markdown(
    size               = rel(10),
    family             = "title",
    color              = title_colour,
    face               = "bold",
    lineheight         = 0.85,
    hjust              = 0.5,
    margin             = margin(t = 5, b = 10)
  ),
  plot.subtitle = element_markdown(
    size               = rel(6),
    family             = "subtitle",
    color              = title_colour,
    hjust = 0.5,
    lineheight         = 0.8,
    margin             = margin(t = 5, b = 20)
  ),
  plot.caption = element_markdown(
    size               = rel(4),
    family             = "caption",
    color              = caption_colour,
    lineheight         = 0.6,
    hjust              = c(0.03, -4.1),
    halign             = 0, #c(0,0),
    margin             = margin(t = 10, b = 5)
  ),
  axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = rel(7), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.text = element_text(size = rel(6), color = text_colour, family = "text"),
  axis.line.x = element_line(color = "#d7d7d8", linewidth = .2),
  
  # Format background colours------------
  panel.background = element_rect(fill = bkg_colour,color = NA),
  plot.background = element_rect(fill = bkg_colour, color = NA),
  
  # Format the grid------------------------
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.1, color = "gray40"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  
  # If using facet wrap/grid - theme for that
  strip.text            = element_textbox(size     = rel(6),
                                          color    = text_colour,
                                          family   = 'text',
                                          face = 'bold',
                                          hjust    = 0.5,
                                          halign   = 0.5,
                                          margin   = margin(10, 20, 0, -1),
                                          fill     = "transparent"),
)
