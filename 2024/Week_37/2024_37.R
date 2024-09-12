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

### 1.2 RESOLUTION ----
# showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA =========================================================
tt <- tidytuesdayR::tt_load(
  x = base::as.double("2024"),
  week = base::as.double("37")
)

admissions <- tt$college_admissions |>
  clean_names() |>
  glimpse()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA =======================================================
skim(admissions)
glimpse(admissions)
colnames(college_admissions_clean) |> sort()


## 4. TIDYDATA ================================================================

college_admissions_clean <- admissions  |> 
  filter(!is.na(attend)) |> 
  mutate(
    tier = str_to_title(tier),
    tier = case_when(
      tier == "Highly Selective Private" ~ "Highly Selective Private",
      tier == "Highly Selective Public"  ~ "Highly Selective Public",
      tier == "Other Elite Schools (Public And Private)" ~ "Other Elite Schools (Public And Private)",
      TRUE ~ tier
    ),
    tier = factor(tier, 
                  levels = c("Ivy Plus", 
                             "Highly Selective Private", 
                             "Highly Selective Public", 
                             "Other Elite Schools (Public And Private)", 
                             "Selective Private", 
                             "Selective Public"))
  ) 

plot_data <- college_admissions_clean |> 
  group_by(par_income_bin, tier) |> 
  summarise(avg_apply = mean(rel_apply)) |> 
  ungroup()


# 5. VISUALIZATION ============================================================

### 5.1 PLOT AESTHETICS ----
bkg_colour <- "#effffa"
title_colour <- "#F75590"
caption_colour <- "#383B53"
text_colour <- "#F75590"

# col_palette  <- c("" = "", "" = "")


### 5.2 TITLES AND CAPTION ----
tt <- str_glue("#TidyTuesday: { 2024 } Week { 37 }")
linkedin <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span> paul-geneta")
github <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span> pgeneta")

title_text <- str_glue("How incomes shape school applications")
caption_text <- str_glue("{tt} &bull; {linkedin} &bull; {github}")
source_text <- str_glue("Source: Opportunity Insights")

### 5.3 FONTS ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Roboto", family = "title")
font_add_google("Roboto", family = "subtitle")
font_add_google("Roboto", family = "text")
font_add_google("Roboto", family = "caption")
showtext_auto(enable = TRUE)

### 5.6 FINAL PLOT =============================================================

plot_data |>
  ggplot(aes(x = par_income_bin, y = avg_apply, colour = tier)) +

# GEOMS
  geom_line()+
  geom_point(size = 2) +
# SCALES
  scale_color_manual(values = met.brewer("Navajo", n = 6))+
# LABELS
  labs(
    title = title_text,
    y = "Application Rate",
    x = "Parents Income Group",
    caption = c(caption_text, source_text)
  ) +
# FACETS (if applicable)
  facet_wrap(~tier, scales = "free")+
# THEME
  theme_minimal(base_size = 12, base_family = "text")+
  theme(
  # Format titles and axis titles----------------
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_markdown(
    size               = rel(7),
    family             = "title",
    color              = title_colour,
    face               = "bold",
    lineheight         = 0.85,
    hjust              = 0.5,
    margin             = margin(t = 5, b = 10)
  ),
  plot.caption = element_markdown(
    size               = rel(3),
    family             = "caption",
    color              = caption_colour,
    lineheight         = 0.6,
    hjust              = c(0.01, -6),
    halign             = c(0,0),
    margin             = margin(t = 10, b = 0)
  ),
  plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
  axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = rel(5), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.title.y = element_text(margin = margin(0, 10, 0, 0), size = rel(5), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.text = element_text(size = rel(4), color = text_colour, family = "text"),
  axis.line.x = element_line(color = "#d7d7d8", linewidth = .2),
  
  # Format background colours------------
  panel.background = element_rect(fill = bkg_colour,color = NA),
  plot.background = element_rect(fill = bkg_colour, color = NA),
  panel.border          = element_rect(color = 'black', fill = NA),
  
  # Format the grid------------------------
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.1, color = "gray10"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_line(linetype = "dotted", linewidth = 0.1, color = "gray10"),
  
  # Format legend----------------------
  legend.position = "plot",
  strip.text = element_textbox(face = "bold",
                               color = text_colour,
                               family = 'text',
                               halign = 0.5,
                               size = rel(4.3)),
  panel.spacing = unit(1, 'lines')
)
