## 1. LOAD PACKAGES & SETUP ===================================================
pacman::p_load(
  tidyverse, plotly, ggplot2, showtext, janitor, skimr, scales, lubridate,
  ggtext, MetBrewer
)

### 1.1 FIGURE SIZE ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 6.7,
  height = 4,
  units  = "in",
  dpi    = 320)


## 2. READ IN THE DATA =========================================================
tt <- tidytuesdayR::tt_load(
  x = base::as.double("2024"),
  week = base::as.double("34")
)

monarchs <- tt$english_monarchs_marriages_df |> clean_names()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA =======================================================
skim(monarchs)
glimpse(monarchs)
colnames(monarchs) |> sort()


## 4. TIDYDATA ================================================================

missing_data <- monarchs |>
  get_dupes(king_age:year_of_marriage) |>
  pull(king_name)

clean_missing_data <- monarchs |>
  filter(
    !is.na(king_age),
    !king_name %in% missing_data,
    king_age != "?",
    consort_age != "?"
  )

clean_age <- clean_missing_data |>
  mutate(
    king_age = ifelse(king_age == "50(?)", 50, king_age),
    king_age = as.numeric(king_age),
    consort_age = as.numeric(consort_age)
  )

plot_data <- clean_age |>
  mutate(
    pair = paste0(king_name, " / ", consort_name),
    age_diff = king_age - consort_age
  ) |>
  select(-year_of_marriage, -king_name, -consort_name) |> 
  filter(abs(age_diff) >= 10)



# 5. VISUALIZATION ============================================================

### 5.1 PLOT AESTHETICS ----
bkg_colour <- "#fffff2"
title_colour <- "#800020"
subtitle_colour <- "gray40"
caption_colour <- "gray40"
text_colour <- "gray20"

### 5.2 TITLES AND CAPTION ----
title_text <- str_glue("Age differences between Kings and Consorts (≥ 10 years)")
subtitle_text <- str_glue(
  "insert subtitle.<br>",
  "insert subtitle.<br>"
)
caption_text <- str_glue("#TidyTuesday: { 2024 } Week { 34 } &bull; Source: List of Monarchs by marriage")

### 5.3 FONTS ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Roboto", family = "title")
font_add_google("Roboto", family = "subtitle")
font_add_google("Roboto", family = "text")
font_add_google("Roboto", family = "caption")
showtext_auto(enable = TRUE)

### 5.4 PLOT THEME ----
theme_set(theme_minimal(base_size = 16, base_family = "text"))

theme_update(
  # Format titles and axis titles----------------
  plot.title.position = "plot",
  plot.title = element_markdown(
    size               = rel(2.7),
    family             = "title",
    color              = title_colour,
    face               = "bold",
    lineheight         = 0.85,
    hjust              = 0.5,
    margin             = margin(t = 3, b = 5)
  ),
  plot.caption         = element_markdown(
    size               = rel(1.2),
    family             = 'caption',
    color              = caption_colour,
    lineheight         = 0,
    hjust              = -1.1,
    halign             = 0.5
    ),
  axis.title.x = element_text(size = rel(2), color = text_colour, family = "text", face = "bold", hjust = 0.3),
  axis.text = element_text(size = rel(1.5), color = text_colour, family = "text"),
  # # Format background colours------------
  panel.background = element_rect(fill = bkg_colour, color = bkg_colour),
  plot.background = element_rect(fill = bkg_colour, color = bkg_colour),
  strip.background      = element_rect(fill = bkg_colour, color = bkg_colour),
  # # Format the grid------------------------
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.1, color = "gray50"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
)

### 5.6 FINAL PLOT =============================================================

plot_data |>
  ggplot(aes(y = pair)) +
  # GEOMS
  geom_point(aes(x = age_diff), size = 2, color = 'orange') +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_segment(aes(x = 0, xend = age_diff), color = 'orange') +
  # LABELS
  labs(
    title = title_text,
    x = "Age Difference",
    y = "",
    caption = caption_text
  )

  



