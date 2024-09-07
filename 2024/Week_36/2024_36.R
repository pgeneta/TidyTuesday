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
  width  = 7,
  height = 5,
  units  = "in",
  dpi    = 320)

### 1.2 RESOLUTION ----
# showtext_opts(dpi = 320, regular.wt = 300, bold.wt = 800)


## 2. READ IN THE DATA =========================================================
tt <- tidytuesdayR::tt_load(
  x = base::as.double("2024"),
  week = base::as.double("36")
)

crosswalk <- tt$qname_levels_single_response_crosswalk |>
  clean_names() |>
  glimpse()

survey_questions <- tt$stackoverflow_survey_questions |>
  clean_names() |>
  glimpse()

single_response <- tt$stackoverflow_survey_single_response |>
  clean_names() |>
  glimpse()

tidytuesdayR::readme(tt)
rm(tt)


## 3. EXAMINING THE DATA =======================================================
skim(var)
glimpse(var)
colnames(var) |> sort()


## 4. TIDYDATA ================================================================

response_label <- single_response |> 
  mutate(across(everything(), as.character)) |> 
  pivot_longer(-response_id, names_to = "qname", values_to = "level") |> 
  left_join(crosswalk |> mutate(across(everything(), as.character)),
            by = c("qname", "level"))

qname_list <- c("years_code", "years_code_pro", "country", "currency", 
                "comp_total", "converted_comp_yearly", "r_used", 
                "r_want_to_use")

response_label2 <- response_label |> 
  mutate(value = ifelse(qname %in% qname_list, level, label)) |> 
  select(!c(level, label))
  
plot_data <- response_label2 |> 
  filter(qname %in% c("age", "ai_select")) |>
  pivot_wider(names_from = qname, values_from = value) |> 
  count(age, ai_select) |> 
  mutate(age = fct_relevel(age, "Under 18 years old"))


# 5. VISUALIZATION ============================================================

### 5.1 PLOT AESTHETICS ----
bkg_colour <- "#f0f0f0"
title_colour <- "grey10"
subtitle_colour <- "grey10"
caption_colour <- "grey10"
text_colour <- "grey10"

col_palette  <- c("NA" = "#BBCCEE",
                  "Yes" = "#CCEEFF",
                  "No, but I plan to soon" = "#CCDDAA",
                  "No, and I don't plan to" = "#FFCCCC"
                  )


### 5.2 TITLES AND CAPTION ----
tt <- str_glue("#TidyTuesday: 2024 Week 36 <br>")
linkedin <- str_glue("<span style='font-family:fa6-brands'>&#xf08c;</span> paul-geneta")
github <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span> pgeneta")

title_text <- str_glue("Developers vs AI use")
subtitle_text <- str_glue("Do developers currently use AI in their development process?")
caption_text <- str_glue("{tt} {linkedin} &bull; {github}")
source_text <- str_glue("Source: Stack Overflow Annual Developer Survey")

### 5.3 FONTS ----
font_add("fa6-brands", "fonts/6.4.2/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Roboto", family = "title")
font_add_google("Roboto", family = "subtitle")
font_add_google("Roboto", family = "text")
font_add_google("Roboto", family = "caption")
showtext_auto(enable = TRUE)

### 5.6 FINAL PLOT =============================================================

  plot_data |> 
  ggplot(aes(
    x = n,
    y = ai_select
  )) +

# GEOMS
  # geom_col(aes(fill = ai_select), show.legend = FALSE)+
  geom_bar(stat = 'identity', aes(fill = ai_select), show.legend = FALSE)+
# SCALES
  scale_x_continuous(label = scales::comma)+
  scale_fill_manual(values = col_palette)+
# LABELS
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = c(caption_text, source_text),
    x = "Number of Respondents",
    y = ""
  )+
# FACETS + MISC
  facet_wrap(~age, scales = "free_x")+
  # THEME
  theme_minimal(base_size = 12, base_family = "text")+
  theme(
  # Format titles and axis titles----------------
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_markdown(
    size               = rel(3),
    family             = "title",
    color              = title_colour,
    face               = "bold",
    lineheight         = 0.85,
    hjust              = 0.5,
    margin             = margin(t = 5, b = 5)
  ),
  plot.subtitle = element_markdown(
    size               = rel(2.5),
    family             = "subtitle",
    color              = title_colour,
    lineheight         = 0.8,
    margin             = margin(t = 0, b = 10),
    hjust              = 0.5
  ),
  plot.caption = element_markdown(
    size               = rel(1.5),
    family             = "caption",
    color              = caption_colour,
    lineheight         = 0.6,
    hjust              = c(0,-3.2),
    halign             = c(0,0),
    margin             = margin(t = 10, b = 5)
  ),
  axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = rel(2), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.title.y = element_text(margin = margin(0, 0, 0, 0), size = rel(2), color = text_colour, family = "text", face = "bold", hjust = 0.5),
  axis.text = element_text(size = rel(1.7), color = text_colour, family = "text"),
  axis.line.x = element_line(color = "#d7d7d8", linewidth = .2),
  
  # Format background colours------------
  panel.background = element_rect(
    fill = bkg_colour, 
    color = NA
    ),
  plot.background = element_rect(
    fill = bkg_colour, 
    color = NA
    ),
  # panel.border          = element_rect(fill = bkg_colour, color=bkg_colour),
  strip.background      = element_rect(fill = bkg_colour, color = bkg_colour),
  
  # Format the grid------------------------
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.1, color = "gray40"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  # axis.ticks            = element_blank(),
  
  # Format legend----------------------
  # legend.position = "plot",
  # legend.background = element_rect(fill = bkg_colour, color = bkg_colour),
  # # legend.text = element_text(size = 15, face = "bold", color=text_colour),
  # # legend.justification = "center",
  # legend.title = element_text(
  #   family = "serif",
  #   color = text_colour,
  #   size = 20, face = "bold"
  # ),
  
  
  # If using facet wrap/grid - theme for that
  strip.text            = element_textbox(size     = rel(1.5),
                                          color    = text_colour,
                                          face = 'bold',
                                          family   = 'text',
                                          hjust    = 0.5,
                                          halign   = 0.5,
                                          r        = unit(5, "pt"),
                                          width    = unit(5.5, "npc"),
                                          padding  = margin(0, 0, 0, 0),
                                          margin   = margin(0, 20, 0, -1),
                                          fill     = "transparent"),
)
