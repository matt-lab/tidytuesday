# 📦 Load packages-------------------------------------------------------------
library(tidyverse) # Data wrangling
library(showtext) # Fonts
library(ggtext) # Text annotation
library(viridis) # Plot colours
library(glue) # HTML

# ✍️ Load fonts-----------------------------------------------------------------
font_add_google("Quicksand", "quicksand")
font_add('fa-brands', '../../fonts/Font Awesome 5 Brands-Regular-400.otf')
font_add("fa-solid", "../../fonts/Font Awesome 5 Free-Solid-900.otf")
showtext_auto()

# 💾 Load data------------------------------------------------------------------
afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')

# 💪 Data wrangling------------------------------------------------------------
data <- afrisenti |>
    left_join(language_countries, by = 'language_iso_code') |>
    left_join(country_regions, by = 'country') |>
    # Manually clean Mozambique data to be consistent with article
    filter(!(region != "Southeastern Africa" & language_iso_code == "pt-MZ")) |>
    filter(!(region != "East Africa" & language_iso_code == "swa")) |>
    filter(!(region != "Southern Africa" & language_iso_code == "tso")) |>
    # Count languages by region
    group_by(region) |>
    count(language_iso_code) |>
    left_join(languages, by = 'language_iso_code') |>
    ungroup() |>
    # Organise data for polar coordinates
    # Cluster regions in clockwise order
    mutate(cluster = case_when(
        str_detect(region, "^North ")        ~ 0,
        str_detect(region, "^Northern ")     ~ 0.2,
        str_detect(region, "^East ")         ~ 1,
        str_detect(region, "^Southeastern ") ~ 1.7,
        str_detect(region, "^South")        ~ 2,
        str_detect(region, "^West ")         ~ 3)) |>
    arrange(cluster) |>
    group_by(cluster) |>
    mutate(cluster_language = n_distinct(language)) |>
    # Identify initial offset for North region
    mutate(north_midpoint = ifelse(cluster == 0, floor(n() / 2), NA)) |>
    ungroup() |>
    fill(north_midpoint, .direction = "down") |>
    # Largest cluster size determines whitespace in figure
    mutate(cluster_language_max = max(cluster_language)) |>
    mutate(cluster_size_max = cluster_language_max + 2) |>
    # Divide languages along polar coordinates via id
    group_by(cluster) |>
    arrange(region, n) |>
    mutate(id =
        # Cluster offset from 12 o'clock
        floor(cluster * cluster_size_max)
        # Counterclockwise offset to ensure regions correspond to compass
        # directions
        - floor(n() / 2)
        # Clockwise offset for each language
        + row_number()) |>
    # Rotate counterclockwise to center north regions
    mutate(id = ifelse(id <= 1, 4 * cluster_size_max + id, id)) |>
    ungroup() |>
    complete(id = 1:max(id), fill = list(n = 0)) |>
    # Add label information
    mutate(hjust = case_when(
        id %in% c(0, median(id))        ~ 0.5,
        id < median(id)                 ~ 0,
        id > median(id)                 ~ 1,
        TRUE                            ~ 0.5,
    )) |>
    mutate(vjust = case_when(
        cluster < 1                     ~ 0,
        cluster > 1 & cluster < 3       ~ 1,
        TRUE                            ~ 0.5,
    )) |>
    # Region labels, exclude Nothern and Southeastern Africa for clarity
    mutate(label_region = str_replace_all(region, " ", "\n")) |>
    mutate(label_language = case_when(
        language == "Mozambican Portuguese"     ~ "Mozambican\n Portuguese",
        str_detect(language, "/")               ~ str_replace_all(language, "/", "/\n"),
        TRUE                                    ~ language
    ))
glimpse(data)

# 🖌️ Plot----------------------------------------------------------------------
# Set colours
colour_background <- "#F5F5DC"
colour_text <- "#030303"
colour_grid <- "#787878"
colour_compass <- colour_text
colour_socials <- colour_text

# Set fonts
text_font <- "quicksand"

# Set y limits
inner_size <- 10
y_min <- .1
y_max <- 10^ceiling(log10(max(data$n)))

# Set title
title <- "<b>Locating AfriSenti's Languages</b>"
# Set subtitle
subtitle <- glue(
    "The AfriSenti project captured thousands of tweets from {nrow(languages)} of Africa's languages.<br>
    Most languages were from the East and West Africa. The most tweeted language was <b>{slice_max(data, n, n = 1)$language}</b>,<br>
    used in <b>{formatC(slice_max(data, n, n = 1)$n, format = 'd', big.mark = ',')}</b> tweets."
    )
# Set caption
# Set text
source <- glue("github.com/afrisenti-semeval/afrisent-semeval-2023")
twitter <- glue("<span style='font-family:fa-brands; color:{colour_socials}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{colour_socials}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{colour_socials}'>&#xf0c7;</span>")
space <- glue("<span style='color:{colour_background}'>-</span>")
bar <- glue("<span style='color:{colour_socials}'> | </span>")
social_text <- glue("<span style ='color:{colour_socials}'>{floppy}{space}<b>Source:{space}{source}</b>{space}{bar}{twitter}{space}@MattAndreotta{space}{bar}{github}{space}matt-lab</span>")

# Let's do it!
data |>
    ggplot(aes(x = id, y = n, colour = region)) +
    # Add custom gridlines
    annotate(
        "segment",
        x = floor(0.65 * max(data$id)) + 0.7,
        xend = max(data$id) + .5,
        y = 10^seq(log10(inner_size) + 1, log10(y_max)),
        yend = 10^seq(log10(inner_size) + 1, log10(y_max)),
        size = 0.5,
        alpha = 0.7,
        colour = colour_grid,
    ) +
    annotate(
         "segment",
        x = 0.5,
        xend = floor(0.65 * max(data$id)) - 0.7,
        y = 10^seq(log10(inner_size) + 1, log10(y_max)),
        yend = 10^seq(log10(inner_size) + 1, log10(y_max)),
        size = 0.5,
        alpha = 0.7,
        colour = colour_grid,
    ) +
    # Specify lollipops
    geom_segment(
        aes(x = id, xend = id, y = 0, yend = n),
        size = 0.7
    ) +
    geom_point(
        aes(size = n)
    ) +
    scale_size(range = c(1, 3)) +
    # Cover the central point
    geom_rect(
        aes(xmin = 0.5, xmax = max(id) + 0.5, ymin = 0, ymax = inner_size),
        fill = colour_background, colour = colour_background
    ) +
    # Language labels
    geom_label(
        aes(
            x = id,
            y = n^1.07, # Log maths...
            label = label_language,
            hjust = hjust,
            vjust = vjust),
        colour = colour_text,
        fill = colour_background,
        lineheight = .4,
        fontface = "bold",
        family = text_font,
        label.size = NA,
        label.padding = unit(0.1, "lines"),
        alpha = 0.9,
        size = 12,
        inherit.aes = FALSE) +
    # Add compass information
    annotate(
        "text",
        x = seq(0.5, max(data$id) - 1, by = 7.25),
        y = inner_size^.6,
        label = c("N", "E", "S", "W"),
        colour = colour_compass,
        size = 12,
        alpha = 0.8,
        angle = 0,
        fontface = "bold",
        family = text_font,
        hjust = 0.5,
        vjust = 0.5,
    ) +
    geom_hline(yintercept = inner_size, colour = colour_compass) +
    geom_hline(yintercept = inner_size^.25, colour = colour_compass) +
    # Add text annotation for y axis values
    annotate(
        "text",
        x = rep(floor(0.65 * max(data$id)), log10(y_max) - log10(inner_size)),
        y = 10^seq(log10(inner_size) + 1, log10(y_max)),
        label = formatC(10^seq(log10(inner_size) + 1, log10(y_max)), format = "d", big.mark = ","),
        colour = colour_text,
        size = 12,
        alpha = 0.8,
        angle = 0,
        fontface = "bold",
        family = text_font,
        hjust = 0.5,
        vjust = 0.5) +
    # Will be a polar plot
    coord_polar(clip = "off") +
    # Log scale
    scale_y_log10(
        expand = c(0, 0),
        limits = c(y_min, y_max*10),
        breaks = 10^(log10(y_min):log10(y_max))) +
    # Add caption
    labs(
        title = title,
        subtitle = subtitle,
        caption = social_text,
    ) +
    # Add colour
    scale_colour_viridis(discrete = TRUE) +
    # Set theme
    theme_void() +
    theme(panel.background = element_rect(
        fill = colour_background,
        colour = colour_background)) +
    theme(plot.background  = element_rect(
        fill = colour_background,
        colour = colour_background)) +
    theme(panel.border       = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks         = element_blank()) +
    # Format the legend
    theme(legend.position = "none") +
    # Format the plot titles
    theme(plot.title    = element_markdown(
        size = 50,
        lineheight = 0.4,
        hjust = 0.5, vjust = 0.5,
        family = text_font,
        face = "bold",
        margin = margin(10, 5, 15, 5),
    )) +
    theme(plot.subtitle = element_markdown(
        lineheight = 0.4,
        hjust = 0.5, vjust = 0.5,
        family = text_font,
        size = 30,
        margin = margin(10, 5, 15, 5),
    )) +
    theme(plot.caption = element_markdown(
        size = 20,
        lineheight = 0.4,
        hjust = 0.5, vjust = 0.5,
        family = text_font,
        margin = unit(c(0, 0, 0, 0), "cm"))) +
    # Format the axis text
    theme(axis.title.x  = element_blank()) +
    theme(axis.title.y  = element_blank()) +
    theme(axis.text.x   = element_blank()) +
    theme(axis.text.y   = element_blank()) +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# 💾 Save plot-----------------------------------------------------------------
ggsave(
  filename = "plot.png", # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)
