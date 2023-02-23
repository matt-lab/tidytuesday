# Load libraries
library(tidyverse) # Data wrangling
library(showtext) # Fonts
library(ggtext) # Text annotation
library(ggforce) # Arc geom
library(glue) # HTML

# Load fonts
font_add_google("Quicksand", "quicksand")
font_add_google("Merienda", "merienda")
font_add('fa-brands', '../../fonts/Font Awesome 5 Brands-Regular-400.otf')
font_add("fa-solid", "../../fonts/Font Awesome 5 Free-Solid-900.otf")
showtext_auto()



# Load data
data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv")
glimpse(data)

# Create colour lookup table
# thanks to @nrennie's wonderful script:
# https://github.com/nrennie/tidytuesday/blob/main/2023/2023-02-21/20230221.R
colour_lookup <- data |>
    select(colors, color_hex) |>
    distinct() |>
    # Process colours
    mutate(colors = str_remove_all(colors, "\\[")) |>
    mutate(colors = str_remove_all(colors, "\\]")) |>
    mutate(colors = str_remove_all(colors, "'")) |>
    mutate(colors = str_remove_all(colors, "\\\\n")) |>
    mutate(colors = str_remove_all(colors, "\\\\r")) |>
    # Process hex
    mutate(color_hex = str_remove_all(color_hex, "\\[")) |>
    mutate(color_hex = str_remove_all(color_hex, "\\]")) |>
    mutate(color_hex = str_remove_all(color_hex, "'")) |>
    # Unlist
    mutate(colors = strsplit(colors, ",")) |>
    mutate(color_hex = strsplit(color_hex, ", ")) |>
    unnest(c(colors, color_hex)) |>
    mutate(colors = str_trim(colors)) |>
    distinct()

# Create correlogram data
# proportion: proportion of paintings that share two colours
plot_data <- data |>
    select(c(painting_index, Black_Gesso:Alizarin_Crimson)) |>
    pivot_longer(
        Black_Gesso:Alizarin_Crimson,
        names_to = "color_x",
        values_to = "present") |>
    left_join(data, by = "painting_index") |>
    group_by(color_x) |>
    summarise(
        across(
            Black_Gesso:Alizarin_Crimson,
            ~ sum(.x & present, na.rm = TRUE) / n())) |>
    ungroup() |>
    pivot_longer(
        Black_Gesso:Alizarin_Crimson,
        names_to = "color_y",
        values_to = "proportion") |>
    mutate(pair_id = 1:n()) |>
    # Each proportion is plotted with two geoms
    pivot_longer(
        starts_with('color_'),
        names_to = "axis",
        values_to = "colors",
        names_prefix = "color_"
    ) |>
    # Lookup colour hexes
    mutate(colors = str_replace_all(colors, "_", " ")) |>
    left_join(colour_lookup, by = "colors") |>
    # Determine start of arc geom
    mutate(is_x = axis == 'x') |>
    mutate(start = ifelse(is_x, pi / 4, -3 * pi / 4)) |>
    # Identify colour order for plot
    group_by(pair_id) |>
    mutate(level =
        ifelse(
            n_distinct(colors) == 1,
            proportion,
            NA)) |>
    group_by(colors, axis) |>
    fill(level, .direction = "downup") |>
    group_by(axis) |>
    mutate(level = dense_rank(desc(level))) |>
    ungroup() |>
    mutate(level = ifelse(is_x, -level + max(level) + 1, level)) |>
    pivot_wider(names_from = 'axis', values_from = 'level') |>
    # Identify coordinates for arc geom
    group_by(pair_id) |>
    fill(x, .direction = "downup") |>
    fill(y, .direction = "downup") |>
    ungroup() |>
    # Remove points not in triangle correlogram plot
    mutate(point_in_plot = (x + y - 1) <= max(x, na.rm = TRUE)) |>
    filter(proportion != 0 & point_in_plot) |>
    # Add color names
    mutate(colors_names = ifelse(
        is_x,
        str_replace_all(colors, " ", "\n"),
        colors))

# Create manual labels for integers
# As arc eom requires continuous integers
x_labels <- plot_data |>
    filter(is_x) |>
    group_by(x) |>
    count(colors_names) |>
    pull(colors_names)
y_labels <- plot_data |>
    filter(!is_x) |>
    group_by(y) |>
    count(colors_names) |>
    pull(colors_names)

# Set colors
colour_background <- "#F5F5DC"
colour_text <- "#030303"
colour_segment <- "#676767"
colour_socials <- colour_segment
# Set fonts
main_font <- "quicksand"
title_font <- "merienda"
# Set text
title_text <- "The Colour Pairings of Bob Ross"
description_text <- "Circle size reflects the number of works from The Joys of Painting that use any two colours." |>
    str_wrap(width = 35)
twitter <- glue("<span style='font-family:fa-brands; color:{colour_socials}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{colour_socials}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{colour_socials}'>&#xf0c7;</span>")
space <- glue("<span style='color:{colour_background}'>-</span>")
bar <- glue("<span style='color:{colour_socials}'> | </span>")
social_text <- glue("<span style ='color:{colour_socials}'>{floppy}{space}<b>Source: &#123;BobRossColors&#125; </b>{bar}{twitter}{space}@MattAndreotta{bar}{github}{space}matt-lab</span>")

# Custom geoms
geom_triangle <- function() {
    # Uses segment geoms to plot a triangle grid on ggplot plot (plot)
    # with points at in top left, bottom left, and bottom right
    triangle_data = {
        # Horizontal segments
        tibble(
            x1 = 0,
            y1 = seq(1, max(plot_data$y)),
            x2 = seq(max(plot_data$x), 1),
            y2 = y1) |>
        # Vertical segments
        add_case(
            x1 = seq(1, max(plot_data$x)),
            y1 = 0,
            x2 = x1,
            y2 = seq(max(plot_data$y), 1)) |>
        # Diagonal segment
        add_case(
            x1 = 1,
            y1 = max(plot_data$y),
            x2 = max(plot_data$x),
            y2 = 1)}
    geom_segment(
        data = triangle_data,
        mapping = aes(x = x1, y = y1, xend = x2, yend = y2),
        color = colour_segment)
}

plot_data |>
    ggplot() +
    # Create grid segments
    geom_triangle() +
    # Add arcs
    geom_arc_bar(
        aes(
            x0 = x, y0 = y,
            r0 = 0, r = proportion * .45,
            start = start, end = start + pi,
            fill = color_hex, color = color_hex)) +
    scale_fill_identity() +
    scale_color_identity() +
    # Add labels
    scale_x_continuous(
        breaks = 1:length(x_labels),
        labels = x_labels,
        expand = c(0, 0)
    ) +
    scale_y_continuous(
        breaks = 1:length(y_labels),
        labels = y_labels,
        expand = c(0, 0)
    ) +
    coord_cartesian(clip = "off") +
    # Add title
    annotate(
        "text",
        label = title_text,
        x = max(plot_data$x) - 7, y = max(plot_data$y) - 0.5,
        hjust = 0.5, vjust = 1,
        size = 19, fontface = "bold",
        family = title_font, colour = colour_text) +
    # Add caption
    labs(caption = social_text) +
    # Add description
    annotate(
        "text",
        label = description_text,
        x = max(plot_data$x) - 7, y = max(plot_data$y) - 1.5,
        hjust = 0.5, vjust = 1,
        size = 17,
        family = main_font, colour = colour_text,
        lineheight = .3) +
    # Begin formatting
    theme_bw(base_size = 15) +
    # Format the grid
    theme(panel.background = element_rect(
        fill = colour_background,
        color = colour_background)) +
    theme(plot.background  = element_rect(
        fill = colour_background,
        color = colour_background)) +
    theme(panel.border       = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks         = element_blank()) +
    # Format the legend
    theme(legend.position = "none") +
    # Format the plot titles
    theme(plot.title    = element_blank()) +
    theme(plot.subtitle = element_blank()) +
    theme(plot.caption = element_markdown(
        size = 17,
        lineheight = 0.4,
        hjust = 0.5, vjust = 0.5,
        margin = unit(c(0, 0, 0, 0), "in"))) +
    # Format the axis text
    theme(axis.title.x  = element_blank()) +
    theme(axis.title.y  = element_blank()) +
    theme(axis.text.x   = element_text(
        size = 16, colour = colour_text,
        hjust = 0.5, vjust = 1,
        face = "bold", family = main_font,
        lineheight = .4)) +
    theme(axis.text.y   = element_text(
        size = 15, colour = colour_text,
        hjust = 1, vjust = 0.5,
        face = "bold", family = main_font))


ggsave(
  filename = "plot.png", # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)
