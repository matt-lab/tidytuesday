library(tidyverse) # data wrangling
library(lubridate) # handles date data
library(ggimage) # adding images to ggplot
library(magick) # cropping images
library(showtext) # for fonts

#### Import data ####
data <- read_csv(
    "deck.csv",
    col_types = cols(
        id = col_integer(),
        name = col_character(),
        date_creation = col_datetime(format = ""),
        date_update = col_datetime(format = ""),
        description_md = col_character(),
        user_id = col_integer(),
        investigator_code = col_character(),
        investigator_name = col_character(),
        version = col_double(),
        xp = col_integer(),
        xp_spent = col_integer(),
        xp_adjustment = col_integer(),
        taboo_id = col_integer(),
        tags = col_character(),
        previous_deck = col_integer(),
        next_deck = col_integer()))
data_investigators <-  read_csv(
    "investigators.csv",
    col_types = cols(
        pack_code = col_character(),
        available = col_date(format = ""),
        faction_code = col_character(),
        code = col_character(),
        name = col_character(),
        imagesrc = col_character())) |>
    rename(investigator_code = code) |>
    rename(investigator_name = name)

#### Wrangle data ####
investigators_in_decks <- data |>
    filter(version == 1) |>
    count(investigator_code) |>
    arrange(n) |>
    left_join(data_investigators, by = "investigator_code")
# Identify months since release
investigators_in_decks <- investigators_in_decks |>
    group_by(investigator_name) |>
    summarise(
        earliest_available = min(available),
        faction = unique(faction_code),
        investigator_name = unique(investigator_name),
        n = sum(n),
        imagesrc = imagesrc[1]
    ) |>
    ungroup() |>
    mutate(years_since_release =
        interval(earliest_available, "2023-01-01") / years(1))

# Create images
crop_investigator_card <- function(image_url) {
    image <- image_read(image_url)
    # Use dimension information to crop image into a square
    image_info(image)
    new_width <- floor(image_info(image)$width / 2)
    height_offset <- floor(image_info(image)$height / 5)
    new_height <- min(new_width, image_info(image)$height - height_offset)
    image_art <- image_crop(
        image,
        geometry = str_c(new_width, "x", new_height, "+0+", height_offset))
    # Create mask
    mask <- image_draw(image_blank(new_height, new_height))
    symbols(
        new_height/2,
        new_height/2,
        circles=(new_height/2)-3,
        bg='black',
        inches = FALSE,
        add = TRUE)
    dev.off()
    # Combine images
    image_circle <- image_composite(image_art, mask, operator = 'copyopacity')
    # Write image
    path <- str_c(tempfile(), ".png")
    image_write(image_circle, path = path, format = "png")
    return(path)
}

investigators_in_decks <- investigators_in_decks |>
    mutate(image = case_when(
        investigator_name == "Darrell Simmons" ~ "https://derbk.com/ancientevils/wp-content/uploads/2022/08/darrell1.png",
        investigator_name == "Kymani Jones" ~ "https://derbk.com/ancientevils/wp-content/uploads/2022/09/kymani-4.png",
        investigator_name == "Vincent Lee" ~ "https://derbk.com/ancientevils/wp-content/uploads/2022/09/vincent1.png",
        !is.na(imagesrc) ~ str_c("https://arkhamdb.com", imagesrc))) |>
    filter(investigator_name %in% c("William Yorick") | is.na(imagesrc)) |>
    rowwise() |>
    mutate(image_circle = crop_investigator_card(image)) |>
    ungroup()

#### Create data ####

my_theme <- function() {
    # fonts
    main_font = 'Cutive Mono'

    # colour
    colour.background = "#030303"
    colour.text = "#F2F2F2"
    
    # Begin construction of chart
    theme_bw(base_size = 15) +
    
    # Format background colour
    theme(panel.background =
        element_rect(fill = colour.background, colour = colour.background)) +
    theme(plot.background  =
        element_rect(fill = colour.background, colour = colour.background)) +
    theme(panel.border     = element_rect(colour = colour.background)) +
    theme(strip.background =
        element_rect(fill = colour.background, colour = colour.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_line(colour = "#686868")) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    theme(legend.background =
        element_rect(fill = colour.background, colour = colour.background)) +
    theme(legend.text =
        element_text(size = 15, face = "bold", colour = colour.text)) +
    theme(legend.justification = "center") +
    theme(legend.title = element_text(family = main_font,
                                    colour = "#030303",
                                    size = 15, face = "bold")) +
                                    
    # Format title and axis labels
    theme(plot.title       = element_text(colour = colour.text, size = 40, face = "bold", hjust = 0.5, family = main_font))+
    theme(plot.subtitle    = element_text(colour = colour.text, size = 30, face = "bold", hjust = 0.5, family = main_font))+
    theme(plot.caption     = element_text(colour = colour.text, size = 20, face = "bold", hjust = 0.5, family = main_font))+
    theme(axis.title.x     = element_text(size=20, colour = colour.text, hjust = 0.5, vjust = 0.5,face = "bold", family = main_font)) +
    theme(axis.title.y     = element_text(size=20, colour = colour.text, hjust = 0.5, vjust = 0.5,face = "bold", family = main_font)) +
    theme(axis.text.x      = element_text(size=25, colour = colour.text, hjust = 0.5, vjust = 0.5,face = "bold", family = main_font)) +
    theme(axis.text.y      = element_text(size=25, colour = colour.text, face = "bold", family = main_font)) +
    theme(strip.text       = element_text(size=25, colour = colour.text, hjust = 0.5, vjust = 0.5,face = "bold", family = main_font)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

faction_colours <- tibble(
    faction = c("mystic", "rogue", "survivor", "seeker", "guardian"),
    faction_colour = c(
        "#4a4296", "#107116", "#cc3038",
        "#ff8f3f", "#2b80c5")
)

# This font is used for the plot:
# font_add_google("Cutive Mono", "Cutive Mono")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
plot <- investigators_in_decks |>
    left_join(faction_colours, by = "faction") |>
    ggplot(aes(years_since_release, n)) +
    geom_image(
        mapping = aes(image = image_circle, colour = faction_colour),
        size = 0.069,
        asp = 1,
        alpha = 0.5) +
    geom_image(
        mapping = aes(image = image_circle),
        size = 0.06,
        asp = 1) +
    scale_colour_identity() +
    scale_y_continuous(
        breaks = seq(
            50,
            ceiling(max(investigators_in_decks$n) / 50) * 50,
            50),
        limits = c(
            0,
            ceiling(max(investigators_in_decks$n) / 50) * 50)) +
    scale_x_continuous(
        breaks = seq(
            0,
            ceiling(max(investigators_in_decks$years_since_release)),
            1),
        limits = c(
            0,
            ceiling(max(investigators_in_decks$years_since_release)))) +
    #ylim(0, (max(investigators_in_decks$n) %/% 100 + 1) * 100) +
    xlab("Years since investigator was released") +
    ylab("Number of decks made in 2022") +
    my_theme()
plot
ggsave("plot.png", width = 10, height = 10, units = "in", dpi = 300)
