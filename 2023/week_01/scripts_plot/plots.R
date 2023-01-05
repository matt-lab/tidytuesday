library(tidyverse) # data wrangling
library(lubridate) # handles date data
library(ggimage) # adding images to ggplot
library(magick) # cropping images
library(showtext) # for fonts
library(ggtext) # for text

#### Import data ####
data <- read_csv(
    "deck.csv",
    col_types = cols(
        id = col_character(),
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
    left_join(data_investigators, by = "investigator_code")
# Aggregate data so each row is an investigator
investigators_in_decks <- investigators_in_decks |>
    group_by(investigator_name) |>
    summarise(
        earliest_available = min(available),
        faction = unique(faction_code),
        investigator_name = unique(investigator_name),
        n = sum(n),
        imagesrc = imagesrc[1],
        pack_code = pack_code[1]
    ) |>
    ungroup()

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
        !is.na(imagesrc) ~ str_c("https://arkhamdb.com", imagesrc)))

#### Create plots ####

# Specifications
my_theme <- function() {
    # colour
    colour.background = "#030303"
    colour.text = "#dbdbdb"
    
    # Begin construction of chart
    theme_bw(base_size = 15) +
        
    # Format the grid
    theme(panel.background = element_rect(fill = colour.background, color = colour.background)) +
    theme(plot.background  = element_rect(fill = colour.background, color = colour.background)) +
    theme(panel.border       = element_blank()) +
    theme(panel.grid.major.x = element_line(colour = "#585858")) +
    theme(panel.grid.minor.x = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks         = element_blank()) +
    
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
    theme(plot.title    = element_blank()) +
    theme(plot.subtitle = element_blank()) +
    theme(plot.caption  = element_blank()) +
    theme(axis.title.x  = element_text(size = 25, colour = colour.text, hjust = 0.5, vjust = 0.5, face = "bold", family = main_font)) +
    theme(axis.title.y  = element_blank()) +
    theme(axis.text.x   = element_text(size = 20, colour = colour.text, hjust = 0.5, vjust = 0.5, face = "bold", family = main_font)) +
    theme(axis.text.y   = element_blank()) +
    theme(legend.position = "none") +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

faction_colours <- tibble(
    faction = c(
        "mystic", "rogue", "survivor",
        "seeker", "guardian", "neutral"),
    faction_colour = c(
        "#4a4296", "#107116", "#cc3038",
        "#ff8f3f", "#2b80c5", "#696969")
)

# Roboto Mono is required for plot
main_font <- "Roboto Mono"
font_add_google(main_font, main_font)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Top investigators
top_investigators <- investigators_in_decks |>
    slice_max(n = 10, n) |>
    rowwise() |>
    mutate(image_circle = crop_investigator_card(image)) |>
    ungroup()

plot <- top_investigators |>
    left_join(faction_colours, by = "faction") |>
    mutate(rank = order(n)) |>
    ggplot(aes(rank, n)) +
    geom_bar(
        mapping = aes(colour = faction_colour, fill = faction_colour),
        stat = "identity",
        width = 0.02) +
    geom_image(
        mapping = aes(image = image_circle, colour = faction_colour),
        size = 0.089,
        asp = 1) +
    geom_image(
        mapping = aes(image = image_circle),
        size = 0.08,
        asp = 1) +
    scale_colour_identity() +
    scale_fill_identity() +
    geom_text(
        aes(label = investigator_name),
        size = 8,
        hjust = 1.4,
        vjust = -0.5,
        colour = "#a5a5a5",
        fontface = "bold",
        family = main_font) +
    ylab("Number of public decks made in 2022") +
    scale_y_continuous(
        breaks = seq(
            0,
            ceiling(max(top_investigators$n) / 50) * 50,
            50),
        limits = c(
            0,
            ceiling(max(top_investigators$n) / 50) * 50)) +
    coord_flip() +
    annotate(
        "text", x = 3.5, y = 200,
        label = "Year of the Dog",
        hjust = 0.5, vjust = 1,
        size = 10, fontface = "bold", family = main_font, colour = "#dbdbdb") +
    annotate(
        "text", x = 3, y = 200,
        label = str_c(
            str_wrap(
                '"Ashcan" Pete and good boy Duke are top of the ten most popular investigators of 2022.',
                27),
            '\nData from ArkhamDB.'),
        hjust = 0.5, vjust =1,
        size = 7, family = main_font, colour = "#d7d7d7") +
    my_theme()
plot
ggsave("top_investigators.png", width = 10, height = 10, units = "in", dpi = 300)

# Top Scarlet Keys investigators
top_investigators_edge <- investigators_in_decks |>
    filter(pack_code == 'eoep') |>
    rowwise() |>
    mutate(image_circle = crop_investigator_card(image)) |>
    ungroup()

plot <- top_investigators_edge |>
    left_join(faction_colours, by = "faction") |>
    mutate(rank = order(n)) |>
    ggplot(aes(rank, n)) +
    geom_bar(
        aes(colour = faction_colour, fill = faction_colour),
        stat = "identity", width = 0.02) +
    geom_image(
        mapping = aes(image = image_circle, colour = faction_colour),
        size = 0.099,
        asp = 1) +
    geom_image(
        mapping = aes(image = image_circle),
        size = 0.09,
        asp = 1) +
    scale_colour_identity() +
    scale_fill_identity() +
    geom_text(
        aes(label = investigator_name),
        size = 8,
        hjust = 1.4,
        vjust = -0.5,
        colour = "#a5a5a5",
        fontface = "bold",
        family = main_font) +
    ylab("Number of public decks made in 2022") +
    scale_y_continuous(
        breaks = seq(
            0,
            ceiling(max(top_investigators_edge$n) / 25) * 25,
            25),
        limits = c(
            0,
            ceiling(max(top_investigators_edge$n) / 25) * 25)) +
    coord_flip() +
    annotate(
        "text", x = 2.5, y = 90,
        label = "Destined to Succeed",
        hjust = 0.5, vjust = 1,
        size = 10, fontface = "bold", family = main_font, colour = "#dbdbdb") +
    annotate(
        "text", x = 2.3, y = 90,
        label = str_c(
            str_wrap(
                'Lily was the most popular Edge of the Earth investigator of 2022.',
                27),
            '\nData from ArkhamDB.'),
        hjust = 0.5, vjust = 1,
        size = 7, family = main_font, colour = "#d7d7d7") +
    my_theme()
plot
ggsave("top_investigators_edge.png", width = 10, height = 10, units = "in", dpi = 300)


#### Top cards ####
data_cards_in_decks <- read_csv(
    file = 'cards_in_decks.csv',
    col_types = cols(
        id = col_character(),
        card_id = col_character(),
        card_quantity = col_double())) |>
    filter(card_quantity > 0)

data_cards <- read_csv(
    file = 'cards.csv',
    col_types = cols(
        code = col_character(),
        pack_code = col_character(),
        faction_code = col_character(),
        name = col_character(),
        type_code = col_character(),
        xp = col_double(),
        imagesrc = col_character()
    )) |>
    rename(card_id = code)

# Filter to only first version of decks
popular_cards <- data |>
    filter(version == 1) |>
    select(id) |>
    left_join(data_cards_in_decks, by = 'id') |>
    count(card_id) |>
    left_join(data_cards, by = 'card_id') |>
    filter(xp == 0) |>
    filter(faction_code != 'neutral') |>
    group_by(name) |>
    summarise(
        n = sum(n),
        faction_code = faction_code[1],
        imagesrc = imagesrc[1]) |>
    group_by(faction_code) |>
    slice_max(n = 10, n)

# Update image link
popular_cards <- popular_cards |>
    mutate(image = str_c("https://arkhamdb.com", imagesrc))

# Crop images

crop_card <- function(image_url) {
    image <- image_read(image_url)
    # Use dimension information to crop image into a square
    image_info(image)
    width_offset <- floor(image_info(image)$width / 6)
    new_width <- image_info(image)$width - (2 * width_offset)
    height_offset <- floor(image_info(image)$height / 12)
    new_height <- new_width
    image_art <- image_crop(
        image,
        geometry = str_c(new_width, "x", new_height, "+", width_offset, "+", height_offset))
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

popular_cards <- popular_cards |>
    rowwise() |>
    mutate(image_circle = crop_card(image)) |>
    ungroup()

factions_to_plot <- unique(popular_cards$faction_code)

for (faction in factions_to_plot) {
    plot_file <- sprintf("top_cards_%s.png", faction)
    plot_data <- popular_cards |>
        filter(faction_code == faction) |>
        rename(faction = faction_code) |>
        left_join(faction_colours, by = "faction") |>
        mutate(rank = rank(n, ties.method = 'first'))
    
    # Ensure there is room for a title on the vertical axis
    left_of_title <- plot_data |>
        filter(rank == 5) |>
        pull(n)
    max_decks <- max(ceiling(max(plot_data$n) / 100) * 100, left_of_title + 400)

    # Roughly, 45 characters fit horizontally on the plot
    # Calculate a unit-to-character ratio
    char_ratio <- floor(max_decks / 50)
    image_offset <- ceiling(.09 * max_decks / 2)
    plot_data <- plot_data |>
        mutate(name_space = n / 2 + 0.5 * (nchar(name) * char_ratio)) |>
        mutate(name_space_available = n - image_offset) |>
        rowwise() |>
        mutate(name = ifelse(
            name_space > name_space_available,
            str_wrap(name, floor(n / (char_ratio * 2))),
            str_c(name, "\n"))) |>
        ungroup() |>
        mutate(name = ifelse(str_detect(name, 'Cigarette'), 'Lucky\nCigarette Case', name))

    plot <- plot_data |>
        ggplot(aes(rank, n)) +
        geom_bar(
            aes(colour = faction_colour, fill = faction_colour),
            stat = "identity", width = 0.02) +
        geom_image(
            mapping = aes(image = image_circle, colour = faction_colour),
            size = 0.089,
            asp = 1) +
        geom_image(
            mapping = aes(image = image_circle),
            size = 0.08,
            asp = 1) +
        scale_colour_identity() +
        scale_fill_identity() +
        geom_text(
            aes(y = n / 2, label = name),
            size = 8,
            hjust = 0.5,
            vjust = 0.5,
            colour = "#a5a5a5",
            fontface = "bold",
            family = main_font) +
        ylab("Number of public decks made in 2022") +
        scale_y_continuous(
            breaks = seq(
                0,
                max_decks,
                100),
            limits = c(
                0,
                max_decks)) +
    annotate(
        "text", x = 4.5, y = max_decks - (max_decks * .20),
        label = str_to_title(sprintf("Top %s cards", faction)),
        hjust = 0.5, vjust = 1,
        size = 10, fontface = "bold", family = main_font, colour = "#dbdbdb") +
    annotate(
        "text", x = 4, y = max_decks - (max_decks * .20),
        label = str_c(
            str_wrap(
                sprintf('Arkham Horror has never been more diverse, but there are some 0 xp staples of %s deckbuilding.', str_to_title(faction)),
                23),
            '\nData from ArkhamDB.'),
        hjust = 0.5, vjust = 1,
        size = 7, family = main_font, colour = "#d7d7d7") +
        coord_flip() +
        my_theme()
    plot
    ggsave(plot_file, width = 10, height = 10, units = "in", dpi = 300)
}
