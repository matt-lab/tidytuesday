library(tidyverse) # for data wrangling

source("scripts_webscraping/api_helpers.R")

# Use arkhamdb_api to access card information
arkhamdb_api_get_card <- function(code) {
    query <- sprintf(
        "/api/public/card/%s",
        code)
    arkhamdb_api(query)
}

# Read in data
if (file.exists("cards_in_decks.csv")) {
    cards_in_decks <- read_csv(
        file = 'cards_in_decks.csv',
        col_types = cols(
            id = col_character(),
            card_id = col_character(),
            card_quantity = col_double())) |>
    filter(card_quantity > 0)

    card_ids <- unique(cards_in_decks$card_id)

    response <- card_ids |>
        map(arkhamdb_api_get_card)

    card_variables <- c(
        "code",
        "pack_code",
        "faction_code",
        "name",
        "xp",
        "type_code",
        "imagesrc")

    cards <- response |>
        map(~ .x[[1]]) |>
        map(~ .x[card_variables]) |>
        map_dfr(bind_rows)
    write_csv(cards, 'cards.csv')
}