library(lubridate) # for dates
library(tidyverse) # for data wrangling

source("scripts_webscraping/api_helpers.R")

# Use arkhamdb_api to access decklist of given date
arkhamdb_api_get_decklist <- function(date) {
    query <- sprintf(
        "/api/public/decklists/by_date/%s.json?_format=json",
        date)
    arkhamdb_api(query)
}

# Collect response
dates <- seq(
    ymd("2022-01-01"),
    ymd("2022-12-31"),
    by = '1 day')
response <- dates |>
    map(arkhamdb_api_get_decklist)

# Identify deck variables
deck_variables <- c(
    'id',
    'name',
    'date_creation',
    'date_update',
    'description_md',
    'user_id',
    'investigator_code',
    'investigator_name',
    'version',
    'xp',
    'xp_spent',
    'xp_adjustment',
    'taboo_id',
    'tags',
    'previous_deck',
    'next_deck')

# Parse deck variables from response
data_decks <- response |>
    map(~ .x[[1]]) |>
    map(~ .x[deck_variables]) |>
    map_dfr(bind_rows) |>
    as_tibble()
write_csv(data_decks, 'deck.csv')

# Parse cards in each deck from response
data_cards <- response |>
    map(~ .x[[1]]) |>
    map(~ .x[c('id', 'slots')]) |>
    map(~ unnest(.x, 'slots')) |>
    map_dfr(bind_rows)
data_cards <- data_cards |>
    pivot_longer(
        -id,
        names_to = "card_id",
        values_to = "card_quantity"
    ) |>
    mutate(card_quantity = replace_na(card_quantity, 0))
write_csv(data_cards, 'cards_in_decks.csv')