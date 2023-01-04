library(tidyverse) # for data wrangling
library(lubridate) # for dates

source("scripts_webscraping/api_helpers.R")

# Use arkhamdb_api to access card information
arkhamdb_api_get_card <- function(code) {
    query <- sprintf(
        "/api/public/card/%s",
        code)
    arkhamdb_api(query)
}
# Use arkhamdb_api to fetch release dates
arkhamdb_api_get_release_date <- function() {
    query <- "api/public/packs/?json"
    arkhamdb_api(query)
}


# Read in data
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
response <- data$investigator_code |>
    unique() |>
    map(arkhamdb_api_get_card)

card_variables <- c(
    "pack_code",
    "faction_code",
    "code",
    "name",
    "imagesrc"
)
investigators <- response |>
    map(~ .x[[1]]) |>
    map(~ .x[card_variables]) |>
    map_dfr(bind_rows)

response_packs <- arkhamdb_api_get_release_date()
investigators <- response_packs[[1]] |>
    as_tibble() |>
    rename(pack_code = code) |>
    select(pack_code, available) |>
    right_join(investigators, by = 'pack_code')
write_csv(investigators, 'investigators.csv')