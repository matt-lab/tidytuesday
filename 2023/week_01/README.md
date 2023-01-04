# Arkham Horror

The data for this week concerns Arkham Horror, a cooperative card game where players fight the monsters from the Cthulu Mythos and other beats from the dark between the stars. Players can use the (incredible) website [ArkhamDB](arkhamdb.com) to build their decks. The data from this week concerns all decks made public on ArkhamDB in 2022.

- `decks` - deck information (wide format)
- `cards_in_decks` - cards in each deck (long format)

## Data Dictionary

### `decks.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|id |integer |Unique identifier of deck |
|name |character |Text title of deck |
|date_creation |character |Date when deck was created (YYYY-MM-DD, ISO representation) |
|date_update |character |Date when deck was updated (YYYY-MM-DD, ISO representation) |
|description_md |character |Description of deck, in markdown format |
|user_id |integer |Number that uniquely identifies deck creator |
|investigator_code |character |Identifier of investigator |
|investigator_name |character |Name of investigator |
|version  |double |Number of times deck has been updated |
|xp |integer |Experience available for deck changes |
|xp_spent |integer |Experience spent for deck changes |
|xp_adjustment |integer |Manual changes to experience |
|taboo_id |integer |Taboo list used |
|tags |character |Tags added to decks (delineated by commas) |
|previous_deck |integer |Former version of the deck (id) |
|next_deck |integer |Upgraded version of the deck (id) |

### `cards_in_decks.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|id |integer |Unique identifier of deck |
|card_id |character |Unique identifier of card |
|card_quantity |integer |Amount of cards in deck |

### `investigators.csv`

|variable        |class     |description |
|:---------------|:---------|:-----------|
|pack_code |character |Unique identifier for investigator pack |
|available |character |Date card was released (YYYY-MM-DD) |
|faction_code |character |Investigator faction |
|code |character |Identifier of investigator |
|name |character |Name of investigator |
|imagesrc |character |Path to card image |

********************************************************

Scripts to retrieve the data: 
- `method/data_collection.R`
