library(tidyverse)


create_zip_file <- TRUE # option to save csv-data in zip file

cab_raw <- read_csv("01-cabinet-dataset.csv")


## Functions to create potential coalitions in different formats

get_coalitions <- function(parties) {
  if (length(parties) == 1) {
    return(parties)
  }
  map(1:length(parties), ~ combn(parties, ., simplify = FALSE)) %>% flatten()
}

get_coalitions_chr <- function(parties, collapse_chr = " ") {
  map_chr(get_coalitions(parties), ~ paste(., collapse = collapse_chr))
}


## Example

parties <- c("CDU/CSU", "SPD", "FDP")
get_coalitions(parties)
get_coalitions_chr(parties)


## Potential coalitions for ParlGov cabinets

cab_parties <-
  cab_raw %>%
  group_by(cabinet_id) %>%
  summarise(parties = list(party_id))

coal_chr <-
  cab_parties %>%
  mutate(coalition_parties = map(parties, get_coalitions_chr)) %>%
  unnest(coalition_parties)

# coal_df <- coal_chr %>% unnest(coalition = strsplit(coalition, " "), .id = "coalition")

name_file <- "02-cabinet-potential.csv"
write_csv(coal_chr, name_file)


## Zip results and remove csv file

# Windows usage -- http://stackoverflow.com/questions/29129681/create-zip-file-error-running-command-had-status-127

if (create_zip_file) {
  zip_file <- stringr::str_replace(name_file, "csv", "zip")
  if (file.exists(zip_file)) file.remove(zip_file)
  zip(zip_file, name_file)
  file.remove(name_file)
}
