library(tidyverse)


filter_seats <- 1 # option to remove parties with one (or more) seats


## Data from database ----

con <- DBI::dbConnect(RSQLite::SQLite(), "parlgov-stable.db")

cab_raw <- tbl(con, "view_cabinet") %>% collect()
party_raw <- tbl(con, "view_party") %>% collect()
pres_raw <- tbl(con, "politician_president") %>% collect()

DBI::dbDisconnect(con)


## Presidents data ----

# function to get cabinet for a president
get_cabinet_id <- function(country, start) {
  cabinet_id <-
    cab_raw %>%
    filter(
      cabinet_party == 1,
      country_id == {{ country }},
      start_date <= {{ start }}
    ) %>%
    group_by(country_id) %>%
    filter(start_date == max(start_date)) %>%
    ungroup() %>%
    pull(cabinet_id) %>%
    unique()

  if (length(cabinet_id == 1)) {
    cabinet_id
  } else {
    NA_integer_
  }
}

pres <-
  pres_raw %>%
  mutate(cabinet_id = map2_int(country_id, start_date, get_cabinet_id)) %>%
  left_join(party_raw %>% select(party_id, president_party = party_name_short)) %>%
  select(cabinet_id, party_id, start_date, president_party, person_id_source)

pres_join <-
  pres %>%
  select(cabinet_id, party_id) %>%
  mutate(president = 1)


## Final dataset ----

cab_out <-
  cab_raw %>%
  filter(seats > filter_seats) %>%
  left_join(party_raw %>% select(party_id, family = family_name_short)) %>%
  left_join(pres_join) %>%
  mutate(president = if_else(is.na(president), 0, president)) %>%
  relocate(president, .after = prime_minister)

# write_csv(cab_out, "01-cabinet-dataset.csv")
