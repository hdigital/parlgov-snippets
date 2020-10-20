library(tidyverse)
library(lubridate)


## Get data ----

# Get data from ParlGov database

db_file <- "source__parlgov.db"
url <- "http://www.parlgov.org/static/data/parlgov-development.db"

# download ParlGov database if not in local folder
if (!db_file %in% list.files()) {
  download.file(url, db_file, mode = "wb")
}

# get data from ParlGov database tables
parlgov_db <- DBI::dbConnect(RSQLite::SQLite(), db_file)
get_parlgov_table <-
  function(table_name)
    tbl(parlgov_db, table_name) %>% collect()

party_raw <- get_parlgov_table("view_party")
elec_raw <- get_parlgov_table("view_election")
cab_raw <- get_parlgov_table("view_cabinet")


## Party family ----

fam_level <-
  c("com", "eco", "soc", "agr", "lib", "chr", "con", "right", "other")

fam_color <- c(
  "#FB9A99",
  "#33A02C",
  "#E31A1C",
  "#B2DF8A",
  "#FDBF6F",
  "#FF7F00",
  "#1F78B4",
  "#A6CEE3",
  "grey60"
)

party <-
  party_raw %>%
  mutate(family = family_name_short,
         family = if_else(family %in% fam_level, family, "other"))


# Country and years ----

# country information
country <-
  party_raw %>%
  select(starts_with("country_")) %>%
  distinct() %>%
  rename(country = country_name_short)

# dataset with all years based on first ParlGov year per country
years <-
  elec_raw %>%
  group_by(country_id) %>%
  do(year = seq(min(year(.$election_date)), year(today()))) %>%
  unnest(year)

# function to get last election or cabinet for a year
get_year <- function(df, param_country_id, param_year) {
  df %>%
    filter(country_id == param_country_id, year <= param_year) %>%
    filter(year == max(year)) %>%
    mutate(year = param_year)
}


## Elections ----

# last election per year
elec <-
  elec_raw %>%
  filter(election_type == "parliament") %>%
  mutate(year = year(election_date) + 1,
         seat_share = 100 * seats / seats_total) %>%
  group_by(country_id, year) %>%
  filter(election_date == max(election_date)) %>%
  ungroup() %>%
  left_join(party %>% select(party_id, family))


# use reshape to add 0.0% for complete family data
elec_fam <-
  elec %>%
  group_by(country_id, year, family) %>%
  summarise(seat_share = sum(seat_share, na.rm = TRUE) %>% round(1)) %>%
  spread(family, seat_share, fill = 0) %>%
  gather(family, seat_share, -country_id, -year) %>%
  ungroup()

# yearly seat share by party family
elec_year <-
  years %>%
  rowwise() %>%
  do(get_year(elec_fam, .$country_id, .$year)) %>%
  ungroup()


## Cabinets ----

# select first cabinet per year (last previous year) and calculate seat share
cab <-
  cab_raw %>%
  mutate(year = year(start_date) + 1) %>%
  group_by(country_id, year) %>%
  filter(cabinet_party == 1,
         start_date == max(start_date)) %>%
  mutate(cabinet_share = round(100 * seats / sum(seats), 1)) %>%
  ungroup()

# cabinet share by party family
cab_fam <-
  cab %>%
  left_join(party %>% select(party_id, family)) %>%
  group_by(country_id, year, family) %>%
  summarise(cabinet_share = sum(cabinet_share)) %>%
  ungroup()

# yearly cabinet parties by party family
cab_year <-
  years %>%
  rowwise() %>%
  do(get_year(cab_fam, .$country_id, .$year)) %>%
  ungroup()


## Results ----

# party family data in long format
fam <-
  country %>%
  left_join(elec_year) %>%
  left_join(cab_year) %>%
  mutate(cabinet_share = if_else(is.na(cabinet_share), 0, cabinet_share)) %>%
  arrange(country_name, year, family)

# party family seat share in wide format
fam_elec <-
  fam %>%
  select(-cabinet_share) %>%
  spread(family, seat_share)

# party family cabinet share in wide format
fam_cab <-
  fam %>%
  select(-seat_share) %>%
  spread(family, cabinet_share)

# results into data files
write_csv(fam, "party-family.csv", na = "")
write_csv(fam_elec, "party-family-seat-share.csv", na = "")
write_csv(fam_cab, "party-family-cabinet-share.csv", na = "")


## Figure ----

pl_dt <-
  fam %>%
  mutate(family = factor(family, fam_level)) %>%
  group_by(country) %>%
  filter(min(year) < 1950) %>%
  ungroup()

pl <-
  ggplot(pl_dt, aes(year, cabinet_share, fill = family)) +
  geom_area(position = "stack") +
  scale_fill_manual(values = fam_color) +
  scale_x_continuous(breaks = seq(1900, 2000, 50)) +
  facet_wrap(~ country_name) +
  labs(caption = "(ParlGov countries democratic before 1950)")

print(pl)
ggsave("cabinet-share.png", pl, width = 8, height = 6)
