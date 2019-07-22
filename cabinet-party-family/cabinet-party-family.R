# Create dataset of cabinets by party families

library(tidyverse)

rm(list = ls())


## Get and read cabinet and party data from ParlGov database

db_file <- "parlgov-development.db"
url <- "http://www.parlgov.org/static/data/"

# download ParlGov database if not in local folder
if( ! db_file %in% list.files()) {
  download.file(paste0(url, db_file), db_file, mode="wb")
}

# retrieve data from ParlGov database tables
con <- DBI::dbConnect(RSQLite::SQLite(), db_file)

cab_raw <- tbl(con, "view_cabinet") %>% collect()
party_raw <- tbl(con, "view_party") %>% collect()
ctr_yr_raw <- tbl(con, "viewcalc_country_year_share") %>% collect()

DBI::dbDisconnect(con)


## Calculate cabinet level information

# get party family and order by left-right position
other <- c("code", "none", "spec")
party <- party_raw %>%
  mutate(family = factor(family_name_short),
         family = forcats::fct_reorder(family, left_right, na.rm = TRUE),
         family = forcats::fct_other(family, drop = other, other_level = "other")) %>% 
  select(party_id, family)

cab <- cab_raw %>% 
  filter(cabinet_party == 1) %>% 
  left_join(party)

cab_info <- cab_raw %>% 
  distinct(country_name_short, country_name, start_date, cabinet_name, caretaker, cabinet_id)

# calculate cabinet level party family shares
cab_fam <- cab %>% 
  group_by(cabinet_id, family) %>% 
  summarise(seats = sum(seats)) %>% 
  group_by(cabinet_id) %>% 
  mutate(share = round(seats * 100 / sum(seats), 1)) %>% 
  select(-seats)

# convert long into wide format with tidyr
cab_fam_wide <- cab_fam %>% spread(family, share, fill = 0)

cab_long <- cab_info %>% left_join(cab_fam)  
cab_wide <- cab_info %>% left_join(cab_fam_wide)

write_csv(cab_wide, "cabinet-party-family.csv")


## Data in country-year format

ctr_yr <- ctr_yr_raw %>% 
  filter(id_type == "cabinet") %>% 
  select(cabinet_id=id, year, year_share = share)

ctr_yr_n <- ctr_yr %>%
  left_join(cab %>%distinct(country_name_short, cabinet_id)) %>% 
  group_by(country_name_short, year) %>% 
  summarise(cabinets = n())

cab_yr_fam <- ctr_yr %>%
  left_join(cab_fam) %>% 
  drop_na(family) %>% 
  left_join(cab_info %>% select(country_name_short, country_name, cabinet_id)) %>% 
  group_by(country_name_short, country_name, year, family) %>% 
  summarise(share = round(sum(share * year_share), 1))

# remove first and last year per country (no full cabinet)
cab_yr_fam <- cab_yr_fam %>% 
  group_by(country_name_short) %>% 
  filter(year != min(year), year != max(year)) %>% 
  ungroup()
cab_yr_wide <- cab_yr_fam %>%
  left_join(ctr_yr_n) %>% 
  spread(family, share, fill = 0)

write_csv(cab_yr_wide, "cabinet-party-family-year.csv")
