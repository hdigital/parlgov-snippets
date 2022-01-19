library(tidyverse)


## ParlGov data ----

parlgov_xlsx <- "parlgov.xlsx"

if(FALSE) {
  # run once to get recent data
  download.file("https://parlgov.org/data/parlgov.xlsx",
                parlgov_xlsx,
                mode = "wb") # needed on Windows
}

elec_raw <- readxl::read_excel(parlgov_xlsx, sheet = "election")
cab_raw <- readxl::read_excel(parlgov_xlsx, sheet = "cabinet")


## Create dataset ----

# select first cabinet after elections
cabinet_id_after_election <- 
  cab_raw %>% 
  # filter(caretaker != 1) %>% 
  group_by(election_id) %>% 
  slice(1) %>% 
  pull(cabinet_id)

# select cabinet parties after first election
cab <- 
  cab_raw %>% 
  filter(cabinet_id %in% cabinet_id_after_election) %>% 
  select(election_id, party_id, cabinet_party)

elec_cab <- 
  elec_raw %>% 
  left_join(cab)


## Results into csv ----

write_csv(elec_cab, "cabinet-parties-after-election.csv")
