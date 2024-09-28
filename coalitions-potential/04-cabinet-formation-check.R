library(tidyverse)


select_cabinet_n <- 5 # set parameter for number of cabinets to select
select_formation_max_n <- 4 # maximum number of formation opportunities to select

set.seed(0) # change seed number to select different cabinets


cab_raw <- read_csv("01-cabinet-dataset.csv")
coal_raw <- read_csv("03-cabinet-formation.zip")

cab_select <-
  cab_raw %>%
  group_by(cabinet_id) %>%
  summarise(n = n()) %>%
  filter(n <= select_formation_max_n) %>%
  sample_n(select_cabinet_n)

cab <- cab_raw %>% filter(cabinet_id %in% cab_select$cabinet_id)

coal <-
  coal_raw %>%
  filter(cabinet_id %in% cab_select$cabinet_id) %>%
  rowid_to_column(var = "coalition") %>%
  mutate(party_id = strsplit(coalition_parties, " ")) %>%
  unnest(party_id) %>%
  mutate(party_id = as.integer(party_id)) %>%
  rename_all(~ paste0("coal_", .)) %>%
  select(
    cabinet_id = coal_cabinet_id, party_id = coal_party_id,
    coal_coalition, everything(), -coal_coalition_parties
  )

coal_out <- coal %>% left_join(cab)

write_csv(coal_out, "04-cabinet-formation-check.csv")
