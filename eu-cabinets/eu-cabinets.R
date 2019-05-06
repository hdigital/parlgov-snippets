library(tidyverse)
library(ggrepel)


## Get sources -----

if(FALSE) {
  # run once to get recent data
  download.file("http://www.parlgov.org/static/data/development-utf-8/view_party.csv", "source__parlgov-party-csv")
  download.file("http://www.parlgov.org/static/data/development-utf-8/view_cabinet.csv", "source__parlgov-cabinets-csv")
  download.file("https://partyfacts.herokuapp.com/download/external-parties-csv/", "source__partyfacts.csv")
  download.file("https://www.chesdata.eu/s/2014_CHES_dataset_means.csv", "source__ches-2014-csv")
}

pg_cab_raw <- read_csv("source__parlgov-cabinets-csv")
pg_party_raw <- read_csv("source__parlgov-party-csv")
pf_raw <- read_csv("source__partyfacts.csv")
ches_raw <- read_csv("source__ches-2014-csv")

eu_28 <- str_split("AUT BEL BGR CYP CZE DEU DNK ESP EST FIN FRA GBR GRC HRV HUN
                    IRL ITA LTU LUX LVA MLT NLD POL PRT ROU SVK SVN SWE", "\\W+")[[1]]

# current EU cabinets from ParlGov
pg_cab_eu <-
  pg_cab_raw %>%
  mutate(share = round(100 * seats / election_seats_total), 1) %>%
  filter(cabinet_party == 1) %>%
  group_by(country_id) %>%
  filter(start_date == max(start_date), country_name_short %in% eu_28) %>%
  select(-left_right) %>%
  ungroup()


## Party positons ----

# link datasets (select only linked parties)
link_pg_ches <-
  pf_raw %>% filter(dataset_key == "parlgov") %>%
  inner_join(pf_raw %>% filter(dataset_key == "ches"), by = c("partyfacts_id" = "partyfacts_id")) %>%
  select(country = country.x, parlgov_id = dataset_party_id.x, ches_id = dataset_party_id.y) %>%
  left_join(pg_party_raw %>% select(parlgov_id = party_id, family = family_name_short))

# CHES party positions EU
ches_party <-
  ches_raw %>%
  select(ches_id = party_id, left_right = lrgen, eu_contra_pro = eu_position) %>%
  gather(dimension, position, -ches_id) %>%
  mutate(position = round(position, 1)) %>%
  inner_join(link_pg_ches %>% rename(party_id = parlgov_id)) %>%
  filter(country %in% eu_28) %>%
  select(-country)

# CHES positions party families
ches_family <-
  ches_party %>%
  group_by(family, dimension) %>%
  summarise(position = round(mean(position), 1) + 0.0001)  # add minor noise for party family positions

# Cabinet parties CHES positions
party_pos <-
  pg_cab_eu %>%
  select(party_id) %>%
  inner_join(ches_party %>% select(-family), by = "party_id")

# Cabinet parties CHES positions with party family
party_pos_family <-
  pg_cab_eu %>%
  select(party_id) %>%
  filter(! party_id %in% party_pos$party_id) %>%
  left_join(pg_party_raw %>% select(party_id, family = family_name_short), by = "party_id") %>%
  left_join(ches_family, by = "family") %>%
  filter(! family %in% c("none", "spec"))

# Cabinet parties CHES positions combined
party <-
  party_pos %>%
  bind_rows(party_pos_family %>% select(-family)) %>%
  spread(dimension, position)


## Cabinets graphs data ----

cab_eu <-
  pg_cab_eu %>%
  left_join(party) %>%
  mutate(
    party = paste(country_name_short, party_name_short),
    party = if_else(
      party_id %in% party_pos_family$party_id,
      paste0(party, "*"),
      party
    ),
    cabinet = paste(country_name_short, cabinet_name)
    ) %>%
  select(cabinet, start_date, party, share, left_right, eu_contra_pro)

cab_ctry <-
  cab_eu %>%
  group_by(cabinet) %>%
  summarise(
    left_right = weighted.mean(left_right, share, na.rm = TRUE),
    eu_contra_pro = weighted.mean(eu_contra_pro, share, na.rm = TRUE)
  )


## Graphs EU cabinets ----

pl_ctry <-
  ggplot(cab_ctry, aes(left_right, eu_contra_pro, label = cabinet)) +
  geom_point() +
  geom_text_repel() +
  xlim(1, 9)

print(pl_ctry)
ggsave("eu-cabinets.png", pl_ctry, width = 8, height = 6)

pl_party <-
  ggplot(cab_eu, aes(left_right, eu_contra_pro, label = party)) +
  geom_point(aes(size = share), colour = "pink", alpha = 0.8) +
  geom_text_repel(size = 2.5) +
  labs(caption = "[ * party family position ]")

print(pl_party)
ggsave("eu-cabinet-parties.png", pl_party, width = 8, height = 6)
