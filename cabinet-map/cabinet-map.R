library(tidyverse)
library(sf)

## ParlGov data -----

if(FALSE) {
  # run once to get recent data
  download.file("http://www.parlgov.org/static/data/development-utf-8/view_cabinet.csv",
                "source__parlgov-cabinets-csv")
}

cab_raw <- read_csv("source__parlgov-cabinets-csv")


## Cabinet data ----

cab_recent <- 
  cab_raw %>% 
  group_by(country_name_short) %>% 
  filter(
    cabinet_party == 1,
    start_date == max(start_date)
    ) %>% 
  mutate(
    seats_share = round(100 * seats / election_seats_total, 1),
    left_right = round(left_right, 1)
    ) %>% 
  select(country = country_name_short, cabinet = cabinet_name, start = start_date, 
         pm = prime_minister, party = party_name_short, seats_share, seats_share, left_right) %>% 
  arrange(country, desc(pm), desc(seats_share))

write_csv(cab_recent, "cabinets-recent.csv")

cab_lr <- 
  cab_recent %>% 
  group_by(country, cabinet) %>% 
  summarise(left_right = weighted.mean(left_right, seats_share, na.rm = TRUE)) %>% 
  ungroup() 


## Map ----

map <- read_rds("worldmap.rds")  # Natural Earth based world map

map_lr <- 
  map %>% 
  left_join(cab_lr) %>% 
  mutate(left_right = left_right - 5) %>% 
  filter(! is.na(left_right))

pl <- ggplot() + 
  geom_sf(data = map, lwd = 0.1, fill = "grey85") +
  geom_sf(data = map_lr, aes(fill = left_right), lwd = 0.25) +
  coord_sf(crs = "+proj=lcc +lat_0=31 +lon_0=10",  # projection with parameters
           xlim = c(-1700000, 1800000), ylim = c(600000, 4000000)) +
  scale_fill_gradient2() +
  labs(caption = paste("Last cabinet included:", max(cab_recent$start))) +
  theme_bw()

print(pl)
ggsave("cabinet-map.png", pl, width = 8, height = 6)
