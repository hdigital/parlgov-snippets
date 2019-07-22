# Create map of yearly cabinet composition

library(tidyverse)
library(maptools)  # dependency ggplot::coord_map()

# parameters for party family selection
family_select <- "soc"
family_color <- "red"
family_caption <- "(Social Democratic cabinet share since 1945)"


## Cabinet data ----

# read ParlGov cabinet table and select cabinet parties
cab_raw <- read_csv("cabinet-party-family-year.csv")

# calculate post-war mean yearly share of party families
cab_share_45 <-
  cab_raw %>%
  filter(year >= 1945) %>%
  rename(country = country_name_short) %>%
  group_by(country) %>%
  summarise_at(vars(com:other), mean)


## Graph map ----

europe_map <- read_rds("europe-map.rds")

# add countries not in cabinet subset to have map with all countries
country_add <-
  europe_map %>%
  distinct(id) %>%
  filter(!id %in% cab_raw$country_name_short) %>%
  select(country = id)

# create dataset for plot
cab_pl <-
  cab_share_45 %>%
  select(country, share = family_select) %>%
  bind_rows(country_add)

# filter(lat > 35.5, lat < 69.5, long > -12.5, long < 33)  # Europe

pl <-
  ggplot(cab_pl, aes(map_id = country)) +
  geom_map(aes(fill = share), map = europe_map, colour = "darkgrey") +
  scale_fill_gradient(low = "grey90",
                      high = family_color,
                      na.value = "gray80") +
  expand_limits(x = c(-12.5, 33), y = c(35.5, 69.5)) +
  coord_map("polyconic") +
  labs(caption = family_caption) +
  theme(axis.title = element_blank())
print(pl)

file_pl <- "cabinet-map-europe.png"
ggsave(
  plot = pl,
  file = file_pl,
  width = 8,
  height = 6
)
