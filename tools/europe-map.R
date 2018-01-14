library(tidyverse)

# map packages needed -- reverse dependency (broom::broom() -> maptools -> rgeos)
pkgs <- c("maptools", "rgeos", "rworldmap")
reqs <- map_lgl(pkgs, require, character.only = TRUE)
if (any(!reqs)) {
  install.packages(pkgs[!reqs])  # install packages required
}

# create Europe map data for ggplot
world_map <- rworldmap::getMap(resolution = "low")
world_ggmap <- broom::tidy(world_map, region = "ISO3")
europe_ggmap <-
  world_ggmap %>%
  filter(lat > 30, lat < 75, long > -25, long < 65)  # Europe

write_rds(europe_ggmap, "europe-map.rds")
