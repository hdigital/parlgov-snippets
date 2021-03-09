library(tidyverse)
library(countrycode) # regions
library(ggsci) # color palette

# set working directory in subfolder
setwd(here::here("election-vanhanen"))

# ParlGov data ------------------------------------------------------------

pg_url <- "http://www.parlgov.org/static/data/experimental-cp1252/"

elec_raw <- read_csv(paste0(pg_url, "view_election.csv"))
elec_level_raw <- read_csv(paste0(pg_url, "election.csv"))

# combine data
elec <- elec_raw %>% 
  rename(id = election_id) %>% 
  left_join(elec_level_raw)

# data wrangling ----------------------------------------------------------

# generate Vanhanen Index (ID)
vanhanen_id <- elec %>% 
  group_by(country_name, country_name_short, election_date, id) %>% 
  summarise(
    vote_share_max = max(vote_share, na.rm = TRUE),
    electorate, 
    votes_cast,
    .groups = "drop"
  ) %>% 
  distinct() %>% 
  mutate(competition = 100 - vote_share_max,
         turnout = votes_cast / electorate * 100) %>% 
  # Vanhanen thresholds
  filter(competition > 30 & turnout > 10) %>%
  mutate(index = competition * turnout / 100,
         region = countrycode(country_name_short, "iso3c", "region23"))

# data visualization ------------------------------------------------------

# ggplot2 theme updates
theme_set(theme_linedraw())

theme_update(strip.background = element_blank(),
             strip.text = element_text(color = "black"),
             panel.grid.minor = element_blank())

# scatterplot
plot_1 <- ggplot(vanhanen_id, aes(x = election_date, y = index, color = region)) + 
  geom_point() +
  # geom_line() +
  # geom_step() +
  scale_color_npg()

plot_1  
