library(tidyverse)
library(countrycode) # regions
library(lubridate) # date manipulation
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
         # Regions from World Bank Development Indicators 
         region = countrycode(country_name_short, "iso3c", "region23"),
         year = lubridate::year(election_date)) 

vanhanen_id_mean <- vanhanen_id %>% 
  group_by(region, year) %>% 
  mutate(index_mean = mean(index, na.rm = TRUE)) 

# data visualization ------------------------------------------------------

# ggplot2 theme updates
theme_set(theme_linedraw())

theme_update(strip.background = element_blank(),
             strip.text = element_text(color = "black"),
             panel.grid.minor = element_blank(),
             legend.position = "top")

# plot all
plot_all <- ggplot(vanhanen_id, aes(x = year, y = index)) + 
  geom_point(alpha = 0.5, aes(color = region)) + 
  geom_line(size = 1, alpha = 0.5, stat = "smooth") +
  scale_color_nejm(name = "") +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) + 
  guides(fill = guide_legend(ncol = 4)) + 
  labs(title = "Vanhanen Index of Democratization (ID)",
       x = "Year",
       y = "Index Value") +
  theme(text = element_text(family = "Helvetica Light", size = 12),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# print plot
plot_all

# saving plot
ggsave("vanhanen_plot_all.png",
       plot = plot_all,
       width = 8,
       height = 4)

plot_grp <- ggplot(vanhanen_id, aes(x = year, color = region)) +
  geom_point(aes(y = index), alpha = 0.5, show.legend = FALSE) + 
  geom_smooth(aes(y = index), se = FALSE) +
  scale_color_nejm(name = "") +
  scale_x_continuous(breaks = seq(1900, 2020, 20)) + 
  lemon::facet_rep_wrap(~ region, ncol = 4, repeat.tick.labels = "all") +
  guides(fill = guide_legend(ncol = 4)) + 
  labs(title = "Vanhanen Index of Democratization (ID)",
       x = "Year",
       y = "Index Value",
       caption = "ID is computed as grouped mean (region and year).") +
  theme(text = element_text(family = "Helvetica Light", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# print plot
plot_grp

# saving plot
ggsave("vanhanen_plot_grouped.png",
       plot = plot_grp,
       width = 12,
       height = 6)
