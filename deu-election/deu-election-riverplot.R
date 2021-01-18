library(tidyverse)
library(partycoloR)
library(ggalluvial)
library(scales)

# install partycoloR package
# devtools::install_github("lwarode/partycoloR")


# ParlGov data ------------------------------------------------------------

# Election data
elec_url <- "http://www.parlgov.org/static/data/experimental-cp1252/view_election.csv"
elec_raw <- read_csv(elec_url)

# DEU subset
elec_deu <- elec_raw %>% 
  filter(country_name == "Germany")



# Party Facts data --------------------------------------------------------

# check if merge csv file is in folder
file_name <- "pf_pg_deu_merge.csv"

if (! file_name %in% list.files(paste0(here::here(), "/deu-election"))) {
  
  # load party facts data for merge
  pf_url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  pf_raw <- read_csv(pf_url)
  
  # subset with ParlGov data
  pf_pg_deu <- pf_raw %>% 
    filter(dataset_key == "parlgov", country == "DEU") %>% 
    rename(party_id = dataset_party_id) %>% 
    select(party_id, partyfacts_id)
  
  # write csv file
  write_csv(pf_pg_deu, paste0(here::here(), "/deu-election/", file_name))
  
}

# read merge file
pf_pg_deu <- read_csv(paste0(here::here(), "/deu-election/", file_name))



# partycoloR --------------------------------------------------------------

# URL list of Wikipedia party articles
wikipedia_raw <- read_csv("https://raw.githubusercontent.com/hdigital/partyfactsdata/master/import/wikipedia/wikipedia.csv")

# DEU subset
wikipedia_de <- wikipedia_raw %>%
  filter(country == "DEU")

# URL list
wikipedia_de_url_list <- as.list(wikipedia_de$url)

# applying partycoloR::wikipedia_party_color()
party_de_color <- wikipedia_party_color(wikipedia_de_url_list)

# wrangling data
party_de_color_link <- wikipedia_de %>%
  left_join(party_de_color, by = "url") %>%
  filter(! is.na(color_1)) %>%
  # switching first color
  mutate(color_1 = case_when(
    name_short %in% c("CDU", "DIE/LINKE") ~ color_2,
    name_short == "NPD" ~ color_4,
    TRUE ~ color_1)
  ) %>% 
  right_join(pf_pg_deu, by = "partyfacts_id") %>% 
  left_join(elec_deu, by = "party_id") %>% 
  select(party_id, party_name_short, color_1) %>% 
  distinct(party_id, .keep_all = TRUE)

# named character with color codes
party_de_color <- party_de_color_link %>% 
  na.omit(color_1) %>% 
  pull(color_1) 
  
names(party_de_color) <- party_de_color_link %>% 
  na.omit(color_1) %>% 
  pull(party_name_short)

print(party_de_color)



# data visualization ------------------------------------------------------

# ggplot2 theme updates
theme_set(theme_linedraw())

theme_update(strip.background = element_blank(),
             strip.text = element_text(color = "black"),
             panel.grid.minor = element_blank())

# facet labels
facet_names <- c("European Parliament Election", "National Parliament Election")
names(facet_names) <- c("ep", "parliament")

# riverplot DEU election vote share
riverplot_de_parl <- elec_deu %>% 
  # final data manipulation
  filter(! is.na(vote_share),
         party_name_short %in% names(party_de_color),
         party_name_short != "B90/Gr",
         election_date >= "1949-08-14") %>%
  mutate(vote_share = vote_share / 100) %>% 
  # ggplot2 part
  ggplot() + 
  geom_alluvium(aes(x = election_date, 
                    y = vote_share, 
                    alluvium = party_name_short, 
                    fill = party_name_short),
                decreasing = FALSE,
                alpha = 0.75) +
  # geom_stratum(aes(x = election_date, 
  #                  y = vote_share, 
  #                  alluvium = party_name_short,
  #                  stratum = party_name_short),
  #              decreasing = FALSE,
  #              alpha = 0.25,
  #              width = 0.25) +
  facet_wrap(~ election_type, 
             labeller = labeller(election_type = facet_names),
             scales = "free") +
  scale_fill_manual(values = party_de_color, 
                    name = "") + 
  # scale_x_date(breaks = elec_deu$election_date) + doesn't work with facetting syntax
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     labels = scales::label_percent(1)) + 
  guides(fill = guide_legend(ncol = 6)) + 
  labs(title = "German Party Vote Share",
       subtitle = "Riverplot",
       x = "",
       y = "", 
       caption = "CDU and CSU are listed as separate parties with individual election results") +
  theme(text = element_text(family = "Helvetica Light", size = 12),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.position = "top")

# print plot  
riverplot_de_parl

# saving plot
ggsave(paste0(here::here(), "/deu-election/riverplot_de.png"),
       plot = riverplot_de_parl,
       width = 12,
       height = 6)


