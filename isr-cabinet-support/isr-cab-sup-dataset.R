library(tidyverse)

## ParlGov data ----

# Cabinet data
cab_url <- "http://www.parlgov.org/static/data/experimental-cp1252/view_cabinet.csv"
cab_raw <- read_csv(cab_url)

# Cabinet support data
cab_sup_url <- "http://www.parlgov.org/static/data/experimental-utf-8/cabinet_support.csv"
cab_sup_raw <- read_csv(cab_sup_url)

# Merge datasets
cab <- left_join(cab_raw, cab_sup_raw)

## Data manipulation ----

# ISR subset
isr_cab <- cab %>% 
  filter(country_name == "Israel") %>% 
  rename(support_id = id) %>% 
  mutate(cab_sup = if_else(!is.na(support_id), # dummy for cabinet support parties
                           1,
                           0)) %>% 
  filter(cabinet_party == 1 | cab_sup == 1) %>% # just include cabinet or cabinet support parties
  select(country_name_short:left_right, cab_sup) # just keep relevant variables

# Save data
write_csv(isr_cab, "isr_cab_sub.csv")
