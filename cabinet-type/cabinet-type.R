library(tidyverse)
library(lubridate)

# read ParlGov cabinet table
cab_raw <- read_csv("view_cabinet.csv")


## Cabinet types -- minority, minimal winning, surplus

# seats strength -- parliament, cabinet, minimum and maximum
cab_seats <-
  cab_raw %>%
  filter(cabinet_party == 1) %>%  # keep cabinet parties only
  group_by(cabinet_id) %>%
  summarise(
    election_seats = first(election_seats_total),
    cabinet_seats = sum(seats, na.rm = TRUE),
    seats_min = min(seats, na.rm = TRUE),
    seats_max = max(seats, na.rm = TRUE)
  )

# cabinet type -- determine based on cabinet seats
cab_type <-
  cab_seats %>%
  mutate(
    cabinet_type = case_when(
      cabinet_seats <= election_seats / 2 ~ "minority",
      cabinet_seats - seats_min > election_seats / 2 ~ "surplus",
      TRUE ~ "min_win"
    )
  ) %>%
  select(cabinet_id, cabinet_type)

# grand coalition -- determine by two largest parties
cab_gc <-
  cab_raw %>%
  group_by(cabinet_id) %>%
  mutate(seats_rank = min_rank(desc(seats))) %>%
  filter(seats_rank %in% c(1, 2)) %>%
  summarise(grand_coalition = sum(cabinet_party, na.rm = TRUE)) %>%
  mutate(grand_coalition = if_else(grand_coalition >= 2, 1, 0))


## Determine duration of cabinet

cab_info <-
  cab_raw %>%
  distinct(cabinet_id, .keep_all = TRUE) %>%
  select(country_name_short:caretaker,
         cabinet_id,
         previous_cabinet_id)

cab_end <-
  cab_info %>% select(cabinet_id = previous_cabinet_id, end_date = start_date)

cab_info <-
  cab_info %>%
  left_join(cab_end) %>%
  mutate(
    start_date = ymd(cab_info$start_date),
    end_date = if_else(is.na(end_date), today(), ymd(end_date)),
    duration = end_date - start_date
  ) %>%
  select(-previous_cabinet_id, -end_date)


## Merge all data and save output

cab_out <-
  cab_info %>%
  left_join(cab_type) %>%
  left_join(cab_gc) %>%
  arrange(country_name_short, start_date)

write_csv(cab_out, "cabinet-type.csv", na = "")
