library(tidyverse)


create_zip_file <- TRUE # option to save csv-data in zip file

cab_raw <- read_csv("01-cabinet-dataset.csv")

coal_raw <- read_csv("02-cabinet-potential.zip")
# coal_raw <- coal_raw %>% slice(1:10000)  # TEMPORARY -- for testing script


## Cabinet information

cab <- cab_raw

cab_info_misc <-
  cab %>%
  distinct(cabinet_id, previous_cabinet_id, election_seats_total) %>%
  rename(parl_seats_total = election_seats_total)

cab_info_seats_max <-
  cab %>%
  group_by(cabinet_id) %>%
  summarise(parl_seats_max = max(seats, na.rm = TRUE))

cab_info_parties <-
  cab %>%
  filter(cabinet_party == 1) %>%
  select(cabinet_id, party_id) %>%
  nest(cab_parties = c(party_id))

cab_info_pres <-
  cab %>%
  filter(president == 1) %>%
  select(cabinet_id, president_party_id = party_id)

cab_info_median <-
  cab %>%
  group_by(cabinet_id) %>%
  mutate(median = median(rep(left_right, times = seats), na.rm = TRUE)) %>%
  filter(left_right == median) %>%
  distinct(cabinet_id, party_id) %>%
  nest(parl_median_party_id = c(party_id))

cab_info_prev <-
  cab %>%
  filter(prime_minister == 1) %>%
  select(cabinet_id, party_id) %>%
  left_join(cab_info_parties %>% na.omit()) %>%
  select(cabinet_id, prev_pm_party_id = party_id, prev_data = cab_parties)

cab_info <-
  cab_info_misc %>%
  left_join(cab_info_seats_max) %>%
  left_join(cab_info_parties) %>%
  left_join(cab_info_pres) %>%
  left_join(cab_info_median) %>%
  left_join(cab_info_prev, by = c("previous_cabinet_id" = "cabinet_id")) %>%
  select(
    starts_with("cab"),
    president_party_id,
    starts_with("prev"),
    starts_with("parl"),
    -previous_cabinet_id
  )

cab_pa_data <-
  cab %>%
  select(cabinet_id, party_id, seats, family, left_right) %>%
  distinct(cabinet_id, party_id, .keep_all = TRUE)


## Coalition data

coal_unnest <-
  coal_raw %>%
  rowid_to_column(var = "coalition") %>%
  mutate(party_id = strsplit(coalition_parties, " ")) %>%
  unnest(party_id) %>%
  mutate(party_id = as.integer(party_id)) %>%
  select(-coalition_parties) %>%
  left_join(cab_pa_data)

coal_nest <-
  coal_unnest %>%
  nest(data = c(-cabinet_id, -coalition)) %>%
  left_join(cab_info)


na_rm <- function(fun, .x) fun(.x, na.rm = TRUE) # helper function

coal_vars <-
  coal_nest %>%
  mutate(
    # helper variables
    seats = map_int(data, ~ na_rm(sum, .$seats) %>% as.integer()),
    seats_max = map_int(data, ~ na_rm(max, .$seats) %>% as.integer()),
    seats_min = map_int(data, ~ na_rm(min, .$seats) %>% as.integer()),
    # dataset variables
    coalition_parties = map_chr(data, ~ paste(.$party_id, collapse = " ")),
    formed = map2_int(data, cab_parties, ~ setequal(.x$party_id, .y$party_id)),
    minority = (seats <= parl_seats_total / 2) %>% as.integer(),
    min_win = NA, # to keep order of variables
    surplus = (seats - seats_min > parl_seats_total / 2) %>% as.integer(),
    min_win = ifelse(minority + surplus, 0, 1),
    incumbent = map2_int(data, prev_data, ~ setequal(.x$party_id, .y$party_id)),
    incumbent_pm = map2_int(prev_pm_party_id, data, ~ .x %in% .y$party_id),
    seats_share = (100 * seats / parl_seats_total) %>% round(1),
    n = map_int(data, nrow),
    enp = map2_dbl(data, seats, ~ round(1 / sum((.x$seats / .y)^2), 1)),
    largest = (seats_max == parl_seats_max) %>% as.integer(),
    president = map2_int(president_party_id, data, ~ any(.x %in% .y$party_id)),
    median = map2_int(parl_median_party_id, data, ~ any(.x$party_id %in% .y$party_id)),
    lr_range = map_dbl(data, ~ ifelse(
      all(is.na(.$left_right)),
      NA,
      round(na_rm(max, .$left_right) - na_rm(min, .$left_right), 1)
    )),
    anti_sytem = map_int(data, ~ all(c("com", "right") %in% .$family))
  )

coal_out <- coal_vars %>% select(cabinet_id, coalition_parties:anti_sytem)


name_file <- "03-cabinet-formation.csv"
write_csv(coal_out, name_file)


## Zip results and remove csv file

# Windows usage -- http://stackoverflow.com/questions/29129681/create-zip-file-error-running-command-had-status-127

if (create_zip_file) {
  zip_file <- stringr::str_replace(name_file, "csv", "zip")
  if (file.exists(zip_file)) file.remove(zip_file)
  zip(zip_file, name_file)
  file.remove(name_file)
}
