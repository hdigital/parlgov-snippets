library(tidyverse)
library(magrittr)
library(ggrepel)
library(ggpubr)
library(gridExtra)

## ParlGov data ----

# Cabinet data
cab_url <- "http://www.parlgov.org/static/data/experimental-cp1252/view_cabinet.csv"
cab_raw <- read_csv(cab_url)

# Cabinet support data
cab_sup_url <- "http://www.parlgov.org/static/data/experimental-utf-8/cabinet_support.csv"
cab_sup_raw <- read_csv(cab_sup_url)

# Merge datasets
cab <- left_join(cab_raw, cab_sup_raw)

## Data wrangling ---- 

# ISR subset
isr <- cab %>% 
  filter(country_name == "Israel") %>% 
  rename(support_id = id) %>% 
  mutate(cab_sup = if_else(!is.na(support_id),
                           1,
                           0)) %>% 
  filter(cabinet_party == 1 | cab_sup == 1)

# Further data manipulation
isr <- isr %>% 
  mutate(year = as.numeric(substr(start_date, 1, 4))) %>% 
  group_by(cabinet_name, cab_sup) %>% 
  mutate(cab_seats = sum(seats)) %>% # total seats of cabinet without cabinet support
  group_by(cabinet_name) %>% 
  mutate(cab_seats_sup = sum(seats), # total seats of cabinet with cabinet support
         cab_seats = if_else(cab_seats == min(cab_seats),
                             max(cab_seats),
                             cab_seats),
         cab_sup_all = as_factor(if_else(cab_seats != cab_seats_sup,
         1,
         0))) %>% 
  # mutate(seat_share_cab = seats / sum(seats), # seat share of cabinet party -- related to the maximum of cabinet seats (one party government ~ 100%)
  #        cab_mean_lr = sum(seat_share_cab * left_right, na.rm = TRUE), # weighted mean of lr position related to cabinet seat share
  #        nr_cab_parties = sum(n()), # number of parties of the cabinet
  #        cab_min_lr = min(left_right, na.rm = TRUE), # lowest lr value of cabinet party
  #        cab_max_lr = max(left_right, na.rm = TRUE) # highest lr value of cabinet party
  # ) %>% 
  ungroup() %>% 
  mutate(seat_share = seats / election_seats_total,
         pm_name = sapply(str_split(cabinet_name, " "), extract, 1), # name of PM
         end_date = ifelse(cabinet_name != lead(cabinet_name), # end date of cabinet
                           lead(start_date), 
                           as.Date(NA))
  ) %>% 
  group_by(cabinet_name) %>%
  mutate(end_date = ifelse(is.na(end_date), 
                           max(end_date, na.rm = TRUE), 
                           end_date)
  ) %>% 
  mutate(end_date = ifelse(is.infinite(end_date),
                           as.Date(Sys.Date()),
                           end_date), 
         end_date = as.Date(end_date, origin = "1970-01-01"),
         duration = end_date - start_date) %>% 
  ungroup()

isr_sup <- isr %>% 
  filter(cab_sup == 1) %>% 
  mutate(party_name = case_when(
    party_name_english == "Spiritual Centre" ~ "HaMizrahi - Merkaz Ruhani",
    TRUE ~ party_name
    ),
    cabinet_name_year = paste0(cabinet_name, " (", year, ")")
  ) %>% 
  mutate(party_name_pl = paste0(party_name_english, 
                                " |\n",
                                party_name)) %>% 
  group_by(cabinet_name) %>% 
  mutate(cab_sup_nr = n())

isr_dist <- isr %>% 
  group_by(cabinet_name) %>% 
  distinct(cabinet_name, cab_seats, cab_seats_sup, duration, cab_sup_all)

## Visualization 1 -- Histogram of cabinet support ----

plot_1 <- ggplot(isr_sup, aes(x = fct_infreq(party_name_pl), fill = pm_name)) +
  geom_histogram(stat = "count") + 
  coord_flip() + 
  theme_light() +
  theme(text = element_text(family = "Helvetica Light", size = 12),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.title = element_blank()) +
  labs(title = "Israeli Cabinet Support Parties",
       subtitle = "Histogram grouped by PM",
       x = "",
       y = "Number of Cabinets Supported") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(breaks = seq(1, 20, 1)) 
  
print(plot_1)
  
# save plot 1
ggsave(
  file = "isr_cab_sup_histogram.png",
  plot = plot_1,
  height = 8,
  width = 12
)

## Visualization 2 -- Ideological position of cabinet support ----

plot_2 <- ggplot(subset(isr_sup, !is.na(left_right)), aes(x = left_right, 
                             y = fct_reorder(cabinet_name_year, start_date, max),
                             label = paste0(party_name_english, 
                                            " | ",
                                            round(seat_share * 100, 1),
                                            "%")
                             )) +
  geom_point(aes(size = seat_share)) +
  geom_label_repel(hjust = -0.5, size = 2.5, force = 4, family = "Helvetica") +
  theme_light() +
  theme(text = element_text(family = "Helvetica Light", size = 12),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.position = "none") +
  labs(title = "Israeli Cabinet Support Parties",
       subtitle = "Grouped by Cabinet and L/R-Value",
       x = "Left-Right Value",
       y = "",
       caption = "Seat share is indicated at the party label") +
  scale_x_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) 

print(plot_2)

# save plot 2
ggsave(
  file = "isr_cab_sup_lr.png",
  plot = plot_2,
  height = 8,
  width = 12
)

## Visualization 3 -- Relationship between seat share and cabinet duration ----

# Plot a
plot_3a <- ggplot(subset(isr_dist, cab_sup_all == 0), 
                  aes(x = cab_seats_sup, y = duration, label = cabinet_name)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text_repel(force = 3, size = 3, segment.size = 0.25, family = "Helvetica") +
  stat_cor(aes(label = ..r.label.., family = "Helvetica"), 
           label.x = 6, label.y = 300, size = 6) +
  theme_light() +
  theme(text = element_text(family = "Helvetica Light", size = 12),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.position = "none") +
  labs(title = "Relationship between Cabinet Duration and Absolute Seat Share",
       subtitle = "Cabinets without Support Parties",
       x = "Number of Cabinet Seats",
       y = "Cabinet Duration (Days)") +
  scale_x_continuous(breaks = seq(0, 110, 10), limits = c(0, 110)) +
  scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(0, 1400))
  
print(plot_3a)

# Plot b
plot_3b <- ggplot(subset(isr_dist, cab_sup_all == 1), 
                  aes(x = cab_seats_sup, y = duration, label = cabinet_name)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text_repel(force = 3, size = 3, segment.size = 0.25, family = "Helvetica") +
  stat_cor(aes(label = ..r.label.., family = "Helvetica"), 
           label.x = 24, label.y = 300, size = 6) +
  theme_light() +
  theme(text = element_text(family = "Helvetica Light", size = 12),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.position = "none") +
  labs(title = "Relationship between Cabinet Duration and Absolute Seat Share",
       subtitle = "Cabinets with Support Parties",
       x = "Number of Cabinet Seats",
       y = "Cabinet Duration (Days)") +
  scale_x_continuous(breaks = seq(0, 110, 10), limits = c(0, 110)) +
  scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(0, 1400))

print(plot_3b)

# combine plots
plot_3 <- grid.arrange(plot_3a, plot_3b, nrow = 1)

print(plot_3)

# save plot 3
ggsave(
  file = "isr_cab_sup_corr.png",
  plot = plot_3,
  height = 7,
  width = 21
)



