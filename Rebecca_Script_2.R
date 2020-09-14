# Lab 3B
# Rebecca Sebastian  
# Manipulating Data

library(tidyverse)
library(readr)

surveys <- read_csv("EvoGenomics-course/data/portal_data_joined.csv")

## Challenge Answers (pipes)
surveys %>% 
  filter (year<1995) %>% 
  select (year,sex,weight)

## Challenge Answers (pipes and more)

surveys_new <- surveys %>% 
  mutate(hindfoot_cm = hindfoot_length/10) %>% 
  filter(hindfoot_cm < 3, !is.na(hindfoot_cm)) %>% 
  select(species_id, hindfoot_cm)

## Challenge Answers (counts)

surveys %>% 
  count(plot_type)

surveys %>% 
  group_by(plot_type) %>% 
  summarise(count = n())

surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
  summarise(mean_height = mean(hindfoot_length),
            min_height = min(hindfoot_length),
            max_height = max(hindfoot_length),
            observations = n())

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>%
  select(year,genus, species, weight)

## Challenge Answers (spread function and more)
surveys_spread_g <- surveys %>%
  group_by(plot_id, year) %>%
  summarize(genera_n = n_distinct(genus)) %>%
  spread(year, genera_n)

surveys_spread_g %>% 
  gather("year","genera_n",-plot_id)

surveys_new_2 <- surveys %>% 
  gather("measurement", "value", hindfoot_length, weight)

surveys_new_2 %>%
  group_by(year, measurement, plot_type) %>%
  summarize(mean_value = mean(value, na.rm=TRUE)) %>%
  spread(measurement, mean_value)
