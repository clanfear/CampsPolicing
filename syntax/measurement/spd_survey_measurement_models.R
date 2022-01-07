library(tidyverse)
library(lme4)
source("./syntax/project_functions.R")
load("./data/derived/disaggregated/spd_survey.RData")

police_efficacy_df <- spd_survey %>%
  select(unique_id,
         q6_1_dept_available,
         q6_5_dept_focuses_concerns,
         q6_6_dept_understands_issues,
         q7_1_neighbpolice_overall,
         q7_3_neighbpolice_good_resource,
         q7_5_neighbpolice_parks,
         q7_6_neighbpolice_focuses_concerns
  ) %>% 
  mutate_at(vars(-unique_id), ~ ifelse(. %in% c(7,9), NA, .)) %>%
  mutate_at(vars(-unique_id), ~ standardize(.)) %>%
  pivot_longer(-unique_id, names_to = "measure", values_to = "value") %>%
  group_by(unique_id) %>%
  filter(sum(!is.na(value)) > 3) %>%
  summarize(police_efficacy = mean(value, na.rm=TRUE))

fear_of_crime_df <- spd_survey %>%
  select(unique_id,
         q12_fear_overall,
         q16_fear_neighb_walk_day,
         q17_fear_neighb_walk_night) %>% 
  mutate_at(vars(-unique_id), ~ ifelse(. %in% c(7,9), NA, .)) %>%
  mutate_at(vars(-unique_id), ~ standardize(.)) %>%
  pivot_longer(-unique_id, names_to = "measure", values_to = "value") %>%
  group_by(unique_id) %>%
  filter(sum(!is.na(value)) > 1) %>%
  summarize(fear_of_crime = mean(-value, na.rm=TRUE)) # reverse values to get fear instead of safety scale

beat_year_pe_fear <- spd_cs_survey %>%
  select(unique_id, beat, year) %>%
  inner_join(police_efficacy_df) %>%
  inner_join(fear_of_crime_df) %>%
  group_by(beat, year) %>%
  summarize(n=n(), police_efficacy = mean(police_efficacy), fear_of_crime = mean(fear_of_crime)) %>% 
  filter(n > 2) 

beat_2018_pe_fear <- beat_year_pe_fear %>%
  filter(year == 2018) %>% select(-year, -n)

beat_2019_pe_fear <- beat_year_pe_fear %>%
  filter(year == 2019) %>% select(-year, -n)

save(beat_2019_pe_fear, file = "./data/derived/beat_2018_pe_fear.RData")
save(beat_2018_pe_fear, file = "./data/derived/beat_2018_pe_fear.RData")
save(beat_year_pe_fear, file = "./data/derived/beat_year_pe_fear.RData")



spd_survey %>%
  select(unique_id,
         beat,
         year,
         q12_fear_overall,
         q16_fear_neighb_walk_day,
         q17_fear_neighb_walk_night) %>% 
  filter(year == 2019) %>%
  mutate(across(matches("fear"), ~ ifelse(. %in% c(7,9), NA, .))) %>%
  mutate(across(matches("fear"), ~ standardize(.))) %>%
  pivot_longer(matches("fear"), names_to = "measure", values_to = "value") %>%
  lme4::lmer(value ~ measure + (1|beat/unique_id), data = .) %>% summary()

spd_survey %>%
  select(unique_id,
         beat,
         year,
         q6_1_dept_available,
         q6_5_dept_focuses_concerns,
         q6_6_dept_understands_issues,
         q7_1_neighbpolice_overall,
         q7_3_neighbpolice_good_resource,
         q7_5_neighbpolice_parks,
         q7_6_neighbpolice_focuses_concerns) %>% 
  filter(year == 2019) %>%
  mutate(across(matches("^q"), ~ ifelse(. %in% c(7,9), NA, .))) %>%
  mutate(across(matches("^q"), ~ standardize(.))) %>%
  pivot_longer(matches("^q"), names_to = "measure", values_to = "value") %>%
  lme4::lmer(value ~ measure + (1|beat/unique_id), data = .) %>% summary()