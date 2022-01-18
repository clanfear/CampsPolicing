library(tidyverse)
library(sf)
library(lubridate)
source("./syntax/project_functions.R")

# Cross-sectional summer 2019

# Four-wave subset in sampled zones; predict crime / complaints at three points
# Tracts with at least 1 tent in either resample

load("./data/derived/tract/tent_census_summer_2019_tract.RData")
load("./data/derived/tract/tent_census_autumn_2019_tract.RData")
load("./data/derived/tract/tent_census_summer_2020_tract.RData")
load("./data/derived/tract/acs5_tract_2019.RData")
load("./data/derived/tract/spd_public_tract_month.RData")
load("./data/derived/tract/unauthorized_camping_complaints_monthly_tract.RData")
load("./data/derived/tract/sweeps_monthly_tract.RData")
load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/tract/seattle_tract_neighbors.RData")

sum_na <- function(x){
  if(all(is.na(x))){
    return(NA)
  } else {
    return(sum(x, na.rm=TRUE))
  }
}

resample_tract <- tent_census_summer_2020_tract %>% 
  filter(n_dwellings >=1) %>%
  mutate(wave = 3) %>%
  bind_rows(tent_census_autumn_2019_tract %>% 
              filter(n_dwellings >=1) %>%
              mutate(wave = 2)) %>%
  complete(tract, wave, fill = list(n_tents = 0, n_structures = 0, n_dwellings = 0)) %>%
  mutate(resampled = 1)

tent_panel_tract <-  
  bind_rows(resample_tract, 
            tent_census_summer_2019_tract %>%
              mutate(wave = 1)) %>%
  complete(tract, wave=1:4, fill = list(n_tents = NA, n_structures = NA, n_dwellings = NA, resampled = 0)) %>%
  group_by(tract) %>%
  mutate(resampled = as.numeric(any(resampled == 1)))

## WAVE 1: 4-April-2019 to 23-Aug-2019
## WAVE 2: 14-October-2019 to 14-Dec-2019
## WAVE 3: 5-April-2020 to 30-July-2020

wave_data_tract_nosplag <- spd_public_tract_month %>%
  full_join(unauthorized_camping_complaints_monthly_tract) %>%
  full_join(sweeps_monthly_tract) %>%
  mutate(wave = case_when(
    date >= ymd("2018-10-01") & date < ymd("2019-01-01") ~ 0,
    date >= ymd("2019-04-01") & date < ymd("2019-08-01") ~ 1,
    date >= ymd("2019-10-01") & date < ymd("2020-01-01") ~ 2,
    date >= ymd("2020-04-01") & date < ymd("2020-08-01") ~ 3,
    date >= ymd("2020-10-01") & date < ymd("2021-01-01") ~ 4,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(wave)) %>%
  group_by(tract, wave) %>%
  summarize(across(-date, ~sum_na(.)), .groups = "drop") %>%
  full_join(tent_panel_tract)  %>%
  group_by(tract) %>%
  mutate(tract = str_sub(tract, 1, -2)) %>%
  mutate(resampled = ifelse(wave == 0, lead(resampled), resampled)) %>% 
  ungroup() %>%
  left_join(acs5_tract_2019)

wave_data_tract_splag <- seattle_tract_neighbors %>%
  inner_join(wave_data_tract_nosplag %>% rename(neighbors = tract)) %>%
  group_by(tract, wave) %>%
  summarize(across(matches("burglary|gta|property|violent|complaints|sweeps|n_dwellings|disadv|pop_sqkm|pr_ownhome"), ~ mean(., na.rm = TRUE), .names = "splag_{.col}"), .groups = "drop")

wave_data_tract <- wave_data_tract_nosplag |> 
  full_join(wave_data_tract_splag)

save(wave_data_tract, file = "./data/derived/analysis/wave_data_tract.RData")
