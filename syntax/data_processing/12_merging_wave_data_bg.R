library(tidyverse)
library(sf)
library(lubridate)
source("./syntax/project_functions.R")

# Cross-sectional summer 2019

# Four-wave subset in sampled zones; predict crime / complaints at three points
# Tracts with at least 1 tent in either resample

load("./data/derived/block/all_resampled_blocks.RData")
load("./data/derived/bg/tent_census_summer_2019_bg.RData")
load("./data/derived/bg/tent_census_autumn_2019_bg.RData")
load("./data/derived/bg/tent_census_summer_2020_bg.RData")
load("./data/derived/bg/acs5_bg_2019.RData")
load("./data/derived/bg/spd_public_bg_month.RData")
load("./data/derived/bg/unauthorized_camping_complaints_monthly_bg.RData")
load("./data/derived/bg/sweeps_monthly_bg.RData")
load("./data/derived/bg/seattle_bg_boundaries.RData")
load("./data/derived/bg/seattle_bg_neighbors.RData")

sum_na <- function(x){
  if(all(is.na(x))){
    return(NA)
  } else {
    return(sum(x, na.rm=TRUE))
  }
}

resample_bg <- tent_census_summer_2020_bg %>% 
  filter(n_dwellings >=1) %>%
  mutate(wave = 3) %>%
  bind_rows(tent_census_autumn_2019_bg %>% 
              filter(n_dwellings >=1) %>%
              mutate(wave = 2)) %>%
  complete(blockgroup, wave, fill = list(n_tents = 0, n_structures = 0, n_dwellings = 0)) %>%
  mutate(resampled = 1)

tent_panel_bg <-  
  bind_rows(resample_bg, 
    tent_census_summer_2019_bg %>%
    mutate(wave = 1)) %>%
  complete(blockgroup, wave=1:4, fill = list(n_tents = NA, n_structures = NA, n_dwellings = NA, resampled = 0)) %>%
  group_by(blockgroup) %>%
  mutate(resampled = as.numeric(any(resampled == 1)))

## WAVE 1: 4-April-2019 to 23-Aug-2019
## WAVE 2: 14-October-2019 to 14-Dec-2019
## WAVE 3: 5-April-2020 to 30-July-2020

wave_data_bg_nosplag <- spd_public_bg_month %>%
  full_join(unauthorized_camping_complaints_monthly_bg) %>%
  full_join(sweeps_monthly_bg) %>%
  mutate(wave = case_when(
    date >= ymd("2018-10-01") & date < ymd("2019-01-01") ~ 0,
    date >= ymd("2019-04-01") & date < ymd("2019-08-01") ~ 1,
    date >= ymd("2019-10-01") & date < ymd("2020-01-01") ~ 2,
    date >= ymd("2020-04-01") & date < ymd("2020-08-01") ~ 3,
    date >= ymd("2020-10-01") & date < ymd("2021-01-01") ~ 4,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(wave)) %>%
  group_by(blockgroup, wave) %>%
  summarize(across(-date, ~sum_na(.)), .groups = "drop") %>%
  full_join(tent_panel_bg)  %>%
  group_by(blockgroup) %>%
  mutate(tract = str_sub(blockgroup, 1, -2)) %>%
  mutate(resampled = ifelse(wave == 0, dplyr::lead(resampled), resampled)) %>% 
  ungroup() %>%
  left_join(acs5_bg_2019)

wave_data_bg_splag <- seattle_bg_neighbors %>%
  inner_join(wave_data_bg_nosplag %>% rename(neighbors = blockgroup)) %>%
  group_by(blockgroup, wave) %>%
  summarize(across(matches("burglary|gta|property|violent|complaints|sweeps|n_dwellings|disadv|pop_sqkm|pr_ownhome"), ~ mean(., na.rm = TRUE), .names = "splag_{.col}"), .groups = "drop")

wave_data_bg <- wave_data_bg_nosplag |> 
  full_join(wave_data_bg_splag)

save(wave_data_bg, file = "./data/derived/analysis/wave_data_bg.RData")