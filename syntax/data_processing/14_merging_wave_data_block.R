library(tidyverse)
library(sf)
library(lubridate)
source("./syntax/project_functions.R")

# Cross-sectional summer 2019

# Four-wave subset in sampled zones; predict crime / complaints at three points
# Tracts with at least 1 tent in either resample

load("./data/derived/block/tent_census_summer_2019_block.RData")
load("./data/derived/block/tent_census_autumn_2019_block.RData")
load("./data/derived/block/tent_census_summer_2020_block.RData")
load("./data/derived/block/spd_public_block_month.RData")
load("./data/derived/block/unauthorized_camping_complaints_monthly_block.RData")
load("./data/derived/block/sweeps_monthly_block.RData")
load("./data/derived/block/seattle_block_boundaries.RData")
load("./data/derived/block/seattle_block_neighbors.RData")
load("./data/derived/bg/seattle_bg_neighbors.RData")
load("./data/derived/tract/seattle_tract_neighbors.RData")
load("./data/derived/block/all_resampled_blocks.RData")

sum_na <- function(x){
  if(all(is.na(x))){
    return(NA)
  } else {
    return(sum(x, na.rm=TRUE))
  }
}

tent_wave_block <- bind_rows(
  tent_census_summer_2019_block %>%
    mutate(wave = 1),
  tent_census_autumn_2019_block %>% 
    mutate(wave = 2),
  tent_census_summer_2020_block %>% 
    mutate(wave = 3)) %>%
  complete(block, wave, fill = list(n_dwellings = 0)) %>%
  inner_join(all_resampled_blocks %>% 
               st_drop_geometry()) %>%
  filter(type == "sampled") %>%
  select(-type) %>%
  complete(block, wave=0:4, fill = list(n_dwellings = NA))

## WAVE 1: 4-April-2019 to 23-Aug-2019
## WAVE 2: 14-October-2019 to 14-Dec-2019
## WAVE 3: 5-April-2020 to 30-July-2020

wave_data_block_nosplag <- spd_public_block_month %>%
  full_join(unauthorized_camping_complaints_monthly_block) %>%
  full_join(sweeps_monthly_block) %>%
  mutate(wave = case_when(
    date >= ymd("2018-10-01") & date < ymd("2019-01-01") ~ 0,
    date >= ymd("2019-04-01") & date < ymd("2019-08-01") ~ 1,
    date >= ymd("2019-10-01") & date < ymd("2020-01-01") ~ 2,
    date >= ymd("2020-04-01") & date < ymd("2020-08-01") ~ 3,
    date >= ymd("2020-10-01") & date < ymd("2021-01-01") ~ 4,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(wave)) %>%
  group_by(block, wave) %>%
  summarize(across(-date, ~sum_na(.)), .groups = "drop") %>%
  right_join(tent_wave_block)  %>%
  mutate(tract = str_sub(block, 1, -5),
         blockgroup = str_sub(block, 1, -4))


wave_data_bg_nosplag <- wave_data_block_nosplag %>%
  select(-block, -tract) %>%
  group_by(blockgroup, wave) %>%
  summarize(across(everything(), ~sum(.)), .groups = "drop")
  

wave_data_tract_nosplag <- wave_data_block_nosplag %>%
  select(-block, -blockgroup) %>%
  group_by(tract, wave) %>%
  summarize(across(everything(), ~sum(.)), .groups = "drop")


wave_data_block_splag <- seattle_block_neighbors %>%
  inner_join(wave_data_block_nosplag %>% 
               rename(neighbors = block)) %>%
  group_by(block, wave) %>%
  summarize(across(matches("burglary|gta|property|violent|complaints|sweeps|n_dwellings"), ~ mean(., na.rm = TRUE), 
                   .names = "splag_{.col}"), .groups = "drop")

wave_data_bg_splag <- seattle_bg_neighbors %>%
  inner_join(wave_data_bg_nosplag %>% 
               rename(neighbors = blockgroup)) %>%
  group_by(blockgroup, wave) %>%
  summarize(across(matches("burglary|gta|property|violent|complaints|sweeps|n_dwellings"), ~ mean(., na.rm = TRUE), 
                   .names = "splag_{.col}"), .groups = "drop")

wave_data_tract_splag <- seattle_tract_neighbors %>%
  inner_join(wave_data_tract_nosplag %>% 
               rename(neighbors = tract)) %>%
  group_by(tract, wave) %>%
  summarize(across(matches("burglary|gta|property|violent|complaints|sweeps|n_dwellings"), ~ mean(., na.rm = TRUE), 
                   .names = "splag_{.col}"), .groups = "drop")

wave_data_block <- wave_data_block_nosplag |> 
  full_join(wave_data_block_splag)

wave_data_bg <- wave_data_bg_nosplag |> 
  full_join(wave_data_bg_splag)

wave_data_tract <- wave_data_tract_nosplag |> 
  full_join(wave_data_tract_splag)

save(wave_data_block, file = "./data/derived/analysis/wave_data_block.RData")
save(wave_data_bg, file = "./data/derived/analysis/wave_data_bg.RData")
save(wave_data_tract, file = "./data/derived/analysis/wave_data_tract.RData")
