# Tract-months
library(tidyverse)
library(sf)
library(lubridate)
source("./syntax/project_functions.R")

# BG
load("./data/derived/bg/tent_census_summer_2019_bg.RData")
load("./data/derived/bg/sweeps_monthly_bg.RData")
load("./data/derived/bg/acs5_bg_2019.RData")
load("./data/derived/bg/spd_public_bg_month.RData")
load("./data/derived/bg/unauthorized_camping_complaints_monthly_bg.RData")
load("./data/derived/bg/seattle_bg_neighbors.RData")
load("./data/derived/bg/streets_bg.RData")

load("./data/derived/tract/tent_census_summer_2019_tract.RData")
load("./data/derived/tract/sweeps_monthly_tract.RData")
load("./data/derived/tract/acs5_tract_2019.RData")
load("./data/derived/tract/spd_public_tract_month.RData")
load("./data/derived/tract/unauthorized_camping_complaints_monthly_tract.RData")
load("./data/derived/tract/seattle_tract_neighbors.RData")
load("./data/derived/tract/streets_tract.RData")

comp_prop_panel_bg_month <- spd_public_bg_month %>%
  full_join(unauthorized_camping_complaints_monthly_bg) %>%
  filter(!is.na(complaints) & date >= ymd("2016-01-01")) %>% 
  mutate(quarter = as.numeric(factor(quarter(date, type = "date_first"))),
         tract = str_sub(blockgroup, 1, -2)) 

comp_prop_panel_tract_month <- comp_prop_panel_bg_month %>%
  group_by(date, tract) %>% summarize(across(c(property, violent, complaints), ~sum(.)), .groups = "drop")

comp_prop_panel_tract_quarter <- comp_prop_panel_bg_month %>%
  group_by(tract, quarter) %>%
  summarize(across(c(property, violent, complaints), ~sum(.)), .groups = "drop")

save(comp_prop_panel_tract_month, file = "./data/derived/analysis/comp_prop_panel_tract_month.RData")
save(comp_prop_panel_bg_month, file = "./data/derived/analysis/comp_prop_panel_bg_month.RData")
save(comp_prop_panel_tract_quarter, file = "./data/derived/analysis/comp_prop_panel_tract_quarter.RData")

# BG CROSS


comp_prop_cross_bg_nosplag <- comp_prop_panel_bg_month %>%
  filter(date >= ymd("2019-04-01") & date < ymd("2019-08-01")) %>%
  group_by(blockgroup) %>%
  summarize(across(c(property, violent, complaints), ~sum(.))) %>%
  full_join(tent_census_summer_2019_bg) %>%
  full_join(acs5_bg_2019) %>%
  full_join(streets_bg)

comp_prop_cross_bg_splag <- seattle_bg_neighbors %>%
  inner_join(comp_prop_cross_bg_nosplag %>% rename(neighbors = blockgroup)) %>%
  group_by(blockgroup) %>%
  summarize(across(matches("major|highway|burglary|gta|property|violent|complaints|sweeps|n_dwellings|disadv|pop_sqkm|pr_ownhome"), ~ mean(., na.rm = TRUE), .names = "splag_{.col}"), .groups = "drop")

comp_prop_cross_bg <- comp_prop_cross_bg_nosplag |> 
  full_join(comp_prop_cross_bg_splag)

save(comp_prop_cross_bg, file = "./data/derived/analysis/comp_prop_cross_bg.RData")


comp_prop_sweeps_cross_bg_nosplag <- comp_prop_panel_bg_month %>%
  full_join(sweeps_monthly_bg) %>%
  filter(date >= ymd("2019-04-01") & date < ymd("2019-08-01")) %>%
  group_by(blockgroup) %>%
  summarize(across(c(property, violent, complaints, sweeps), ~sum(.))) %>%
  full_join(tent_census_summer_2019_bg) %>%
  full_join(acs5_bg_2019)

comp_prop_sweeps_cross_bg_splag <- seattle_bg_neighbors %>%
  inner_join(comp_prop_sweeps_cross_bg_nosplag %>% rename(neighbors = blockgroup)) %>%
  group_by(blockgroup) %>%
  summarize(across(matches("major|highway|burglary|gta|property|violent|complaints|sweeps|n_dwellings|disadv|pop_sqkm|pr_ownhome"), ~ mean(., na.rm = TRUE), .names = "splag_{.col}"), .groups = "drop")

comp_prop_sweeps_cross_bg <- comp_prop_sweeps_cross_bg_nosplag |> 
  full_join(comp_prop_sweeps_cross_bg_splag)

save(comp_prop_sweeps_cross_bg, file = "./data/derived/analysis/comp_prop_sweeps_cross_bg.RData")


comp_prop_sweeps_monthly_bg <- comp_prop_panel_bg_month %>%
  full_join(sweeps_monthly_bg) %>%
  filter(date >= ymd("2019-04-01") & date < ymd("2019-08-01")) %>%
  full_join(tent_census_summer_2019_bg) %>%
  full_join(acs5_bg_2019) 

save(comp_prop_sweeps_monthly_bg, file = "./data/derived/analysis/comp_prop_sweeps_monthly_bg.RData")

# TRACT CROSS

comp_prop_cross_tract_nosplag <- comp_prop_panel_tract_month %>%
  filter(date >= ymd("2019-04-01") & date < ymd("2019-08-01")) %>%
  group_by(tract) %>%
  summarize(across(c(property, violent, complaints), ~sum(.))) %>%
  full_join(tent_census_summer_2019_tract) %>%
  full_join(acs5_tract_2019) %>%
  full_join(streets_tract)

comp_prop_cross_tract_splag <- seattle_tract_neighbors %>%
  inner_join(comp_prop_cross_tract_nosplag %>% rename(neighbors = tract)) %>%
  group_by(tract) %>%
  summarize(across(matches("major|highway|burglary|gta|property|violent|complaints|sweeps|n_dwellings|disadv|pop_sqkm|pr_ownhome"), ~ mean(., na.rm = TRUE), .names = "splag_{.col}"), .groups = "drop")


comp_prop_cross_tract <- comp_prop_cross_tract_nosplag |> 
  full_join(comp_prop_cross_tract_splag)

save(comp_prop_cross_tract, file = "./data/derived/analysis/comp_prop_cross_tract.RData")
