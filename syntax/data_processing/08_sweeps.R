# This file processes raw data on sweeps of encampments.
# Sweeps here encompass 8-Apr-2018 to 27-Aug-2019

library(tidyverse)
library(lubridate)
library(sf)

load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/bg/seattle_bg_boundaries.RData")

sweeps <- 
  read_csv("./data/raw/Sweeps Data - Sheet1.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(cols = matches("ed_by_"), 
               names_to = "name", 
               values_to = "referrer", 
               values_drop_na=TRUE) %>% 
  group_by(date, location) %>%
  mutate(ref_community = as.numeric(any(str_to_upper(referrer) %in% c("COMMUNITY", "CITIZEN"))),
         ref_government = as.numeric(any(str_to_upper(referrer) %in% c("EPA", "PARKS", "SDOT", "SFD", "SOUND TRANSIT", "SPU", "WS-DOT")))) %>%
  ungroup() %>%
  mutate(date = mdy(date)) %>%
  select(-referrer, -name) %>%
  distinct() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(3689)

save(sweeps, file = "./data/derived/disaggregated/sweeps.RData")

## Block Groups
sweeps_bg <- seattle_bg_boundaries %>%
  st_join(sweeps %>% 
            select(geometry, 
                   sweeps_community = ref_community, 
                   sweeps_government = ref_government) %>% 
            mutate(sweeps = 1)) %>% 
  st_drop_geometry() %>%
  replace_na(list(sweeps = 0, sweeps_community = 0, sweeps_government = 0)) %>%
  group_by(blockgroup) %>% 
  summarize(across(everything(),~sum(.)))
  
save(sweeps_bg, file = "./data/derived/bg/sweeps_bg.RData")

## Tracts
sweeps_tract <- seattle_tract_boundaries %>% 
  st_join(sweeps %>% 
            select(geometry, 
                   sweeps_community = ref_community, 
                   sweeps_government = ref_government) %>% 
            mutate(sweeps = 1)) %>% 
  st_drop_geometry() %>%
  replace_na(list(sweeps = 0, sweeps_community = 0, sweeps_government = 0)) %>%
  group_by(tract) %>% 
  summarize(across(everything(),~sum(.)))

save(sweeps_tract, file = "./data/derived/tract/sweeps_tract.RData")

sweeps_monthly_tract <-  sweeps %>% 
  select(geometry, 
         sweeps_community = ref_community, 
         sweeps_government = ref_government,
         date) %>% 
  mutate(sweeps = 1,
         year = year(date),
         month = month(date)) %>% 
  st_join(seattle_tract_boundaries) %>% 
  st_drop_geometry() %>%
  group_by(tract, year, month) %>% 
  summarize(across(matches("^sweeps"), ~sum(.)), .groups = "drop") %>%
  complete(tract = seattle_tract_boundaries$tract, nesting(year, month), fill = list(sweeps = 0, sweeps_community = 0, sweeps_government = 0)) %>%
  mutate(date = ym(paste(year, month, sep = "-")))
save(sweeps_monthly_tract, file = "./data/derived/tract/sweeps_monthly_tract.RData")

sweeps_monthly_bg <-  sweeps %>% 
  select(geometry, 
         sweeps_community = ref_community, 
         sweeps_government = ref_government,
         date) %>% 
  mutate(sweeps = 1,
         year = year(date),
         month = month(date)) %>% 
  st_join(seattle_bg_boundaries) %>% 
  st_drop_geometry() %>%
  group_by(blockgroup, year, month) %>% 
  summarize(across(matches("^sweeps"), ~sum(.)), .groups = "drop") %>%
  complete(blockgroup = seattle_bg_boundaries$blockgroup, nesting(year, month), fill = list(sweeps = 0, sweeps_community = 0, sweeps_government = 0)) %>%
  mutate(date = ym(paste(year, month, sep = "-"))) %>%
  select(-year, -month)
save(sweeps_monthly_bg, file = "./data/derived/bg/sweeps_monthly_bg.RData")
