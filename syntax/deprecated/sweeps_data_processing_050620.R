library(tidyverse)
library(ggmap)
library(janitor)
library(lubridate)
library(sf)

load("./data/derived/seattle_tract.RData")
load("./data/derived/seattle_bg.RData")

sweeps_data <- 
  read_csv("./data/raw/Sweeps Data - Sheet1.csv") %>%
  clean_names() %>%
  pivot_longer(cols = matches("ed_by_"), names_to = "name", values_to = "referrer", values_drop_na=TRUE) %>% 
  group_by(date, location) %>%
  mutate(ref_community = as.numeric(any(str_to_upper(referrer) %in% c("COMMUNITY", "CITIZEN"))),
         ref_government = as.numeric(any(str_to_upper(referrer) %in% c("EPA", "PARKS", "SDOT", "SFD", "SOUND TRANSIT", "SPU", "WS-DOT")))) %>%
  ungroup() %>%
  mutate(date = mdy(date)) %>%
  select(-referrer, -name) %>%
  distinct()

sweeps_data_sf <- sweeps_data %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(3689)

## Block Groups
sweeps_bg <- seattle_bg %>%
  st_join(sweeps_data_sf %>% 
            select(geometry, 
                   sweeps_community = ref_community, 
                   sweeps_government = ref_government) %>% 
            mutate(sweeps = 1)) %>% 
  st_drop_geometry() %>%
  replace_na(list(sweeps = 0, sweeps_community = 0, sweeps_government = 0)) %>%
  group_by(GEOID) %>% 
  summarize_all(~sum(.))
  
save(sweeps_bg, file = "./data/derived/sweeps_bg.RData")

## Tracts
sweeps_tract <- seattle_tract %>% 
  st_join(sweeps_data_sf %>% 
            select(geometry, 
                   sweeps_community = ref_community, 
                   sweeps_government = ref_government) %>% 
            mutate(sweeps = 1)) %>% 
  st_drop_geometry() %>%
  replace_na(list(sweeps = 0, sweeps_community = 0, sweeps_government = 0)) %>%
  group_by(GEOID) %>% 
  summarize_all(~sum(.))

save(sweeps_tract, file = "./data/derived/sweeps_tract.RData")
