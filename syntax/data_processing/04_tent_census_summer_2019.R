# This file processes the raw spreadsheet from the Summer 2019 tent census, 
# which searched the entire city of Seattle.
# The census ran from April 4th to August 23rd, 2019 (141 days), according to timestamps.

library(tidyverse)
library(sf)
library(lubridate)
library(sf)

load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/bg/seattle_bg_boundaries.RData")
load("./data/derived/block/seattle_block_boundaries.RData")

tent_census_summer_2019_raw <- readxl::read_excel("./data/raw/Tent Census_April-August2019.xlsx")

tent_census_summer_2019_raw %>%
  select(Note) %>%
  mutate(est_date = str_squish(str_extract(Note, "(^| )[0-9]+/[0-9]+"))) %>%
  filter(!is.na(est_date)) %>%
  mutate(est_date = mdy(paste0(est_date, "/2019"))) %>%
  summarize(start = min(est_date, na.rm=TRUE),
            end = max(est_date, na.rm=TRUE),
            diff = end - start)

tent_census_summer_2019 <- tent_census_summer_2019_raw %>%
  mutate(coordinate = str_extract(URL, "[0-9\\.]*,-[0-9\\.]*$")) %>% 
  mutate(coordinate = case_when(
    Title == "4799-4701 9th Ave NW" ~ "47.663364,-122.370718", 
    URL == "https://www.google.com/maps/place/Seattle/data=!4m2!3m1!1s0x54906a961df6ee21:0x8119b0b7d5eea108" ~ "47.595891,-122.3219297",
    URL == "https://www.google.com/maps/place/Elliott+Bay+Trail/data=!4m2!3m1!1s0x54906ab27ec1f023:0x51ba1d0307faef5e" ~ "47.6077857,-122.3443708",
    TRUE ~ coordinate)) %>%
  separate(coordinate, c("latitude", "longitude"), ",", remove=FALSE, convert=TRUE) %>%
  mutate(note_upper = str_to_upper(Note)) %>%
  mutate(note_clean = str_remove(note_upper, "^.*//")) %>%
  mutate(note_clean = str_remove_all(note_clean, 
    "(ABANDONED |TREE |NATURAL |WOODEN |LARGE |ORANGE |SMALL NORTHFACE |SHARK-LOOKING |GREEN |DILAPITATED |LITTLE |BROWN |YELLOW |BIG |BLUE |GREY |HUGE |SM |LG |CARDBOARD |THREE PART )")) %>%
  mutate(n_tents = ifelse(str_detect(note_clean, "TENT"), 
                          str_extract(note_clean, "[0-9][0-9]? TENT(S)?"), 0)) %>%
  mutate(n_tents = as.numeric(ifelse(is.na(n_tents), 0, str_extract(n_tents, "\\d+")))) %>%
  mutate(n_tents = case_when(
    str_detect(Note, "Camp, cannot see in 6/20 11:21") ~ 5, 
    str_detect(Note, "Bottom of 8 tents 6/13 10:33 am") ~ 4,
    str_detect(Note, "At least one t one s 7/15 2:30") ~ 1,
    TRUE ~ n_tents)) %>%
  mutate(n_structures = ifelse(str_detect(note_clean, "STRUCTURE"), 
                               str_extract(note_clean, "[0-9][0-9]? STRUCTURE(S)?"), 0)) %>%
  mutate(n_structures = as.numeric(ifelse(is.na(n_structures), 0, str_extract(n_structures, "\\d+")))) %>%
  mutate(n_structures = case_when(
    str_detect(Note, "At least one t one s 7/15 2:30") ~ 1,
    str_detect(note_upper, "6 TENTS 3 STRUCTURES 6/24 10:59") ~ 3,
    str_detect(note_upper, "AT LEAST 4 STRUCTURES 6/25 AFTERNOON") ~ 5,
    str_detect(note_upper, "1 HUGE STRUCTURE 5/13 11:00 AM") ~ 5,
    TRUE ~ n_structures
  )) %>%
  mutate(n_dwellings = n_tents + n_structures) %>%
  distinct(latitude, longitude, n_dwellings, Note) %>%
  rename(note = Note) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3689)
  
save(tent_census_summer_2019, file = "./data/derived/disaggregated/tent_census_summer_2019.RData")

tent_census_summer_2019_tract <- tent_census_summer_2019 %>%
  st_join(seattle_tract_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  select(-note) %>%
  group_by(tract) %>%
  summarize(across(everything(), ~sum(.))) %>%
  full_join(seattle_tract_boundaries %>% st_drop_geometry() %>% select(tract)) %>%
  mutate(across(-tract, ~ ifelse(is.na(.), 0, .))) %>%
  rename(tract = tract)

save(tent_census_summer_2019_tract, file = "./data/derived/tract/tent_census_summer_2019_tract.RData")

tent_census_summer_2019_bg <- tent_census_summer_2019 %>%
  st_join(seattle_bg_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  select(-note) %>%
  group_by(blockgroup) %>%
  summarize(across(everything(), ~sum(.))) %>%
  full_join(seattle_bg_boundaries %>% st_drop_geometry() %>% select(blockgroup)) %>%
  mutate(across(-blockgroup, ~ ifelse(is.na(.), 0, .)))

save(tent_census_summer_2019_bg, file = "./data/derived/bg/tent_census_summer_2019_bg.RData")

tent_census_summer_2019_block <- tent_census_summer_2019 %>%
  st_join(seattle_block_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  select(-note) %>%
  group_by(block) %>%
  summarize(across(everything(), ~sum(.))) %>%
  full_join(seattle_block_boundaries %>% st_drop_geometry() %>% select(block)) %>%
  mutate(across(-block, ~ ifelse(is.na(.), 0, .)))

save(tent_census_summer_2019_block, file = "./data/derived/block/tent_census_summer_2019_block.RData")
