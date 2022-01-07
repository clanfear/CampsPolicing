# This file processes the raw spreadsheet from the Autumn 2019 tent census resample, 
# which searched areas with high tent density in the original full search
# The census ran from October 14th to December 14th, 2019 (61 days), according to timestamps.

library(tidyverse)
library(sf)
library(lubridate)

load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/bg/seattle_bg_boundaries.RData")

tent_census_autumn_2019_raw <- 
  readxl::read_excel("./data/raw/Master TC_TR Database 11 28 2019 Tent Census Originals Database Full Form.xlsx", 
                     sheet = "TR1 Encampments Unexpanded")

tent_census_autumn_2019_raw %>%
  select(Description) %>%
  mutate(est_date = str_squish(str_extract(Description, "(^| )[0-9]+/[0-9]+"))) %>%
  filter(!is.na(est_date)) %>%
  mutate(est_date = mdy(paste0(est_date, "/2019"))) %>%
  summarize(start = min(est_date, na.rm=TRUE),
            end = max(est_date, na.rm=TRUE),
            diff = end - start)

# REDO USING THE MASTER SHEET

tent_census_autumn_2019<- tent_census_autumn_2019_raw %>%
  rename(note = Description, latitude = `LAT DD`, longitude = `LONG DD`, n_dwellings = Size, id = `Encampment #`) %>%
  distinct(latitude, longitude, n_dwellings, note)  %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3689)


save(tent_census_autumn_2019, file = "./data/derived/disaggregated/tent_census_autumn_2019.RData")

tent_census_autumn_2019_tract <- tent_census_autumn_2019 %>%
  st_join(seattle_tract_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  select(-note) %>%
  group_by(tract) %>%
  summarize(across(everything(), ~sum(.))) %>%
  full_join(seattle_tract_boundaries %>% st_drop_geometry() %>% select(tract)) %>%
  mutate(across(-tract, ~ ifelse(is.na(.), 0, .))) 

save(tent_census_autumn_2019_tract, file = "./data/derived/tract/tent_census_autumn_2019_tract.RData")

tent_census_autumn_2019_bg <- tent_census_autumn_2019 %>%
  st_join(seattle_bg_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  select(-note) %>%
  group_by(blockgroup) %>%
  summarize(across(everything(), ~sum(.))) %>%
  full_join(seattle_bg_boundaries %>% st_drop_geometry() %>% select(blockgroup)) %>%
  mutate(across(-blockgroup, ~ ifelse(is.na(.), 0, .)))

save(tent_census_autumn_2019_bg, file = "./data/derived/bg/tent_census_autumn_2019_bg.RData")
