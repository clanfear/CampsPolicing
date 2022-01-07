# This file processes the raw spreadsheet from the Autumn 2019 tent census resample, 
# which searched areas with high tent density in the original full search
# The census ran from April 5th to July 30th, 2019 (116 days), according to timestamps.

library(tidyverse)
library(sf)
library(lubridate)

load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/bg/seattle_bg_boundaries.RData")

tent_census_summer_2020_raw <- readxl::read_excel("./data/raw/Master TC_TR Database 11 28 2019 Tent Census Originals Database Full Form.xlsx", 
                                                         sheet = "TR2 Encampments Unexpanded")

tent_census_summer_2020 <- tent_census_summer_2020_raw %>%
  rename(note = Description, latitude = `LAT DD`, longitude = `LONG DD`, n_dwellings = `Size of encampment`, id = `Encampment Number`) %>%
  distinct(latitude, longitude, n_dwellings, note)  %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3689)

tent_census_summer_2020_raw %>%
  select(Description) %>%
  mutate(est_date = str_squish(str_extract(Description, "(^| )[0-9]+/[0-9]+"))) %>%
  filter(!is.na(est_date)) %>%
  mutate(est_date = mdy(paste0(est_date, "/2019"))) %>%
  summarize(start = min(est_date, na.rm=TRUE),
            end = max(est_date, na.rm=TRUE),
            diff = end - start)

save(tent_census_summer_2020, file = "./data/derived/disaggregated/tent_census_summer_2020.RData")

tent_census_summer_2020_tract <- tent_census_summer_2020 %>%
  st_join(seattle_tract_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  select(-note) %>%
  group_by(tract) %>%
  summarize(across(everything(), ~sum(.))) %>%
  full_join(seattle_tract_boundaries %>% st_drop_geometry() %>% select(tract)) %>%
  mutate(across(-tract, ~ ifelse(is.na(.), 0, .)))

save(tent_census_summer_2020_tract, file = "./data/derived/tract/tent_census_summer_2020_tract.RData")

# CHECK VALIDITY OF THESE

tent_census_summer_2020_bg <- tent_census_summer_2020 %>%
  st_join(seattle_bg_boundaries, join = st_nearest_feature) %>%
  st_drop_geometry()%>%
  select(-note) %>%
  group_by(blockgroup) %>%
  summarize(across(everything(), ~sum(.))) %>%
  full_join(seattle_bg_boundaries %>% st_drop_geometry() %>% select(blockgroup)) %>%
  mutate(across(-blockgroup, ~ ifelse(is.na(.), 0, .)))

save(tent_census_summer_2020_bg, file = "./data/derived/bg/tent_census_summer_2020_bg.RData")
