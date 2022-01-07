library(tidyverse)
library(lubridate)
library(sf)
load("./data/derived/spd_rms_offenses.RData")

spd_rms_geo <- spd_rms_offenses %>%
  filter(!is.na(start_time) & !is.na(offense_type_hierarchy) & !is.na(location_x)) %>%
 # filter(start_time > (max(spd_rms_offenses$start_time) - dyears(1))) %>%
  select(event_id, offense_type_hierarchy, location_x, location_y, start_time) %>%
  distinct(event_id, offense_type_hierarchy, location_x, location_y, .keep_all=TRUE) %>%
  st_as_sf(coords = c("location_x", "location_y"), crs = 3689) %>%
  st_transform(4326)

save(spd_rms_geo, file = "./data/derived/spd_rms_geo.RData")

spd_rms_geo_2018_date <- spd_rms_geo %>% 
  filter(year(start_time)==2018)

save(spd_rms_geo_2018, file = "./data/derived/spd_rms_geo_2018.RData")
