library(tidyverse)
library(ggmap)
library(lubridate)
library(sf)
library(janitor)

load("./data/derived/spd_rms_geo.RData")
load("./data/derived/seattle_tract.RData")
load("./data/derived/seattle_bg.RData")
## TRACT


rms_tract_month_year <- spd_rms_geo %>% 
  st_transform(3689) %>%
  st_join(seattle_tract) %>%
  st_drop_geometry() %>%
  filter(!is.na(GEOID)) %>%
  mutate(year = year(start_time),
         month = month(start_time)) %>%
  count(GEOID, year, month, offense_type_hierarchy) %>%
  pivot_wider(names_from = offense_type_hierarchy, values_from = n) %>%
  clean_names() %>% rename(GEOID = geoid) %>%
  replace_na(list(other = 0, property = 0, violent = 0, public_order = 0)) %>%
  complete(GEOID, nesting(year, month), fill = list(other = 0, property = 0, violent = 0, public_order = 0))

save(rms_tract_month_year, file = "./data/derived/rms_tract_month_year.RData")

## BG


rms_bg_month_year <- spd_rms_geo %>% 
  st_transform(3689) %>%
  st_join(seattle_bg) %>%
  st_drop_geometry() %>%
  filter(!is.na(GEOID)) %>%
  mutate(year = year(start_time),
         month = month(start_time)) %>%
  count(GEOID, year, month, offense_type_hierarchy) %>%
  pivot_wider(names_from = offense_type_hierarchy, values_from = n) %>%
  clean_names() %>% rename(GEOID = geoid) %>%
  replace_na(list(other = 0, property = 0, violent = 0, public_order = 0)) %>%
  complete(GEOID, nesting(year, month), fill = list(other = 0, property = 0, violent = 0, public_order = 0))
  
save(rms_bg_month_year, file = "./data/derived/rms_bg_month_year.RData")
