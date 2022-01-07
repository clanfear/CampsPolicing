library(tidyverse)
library(sf)

load("./data/derived/tent_census.RData")
load("./data/derived/seattle_tract.RData")
load("./data/derived/seattle_bg.RData")

tents_bg <- seattle_bg %>% 
  st_join(tent_census %>%
           select(-Note) %>%
           st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
           st_transform(3689)) %>%
  replace_na(list(n_tents = 0, n_structures = 0, n_dwellings = 0)) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize_all(~sum(.))

save(tents_bg, file = "./data/derived/tents_bg.RData")

tents_tract <- seattle_tract %>% 
  st_join(tent_census %>%
            select(-Note) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
            st_transform(3689)) %>%
  replace_na(list(n_tents = 0, n_structures = 0, n_dwellings = 0)) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize_all(~sum(.))

save(tents_tract, file = "./data/derived/tents_tract.RData")