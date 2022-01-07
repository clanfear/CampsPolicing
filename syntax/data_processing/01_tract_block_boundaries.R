library(tidyverse)
library(sf)


  
kc_water <- tigris::area_water("WA", county = "King", class="sf") %>%
  st_transform(3689)

seattle_tract_boundaries <- tigris::tracts("WA", county = "King", class="sf") %>% 
  select(tract = GEOID, geometry) %>%
  filter(as.numeric(str_sub(tract, -5, -1)) < 13000 | tract == 53033026500) %>%
  # filter(GEOID != 53033005302) %>% # University
  st_transform(3689)

save(seattle_tract_boundaries, file = "./data/derived/tract/seattle_tract_boundaries.RData")

seattle_bg_boundaries <- tigris::block_groups("WA", county = "King", class="sf") %>% 
  select(blockgroup = GEOID, geometry) %>% 
  filter(as.numeric(str_sub(blockgroup, -6, -1)) < 130000 | str_detect(blockgroup, "530330265001")) %>%
  # filter(str_sub(GEOID, 1, -2) != "53033005302") %>% # University
  st_transform(3689)

save(seattle_bg_boundaries, file = "./data/derived/bg/seattle_bg_boundaries.RData")


seattle_block_boundaries <- tigris::blocks("WA", county = "King", class="sf") %>%
  filter(as.numeric(TRACTCE10) < 13000 | str_detect(TRACTCE10, "26500")) %>%
  filter(ALAND10 > 0) %>%
  select(block = GEOID10, geometry) %>%
  st_transform(3689)

save(seattle_block_boundaries, file = "./data/derived/block/seattle_block_boundaries.RData")

seattle_withwater <- seattle_bg_boundaries %>%
  st_union() %>% st_combine()

save(seattle_withwater, file = "./data/derived/other/seattle_withwater.RData")

seattle_nowater <- seattle_withwater %>%
  st_difference(kc_water %>% st_union() %>% st_combine())

save(seattle_nowater, file = "./data/derived/other/seattle_nowater.RData")

streets_filtered <- st_read("./data/raw/Street_Network_Database_SND/Street_Network_Database_SND.shp") %>%
  st_transform(3689) %>%
  filter(SND_FEACOD %in% c("State Highway", "Interstate Highway") | str_detect(ORD_STNAME, "AURORA") | (str_detect(ORD_STNAME, "EAST MARGINAL WAY") & L_ADRS_FRO < 6500)) %>%
  select(geometry) %>%
  st_intersection(seattle_withwater)

save(streets_filtered, file = "./data/derived/other/streets_filtered.RData")