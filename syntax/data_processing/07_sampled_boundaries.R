# Getting correct sampled areas
library(tidyverse)
library(sf)
source("./syntax/project_functions.R")
load("./data/derived/bg/seattle_bg_boundaries.RData")
load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/bg/seattle_bg_boundaries.RData")
load("./data/derived/block/seattle_block_boundaries.RData")
load("./data/derived/disaggregated/tent_census_autumn_2019.RData")
load("./data/derived/disaggregated/tent_census_summer_2020.RData")
load("./data/derived/other/seattle_withwater.RData")
load("./data/derived/other/seattle_nowater.RData")

# Okay so no shapefiles exist for what they sample. They sampled these:
# https://www.seattle.gov/neighborhoods/programs-and-services/neighborhood-planning

# From Karen:
# Areas Resampled:
# - All of I-5
# - Ballard (highlighted, but not the Crown Hill highlighted section)
# - Fremont (highlighted)
# - Interbay (Ballard side - above the canal)
# - Queen Anne (highlighted uptown, as indicated on the city planning map, not the other highlighted section)
# - Georgetown
# - Commercial Core
# - Chinatown & International District
# - Pioneer Square
#
# Andria also told me:
#   
#   We did continue to use the city planning maps for the resamples. When we say
#   we covered the highlighted part of a map for the resamples, we mean we
#   covered the darker section on the map. For some maps, the entire section is
#   highlighted so we did not say highlighted for those. These were the specific
#   areas we covered for the resamples. They are not specifically indicated on
#   the physical map in any specific way because we used that primarily during
#   the full census.

sample_boundaries <- read_sf("./data/raw/sample_boundaries/sample_boundaries.shp")
street_network <- st_read("./data/raw/Street_Network_Database_SND/Street_Network_Database_SND.shp") %>%
  st_transform(3689)
resample_points <- bind_rows(tent_census_summer_2020,
                             tent_census_autumn_2019)

water <- tigris::area_water("WA", county = "King", class="sf") %>%
  st_transform(3689) %>% 
  st_union() %>% 
  st_combine()

freeway_buffer<- street_network %>% 
  janitor::clean_names() %>%
  filter(snd_feacod == "Interstate Highway") %>%
  filter(structure != "Tunnel" & objectid %!in% c(33859, 32621, 32651)) %>%
  summarize(geometry = st_union(geometry)) %>%
  mutate(geometry = st_buffer(geometry, dist = units::set_units(100, "ft"))) %>%
  mutate(geometry = st_intersection(geometry, seattle_withwater)) 

save(sample_boundaries, file = "./data/derived/other/sample_boundaries.RData")
save(freeway_buffer, file = "./data/derived/other/freeway_buffer.RData")

boundary_blocks <- seattle_block_boundaries %>%
  st_filter(sample_boundaries) %>%
  mutate(type = "sampled")
freeway_blocks <- seattle_block_boundaries %>%
  filter(block %!in% boundary_blocks$block) %>%
  st_filter(freeway_buffer) %>%
  mutate(type = "freeway")
outside_blocks <-  seattle_block_boundaries %>%
    filter(block %!in% boundary_blocks$block & block %!in% freeway_blocks$block) %>%
  st_filter(resample_points) %>%
  mutate(type = "outside")

all_resampled_blocks <- bind_rows(boundary_blocks,
          freeway_blocks,
          outside_blocks)

save(all_resampled_blocks, file = "./data/derived/block/all_resampled_blocks.RData")

seattle_bg_boundaries %>%
  st_filter(rbind(freeway_buffer, sample_boundaries %>% select(geometry))) %>% 
  plot()
