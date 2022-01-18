library(tidyverse)
library(sf)
source("./syntax/project_functions.R")

get_neighbors <- function(df, id, snap = 0.001){
  out <- df %>%
    select(all_of(id), "geometry") |>
    # Reclassing poly2nb lets it be used in a normal mutate call
    mutate(neighbors = magrittr::set_class(spdep::poly2nb(geometry, snap = snap), "list")) |>
    st_drop_geometry() |>
    unnest(neighbors) |>
    mutate(neighbors = df[[id]][neighbors])
  return(out)
}

# Tracts
seattle_tract_boundaries <- tigris::tracts("WA", county = "King", class="sf") %>% 
  select(tract = GEOID, geometry) %>%
  filter(as.numeric(str_sub(tract, -5, -1)) < 13000 | tract == 53033026500) %>%
  # filter(GEOID != 53033005302) %>% # University
  st_transform(3689)
seattle_tract_neighbors <- seattle_tract_boundaries |> get_neighbors("tract")

save(seattle_tract_boundaries, file = "./data/derived/tract/seattle_tract_boundaries.RData")
save(seattle_tract_neighbors, file = "./data/derived/tract/seattle_tract_neighbors.RData")

# Blockgroups
seattle_bg_boundaries <- tigris::block_groups("WA", county = "King", class="sf") %>% 
  select(blockgroup = GEOID, geometry) %>% 
  filter(as.numeric(str_sub(blockgroup, -6, -1)) < 130000 | str_detect(blockgroup, "530330265001")) %>%
  # filter(str_sub(GEOID, 1, -2) != "53033005302") %>% # University
  st_transform(3689)
seattle_bg_neighbors <- seattle_bg_boundaries |> get_neighbors("blockgroup")

save(seattle_bg_boundaries, file = "./data/derived/bg/seattle_bg_boundaries.RData")
save(seattle_bg_neighbors, file = "./data/derived/bg/seattle_bg_neighbors.RData")

# Blocks
seattle_block_boundaries <- tigris::blocks("WA", county = "King", class="sf") %>%
  filter(as.numeric(TRACTCE10) < 13000 | str_detect(GEOID10, "530330265001")) %>%
  filter(ALAND10 > 0) %>%
  select(block = GEOID10, geometry) %>%
  st_transform(3689) %>%
  filter(block %!in% c(530330046001028, 530330108001002)) # Two uninhabited islands.

seattle_block_neighbors <- seattle_block_boundaries |> get_neighbors("block")

save(seattle_block_boundaries, file = "./data/derived/block/seattle_block_boundaries.RData")
save(seattle_block_neighbors, file = "./data/derived/block/seattle_block_neighbors.RData")

# Dealing with water bodies

seattle_withwater <- seattle_bg_boundaries %>%
  st_union() %>% st_combine()

save(seattle_withwater, file = "./data/derived/other/seattle_withwater.RData")

seattle_nowater <- seattle_withwater %>%
  st_difference(tigris::area_water("WA", county = "King", class="sf") %>%
                  st_transform(3689) %>% 
                  st_union() %>% 
                  st_combine())

save(seattle_nowater, file = "./data/derived/other/seattle_nowater.RData")

# Street grid
street_network <- st_read("./data/raw/Street_Network_Database_SND/Street_Network_Database_SND.shp") %>%
  st_transform(3689)

streets_filtered <-  street_network %>%
  filter(SND_FEACOD %in% c("State Highway", "Interstate Highway") | str_detect(ORD_STNAME, "AURORA") | (str_detect(ORD_STNAME, "EAST MARGINAL WAY") & L_ADRS_FRO < 6500)) %>%
  select(geometry) %>%
  st_intersection(seattle_withwater)

save(streets_filtered, file = "./data/derived/other/streets_filtered.RData")

major_streets <- street_network %>%
  mutate(class = case_when(
    str_detect(SND_FEACOD, "Highway") ~ "Highway",
    SND_FEACOD == "Major Street" ~ "Major",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(class)) %>%
  select(geometry, class)

streets_bg <- seattle_bg_boundaries %>%
  st_join(major_streets) %>%
  st_drop_geometry() %>%
  count(blockgroup, class) %>%
  pivot_wider(names_from = class, values_from = n, values_fill = 0) %>% 
  select(-`NA`) %>%
  mutate(across(-blockgroup, ~ as.numeric(.>0)))
save(streets_bg, file = "./data/derived/bg/streets_bg.RData")

streets_block <- seattle_block_boundaries %>%
  st_join(major_streets) %>%
  st_drop_geometry() %>%
  count(block, class) %>%
  pivot_wider(names_from = class, values_from = n, values_fill = 0) %>% 
  select(-`NA`) %>%
  mutate(across(-block, ~ as.numeric(.>0))) %>%
  janitor::clean_names() 

save(streets_block, file = "./data/derived/block/streets_block.RData")

streets_bg <- streets_block %>%
  mutate(blockgroup = str_sub(block, 1, -4)) %>%
  group_by(blockgroup) %>%
  summarize(across(c(major, highway), ~sum(.)))

save(streets_bg, file = "./data/derived/bg/streets_bg.RData")

streets_tract <- streets_block %>%
  mutate(tract = str_sub(block, 1, -5)) %>%
  group_by(tract) %>%
  summarize(across(c(major, highway), ~sum(.))) 

save(streets_tract, file = "./data/derived/tract/streets_tract.RData")
