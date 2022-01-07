library(tidyverse)
library(sf)
library(ggmap)
library(ragg)
library(showtext)
showtext_auto()
font_add_google("Quattrocento")

ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
# Load the sample and resample

load("./data/derived/disaggregated/tent_census_summer_2019.RData")
load("./data/derived/disaggregated/tent_census_autumn_2019.RData")
load("./data/derived/disaggregated/tent_census_summer_2020.RData")
load("./data/derived/other/seattle_nowater.RData")
load("./data/derived/other/seattle_withwater.RData")
load("./data/derived/other/streets_filtered.RData")

tent_census_counts <- 
  bind_rows(tent_census_summer_2019 %>% mutate(sample = "Summer 2019"),
            tent_census_autumn_2019 %>% mutate(sample = "Autumn 2019"),
            tent_census_summer_2020 %>% mutate(sample = "Summer 2020")) %>%
  select(note, sample, n_dwellings, geometry) %>%
  mutate(x_coord = st_coordinates(.)[,1],
         y_coord  = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("x_coord", "y_coord"), crs = 3689) %>%
  mutate(sample = fct_relevel(sample, "Summer 2019", "Autumn 2019", "Summer 2020"))

save(tent_census_counts, file = "./data/derived/other/tent_census_counts.RData")
