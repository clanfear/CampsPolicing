library(tidyverse)
library(sf)

city_clerk_neighborhoods <- read_sf("./data/raw/city_clerk_neighborhoods/City_Clerk_Neighborhoods.shp")
city_clerk_neighborhoods %>% select(geometry) %>% plot()

community_reporting_areas <- read_sf("./data/raw/community_reporting_areas/Community_Reporting_Areas.shp")
community_reporting_areas %>% select(geometry) %>% plot()
community_reporting_areas %>% pull(GEN_ALIAS) %>% as.data.frame()

SPU_neighborhoods <- read_sf("./data/raw/DON_cityclerk_nma_nhoods/DON_cityclerk_nma_nhoods.shp")
SPU_neighborhoods %>% select(geometry) %>% plot()

