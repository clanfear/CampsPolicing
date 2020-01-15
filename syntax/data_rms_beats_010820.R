library(tidyverse)
library(sf)
library(areal)

# About 70k each year 2008 through 2018, then 38k for 2019; could def get 2019 though from Loren
load("./data/derived/spd_rms_geo.RData")
beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")
# spd_rms_geo %>% count(year(start_time))

spd_rms_geo_beat <- spd_rms_geo %>% 
  st_transform(3690) %>%
  st_join(beats_2015_2017 %>% st_transform(3690)) %>%
  st_drop_geometry() %>%
  mutate(year = lubridate::year(start_time)) %>%
  select(-beat_area, -start_time) %>%
  group_by(beat, year) %>%
  summarize(offense_property = sum(offense_type_hierarchy == "Property"),
            offense_public_order = sum(offense_type_hierarchy == "Public Order"),
            offense_violent = sum(offense_type_hierarchy == "Violent"),
            offense_other = sum(offense_type_hierarchy == "Other")) %>%
  mutate_at(vars(starts_with("offense")), 
            ~ ifelse(year == 2019, ./(lubridate::yday(max(spd_rms_geo$start_time))/365), .))

# We don't have all of 2019, so I divide counts by the proportion of the year covered.

load("./data/derived/king_pop_2008_2019_aw.RData")

spd_rms_rates_beat <- spd_rms_geo_beat %>%
  inner_join(king_pop_2008_2019_aw, by = c("beat","year")) %>%
  mutate_at(vars(starts_with("offense")), list(rate = ~./population))

save(spd_rms_rates_beat, file = "./data/derived/spd_rms_rates_beat.RData")

