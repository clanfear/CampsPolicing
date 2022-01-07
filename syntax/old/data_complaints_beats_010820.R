library(tidyverse)
library(sf)

load("./data/derived/dumping.RData")
load("./data/derived/general.RData")

beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

camping_beats_2012_2019 <- general %>% 
  rename_all(~tolower(str_replace_all(str_remove(., "\\?")," ", "_"))) %>%
  filter(issue_category %in% c("Unauthorized Camping")) %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 3690) %>%
  mutate(year = lubridate::year(created_date)) %>%
  select(year, geometry) %>%
  st_join(beats_2015_2017 %>% st_transform(3690) %>% select(beat, geometry) %>% filter(!is.na(beat))) %>%
  st_drop_geometry() %>%
  group_by(beat, year) %>%
  summarize(n_camping_complaints = n()) %>%
  ungroup() %>%
  complete(beat, year = 2012:2019, fill = list(n_camping_complaints =0)) %>%
  mutate(n_camping_complaints = 
           ifelse(year == 2019, 
                  n_camping_complaints/(lubridate::yday(max(general$created_date))/365), 
                  n_camping_complaints))

save(camping_beats_2012_2019, file = "./data/derived/camping_beats_2012_2019.RData")
