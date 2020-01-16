library(tidyverse)
library(sf)

load("./data/derived/dumping.RData")
load("./data/derived/general.RData")

beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

camping_beats_quarter_2012_2019 <- general %>% 
  rename_all(~tolower(str_replace_all(str_remove(., "\\?")," ", "_"))) %>%
  filter(issue_category %in% c("Unauthorized Camping")) %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 3690) %>%
  mutate(year = lubridate::year(created_date),
         call_quarter = lubridate::quarter(created_date), 
         call_half = ifelse(call_quarter %in% c(1,2), 1, 2)) %>%
  select(year, call_quarter, geometry) %>%
  st_join(beats_2015_2017 %>% st_transform(3690) %>% select(beat, geometry) %>% filter(!is.na(beat))) %>%
  st_drop_geometry() %>%
  group_by(beat, year, call_quarter) %>%
  summarize(n_camping_complaints = n()) %>%
  ungroup() %>%
  complete(beat, year = 2012:2019, call_quarter = 1:4, fill = list(n_camping_complaints =0)) %>%
  filter(!(year==2019 & call_quarter %in% c(3,4))) %>%
  arrange(beat, year, call_quarter)

save(camping_beats_quarter_2012_2019, file = "./data/derived/camping_beats_quarter_2012_2019.RData")
