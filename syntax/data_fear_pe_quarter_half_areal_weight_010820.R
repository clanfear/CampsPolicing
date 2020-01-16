library(tidyverse)
library(sf)
library(purrr)
library(areal)

load("./data/derived/beat_half_eb_residuals.RData")

beat_half_eb_residuals <- beat_half_eb_residuals %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2") %>%
  mutate(year = as.numeric(year))

load("./data/derived/beat_quarter_eb_residuals.RData")

beat_quarter_eb_residuals <- beat_quarter_eb_residuals %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2") %>%
  mutate(year = as.numeric(year))

beats_2008 <- st_read("./data/raw/spd_shapefiles/beats_2008/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat = BEAT, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

beats_2018_2019 <- st_read("./data/raw/spd_shapefiles/beats_2018/SPD_Beats_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry)) %>% 
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")


# MEASUREMENT MODEL
# Half
beats_2008_2014_eb_h <- beats_2008 %>%
  left_join(beat_half_eb_residuals %>%
              filter(between(year, 2008, 2014)), by ="beat") %>%
  st_transform(3690)%>%
  mutate(year_half = str_c(year, call_half, sep = "-"))

beats_2015_2017_eb_h <- beats_2015_2017 %>%
  left_join(beat_half_eb_residuals %>%
              filter(between(year, 2015, 2017)), by ="beat") %>%
  st_transform(3690) %>%
  mutate(call_half = as.numeric(call_half))

beats_2018_2019_eb_h <- beats_2018_2019 %>%
  left_join(beat_half_eb_residuals %>%
              filter(year>=2018), by ="beat") %>%
  st_transform(3690) %>%
  mutate(year_half = str_c(year, call_half, sep = "-"))
# Quarter
beats_2008_2014_eb_q <- beats_2008 %>%
  left_join(beat_quarter_eb_residuals %>%
              filter(between(year, 2008, 2014)), by ="beat") %>%
  st_transform(3690)%>%
  mutate(year_quarter = str_c(year, call_quarter, sep = "-"))

beats_2015_2017_eb_q <- beats_2015_2017 %>%
  left_join(beat_quarter_eb_residuals %>%
              filter(between(year, 2015, 2017)), by ="beat") %>%
  st_transform(3690) %>%
  mutate(call_quarter = as.numeric(call_quarter))

beats_2018_2019_eb_q <- beats_2018_2019 %>%
  left_join(beat_quarter_eb_residuals %>%
              filter(year>=2018), by ="beat") %>%
  st_transform(3690) %>%
  mutate(year_quarter = str_c(year, call_quarter, sep = "-"))

# MEASUREMENT
#Half
beats_2008_2014_eb_aw_h <- bind_rows(lapply(unique(beats_2008_2014_eb_h$year_half), 
                                       function(x) {
                                         aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
                                                        source=beats_2008_2014_eb_h %>% filter(year_half == x), sid=beat,
                                                        weight = "sum", output = "sf",
                                                        intensive = c("eb_pe_mi", "eb_fear_mi")) %>%
                                           st_drop_geometry() %>%
                                           mutate(year_half = x) %>%
                                           separate(year_half, into = c("year", "call_half"), convert=TRUE) 
                                       })) %>% 
  arrange(beat, year, call_half)

beats_2018_2019_eb_aw_h <- bind_rows(lapply(unique(beats_2018_2019_eb_h$year_half), 
                                       function(x) {
                                         aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
                                                        source=beats_2018_2019_eb_h %>% filter(year_half == x), sid=beat,
                                                        weight = "sum", output = "sf",
                                                        intensive = c("eb_pe_mi", "eb_fear_mi")) %>%
                                           st_drop_geometry() %>%
                                           mutate(year_half = x) %>%
                                           separate(year_half, into = c("year", "call_half"), convert=TRUE) 
                                       }))%>% 
  arrange(beat, year, call_half)
#Quarter
beats_2008_2014_eb_aw_q <- bind_rows(lapply(unique(beats_2008_2014_eb_q$year_quarter), 
                                          function(x) {
                                            aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
                                                           source=beats_2008_2014_eb_q %>% filter(year_quarter == x), sid=beat,
                                                           weight = "sum", output = "sf",
                                                           intensive = c("eb_pe_mi", "eb_fear_mi")) %>%
                                              st_drop_geometry() %>%
                                              mutate(year_quarter = x) %>%
                                              separate(year_quarter, into = c("year", "call_quarter"), convert=TRUE) 
                                          })) %>% 
  arrange(beat, year, call_quarter)

beats_2018_2019_eb_aw_q <- bind_rows(lapply(unique(beats_2018_2019_eb_q$year_quarter), 
                                          function(x) {
                                            aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
                                                           source=beats_2018_2019_eb_q %>% filter(year_quarter == x), sid=beat,
                                                           weight = "sum", output = "sf",
                                                           intensive = c("eb_pe_mi", "eb_fear_mi")) %>%
                                              st_drop_geometry() %>%
                                              mutate(year_quarter = x) %>%
                                              separate(year_quarter, into = c("year", "call_quarter"), convert=TRUE) 
                                          }))%>% 
  arrange(beat, year, call_quarter)

# JOIN AND SAVE
# Half
beats_all_half_eb_aw <- beats_2015_2017_eb_h %>%
  distinct(beat, geometry) %>%
  inner_join(
    bind_rows(beats_2015_2017_eb_h %>% st_drop_geometry(),
              beats_2008_2014_eb_aw_h, beats_2018_2019_eb_aw_h) %>%
      arrange(year, beat), by ="beat") %>%
  st_drop_geometry() %>% select(-beat_area)

save(beats_all_half_eb_aw, file = "./data/derived/beats_all_half_eb_aw.RData")

# Half
beats_all_quarter_eb_aw <- beats_2015_2017_eb_q %>%
  distinct(beat, geometry) %>%
  inner_join(
    bind_rows(beats_2015_2017_eb_q %>% st_drop_geometry(),
              beats_2008_2014_eb_aw_q, beats_2018_2019_eb_aw_q) %>%
      arrange(year, beat), by ="beat") %>%
  st_drop_geometry() %>% select(-beat_area)

save(beats_all_quarter_eb_aw, file = "./data/derived/beats_all_quarter_eb_aw.RData")
  