library(tidyverse)
library(sf)
library(purrr)
library(areal)

load("./data/derived/beat_year_pe_fear.RData")
load("./data/derived/beat_year_eb_residuals.RData")

beat_year_pe_fear <- beat_year_pe_fear %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")
beat_year_eb_residuals <- beat_year_eb_residuals %>%
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

# beats_2008 %>% mutate(period = "2008-2014") %>% 
#   rbind(., 
#         beats_2015_2017 %>% mutate(period = "2015-2017")) %>%
#   rbind(.,
#         beats_2018_2019 %>% mutate(period = "2018")) %>%
#   ggplot(aes(fill = beat_area)) + geom_sf() + facet_wrap(~period) + geom_sf_label(aes(label=beat))

# Need to assign beat-year data to appropriate shapes, then areal-interpolate them to some set of beats.
# Which should I use as basis? 2015-2017 maybe?
# Will then need to qualify 2008-2014 results as being interpolated, but 2018 is basically identical.

# NON-MEASUREMENT MODEL
beats_2008_2014_pe_fe <- beats_2008 %>%
  left_join(beat_year_pe_fear %>%
              filter(between(year, 2008, 2014)), by ="beat") %>%
  filter(!is.na(police_efficacy)) %>%
  st_transform(3690)
  
beats_2015_2017_pe_fe <- beats_2015_2017 %>%
  left_join(beat_year_pe_fear %>%
              filter(between(year, 2015, 2017)), by ="beat") %>%
  filter(!is.na(police_efficacy)) %>%
  st_transform(3690)

beats_2018_2019_pe_fe <- beats_2018_2019 %>%
  left_join(beat_year_pe_fear %>%
              filter(year>=2018), by ="beat") %>%
  filter(!is.na(police_efficacy)) %>%
  st_transform(3690)

# MEASUREMENT MODEL
beats_2008_2014_eb <- beats_2008 %>%
  left_join(beat_year_eb_residuals %>%
              filter(between(year, 2008, 2014)), by ="beat") %>%
  st_transform(3690)

beats_2015_2017_eb <- beats_2015_2017 %>%
  left_join(beat_year_eb_residuals %>%
              filter(between(year, 2015, 2017)), by ="beat") %>%
  st_transform(3690)

beats_2018_2019_eb <- beats_2018_2019 %>%
  left_join(beat_year_eb_residuals %>%
              filter(year>=2018), by ="beat") %>%
  st_transform(3690)

# before aw, should put on a projection
# NON-MEASUREMENT
beats_2008_2014_aw <- bind_rows(lapply(unique(beats_2008_2014_pe_fe$year), 
                        function(x) {
                          aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
                   source=beats_2008_2014_pe_fe %>% filter(year == x), sid=beat,
                   extensive = "n", weight = "sum", output = "sf",
                   intensive = c("police_efficacy", "fear_of_crime")) %>%
                            st_drop_geometry() %>%
                            mutate(year = x)
                          }))

beats_2018_2019_aw <- bind_rows(lapply(2018:2019, 
                                  function(x) {
                                    aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
                                                   source=beats_2018_2019_pe_fe %>% filter(year == x), sid=beat,
                                                   extensive = "n", weight = "sum", output = "sf",
                                                   intensive = c("police_efficacy", "fear_of_crime")) %>%
                                      st_drop_geometry() %>%
                                      mutate(year = x)
                                  }))
## DIAGNOSTIC
ggplot() + 
  geom_sf(data = beats_2008_2014_aw %>% 
            mutate(fear_of_crime = standardize(fear_of_crime)) %>%
  mutate(areal_weighted = "WEIGHTED") %>% 
  left_join(beats_2015_2017 %>% 
              st_transform(3690) %>% 
              select(beat, geometry)) %>% 
    st_as_sf(), 
  aes(fill = fear_of_crime), alpha=0.1, color = "blue") +
  geom_sf(data =beats_2008_2014_pe_fe %>% 
            mutate(fear_of_crime = -standardize(fear_of_crime)) %>% 
            mutate(areal_weighted = "UNWEIGHTED") %>% 
            st_transform(3690), 
          aes(fill = fear_of_crime), alpha = 0.1, color = "red") +
  scale_fill_gradient(low = "#000000", high = "#EEEEEE") + theme_void()
# MEASUREMENT
beats_2008_2014_eb_aw <- bind_rows(lapply(unique(beats_2008_2014_eb$year), 
                                       function(x) {
                                         aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
                                                        source=beats_2008_2014_eb %>% filter(year == x), sid=beat,
                                                        weight = "sum", output = "sf",
                                                        intensive = c("eb_pe", "eb_fear", "eb_pe_mi", "eb_fear_mi")) %>%
                                           st_drop_geometry() %>%
                                           mutate(year = x)
                                       }))

beats_2018_2019_eb_aw <- bind_rows(lapply(2018:2019, 
                                       function(x) {
                                         aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
                                                        source=beats_2018_2019_eb %>% filter(year == x), sid=beat,
                                                        weight = "sum", output = "sf",
                                                        intensive = c("eb_pe", "eb_fear", "eb_pe_mi", "eb_fear_mi")) %>%
                                           st_drop_geometry() %>%
                                           mutate(year = x)
                                       }))


beats_all_pe_fe_aw <- beats_2015_2017_pe_fe %>%
  distinct(beat, geometry) %>%
  inner_join(
    bind_rows(beats_2015_2017_pe_fe %>% st_drop_geometry(),
              beats_2008_2014_aw, beats_2018_2019_aw) %>%
  arrange(year, beat), by ="beat") %>%
  st_drop_geometry()

beats_all_eb_aw <- beats_2015_2017_eb %>%
  distinct(beat, geometry) %>%
  inner_join(
    bind_rows(beats_2015_2017_eb %>% st_drop_geometry(),
              beats_2008_2014_eb_aw, beats_2018_2019_eb_aw) %>%
      arrange(year, beat), by ="beat") %>%
  st_drop_geometry() %>% select(-beat_area)

beats_all_pe_fe_aw <- beats_all_pe_fe_aw %>% left_join(beats_all_eb_aw)

save(beats_all_pe_fe_aw, file = "./data/derived/beats_all_pe_fe_aw.RData")
  