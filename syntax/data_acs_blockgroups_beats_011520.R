library(tidyverse)
library(sf)
library(tidycensus)

# REDO WITH BLOCK GROUPS
# DOES NOT WORK BECAUSE OF CENSUS API

beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

king_pop_2009_2018 <- 
  bind_rows(lapply(2009:2018, function(x) {
  get_acs("blockgroup", variables = "B01001_001E", year = x, state = "WA",
          county = "King", output = "wide", geometry = FALSE) %>%
      mutate(year = x) %>% select(GEOID, year, population = B01001_001E)}
  ))

get_acs("block group", variables = "B01001_001E", year = 2012, state = "WA",
        county = "King", output = "wide", geometry = FALSE)

king_tracts_2000 <- get_acs("tract", variables = "B01001_001E", year = 2009, state = "WA",
                            county = "King", output = "wide", geometry = TRUE) %>% select(GEOID, geometry)
king_tracts_2010 <- get_acs("tract", variables = "B01001_001E", year = 2017, state = "WA",
        county = "King", output = "wide", geometry = TRUE) %>% select(GEOID, geometry)

king_pop_2010_2018_geo <- king_pop_2009_2018 %>% 
  filter(year > 2009) %>% 
  left_join(king_tracts_2010, by = "GEOID") %>% st_as_sf()
king_pop_2009_geo <- king_pop_2009_2018 %>% 
  filter(year == 2009) %>% 
  left_join(king_tracts_2000, by = "GEOID") %>%
  st_as_sf()

save(king_pop_2010_2018_geo, file = "king_pop_2010_2018_geo.RData")
save(king_pop_2009_geo, file = "king_pop_2009_geo.RData")

king_pop_2010_2018_aw <- 
  bind_rows(lapply(unique(king_pop_2010_2018_geo$year), 
                   function(x) {
                     aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
                                    source=king_pop_2010_2018_geo %>% 
                                      st_transform(3690) %>% 
                                      filter(year == x), sid=GEOID,
                                    extensive = "population", weight = "sum", output = "sf") %>%
                       st_drop_geometry() %>%
                       mutate(year = x)
                   }))

king_pop_2009_aw <- aw_interpolate(beats_2015_2017 %>% st_transform(3690), beat,
               source=king_pop_2009_geo %>% 
                 st_transform(3690) %>% 
                 filter(year == 2009), sid=GEOID,
               extensive = "population", weight = "sum", output = "sf") %>%
  st_drop_geometry() %>%
  mutate(year = 2009)

king_pop_2009_2018_aw <- bind_rows(king_pop_2009_aw, king_pop_2010_2018_aw)

king_pop_2008_interpolated <- bind_rows(lapply(unique(king_pop_2009_2018_aw$beat), function(x) {
  king_pop_2009_2018_aw %>% filter(beat == x) %>% lm(population ~ year, data =.) %>% 
  predict(., newdata = data.frame(year=2008)) %>%
    tibble(beat =x, population = ., year=2008, 
           beat_area = king_pop_2009_2018_aw %>% filter(beat == x) %>% pull(beat_area) %>% unique(.))
}))
king_pop_2019_interpolated <- bind_rows(lapply(unique(king_pop_2009_2018_aw$beat), function(x) {
  king_pop_2009_2018_aw %>% filter(beat == x) %>% lm(population ~ year, data =.) %>% 
    predict(., newdata = data.frame(year=2019)) %>%
    tibble(beat =x, population = ., year=2019, 
           beat_area = king_pop_2009_2018_aw %>% filter(beat == x) %>% pull(beat_area) %>% unique(.))
}))

king_pop_2008_2019_aw <- bind_rows(king_pop_2008_interpolated, king_pop_2009_2018_aw, king_pop_2019_interpolated)

save(king_pop_2008_2019_aw, file = "./data/derived/king_pop_2008_2019_aw.RData")
