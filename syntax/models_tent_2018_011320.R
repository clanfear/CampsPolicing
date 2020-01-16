library(tidyverse)
library(tidylog, warn.conflicts = FALSE)
library(ggeffects)
library(patchwork)
load("./data/derived/beats_analysis_half.RData")
load("./data/derived/tent_census.RData")
load("./data/derived/spd_rms_geo_2018.RData")
standardize <- function(x, na.rm=TRUE){
  return((x - mean(x, na.rm=na.rm) ) / sd(x, na.rm=na.rm))
}

tent_census <- tent_census %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(3690)

beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(3690) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

beats_tents <- beats_2015_2017 %>%
  st_join(tent_census) %>%
  mutate_at(vars(n_tents, n_structures, n_dwellings), ~ ifelse(is.na(.), 0, .)) %>%
  select(-beat_area, -Note) %>%
  st_drop_geometry() %>%
  group_by(beat) %>%
  summarize_all(~ sum(.))

beats_tents_analysis <- beats_analysis_half %>%
  filter(year==2018) %>%
  group_by(beat) %>%
  summarize(eb_fear_mi = mean(eb_fear_mi),
            eb_pe_mi = mean(eb_pe_mi),
            n_camping_complaints = sum(n_camping_complaints),
            offense_property_rate = mean(offense_property_rate),
            offense_violent_rate = mean(offense_violent_rate),
            offense_property = round(sum(offense_property)),
            population = mean(population)) %>%
  left_join(beats_tents, by = "beat")

beats_tents_analysis %>%
  ggplot(aes(x = n_dwellings, y = offense_property)) + geom_point() + geom_smooth()

summary(MASS::glm.nb(n_camping_complaints ~ offense_property, data = beats_tents_analysis))

tibble(x = 0:100, sqrt_x = sqrt(x)/2, log_x = log(x)) %>% pivot_longer(-x, names_to = "transform", values_to = "value") %>% ggplot(aes(x=x, y = value, color = transform)) + geom_line() 
## Spatial lag models
### Complaints
# complaints_nb_2016 <- MASS::glm.nb(n_camping_complaints ~ offense_property_rate +
#                                      eb_fear_mi  + eb_pe_mi +
#                                      year_half * factor(beat) + offset(log(population)),
#                                    data = beats_analysis_half %>% filter(year >= 2016))
# summary(complaints_nb_2016)

# complaints_nb_2016_int <- MASS::glm.nb(n_camping_complaints ~ offense_property_rate + 
#                                      eb_fear_mi  + eb_pe_mi +
#                                      year_half * factor(beat) + offset(log(population_int)),
#                                    data = beats_analysis_half %>% filter(year >= 2016))
# summary(complaints_nb_2016_int)

complaints_nb_2018_tents <- MASS::glm.nb(n_camping_complaints ~ offense_property_rate +
                                               eb_fear_mi   + n_dwellings +
                                               offset(log(population)),
                                               data = beats_tents_analysis %>%
                                               mutate(offense_property_rate = standardize(offense_property_rate),
                                                      eb_fear_mi = standardize(eb_fear_mi),
                                                      eb_pe_mi = standardize(eb_pe_mi),
                                                      n_dwellings = standardize(n_dwellings)))
summary(complaints_nb_2018_tents)

property_nb_2018_tents <- MASS::glm.nb(offense_property ~ eb_fear_mi   + eb_pe_mi + dwellings_rate + n_camping_complaints +
                                offset(log(population)),
                              data = beats_tents_analysis %>%
                                mutate(offense_property_rate = standardize(offense_property_rate),
                                       eb_fear_mi = standardize(eb_fear_mi),
                                       eb_pe_mi = standardize(eb_pe_mi),
                                       n_dwellings = standardize(n_dwellings),
                                       dwellings_rate = n_dwellings/population ))
summary(property_nb_2018_tents)

summary(lm(offense_property_rate ~ eb_fear_mi   + eb_pe_mi + n_dwellings + n_camping_complaints + log(population),data = beats_tents_analysis %>%
     mutate(offense_property_rate = standardize(offense_property_rate),
            eb_fear_mi = standardize(eb_fear_mi),
            eb_pe_mi = standardize(eb_pe_mi),
            n_dwellings = standardize(n_dwellings))))
