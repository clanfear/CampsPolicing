# What do I want to do here?
# Beat-level model of fear of crime, police efficacy, tent counts, controlling for crime

library(tidyverse)
library(sf)
library(areal)

beats_2018 <- st_read("./data/raw/spd_shapefiles/beats_2018/SPD_Beats_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(3690) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry)) %>% 
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

# load("./data/derived/beat_2018_pe_fear.RData")

load("./data/derived/beat_year_eb_residuals.RData")
load("./data/derived/spd_rms_geo_2018.RData")
load("./data/derived/tent_census.RData")
load("./data/derived/tract_disadvantage.RData")
load("./data/derived/dumping.RData")
load("./data/derived/general.RData")



overlap_plot <- ggplot() + geom_sf(data=beats_2018%>% st_transform(3690), color = "black", alpha=0.2, size=1.2) + 
  geom_sf(data = tract_disadvantage  %>% st_transform(3690) %>%
            st_join(beats_2018%>% st_transform(3690), join = st_overlaps, left=FALSE, largest=TRUE), color = "red", alpha=0.2) + 
  theme_void()

ggsave("./img/overlap_plot.svg", plot = overlap_plot)

beat_disadvantage <- aw_interpolate(beats_2018 %>% st_transform(3690), beat,
               source=tract_disadvantage  %>% st_transform(3690), sid=GEOID,
               weight = "sum", output = "sf",
               intensive = c("disadvantage", "poverty", "assistance", "median_family_income", "unemployment"), extensive = "population") %>% st_drop_geometry()

tent_census <- tent_census %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(3690)

beat_analysis <- beats_2018 %>%
  st_join(spd_rms_geo_2018 %>% st_transform(3690)) %>%
  group_by(beat) %>%
  summarize(n_property = sum(offense_type_hierarchy=="Property"),
            n_violent = sum(offense_type_hierarchy=="Violent"),
            n_public_order = sum(offense_type_hierarchy=="Public Order"),
            n_any_offense = n(),
            beat_area = first(beat_area)) %>%
  left_join(
    beats_2018 %>%
      st_join(tent_census) %>%
      mutate_at(vars(n_tents, n_structures, n_dwellings), ~ ifelse(is.na(.), 0, .)) %>%
      select(-beat_area, -Note) %>%
      st_drop_geometry() %>%
      group_by(beat) %>%
      summarize_all(~ sum(.))) %>%
  left_join(beat_year_eb_residuals %>% 
               filter(year==2018) %>%
               select(beat, eb_fear_mi, eb_pe_mi)) %>%
  left_join(beat_disadvantage)

beat_analysis <- beats_2018 %>%
  st_join(
    general %>% 
      rename_all(~tolower(str_replace_all(str_remove(., "\\?")," ", "_"))) %>%
      filter(issue_category %in% c("Unauthorized Camping")) %>%
      filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0) %>%
      st_as_sf(coords = c("x_value", "y_value"), 
               crs = 2285,
               agr = "identity") %>%
      st_transform(crs = 3690) %>%
      mutate(year = lubridate::year(created_date)) %>%
      filter(year == 2018) %>%
      select(geometry)) %>%
  st_drop_geometry() %>%
  group_by(beat) %>%
  summarize(n_camping_complaints = n()) %>%
  right_join(beat_analysis) %>%
  mutate_at(vars(n_property, n_violent, n_camping_complaints, n_dwellings), 
            list(rate = ~ (./population))) %>% 
  mutate(dwellings_sqkm = n_dwellings / beat_area)

summary(lm(police_efficacy ~  log(I(1+n_dwellings)), data = beat_analysis))

beat_analysis %>% select(-beat) %>% st_drop_geometry %>% mutate_all(~as.numeric(.)) %>% cor()

beat_analysis %>% ggplot(aes(fill = log(1+n_dwellings))) + geom_sf()

# Not much there. NExt step is look at SPU complaints.



  
beat_analysis %>% select(-beat, - geometry) %>% cor()

summary(lm(police_efficacy ~  log(I(1+n_dwellings)) + n_camping_complaints, data = beat_analysis))

summary(lm(eb_fear_mi ~  n_dwellings + n_violent + log(n_property), data = beat_analysis))

summary(lm(log(n_camping_complaints) ~  n_dwellings + eb_fear_mi + n_property, data = beat_analysis))

summary(lm(n_property ~ n_dwellings, data = beat_analysis %>% mutate(density = population / beat_area)))
summary(lm(n_violent ~ n_dwellings, data = beat_analysis %>% mutate(density = population / beat_area)))

summary(lm(n_property_rate ~ n_dwellings + population, data = beat_analysis %>% mutate(density = population / beat_area)))
summary(lm(n_violent_rate ~ n_dwellings, data = beat_analysis %>% mutate(density = population / beat_area)))

summary(lm(n_property_rate ~ n_dwellings  + poverty, data = beat_analysis %>% mutate(density = population / beat_area)))
summary(lm(n_violent_rate ~ n_dwellings  + poverty, data = beat_analysis %>% mutate(density = population / beat_area)))

summary(MASS::glm.nb(n_camping_complaints ~ scale(n_dwellings) + scale(n_property) + scale(n_violent) + scale(eb_fear_mi), data = beat_analysis %>% mutate(density = population / beat_area)))
summary(lm(n_camping_complaints ~ scale(sqrt(n_dwellings)) + scale(n_property) + scale(eb_fear_mi), data = beat_analysis %>% mutate(density = population / beat_area)))

summary(MASS::glm.nb(n_property ~ n_dwellings + density + offset(log(population)), data = beat_analysis %>% mutate(density = population / beat_area)))

summary(MASS::glm.nb(n_property ~ n_dwellings + density + poverty + assistance + unemployment, data = beat_analysis %>% mutate(density = population / beat_area)))

# Flips sign negative if you go sqrt or log
summary(lm(eb_fear_mi ~ n_violent + n_dwellings + median_family_income, data = beat_analysis %>% mutate(density = population / beat_area)))
summary(lm(eb_fear_mi ~ n_dwellings + n_property_rate, data = beat_analysis %>% mutate(density = population / beat_area)))

beat_analysis %>% ggplot(aes(x = n_dwellings, y = eb_fear_mi)) + geom_point() + geom_smooth()
