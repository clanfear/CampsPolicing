library(tidyverse)
library(sf)
library(lubridate)
load("./data/derived/bg/seattle_bg_boundaries.RData")
load("./data/derived/tract/seattle_tract_boundaries.RData")

spd_public_raw <- read_csv("./data/raw/SPD_Crime_Data__2008-Present.csv") 

# Offense ID is unique; Report Number is not

property_parent_groups <- c("LARCENY-THEFT", 
                           "BURGLARY/BREAKING&ENTERING",
                           "MOTOR VEHICLE THEFT")
violent_parent_groups <- c("ASSAULT OFFENSES", "HOMICIDE OFFENSES", "ROBBERY", "SEX OFFENSES")

spd_public_geo <- spd_public_raw %>%
  janitor::clean_names() %>%
  filter(!is.na(latitude) & # Only a tiny fraction (900) missing any of these
         !is.na(longitude) & 
         !is.na(offense_start_date_time)) %>%
  mutate(date = mdy_hms(offense_start_date_time)) %>%
  filter(year(date) >= 2008) %>% # limiting scope to when data are supposed to cover
  filter(group_a_b == "A" & 
           offense != "Justifiable Homicide") %>%
  group_by(report_number) %>%
  arrange(date) %>%
  summarize(property = as.numeric(any(offense_parent_group %in% property_parent_groups)),
            violent  = as.numeric(any(offense_parent_group %in% violent_parent_groups)),
            latitude = first(latitude),
            longitude = first(longitude),
            date = first(date), .groups = "drop") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3689) %>%
  st_join(seattle_bg_boundaries) %>%
  filter(!is.na(blockgroup)) %>%
  mutate(date = ym(paste(year(date), month(date), sep = "-")),
         tract = str_sub(blockgroup, 1 , -2)) %>%
  st_drop_geometry()
save(spd_public_geo, file = "./data/derived/disaggregated/spd_public_geo.RData")

spd_public_tract_month <- spd_public_geo %>%
  pivot_longer(c(property, violent)) %>%
  group_by(tract, date, name) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  complete(tract, date, name, fill = list(value = 0)) %>%
  pivot_wider(names_from = name, values_from = value)

save(spd_public_tract_month, file = "./data/derived/tract/spd_public_tract_month.RData")

spd_public_bg_month <- spd_public_geo %>%
  pivot_longer(c(property, violent)) %>%
  group_by(blockgroup, date, name) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  complete(blockgroup, date, name, fill = list(value = 0)) %>%
  pivot_wider(names_from = name, values_from = value)

save(spd_public_bg_month, file = "./data/derived/bg/spd_public_bg_month.RData")
