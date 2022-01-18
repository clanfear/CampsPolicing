library(tidyverse)
library(sf)
library(lubridate)
load("./data/derived/bg/seattle_bg_boundaries.RData")
load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/block/seattle_block_boundaries.RData")

plot(seattle_block_boundaries)

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
           !is.na(offense_start_date_time) &
           !near(longitude, 0) & # Lot of zero long / lat
           !near(latitude, 0)) %>%
  mutate(date = mdy_hms(offense_start_date_time)) %>%
  filter(year(date) >= 2008 & year(date) < 2022) %>% # limiting scope to when data are supposed to cover
  filter(group_a_b == "A" & 
           offense != "Justifiable Homicide") %>%
  group_by(report_number) %>%
  arrange(date) %>%
  summarize(property = as.numeric(any(offense_parent_group %in% property_parent_groups)),
            violent  = as.numeric(any(offense_parent_group %in% violent_parent_groups)),
            gta      = as.numeric(any(offense_parent_group == "MOTOR VEHICLE THEFT")),
            homicide = as.numeric(any(offense_parent_group == "HOMICIDE OFFENSES")),
            burglary = as.numeric(any(offense_parent_group == "BURGLARY/BREAKING&ENTERING")),
            latitude = first(latitude),
            longitude = first(longitude),
            date = first(date), .groups = "drop") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3689) %>%
  st_join(seattle_block_boundaries) %>%
  st_drop_geometry() %>%
  filter(!is.na(block)) %>%
  mutate(date = ym(paste(year(date), month(date), sep = "-")),
         blockgroup = str_sub(block, 1 , -4),
         tract = str_sub(block, 1 , -5))
  
save(spd_public_geo, file = "./data/derived/disaggregated/spd_public_geo.RData")

spd_public_block_month <- spd_public_geo %>%
  pivot_longer(c(property, violent, gta, burglary)) %>%
  group_by(block, date, name) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  complete(block = seattle_block_boundaries$block, date, name, fill = list(value = 0)) %>%
  pivot_wider(names_from = name, values_from = value)

save(spd_public_block_month, file = "./data/derived/block/spd_public_block_month.RData")

spd_public_bg_month <- spd_public_block_month %>%
  mutate(blockgroup = str_sub(block, 1 , -4)) %>%
  select(-block) %>%
  group_by(blockgroup, date) %>%
  summarize(across(everything(), ~sum(.)), .groups = "drop")

save(spd_public_bg_month, file = "./data/derived/bg/spd_public_bg_month.RData")

spd_public_tract_month <- spd_public_block_month %>%
  mutate(tract = str_sub(block, 1 , -5)) %>%
  select(-block) %>%
  group_by(tract, date) %>%
  summarize(across(everything(), ~sum(.)), .groups = "drop")

save(spd_public_tract_month, file = "./data/derived/tract/spd_public_tract_month.RData")

