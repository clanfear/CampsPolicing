library(tidyverse)
library(ggmap)
library(lubridate)
library(sf)
library(janitor)

load("./data/derived/general.RData")
load("./data/derived/seattle_tract.RData")
load("./data/derived/seattle_bg.RData")

# Processing complaints
camping_complaints_sf <- general %>% 
  clean_names() %>%
  filter(issue_category %in% c("Unauthorized Camping")) %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 3689) %>%
  mutate(year = year(created_date),
         month = month(created_date)) %>%
  select(geometry, method_received, year, month)

# TRACT
complaints_tract <- seattle_tract %>%
  st_join(camping_complaints_sf) %>%
  st_drop_geometry() %>%
  mutate(complaints = 1,
         fifi_complaints = ifelse(method_received == "Find It Fix It Apps", 1 ,0)) %>%
  select(-method_received) %>%
  group_by(GEOID, year, month) %>%
  summarize_all(~sum(.)) %>%
  complete(GEOID, year = 2012:2019, month = 1:12, fill = list(complaints = 0, fifi_complaints = 0)) %>%
  filter(!(year==2019 & month >= 7))
  
save(complaints_tract, file = "./data/derived/complaints_tract.RData")

# BG
complaints_bg <- seattle_bg %>%
  st_join(camping_complaints_sf) %>%
  st_drop_geometry() %>%
  mutate(complaints = 1,
         fifi_complaints = ifelse(method_received == "Find It Fix It Apps", 1 ,0)) %>%
  select(-method_received) %>%
  group_by(GEOID, year, month) %>%
  summarize_all(~sum(.)) %>%
  complete(GEOID, year = 2012:2019, month = 1:12, fill = list(complaints = 0, fifi_complaints = 0)) %>%
  filter(!(year==2019 & month >= 7))

save(complaints_bg, file = "./data/derived/complaints_bg.RData")
