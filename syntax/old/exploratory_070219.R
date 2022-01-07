library(tidyverse)
library(readxl)
library(ggmap)
library(sf)
library(tidycensus)
library(tigris)



load("./data/derived/general.RData")
load("./data/derived/dumping.RData")
load("./data/derived/graffiti.RData")


# General
general %>% count(issue_category) %>% arrange(desc(n)) %>% print(n=100)




st_erase <- function(x, y) {
  st_difference(x, lwgeom::st_make_valid(st_union(st_combine(y))))
}

king_county <- tigris::tracts("WA", "King", class = "sf") %>%
  st_transform(2285)

kc_water <- tigris::area_water("WA", count="King", class = "sf") %>%
  st_transform(2285)

kc_nowater <- king_county %>% 
  st_erase(kc_water)

seattle <- kc_nowater %>% 
  select(GEOID, geometry) %>%
  filter(as.numeric(str_sub(GEOID, -5, -1)) < 13000)

general %>% 
  rename_all(~tolower(str_replace_all(str_remove(., "\\?")," ", "_"))) %>%
  filter(issue_category %in% c("Unauthorized Camping")) %>%
  mutate(year = lubridate::year(created_date)) %>%
  count(year)

general %>% 
  rename_all(~tolower(str_replace_all(str_remove(., "\\?")," ", "_"))) %>%
  filter(issue_category %in% c("Unauthorized Camping")) %>%
  pull(created_date) %>% max()

general %>% 
  rename_all(~tolower(str_replace_all(str_remove(., "\\?")," ", "_"))) %>%
  filter(issue_category %in% c("Unauthorized Camping")) %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
                      crs = 2285,
                      agr = "identity") %>%
  st_transform(crs = 4326) %>%
  mutate(x_coord = st_coordinates(.)[,1], y_coord=st_coordinates(.)[,2]) %>%
  filter(y_coord < 47.7413 & y_coord > 47.493) %>%
  st_drop_geometry() %>%
  mutate(year = lubridate::year(created_date)) %>%
  qmplot(x = x_coord, y = y_coord, data = ., zoom=12, size=I(0.5), color=I("blue")) + facet_wrap(~year, ncol = 4)

general %>% 
  rename_all(~tolower(str_replace_all(str_remove(., "\\?")," ", "_"))) %>%
  filter(issue_category %in% c("Unauthorized Camping")) %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 4326) %>%
  mutate(x_coord = st_coordinates(.)[,1], y_coord=st_coordinates(.)[,2]) %>%
  filter(y_coord < 47.7413 & y_coord > 47.493) %>%
  st_drop_geometry() %>%
  mutate(year = lubridate::year(created_date)) %>%
  qmplot(x = x_coord, y = y_coord, data = ., zoom=12, geom="blank", darken = 0.5) +
  stat_density_2d(
    aes(fill = stat(level)),
    geom = "polygon", 
    alpha = .2, color = NA, n = 200) + 
  scale_fill_gradient2(
    "Incident\nConcentration", 
    low = "white", 
    mid = "yellow", 
    high = "red") +
  facet_wrap(~year, ncol = 4)
  
general %>% 
  rename_all(~tolower(str_replace_all(str_remove(., "\\?")," ", "_"))) %>%
  filter(issue_category %in% c("Unauthorized Camping")) %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 4326) %>%
  mutate(x_coord = st_coordinates(.)[,1], y_coord=st_coordinates(.)[,2]) %>%
  filter(y_coord < 47.7413 & y_coord > 47.493) %>%
  st_drop_geometry() %>%
  mutate(year = lubridate::year(created_date)) %>%
  count(year)


general %>% 
  rename_all(~tolower(str_replace_all(str_remove(., "\\?")," ", "_"))) %>%
  filter(issue_category %in% c("Unauthorized Camping")) %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 4326) %>%
  mutate(x_coord = st_coordinates(.)[,1], y_coord=st_coordinates(.)[,2]) %>%
  filter(y_coord < 47.7413 & y_coord > 47.493) %>%
  st_drop_geometry() %>%
  mutate(year = lubridate::year(created_date), month = lubridate::month(created_date)) %>%
  ggplot(aes(x=month, y = stat(count))) + geom_histogram(bins=12) + facet_wrap(~year) + scale_x_continuous(breaks = 1:12)


general %>% filter(method_received == "Find It Fix It Apps") %>% 
  group_by(lubridate::year(created_date)) %>% 
  distinct(mobile_device_id) %>% tally() %>% rename(year = `lubridate::year(created_date)`, unique_users = n)

general %>% filter(method_received == "Find It Fix It Apps") %>% 
  group_by(lubridate::year(created_date)) %>% 
  count(mobile_device_id) %>% 
  group_by(`lubridate::year(created_date)`) %>% summarize(avg_reports_per_id = mean(n))

general %>% 
  filter(issue_category == "Unauthorized Camping") %>%
  filter(method_received == "Find It Fix It Apps") %>%
  count(mobile_device_id) %>% arrange(desc(n)) %>%
  slice(1:70) %>% pull(n) %>% sum()

general %>% 
  filter(issue_category == "Unauthorized Camping") %>%
  filter(method_received == "Find It Fix It Apps") %>%
  count(mobile_device_id) %>% arrange(desc(n)) %>%
  print(n=50)

# This person is a beast
general %>% filter(mobile_device_id == "59d6c343ff03046795dc191e") %>% 
  select(issue_category, what_is_the_nature_of_your_inquiry) %>%
  print(n=50)

general %>% filter(mobile_device_id == "59d6c343ff03046795dc191e") %>%
  filter(!is.na(x_value) & !is.na(y_value) & x_value > 0 & y_value > 0) %>%
  st_as_sf(coords = c("x_value", "y_value"), 
           crs = 2285,
           agr = "identity") %>%
  st_transform(crs = 4326) %>%
  mutate(x_coord = st_coordinates(.)[,1], y_coord=st_coordinates(.)[,2]) %>%
  filter(y_coord < 47.7413 & y_coord > 47.493) %>%
  st_drop_geometry() %>%
  qmplot(x = x_coord, y = y_coord, data = .)

