library(tidyverse)
library(sf)
library(ggmap)

# Load the sample and resample
load("./data/derived/tent_census_resample.RData")
load("./data/derived/tent_census.RData")

#---


#---
tent_census_resample <- tent_census_resample %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
tent_census <- tent_census %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
ggplot(tent_census_resample) + geom_sf(aes(size = n_dwellings))

#---
tent_census_uncount <- tent_census %>%
 # st_transform(3857) %>%
  uncount(n_dwellings) %>%
  st_as_sf() %>%
  mutate(longitude = st_coordinates(.)[,1], latitude = st_coordinates(.)[,2])

ggmap(get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw")) + 
  coord_cartesian() + 
  geom_hex(data = tent_census_uncount, aes(x = longitude, y = latitude), bins = 60) +
  scale_fill_viridis_c()

test <- get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw")
#---
# Maybe create an expanded buffer around new data, then subsample old data to that area, then compare counts

resample_area <- tent_census_resample %>% mutate(geometry = st_buffer(geometry, dist = 0.0025)) %>% st_geometry() %>% st_union() %>% st_combine()

tent_census %>% filter(!is.na(as.numeric(st_intersects(geometry, resample_area)))) %>% st_drop_geometry() %>% summarize(total_count = sum(n_dwellings))
tent_census_resample %>% summarize(total_count = sum(n_dwellings))
ggplot(resample_area) + geom_sf(data = resample_area)

ggmap(get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw")) + 
  geom_sf(data = resample_area)

plot(st_transform(st_set_crs(resample_area, 4326), 3857), bgMap = get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw"), col = "red")

