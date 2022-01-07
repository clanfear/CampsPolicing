library(tidyverse)
library(sf)
library(ggmap)

# Load the sample and resample
load("./data/derived/tent_census_resample.RData")
load("./data/derived/tent_census.RData")
load("./data/derived/tent_census_summer_2020.RData")
#---
tent_census_uncount <- 
  bind_rows(tent_census_resample %>% mutate(sample = "Resample 1"),
            tent_census %>% mutate(sample = "Census") %>% rename(note = Note),
            tent_census_summer_2020 %>% mutate(sample = "Resample 2")) %>%
  select(-n_dwellings) %>%
  pivot_longer(n_tents:n_structures, names_to = "type") %>% 
  uncount(value) %>%
  mutate(type = str_remove_all(type, "n_|s$")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove=FALSE) %>% 
  mutate(latitude = st_coordinates(.)[,2], 
         longitude = st_coordinates(.)[,1]) %>%
  st_transform(3689) %>%
  mutate(lat_wa = st_coordinates(.)[,2], 
         lon_wa = st_coordinates(.)[,1]) %>%
  st_transform(4326)

save(tent_census_uncount, file = "./data/derived/tent_census_uncount")
  
# #---
tent_census_resample <- tent_census_resample %>%
   st_as_sf(coords = c("longitude", "latitude"), crs = 3689)
 tent_census <- tent_census %>%
   st_as_sf(coords = c("longitude", "latitude"), crs = 3689)
 tent_census_summer_2020 <- tent_census_summer_2020 %>%
   st_as_sf(coords = c("longitude", "latitude"), crs = 3689)
 # ggplot(tent_census_resample) + geom_sf(aes(size = n_dwellings))

# #---
# tent_census %>% 
#   select(starts_with("n_"), geometry) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 3689)


 st_erase <- function(x, y) {
   st_difference(x, st_make_valid(st_union(st_combine(y))))
 }
seattle <- tigris::tracts("WA", "King") %>%
   filter(GEOID < 53033013000 & GEOID > 53033000100) %>% 
   st_erase(tigris::area_water("WA", "King", class = "sf"))
 
 

  current_map <- ggmap(get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw")) + 
    coord_cartesian() + 
    geom_hex(data = tent_census_uncount %>% filter(sample == current_sample), aes(x = longitude, y = latitude), binwidth = c(.003,.003)) +
    # facet_wrap(~ sample) + 
    scale_fill_viridis_c() + coord_fixed(ratio = 0.8)
  ggsave(filename = paste0(current_sample, ".png"), path = "img/", plot = current_map)


census_animation_base <-  ggmap(get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw")) + 
    coord_cartesian() + 
    geom_point(data = tent_census_uncount, aes(x = longitude, y = latitude, group = sample, color = sample)) +
    transition_states(sample, transition_length = 1, state_length = 3) + 
    enter_fade() + exit_shrink() +
    ggtitle("Now showing {closest_state}")
    
census_animation <- gganimate::animate(plot = census_animation_base, width = 1000, height = 1000, nframes = 30)
anim_save("census_animation.gif", animation = census_animation, path = "img/")

ggplot(tent_census_uncount, aes(x = lon_wa, y = lat_wa)) + 
  coord_fixed(ratio=1) +
  geom_hex(binwidth = 500)  + facet_wrap(~ sample) + 
  scale_fill_viridis_c() 

#---
# Maybe create an expanded buffer around new data, then subsample old data to that area, then compare counts

resample_area <- tent_census_resample %>% mutate(geometry = st_buffer(geometry, dist = 0.0025)) %>% st_geometry() %>% st_union() %>% st_combine()

tent_census %>% filter(!is.na(as.numeric(st_intersects(geometry, resample_area)))) %>% st_drop_geometry() %>% summarize(total_count = sum(n_dwellings))
tent_census_resample %>% summarize(total_count = sum(n_dwellings))

#

resample_area_2 <- tent_census_summer_2020 %>% mutate(geometry = st_buffer(geometry, dist = 0.0025)) %>% st_geometry() %>% st_union() %>% st_combine()

tent_census %>% filter(!is.na(as.numeric(st_intersects(geometry, resample_area_2)))) %>% st_drop_geometry() %>% summarize(total_count = sum(n_dwellings))
tent_census_summer_2020 %>% summarize(total_count = sum(n_dwellings))


#

ggplot(resample_area) + geom_sf(data = resample_area)

ggmap(get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw")) + 
  geom_sf(data = resample_area)

plot(st_transform(st_set_crs(resample_area, 4326), 3857), bgMap = get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw"), col = "red")

