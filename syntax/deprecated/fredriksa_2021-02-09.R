library(tidyverse)
library(sf)
library(ggmap)
load("./data/derived/tent_census_uncount")
neighborhoods <- st_read("./data/raw/city_clerk_neighborhoods/City_Clerk_Neighborhoods.shp") %>% 
  filter(!is.na(S_HOOD) & S_HOOD != "OOO")

streets <- st_read("./data/raw/Street_Network_Database_SND/Street_Network_Database_SND.shp") 

streets %>% st_drop_geometry() %>% count(SND_FEACOD)
tent_census_jitter <- tent_census_uncount

st_geometry(tent_census_jitter) <- st_jitter(st_geometry(tent_census_uncount), factor = 0.003) 
tent_census_uncount %>% ggplot() + geom_sf()
tent_census_jitter %>% ggplot() + geom_sf()
tent_census_jitter <-tent_census_jitter %>%
  select(geometry, sample) %>%
  mutate(sample = case_when(
    sample == "Census"     ~ "Summer 2019",
    sample == "Resample 1" ~ "Autumn 2019",
    sample == "Resample 2" ~ "Summer 2020"
  )) %>%
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1]) %>% 
  st_drop_geometry()

write_csv(tent_census_jitter, file = "./tent_census_jitter_sample_id.csv")

tent_census_jitter_agg <- tent_census_uncount %>% 
  st_drop_geometry() %>% 
  count(sample, latitude, longitude) %>%
  mutate(sample = case_when(
    sample == "Census"     ~ "Summer 2019",
    sample == "Resample 1" ~ "Autumn 2019",
    sample == "Resample 2" ~ "Summer 2020"
  ))
write_csv(tent_census_jitter_agg, file = "./tent_census_jitter_agg.csv")

tent_census_jitter_agg %>% st_jitter(factor = 0.004) %>% ggplot() + coord_sf()

tent_census <- tent_census_uncount %>%
  st_transform(3689) 
ggplot(tent_census %>% st_jitter(factor = 0.004)) +
  coord_sf() +
  geom_sf(size = 1, alpha = 0.1, color = "purple") +
  geom_sf(data = streets %>%
            filter(SND_FEACOD %in% c("State Highway", "Interstate Highway", "Major Street")) %>% 
            st_transform(3689), alpha = 0.3, size = 0.1) +
  theme_void(base_size = 20) + facet_wrap(~sample)
  # stat_density_2d(mapping = ggplot2::aes(x = purrr::map_dbl(geometry, ~.[1]),
  #                                        y = purrr::map_dbl(geometry, ~.[2]),
  #                                        fill = stat(density)),
  #                 geom = 'raster',
  #                 contour = FALSE,
  #                 alpha = 0.75,
  #                 adjust = .5,
  #                 n = 200) + scale_fill_viridis_c(option = "cividis")


tent_census_uncount %>%
  mutate(lat = st_coordinates(.)[,2],
         lon = st_coordinates(.)[,1]) %>%
  st_drop_geometry() %>%
  qmplot(x=lon, y = lat, data =., geom = "blank", darken = .5) + 
  facet_wrap(~sample) +
  coord_cartesian() +
  stat_density_2d(mapping = aes(x=lon, y=lat, fill = stat(density)),
                  geom = 'raster',
                  contour = FALSE,
                  alpha = 0.75) + 

# 200 ft buffer around interstate highways

interstate_buffer <- streets %>% 
  filter(SND_FEACOD == "Interstate Highway") %>% 
  st_transform(3689) %>% 
  st_union() %>%
  st_buffer(dist = units::set_units(200, "feet"))

tent_census_uncount %>%
  st_transform(3689) %>% 
  mutate(in_buffer = as.logical(st_intersects(geometry, interstate_buffer, sparse=FALSE))) %>%
  ggplot(aes(color = in_buffer)) +facet_wrap(~sample) + geom_sf(data = streets %>% 
                                                                  filter(SND_FEACOD == "Interstate Highway") %>% 
                                                                  st_transform(3689) %>% 
                                                                  st_union(), inherit.aes = FALSE) + geom_sf()

# #1: N of individual tens along highways vs not highway out of total 839. All samples, separately.
tent_census_uncount %>%
  st_transform(3689) %>% 
  mutate(in_buffer = as.logical(st_intersects(geometry, interstate_buffer, sparse=FALSE))) %>%
  st_drop_geometry() %>%
  count(sample, in_buffer) %>%
  group_by(sample) %>%
  mutate(prop = n / sum(n))

tent_census_uncount %>%
  group_by(note, latitude, longitude) %>%
  filter(n()==1) %>%
  ungroup() %>%
  st_transform(3689) %>% 
  mutate(in_buffer = as.logical(st_intersects(geometry, interstate_buffer, sparse=FALSE))) %>%
  st_drop_geometry() %>%
  count(sample, in_buffer) %>%
  group_by(sample) %>%
  mutate(prop = n / sum(n))

tent_census_uncount %>%
  group_by(note, latitude, longitude) %>%
  filter(n()>1) %>%
  slice(1L) %>%
  ungroup() %>%
  st_transform(3689) %>% 
  mutate(in_buffer = as.logical(st_intersects(geometry, interstate_buffer, sparse=FALSE))) %>%
  st_drop_geometry() %>%
  count(sample, in_buffer) %>%
  group_by(sample) %>%
  mutate(prop = n / sum(n))

tent_census_uncount %>%
  group_by(note, latitude, longitude) %>%
  filter(n()>1) %>%
  ungroup() %>%
  st_transform(3689) %>% 
  mutate(in_buffer = as.logical(st_intersects(geometry, interstate_buffer, sparse=FALSE))) %>%
  st_drop_geometry() %>%
  count(sample, in_buffer) %>%
  group_by(sample) %>%
  mutate(prop = n / sum(n))

# #2: Separate counts for solitaries and encampments

# #3: Resample corresponding to original using better method than rough buffer I used
st_intersects(tent_census_uncount %>%
                st_transform(3689), interstate_buffer, sparse=FALSE)

# Neighborhoods
neighborhoods %>%
  st_join(tent_census_uncount)

tent_census_neighb <- tent_census_uncount %>%
  st_join(neighborhoods, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  mutate(OBJECTID = as.character(OBJECTID)) %>%
  group_by(OBJECTID) %>%
  count(sample, type, .drop=FALSE) %>%
  complete(OBJECTID= unique(as.character(OBJECTID)), sample, type, fill = list(n = 0)) %>%
  arrange(OBJECTID , sample, type)
  

neighborhoods %>%
  left_join(tent_census_neighb)

  right_join(tent_census_uncount)
   %>%
  complete(sample, type, fill = list(n = 0))
  right_join(neighborhoods) %>% 
  select(OBJECTID, sample, type, n, S_HOOD, L_HOOD, geometry) %>% 
  complete(OBJECTID, fill = list(n = 0)) 
  st_as_sf()



  mutate(lat = st_coordinates(.)[2], lon = st_coordinates(.)[1])
  ggplot(aes(color = outside, size = outside))  + 
    geom_sf(data=neighborhoods, inherit.aes = FALSE) + geom_sf()

  neighborhoods  %>%
  ggplot() + geom_sf() + geom_sf_label(aes(label = OBJECTID))
