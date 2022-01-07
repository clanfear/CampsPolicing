library(tidyverse)
library(sf)
library(ggmap)
library(ragg)
ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")
# Load the sample and resample
load("./data/derived/tent_census_resample.RData")
load("./data/derived/tent_census.RData")
load("./data/derived/tent_census_summer_2020.RData")

streets <- st_read("./data/raw/Street_Network_Database_SND/Street_Network_Database_SND.shp") %>%
  st_transform(3689)

tent_census_resample <- tent_census_resample %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
tent_census <- tent_census %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
tent_census_summer_2020 <- tent_census_summer_2020 %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tent_census_counts <- 
  bind_rows(tent_census_resample %>% mutate(sample = "Resample 1"),
            tent_census %>% mutate(sample = "Census") %>% rename(note = Note),
            tent_census_summer_2020 %>% mutate(sample = "Resample 2")) %>%
  select(-n_tents, - n_structures) %>%
  mutate(latitude = st_coordinates(.)[,2], 
         longitude = st_coordinates(.)[,1]) %>%
  st_transform(3689) %>%
  mutate(lat_wa = st_coordinates(.)[,2], 
         lon_wa = st_coordinates(.)[,1]) %>%
  mutate(sample_period = fct_recode(sample, 
     `Census\nApr-Aug 2019` = "Census",
     `Resample 1\nOct-Dec 2019` = "Resample 1",
     `Resample 2\nJuly 2020` = "Resample 2"
  ))

tent_census_agg <- tent_census_counts %>%
  st_drop_geometry() %>%
  group_by(sample_period) %>%
  summarize(Total = sum(n_dwellings))

st_erase <- function(x, y) {
   st_difference(x, st_make_valid(st_union(st_combine(y))))
}

seattle_tracts <- tigris::tracts("WA", "King") %>%
   filter(GEOID < 53033013000 & GEOID > 53033000100) 

seattle <- seattle_tracts %>% st_transform(3689)  %>% 
  st_union() %>% st_make_valid()

seattle_nowater <- seattle %>%   
  st_erase(tigris::area_water("WA", "King", class = "sf")  %>% st_transform(3689))

streets_filtered <- streets %>%
  filter(SND_FEACOD %in% c("State Highway", "Interstate Highway") | str_detect(ORD_STNAME, "AURORA") | (str_detect(ORD_STNAME, "EAST MARGINAL WAY") & L_ADRS_FRO < 6500)) %>%
           select(geometry) %>%
  st_intersection(seattle)

presentation_plot <- ggplot() + 
  geom_sf(data = seattle_nowater) + 
  geom_sf(data = streets_filtered, color = "#405b70") + 
  geom_sf(data = tent_census_counts, alpha = 0.5, 
          aes(size = n_dwellings), 
          color = "black", shape = 21, fill = "#FBC928") + 
  facet_wrap(~ sample_period) +
  scale_size_continuous("Number of Tents or Structures", breaks = c(1, 5, 25, 50)) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "#b4d1df"), 
        legend.position = "bottom",
        strip.text = element_text(size = rel(2), family = "Times New Roman", color = "#405b70", margin = margin(b=2)),
        legend.title = element_text(size = rel(1.4), family = "Times New Roman", color = "#405b70"))

ggsave(filename = "./img/presentation_plot.png" , plot = presentation_plot, device = ragg_png, width = 11, height = 8.5)

presentation_plot_alt <- ggplot() + 
  geom_sf(data = seattle_nowater, fill = "#b4d1df") + 
  geom_sf(data = streets_filtered, color = "#405b70") + 
  geom_sf(data = tent_census_counts, alpha = 0.5, 
          aes(size = n_dwellings), 
          color = "black", shape = 21, fill = "#FBC928") + 
  facet_wrap(~ sample_period) +
  scale_size_continuous("Number of Tents or Structures", breaks = c(1, 5, 25, 50)) +
  theme_void() + 
  coord_sf(clip = "off") +
  theme(panel.background = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = rel(2), family = "Times New Roman", color = "#405b70", margin = margin(b=2)),
        strip.placement = "inside",
        legend.title = element_text(size = rel(1.4), family = "Times New Roman", color = "#405b70"))

ggsave(filename = "./img/presentation_plot_alt.png" , plot = presentation_plot_alt, device = ragg_png, width = 11, height = 8.5)