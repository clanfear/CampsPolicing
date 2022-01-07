library(tidyverse)
library(sf)
library(ggmap)

load("./data/derived/tent_census.RData")

# tent_census <- tent_census %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(3690)

# tent_census %>%

qmplot(x = longitude, y = latitude, data = tent_census, source = "stamen", maptype = "terrain-lines", mapcolor = "bw", geom="blank") +
  geom_point(color = "purple", alpha = 0.5, size = 3)
