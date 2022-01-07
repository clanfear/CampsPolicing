library(tidyverse)
library(sf)

load("./data/derived/seattle_tract.RData")
load("./data/derived/seattle_bg.RData")

tent_cities_tiny_houses_19 <- readxl::read_excel("./data/raw/City-Sanctioned Tiny House Villages and Tent Encampments Spring and Summer 2019.xlsx")  %>%
  mutate(Coordinates = str_replace(Coordinates, "^47.", "47d")) %>%
  mutate(Coordinates = str_replace(Coordinates, " 122.", " 122d")) %>%
  mutate(Coordinates = str_replace_all(Coordinates, c("'" = "m",
                                                      "\"" = "s"))) %>%
  separate(Coordinates, into = c("latitude", "longitude"), sep = " ") %>%
  mutate_at(vars(latitude, longitude), ~  as.numeric(sp::char2dms(., chd = "d", chm = "m", chs = "s"))) %>%
  rename(note = Label, url = URL, comment = Comment) %>%
  mutate(type = case_when(
    is.na(note) ~ NA_character_,
    str_detect(note, "tiny") ~ "Tiny House Village",
    str_detect(note, "tent") ~ "Tent City",
    TRUE ~ NA_character_
  )) %>%
  mutate(count = as.numeric(str_extract(note, "[0-9][0-9]"))) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3689) %>%
  select(-comment, -url, -note)

villages_tract <- seattle_tract %>%
  st_join(tent_cities_tiny_houses_19) %>%
  replace_na(list(count = 0, type = "None")) %>%
  st_drop_geometry() %>%
  mutate(any_village = ifelse(type != "None", "Village", "None"))

save(villages_tract, file = "./data/derived/villages_tract.RData")

villages_bg <- seattle_bg %>%
  st_join(tent_cities_tiny_houses_19) %>%
  replace_na(list(count = 0, type = "None")) %>%
  st_drop_geometry() %>%
  mutate(any_village = ifelse(type != "None", "Village", "None"))

save(villages_bg, file = "./data/derived/villages_bg.RData")