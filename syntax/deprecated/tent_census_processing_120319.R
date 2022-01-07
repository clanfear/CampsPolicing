library(tidyverse)
library(ggmap)
tent_census <- readxl::read_excel("./data/raw/Tent Census_April-August2019.xlsx")
tent_census <- tent_census %>%
  mutate(coordinate = str_extract(URL, "[0-9\\.]*,-[0-9\\.]*$")) %>% 
  mutate(coordinate = case_when(
    Title == "4799-4701 9th Ave NW" ~ "47.663364,-122.370718", 
    URL == "https://www.google.com/maps/place/Seattle/data=!4m2!3m1!1s0x54906a961df6ee21:0x8119b0b7d5eea108" ~ "47.595891,-122.3219297",
    URL == "https://www.google.com/maps/place/Elliott+Bay+Trail/data=!4m2!3m1!1s0x54906ab27ec1f023:0x51ba1d0307faef5e" ~ "47.6077857,-122.3443708",
    TRUE ~ coordinate)) %>%
  separate(coordinate, c("latitude", "longitude"), ",", remove=FALSE, convert=TRUE)

tent_census <- tent_census %>%
  mutate(note_upper = str_to_upper(Note)) %>%
  mutate(note_clean = str_remove(note_upper, "^.*//")) %>%
  mutate(note_clean = str_remove_all(note_clean, 
    "(ABANDONED |TREE |NATURAL |WOODEN |LARGE |ORANGE |SMALL NORTHFACE |SHARK-LOOKING |GREEN |DILAPITATED |LITTLE |BROWN |YELLOW |BIG |BLUE |GREY |HUGE |SM |LG |CARDBOARD |THREE PART )")) %>%
  mutate(n_tents = ifelse(str_detect(note_clean, "TENT"), 
                          str_extract(note_clean, "[0-9][0-9]? TENT(S)?"), 0)) %>%
  mutate(n_tents = as.numeric(ifelse(is.na(n_tents), 0, str_extract(n_tents, "\\d+")))) %>%
  mutate(n_tents = case_when(
    str_detect(Note, "Camp, cannot see in 6/20 11:21") ~ 5, 
    str_detect(Note, "Bottom of 8 tents 6/13 10:33 am") ~ 4,
    str_detect(Note, "At least one t one s 7/15 2:30") ~ 1,
    TRUE ~ n_tents)) %>%
  mutate(n_structures = ifelse(str_detect(note_clean, "STRUCTURE"), 
                               str_extract(note_clean, "[0-9][0-9]? STRUCTURE(S)?"), 0)) %>%
  mutate(n_structures = as.numeric(ifelse(is.na(n_structures), 0, str_extract(n_structures, "\\d+")))) %>%
  mutate(n_structures = case_when(
    str_detect(Note, "At least one t one s 7/15 2:30") ~ 1,
    str_detect(note_upper, "6 TENTS 3 STRUCTURES 6/24 10:59") ~ 3,
    str_detect(note_upper, "AT LEAST 4 STRUCTURES 6/25 AFTERNOON") ~ 5,
    str_detect(note_upper, "1 HUGE STRUCTURE 5/13 11:00 AM") ~ 5,
    TRUE ~ n_structures
  )) %>%
  mutate(n_dwellings = n_tents + n_structures) %>%
  # mutate(trash = ifelse(str_detect(note_upper, "TRASH"), 1, 0)) %>%
  # mutate(trash = ifelse(str_detect(note_upper, "NO TRASH"), 0, trash)) %>%
  distinct(latitude, longitude, n_tents, n_structures, n_dwellings, Note)

  qmplot(x = longitude, y = latitude, data = tent_census, color = I("darkred"), alpha = I(0.5), size = n_dwellings) %>%
  ggsave("quick_tent_plot.png", plot=., width=8.5, height=11, units="in")
  
  ggmap(get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw")) + 
    geom_point(data = tent_census, aes(x = longitude, y = latitude, size = n_dwellings), alpha = 0.5, color = "red")
  
save(tent_census, file = "./data/derived/tent_census.RData")
write_csv(tent_census, path = "./data/derived/tent_census.csv")
