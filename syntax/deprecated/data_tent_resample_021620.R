library(tidyverse)
library(sf)
library(ggmap)

tent_census_resample <- readxl::read_excel("./data/raw/Tent Census Resample 2019.xlsx")
tent_census_resample <- tent_census_resample %>%
  mutate(Coordinates = str_replace_all(Coordinates, c("Â?°"="d",
                                                                       "'" = "m",
                                                                       "\"" = "s"))) %>%
  separate(Coordinates, into = c("latitude", "longitude"), sep = " ") %>%
  mutate_at(vars(latitude, longitude), ~  as.numeric(sp::char2dms(., chd = "d", chm = "m", chs = "s"))) %>%
  rename(note = Label, url = URL, comment = Comment)
  
#tent_census_resample <- 
tent_census_resample <- tent_census_resample %>%
  mutate(note_upper = str_to_upper(note)) %>%
  mutate(note_clean = str_remove(note_upper, "^.*//")) %>%
  mutate(note_clean = str_remove_all(note_clean, 
                                     "(WOOD |UNSET |ABANDONED |TREE |NATURAL |WOODEN |LARGE |ORANGE |SMALL NORTHFACE |SHARK-LOOKING |GREEN |DILAPITATED |LITTLE |BROWN |YELLOW |BIG |BLUE |GREY |HUGE |SM |LG |CARDBOARD |THREE PART )")) %>%
  mutate(note_clean = str_replace(note_clean, "(TENT(S)?/STRUCTURE(S)?)(\\.|S)?", "BOTH")) %>%
  mutate(note_clean = str_trim(str_remove_all(note_clean, "([0-9][0-9]?/[0-9][0-9]?)|(\\.)|([0-9][0-9]?:[0-9][0-9]?( )?(AM|PM)?)|(~)|(\\+)"))) %>%
  mutate(n_tents = case_when(
    str_detect(note_clean, "TENT") ~ as.numeric(str_extract(str_extract(note_clean, "[0-9][0-9]? TENT(S)?"), "\\d+")),
    str_detect(note_clean, "BOTH") & as.numeric(str_extract(str_extract(note_clean, "[0-9][0-9]? BOTH"), "\\d+")) == 1 ~ 1,
    str_detect(note_clean, "BOTH") ~ as.numeric(str_extract(str_extract(note_clean, "[0-9][0-9]? BOTH"), "\\d+")) %/% 2 + 
      as.numeric(str_extract(str_extract(note_clean, "[0-9][0-9]? BOTH"), "\\d+")) %% 2,
    TRUE ~ 0
  )) %>%
  mutate(n_structures = case_when(
    str_detect(note_clean, "STRUCT") ~ as.numeric(str_extract(str_extract(note_clean, "[0-9][0-9]? (STRUCT|STRUCTURE(S)?)"), "\\d+")),
    str_detect(note_clean, "BOTH") & as.numeric(str_extract(str_extract(note_clean, "[0-9][0-9]? BOTH"), "\\d+")) == 1 ~ 0,
    str_detect(note_clean, "BOTH") ~ as.numeric(str_extract(str_extract(note_clean, "[0-9][0-9]? BOTH"), "\\d+")) %/% 2,
    TRUE ~ 0
  )) %>%
  mutate(n_dwellings = n_tents + n_structures) %>%
  # mutate(trash = ifelse(str_detect(note_upper, "TRASH"), 1, 0)) %>%
  # mutate(trash = ifelse(str_detect(note_upper, "NO TRASH"), 0, trash)) %>%
  distinct(latitude, longitude, n_tents, n_structures, n_dwellings, note)

qmplot(x = longitude, y = latitude, data = tent_census_resample, color = I("darkred"), alpha = I(0.5), size = n_dwellings) 

qmplot(x = longitude, y = latitude, data = tent_census_resample, geom = "blank") +
  coord_equal() +
  geom_hex(data =tent_census_resample, aes(x = longitude, y = latitude), bins = 50)

ggmap(get_map(location = "Seattle", zoom = 11, maptype = "satellite", color = "bw")) + 
  geom_point(data = tent_census_resample, aes(x = longitude, y = latitude, size = n_dwellings), alpha = 0.5, color = "red")

ggplot(tent_census_resample, aes(x = longitude, y = latitude)) + geom_hex(bins = 50)

save(tent_census_resample, file = "./data/derived/tent_census_resample.RData")
write_csv(tent_census_resample, path = "./data/derived/tent_census_resample.csv")
