library(tidyverse)
library(sf)
library(janitor)
library(tidycensus)


# What do I need here?
## Social Disorg
### Heterogeneity
### Stability
### Disadvantage
## Families with children
## Housing Type
## Home Ownership
## Population
## LAND Area
## Density

acs5_2018_vars <- load_variables(2018, "acs5", cache=TRUE)

var_vector <- c("C17002_001", "C17002_002", "C17002_003", # % under poverty line
                "B19057_001", "B19057_002", # %public assistance
                "B11001_001", "B11001_006", # % FHH
                "B23025_001", "B23025_005", # % unemployed
                "B01001_001" , "B01001_003", "B01001_004", "B01001_005", "B01001_006", # % under 18 M
                               "B01001_027", "B01001_028", "B01001_029", "B01001_030", # % under 18 F
                "B02001_001", "B02001_003", # % black
                              "B02001_005", #  count asian
                              "B02001_006", "B02001_004", #  count NHOPI, AIAN
                "B05002_001", "B05002_013", # foreign born
                "B03001_001", "B03001_003", # hispanic or latino
                "B25003_001", "B25003_002", # owner occupied
                "B23010_001", "B23010_002", # own children under 18
                "B01003_001",# population
                "B19126_001") # median family income

# S&R heterogeneity: % latino, # foreign born; I should add asian

acs5_2018_vars %>% filter(name %in% var_vector) %>% print(n=50)

acs5_2018_vars %>% filter(str_detect(str_to_lower(label), "owner")) %>% print(n=100)
acs5_2018_vars %>% filter(str_detect(concept,"POPULATION")) %>%  print(n=500)
acs5_2018_vars %>% filter(str_detect(concept,"MEDIAN FAMILY INCOME")) %>% print(n=500)
acs5_2018_vars %>% filter(name == "B19126_001")

# AREA
load("./data/derived/seattle_tract.RData")
load("./data/derived/seattle_bg.RData")

kc_water <- tigris::area_water("WA", "King", class = "sf") %>%
  st_transform(3689)
st_erase <- function(x, y) {
  st_difference(x, st_make_valid(st_union(st_combine(y))))
}
seattle_bg_nowater <- seattle_bg %>% 
  st_erase(kc_water)
seattle_tract_nowater <- seattle_tract %>% 
  st_erase(kc_water)

# TRACT
acs5_2018_tract_data <- get_acs("tract", variables = var_vector, year = 2018, state = "WA",
        county = "King", output = "wide", geometry = FALSE) %>%
  filter(GEOID %in% seattle_tract$GEOID) %>%
  select(-ends_with("M")) %>%
  rename_with(~ str_remove(., "E$"), .cols = c(-GEOID, -NAME)) %>%
  transmute(GEOID      = GEOID,
            pop        = B01003_001,
            median_inc = B19126_001,
            pr_poverty    = (C17002_002 + C17002_003) / C17002_001,
            pr_pub_assist = B19057_002 / B19057_001,
            pr_fhh        = B11001_006 / B11001_001,
            pr_under_18   = (B01001_003 + B01001_004 + B01001_005 + B01001_006 + B01001_027 + B01001_028 + B01001_029 + B01001_030) / B01001_001,
            pr_black   = B02001_003 / B02001_001,
            pr_asian   = B02001_005 / B02001_001,
            pr_nhopi   = B02001_006 / B02001_001,
            pr_aian    = B02001_004 / B02001_001,
            pr_foreign = B05002_013 / B05002_001,
            pr_hisp    = B03001_003 / B03001_001,
            pr_ownhome = B25003_002 / B25003_001,
            pr_children = B23010_002/B23010_001
            )

acs5_2018_tract_data <- acs5_2018_tract_data %>% 
  left_join(seattle_tract_nowater, by = "GEOID") %>%
  st_as_sf() %>%
  mutate(area_sqkm = as.numeric(units::set_units(st_area(.), "km^2"))) %>% 
  mutate(pop_sqkm = as.numeric(pop / area_sqkm))

save(acs5_2018_tract_data, file = "./data/derived/acs5_2018_tract_data.RData")

# BG
acs5_2018_bg_data <- get_acs("block group", variables = var_vector, year = 2018, state = "WA",
                          county = "King", output = "wide", geometry = FALSE) %>%
  filter(GEOID %in% seattle_bg$GEOID) %>%
  select(-ends_with("M")) %>%
  rename_with(~ str_remove(., "E$"), .cols = c(-GEOID, -NAME)) %>%
  transmute(GEOID      = GEOID,
            pop        = B01003_001,
            #median_inc = B19126_001,
            pr_poverty    = (C17002_002 + C17002_003) / C17002_001,
            pr_pub_assist = B19057_002 / B19057_001,
            pr_fhh        = B11001_006 / B11001_001,
            pr_under_18   = (B01001_003 + B01001_004 + B01001_005 + B01001_006 + B01001_027 + B01001_028 + B01001_029 + B01001_030) / B01001_001,
            pr_black   = B02001_003 / B02001_001,
            pr_asian   = B02001_005 / B02001_001,
            pr_nhopi   = B02001_006 / B02001_001,
            pr_aian    = B02001_004 / B02001_001,
            # pr_foreign = B05002_013 / B05002_001,
            # pr_hisp    = B03001_003 / B03001_001,
            pr_ownhome = B25003_002 / B25003_001,
            #pr_children = B23010_002/B23010_001
            ) # No foreign born, hispanic, median income, or children at BG level

acs5_2018_bg_data <- acs5_2018_bg_data %>% 
  left_join(seattle_bg_nowater, by = "GEOID") %>%
  st_as_sf() %>%
  mutate(area_sqkm = as.numeric(units::set_units(st_area(.), "km^2"))) %>% 
  mutate(pop_sqkm = as.numeric(pop / area_sqkm))

save(acs5_2018_bg_data, file = "./data/derived/acs5_2018_bg_data.RData")