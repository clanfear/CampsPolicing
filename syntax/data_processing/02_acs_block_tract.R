library(tidyverse)
library(sf)
library(janitor)
library(tidycensus)

acs5_2018_vars <- load_variables(2019, "acs5", cache=TRUE)

var_vector <- c("C17002_001", "C17002_002", "C17002_003", # % under poverty line
                "B19057_001", "B19057_002", # %public assistance
                "B11001_001", "B11001_006", # % FHH
                "B23025_001", "B23025_005", # % unemployed
                "B01001_001" , "B01001_003", "B01001_004", "B01001_005", "B01001_006", # % under 18 M
                               "B01001_027", "B01001_028", "B01001_029", "B01001_030", # % under 18 F
                "B02001_001", "B02001_003", # % black
                              "B02001_002", #  count white
                              "B02001_005", #  count asian
                              "B02001_006", "B02001_004", #  count NHOPI, AIAN
                "B05002_001", "B05002_013", # foreign born
                "B03001_001", "B03001_003", # hispanic or latino
                "B25003_001", "B25003_002", # owner occupied
                "B23010_001", "B23010_002", # own children under 18
                "B01003_001", # population
                "B19126_001", # median family income
                "B07013_001", "B07013_004" # Same house 1 year ago
                ) 

# AREA
load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/bg/seattle_bg_boundaries.RData")

kc_water <- tigris::area_water("WA", "King", class = "sf") %>%
  st_transform(3689)
st_erase <- function(x, y) {
  st_difference(x, st_make_valid(st_union(st_combine(y))))
}
seattle_bg_nowater <- seattle_bg_boundaries %>% 
  st_erase(kc_water)
seattle_tract_nowater <- seattle_tract_boundaries %>% 
  st_erase(kc_water)

# TRACT
acs5_tract_2019 <- get_acs("tract", variables = var_vector, year = 2019, state = "WA",
        county = "King", output = "wide", geometry = FALSE) %>%
  rename(tract         = GEOID) %>%
  filter(tract %in% seattle_tract_nowater$tract) %>%
  select(-ends_with("M")) %>%
  rename_with(~ str_remove(., "E$"), .cols = c(-tract, -NAME)) %>%
  transmute(tract,
            pop           = B01003_001,
            median_inc    = B19126_001,
            pr_poverty    = (C17002_002 + C17002_003) / C17002_001,
            pr_pub_assist = B19057_002 / B19057_001,
            pr_fhh        = B11001_006 / B11001_001,
            pr_under_18   = (B01001_003 + B01001_004 + B01001_005 + B01001_006 + B01001_027 + B01001_028 + B01001_029 + B01001_030) / B01001_001,
            pr_white      = B02001_002 / B02001_001,
            pr_black      = B02001_003 / B02001_001,
            pr_asian      = B02001_005 / B02001_001,
            pr_nhopi      = B02001_006 / B02001_001,
            pr_aian       = B02001_004 / B02001_001,
            pr_foreign    = B05002_013 / B05002_001,
            pr_hisp       = B03001_003 / B03001_001,
            pr_ownhome    = B25003_002 / B25003_001,
            pr_children   = B23010_002 / B23010_001,
            pr_unemp      = B23025_005 / B23025_001,
            pr_same_house = B07013_004 / B07013_001
            ) %>% 
  left_join(seattle_tract_nowater, by = "tract") %>%
  st_as_sf() %>%
  mutate(area_sqkm = as.numeric(units::set_units(st_area(.), "km^2"))) %>% 
  mutate(pop_sqkm = as.numeric(pop / area_sqkm)) %>% 
  st_drop_geometry() %>%
  mutate(.,
         stability    = psych::principal(select(., pr_ownhome, pr_same_house))$scores[,1],
         disadvantage = psych::principal(select(., pr_pub_assist, pr_poverty, pr_unemp, pr_fhh, pr_under_18))$scores[,1]
  )

save(acs5_tract_2019, file = "./data/derived/tract/acs5_tract_2019.RData")

# BG
acs5_bg_2019 <- get_acs("block group", variables = var_vector, year = 2019, state = "WA",
                          county = "King", output = "wide", geometry = FALSE) %>%
  rename(blockgroup = GEOID) %>%
  filter(blockgroup %in% seattle_bg_nowater$blockgroup) %>%
  select(-ends_with("M")) %>%
  rename_with(~ str_remove(., "E$"), .cols = c(-blockgroup, -NAME)) %>%
  transmute(blockgroup,
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
            pr_unemp      = B23025_005 / B23025_001,
            pr_ownhome = B25003_002 / B25003_001,
            #pr_children = B23010_002/B23010_001,
            # No foreign born, hispanic, median income, or children at BG level
           ) %>% 
  left_join(seattle_bg_nowater, by = "blockgroup") %>%
  st_as_sf() %>%
  mutate(area_sqkm = as.numeric(units::set_units(st_area(.), "km^2"))) %>% 
  mutate(pop_sqkm = as.numeric(pop / area_sqkm)) %>% 
  st_drop_geometry() %>%
  mutate(.,
         # stability    = psych::principal(select(., pr_ownhome, pr_same_house))$scores[,1],
         disadvantage = psych::principal(select(., pr_pub_assist, pr_poverty, pr_unemp, pr_fhh, pr_under_18))$scores[,1]
  )

save(acs5_bg_2019, file = "./data/derived/bg/acs5_bg_2019.RData")


