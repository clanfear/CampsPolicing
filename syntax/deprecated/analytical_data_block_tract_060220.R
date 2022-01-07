library(tidyverse)
library(sf)

# What I need:

# Exogenous spatial lags

# TRACT

load("./data/derived/complaints_tract.RData") ## Complaints

complaints_tract_sum19 <- complaints_tract %>%
  filter(year == 2019 & month >=1 & month <= 6) %>%
  select(GEOID, complaints) %>%
  group_by(GEOID) %>%
  summarize(complaints = sum(complaints))

load("./data/derived/rms_tract_month_year.RData") ## Property Crime

rms_tract_sum19 <- rms_tract_month_year %>%
  filter(year == 2019 & month >=1 & month <= 6) %>%
  select(-year, -month) %>%
  group_by(GEOID) %>%
  summarize_all(~ sum(.))

load("./data/derived/acs5_2018_tract_data.RData")
load("./data/derived/sweeps_tract.RData")
load("./data/derived/tents_tract.RData")
load("./data/derived/villages_tract.RData")

# BG

load("./data/derived/complaints_bg.RData") ## Complaints

complaints_bg_sum19 <- complaints_bg %>%
  filter(year == 2019 & month >=5 & month <= 9) %>%
  select(GEOID, complaints) %>%
  group_by(GEOID) %>%
  summarize(complaints = sum(complaints))

load("./data/derived/rms_bg_month_year.RData") ## Property Crime

rms_bg_sum19 <- rms_bg_month_year %>%
  filter(year == 2019 & month >=5 & month <= 9) %>%
  select(-year, -month) %>%
  group_by(GEOID) %>%
  summarize_all(~ sum(.))

load("./data/derived/acs5_2018_bg_data.RData")
load("./data/derived/sweeps_bg.RData")
load("./data/derived/tents_bg.RData")
load("./data/derived/villages_bg.RData")

create_splags <- function(x){
  st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
  splags <- x %>%
    mutate(neighbors = apply(as.matrix(st_queen(.)), 1, which)) %>% 
    st_drop_geometry() %>%
    select(GEOID, neighbors) %>%
    tidyr::unnest(neighbors) %>%
    mutate(neighbor_GEOID = x$GEOID[neighbors]) %>%
    left_join(x, by = c("neighbor_GEOID"="GEOID")) %>%
    select(-neighbors, -neighbor_GEOID) %>%
    group_by(GEOID) %>% 
    summarize(across(where(is.numeric), ~mean(., na.rm=TRUE)), villages = sum(type!="None")) %>% 
    rename_at(vars(-GEOID), ~paste0("splag_", .))
  x %>% left_join(splags, by = "GEOID") %>% return()
}

# BG_analytical

bg_analytical_sum19 <- acs5_2018_bg_data %>%
  left_join(sweeps_bg, by = "GEOID") %>%
  left_join(tents_bg, by = "GEOID") %>%
  left_join(villages_bg, by = "GEOID") %>%
  left_join(complaints_bg_sum19, by = "GEOID") %>%
  left_join(rms_bg_sum19, by = "GEOID") %>% 
  create_splags() %>% st_drop_geometry()

save(bg_analytical_sum19, file = "./data/derived/bg_analytical_sum19.RData")
  

# TRACT_analytical
tract_analytical_sum19 <- acs5_2018_tract_data %>%
  left_join(sweeps_tract, by = "GEOID") %>%
  left_join(tents_tract, by = "GEOID") %>%
  left_join(villages_tract, by = "GEOID") %>%
  left_join(complaints_tract_sum19, by = "GEOID") %>%
  left_join(rms_tract_sum19, by = "GEOID") %>% 
  create_splags() %>% st_drop_geometry()

save(tract_analytical_sum19, file = "./data/derived/tract_analytical_sum19.RData")



