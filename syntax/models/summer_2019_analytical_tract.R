library(tidyverse)

load("./data/derived/tract/sweeps_summer_2019_tract.RData") # sweeps
load("./data/derived/tract/unauthorized_camping_complaints_summer_2019_tract.RData") # complaints
load("./data/derived/tract/spd_rms_sep_2018_to_aug_2019_tract.RData") #crime counts
load("./data/derived/tract/tent_census_summer_2019_tract.RData") # tents
load("./data/derived/tract/sanctioned_camps_tract_2019.RData") # sanctioned camps
load("./data/derived/tract/sociodemographics_tract.RData") # sociodemographic structures

create_splags <- function(x){
  st_queen <- function(a, b = a) sf::st_relate(a, b, pattern = "F***T****")
  splags <- x %>%
    mutate(neighbors = apply(as.matrix(st_queen(.)), 1, which)) %>% 
    sf::st_drop_geometry() %>%
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

summer_2019_analytical_tract <- seattle_tract_boundaries %>%
  right_join(sweeps_summer_2019_tract) %>%
  inner_join(unauthorized_camping_complaints_summer_2019_tract) %>%
  inner_join(spd_rms_sep_2018_to_aug_2019_tract) %>%
  inner_join(tent_census_summer_2019_tract) %>%
  inner_join(sanctioned_camps_tract_2019 %>% select(GEOID, type, any_village)) %>%
  inner_join(sociodemographics_tract) %>%
  create_splags() %>%
  st_drop_geometry()


MASS::glm.nb(complaints ~ property + splag_property + violent + public_order + n_dwellings + splag_n_dwellings + any_village + splag_villages + disadvantage + instability, data = summer_2019_analytical_tract, control = glm.control(maxit=10000)) |> summary()

MASS::glm.nb(complaints ~ property + violent + public_order + n_dwellings + any_village + pop_sqkm + pr_poverty + pr_ownhome + median_inc + pr_children, data = summer_2019_analytical_tract %>%
               mutate(across(c(property, violent, public_order, n_dwellings, pr_poverty, pr_ownhome, pop_sqkm, median_inc, pr_children), ~standardize(.))), control = glm.control(maxit=10000)) |> summary()

# Maybe do something like preceding 1 year property crime, original tent census, complaints overlapping tent census and sanctioned camps, summer sweeps.


summary(MASS::glm.nb(complaints ~ sqrt(property) + sqrt(splag_property) + violent + splag_violent + public_order + 
                       n_dwellings + any_village + 
                       pop_sqkm + pr_poverty + pr_ownhome + median_inc + pr_children, 
                     data = tract_analytical_sum19, 
                     control = glm.control(maxit=10000)))