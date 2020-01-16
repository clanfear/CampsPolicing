library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)

tract_disadvantage <- get_acs(geography="tract", state="WA", 
                           county="King", geometry = FALSE, year = 2017,
                           variables=c("B03002_001",
                                       "B17010_001",
                                       "B23025_001",
                                       "B23025_007",
                                       "B19057_001",
                                       "B19057_002",
                                       "B19119_001")) %>%
  select(GEOID, NAME, variable, estimate) %>%
  mutate(variable = 
           case_when(
             variable == "B03002_001"  ~ "pop_total",
             variable == "B17010_001"  ~ "n_below_poverty",
             variable == "B23025_001"  ~ "n_16_plus",
             variable == "B23025_007"  ~ "n_16_plus_not_in_labor_force",
             variable == "B19057_001"  ~ "n_households_eligible_assistance",
             variable == "B19057_002"  ~ "n_households_rec_assistance",
             variable == "B19119_001"  ~ "median_family_income")
  ) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  filter(pop_total!=0 & n_households_eligible_assistance !=0 & !is.na(median_family_income)) %>%
  mutate(poverty = n_below_poverty / pop_total,
         unemployment = n_16_plus_not_in_labor_force / n_16_plus,
         assistance = n_households_rec_assistance / n_households_eligible_assistance) %>%
  mutate_at(vars(poverty, unemployment, assistance, median_family_income), ~ ( (. - mean(.))/sd(.) )) %>%
  mutate(disadvantage = (poverty + unemployment + assistance)/3) %>%
  select(GEOID, population = pop_total, poverty, unemployment, assistance, disadvantage, median_family_income)

tract_boundaries <- get_acs(geography="tract", state="WA", 
        county="King", geometry = TRUE, year = 2018,
        variables=c("B03002_001")) %>% select(GEOID, geometry)

tract_disadvantage <- tract_disadvantage %>% left_join(tract_boundaries) %>% st_as_sf()
        
save(tract_disadvantage, file = "./data/derived/tract_disadvantage.RData")

# acs_2017_vars <- load_variables(2017, "ACS5", cache=TRUE)
# 
# tract_income <- get_acs(geography="tract", state="WA", 
#         county="King", geometry = FALSE, year = 2017,
#         variables=c( "B19101_001",
# "B19119_001",
# "B19001_001")) %>%
#   select(GEOID, NAME, variable, estimate) %>%
#   mutate(variable = 
#            case_when(
#              variable == "B19101_001"  ~ "FAMILY INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)",
#              ,
#              variable == "B19001_001"  ~ "HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)")
#   ) %>%
#   pivot_wider(names_from = variable, values_from = estimate)
#   