library(tidyverse)
library(lavaan)
library(psych)

# Nothing much working here. Most of the measures seem separable.

load("./data/derived/acs5_2018_tract_data.RData")
load("./data/derived/acs5_2018_bg_data.RData")

standardize <- function(x){
  (x - mean(x) )/ sd(x)
}

acs5_2018_bg_data %>% select(-GEOID) %>% filter(across(everything(), ~ !is.na(.))) %>%
  select(-pr_nhopi, - pr_aian) %>% factanal(factors = 3, rotation = "varimax")

acs5_2018_tract_data %>% select(-GEOID) %>% filter(across(everything(), ~ !is.na(.))) %>%
  select(-pr_nhopi, - pr_aian) %>% factanal(factors = 4, rotation = "varimax")

acs5_2018_tract_data %>% select(-GEOID) %>% filter(across(everything(), ~ !is.na(.))) %>%
  select(-pr_nhopi, - pr_aian) %>% princomp() %>% .$loadings



summary(cfa("
    family =~ under_18 + pr_black
    heterogeneity =~ pr_foreign + pr_asian 
    ", data = acs5_2018_tract_data %>% mutate(across(-GEOID, ~standardize(.) ))))


acs5_2018_tract_data %>% select(-GEOID) %>% filter(across(everything(), ~ !is.na(.))) %>%
  select(-pr_nhopi, - pr_aian) %>% lapply(., hist)
