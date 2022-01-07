library(tidyverse)
library(psych)
library(sf)
source("./syntax/project_functions.R")

load("./data/derived/tract/acs5_tract_2019.RData")
load("./data/derived/bg/acs5_bg_2019.RData")

sociodemographics_tract <- acs5_tract_2019 %>%
  st_drop_geometry() %>%
  mutate(disadvantage = prcomp(~ pr_pub_assist +  pr_poverty + pr_unemp  + pr_fhh, data =. , retx =TRUE, scale = TRUE, center = TRUE, rank. = 1)$x[,"PC1"],
         instability = prcomp(~ pr_ownhome + pr_same_house, data =. , retx =TRUE, scale = TRUE, center = TRUE, rank. = 1)$x[,"PC1"],
         )

# Percent under 18 excluded, unrelated to public assistance and unemployment, negative with poverty.

save(sociodemographics_tract, file = "./data/derived/tract/sociodemographics_tract.RData")
