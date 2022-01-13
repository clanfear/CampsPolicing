library(plm)
library(tidyverse)
library(dpm)
library(fixest)
library(broom)
library(DHARMa)
source("./syntax/project_functions.R")
load("./data/derived/analysis/comp_prop_panel_tract_month.RData")
load("./data/derived/analysis/comp_prop_panel_bg_month.RData")
load("./data/derived/analysis/comp_prop_cross_bg.RData")
load("./data/derived/analysis/comp_prop_cross_tract.RData")

library(lme4)
glmer_fit <- glmer(complaints ~ property + violent + n_dwellings + disadvantage + pr_ownhome + pop_sqkm + (1|blockgroup), 
                   data = comp_prop_cross_bg %>% 
                     mutate(across(c(property, violent, n_dwellings, disadvantage, pr_ownhome, pop_sqkm), ~standardize(.))), 
                   family = poisson)

# Tract 5302 is dropped due to absence of census indicators. This is UW campus.

comp_prop_cross_bg %>% 
  mutate(across(c(property, violent, n_dwellings, disadvantage, pr_ownhome, pop_sqkm), ~standardize(.))) %>%
  filter(is.na(disadvantage))

glmer_sim <- simulateResiduals(fittedModel = glmer_fit, plot = F)
plot(glmer_sim)

glmer_table <- broom.mixed::tidy(glmer_fit) %>%
  filter(effect == "fixed" & term != "(Intercept)") %>%
  select(term, estimate, std.error) %>%
  mutate(Term = case_when(
    term == "n_dwellings" ~ "Tents",
    term == "pr_ownhome" ~ "% Own Home",
    term == "pop_sqkm" ~ "Density",
    TRUE ~ str_to_title(term)
  )) %>%
  mutate(across(where(is.numeric), ~round(.,3)),
         Estimate = paste0(estimate, "\n(", std.error, ")"),
         Term = fct_relevel(Term, "Tents", "Property",  "Violent")) %>%
  select(Term, Estimate) %>%
  arrange(Term)
save(glmer_table, file = "./data/derived/output/glmer_table.RData")
#


comp_prop_cross_bg$property %>% sd()

