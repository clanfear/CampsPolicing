library(tidyverse)
library(broom)
library(DHARMa)
library(ggeffects)
library(lme4)
source("./syntax/project_functions.R")
load("./data/derived/analysis/comp_prop_panel_tract_month.RData")
load("./data/derived/analysis/comp_prop_panel_bg_month.RData")
load("./data/derived/analysis/comp_prop_cross_bg.RData")
load("./data/derived/analysis/comp_prop_cross_tract.RData")

glmer_fit <- glmer(complaints ~ property + splag_property + violent +
                    n_dwellings +  disadvantage + pr_ownhome  + pop_sqkm + (1|blockgroup), 
                   control = glmerControl(optimizer = "bobyqa"),
                   data = comp_prop_cross_bg %>% 
                     mutate(across(c(where(is.numeric), -complaints), ~standardize(.))), 
                   family = poisson)
summary(glmer_fit)

comp_prop_cross_bg %>% select(property, n_dwellings) %>% summarize(across(everything(), list(~sd(.))))
# splag introduces back in a tiny bit of deviation on residuals vs. predicted
# This is due to splag having a quadratic shape
# NB and Poisson models still godawful in comparison

glmer_sim <- simulateResiduals(fittedModel = glmer_fit, plot = F)
plot(glmer_sim)

glmer_table <- broom.mixed::tidy(glmer_fit) %>%
  filter(effect == "fixed" & term != "(Intercept)") %>%
  select(term, estimate, std.error) %>%
  mutate(Term = case_when(
    term == "n_dwellings" ~ "Tents",
    term == "splag_property" ~ "Property (S. Lag)",
    term == "pr_ownhome" ~ "% Own Home",
    term == "pop_sqkm" ~ "Density",
    TRUE ~ str_to_title(term)
  )) %>%
  mutate(across(where(is.numeric), ~sprintf("%.03f",round(.,3))),
         Estimate = paste0(estimate, "\n(", std.error, ")"),
         Term = fct_relevel(Term, "Tents", "Property", "Property (S. Lag)", "Violent")) %>%
  select(Term, Estimate) %>%
  arrange(Term)
save(glmer_table, file = "./data/derived/output/glmer_table.RData")

