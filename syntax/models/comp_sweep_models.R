library(tidyverse)
library(DHARMa)
load("./data/derived/analysis/comp_prop_sweeps_cross_bg.RData")
load("./data/derived/analysis/comp_prop_sweeps_monthly_bg.RData")


pois_sweep_cs <- glm(sweeps ~  complaints + property + violent + n_dwellings + disadvantage + pr_ownhome + pop_sqkm, 
    data = comp_prop_sweeps_cross_bg %>% 
      mutate(across(c(complaints, property, violent, n_dwellings, disadvantage, pr_ownhome, pop_sqkm), ~standardize(.))), family = poisson())
lmtest::coeftest(pois_sweep_cs, vcov. = sandwich::vcovHC)

pois_sweep_cs_sim <- simulateResiduals(fittedModel = pois_sweep_cs, plot = F)
plot(pois_sweep_cs_sim)

pois_table <- lmtest::coeftest(pois_sweep_cs, vcov. = sandwich::vcovHC) %>%
  broom::tidy() %>%
  filter(term != "(Intercept)") %>%
  select(term, estimate, std.error) %>%
  mutate(Term = case_when(
    term == "n_dwellings" ~ "Tents",
    term == "pr_ownhome" ~ "% Own Home",
    term == "pop_sqkm" ~ "Density",
    TRUE ~ str_to_title(term)
  )) %>%
  mutate(across(where(is.numeric), ~sprintf("%.03f",round(.,3))),
         Estimate = paste0(estimate, "\n(", std.error, ")"),
         Term = fct_relevel(Term, "Complaints", "Tents", "Property",  "Violent")) %>%
  select(Term, Estimate) %>%
  arrange(Term)
save(pois_table, file = "./data/derived/output/pois_table.RData")