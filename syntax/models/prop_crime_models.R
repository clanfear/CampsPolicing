library(tidyverse)
library(dpm) # remotes::install_github("jacob-long/dpm")
library(lme4)
library(DHARMa)
library(flextable)
source("./syntax/project_functions.R")


load("./data/derived/analysis/wave_data_bg.RData")
load("./data/derived/analysis/comp_prop_cross_bg.RData")
load("./data/derived/analysis/comp_prop_cross_tract.RData")


# Cross-sections on wave 1
glm_out_bg <- lme4::glmer(property ~ n_dwellings + splag_n_dwellings +
                          pr_ownhome + 
                          pop_sqkm  +
                          disadvantage +
                          (1|blockgroup), 
    family = "poisson",
    control = glmerControl(optimizer = "bobyqa"),
    data = comp_prop_cross_bg %>% 
    mutate(across(c(where(is.numeric), -property), ~standardize(.))))

summary(glm_out_bg)
plot(DHARMa::simulateResiduals(glm_out_bg))

glm_out_tract <- lme4::glmer(property ~ n_dwellings + splag_n_dwellings +
                               pr_ownhome + 
                               pop_sqkm  +
                               disadvantage + (1|tract), 
                       family = "poisson",
                       control = glmerControl(optimizer = "bobyqa"),
                       data = comp_prop_cross_tract %>% 
                         mutate(across(c(where(is.numeric), -property), ~standardize(.)))) 
summary(glm_out_tract)
plot(DHARMa::simulateResiduals(glm_out_tract))

# Panel data
dpm_lag_panel <- panel_data(wave_data_bg %>% 
                              filter(resampled == 1, wave >= 1) %>%
                              group_by(blockgroup) %>%
                              filter(!near(var(n_dwellings, na.rm=TRUE), 0) & 
                                       !near(var(property, na.rm=TRUE), 0)) %>%
                              ungroup(),
                            id = blockgroup, wave = wave)

dpm_blockgroups <- unique(dpm_lag_panel$blockgroup)
save(dpm_blockgroups, file = "./data/derived/other/dpm_blockgroups.RData")

# DPMs
dpm_prop_bg_out_free <- dpm(property ~ pre(lag(n_dwellings)), 
               y.free = TRUE,
               x.free = TRUE,
               std.ov = TRUE,
               estimator = "MLM",
    data = dpm_lag_panel)

summary(dpm_prop_bg_out_free)
lav_summary(dpm_prop_bg_out_free)
save(dpm_prop_bg_out_free, file = "./data/derived/output/dpm_prop_bg_out_free.RData")

dpm_prop_bg_out_cons <- dpm(property ~ pre(lag(n_dwellings)), 
                         estimator = "MLM",
                         std.ov = TRUE,
                         data = dpm_lag_panel)
summary(dpm_prop_bg_out_cons)
lav_summary(dpm_prop_bg_out_cons)
save(dpm_prop_bg_out_cons, file = "./data/derived/output/dpm_prop_bg_out_cons.RData")


dpm_prop_bg_table_data <- broom::tidy(dpm_prop_bg_out_cons) %>%
  select(-t) %>%
  mutate(wave = "t - 1",
         Model = "Constrained") %>%
  bind_rows(
    broom::tidy(dpm_prop_bg_out_free) %>%
      mutate(wave = as.character(as.numeric(t)-1)) %>%
      mutate(Model = "Free")
  ) %>%
  mutate(Term = paste0(ifelse(str_detect(term, "dwelling"), "Tents", "Property"), " (", wave,")"),
         Estimate = paste0(sprintf("%.03f", round(estimate, 3)),"\n(",sprintf("%.03f", round(std.error,3)),")")) %>%
  select(Term, Estimate, Model) %>%
  pivot_wider(names_from = Model, values_from = Estimate) 
save(dpm_prop_bg_table_data, file = "./data/derived/output/dpm_prop_bg_table_data.RData")

# Contemporaneous
# These appear to have some issues fitting. Some unlikely parameter estimates.

dpm_cont_panel <- panel_data(wave_data_bg %>% 
                               filter(resampled ==1 & wave <= 3) %>%
                              group_by(blockgroup) %>%
                              filter(!near(var(n_dwellings, na.rm=TRUE), 0) & 
                                       !near(var(property, na.rm=TRUE), 0)) %>%
                              ungroup(),
                            id = blockgroup, wave = wave)

pm_prop_bg_out_free_cont <- dpm(property ~ pre(n_dwellings), 
                           error.inv = TRUE,
                           y.free = TRUE,
                           x.free = TRUE,
                           std.ov = TRUE,
                           se = 'robust',
                           data = dpm_cont_panel)
summary(pm_prop_bg_out_free_cont)
lav_summary(pm_prop_bg_out_free_cont)
save(pm_prop_bg_out_free_cont, file = "./data/derived/output/pm_prop_bg_out_free_cont.RData")

dpm_prop_bg_out_cons_cont <- dpm(property ~ pre(n_dwellings), 
                                 error.inv = TRUE,
                                 std.ov = TRUE,
                            se = 'robust',
                            data = dpm_cont_panel)
summary(dpm_prop_bg_out_cons_cont)
lav_summary(dpm_prop_bg_out_cons_cont)
save(dpm_prop_bg_out_cons_cont, file = "./data/derived/output/dpm_prop_bg_out_cons_cont.RData")