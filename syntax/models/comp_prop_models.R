library(plm)
library(tidyverse)
library(dpm)
library(fixest)
library(broom)
source("./syntax/project_functions.R")
load("./data/derived/analysis/comp_prop_panel_tract_quarter.RData")
load("./data/derived/analysis/comp_prop_cross.RData")


# Just do big dot plot with all params, fit stats, type of FE / trend; all show same thing


panels_tract <- comp_prop_panel_tract_month %>% 
  mutate(date = as.numeric(as.factor(date))) %>%
  group_by(tract) %>%
  filter(!all(complaints==0)) %>%
  ungroup() %>%
  {list(
     "pois_fe_lag_viol" = fepois(complaints ~ l(property, 1) + l(violent, 1) + l(complaints, 1) | tract + date, 
       panel.id = ~ tract + date, data = ., vcov = "DK"),
     "pois_fe_nolag_viol" = fepois(complaints ~ l(property) + l(violent) | tract + date, 
       panel.id = ~ tract + date, data = ., vcov = "DK"),
     "pois_trend_lag_viol" = fepois(complaints ~ l(property) + l(violent) + l(complaints) | tract[date], 
       panel.id = ~ tract + date, data = . , vcov = "DK"),
     "pois_trend_nolag_viol" =fepois(complaints ~ l(property) + l(violent) | tract[date], 
       panel.id = ~ tract + date, data = ., vcov = "DK"),
     "lm_fe_lag_viol" = feglm(complaints ~ l(property) + l(violent) + l(complaints) | tract + date, 
      panel.id = ~ tract + date, data = ., vcov = "DK"),
     "lm_fe_nolag_viol" = feglm(complaints ~ l(property) + l(violent) | tract + date, 
      panel.id = ~ tract + date, data = ., vcov = "DK"),
     "lm_trend_lag_viol" = feglm(complaints ~ l(property) + l(violent) + l(complaints) | tract[date], 
       panel.id = ~ tract + date, data = ., vcov = "DK"),
     "lm_trend_nolag_viol" = feglm(complaints ~ l(property) + l(violent) | tract[date], 
       panel.id = ~ tract + date, data = ., vcov = "DK"),
    "pois_fe_lag_noviol" = fepois(complaints ~ l(property, 1)  + l(complaints, 1) | tract + date, 
                                panel.id = ~ tract + date, data = ., vcov = "DK"),
    "pois_fe_nolag_noviol" = fepois(complaints ~ l(property)  | tract + date, 
                                  panel.id = ~ tract + date, data = ., vcov = "DK"),
    "pois_trend_lag_noviol" = fepois(complaints ~ l(property) + l(complaints) | tract[date], 
                                   panel.id = ~ tract + date, data = . , vcov = "DK"),
    "pois_trend_nolag_noviol" =fepois(complaints ~ l(property)  | tract[date], 
                                    panel.id = ~ tract + date, data = ., vcov = "DK"),
    "lm_fe_lag_noviol" = feglm(complaints ~ l(property)  + l(complaints) | tract + date, 
                             panel.id = ~ tract + date, data = ., vcov = "DK"),
    "lm_fe_nolag_noviol" = feglm(complaints ~ l(property) | tract + date, 
                               panel.id = ~ tract + date, data = ., vcov = "DK"),
    "lm_trend_lag_noviol" = feglm(complaints ~ l(property)  + l(complaints) | tract[date], 
                                panel.id = ~ tract + date, data = ., vcov = "DK"),
    "lm_trend_nolag_noviol" = feglm(complaints ~ l(property)  | tract[date], 
                                  panel.id = ~ tract + date, data = ., vcov = "DK"))}

map_df(panels_tract, ~ tidy(.x, conf.int=TRUE), .id = "model") %>%
  separate(model, into = c("link", "fe", "lag", "violence"), sep = "_") %>%
  mutate(model = paste(fe, lag, violence, sep = "_")) %>%
  filter(str_detect(term, "property")) %>%
  ggplot(aes(x = estimate, y = model)) + 
  geom_point() + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  facet_wrap(~link, scales = "free_x") + geom_vline(xintercept = 0)
map_df(panels, broom::glance, .id = "model")

fepois(complaints ~ l(property, 1:3)  + l(complaints, 1:3) | tract + date, 
       panel.id = ~ tract + date, data = comp_prop_panel_tract_month, vcov = "DK")
fepois(complaints ~ l(property, 1:3)  + l(complaints, 1:3) | blockgroup + date, 
       panel.id = ~ blockgroup + date, data = comp_prop_panel_bg_month, vcov = "DK")

plm::pgmm(complaints ~ lag(complaints, 1:2) + lag(property, 1:2) + lag(violent, 1:2) | lag(complaints, 2:99), 
          effect = "twoways", 
          data = comp_prop_panel_tract_month %>% mutate(across(c(complaints, property, violent), ~standardize(.))), 
          index = c("tract", "date")) %>% summary()
plm::pgmm(complaints ~ lag(complaints, 1:2) + lag(property, 1:2) + lag(violent,1:2) | lag(complaints, 2:99), 
          effect = "twoways", 
          data = comp_prop_panel_bg_month %>% mutate(across(c(complaints, property, violent), ~standardize(.))), 
          index = c("blockgroup", "date")) %>% summary()

# Cross-section
lm(complaints ~ property + violent + n_dwellings + disadvantage + pr_ownhome + pop_sqkm, data = comp_prop_cross) %>% summary()

# Panel

dpm_comp_prop_tract_quarter_out <- dpm(complaints ~ pre(lag(property)), 
                            data = panel_data(comp_prop_panel_tract_quarter %>%
                                                group_by(tract) %>% 
                                                filter(var(complaints) > 0 & var(property) > 0) %>%
                                                ungroup(), id = tract, wave = quarter),
                            std.ov = TRUE
                            )
summary(dpm_comp_prop_tract_quarter_out)

lav_summary(dpm_comp_prop_tract_quarter_out)
lavInspect(dpm_comp_prop_tract_quarter_out, "theta")
varTable(dpm_comp_prop_tract_quarter_out)
comp_prop_panel_tract_quarter %>% 
  group_by(tract) %>%
  summarize(corr = cov(property, complaints))
