library(tidyverse)
library(dpm) # remotes::install_github("jacob-long/dpm")

standardize <- \(x, na.rm=TRUE){
  (x-mean(x, na.rm=na.rm))/sd(x, na.rm=na.rm)
}
log_na <- \(x){
  return(ifelse(is.na(x) | x <= 0, NA, log(x)))
}

load("./data/derived/analysis/wave_data_bg.RData")
load("./data/derived/analysis/wave_data_tract.RData")


dpm_prop_bg_out_free <- dpm(property ~ pre(lag(n_dwellings)), 
               y.free = TRUE,
               x.free = TRUE,
               se = 'robust',
    data = panel_data(wave_data_bg %>% 
                        filter(resampled ==1 & wave >= 1) %>%
                        mutate(across(c(property, n_dwellings), ~standardize(.))), id = blockgroup, wave = wave))
summary(dpm_prop_bg_out_free)
lav_summary(dpm_prop_bg_out_free)
save(dpm_prop_bg_out_free, file = "./data/derived/output/dpm_prop_bg_out_free.RData")

dpm_prop_bg_out_cons <- dpm(property ~ pre(lag(n_dwellings)), 
                         se = 'robust',
                         data = panel_data(wave_data_bg %>% 
                                             filter(resampled ==1 & wave >= 1) %>%
                                             mutate(across(c(property, n_dwellings), ~standardize(.))), id = blockgroup, wave = wave))
summary(dpm_prop_bg_out_cons)
lav_summary(dpm_prop_bg_out_cons)
save(dpm_prop_bg_out_cons, file = "./data/derived/output/dpm_prop_bg_out_cons.RData")

# Contemporaneous
pm_prop_bg_out_free_cont <- dpm(property ~ pre(n_dwellings), 
                           error.inv = TRUE,
                           y.free = TRUE,
                           x.free = TRUE,
                           se = 'robust',
                           data = panel_data(wave_data_bg %>% 
                                               filter(resampled ==1 & wave <= 3) %>%
                                               mutate(across(c(property, n_dwellings), ~standardize(.))), id = blockgroup, wave = wave))
summary(pm_prop_bg_out_free_cont)
lav_summary(pm_prop_bg_out_free_cont)
save(pm_prop_bg_out_free_cont, file = "./data/derived/output/pm_prop_bg_out_free_cont.RData")

dpm_prop_bg_out_cons_cont <- dpm(property ~ pre(n_dwellings), 
                            se = 'robust',
                            data = panel_data(wave_data_bg %>% 
                                                filter(resampled ==1 & wave <= 3) %>%
                                                mutate(across(c(property, n_dwellings), ~standardize(.))), id = blockgroup, wave = wave))
summary(dpm_prop_bg_out_cons)
lav_summary(dpm_prop_bg_out_cons)
save(dpm_prop_bg_out_cons, file = "./data/derived/output/dpm_prop_bg_out_cons.RData")