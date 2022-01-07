library(tidyverse)
library(flextable)
load("./data/derived/output/dpm_prop_bg_out_cons.RData")
load("./data/derived/output/dpm_prop_bg_out_free.RData")

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
         Estimate = paste0(round(estimate, 3),"\n(",round(std.error,3),")")) %>%
  select(Term, Estimate, Model) %>%
  pivot_wider(names_from = Model, values_from = Estimate) 
save(dpm_prop_bg_table_data, file = "./data/derived/output/dpm_prop_bg_table_data.RData")


