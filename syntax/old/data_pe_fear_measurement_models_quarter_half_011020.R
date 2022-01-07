library(tidyverse)
library(tidylog)
library(lavaan)
library(lme4)

load("./data/derived/measurement_fs.RData")


measurement_fs_2012_2018 <- measurement_fs %>%
  rename(callhalf=call_half, callquarter = call_quarter)

# HALF
measurement_fs_2012_2018 %>% count(beat, year, callhalf) %>% ggplot(aes(x=n)) + geom_bar() + geom_vline(aes(xintercept = mean(n)))
lmer_fear_mi <- lmer(fear_mi ~ dispensation + call_type + race + gender + fear_incident_change + fear_police_change + (1|beat:year:callhalf),
                     data=measurement_fs_2012_2018)

lmer_pe_mi <- lmer(pe_mi ~ dispensation + call_type + race + gender + pe_police_change + (1|beat:year:callhalf),
                   data=measurement_fs_2012_2018, control = lmerControl(optimizer = "Nelder_Mead"))

eb_pe_mi <- data.frame(eb_pe_mi = ranef(lmer_pe_mi, drop=T)$`beat:year:callhalf`) %>%
  tibble::rownames_to_column(., var="beat_year_callhalf") %>%
  separate(beat_year_callhalf, c("beat", "year", "callhalf")) %>%
  select(beat, year, call_half = callhalf,  eb_pe_mi)

eb_fear_mi <- data.frame(eb_fear_mi = ranef(lmer_fear_mi, drop=T)$`beat:year:callhalf`) %>%
  tibble::rownames_to_column(., var="beat_year_callhalf") %>%
  separate(beat_year_callhalf, c("beat", "year", "callhalf")) %>%
  select(beat, year, call_half = callhalf,  eb_fear_mi)

beat_half_eb_residuals <- eb_pe_mi %>% left_join(eb_fear_mi)
save(beat_half_eb_residuals, file = "./data/derived/beat_half_eb_residuals.RData")

# QUARTER
measurement_fs_2012_2018 %>% count(beat, year, callquarter) %>% ggplot(aes(x=n)) + geom_bar() + geom_vline(aes(xintercept = mean(n)))

lmer_fear_mi <- lmer(fear_mi ~ dispensation + call_type + race + gender + fear_incident_change + fear_police_change + (1|beat:year:callquarter),
                     data=measurement_fs_2012_2018)

lmer_pe_mi <- lmer(pe_mi ~ dispensation + call_type + race + gender + pe_police_change + (1|beat:year:callquarter),
                   data=measurement_fs_2012_2018, control = lmerControl(optimizer = "Nelder_Mead"))

eb_pe_mi <- data.frame(eb_pe_mi = ranef(lmer_pe_mi, drop=T)$`beat:year:callquarter`) %>%
  tibble::rownames_to_column(., var="beat_year_callquarter") %>%
  separate(beat_year_callquarter, c("beat", "year", "callquarter")) %>%
  select(beat, year, call_quarter = callquarter,  eb_pe_mi)

eb_fear_mi <- data.frame(eb_fear_mi = ranef(lmer_fear_mi, drop=T)$`beat:year:callquarter`) %>%
  tibble::rownames_to_column(., var="beat_year_callquarter") %>%
  separate(beat_year_callquarter, c("beat", "year", "callquarter")) %>%
  select(beat, year, call_quarter = callquarter,  eb_fear_mi)

beat_quarter_eb_residuals <- eb_pe_mi %>% left_join(eb_fear_mi)
save(beat_quarter_eb_residuals, file = "./data/derived/beat_quarter_eb_residuals.RData")

# beat_quarter_eb_residuals %>% mutate(call_half = as.character(ifelse(call_quarter <3, 1, 2))) %>% select(-call_quarter) %>% group_by(beat, year, call_half) %>% summarize_all(list(half = ~mean(.))) %>%
#  inner_join(beat_half_eb_residuals) %>% ungroup() %>% select(-beat, -year, -call_half) %>% cor()
