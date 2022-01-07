library(tidyverse)
library(plm, pos = 9999)
library(pglm, pos = 9999)
library(tidylog, warn.conflicts = FALSE)
library(lme4, pos=9999)

load("./data/derived/beats_analysis_quarter.RData")

## Spatial lag models
### Complaints
complaints_nb_2016 <- MASS::glm.nb(n_camping_complaints ~ 
                                     eb_fear_mi   +
                                     year_half * factor(beat) + offset(log(population)),
                                   data = beats_analysis_quarter %>% filter(year >= 2016))
summary(complaints_nb_2016)

complaints_nb <- MASS::glm.nb(n_camping_complaints ~ 
                                eb_fear_mi   +
                                year_quarter * factor(beat) + offset(log(population)),
                              data = beats_analysis_quarter )
summary(complaints_nb)

plot(complaints_nb)

complaints_nb_re <- lme4::glmer.nb(n_camping_complaints ~ offense_property_rate + offense_violent_rate +
                                     eb_fear_mi  + year_0 + (year_0|beat) + offset(log(population)),
                                   data = beats_analysis_quarter %>% mutate_at(vars(offense_property_rate, offense_violent_rate), ~ scale(.)),
                                   control = glmerControl(optimizer = "Nelder_Mead"))
summary(complaints_nb_re)

mean(residuals(complaints_nb)^2) / mean(residuals(complaints_nb_re)^2)

cbind(beats_analysis_quarter %>% select(beat, year_half), resid = residuals(complaints_nb)) %>%
  mutate(lag_resid = dplyr::lag(resid), lag2_resid = dplyr::lag(resid, 2)) %>%
  select(resid, lag_resid, lag2_resid) %>% lm(resid ~ lag_resid + lag2_resid, data=.) %>% summary() # uncorrelated residuals!

# coef(complaints_nb) %>% as.data.frame(.)%>% rownames_to_column("term") %>% setNames(., c("term", "est")) %>% filter(str_detect(term, "year_half")) %>%
#   mutate(year_half = str_extract(term, "[0-9].*")) %>% separate(year_half, c("year", "half"), convert=TRUE) %>% mutate(year_half = year + (half-1)*.5) %>%
#   ggplot(aes(x=year_half, y = est)) + geom_line()


tibble(x = 1:100, log_x = log(x), sqrt_x = sqrt(x)) %>% pivot_longer(names_to = "name", values_to = "value")

library(pglm)

beats_analysis_spatial_lags %>%
  mutate(year_half = year + (half-1)*.5) %>%
  (function(x) ggplot(data=x,aes(y = n_camping_complaints, x = year_half, color = beat)) + geom_line() + scale_y_log10() + geom_smooth(data=x, aes(y = n_camping_complaints, x = year_half), inherit.aes = FALSE))

complaints_lm <- lm(camping_complaints_rate ~ splag_camping_complaints_rate +
                      eb_fear_mi + eb_pe_mi + 
                      offense_property_rate + offense_violent_rate +
                      year + factor(beat),
                    data = beats_analysis_quarter)
summary(complaints_lm)
lmtest::bgtest(complaints_lm)

# Temporal autocorrelation
data.frame(beat=beats_analysis_quarter$beat, year=beats_analysis_quarter$year, fitted = fitted(complaints_nb), actual = beats_analysis_spatial_lags$n_camping_complaints) %>%
  mutate(residual = scale(actual-fitted)) %>%
  arrange(beat, year) %>% group_by(beat) %>% mutate(lag_residual = lag(residual)) %>%
  ungroup() %>%
  select(residual, lag_residual) %>%
  na.omit() %>%
  lmtest::dwtest(residual ~ lag_residual, data=.)

plot(complaints_nb_gam)
data.frame(beat=beats_analysis_quarter$beat, year=beats_analysis_quarter$year, fitted = fitted(complaints_nb), actual = beats_analysis_quarter$n_camping_complaints) %>%
  mutate(residual = scale(actual-fitted)) %>%   
  left_join(beats_2015_2017, by = c("beat"="beat")) %>% st_as_sf() %>% ggplot(aes(fill = residual)) + geom_sf() + facet_wrap(~year)

beats_analysis_quarter %>% ggplot(aes(y=n_camping_complaints, x= year, color = beat)) + geom_line() + theme(legend.position = "none")

library(ggeffects)
complaints_nb %>% ggpredict(terms = c("eb_fear_mi"), condition = c(year = 2018)) %>% plot() 

beats_analysis_quarter %>% ggplot(aes(x=n_camping_complaints)) + geom_density() + facet_wrap(~year)

summary(lm(camping_complaints_rate ~ 
             eb_fear + eb_pe + 
             offense_property_rate + offense_violent_rate +
             year + factor(beat),
           data = beats_analysis_quarter))

summary(MASS::glm.nb(offense_public_order ~ splag_offense_public_order +
                       eb_fear + 
                       offense_property_rate + offense_violent_rate +
                       year + factor(beat) +
                       offset(log(population)),
                     data = beats_analysis_quarter ))

library(rsample)
library(broom)
fit_glmnb <- function(split){
  MASS::glm.nb(n_camping_complaints ~ 
                 eb_fear_mi  + 
                 year_half * factor(beat) + offset(log(population)),
               data = analysis(split))
}
boots <- bootstraps(beats_analysis_quarter, times = 100, strata = "beat", breaks =2 )
boot_models <- boots %>% 
  mutate(model = map(splits, fit_glmnb),
         coef_info = map(model, tidy))

boot_coefs <- boot_models %>% 
  unnest(coef_info)

alpha <- .05
boot_coefs %>% 
  group_by(term) %>%
  summarize(low = quantile(estimate, alpha / 2),
            high = quantile(estimate, 1 - alpha / 2),
            mean = mean(estimate)) %>%
  filter(!str_detect(term, "factor"))


