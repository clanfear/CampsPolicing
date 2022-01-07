library(tidyverse)
# library(plm, pos = 9999)
# library(pglm, pos = 9999)
library(tidylog, warn.conflicts = FALSE)
library(ggeffects)
library(patchwork)
load("./data/derived/beats_analysis_half.RData")

standardize <- function(x, na.rm=TRUE){
  return((x - mean(x, na.rm=na.rm) ) / sd(x, na.rm=na.rm))
}
analytical_data <- beats_analysis_half %>% filter(year >= 2016) %>%
  mutate(splag_eb_fear_mi = standardize(splag_eb_fear_mi),
         offense_property_rate = standardize(log(offense_property)),
         offense_property = standardize(offense_property),
         eb_fear_mi = standardize(eb_fear_mi),
         eb_pe_mi = standardize(eb_pe_mi))
complaints_nb_2016_int_splag <- MASS::glm.nb(n_camping_complaints ~ offense_property + splag_eb_fear_mi +
                                                eb_fear_mi +
                                                year_half * factor(beat) + offset(log(population_int)),
                                              data = analytical_data)


summary(complaints_nb_2016_int_splag)

plot(complaints_nb_2016_int_splag)

complaints_nb_2016_int_splag %>% broom::augment() %>% arrange(.resid)

#beats_analysis_half %>% filter(year >= 2016) %>% select(n_camping_complaints, offense_property_rate,splag_offense_property_rate, splag_eb_fear_mi,
 #                                                         eb_fear_mi, eb_pe_mi) %>% cor()
# Spatial autocorrelation
# library(sp)
# sp_resid <- cbind(beats_analysis_half %>% filter(year >= 2016), resid = residuals(complaints_nb_2016_int_splag)) %>%
#   left_join(beats_2015_2017) %>% st_as_sf() %>% filter(year==2016) %>% as(., "Spatial")
# 
# sp_wnb <- spdep::poly2nb(sp_resid) %>% # convert polygons to neighbor list
#   spdep::nb2listw() # weight the list by 
# 
# spdep::moran.mc(sp_resid$resid, sp_wnb, nsim = 999)

library(gt)
library(modelsummary)
library(broom)
coef_table <- complaints_nb_2016_int_splag %>% tidy() %>% 
  filter(!str_detect(term, "factor|Intercept|year")) %>%
  mutate_at(vars(-term), ~round(., 3)) %>%
  select(-statistic) %>%
  mutate(term = case_when(
    term == "offense_property" ~ "Property Crime Count",
    term == "splag_eb_fear_mi" ~ "Fear of Crime (Sp. Lag)",
    term == "eb_fear_mi" ~ "Fear of Crime"
  )) %>%
  arrange(term) %>%
  setNames(c(" ", "Est.", "SE", "p")) %>%
  gt()
save(coef_table, file= "./presentations/socsem_011720/output/coef_table.RData")


rate_plot <- function(model = complaints_nb_2016_int_splag, variable = "eb_fear_mi", varname = "Fear of Crime", df = analytical_data){
  predictions <- model %>% 
    ggpredict(terms = str_c(variable, " [-3,-2.5,-2,-1.5,-1,-.5,0,.5,1,1.5,2,2.5,3]")) %>%
    as_tibble() %>% 
    mutate_at(vars(-x, -group), ~(.*1000))
  key_values <- predictions %>% filter(x %in% c(-1, 1)) %>% arrange(x)
  first_value <- key_values$predicted[1]; second_value <- key_values$predicted[2]
  output_plot <- ( predictions %>%
                              ggplot(aes(x=x, y=predicted)) + 
                              geom_line() + 
                              geom_ribbon(aes(ymin = conf.low, ymax=conf.high), alpha = 0.2) + 
                              xlab(str_c(varname, " (SD)")) + ylab("") +
                              geom_segment(data = tibble(x = c(-1, 1, -1),
                                                         xend = c(-1, 1, 1),
                                                         y = key_values$predicted[c(1,2,1)] + c(0,0,100),
                                                         yend = key_values$predicted[c(1,2,2)] + 100), 
                                           aes(x = x, xend=xend, y = y, yend=yend), lty ="dashed") +
                              geom_label(data = tibble(x    = c(-1, 1, 0),
                                                       rate = c(round(first_value), round(second_value), str_c("+", round(100*(second_value-first_value)/first_value),"%")),
                                                       y    = 100+c(first_value, second_value, mean(c(first_value, second_value)))),
                                         aes(x=x, y =y, label=rate)) +
                              ggtitle("Predicted Complaints in 6 Months per 1000 Population", subtitle = str_c("By ", varname," (95% CI)")) +
                              theme_minimal() +
                              coord_cartesian(xlim = c(-3,3)) +
                              theme(panel.grid.major.x = element_blank(),
                                    panel.grid.minor.x = element_blank(),
                                    panel.grid.minor.y = element_blank(),
                                    plot.margin = margin(0, 0, 0, 0),
                                    axis.title.x = element_text(hjust=0.5, vjust = 10),
                                    axis.text.x = element_text(vjust=-5))) /
    (df %>% 
       ggplot(aes_string(x=variable)) + 
       geom_density(fill = "black") + 
       scale_y_reverse() + 
       geom_segment(data = tibble(x = -3:3,
                                  xend = -3:3,
                                  y = 0,
                                  yend = 0.41), 
                    aes(x = x, xend=xend, y = y, yend=yend), color="white") +
       coord_cartesian(xlim = c(-3,3)) +
       theme_void() + 
       theme(plot.margin = margin(0, 0, 0, 0))) +
    plot_layout(heights = c(9,1))
  return(output_plot)
}

fear_plot <- rate_plot(model = complaints_nb_2016_int_splag, variable = "eb_fear_mi", varname = "Fear of Crime", df = analytical_data)
property_plot <- rate_plot(model = complaints_nb_2016_int_splag, variable = "offense_property", varname = "Property Crime Counts", df = analytical_data)



ggsave("fear_plot.svg", width = 6*.9, height = 5*.9, plot=fear_plot, path = "./presentations/socsem_011720/img/")
ggsave("property_plot.svg", width = 6*.9, height = 5*.9, plot=property_plot, path = "./presentations/socsem_011720/img/")
###

fear_beat_half <- glm(eb_fear_mi ~ offense_property_rate + offense_violent_rate +
                                     
                                     year_half + factor(beat) + log(population),
                                   data = beats_analysis_half)
summary(fear_beat_half)

beats_analysis_half %>% ggplot(aes(y = n_camping_complaints, x = year_half)) + geom_smooth() + geom_line(aes(group = beat), alpha = 0.5) + scale_y_log10()

plot(complaints_nb)

complaints_nb_re_2016 <- lme4::glmer.nb(n_camping_complaints ~ offense_property_rate + offense_violent_rate +
                                     eb_fear_mi  + year_half + (year_half|beat) + offset(log(population)),
                                   data = beats_analysis_half %>%
                                     filter(year >= 2016) %>%
                                     mutate_at(vars(year_half, offense_property_rate, offense_violent_rate), ~ scale(.)),
                                   control = glmerControl(optimizer = "Nelder_Mead"))
summary(complaints_nb_re_2016)

mean(residuals(complaints_nb)^2) / mean(residuals(complaints_nb_re)^2)

cbind(beats_analysis_half %>% select(beat, year_half), resid = residuals(complaints_nb)) %>%
  mutate(lag_resid = dplyr::lag(resid), lag2_resid = dplyr::lag(resid, 2)) %>%
  select(resid, lag_resid, lag2_resid) %>% lm(resid ~ lag_resid + lag2_resid, data=.) %>% summary() # uncorrelated residuals!

# coef(complaints_nb) %>% as.data.frame(.)%>% rownames_to_column("term") %>% setNames(., c("term", "est")) %>% filter(str_detect(term, "year_half")) %>%
#   mutate(year_half = str_extract(term, "[0-9].*")) %>% separate(year_half, c("year", "half"), convert=TRUE) %>% mutate(year_half = year + (half-1)*.5) %>%
#   ggplot(aes(x=year_half, y = est)) + geom_line()


tibble(x = 1:100, log_x = log(x), sqrt_x = sqrt(x)) %>% pivot_longer(names_to = "name", values_to = "value")

library(pglm)

beats_analysis_half %>%
  mutate(year_half = year + (half-1)*.5) %>%
  (function(x) ggplot(data=x,aes(y = n_camping_complaints, x = year_half, color = beat)) + geom_line() + scale_y_log10() + geom_smooth(data=x, aes(y = n_camping_complaints, x = year_half), inherit.aes = FALSE))

complaints_lm <- lm(camping_complaints_rate ~ splag_camping_complaints_rate +
                      eb_fear_mi + eb_pe_mi + 
                      offense_property_rate + offense_violent_rate +
                      year + factor(beat),
                    data = beats_analysis_half)
summary(complaints_lm)
lmtest::bgtest(complaints_lm)

# Temporal autocorrelation
data.frame(beat=beats_analysis_half$beat, year=beats_analysis_half$year, fitted = fitted(complaints_nb), actual = beats_analysis_half$n_camping_complaints) %>%
  mutate(residual = scale(actual-fitted)) %>%
  arrange(beat, year) %>% group_by(beat) %>% mutate(lag_residual = lag(residual)) %>%
  ungroup() %>%
  select(residual, lag_residual) %>%
  na.omit() %>%
  lmtest::dwtest(residual ~ lag_residual, data=.)

plot(complaints_nb_gam)
data.frame(beat=beats_analysis_half$beat, year=beats_analysis_half$year, fitted = fitted(complaints_nb), actual = beats_analysis_half$n_camping_complaints) %>%
  mutate(residual = scale(actual-fitted)) %>%   
  left_join(beats_2015_2017, by = c("beat"="beat")) %>% st_as_sf() %>% ggplot(aes(fill = residual)) + geom_sf() + facet_wrap(~year)

beats_analysis_half %>% ggplot(aes(y=n_camping_complaints, x= year, color = beat)) + geom_line() + theme(legend.position = "none")

library(ggeffects)
complaints_nb %>% ggpredict(terms = c("eb_fear_mi"), condition = c(year = 2018)) %>% plot() 

beats_analysis_half %>% ggplot(aes(x=n_camping_complaints)) + geom_density() + facet_wrap(~year)

summary(lm(camping_complaints_rate ~ 
             eb_fear + eb_pe + 
             offense_property_rate + offense_violent_rate +
             year + factor(beat),
           data = beats_analysis_half))

summary(MASS::glm.nb(offense_public_order ~ splag_offense_public_order +
                       eb_fear + 
                       offense_property_rate + offense_violent_rate +
                       year + factor(beat) +
                       offset(log(population)),
                     data = beats_analysis_half ))

library(rsample)
library(broom)
fit_glmnb <- function(split){
  MASS::glm.nb(n_camping_complaints ~ 
                 eb_fear_mi  + 
                 year_half * factor(beat) + offset(log(population)),
               data = analysis(split))
}
boots <- bootstraps(beats_analysis_half, times = 100, strata = "beat", breaks =2 )
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

# pred_complaints_fear <- ( predictions %>%
#                            ggplot(aes(x=x, y=predicted)) + 
#                            geom_line() + 
#                            geom_ribbon(aes(ymin = conf.low, ymax=conf.high), alpha = 0.2) + 
#     xlab("") + ylab("") +
#     geom_segment(data = tibble(x = c(-2, 2, -2),
#                                xend = c(-2, 2, 2),
#                                y = c(0.196, 0.258, 0.296),
#                                yend = c(0.296, 0.358, 0.358)) %>%
#                                  mutate_at(vars(y, yend), ~(.*1000)), 
#                  aes(x = x, xend=xend, y = y, yend=yend), lty ="dashed") +
#     geom_label(data = tibble(x = c(-2, 2, 0),
#                             rate = c(0.196*1000, 0.296*1000, '+30.6%'),
#                             y = c(0.296, 0.358, 0.327))%>%
#                               mutate_at(vars(y), ~(.*1000)),
#               aes(x=x, y =y, label=rate)) +
#     ggtitle("Predicted Complaints per 1000 Population", subtitle = "By fear of crime (95% CI)") +
#     theme_minimal() +
#     theme(panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           panel.grid.minor.y = element_blank(),
#           plot.margin = margin(0, 0, 0, 0))) /
# (complaints_nb_2016 %>% 
#    ggplot(aes(x=eb_fear_mi)) + 
#    geom_density(fill = "black") + 
#    scale_y_reverse() + 
#    theme_void() + 
#    theme(plot.margin = margin(0, 0, 0, 0)) + 
#   geom_segment(data = tibble(x = -3:3,
#                              xend = -3:3,
#                              y = 0,
#                              yend = 0.41), 
#                aes(x = x, xend=xend, y = y, yend=yend), color="white") +
#    xlim(-3,4)) +
#   plot_layout(heights = c(9,1))