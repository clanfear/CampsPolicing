library(tidyverse)
library(sf)
library(tidylog, warn.conflicts = FALSE)

load("./data/derived/camping_beats_2012_2019.RData")
load("./data/derived/beats_all_pe_fe_aw.RData")
load("./data/derived/spd_rms_rates_beat.RData")

beats_analysis <- beats_all_pe_fe_aw %>%
  select(-beat_area) %>%
  inner_join(camping_beats_2012_2019, by = c("beat", "year")) %>%
  inner_join(spd_rms_rates_beat, by = c("beat", "year")) %>%
  mutate(n_camping_complaints = round(n_camping_complaints),
         n_camping_complaints_lead = lead(n_camping_complaints),
         n_camping_complaints_fd = n_camping_complaints-lag(n_camping_complaints),
         n_offense_public_order = round(offense_public_order),
         camping_complaints_rate = n_camping_complaints/population)


# SPatial Lags
beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(3690) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

 
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
beats_analysis_spatial_lags <- 
  lapply(2008:2019, function(x){
  beats_year <- beats_analysis %>% 
    filter(year == x) %>%
    select(beat, starts_with("police"), starts_with("fear"), starts_with("eb"), n_camping_complaints, camping_complaints_rate, starts_with("offense"))
  out <- beats_2015_2017 %>%
    right_join(beats_year, by = "beat") %>%
    mutate(neighbors = st_queen(.)) %>% 
    st_drop_geometry() %>%
    select(beat, neighbors) %>%
    tidyr::unnest(neighbors) %>%
    mutate(neighbor_beat = beats_year$beat[neighbors]) %>%
    left_join(beats_year, by = c("neighbor_beat"="beat")) %>%
    select(-neighbors, -neighbor_beat) %>%
    group_by(beat) %>% 
    summarize_all(~mean(.)) %>% rename_at(vars(-beat), ~paste0("splag_", .)) %>%
    mutate(year = x)
  return(out)
}) %>% bind_rows() %>% 
  select(beat, year, everything()) %>%
  right_join(beats_analysis, by = c("beat", "year"))

###
# summary(MASS::glm.nb(n_camping_complaints ~ 
#                        eb_fear_mi + eb_pe_mi + 
#                        offense_property_rate + offense_violent_rate +
#                        year +
#                        offset(log(population)),
#                      data = beats_analysis))
# 
# summary(MASS::glm.nb(n_camping_complaints_lead ~ 
#                        eb_fear_mi + eb_pe_mi + 
#                        offense_property_rate + offense_violent_rate +
#                        year +
#                        offset(log(population)),
#                      data = beats_analysis))


# Wow, with beat AND year fixed effects, still recover positive fear of crime -> complaints and property crime
# Note very few complaints in early years so maybe artifact?
# summary(MASS::glm.nb(n_camping_complaints ~ 
#                        eb_fear + eb_pe + 
#                        offense_property_rate + offense_violent_rate +
#                        year + factor(beat) +
#                        offset(log(population)),
#                      data = beats_analysis))
# 
# summary(MASS::glm.nb(n_camping_complaints_lead ~ 
#                        eb_fear_mi + eb_pe_mi + 
#                        offense_property_rate + offense_violent_rate +
#                        year + factor(beat) +
#                        offset(log(population)),
#                      data = beats_analysis))
# 
# summary(lm(n_camping_complaints_fd ~ 
#                        eb_fear_mi + eb_pe_mi + 
#                        offense_property_rate + offense_violent_rate +
#                        year +
#                        population,
#                      data = beats_analysis))

## Spatial lag models
### Complaints
complaints_nb <- MASS::glm.nb(n_camping_complaints ~ 
                       eb_fear_mi + eb_pe_mi + 
                       offense_property_rate + offense_violent_rate +
                       year + factor(beat) +
                       offset(log(population)),
                     data = beats_analysis_spatial_lags )
summary(complaints_nb)

library(pglm)

complaints_pglm <- pglm(n_camping_complaints ~ 
       eb_fear_mi + eb_pe_mi + 
       offense_property_rate + offense_violent_rate +
       year ,  method = "nr", offset =log(beats_analysis_spatial_lags$population),
     model = "within", effect = "individual", index = c("beat", "year"), family = negbin,
     data = beats_analysis_spatial_lags)

complaints_lm <- lm(n_camping_complaints ~ splag_camping_complaints_rate +
                                eb_fear_mi + eb_pe_mi + 
                                offense_property_rate + offense_violent_rate +
                                year + factor(beat) +
                                offset(log(population)),
                              data = beats_analysis_spatial_lags %>% filter(year < 2019))
summary(complaints_lm)
lmtest::bgtest(complaints_lm)

# Temporal autocorrelation
data.frame(beat=beats_analysis_spatial_lags$beat, year=beats_analysis_spatial_lags$year, fitted = fitted(complaints_nb), actual = beats_analysis_spatial_lags$n_camping_complaints) %>%
  mutate(residual = scale(actual-fitted)) %>%
  arrange(beat, year) %>% group_by(beat) %>% mutate(lag_residual = lag(residual)) %>%
  ungroup() %>%
  select(residual, lag_residual) %>%
  na.omit() %>%
  lmtest::dwtest(residual ~ lag_residual, data=.)

plot(complaints_nb_gam)
data.frame(beat=beats_analysis_spatial_lags$beat, year=beats_analysis_spatial_lags$year, fitted = fitted(complaints_nb), actual = beats_analysis_spatial_lags$n_camping_complaints) %>%
  mutate(residual = scale(actual-fitted)) %>%   
  left_join(beats_2015_2017, by = c("beat"="beat")) %>% st_as_sf() %>% ggplot(aes(fill = residual)) + geom_sf() + facet_wrap(~year)

beats_analysis_spatial_lags %>% ggplot(aes(y=n_camping_complaints, x= year, color = beat)) + geom_line() + theme(legend.position = "none")

library(ggeffects)
complaints_nb %>% ggpredict(terms = c("eb_fear_mi"), condition = c(year = 2018)) %>% plot() 

beats_analysis_spatial_lags %>% ggplot(aes(x=n_camping_complaints)) + geom_density() + facet_wrap(~year)

summary(lm(camping_complaints_rate ~ 
                       eb_fear + eb_pe + 
                       offense_property_rate + offense_violent_rate +
                       year + factor(beat),
                     data = beats_analysis_spatial_lags))

summary(MASS::glm.nb(offense_public_order ~ splag_offense_public_order +
                       eb_fear + 
                       offense_property_rate + offense_violent_rate +
                       year + factor(beat) +
                       offset(log(population)),
                     data = beats_analysis_spatial_lags ))

library(rsample)
library(broom)
fit_glmnb <- function(split){
  MASS::glm.nb(n_camping_complaints ~ splag_n_camping_complaints +
                 eb_fear + 
                 offense_property_rate + offense_violent_rate +
                 year + factor(beat) +
                 offset(log(population)),
               data = analysis(split))
}
boots <- bootstraps(beats_analysis_spatial_lags, times = 500)
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
