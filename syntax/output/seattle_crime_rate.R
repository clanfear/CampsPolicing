library(tidyverse)

load("./data/derived/bg/spd_public_bg_month.RData")

pop_df <- data.frame(year = 2008:2021,
           pop = predict(lm(pop ~ year, data =
                   tribble(~ year, ~ pop,
                             2010,    608660, 
                             2020,    737015)), 
                   newdata = data.frame(year = 2008:2021)))

seattle_crime_rates <- spd_public_bg_month %>% 
  mutate(year = year(date)) %>%
  filter(year <= 2021) %>%
  group_by(year ) %>% 
  summarize(property = sum(property),
            violent  = sum(violent)) %>%
  pivot_longer(c(property, violent)) %>%
  left_join(pop_df) %>%
  mutate(rate = (value / pop)*100000)

save(seattle_crime_rates, file = "./data/derived/output/seattle_crime_rates.RData")
