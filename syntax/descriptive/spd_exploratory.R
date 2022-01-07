library(tidyverse)
load("./data/derived/disaggregated/spd_rms.RData")
load("./data/derived/tract/spd_rms_month_tract.RData")


glimpse(spd_rms)
spd_rms %>%
  sf::st_drop_geometry() %>%
  mutate(date = lubridate::ym(paste(lubridate::year(date), lubridate::month(date), setp = "-"))) %>%
  count(offense_type_hierarchy, date) %>%
  ggplot(aes( x = date, y = n, color = offense_type_hierarchy)) + geom_line()


load("./data/derived/bg/spd_public_bg_month.RData")

pop_df <- data.frame(year = 2008:2020,
           pop = predict(lm(pop ~ year, data =
                   tribble(~ year, ~ pop,
                             2010,    608660, 
                             2020,    737015)), 
                   newdata = data.frame(year = 2008:2020)))

spd_public_bg_month %>% 
  mutate(year = year(date)) %>%
  filter(year <= 2020) %>%
  group_by(year ) %>% 
  summarize(property = sum(property),
            violent = sum(violent)) %>%
  pivot_longer(c(property, violent)) %>%
  left_join(pop_df) %>%
  mutate(rate = (value / pop)*100000) %>%
  ggplot(aes(x = year, y = rate, group = name, color= name, linetype = name)) + 
  geom_line() + 
  theme_minimal() + 
  ggtitle("Crime Rates in Seattle", subtitle = "2008 to 2020") +
  scale_x_continuous(breaks = seq(2008, 2020, by = 4)) + theme(legend.title = element_blank())
