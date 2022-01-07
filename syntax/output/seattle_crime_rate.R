library(tidyverse)

load("./data/derived/bg/spd_public_bg_month.RData")

pop_df <- data.frame(year = 2008:2020,
           pop = predict(lm(pop ~ year, data =
                   tribble(~ year, ~ pop,
                             2010,    608660, 
                             2020,    737015)), 
                   newdata = data.frame(year = 2008:2020)))

seattle_crime_rates <- spd_public_bg_month %>% 
  mutate(year = year(date)) %>%
  filter(year <= 2020) %>%
  group_by(year ) %>% 
  summarize(property = sum(property),
            violent = sum(violent)) %>%
  pivot_longer(c(property, violent)) %>%
  left_join(pop_df) %>%
  mutate(rate = (value / pop)*100000)

save(seattle_crime_rates, file = "./data/derived/output/seattle_crime_rates.RData")

  ggplot(seattle_crime_rates, aes(x = year, y = rate, group = name, color= name, linetype = name)) + 
  geom_line() + 
  xlab("") + ylab("") +
  theme_minimal() + 
  ggtitle("Crime Rates in Seattle", subtitle = "Per 100,000 people, 2008 to 2020") +
  scale_x_continuous(breaks = seq(2008, 2020, by = 4)) + theme(legend.title = element_blank())
