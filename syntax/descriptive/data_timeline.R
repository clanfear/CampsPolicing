# This file produces a timeline of the available datasets
library(tidyverse)
library(lubridate)

load("./data/derived/disaggregated/spd_public_geo.RData")
load("./data/derived/disaggregated/sweeps.RData")
load("./data/derived/disaggregated/unauthorized_camping_complaints.RData")
load("./data/derived/disaggregated/spd_survey.RData")

data_dates <- tribble(~source, ~group, ~start, ~end,
        "Tent Census", 1,  "2019-04-04", "2019-08-23",
        "Tent Census", 2,  "2019-10-14", "2019-12-14",
        "Tent Census", 3,  "2020-04-05", "2020-07-30")%>%
  mutate(across(c(start, end), as.Date)) %>%
  bind_rows(
    tribble(~source, ~group, ~start, ~end,
            "SPD Crime", 4, min(spd_public_geo$date), max(spd_public_geo$date),
            "SPD Survey", 5, min(spd_survey$call_date), max(spd_survey$call_date),
            "Sweeps", 6, min(sweeps$date), max(sweeps$date),
            "SPU Complaints", 7, min(as.Date(unauthorized_camping_complaints$created_date)), max(as.Date(unauthorized_camping_complaints$created_date)),
            )
  ) %>%
  mutate(start = if_else(start < as.Date("2016-01-01"), 
                         as.Date("2016-01-01"),
                        start),
         end = if_else(end > as.Date("2020-12-31"), 
                       as.Date("2020-12-31"),
                       end)) %>%
  pivot_longer(c(start, end))

save(data_dates, file = "./data/derived/other/data_dates.RData")

ggplot() + 
  geom_line(data = data_dates, aes(x = value, group = group, y = source), size = 5) + 
  geom_rect(aes(xmin=as.Date("2019-04-04"), 
                xmax=max(as.Date(unauthorized_camping_complaints$created_date)), 
                ymin=0, ymax=Inf), alpha = 0.5) +
  ylab("Data Source") + 
  xlab("Coverage") + 
  ggtitle("Data Availability") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

