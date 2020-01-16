library(tidyverse)
library(sf)
library(plm, pos = 9999)
library(pglm, pos = 9999)
library(tidylog, warn.conflicts = FALSE)
library(lme4, pos=9999)

# REDO FOR QUARTERS
load("./data/derived/camping_beats_quarter_2012_2019.RData")
load("./data/derived/beats_all_quarter_eb_aw.RData")
load("./data/derived/spd_rms_rates_beat_quarter.RData")

beats_quarter_analysis <- beats_all_quarter_eb_aw %>%
  inner_join(camping_beats_quarter_2012_2019, by = c("beat", "year", "call_quarter")) %>%
  inner_join(spd_rms_rates_beat_quarter, by = c("beat", "year", "call_quarter")) %>%
  mutate(n_camping_complaints = round(n_camping_complaints),
         n_camping_complaints_lead = lead(n_camping_complaints),
         n_camping_complaints_fd = n_camping_complaints-lag(n_camping_complaints),
         n_offense_public_order = round(offense_public_order),
         camping_complaints_rate = n_camping_complaints/population) %>%
  mutate(year_quarter = str_c(year, call_quarter, sep = "-"))


# SPatial Lags
beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(3690) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

 st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

beats_analysis_quarter <- 
  lapply(unique(beats_quarter_analysis$year_quarter), function(x){
  beats_year_quarter <- beats_quarter_analysis %>% 
    filter(year_quarter == x) %>%
    select(beat, starts_with("police"), starts_with("fear"), starts_with("eb"), n_camping_complaints, camping_complaints_rate, starts_with("offense"))
  out <- beats_2015_2017 %>%
    right_join(beats_year_quarter, by = "beat") %>%
    mutate(neighbors = st_queen(.)) %>% 
    st_drop_geometry() %>%
    select(beat, neighbors) %>%
    tidyr::unnest(neighbors) %>%
    mutate(neighbor_beat = beats_year_quarter$beat[neighbors]) %>%
    left_join(beats_year_quarter, by = c("neighbor_beat"="beat")) %>%
    select(-neighbors, -neighbor_beat) %>%
    group_by(beat) %>% 
    summarize_all(~mean(.)) %>% rename_at(vars(-beat), ~paste0("splag_", .)) %>%
    mutate(year_quarter = x)
  return(out)
}) %>% bind_rows() %>% 
  select(beat, year_quarter, everything()) %>%
  right_join(beats_quarter_analysis, by = c("beat", "year_quarter")) %>%
  select(beat, year, quarter=call_quarter, everything()) %>%
  arrange(beat, year, quarter) %>% 
  mutate(year_quarter = year-min(year) + (quarter-1)*.25) %>%
  mutate(year_0 = year - min(year))

save(beats_analysis_quarter, file = "./data/derived/beats_analysis_quarter.RData")

