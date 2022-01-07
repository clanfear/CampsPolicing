library(tidyverse)
library(sf)
library(plm, pos = 9999)
library(pglm, pos = 9999)
library(tidylog, warn.conflicts = FALSE)
standardize <- function(x, na.rm=TRUE){
  return((x - mean(x, na.rm=na.rm) ) / sd(x, na.rm=na.rm))
}
load("./data/derived/camping_beats_half_2012_2019.RData")
load("./data/derived/beats_all_half_eb_aw.RData")
load("./data/derived/spd_rms_rates_beat_half.RData")

beats_half_analysis <- beats_all_half_eb_aw %>%
  inner_join(camping_beats_half_2012_2019, by = c("beat", "year", "call_half")) %>%
  inner_join(spd_rms_rates_beat_half, by = c("beat", "year", "call_half")) %>%
  mutate(n_camping_complaints = round(n_camping_complaints),
         n_camping_complaints_lead = lead(n_camping_complaints),
         n_camping_complaints_fd = n_camping_complaints-lag(n_camping_complaints),
         n_offense_public_order = round(offense_public_order),
         camping_complaints_rate = n_camping_complaints/population) %>%
  mutate(year_half = str_c(year, call_half, sep = "-"))


# SPatial Lags
beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(3690) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

 
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
beats_analysis_half <- 
  lapply(unique(beats_half_analysis$year_half), function(x){
  beats_year_half <- beats_half_analysis %>% 
    filter(year_half == x) %>%
    select(beat, starts_with("police"), starts_with("fear"), starts_with("eb"), n_camping_complaints, camping_complaints_rate, starts_with("offense"))
  out <- beats_2015_2017 %>%
    right_join(beats_year_half, by = "beat") %>%
    mutate(neighbors = st_queen(.)) %>% 
    st_drop_geometry() %>%
    select(beat, neighbors) %>%
    tidyr::unnest(neighbors) %>%
    mutate(neighbor_beat = beats_year_half$beat[neighbors]) %>%
    left_join(beats_year_half, by = c("neighbor_beat"="beat")) %>%
    select(-neighbors, -neighbor_beat) %>%
    group_by(beat) %>% 
    summarize_all(~mean(.)) %>% rename_at(vars(-beat), ~paste0("splag_", .)) %>%
    mutate(year_half = x)
  return(out)
}) %>% bind_rows() %>% 
  select(beat, year_half, everything()) %>%
  right_join(beats_half_analysis, by = c("beat", "year_half")) %>%
  select(beat, year, half=call_half, everything()) %>%
  arrange(beat, year, half) %>% 
  mutate(year_half = year + (half-1)*.5) %>% 
  mutate_at(vars(eb_fear_mi, eb_pe_mi), ~standardize(.))

save(beats_analysis_half, file = "./data/derived/beats_analysis_half.RData")



