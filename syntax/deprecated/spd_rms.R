library(tidyverse)
library(sf)
source("./syntax/project_functions.R")
load("./data/derived/tract/seattle_tract_boundaries.RData")
load("./data/derived/bg/seattle_bg_boundaries.RData")


spd_rms_raw <- read_csv("./data/raw/RMS Data 2008 to 8-12-2019 with gender-precinct-nibrs.csv", 
                    col_types = paste0(c("c", "c", "T", rep("c", 14)), collapse="")) %>%
  rename(
    event_id         = vw_report_event_infos.reporting_event_number,
    cad_id           = vw_report_event_infos.cad_ticket_id,
    start_time       = vw_report_event_infos.event_start_date,
    person_id        = `person ID`,
    person_type      = vw_report_persons.link_type,
    person_race      = vw_persons.race,
    person_age       = vw_persons.age,
    person_sex       = vw_persons.sex,
    person_ethnicity = vw_persons.ethnicity,
    offense_code     = vw_report_offenses.offense_code_name,
    nibrs_code       = vw_report_offences.nibrs_code_name,
    precinct         = precinct_code,
    sector           = sector,
    beat             = beat,
    reporting_area   = RA,
    location_x       = vw_report_offenses.offense_location_x,
    location_y       = vw_report_offenses.offense_location_y
  ) %>%
  mutate(event_id = str_replace(event_id, " ", "000")) %>% # Three or so events missing 000
  mutate_at(vars(-start_time), ~ ifelse(. == "NULL", NA_character_, .)) %>%
  mutate_at(vars(location_x, location_y), ~ as.numeric(.))

ct_property <- c(
  "Theft From Motor Vehicle",
  "Burglary/Breaking & Entering",
  "Destruction/Damage/Vandalism of Property",
  "Shoplifting",
  "All Other Larceny",
  "Motor Vehicle Theft",
  "Theft From Building",
  "Theft of Motor Vehicle Parts or Accessories",
  "Credit Card/Automated Teller Machine Fraud",
  "Stolen Property Offenses",
  "False Pretenses/Swindle/Confidence Game",
  "Impersonation",
  "Bad Checks",
  "Counterfeiting/Forgery",
  "Embezzlement",
  "Pocket-picking",
  "Wire Fraud",
  "Purse-snatching",
  "Identity Theft",
  "Theft From Coin-Operated Machine or Device",
  "Welfare Fraud",
  "Hacking/Computer Invasion",
  "Gambling Equipment Violation")
  
ct_violent <-c(
  "Simple Assault",
  "Aggravated Assault",
  "Robbery",
  "Rape",
  "Murder & Nonnegligent Manslaughter",
  "Sexual Assault With An Object")
  
ct_public_order <- c(
  "Trespass of Real",
  "Drug/Narcotic Violations",
  "Drug Equipment Violations",
  "Liquor Law Violations",
  "Prostitution",
  "Curfew/Loitering/Vagrancy Violations",
  "Disorderly Conduct",
  "Assisting or Promoting Prostitution",
  "Pornography/Obscene Material",
  "Peeping Tom",
  "Betting/Wagering",
  "Purchasing Prostitution",
  "Human Trafficking, Commercial Sex Acts",
  "Operating/Promoting/Assisting Gambling",
  "Drunkenness")


spd_rms <- spd_rms_raw |>
  filter(person_type %!in% c("OTHER NAME IN REPORT", "OTHER NAME IN OFFENSE") & !is.na(start_time) & !is.na(location_x) & !is.na(location_y)) |>
  mutate(offense_type = case_when(
    nibrs_code %in% ct_property     ~ "property",
    nibrs_code %in% ct_violent      ~ "violent",
    nibrs_code %in% ct_public_order ~ "public_order",
    TRUE ~ NA_character_
  )) |>
  group_by(event_id) |>
  arrange(start_time) |>
  summarize(offense_type_hierarchy = case_when(
    all(is.na(offense_type)) ~ NA_character_,
    any(offense_type %in% "violent") ~ "violent",
    any(offense_type %in% "property") ~ "property",
    any(offense_type %in% "public_order") ~ "public_order",
    TRUE ~ "ERROR"),
    location_x = first(location_x), 
    location_y = first(location_y),
    start_time = min(start_time)) |>
  filter(!is.na(offense_type_hierarchy)) |>
  mutate(date = as.Date(start_time)) |>
  select(-start_time) |>
  st_as_sf(coords = c("location_x", "location_y"), crs = 3689) |>
  st_join(seattle_tract_boundaries |> rename(tract = GEOID)) |> # More efficient to do BG spatial, join tract by ID but whatever
  st_join(seattle_bg_boundaries |> rename(blockgroup = GEOID)) 

save(spd_rms, file = "./data/derived/disaggregated/spd_rms.RData")

glimpse(spd_rms)

spd_rms_month_tract <- spd_rms |>
  st_drop_geometry() |>
  mutate(month = lubridate::month(date),
         year  = lubridate::year(date)) %>%
  count(year, month, tract, offense_type_hierarchy) %>%
  complete(nesting(year, month), tract, offense_type_hierarchy, fill = list(n=0)) %>%
  pivot_wider(names_from = offense_type_hierarchy, values_from = n)

save(spd_rms_month_tract, file = "./data/derived/tract/spd_rms_month_tract.RData")

spd_rms_month_bg <- spd_rms |>
  st_drop_geometry() |>
  mutate(month = lubridate::month(date),
         year  = lubridate::year(date)) %>%
  count(year, month, blockgroup, offense_type_hierarchy) %>%
  complete(nesting(year, month), blockgroup, offense_type_hierarchy, fill = list(n=0)) %>%
  pivot_wider(names_from = offense_type_hierarchy, values_from = n)

save(spd_rms_month_bg, file = "./data/derived/bg/spd_rms_month_bg.RData")

spd_rms_sep_2018_to_aug_2019_tract <- spd_rms_month_tract %>%
  mutate(date = lubridate::ym(paste0(year, "-", month))) %>%
  filter(date <= lubridate::ymd("2019-08-31") & date >= lubridate::ymd("2018-09-01")) %>%
  group_by(tract) %>%
  summarize(across(c(property, public_order, violent), ~sum(.)))

save(spd_rms_sep_2018_to_aug_2019_tract, file = "./data/derived/tract/spd_rms_sep_2018_to_aug_2019_tract.RData")

spd_rms_sep_2018_to_aug_2019_bg <- spd_rms_month_bg %>%
  mutate(date = lubridate::ym(paste0(year, "-", month))) %>%
  filter(date <= lubridate::ymd("2019-08-31") & date >= lubridate::ymd("2018-09-01")) %>%
  group_by(blockgroup) %>%
  summarize(across(c(property, public_order, violent), ~sum(.)))

save(spd_rms_sep_2018_to_aug_2019_bg, file = "./data/derived/tract/spd_rms_sep_2018_to_aug_2019_bg.RData")
