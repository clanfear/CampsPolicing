library(tidyverse)
library(sf)
library(purrr)

# BEATS
beats_pre_2008 <- st_read("./data/raw/spd_shapefiles/beats_pre_2008/spdbeat_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat = BEAT, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2")))

beats_2008 <- st_read("./data/raw/spd_shapefiles/beats_2008/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat = BEAT, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2")))

beats_2015_2017 <- st_read("./data/raw/spd_shapefiles/beats_2015_2017/SPD_BEATS_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2")))

beats_2018 <- st_read("./data/raw/spd_shapefiles/beats_2018/SPD_Beats_WGS84.shp", stringsAsFactors=FALSE) %>%
  st_transform(4326) %>% select(beat, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry)) %>% 
  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2")))

ggplot(beats_2008) + geom_sf() +  geom_sf_label(aes(label = beat))



plot(beats_2008)
plot(beats_2015_2017)
plot(beats_2018)


# CS SURVEY

survey_1_42 <- read_csv("./data/raw/customer_satisfaction_surveys/SPDCSWv1-43.csv", guess_max = 8612)


spd_cs_survey_old <- survey_1_42 %>%
  select(id = ID,
         beat = Beat,
         call_date = CallDate,
         dispensation_code = Disp.Code,
         q3_call_satisfaction = Q3,
         q4_operator_satisfaction = Q4,
         q5_1_off_provided_info = Q5.1,
         q5_2_off_explained = Q5.2,
         q5_3_off_professional = Q5.3,
         q5_4_off_nextsteps = Q5.4,
         q5_5_off_prevention_tips = Q5.5,
         q5_6_off_area_crime_info = Q5.6,
         q5_7_off_website_ref = Q5.7,
         q5_8_off_assistance = Q5.8,
         q5_9_off_listened = Q5.9,
         q5_10_off_answered = Q5.10,
         q6_1_dept_available = Q6.1,
         q6_2_dept_provides_info = Q6.2,
         q6_3_dept_explains = Q6.3,
         q6_4_dept_professional = Q6.4,
         q6_5_dept_focuses_concerns = Q6.5,
         q6_6_dept_understands_issues = Q6.6,
         q6_7_dept_listens = Q6.7,
         q7_1_neighbpolice_overall = Q7.1,
         q7_2_neighbpolice_professional = Q7.2,
         q7_3_neighbpolice_good_resource = Q7.3,
         q7_4_neighbpolice_traffic = Q7.4,
         q7_5_neighbpolice_parks = Q7.5,
         q7_6_neighbpolice_focuses_concerns = Q7.6,
         q8_call_speed = Q8,
         q9_call_multiple_dials = Q9,
         q10_call_n_dials = Q10,
         q11_fear_overall = Q11,
         q12_fear_incident_change = Q12,
         q13_fear_polresponse_change = Q13,
         q14_dept_satisfaction_change = Q14,
         q15_fear_neighb_walk_day = Q15,
         q16_fear_neighb_walk_night = Q16,
         q18_recontact_request = Q18,
         q20_victim_nonreported = Q20,
         q21_victim_nr_when = Q21,
         q23_race = Q23,
         q24_gender = Q24,
         mir_code = MIRCode,
         mir_category = MIRCat,
         mir_category_2 = MIRCat2,
         precinct = PRECINCT,
         watch = Watch,
         race = Race,
         survey_wave = Wave,
         q11_1_17_firsthand_experience = Q11.1.17,
         q11_2_17_ties_experience = Q11.2.17,
         q11_3_17_television = Q11.3.17,
         q11_4_17_radio = Q11.4.17,
         q11_5_17_newspaper = Q11.5.17,
         q11_6_17_neighb_paper = Q11.6.17,
         q11_7_17_spd_blotter = Q11.7.17,
         q11_8_17_neighb_blogs = Q11.8.17,
         q11_9_17_internet = Q11.9.17,
         q11_10_17_email_text = Q11.10.17,
         q11_11_17_spd_socialmedia = Q11.11.17,
         q11_12_17_oth_socialmedia = Q11.12.17,
         q5_8_2015_off_contact_info = Q5.8.2015, # becomes?
         q7_4_2015_neighbpolice_traffic = Q7.4.2015, # becomes q7_4
         q6_5_13_dept_good_resource = Q6.5.13, # becomes q7_3
         q8_11_13_oth_socialmedia = Q8.11.13, # becomes q11_12_17
         q8_8_1_internet = Q8.8.1, # becomes q11_9_17
         q8_9_1_email = Q8.9.1, # this plus next equal q11_10_17,
         q8_10_1_text = Q8.10.1,
         q8_1orig_email = Q8.1orig, # this plus next becomes q11_10_17
         q8_2orig_text = Q8.2orig,
         q8_3orig_spd_website = Q8.3orig, # probably becomes q11_7_17?
         q8_4orig_mail = Q8.4orig, # dropped?
         q8_5orig_spd_neighb_meetings = Q8.5orig, # dropped?
         q8_6orig_block_watch = Q8.6orig, # dropped?
         q8_7orig_newspaper = Q8.7orig, # becomes q11_5_17
         q8_8orig_tv_radio = Q8.8orig, # split into q11_3_17 and q11_4_17
         q6_06_detective_followup = Q6.06,
         q7_06_1_detective_provides_info = Q7.06.1, # all these next ones become officer ones
         q7_06_2_detective_explained = Q7.06.2,
         q7_06_3_detective_professional = Q7.06.3,
         q7_06_4_detective_nextsteps = Q7.06.4,
         q7_06_5_detective_prevention_tips = Q7.06.5,
         q7_06_6_detective_area_crime_info = Q7.06.6,
         q8_06_n_speak_officers = Q8.06,
         beat_2 = Beat2,
         data_source = source01
         ) %>%
  mutate(
    dispensation_string = case_when(
      dispensation_code == "A" ~ "arrest",
      dispensation_code == "B" ~ "citation",
      dispensation_code == "C" ~ "no arrest",
      dispensation_code == "D" ~ "evidence submitted",
      dispensation_code == "G" ~ "cancelled",
      dispensation_code == "H" ~ "follow-up report made SPD",
      dispensation_code == "I" ~ "citation non-criminal",
      dispensation_code == "J" ~ "FIR written",
      dispensation_code == "K" ~ "other report written",
      dispensation_code == "L" ~ "service of DVPA order",
      dispensation_code == "N" ~ "non-criminal referral",
      dispensation_code == "O" ~ "oral warning given",
      dispensation_code == "P" ~ "incident located, public order restored",
      dispensation_code == "T" ~ "transportation or export provided",
      dispensation_code == "U" ~ "assistance rendered",
      dispensation_code == "W" ~ "problem solving project",
      dispensation_code == "X" ~ "extra unity",
      dispensation_code == "Y" ~ "broadcast and clear"),
    mir_cat_string = case_when(
      mir_category == 1  ~	"Animal Complaint",
      mir_category == 2  ~ "Alarms, False",
      mir_category == 3  ~ "Arson, Bombs, Explosion",
      mir_category == 4  ~ "Assaults",
      mir_category == 5  ~ "Assigned Duty",
      mir_category == 6  ~ "Assist Other Agency",
      mir_category == 7  ~ "Assist Public - Other Non Specified",
      mir_category == 8  ~ "Automobiles",
      mir_category == 9  ~ "Burglary",
      mir_category == 10	~ "Casualty",
      mir_category == 11	~ "Child",
      mir_category == 12	~ "Disturbance",
      mir_category == 13	~ "Domestic Disturbance/Violence",
      mir_category == 14	~ "Down Time",
      mir_category == 15	~ "Drive by Shooting (No Injuries)",
      mir_category == 16	~ "Fraud",
      mir_category == 17	~ "Harbor (Water)",
      mir_category == 18	~ "Hazards",
      mir_category == 19	~ "Help the Officer",
      mir_category == 20	~ "Homicide",
      mir_category == 21	~ "Intoxication and Liquor Violations",
      mir_category == 22	~ "Kidnap",
      mir_category == 23	~ "Mental Complaint",
      mir_category == 24	~ "Misc. Misdemeanors",
      mir_category == 25	~ "Mischief or Nuisance",
      mir_category == 26	~ "Narcotics",
      mir_category == 27	~ "Person",
      mir_category == 28	~ "Premise Checks",
      mir_category == 29	~ "Property Destruction (Damage)",
      mir_category == 30	~ "Property",
      mir_category == 31	~ "Prowler",
      mir_category == 32	~ "Rape",
      mir_category == 33	~ "Robbery",
      mir_category == 34	~ "Sex Offenses",
      mir_category == 35	~ "Suspicious Circumstances",
      mir_category == 36	~ "Theft",
      mir_category == 37	~ "Traffic",
      mir_category == 38	~ "Vice",
      mir_category == 39	~ "Warrant Services",
      mir_category == 40	~ "Weapon, Person with",
      mir_category == 99	~ "Unknown, Other"),
    mir_cat_2_string = case_when(
      mir_category_2 == 8	~ "Automobiles",
      mir_category_2 == 9	~ "Burglary",
      mir_category_2 == 12	~ "Disturbance",
      mir_category_2 == 35	~ "Suspicious Circumstances",
      mir_category_2 == 36	~ "Theft",
      mir_category_2 == 37	~ "Traffic",
      mir_category_2 == 99	~ "Other"),
    race_string = case_when(
      race == 1 ~	"African American or Black",
      race == 2 ~	"Alaska Native or American Indian",
      race == 3 ~	"Asian, Asian American",
      race == 4 ~	"Pacific Islander",
      race == 5 ~	"Hispanic or Latino",
      race == 6 ~	"White or Caucasian (European, Middle Eastern, or North African)",
      race == 7 ~	"Other",
      race == 8 ~	"Multi-racial",
      race == 9 ~	"Refused, No answer"),
    call_date = lubridate::mdy(call_date),
    year = lubridate::year(call_date),
    unique_id = row_number()
    )

save(spd_cs_survey_old, file = "./data/derived/spd_cs_survey_old.RData")
fear_beat_year <- spd_cs_survey %>% 
  select(beat, year, matches("fear"))  %>% 
  gather(key, value, -beat, -year) %>% 
  select(-key) %>% 
  group_by(beat, year) %>% 
  summarize(fear = mean(value, na.rm=T))


beats %>% select(geometry) %>% plot()


`%!in%` <- Negate(`%in%`)

unique(survey_1_42$Beat[survey_1_42$Beat %!in% beats$beat])
unique(beats$beat[beats$beat  %!in% survey_1_42$Beat])

unique(survey_1_42$Beat[survey_1_42$Beat %in% beats$beat])
unique(beats$beat[beats$beat  %in% survey_1_42$Beat])

spd_cs_survey %>% 
  filter(!is.na(year)) %>%
  filter(year > 2011) %>%
  select(beat, year, matches("fear"))  %>% 
  gather(key, value, -beat, -year) %>% 
  select(-key) %>% 
  filter(!is.na(value)) %>%
  group_by(beat, year) %>% 
  summarize(fear = mean(value, na.rm=T)) %>% 
  left_join(beats_2018, by=c("beat"="beat")) %>% st_as_sf() %>% 
  ggplot(aes(fill=fear)) + geom_sf(size=NA) + facet_wrap(~year)
  
spd_cs_survey %>% 
  filter(!is.na(year)) %>%
  filter(year ==2019) %>%
  select(-year) %>%
  select(beat, matches("fear"))  %>% 
  gather(key, value, -beat) %>% 
  select(-key) %>% 
  filter(!is.na(value)) %>%
  group_by(beat) %>% 
  summarize(fear = mean(value, na.rm=T)) %>% 
  left_join(beats_2018, by=c("beat"="beat")) %>% st_as_sf() %>% 
  ggplot(aes(fill=fear)) + geom_sf(size=NA)
