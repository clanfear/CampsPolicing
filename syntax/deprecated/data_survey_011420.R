library(tidyverse)
library(sf)
library(purrr)

# BEATS
# beats_pre_2008 <- st_read("./data/raw/spd_shapefiles/beats_pre_2008/spdbeat_WGS84.shp", stringsAsFactors=FALSE) %>%
#  st_transform(4326) %>% select(beat = BEAT, geometry) %>% group_by(beat) %>% summarize(geometry = st_union(geometry))%>%
#  mutate(beat_area = as.numeric(units::set_units(st_area(.), "km^2")))


    # ggplot(beats_2018) + geom_sf() +  geom_sf_label(aes(label = beat))


# CS SURVEY

survey_1_43 <- read_csv("./data/raw/customer_satisfaction_surveys/SPDCSWv1-43.csv", guess_max = 8612)
survey_1_45 <- read_csv("./data/raw/customer_satisfaction_surveys/SPDCSWv1-45.csv", guess_max = 9012)

# survey_1_43 %>% tail(10) %>% select(Q10, Q11, Q12, Q13, Q14)
# survey_1_45 %>% tail(10)%>% select(Q10, Q11, Q12, Q13, Q14)
# survey_1_45 %>% tail() %>% select(Q10, Q11, Q12, Q13, Q14,Q21,  Q22, Q24, Q26)

# names(survey_1_43)
 names(survey_1_45)

spd_cs_survey <- survey_1_45 %>%
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
         q6_1_dept_available = Q6.1, # Police efficacy 6A (1)
         q6_2_dept_provides_info = Q6.2,
         q6_3_dept_explains = Q6.3,
         q6_4_dept_professional = Q6.4,
         q6_5_dept_focuses_concerns = Q6.5, # Police efficacy 6E (2)
         q6_6_dept_understands_issues = Q6.6, # Police efficacy 6F (maybe)
         q6_7_dept_listens = Q6.7,
         q7_1_neighbpolice_overall = Q7.1, # Police efficacy 7A (3)
         q7_2_neighbpolice_professional = Q7.2,
         q7_3_neighbpolice_good_resource = Q7.3, # Police efficacy 7C (4)
         q7_4_neighbpolice_traffic = Q7.4,
         q7_5_neighbpolice_parks = Q7.5, # Police efficacy E (5)
         q7_6_neighbpolice_focuses_concerns = Q7.6, # Police efficacy F (6)
         q8_call_speed = Q8,
         q9_call_multiple_dials = Q9,
         Q10_problem_operator  = Q10, # Change starts here
         q11_call_n_dials             = Q11,
         q12_fear_overall             = Q12, # Fear of crime 11 (1)
         q13_fear_incident_change     = Q13,
         q14_fear_polresponse_change  = Q14,
         q15_dept_satisfaction_change = Q15,
         q16_fear_neighb_walk_day     = Q16, # Fear of crime 15 (2)
         q17_fear_neighb_walk_night   = Q17, # Fear of crime 16 (3)
         q19_recontact_request        = Q19,
         q21_victim_nonreported       = Q21,
         q22_victim_nr_when           = Q22,
         q24_race                     = Q24,
         q26_gender                   = Q26,
         mir_code = MIRCode,
         mir_category = MIRCat,
         mir_category_2 = MIRCat2,
         precinct = PRECINCT,
         watch = Watch,
         race = Race,
         survey_wave = Wave,
         # q11_1_17_firsthand_experience = Q11.1.17,
         # q11_2_17_ties_experience = Q11.2.17,
         # q11_3_17_television = Q11.3.17,
         # q11_4_17_radio = Q11.4.17,
         # q11_5_17_newspaper = Q11.5.17,
         # q11_6_17_neighb_paper = Q11.6.17,
         # q11_7_17_spd_blotter = Q11.7.17,
         # q11_8_17_neighb_blogs = Q11.8.17,
         # q11_9_17_internet = Q11.9.17,
         # q11_10_17_email_text = Q11.10.17,
         # q11_11_17_spd_socialmedia = Q11.11.17,
         # q11_12_17_oth_socialmedia = Q11.12.17,
         # q5_8_2015_off_contact_info = Q5.8.2015, # becomes?
         # q7_4_2015_neighbpolice_traffic = Q7.4.2015, # becomes q7_4
         # q6_5_13_dept_good_resource = Q6.5.13,
         # q8_11_13_oth_socialmedia = Q8.11.13, # becomes q11_12_17
         # q8_8_1_internet = Q8.8.1, # becomes q11_9_17
         # q8_9_1_email = Q8.9.1, # this plus next equal q11_10_17,
         # q8_10_1_text = Q8.10.1,
         # q8_1orig_email = Q8.1orig, # this plus next becomes q11_10_17
         # q8_2orig_text = Q8.2orig,
         # q8_3orig_spd_website = Q8.3orig, # probably becomes q11_7_17?
         # q8_4orig_mail = Q8.4orig, # dropped?
         # q8_5orig_spd_neighb_meetings = Q8.5orig, # dropped?
         # q8_6orig_block_watch = Q8.6orig, # dropped?
         # q8_7orig_newspaper = Q8.7orig, # becomes q11_5_17
         # q8_8orig_tv_radio = Q8.8orig, # split into q11_3_17 and q11_4_17
         # q6_06_detective_followup = Q6.06,
         # q7_06_1_detective_provides_info = Q7.06.1, # all these next ones become officer ones
         # q7_06_2_detective_explained = Q7.06.2,
         # q7_06_3_detective_professional = Q7.06.3,
         # q7_06_4_detective_nextsteps = Q7.06.4,
         # q7_06_5_detective_prevention_tips = Q7.06.5,
         # q7_06_6_detective_area_crime_info = Q7.06.6,
         # q8_06_n_speak_officers = Q8.06,
         # beat_2 = Beat2,
         # data_source = source01
         ) %>%
  mutate(q24_race = str_to_lower(q24_race)) %>%
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
    race_string = case_when(
      !is.na(race_string) ~ race_string,
      is.na(q24_race) ~ "Refused, No answer",
      q24_race %in% c("refused", "no", "decline to answer", "human", "no comment", "pr refused", "ref. hung up", "refused - asian/pacific islander") ~ "Refused, No answer",
      q24_race %in% c("african american", "african-american", "black", "african", "african-american/black", "black/sudanese", "ethiopian", "hung up/sounded black") ~	"African American or Black",
      q24_race %in% c("native american", "alaska native- klinket-heida", "clinket - juno alaska native") ~	"Alaska Native or American Indian",
      q24_race %in% c("asian", "asian american", "american asian", "chinese", "east indian", "korean") ~	"Asian, Asian American",
      q24_race %in% c("polynesian", "philipino") ~	"Pacific Islander",
      q24_race %in% c("hispanic", "peruvian", "hisapanic",  "latino", "mexican") ~	"Hispanic or Latino",
      q24_race %in% c("white","cauc.", "caucasian", "caucasian/white", "dutch/white","white/scottish", "egyptian", "iranian", "italian/caucasian", "jewish", "middle eastern") ~	"White or Caucasian (European, Middle Eastern, or North African)",
      q24_race %in% c("creole") ~	"Other",
      q24_race %in% c("white/native american", "hispanic/cauc.", "white/hispanic", "white/asian", "1/2 cauc. - 1/2 asian mixed", "african-american/native american", "hispanic/native american", "japanese/port uguess", "spanish and native american", "mixed - white/latin", "native and white") ~	"Multi-racial",
      TRUE ~	"Refused, No answer"
    ),
    call_date = lubridate::mdy(call_date),
    year = lubridate::year(call_date),
    unique_id = row_number()
    )

# For questions 5, 6, and 7, the value 7 is "does not apply"
# For 11, 15, and 16, values should only be 1 to 5; 9 is don't know

standardize <- function(x, na.rm=TRUE){
  return((x - mean(x, na.rm=na.rm) ) / sd(x, na.rm=na.rm))
}
save(spd_cs_survey, file = "./data/derived/spd_cs_survey.RData")

police_efficacy_df <- spd_cs_survey %>%
  select(unique_id,
         q6_1_dept_available,
         q6_5_dept_focuses_concerns,
         q6_6_dept_understands_issues,
         q7_1_neighbpolice_overall,
         q7_3_neighbpolice_good_resource,
         q7_5_neighbpolice_parks,
         q7_6_neighbpolice_focuses_concerns
         ) %>% 
  mutate_at(vars(-unique_id), ~ ifelse(. %in% c(7,9), NA, .)) %>%
  mutate_at(vars(-unique_id), ~ standardize(.)) %>%
  pivot_longer(-unique_id, names_to = "measure", values_to = "value") %>%
  group_by(unique_id) %>%
  filter(sum(!is.na(value)) > 3) %>%
  summarize(police_efficacy = mean(value, na.rm=TRUE))
  
fear_of_crime_df <- spd_cs_survey %>%
  select(unique_id,
         q12_fear_overall,
         q16_fear_neighb_walk_day,
         q17_fear_neighb_walk_night) %>% 
  mutate_at(vars(-unique_id), ~ ifelse(. %in% c(7,9), NA, .)) %>%
  mutate_at(vars(-unique_id), ~ standardize(.)) %>%
  pivot_longer(-unique_id, names_to = "measure", values_to = "value") %>%
  group_by(unique_id) %>%
  filter(sum(!is.na(value)) > 1) %>%
  summarize(fear_of_crime = mean(-value, na.rm=TRUE)) # reverse values to get fear instead of safety scale

beat_year_pe_fear <- spd_cs_survey %>%
  select(unique_id, beat, year) %>%
  inner_join(police_efficacy_df) %>%
  inner_join(fear_of_crime_df) %>%
  group_by(beat, year) %>%
  summarize(n=n(), police_efficacy = mean(police_efficacy), fear_of_crime = mean(fear_of_crime)) %>% 
  filter(n > 2) 

beat_2018_pe_fear <- beat_year_pe_fear %>%
  filter(year == 2018) %>% select(-year, -n)

beat_2019_pe_fear <- beat_year_pe_fear %>%
  filter(year == 2019) %>% select(-year, -n)
save(beat_2019_pe_fear, file = "./data/derived/beat_2018_pe_fear.RData")
save(beat_2018_pe_fear, file = "./data/derived/beat_2018_pe_fear.RData")
save(beat_year_pe_fear, file = "./data/derived/beat_year_pe_fear.RData")
