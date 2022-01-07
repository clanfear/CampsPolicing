library(tidyverse)
library(tidylog)
library(lavaan)
library(lme4)

standardize <- function(x, na.rm=TRUE){
  return((x - mean(x, na.rm=na.rm) ) / sd(x, na.rm=na.rm))
}

load("./data/derived/spd_cs_survey.RData")
# load("./data/derived/spd_cs_survey_old.RData")

spd_cs_survey <- spd_cs_survey %>%
  filter(beat != "H1" & beat != "99" & beat !="H3" & beat != "H2")

violent <- c("Homicide", "Drive by Shooting (No Injuries)", "Rape", "Assaults", "Robbery", "Domestic Disturbance/Violence", "Weapon, Person with", "Arson, Bombs, Explosion")
property <- c("Theft", "Burglary", "Property Destruction (Damage)", "Fraud", "Property")
public_order <- c("Disturbance", "Mental Complaint", "Mischief or Nuisance", 
                  "Narcotics", "Intoxication and Liquor Violations", "Vice")

survey_base <- spd_cs_survey %>%
  mutate(call_quarter = lubridate::quarter(call_date), 
         call_half = ifelse(call_quarter %in% c(1,2), 1, 2)) %>%
  mutate(call_type = case_when(
    mir_cat_string %in% violent ~ "Violent",
    mir_cat_string %in% property ~ "Property",
    mir_cat_string %in% public_order ~ "Public Order",
    TRUE ~ "Other"
  )) %>%
  mutate(race = case_when(
    is.na(race_string) ~ "Unknown",
    race_string == "African American or Black"                        ~ "Black",
    race_string == "Alaska Native or American Indian"                 ~ "Other",
    race_string == "Asian, Asian American"                            ~ "Asian",
    race_string == "Hispanic or Latino"                               ~ "Latinx",
    race_string == "Multi-racial"                                     ~ "Other",
    race_string == "Other"                                            ~ "Other",
    race_string == "Pacific Islander"                                 ~ "Other",
    race_string == "Refused, No answer"                               ~ "Unknown",
    race_string == "White or Caucasian (European, Middle Eastern, or North African)" ~ "White",
    TRUE ~ "ERROR"
  )) %>%
  mutate(dispensation = case_when(
    dispensation_string == "no arrest" ~ "no arrest",
    dispensation_string == "assistance rendered" ~ "assist",
    dispensation_string == "arrest" ~ "arrest",
    dispensation_string == "oral warning given" ~ "warning",
    dispensation_string == "citation non-criminal" ~ "citation",
    TRUE ~ "other"
  )) %>%
  filter(!is.na(q26_gender)) %>% # 2 NAs dropped
  mutate(gender = ifelse(q26_gender == 1, "male", "female")) %>%
  mutate(fear_incident_change = case_when(
           is.na(q13_fear_incident_change) ~ "Unknown",
           q13_fear_incident_change == 4   ~ "Unknown",
           q13_fear_incident_change == 1   ~ "Safer",
           q13_fear_incident_change == 3   ~ "Same",
           q13_fear_incident_change == 2   ~ "Less Safe"),
         fear_police_change = case_when(
           is.na(q14_fear_polresponse_change) ~ "Unknown",
           q14_fear_polresponse_change == 4   ~ "Unknown",
           q14_fear_polresponse_change == 1   ~ "Safer",
           q14_fear_polresponse_change == 3   ~ "Same",
           q14_fear_polresponse_change == 2   ~ "Less Safe" ),
         pe_police_change = case_when(
           is.na(q15_dept_satisfaction_change) ~ "Unknown" ,          
           q15_dept_satisfaction_change == 4   ~ "Unknown",
           q15_dept_satisfaction_change == 1   ~ "More favorable",
           q15_dept_satisfaction_change == 3   ~ "Same",
           q15_dept_satisfaction_change == 2   ~ "Less Favorable")) %>%
    select(unique_id, beat, year, call_quarter, call_half, dispensation, call_type, race, gender, 
           fear_incident_change, fear_police_change, pe_police_change) 

###
  
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
  mutate_at(vars(-unique_id), list("std" = ~ standardize(.))) %>%
  pivot_longer(-unique_id, names_to = "measure", values_to = "value") %>%
  mutate(measure = str_c("pe_", str_remove(measure, "^q[0-9]_[0-9]_"))) %>%
  group_by(unique_id) %>%
  filter(!all(is.na(value))) %>%
  ungroup() %>%
  pivot_wider(names_from = measure, values_from = value)

fear_of_crime_df <- spd_cs_survey %>%
  select(unique_id,
         q12_fear_overall,
         q16_fear_neighb_walk_day,
         q17_fear_neighb_walk_night) %>% 
  mutate_at(vars(-unique_id), ~ ifelse(. %in% c(7,9), NA, .)) %>%
  mutate_at(vars(-unique_id), list("std" = ~ standardize(.))) %>%
  pivot_longer(-unique_id, names_to = "measure", values_to = "value") %>%
  mutate(measure = str_remove(measure, "^q[0-9]+_")) %>%
  group_by(unique_id) %>%
  filter(!all(is.na(value))) %>%
  ungroup() %>%
  mutate(value = -value) %>%# reverse values to get fear instead of safety scale
  pivot_wider(names_from = measure, values_from = value)

measurement_data <- survey_base %>% inner_join(police_efficacy_df) %>% inner_join(fear_of_crime_df)
save(measurement_data, file = "./data/derived/measurement_data.RData")
##

##

measurement_model <- "
fear =~ fear_overall + fear_neighb_walk_day + fear_neighb_walk_night
pe =~ pe_dept_available + pe_dept_focuses_concerns + pe_dept_understands_issues + pe_neighbpolice_overall + pe_neighbpolice_good_resource + pe_neighbpolice_parks + 1*pe_neighbpolice_focuses_concerns
fear ~~ pe
"

# pe =~ pe_dept_available + pe_dept_focuses_concerns + pe_dept_understands_issues + pe_neighbpolice_overall + pe_neighbpolice_good_resource + pe_neighbpolice_parks + 1*pe_neighbpolice_focuses_concerns

measurement_output <- cfa(measurement_model, data=measurement_data, estimator = "PML",
                           missing = "pairwise",
                           ordered = c("fear_overall","fear_neighb_walk_day","fear_neighb_walk_night", "pe_dept_available", "pe_dept_focuses_concerns", "pe_dept_understands_issues", "pe_neighbpolice_overall", "pe_neighbpolice_good_resource", "pe_neighbpolice_parks", "pe_neighbpolice_focuses_concerns"))

# summary(measurement_output, fit.measures=TRUE)
# modindices(measurement_output) %>% arrange(desc(mi))
measurement_fs <- cbind(measurement_data,lavPredict(measurement_output))

measurement_model_mi <- "
fear =~ fear_overall + fear_neighb_walk_day + 1*fear_neighb_walk_night
pe =~ pe_dept_available + pe_dept_focuses_concerns + pe_dept_understands_issues + pe_neighbpolice_overall + pe_neighbpolice_good_resource + pe_neighbpolice_parks + 1*pe_neighbpolice_focuses_concerns

pe_dept_focuses_concerns ~~ pe_dept_understands_issues + pe_neighbpolice_parks
pe_neighbpolice_focuses_concerns ~~ pe_neighbpolice_parks + pe_dept_available
pe_neighbpolice_overall ~~ pe_neighbpolice_good_resource
fear_neighb_walk_day ~~ fear_neighb_walk_night

fear ~~ pe
"

# measurement_output_mi <- sem(measurement_model_mi, data=measurement_data, estimator = "ML", missing = "ML")

measurement_output_mi <- cfa(measurement_model_mi, data=measurement_data, estimator = "PML",
                                 missing = "available.cases",
                                 ordered = c("fear_overall","fear_neighb_walk_day","fear_neighb_walk_night", "pe_dept_available", "pe_dept_focuses_concerns", "pe_dept_understands_issues", "pe_neighbpolice_overall", "pe_neighbpolice_good_resource", "pe_neighbpolice_parks", "pe_neighbpolice_focuses_concerns"))

# summary(measurement_output, fit.measures=TRUE)
# modindices(measurement_output) %>% arrange(desc(mi))
fs_preds <- lavPredict(measurement_output_mi)
colnames(fs_preds) <- str_c(colnames(fs_preds), "_mi")
measurement_fs <- cbind(measurement_fs, fs_preds)


save(measurement_fs, file = "./data/derived/measurement_fs.RData")

lmer_fear <- lmer(fear ~ dispensation + call_type + race + gender + fear_incident_change + fear_police_change + (1|beat:year),
                      data=measurement_fs)

lmer_pe <- lmer(pe ~ dispensation + call_type + race + gender + pe_police_change + (1|beat:year),
                  data=measurement_fs, control = lmerControl(optimizer = "Nelder_Mead"))

lmer_fear_mi <- lmer(fear_mi ~ dispensation + call_type + race + gender + fear_incident_change + fear_police_change + (1|beat:year),
                  data=measurement_fs)

lmer_pe_mi <- lmer(pe_mi ~ dispensation + call_type + race + gender + pe_police_change + (1|beat:year),
                data=measurement_fs, control = lmerControl(optimizer = "Nelder_Mead"))

performance::icc(lmer_fear_mi)

eb_pe <-  data.frame(eb_pe = ranef(lmer_pe, drop=T)$`beat:year`) %>%
  tibble::rownames_to_column(., var="beat_year") %>%
  separate(beat_year, c("beat", "year")) %>%
  select(beat, year, eb_pe)

eb_fear <- data.frame(eb_fear = ranef(lmer_fear, drop=T)$`beat:year`) %>%
  tibble::rownames_to_column(., var="beat_year") %>%
  separate(beat_year, c("beat", "year")) %>%
  select(beat, year, eb_fear)

eb_pe_mi <- data.frame(eb_pe_mi = ranef(lmer_pe_mi, drop=T)$`beat:year`) %>%
  tibble::rownames_to_column(., var="beat_year") %>%
  separate(beat_year, c("beat", "year")) %>%
  select(beat, year, eb_pe_mi)

eb_fear_mi <- data.frame(eb_fear_mi = ranef(lmer_fear_mi, drop=T)$`beat:year`) %>%
  tibble::rownames_to_column(., var="beat_year") %>%
  separate(beat_year, c("beat", "year")) %>%
  select(beat, year, eb_fear_mi)

beat_year_eb_residuals <- eb_pe %>% left_join(eb_fear) %>% left_join(eb_pe_mi) %>% left_join(eb_fear_mi)
save(beat_year_eb_residuals, file = "./data/derived/beat_year_eb_residuals.RData")
