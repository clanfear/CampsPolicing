# What do I need here?
## Beat-level models of fear of crime, police efficacy, SPU complaints, and offenses.
## Can theoretically do beat-quarter level if they're evenly spaced?

# Fear / satisfaction covers 2007 through 2018 reliably
load("./data/derived/beats_all_pe_fe_aw.RData")
# Observations per beat per year
beats_all_pe_fe_aw %>% group_by(year) %>% summarize(mean_n = mean(n))
beats_all_pe_fe_aw %>% ggplot(aes(fill = police_efficacy)) + geom_sf() + facet_wrap(~year)
beats_all_pe_fe_aw %>% ggplot(aes(fill = fear_of_crime)) + geom_sf() + facet_wrap(~year)
beats_all_pe_fe_aw %>% ggplot(aes(fill = n)) + geom_sf() + facet_wrap(~year)


