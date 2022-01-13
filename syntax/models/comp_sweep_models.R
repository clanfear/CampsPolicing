

load("./data/derived/analysis/comp_prop_sweeps_cross_bg.RData")
load("./data/derived/analysis/comp_prop_sweeps_monthly_bg.RData")



MASS::glm.nb(sweeps ~ complaints + property + n_dwellings, 
             data = comp_prop_sweeps_cross_bg) %>% summary()

glmer(sweeps ~  complaints + n_dwellings + disadvantage + pr_ownhome + pop_sqkm + (1|blockgroup), 
                   data = comp_prop_sweeps_monthly_bg %>% 
                     mutate(date = factor(date), across(c(complaints, property, violent, n_dwellings, disadvantage, pr_ownhome, pop_sqkm), ~standardize(.))), 
                   family = poisson, control = glmerControl(optimizer = "bobyqa")) %>% summary()

pois_sweep_cs <- glm(sweeps ~  complaints + property + violent + n_dwellings + disadvantage + pr_ownhome + pop_sqkm, 
    data = comp_prop_sweeps_cross_bg %>% 
      mutate(across(c(complaints, property, violent, n_dwellings, disadvantage, pr_ownhome, pop_sqkm), ~standardize(.))), family = poisson())
lmtest::coeftest(pois_sweep_cs, vcov. = sandwich::vcovHC)

pois_sweep_cs_sim <- simulateResiduals(fittedModel = pois_sweep_cs, plot = F)
plot(pois_sweep_cs_sim)
