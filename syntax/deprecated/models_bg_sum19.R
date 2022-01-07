library(tidyverse)
library(sf)
library(mgcv)
library(piecewiseSEM)
library(ggeffects)

standardize <- function(x){
  (x - mean(x)) / sd(x)
}

load("./data/derived/bg_analytical_sum19.RData")

# BGs
glimpse(bg_analytical_sum19)

m_complaints <- MASS::glm.nb(complaints ~ property + splag_property + violent + public_order + 
                       n_dwellings + any_village + splag_villages +
                       pop_sqkm + pr_poverty + pr_ownhome, 
                     data = bg_analytical_sum19 %>% 
                       mutate(property = log(property),
                              n_dwellings = sqrt(n_dwellings)) %>%
                       mutate(across(!c(GEOID, complaints, type, any_village), ~standardize(.))), 
                     control = glm.control(maxit=100))
summary(m_complaints)

ggpredict(m_complaints, terms = c("n_dwellings")) %>% plot()
ggpredict(m_complaints, terms = c("property")) %>% plot()

plot(gam(complaints ~ s(property) + s(violent) + s(public_order) + 
                       s(n_dwellings) + type +
                       s(pop_sqkm) + s(pr_poverty) + s(pr_ownhome), 
                     data = bg_analytical_sum19, 
                     family = "nb"))

m_sweeps <- MASS::glm.nb(sweeps ~ complaints + splag_complaints + any_village + splag_villages +
                       n_dwellings + splag_n_dwellings +
                       violent + property + public_order + 
                       pop_sqkm +  pr_poverty + pr_ownhome, 
                     data = bg_analytical_sum19 %>% 
                       mutate(property = log(property),
                              n_dwellings = sqrt(n_dwellings),
                              complaints = sqrt(complaints)) %>%
                       mutate(across(!c(GEOID, type, any_village, sweeps), ~standardize(.))), 
                     control = glm.control(maxit=1000))
summary(m_sweeps)

plot(gam(sweeps ~ s(log1p(complaints)) + s(log(property)) + s(violent) + s(public_order) + 
           s(n_dwellings) +
           s(pop_sqkm) + s(pr_poverty) + s(pr_ownhome), 
         data = bg_analytical_sum19, 
         family = "nb"))

m_property <- MASS::glm.nb(property ~ n_dwellings + splag_n_dwellings  +
                       log(pop) + area_sqkm + any_village + splag_villages +
                        pr_ownhome + pr_pub_assist, data = bg_analytical_sum19 %>% 
                         mutate(n_dwellings = sqrt(n_dwellings),
                                complaints = sqrt(complaints)) %>%
                         mutate(across(!c(GEOID, type, any_village, sweeps, property), ~standardize(.))), control = glm.control(maxit=1000))
summary(m_property)

m_violent <- MASS::glm.nb(violent ~ n_dwellings + splag_n_dwellings  +
                             log(pop) + area_sqkm + any_village + splag_villages +
                             pr_ownhome + pr_pub_assist, data = bg_analytical_sum19, control = glm.control(maxit=1000))
summary(m_violent)


m_dwellings <- MASS::glm.nb(n_dwellings ~ violent  + property +
                            log(pop) + area_sqkm + any_village + splag_villages +
                          pr_ownhome + pr_pub_assist, data = bg_analytical_sum19, control = glm.control(maxit=1000))
summary(m_dwellings)
# pwSEM

psem_out <- psem(m_complaints, m_sweeps, m_property)
summary(psem_out)
