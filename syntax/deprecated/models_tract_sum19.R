library(tidyverse)
library(sf)
library(mgcv)

load("./data/derived/tract_analytical_sum19.RData")

glimpse(tract_analytical_sum19)


summary(MASS::glm.nb(complaints ~ sqrt(property) + sqrt(splag_property) + violent + splag_violent + public_order + 
                       n_dwellings + any_village + 
                       pop_sqkm + pr_poverty + pr_ownhome + median_inc + pr_children, 
                     data = tract_analytical_sum19, 
                     control = glm.control(maxit=10000)))

plot(gam(complaints ~ s(property) + s(violent) + s(public_order) + 
           s(n_dwellings) + type +
           s(pop_sqkm) + s(pr_poverty) + s(pr_ownhome) + s(median_inc) + s(pr_children), 
         data = tract_analytical_sum19, 
         family = "nb"))

summary(MASS::glm.nb(sweeps ~ complaints +
                       n_dwellings +
                       violent + property + public_order + 
                       pop_sqkm +  pr_poverty + pr_ownhome + median_inc + pr_children, 
                     data = tract_analytical_sum19, 
                     control = glm.control(maxit=1000)))

plot(gam(sweeps ~ s(complaints) + s(property) + s(violent) + s(public_order) + 
           s(n_dwellings) +
           s(pop_sqkm) + s(pr_poverty) + s(pr_ownhome), 
         data = tract_analytical_sum19, 
         family = "nb"))

summary(MASS::glm.nb(property ~ n_dwellings + 
                       pop_sqkm + type +
                       pr_poverty + pr_ownhome + median_inc + pr_children, data = tract_analytical_sum19, control = glm.control(maxit=1000)))

plot(gam(property ~ s(n_dwellings) + 
                       pop_sqkm + type +
                       pr_poverty + s(pr_ownhome) + s(median_inc) + s(pr_children), data = tract_analytical_sum19, family = "nb"))
