## Testing Changing reporting fraction at a specific date:
devtools::load_all("../../Squire/squire/squire.Rproj")
# library(squire)
library(tidyverse)
devtools::load_all(".")


### Official statistics from the period:
data <- readRDS(file = "analysis/data/Code-generated-data/00_01_Lusaka_Prov_Deaths.rds")
pop_st_lu <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds") # See 02_zambia_lusaka_initial
prob_death_tot_IFR_frac <- readRDS("analysis/data/Code-generated-data/00_03_Tot_Prob_Death_By_Age_Zam.rds") # See 02_zambia_lusaka_initial

Sys.setenv("SQUIRE_PARALLEL_DEBUG" = TRUE)
fit <- fit_spline_rt(data = data,
                     country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                     population = pop_st_lu,
                     reporting_fraction = 0.1,
                     reporting_fraction_2 = 0.5,
                     reporting_fraction_date_change = as.Date("2020-04-02"),
                     # reporting_fraction_bounds = c(0.25,0.1,1),
                     n_mcmc = 10000,
                     replicates = 100,
                     rw_duration = 14,
                     hosp_beds = 1e10,
                     icu_beds = 1e10,
                     prob_severe = rep(0,17),
                     prob_non_severe_death_treatment = prob_death_tot_IFR_frac,
                     dur_get_ox_survive =12,
                     dur_get_ox_die =10,
                     dur_R = Inf
)

