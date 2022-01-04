## Testing the new likelihood function:
devtools::load_all(".")
library(squire)
library(dplyr)
# bring my data in:
PMP_data <- readRDS("analysis/data/Code-generated-data/00_08_Mortuary_Post-mortem_combined.rds")
PMP_data <- PMP_data %>%
  select(date, Est_Deaths_inc) %>%
  rename(deaths = Est_Deaths_inc)

Lus_Pop_Age <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
Weighted_Durs_Hosp <- readRDS("analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
Lancet_Data <- readRDS("analysis/data/Code-generated-data/00_10_Lancet_Data.rds")
deaths_age <- readRDS("analysis/data/Code-generated-data/00_03_Tot_Prob_Death_By_Age_Zam.rds")

Sys.setenv(SQUIRE_PARALLEL_DEBUG = "TRUE")
fit_BMJ_mort <- fit_spline_rt(data = BMJ,
                              deaths_age = deaths_age,
                              country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                              population = Lus_Pop_Age,
                              reporting_fraction = 1,
                              # reporting_fraction_bounds = c(0.25,0.1,1),
                              n_mcmc = 10000,
                              replicates = 100,
                              rw_duration = 14,
                              hosp_beds = 1e10,
                              icu_beds = 1e10,
                              prob_severe = rep(0,17),
                              # prob_non_severe_death_treatment = prob_death_tot_CFR_mat[,x],
                              dur_get_ox_survive = Weighted_Durs_Hosp$Surv_Dur_Weighted,
                              dur_get_ox_die = Weighted_Durs_Hosp$Death_Dur_Weighted,
                              dur_R = Inf,
                              sero_df_start = as.Date(c("2020-07-04")),#,"2020-07-18")),
                              sero_df_end = as.Date(c("2020-07-27")),#,"2020-08-10")),
                              sero_df_pos = as.numeric(as.integer(c(Lancet_Data$Sero_prev["val"]/100*Lancet_Data$Sero_prev["n"]))),#, 0.106*1952))), # See Table 2
                              sero_df_samples = Lancet_Data$Sero_prev["n"],#,1952),
                              pcr_df_start = as.Date(c("2020-07-04")),
                              pcr_df_end = as.Date(c("2020-07-27")),
                              pcr_df_pos = as.integer(c(Lancet_Data$PCR_prev["val"]/100*Lancet_Data$PCR_prev["n"])), # See Table 2
                              pcr_df_samples = Lancet_Data$PCR_prev["n"],
                              IFR_slope_bounds = c(1,0.5,1.5),
                              IFR_multiplier_bounds = c(1,0.4,1.5)
)
