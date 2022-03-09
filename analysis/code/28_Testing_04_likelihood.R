## Testing the nbinom weeks likelihood
getwd()
rm(list = ls())
## Testing the new likelihood function:
# devtools::install_github("mrc-ide/cma")
devtools::install()
devtools::load_all()
library(squire)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
# devtools::check()

## Load data
# combined_data <- readRDS(file = "~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete_weeks_only.rds")
PMP_data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_12_Post_Mortem_Complete_Date.rds")

PMP_weeks <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_12_Post_Mortem_Complete_weeks.rds")
Mort_weeks <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_07_Mortuary_data_weeks.rds")
Comb_weeks <- merge(Mort_weeks,PMP_weeks)
Comb_weeks[Comb_weeks$week==4, "deaths"] <- round(mean(Comb_weeks[Comb_weeks$week %in% c(3,5), "deaths"]))

Comb_data<- readRDS("analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")

# Missing_rows <- which(table(Combined_Weeks[,c("Week_gr","Age_group")]) ==0, arr.ind = T)
data <- PMP_data %>% select(date, PosTestNum) %>% dplyr::rename(deaths = PosTestNum)

population <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
Weighted_Durs_Hosp <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
# Lancet_Data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_10_Lancet_Data.rds")
# deaths_age <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_values_Brazeau.rds")
prob_non_severe_death_treatment <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Death_Matrix.rds")
baseline_contact_matrix <- as.matrix(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_11_Nyanga_Mixing_Matrix.rds"))


dur_get_ox_survive <- Weighted_Durs_Hosp$Surv_Dur_Weighted
dur_get_ox_die <- Weighted_Durs_Hosp$Death_Dur_Weighted

# fit_spline_rt(
## So I need to give it the combined data:
# Sys.setenv(SQUIRE_PARALLEL_DEBUG = "TRUE")
Test <- cma::fit_spline_rt(prob_non_severe_death_treatment = prob_non_severe_death_treatment$X41,
                           data = data,
                           population = population,
                           baseline_contact_matrix = baseline_contact_matrix,
                           dur_get_ox_survive = dur_get_ox_survive,
                           dur_get_ox_die = dur_get_ox_die,
                           country = "Zambia",
                           reporting_fraction = 1,
                           n_mcmc = 30000,
                           replicates = 100,
                           rw_duration = 14,
                           hosp_beds = 1e10,
                           icu_beds = 1e10,
                           prob_severe = rep(0,17),
                           dur_R = Inf,
                           log_likelihood = calc_loglikelihood_nbinom_weeks,
                           lld = "ll_nb",
                           # Combined PMP and Mort data
                           combined_data = Comb_data,
                           # Combined PMP and Mort data per week
                           combined_data_week = Comb_weeks,
                           # Combined prevalence data
                           comb_df_end = as.Date("2020-07-19"), comb_df_pos = as.integer(0.091*332), comb_df_samples = 332, comb_df_start = as.Date("2020-07-03"),
                           k_death = 50
                           )

IFRvals_fil <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds"),]

Diagnostic_Plot(1, list(Test, NULL), IFRvals_fil, ll_type = "ll_pois")

Test$pmcmc_results$inputs$pars_obs$k_death

p1 <- plot(Test, particle_fit = T) + theme(legend.position = "none")

sero_pcr <- seroprev_df(Test)
ser_pcr_2 <- Summ_sero_pcr_data(sero_pcr)

p2 <- ggplot(ser_pcr_2, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=7.6)) +
  geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0)) +
  ylab(paste0("PCR %")) + xlab("Date")

p3 <- ggplot(ser_pcr_2, aes(x = date, y = mean_combined)) + geom_line(aes(x=date, y=mean_combined),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_combined, ymax=max_combined), alpha=0.3)+
  # geom_point(aes(x= as.Date("2020-07-15"),y=2.1)) +
  geom_point(aes(x= as.Date("2020-07-15"),y=9.1)) +
  geom_errorbar(aes(ymin=2.6,ymax=15.7,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=9.1, height=0)) +

  # geom_point(aes(x= as.Date("2020-07-29"),y=10.6), color="darkblue") +
  # geom_errorbar(aes(ymin=7.3,ymax=13.9,x=as.Date("2020-07-29"), width=10), color="darkblue") +
  # geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=10.6, height=0), color="darkblue") +
  ylab(paste0("Comb+ve %"))  + xlab("Date")

cowplot::plot_grid(p1,p3,p2,nrow = 1)


index <- index <- squire:::odin_index(Test$model)
AgeDeaths <- rowMeans(Test$output[nrow(Test$output), index$D,])
AgeDeaths*0.8

Comb_data %>% group_by(Age_group) %>%
  summarise(total_deaths = sum(total_deaths),
            Samples = sum(Samples),
            PosTestNum = sum(PosTestNum)) %>%
  mutate(total_deaths * PosTestNum/Samples)

Comb_data %>% group_by(Age_group) %>%
  summarise(total_deaths = sum(total_deaths),
            Samples = sum(Samples),
            PosTestNum = sum(PosTestNum)) %>%
  mutate(total_deaths * PosTestNum/Samples)

AgeDeaths <- diff(rowSums(Test$output[as.Date(rownames(Test$output)) %in% unique(Comb_data$date), index$D,]))



# library(cma)
# devtools::load_all(".")
# bring my data in:
PMP_data_age_Test <- readRDS("analysis/data/Code-generated-data/00_06_Mortuary_post-mortem_age.rds") %>% select(date,Age_group,sampled_deaths,CT_45_Either,CT_40_Either)
Mort_data_age_Test <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age.rds")

Mort_data_age_Test %>% filter(date == "2020-07-06")
Combined %>% filter(date == "2020-07-06")
Combined_Weeks_Test %>% filter(date == "2020-07-06")
Combined_Weeks_Test

Combined <- merge(Mort_data_age, PMP_data_age, by.x = c("date","Age_group"), all = T) %>% replace_na(list(total_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0)) %>%
  complete(Age_group, date, fill = list(total_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0))

Combined <- Combined %>% filter(date == "2020-07-06")

Combined %>% mutate(Week_gr = cut.Date(x = date, breaks = "weeks", labels = F))

# Group by weeks
Combined_Weeks_Test <- Combined[-nrow(Combined),] %>% mutate(Week_gr = cut.Date(x = date, breaks = "weeks", labels = F)) %>%
  group_by(Week_gr,Age_group) %>% summarise(date = head(date,1),
                                            total_deaths = sum(total_deaths),
                                            sampled_deaths = sum(sampled_deaths),
                                            CT_45_Either = sum(CT_45_Either),
                                            CT_40_Either = sum(CT_40_Either)) %>%
  ungroup %>% complete(Age_group, nesting(Week_gr,date), fill = list(total_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0))

Combined_Weeks_Test[Combined_Weeks_Test$Week_gr==4, "total_deaths"] <-
  Combined_Weeks_Test[Combined_Weeks_Test$Week_gr %in% c(3,5), c("Age_group","Week_gr","total_deaths")] %>% group_by(Age_group) %>%
  summarise(total_deaths = round(mean(total_deaths))) %>% select(total_deaths)


# Missing_rows <- which(table(Combined_Weeks[,c("Week_gr","Age_group")]) ==0, arr.ind = T)
PMP_data <- readRDS("analysis/data/Code-generated-data/00_06_Mortuary_post-mortem.rds") %>% select(date,sampled_deaths,CT_45_Either,CT_40_Either)
data <- PMP_data %>% select(date, CT_45_Either) %>% rename(deaths = CT_45_Either)

Lus_Pop_Age <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
Weighted_Durs_Hosp <- readRDS("analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
Lancet_Data <- readRDS("analysis/data/Code-generated-data/00_10_Lancet_Data.rds")
deaths_age <- readRDS("analysis/data/Code-generated-data/00_03_IFR_values_Brazeau.rds")

Prob_Death_Mat <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Death_Matrix.rds")


IFR_mat <- readRDS("analysis/data/Code-generated-data/IFR_mat_Ints2.rds")

## Now using the Slopes and Intercepts, I need to calculate specific IFR by age groups
Age_groups <- seq(2.5,82.5, by = 5)
IFR_Age_var_slope_int <- t(apply(IFR_mat, 1, function(x){
  exp(Age_groups * x["Slope_abs"] + x["Int_abs"])
}))


# t(IFR_Age_var_slope_int)/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp)
# max(IFR_Age_var_slope_int[81,]/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp))
IFR_vals_1 <- apply(IFR_Age_var_slope_int, MARGIN = 1, FUN = function(x){x/(100*squire::parameters_explicit_SEEIR("Zambia")$prob_hosp)
  return(max(x/(100*squire::parameters_explicit_SEEIR("Zambia")$prob_hosp)))})>1
IFR_Age_var_slope_int_fil <- IFR_Age_var_slope_int[!IFR_vals_1,]
IFR_mat_fil <- IFR_mat[!IFR_vals_1,]

Input_list <- as.list(data.frame(apply(IFR_Age_var_slope_int_fil,1, function(x){x/(100*squire::parameters_explicit_SEEIR("Zambia")$prob_hosp)})))

Surv_Dur_Weighted <- Weighted_Durs_Hosp$Surv_Dur_Weighted
Death_Dur_Weighted <- Weighted_Durs_Hosp$Death_Dur_Weighted

Mixing_Matrix <- as.matrix(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_11_Nyanga_Mixing_Matrix.rds"))


# Sys.setenv(SQUIRE_PARALLEL_DEBUG = TRUE)
# Test_5 <- fit_spline_rt(data = data,
Test_8 <- fit_spline_rt(data = data,
                        combined_data = Combined_Weeks,
                        population = Lus_Pop_Age,
                        # baseline_contact_matrix = Mixing_Matrix,
                        baseline_contact_matrix = Mixing_Matrix,
                        n_mcmc = 20000, replicates = 100,
                        dur_get_ox_survive = Surv_Dur_Weighted,
                        dur_get_ox_die = Death_Dur_Weighted,
                        prob_non_severe_death_treatment = Prob_Death_Mat[[37]],
                        # prob_non_severe_death_treatment = Input_list[[36]],
                        prob_severe = rep(0,17),
                        dur_R = Inf,
                        country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                        reporting_fraction = 1,
                        rw_duration = 14,
                        hosp_beds = 1e10,
                        icu_beds = 1e10)
