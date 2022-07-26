## Testing the pois bin bin age weeks likelihood using full standardise
getwd()
rm(list = ls())
## Testing the new likelihood function:
# devtools::install_github("mrc-ide/cma")

# devtools::check()
devtools::install()
devtools::load_all()
library(squire)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(Brobdingnag)

## Load data
PMP_data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_12_Post_Mortem_Complete_Date.rds")
data <- PMP_data %>% select(date, PosTests_Strict) %>% dplyr::rename(deaths = PosTests_Strict)

Comb_data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_bur_regs_postmortem_data_complete.rds")
# Comb_data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")
Comb_data <- Comb_data %>% mutate(PosTests = PosTests_Strict)
# Comb_data <- merge(Comb_data, data.frame(Age_gr = 1:17, Bg_dr = as.vector(colMeans(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")))*12/52))

population <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Str_2020_imp_ests.rds")
Weighted_Durs_Hosp <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
prob_non_severe_death_treatment <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Death_List_log_sc_new_pop.rds")
baseline_contact_matrix <- as.matrix(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_11_Nyanga_Mixing_Matrix.rds"))

dur_get_ox_survive <- Weighted_Durs_Hosp$Surv_Dur_Weighted
dur_get_ox_die <- Weighted_Durs_Hosp$Death_Dur_Weighted

dfj_mcmc_data <- readRDS("analysis/data/Code-generated-data/00_16_03_drj_mcmc_data_new_pop_str.rds")

pcr_det <- readRDS(file = "analysis/data/Code-generated-data/00_15_pcr_det_hall_100.rds")
pcr_det_PM <- readRDS(file = "analysis/data/Code-generated-data/00_15_pcr_det_hall_100.rds")
pcr_df <- readRDS(file = "analysis/data/Code-generated-data/00_10_Lancet_Data.rds")$pcr_df
sero_df <- readRDS(file = "analysis/data/Code-generated-data/00_10_Lancet_Data.rds")$sero_df
# options(error=browser)
devtools::load_all()
Sys.setenv(SQUIRE_PARALLEL_DEBUG = "TRUE")
Sys.setenv(SQUIRE_PARALLEL_DEBUG = "")
# system.time(
  Test2 <- fit_spline_rt(prob_non_severe_death_treatment = prob_non_severe_death_treatment$X41,
                              data = data,
                              population = population,
                              baseline_contact_matrix = baseline_contact_matrix,
                              dur_get_ox_survive = dur_get_ox_survive,
                              dur_get_ox_die = dur_get_ox_die,
                              country = "Zambia",
                              reporting_fraction = 1,
                              n_mcmc = 20,
                              replicates = 5,
                              rw_duration = 14,
                              hosp_beds = 1e10,
                              icu_beds = 1e10,
                              prob_severe = rep(0,17),
                              dur_R = Inf,
                              log_likelihood = calc_loglikelihood_10_pois_bin_bin_ag1std_agRR,
                              # lld = "remove_weeks_4_and_5",
                         # lld = "full_data",
                         lld = "remove_weeks_4_and_5_and_age_under_5s",
                              # lld = "ll_pois_bin",
                              # Combined PMP and Mort data
                              # combined_data = list(Comb_data = Comb_data,
                                                   # dfj_mcmc_data = dfj_mcmc_data),
                         combined_data = list(Comb_data = Comb_data,
                                              dfj_mcmc_data = lapply(dfj_mcmc_data, function(x){return(x %>% dplyr::filter(
                                                Age_gr != 1,Week_gr != 4,Week_gr != 5))})),

                              # Combined prevalence data
                              # comb_df = data.frame(date_start = as.Date("2020-07-04"), date_end = as.Date("2020-07-19"),
                              #                      comb_pos = as.integer(0.091*332), samples = 332),
                         pcr_df = pcr_df,
                         sero_df = sero_df,
                         Prior_Rt_rw_unif_lim = 1,
                         pcr_det = pcr_det,
                         pcr_det_PM = pcr_det_PM,
                         frac_mort = 1

  )
# )
  # saveRDS(object = Test2, file = "../Bonus Files/2022-07-14_Test_New_Likelihood.rds")

  devtools::load_all()
  # Res_10 <- readRDS("../Bonus Files/2022-06-24_L10_Stricter_PM_Threshold.rds")
  # Test2 <- Res_10$X41
  res_Test_diag <- Diagnostic_Plot_10(1, list(Test2), IFRvals = IFR_coefficients[41,])
  # get_Prior(Test2)
  res_Test <- Get_the_replicate_likelihood_values(1, list(Test2))
  ## Do these results match up with the expected values

  # get_Likelihood(Test2)

## For example: Get the likelihood values
  ## Do they match up with the likelihood when I add the three components together?
  Test2
# )
# saveRDS(Test2, "../Bonus Files/2022-04-20_Test_for_some_reason_I_think_the_10likelihood_test.rds")
## This took 20807 seconds

IFR_coefficients <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds") %>%
  mutate(Index = 1:nrow(.))

Select_Runs <- IFR_coefficients %>%
  filter(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_death_logical_log_sc.rds"),
         Slope_x %in% c(0.8,1,1.25)) %>%
  pull(Index)


devtools::load_all()
# Res_10 <- readRDS("../Bonus Files/2022-07-20_L10_default_full_set.rds")
# Title <- "Default Model Fit"
Res_10 <- readRDS("../Bonus Files/2022-07-22_L10_Remove_wk_4_5_age_1.rds")
Title <- "Remove weeks 4-5 and age <5"
# Res_10 <- lapply(Res_10, function(x){if(class(x)[1] != "squire_simulation"){NULL}else{x}})
# lapply(Res_10, function(x){table(as.Date(x$replicate_parameters$start_date))})
# Res_10b <- readRDS("../Bonus Files/2022-07-18_L10_disaggregated_sero_and_pcr_remaining_runs.rds")
Res_10_plots <- lapply(X =1:length(Res_10), FUN = Diagnostic_Plot_10, fit_Model_list = Res_10, IFRvals = IFR_coefficients)
# saveRDS(object = Res_10_plots, "../Bonus Files/2022-07-20_L10_Plots_x81_Default.rds")
# Res_10_plots <- readRDS("../Bonus Files/2022-07-18_L10_Plots_x81_Default.rds")
# Full_Res <- Full_Res[order(as.numeric(gsub(pattern = "X","",names(Full_Res))))]
# Res_10_plots <- Res_10_plots[order(as.numeric(gsub(pattern = "X","",names(Full_Res))))]
Heatmaps1 <- Plot_Heatmaps(Mod_Res = Res_10, Res_Figs = Res_10_plots, Select_Runs = 1:81, Title = Title)
pdf("analysis/figures/39_Mod_Fit_Res_10_Default_full_set.pdf", height = 9, width = 13)
Heatmaps1
Res_10_plots
dev.off()

Data_Plots_X41 <- Diagnostic_Plot_10(fit_num = 1, fit_Model_list = list(Res_10$X41), IFRvals = IFR_coefficients[41,],Return_data_only_plots = T)
Data_Plots_with_fit_X41 <- Diagnostic_Plot_10(fit_num = 1, fit_Model_list = list(Res_10$X41), IFRvals = IFR_coefficients[41,])

Res_10_plots <- lapply(X =1:length(Res_10), FUN = Diagnostic_Plot_10, fit_Model_list = Res_10, IFRvals = IFR_coefficients)



# Full_Res <-c(Res_10b[1:27],Res_10,Res_10b[28:57])
# lapply(Full_Res, function(x){
# if(class(x) != "squire_simulation"){x<-NULL}
# return(x)
# })











devtools::load_all()
Res_pcr_det <- readRDS("../Bonus Files/2022-06-20_Vary_pcr_det.rds")
Title <- "Vary PCR det"
Res_pcr_det_plots <- lapply(X =1:length(Res_pcr_det), FUN = Diagnostic_Plot_10, fit_Model_list = Res_pcr_det, IFRvals = data.frame(IFR_x = paste0("Pop PCR: ", c(rep("Quick",3),rep("Default",3),rep("Slow",3))),
                                                                                                                                           Slope_x = rep(paste0("PM PCR: ", c("Quick","Default","Slow")),3)))

pdf("analysis/figures/39_Mod_Fit_Res_10_01_pcr_det_vary.pdf", height = 11, width = 12)
Res_pcr_det_plots
dev.off()


# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior.pdf", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior_set_to_3.pdf", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior_Remove_Weeks_4_5.pdf", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior_Remove_Youngest.pdf", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior_Remove_Weeks_4_5_and_Youngest.pdf", height = 11, width = 12)
Heatmaps1
Res_10_plots
dev.off()


# Test2 <- readRDS("../Bonus Files/2022-04-20_Test_for_some_reason_I_think_the_10likelihood_test.rds")
# test_plots <- cma::Diagnostic_Plot_09(fit_num = 1, fit_Model_list = list(Test2), IFRvals = IFR_coefficients[41,])
# test_plots$Diagnostic
# test_plots$Pois_fits
# test_plots$lls

# Res_10 <- readRDS("../Bonus Files/2022-05-20_L10_Fixed_Int.rds")
# Res_10_1 <- readRDS("../Bonus Files/2022-06-02_L10_Fixed_Int_Fixed_Prior.rds")
# Title <- "Rt_rw prior set to 3"
# Res_10_3 <- readRDS("../Bonus Files/2022-06-08_L10_Fixed_Int_Fixed_Prior_3.rds")

# Title <- "Rt_rw prior 1; Remove Weeks 4-5"
# Res_10 <- readRDS("../Bonus Files/2022-06-08_L10_Fixed_Int_Fixed_Prior_Remove_Weeks.rds")

# min(Res_10$X41$replicate_parameters[,paste0("Rt_rw_",1:10)])
# max(Res_10$X41$replicate_parameters[,paste0("Rt_rw_",1:10)])

# Res_10$X41$replicate_parameters[,paste0("Rt_rw_",1:10)] %>%
#   mutate(Row = 1:100) %>%
#   melt(value.name = "Default", id.vars = "Row", variable.name = c("parm")) %>%
#   merge(Res_10_1$X41$replicate_parameters[,paste0("Rt_rw_",1:10)] %>%
#           mutate(Row = 1:100) %>%
#           melt(value.name = "Unif.1", id.vars = "Row", variable.name = c("parm"))) %>%
#   merge(Res_10_3$X41$replicate_parameters[,paste0("Rt_rw_",1:10)] %>%
#           mutate(Row = 1:100) %>%
#           melt(value.name = "Unif.3", id.vars = "Row", variable.name = c("parm"))) %>%
#   melt(value.name = "Rt_rw", id.vars = c("Row","parm"), variable.name = c("Fit")) %>%
#   ggplot(data = ., aes(x = Rt_rw, fill = Fit)) +
#   facet_wrap(~parm) +
#   geom_histogram() +
#   theme_minimal() +




# Res_10$X41$replicate_parameters[,paste0("Rt_rw_",1:10)] %>%
#   melt() %>%
#   ggplot(data = ., aes(x = value)) +
#   facet_wrap(~variable) +
#   geom_histogram() +
#   theme_minimal() +
#   geom_vline(xintercept = 0, linetype = "dashed")
#
# Res_10_1$X41$replicate_parameters[,paste0("Rt_rw_",1:10)] %>%
#   melt() %>%
#   ggplot(data = ., aes(x = value)) +
#   facet_wrap(~variable) +
#   geom_histogram()
#
# Res_10_3$X41$replicate_parameters[,paste0("Rt_rw_",1:10)] %>%
#   melt() %>%
#   ggplot(data = ., aes(x = value)) +
#   facet_wrap(~variable) +
#   geom_histogram()

Res_10$X41$pmcmc_results$inputs$model_params$contact_matrix_set
Res_10$X41$pmcmc_results$inputs$model_params$mix_mat_set[1,,] %>%
  melt() %>%
  ggplot() + geom_contour_filled(aes(x = Var1, y = Var2, z = value)) +
  geom_vline(aes(xintercept = c(13))) +
  geom_hline(aes(yintercept = c(13))) +
  geom_vline(aes(xintercept = c(16))) +
  geom_hline(aes(yintercept = c(16))) +
  theme_minimal()


t(squire::parameters_explicit_SEEIR(country = "United Kingdom")$mix_mat_set[1,,] * squire::get_population("Zambia")$n) * squire::get_population("Zambia")$n %>%
  matrix() %>%
  ggplot() + geom_contour_filled(aes(x = Var1, y = Var2, z = value)) +
  geom_vline(aes(xintercept = c(13))) +
  geom_hline(aes(yintercept = c(13))) +
  geom_vline(aes(xintercept = c(16))) +
  geom_hline(aes(yintercept = c(16))) +
  theme_minimal()


# squire::parameters_explicit_SEEIR(contact_matrix_set = squire::get_mixing_matrix("Zambia"), population = squire::get_population("Zambia")$n)$mix_mat_set[1,,] %>%
# squire::parameters_explicit_SEEIR(contact_matrix_set = squire::get_mixing_matrix("Zambia"), population = squire::get_population("Zambia")$n)$mix_mat_set[1,,] %>%
squire::parameters_explicit_SEEIR(contact_matrix_set = squire::get_mixing_matrix("Zimbabwe"), population = squire::get_population("Zimbabwe")$n)$mix_mat_set[1,,] %>%
# squire::parameters_explicit_SEEIR(contact_matrix_set = squire::get_mixing_matrix("Zambia"), population = squire::get_population("United Kingdom")$n)$mix_mat_set[1,,] %>%
# squire::parameters_explicit_SEEIR(contact_matrix_set = squire::get_mixing_matrix("United Kingdom"), population = squire::get_population("Zambia")$n)$mix_mat_set[1,,] %>%
# squire::parameters_explicit_SEEIR(contact_matrix_set = squire::get_mixing_matrix("United Kingdom"), population = squire::get_population("United Kingdom")$n)$mix_mat_set[1,,] %>%
  melt() %>%
  ggplot() + geom_contour_filled(aes(x = Var1, y = Var2, z = value)) +
  geom_vline(aes(xintercept = c(13))) +
  geom_hline(aes(yintercept = c(13))) +
  geom_vline(aes(xintercept = c(16))) +
  geom_hline(aes(yintercept = c(16))) +
  theme_minimal()

squire::parameters_explicit_SEEIR(country = "United Kingdom")$mix_mat_set[1,,] %>%
  melt() %>%
  ggplot() + geom_contour_filled(aes(x = Var1, y = Var2, z = value)) +
  geom_vline(aes(xintercept = c(13))) +
  geom_hline(aes(yintercept = c(13))) +
  geom_vline(aes(xintercept = c(16))) +
  geom_hline(aes(yintercept = c(16))) +
  theme_minimal()

squire::parameters_explicit_SEEIR(country = "Zambia")$mix_mat_set[1,,] %>%
  melt() %>%
  ggplot() + geom_contour_filled(aes(x = Var1, y = Var2, z = value)) +
  geom_vline(aes(xintercept = c(13))) +
  geom_hline(aes(yintercept = c(13))) +
  geom_vline(aes(xintercept = c(16))) +
  geom_hline(aes(yintercept = c(16))) +
  theme_minimal()

squire::parameters_explicit_SEEIR(country = "United Kingdom")$contact_matrix_set[[1]] %>%
  unname() %>%
  melt() %>%
  ggplot() + geom_contour_filled(aes(x = Var1, y = Var2, z = value)) +
  geom_vline(aes(xintercept = c(13))) +
  geom_hline(aes(yintercept = c(13))) +
  geom_vline(aes(xintercept = c(16))) +
  geom_hline(aes(yintercept = c(16))) +
  theme_minimal()

squire::parameters_explicit_SEEIR(country = "Zambia")$contact_matrix_set[[1]] %>%
  unname() %>%
  melt() %>%
  ggplot() + geom_contour_filled(aes(x = Var1, y = Var2, z = value)) +
  geom_vline(aes(xintercept = c(13))) +
  geom_hline(aes(yintercept = c(13))) +
  geom_vline(aes(xintercept = c(16))) +
  geom_hline(aes(yintercept = c(16))) +
  theme_minimal()



squire::get_mixing_matrix("Zambia") %>%  unname() %>%
  melt() %>%
  ggplot() + geom_contour_filled(aes(x = Var1, y = Var2, z = value)) +
  geom_vline(aes(xintercept = c(13))) +
  geom_hline(aes(yintercept = c(13))) +
  geom_vline(aes(xintercept = c(16))) +
  geom_hline(aes(yintercept = c(16))) +
  theme_minimal()

Res_10$X41$pmcmc_results$inputs$model_params$contact_matrix_set[[1]] %>%
  # as.matrix() %>%
  unname() %>%
  melt() %>%
  ggplot() + geom_contour_filled(aes(x = Var1, y = Var2, z = value)) +
  geom_vline(aes(xintercept = c(13))) +
  geom_hline(aes(yintercept = c(13))) +
  geom_vline(aes(xintercept = c(16))) +
  geom_hline(aes(yintercept = c(16))) +
  theme_minimal()



devtools::load_all()
# Res_10 <- readRDS("../Bonus Files/2022-06-02_L10_Fixed_Int_Fixed_Prior.rds")
# Res_10 <- readRDS("../Bonus Files/2022-06-08_L10_Fixed_Int_Fixed_Prior_3.rds")
# Res_10 <- readRDS("../Bonus Files/2022-06-08_L10_Fixed_Int_Fixed_Prior_Remove_Weeks.rds")
# Res_10 <- readRDS("../Bonus Files/2022-06-08_L10_Fixed_Int_Fixed_Prior_Remove_Youngest.rds")
# Res_10 <- readRDS("../Bonus Files/2022-06-08_L10_Fixed_Int_Fixed_Prior_Remove_Weeks_and_Youngest.rds")
# Res_10 <- readRDS("../Bonus Files/2022-06-24_L10_Stricter_PM_Threshold.rds")
# Res_10 <- readRDS("../Bonus Files/2022-06-24_L10_Stricter_PM_Threshold_pcr_100.rds")
# Res_10 <- readRDS("../Bonus Files/2022-06-24_L10_Stricter_PM_Threshold_pcr_100_pcr_PM_90.rds")
# Title <- "Rt_rw prior set to 1"
# Title <- "Rt_rw prior set to 3"
# Title <- "Remove weeks 4-5 (Rt_rw prior unif(-1,1))"
# Title <- "Remove youngest (Rt_rw prior unif(-1,1))"
# Title <- "Remove <5 and youngest age group (Rt_rw prior unif(-1,1))"
# Title <- "Stricter Post-mortem Threshold"
# Title <- "Stricter Post_mortem Threshold, 100% PCR det"
# Title <- "Stricter Post_mortem Threshold, 100% PCR det, 90% PM PCR det"

devtools::load_all()
data_plots <- Diagnostic_Plot_10(1, fit_Model_list = list(Res_10$X28), IFRvals = IFR_coefficients[1,], Return_data_only_plots = T)
pdf("analysis/figures/39_PM_data_plot_1x1.pdf", width = 10, height = 5)
data_plots[2]
dev.off()
tiff("analysis/figures/39_PM_data_plot_1x1.tiff", width = 10, height = 5, units = "in", res = 300)
data_plots[2]
dev.off()

Res_10_plots <- lapply(X =1:length(Res_10), FUN = Diagnostic_Plot_10, fit_Model_list = Res_10, IFRvals = IFR_coefficients[Select_Runs,])
# Res_10_plots_1x1 <- lapply(X =1, FUN = Diagnostic_Plot_10, fit_Model_list = list(Res_10$X41), IFRvals = IFR_coefficients[41,], Data_only_plots = T)
# tiff("analysis/figures/39_PM_data_plot_1x1.tiff", res = 300, width = 10, height = 4.5, units = "in")
# Res_10_plots_1x1[[1]]$Prev_plot_data
# dev.off()
# tiff("analysis/figures/39_PM_data_plot_with_fit_1x1.tiff", res = 300, width = 10, height = 4.5, units = "in")
# Res_10_plots_1x1[[1]]$Prev_plot
# dev.off()

Heatmaps1 <- Plot_Heatmaps(Mod_Res = Res_10, Res_Figs = Res_10_plots, Select_Runs = Select_Runs, Title = Title)
# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior.pdf", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior_set_to_3.pdf", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior_Remove_Weeks_4_5.pdf", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior_Remove_Youngest.pdf", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_01_Fixed_Likelihood_Fixed_Prior_Remove_Weeks_4_5_and_Youngest.pdf", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_02_Strict_PM_ct_Threshold", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_02_Strict_PM_ct_Threshold_100_pcr_det", height = 11, width = 12)
# pdf("analysis/figures/39_Mod_Fit_Res_10_02_Strict_PM_ct_Threshold_100_pcr_det_90_PM_pcr_det", height = 11, width = 12)
pdf("analysis/figures/39_Mod_Fit_Res_10_03_split_pcr_sero.pdf", height = 9, width = 12)
Heatmaps1
Res_10_plots
dev.off()

# tiff(filename = "analysis/figures/39_Poisson_fit_1x1.tiff", res = 300, width = 8, height = 8, units = "in")
# Res_10_plots[[14]]$Poisson_Figure
# dev.off()
# Plot_Heatmaps(Mod_Res = Res_10, Res_Figs = Res_10_plots, Select_Runs = Select_Runs, Title = "Fixed Average")


par(mfrow = c(1,2))
hist(Res_10[[10]]$replicate_parameters$start_date, breaks = "day", format = "%d %b", main = "1x0.2: Date", xlab = "Date")
hist(Res_10[[14]]$replicate_parameters$start_date, breaks = "day", format = "%d %b", main = "1x1: Date", xlab = "Date")
mean(Res_10[[10]]$replicate_parameters$start_date)
mean(Res_10[[14]]$replicate_parameters$start_date)
plot(dunif(x = -55:-10, min = -55, max = -10, log = TRUE), type = "l")


## Prior is made of Start date
dunif(x = -55:-10, min = -55, max = -10, log = TRUE)[1] + ## Start date is uniform
## R0
dnorm(x = c(mean(Res_10[[10]]$replicate_parameters$R0),
            mean(Res_10[[14]]$replicate_parameters$R0)), mean = 3, sd = 1, log = TRUE) +
## Rt shift
dnorm(x = c(mean(Res_10[[10]]$replicate_parameters$Rt_shift),
            mean(Res_10[[14]]$replicate_parameters$Rt_shift)), mean = 0, sd = 1, log = TRUE) +
## Rt shift scale, also uniform
dunif(x = c(mean(Res_10[[10]]$replicate_parameters$Rt_shift_scale),
            mean(Res_10[[14]]$replicate_parameters$Rt_shift_scale)), min = 0.1, max = 10, log = TRUE) +
## Rt rw parameters
c(sum(dnorm(x = colMeans(Res_10[[10]]$replicate_parameters[paste0("Rt_rw_",1:10)]), mean = 0, sd = 0.2, log = TRUE)),
  sum(dnorm(x = colMeans(Res_10[[14]]$replicate_parameters[paste0("Rt_rw_",1:10)]), mean = 0, sd = 0.2, log = TRUE)))

plot(x = seq(-3,3,by = 0.01), dnorm(x = seq(-3,3,by = 0.01), mean = 0, sd = 0.2, log = TRUE), type = "l", main = "Rt_rw", ylab = "LL", xlab = "Rt_rw")
abline(v = unlist(lapply(Res_10[[10]]$replicate_parameters[paste0("Rt_rw_",1:10)], mean)))
abline(v = unlist(lapply(Res_10[[14]]$replicate_parameters[paste0("Rt_rw_",1:10)], mean)), col = "red")
# abline(v = unlist(lapply(Res_10[[14]]$replicate_parameters[paste0("Rt_rw_",1:10)], function(x){log(mean(dnorm(x = x, mean = 0, sd = 0.2, log = F)))
})), col = "red")

## Prior is made of Start date
log(mean(exp(dunif(x = -55:-10, min = -55, max = -10, log = TRUE)[1] + ## Start date is uniform
  ## R0
  dnorm(x = Res_10[[10]]$replicate_parameters$R0, mean = 3, sd = 1, log = TRUE) +
  ## Rt shift
  dnorm(x = Res_10[[10]]$replicate_parameters$Rt_shift, mean = 0, sd = 1, log = TRUE) +
  ## Rt shift scale, also uniform
  dunif(x = Res_10[[10]]$replicate_parameters$Rt_shift_scale, min = 0.1, max = 10, log = TRUE) +
  ## Rt rw parameters
  sum(unlist(lapply(Res_10[[10]]$replicate_parameters[paste0("Rt_rw_",1:10)], function(x){
    # browser()
    log(mean(dnorm(x = x, mean = 0, sd = 0.2, log = F)))
  }))) +
    dnorm(x = Res_10[[10]]$replicate_parameters$Meff, mean = 0, sd = 1, log = TRUE) +
    dunif(x = Res_10[[10]]$replicate_parameters$Meff_pl, min = 0, max = 1, log = TRUE)
)))

log(mean(exp(dunif(x = -55:-10, min = -55, max = -10, log = TRUE)[1] + ## Start date is uniform
               ## R0
               dnorm(x = Res_10[[14]]$replicate_parameters$R0, mean = 3, sd = 1, log = TRUE) +
               ## Rt shift
               dnorm(x = Res_10[[14]]$replicate_parameters$Rt_shift, mean = 0, sd = 1, log = TRUE) +
               ## Rt shift scale, also uniform
               dunif(x = Res_10[[14]]$replicate_parameters$Rt_shift_scale, min = 0.1, max = 10, log = TRUE) +
               ## Rt rw parameters
               sum(unlist(lapply(Res_10[[14]]$replicate_parameters[paste0("Rt_rw_",1:10)], function(x){
                 # browser()
                 log(mean(dnorm(x = x, mean = 0, sd = 0.2, log = F)))
               }))) +
               dnorm(x = Res_10[[14]]$replicate_parameters$Meff, mean = 0, sd = 1, log = TRUE) +
               dunif(x = Res_10[[14]]$replicate_parameters$Meff_pl, min = 0, max = 1, log = TRUE)
               )))



sum(unlist(lapply(Res_10[[10]]$replicate_parameters[paste0("Rt_rw_",1:10)], function(x){
  # browser()
  log(mean(dnorm(x = x, mean = 0, sd = 0.2, log = F)))
})) - unlist(lapply(Res_10[[14]]$replicate_parameters[paste0("Rt_rw_",1:10)], function(x){
  # browser()
  log(mean(dnorm(x = x, mean = 0, sd = 0.2, log = F)))
})))



hist(Res_10[[10]]$replicate_parameters$R0, main = "R0", xlab = "R0")
hist(Res_10[[14]]$replicate_parameters$R0, main = "R0", xlab = "R0")
mean(Res_10[[10]]$replicate_parameters$R0)
mean(Res_10[[14]]$replicate_parameters$R0)
plot(x = seq(2,4, by = 0.01), dnorm(x = seq(2,4, by = 0.01), mean = 3, sd = 1, log = TRUE), type = "l", main = "R0", ylab = "LL", xlab = "R0")
abline(v = c(mean(Res_10[[10]]$replicate_parameters$R0),
             mean(Res_10[[14]]$replicate_parameters$R0)))

dnorm(x = c(mean(Res_10[[10]]$replicate_parameters$R0),
            mean(Res_10[[14]]$replicate_parameters$R0)), mean = 3, sd = 1, log = TRUE)

hist(Res_10[[10]]$replicate_parameters$Rt_shift)
hist(Res_10[[14]]$replicate_parameters$Rt_shift)
mean(Res_10[[10]]$replicate_parameters$Rt_shift)
mean(Res_10[[14]]$replicate_parameters$Rt_shift)

plot(x = seq(-0.1,0.1,by =0.01), dnorm(x = seq(-0.1,0.1,by =0.01), mean = 0, sd = 1, log = TRUE), type = "l", main="Rtshift", ylab = "LL", xlab = "Rtshift")
abline(v = c(mean(Res_10[[10]]$replicate_parameters$Rt_shift),
             mean(Res_10[[14]]$replicate_parameters$Rt_shift)))

dnorm(x = c(mean(Res_10[[10]]$replicate_parameters$Rt_shift),
            mean(Res_10[[14]]$replicate_parameters$Rt_shift)), mean = 0, sd = 1, log = TRUE)



hist(Res_10[[10]]$replicate_parameters$Rt_shift_scale)
hist(Res_10[[14]]$replicate_parameters$Rt_shift_scale)
mean(Res_10[[10]]$replicate_parameters$Rt_shift_scale)
mean(Res_10[[14]]$replicate_parameters$Rt_shift_scale)

plot(x = seq(0.1,10,by =0.01), dunif(x = seq(0.1,10,by =0.01), min = 0.1, max = 10, log = TRUE), type = "l")
abline(v = c(mean(Res_10[[10]]$replicate_parameters$Rt_shift_scale),
             mean(Res_10[[14]]$replicate_parameters$Rt_shift_scale)))

dunif(x = c(mean(Res_10[[10]]$replicate_parameters$Rt_shift_scale),
            mean(Res_10[[14]]$replicate_parameters$Rt_shift_scale)), min = 0.1, max = 10, log = TRUE)



hist(Res_10[[10]]$replicate_parameters$Rt_rw_1)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_1)
mean(Res_10[[10]]$replicate_parameters$Rt_rw_1)
mean(Res_10[[14]]$replicate_parameters$Rt_rw_1)

plot(x = seq(-0.5,0.5,by =0.01), dnorm(x = seq(-0.5,0.5,by =0.01), mean = 0, sd = 0.2, log = TRUE), type = "l")
abline(v = c(mean(Res_10[[10]]$replicate_parameters$Rt_rw_1),
             mean(Res_10[[14]]$replicate_parameters$Rt_rw_1)))

dnorm(x = c(mean(Res_10[[10]]$replicate_parameters$Rt_rw_1),
            mean(Res_10[[14]]$replicate_parameters$Rt_rw_1)), mean = 0, sd = 0.2, log = TRUE)

dnorm(x = c(mean(Res_10[[10]]$replicate_parameters$Rt_rw_2),
            mean(Res_10[[14]]$replicate_parameters$Rt_rw_2)), mean = 0, sd = 0.2, log = TRUE)





hist(Res_10[[10]]$replicate_parameters$Rt_rw_2)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_2)
hist(Res_10[[10]]$replicate_parameters$Rt_rw_3)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_3)
hist(Res_10[[10]]$replicate_parameters$Rt_rw_4)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_4)
hist(Res_10[[10]]$replicate_parameters$Rt_rw_5)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_5)
hist(Res_10[[10]]$replicate_parameters$Rt_rw_6)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_6)
hist(Res_10[[10]]$replicate_parameters$Rt_rw_7)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_7)
hist(Res_10[[10]]$replicate_parameters$Rt_rw_8)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_8)
hist(Res_10[[10]]$replicate_parameters$Rt_rw_9)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_9)
hist(Res_10[[10]]$replicate_parameters$Rt_rw_10)
hist(Res_10[[14]]$replicate_parameters$Rt_rw_10)

## Plot traces:
par(mfrow = c(1,3))
plot(Res_10[[10]]$pmcmc_results$chains$chain1$results$start_date)
plot(Res_10[[10]]$pmcmc_results$chains$chain2$results$start_date)
plot(Res_10[[10]]$pmcmc_results$chains$chain3$results$start_date)

plot(Res_10[[14]]$pmcmc_results$chains$chain1$results$start_date)
plot(Res_10[[14]]$pmcmc_results$chains$chain2$results$start_date)
plot(Res_10[[14]]$pmcmc_results$chains$chain3$results$start_date)


Res_10_Likelihoods_Reps <- lapply(X =1:length(Res_10), FUN = Get_the_replicate_likelihood_values,
                       fit_Model_list = Res_10)


sapply(Res_10_plots,"[[",1)
sapply(1:24, function(x){
  # browser()
  round(log(Brobdingnag::sum(exp(Brobdingnag::as.brob(Res_10_Likelihoods_Reps[[x]]$ll_tot)))/100))
})

sapply(1:24, function(x){
  log(Brobdingnag::sum(exp(Brobdingnag::as.brob(Res_10_Likelihoods_Reps[[x]]$ll_bin)))/100) +
    log(Brobdingnag::sum(exp(Brobdingnag::as.brob(Res_10_Likelihoods_Reps[[x]]$ll_pois)))/100) +
    log(Brobdingnag::sum(exp(Brobdingnag::as.brob(Res_10_Likelihoods_Reps[[x]]$ll_comb)))/100)
})

unlist(sapply(1:24, function(x){
  round(log(Brobdingnag::sum(exp(Brobdingnag::as.brob(Res_10_Likelihoods_Reps[[x]]$ll_bin +
    Res_10_Likelihoods_Reps[[x]]$ll_pois +
    Res_10_Likelihoods_Reps[[x]]$ll_comb)))/100))
}))



Get_the_replicate_likelihood_values(fit_num = 1, fit_Model_list = list(Res_10$X41), IFRvals = IFR_coefficients[41,])

unlist(lapply(Res_10, get_Prior))
unlist(lapply(Res_10, get_Posterior))

unlist(lapply(Res_10, get_Likelihood))

