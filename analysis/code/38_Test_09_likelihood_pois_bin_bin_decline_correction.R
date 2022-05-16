## Testing the pois bin bin age weeks likelihood
getwd()
rm(list = ls())
## Testing the new likelihood function:
# devtools::install_github("mrc-ide/cma")

# devtools::check()
devtools::install()
# devtools::test()
devtools::load_all()
library(squire)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

## Load data
# combined_data <- readRDS(file = "~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete_weeks_only.rds")
PMP_data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_12_Post_Mortem_Complete_Date.rds")
data <- PMP_data %>% select(date, PosTests) %>% dplyr::rename(deaths = PosTests)

# data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_01_Lusaka_Dist_Deaths_Official.rds")

# PMP_weeks <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_12_Post_Mortem_Complete_weeks.rds")
# Mort_weeks <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_07_Mortuary_data_weeks.rds")
# Comb_weeks <- merge(Mort_weeks,PMP_weeks)
# Comb_weeks[Comb_weeks$week==4, "deaths"] <- round(mean(Comb_weeks[Comb_weeks$week %in% c(3,5), "deaths"]))



Comb_data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")
Comb_data <- merge(Comb_data, data.frame(Age_gr = 1:17, Bg_dr = as.vector(colMeans(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")))*12/52))

# Comb_data %>% filter(Week_gr %in% c(3,4,5))

# Missing_rows <- which(table(Combined_Weeks[,c("Week_gr","Age_group")]) ==0, arr.ind = T)

population <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
Weighted_Durs_Hosp <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
# Lancet_Data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_10_Lancet_Data.rds")
# deaths_age <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_values_Brazeau.rds")
prob_non_severe_death_treatment <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Death_List_log_sc.rds")
baseline_contact_matrix <- as.matrix(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_11_Nyanga_Mixing_Matrix.rds"))


dur_get_ox_survive <- Weighted_Durs_Hosp$Surv_Dur_Weighted
dur_get_ox_die <- Weighted_Durs_Hosp$Death_Dur_Weighted

# readRDS(file = "analysis/data/Code-generated-data/00_09_Crude_Mortality.rds")

# r_maf <- (q1/p_pos - p_bg)/(1-p_bg)
# pop_lu_prov*r_maf*p_pos

# True_covid_deaths <- p_pos * r_maf
# Total_covid_deaths <- p_pos(r_maf + p_bg - r_maf * p_bg)
# Total_covid_deaths <- p_pos(r_maf + p_bg - r_maf * p_bg + r_maf)
# Total_covid_deaths <- p_pos(r_maf + p_bg - r_maf * p_bg + r_maf)
# Total_covid_deaths <- p_pos(r_maf + p_bg - r_maf * p_bg + r_maf)

# r_maf <- Mod_True_covid_deaths/p_pos


# fit_spline_rt(
## So I need to give it the combined data:
# Sys.setenv(SQUIRE_PARALLEL_DEBUG = "TRUE")
Sys.setenv(SQUIRE_PARALLEL_DEBUG = "")

profvis::profvis({
  Test <- cma::fit_spline_rt(prob_non_severe_death_treatment = prob_non_severe_death_treatment$X41,
                             data = data,
                             population = population,
                             baseline_contact_matrix = baseline_contact_matrix,
                             dur_get_ox_survive = dur_get_ox_survive,
                             dur_get_ox_die = dur_get_ox_die,
                             country = "Zambia",
                             reporting_fraction = 1,
                             n_mcmc = 100,
                             replicates = 10,
                             rw_duration = 14,
                             hosp_beds = 1e10,
                             icu_beds = 1e10,
                             prob_severe = rep(0,17),
                             dur_R = Inf,
                             log_likelihood = cma:::calc_loglikelihood_pois_bin_bin_age_weeks_decline_correction_09,
                             lld = "ll_pois_bin_include_4_5",
                             # Combined PMP and Mort data
                             combined_data = Comb_data,
                             # Combined PMP and Mort data per week
                             # combined_data_week = Comb_weeks,
                             # Combined prevalence data
                             comb_df = data.frame(date_start = as.Date("2020-07-04"), date_end = as.Date("2020-07-19"),
                                                  comb_pos = as.integer(0.091*332), samples = 332)
  )

})

# 950ms

Test <- cma::fit_spline_rt(prob_non_severe_death_treatment = prob_non_severe_death_treatment$X41,
                           data = data,
                           population = population,
                           baseline_contact_matrix = baseline_contact_matrix,
                           dur_get_ox_survive = dur_get_ox_survive,
                           dur_get_ox_die = dur_get_ox_die,
                           country = "Zambia",
                           reporting_fraction = 1,
                           n_mcmc = 100,
                           replicates = 10,
                           rw_duration = 14,
                           hosp_beds = 1e10,
                           icu_beds = 1e10,
                           prob_severe = rep(0,17),
                           dur_R = Inf,
                           log_likelihood = cma:::calc_loglikelihood_pois_bin_bin_age_weeks_decline_correction_09,
                           lld = "ll_pois_bin_include_4_5",
                           # Combined PMP and Mort data
                           combined_data = Comb_data,
                           # Combined PMP and Mort data per week
                           # combined_data_week = Comb_weeks,
                           # Combined prevalence data
                           comb_df = data.frame(date_start = as.Date("2020-07-04"), date_end = as.Date("2020-07-19"),
                                                comb_pos = as.integer(0.091*332), samples = 332)
)


# Plots <- Diagnostic_Plot_09(fit_num = 1, fit_Model_list = list(Test2,Test3,Test), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[c(32,33,41),])
# Plots2 <- Diagnostic_Plot_09(fit_num = 1, fit_Model_list = list(Test2), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[32,])
# Plots$Diagnostic
# Plots$Bin_fits
# Plots$Pois_fits
# Plots$Infection_rate

# cowplot::plot_grid(Plots$Bin_fits,Plots$Pois_fits)
# cowplot::plot_grid(Plots2$Bin_fits,Plots2$Pois_fits)



# Plots2$Diagnostic
# Plots2$Bin_fits
# Plots2$Pois_fits
# Plots2$Infection_rate


# readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),][26,]
# Plots <- Diagnostic_Plot_08(fit_num = 1, fit_Model_list = list(Test), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[1,])

# Plots$Infection_rate

# Test$pmcmc_results$chains$chain1$results$log_posterior
# Test$pmcmc_results$chains$chain1$results$log_likelihood
# Diagnostic_Plot_06(1, fit_Model_list = list(Test), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds"), ll_type = "ll_pois")



# Select_Runs <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),] %>%
#   mutate(run = 1:54) %>%
#   filter(Slope_x %in% c(0.8,1,1.25)) %>%
#   pull(run)

IFR_coefficients <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds") %>%
  mutate(Index = 1:nrow(.))

Select_Runs <- IFR_coefficients %>%
  filter(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_death_logical_log_sc.rds"),
         Slope_x %in% c(0.8,1,1.25)) %>%
  pull(Index)

# IFR_mat <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")
# IFR_vec <- 1:nrow(IFR_mat) %in% readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds")
# IFR_mat_fil <- IFR_mat[IFR_vec,]
# rownames(IFR_mat_fil) <- 1:nrow(IFR_mat_fil)
# IFR_mat_fil <- IFR_mat_fil %>% select(IFR_x, Slope_x)

# which(1:nrow(IFR_mat) %in% as.numeric(rownames(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),])))

# readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),]

# as.numeric(gsub("X", "", names(Mod_Res1)))

Mod_Res1 <- readRDS("../Bonus Files/2022-04-04_09_pois_bin_bin_u5_IFR_200.rds")
Res_Figs1 <- lapply(X =1:length(Mod_Res1), FUN = Diagnostic_Plot_09, fit_Model_list = Mod_Res1, IFRvals = IFR_coefficients[Select_Runs,])
Heatmaps1 <- Plot_Heatmaps(Mod_Res = Mod_Res1, Res_Figs = Res_Figs1, Select_Runs = Select_Runs, Title = "Under 5 IFR: 1/200")
pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_u5_IFR_200.pdf", height = 9, width = 12)
Heatmaps1
Res_Figs1
dev.off()

Mod_Res2 <- readRDS("../Bonus Files/2022-04-04_09_pois_bin_bin_u5_IFR_500.rds")
Res_Figs2 <- lapply(X =1:length(Mod_Res2), FUN = Diagnostic_Plot_09, fit_Model_list = Mod_Res2, IFRvals = IFR_coefficients[Select_Runs,])
Heatmaps2 <- Plot_Heatmaps(Mod_Res = Mod_Res2, Res_Figs = Res_Figs2, Select_Runs = Select_Runs, Title = "Under 5 IFR: 1/500")
pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_u5_IFR_500.pdf", height = 9, width = 12)
Heatmaps2
Res_Figs2
dev.off()

Mod_Res3 <- readRDS("../Bonus Files/2022-04-04_09_pois_bin_bin_u5_IFR_1000.rds")
# Mod_Res3$X36 <- NULL
Res_Figs3 <- lapply(X =1:length(Mod_Res3), FUN = Diagnostic_Plot_09, fit_Model_list = Mod_Res3, IFRvals = IFR_coefficients[Select_Runs,])
Heatmaps3 <- Plot_Heatmaps(Mod_Res = Mod_Res3, Res_Figs = Res_Figs3, Select_Runs = Select_Runs, Title = "Under 5 IFR: 1/1000")
pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_u5_IFR_1000.pdf", height = 9, width = 12)
Heatmaps3
Res_Figs3
dev.off()

# Mod_Res4 <- readRDS("../Bonus Files/2022-03-17_09_pois_bin_bin_remove_4_5_from_pois_ll_only.rds")
# Res_Figs4 <- lapply(X =1:length(Mod_Res4), FUN = Diagnostic_Plot_09, fit_Model_list = Mod_Res4, IFRvals = IFR_coefficients[Select_Runs,])
# Heatmaps4 <- Plot_Heatmaps(Mod_Res = Mod_Res4, Res_Figs = Res_Figs4, Select_Runs = Select_Runs, Title = "Remove weeks 4-5 from pois ll only")
# pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_remove_4_5_from_pois_ll_only.pdf", height = 9, width = 12)
# Heatmaps4
# Res_Figs4
# dev.off()

# Mod_Res5 <- readRDS("../Bonus Files/2022-04-01_09_pois_bin_bin_include_4_5.rds")
Mod_Res5 <- readRDS("../Bonus Files/2022-03-31_09_pois_bin_bin_include_4_5.rds")
Res_Figs5 <- lapply(X =1:length(Mod_Res5), FUN = Diagnostic_Plot_09, fit_Model_list = Mod_Res5, IFRvals = IFR_coefficients[Select_Runs,])
# Res_Figs5_test <- lapply(X =1, FUN = Diagnostic_Plot_09, fit_Model_list = list(Mod_Res5$X34), IFRvals = IFR_coefficients[41,])
Heatmaps5 <- Plot_Heatmaps(Mod_Res = Mod_Res5, Res_Figs = Res_Figs5, Select_Runs = Select_Runs, Title = "Pois-bin-bin fit, inlude weeks 4-5")
pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_include_4_5.pdf", height = 9, width = 12)
Heatmaps5
Res_Figs5
dev.off()

Mod_Res6 <- readRDS("../Bonus Files/2022-04-04_09_pois_bin_bin_remove_youngest_age_group_from_fit.rds")
Res_Figs6 <- lapply(X =1:length(Mod_Res6), FUN = Diagnostic_Plot_09, fit_Model_list = Mod_Res6, IFRvals = IFR_coefficients[Select_Runs,])
Heatmaps6 <- Plot_Heatmaps(Mod_Res = Mod_Res6, Res_Figs = Res_Figs6, Select_Runs = Select_Runs, Title = "Remove youngest age group from fit")
pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_remove_youngest.pdf", height = 9, width = 12)
Heatmaps6
Res_Figs6
dev.off()



# Plot Rt
plot(c(mean(Mod_Res5$X28$replicate_parameters$R0),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_1),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_2),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_3),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_4),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_5),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_6),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_7),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_8),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_9),
mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_10)))

squire:::evaluate_Rt_pmcmc()

Rt_vals_Reps <- lapply(seq_along(Mod_Res5$X28$replicate_parameters$R0), function(y) {

  tt <- squire:::intervention_dates_for_odin(dates = Mod_Res5$X28$interventions$date_R0_change,
                                             change = Mod_Res5$X28$interventions$R0_change,
                                             start_date = Mod_Res5$X28$replicate_parameters$start_date[y],
                                             steps_per_day = 1/Mod_Res5$X28$parameters$dt)

  Rt <- squire:::evaluate_Rt_pmcmc(
    R0_change = tt$change,
    date_R0_change = tt$dates,
    R0 = Mod_Res5$X28$replicate_parameters$R0[y],
    pars = as.list(Mod_Res5$X28$replicate_parameters[y,]),
    Rt_args = Mod_Res5$X28$pmcmc_results$inputs$Rt_args)

  data.frame(Date = as.Date(tt$dates), Rt) %>% filter(Date>="2020-06-15") %>%
    pull(Rt)
  # browser()

  })

Mod_Res5$X28$replicate_parameters[2,]

## I want to plot the first replicate along date
Res_Rt <- as.data.frame(do.call(cbind, Rt_vals_Reps)) %>%
  rowwise() %>%
  mutate(mean = mean(c_across(where(is.numeric))),
         min = min(c_across(where(is.numeric))),
         max = max(c_across(where(is.numeric)))) %>%
  select(mean,max,min) %>%
  cbind(date = seq.Date(from = as.Date("2020-06-15"), to = as.Date("2020-10-02"), by = 1),.)

ggplot(data = Res_Rt, aes(x = date)) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3) +
  xlab("Date") + ylab("Rt")


plot(data.frame(Date = seq.Date(from = as.Date("2020-06-15"), to = as.Date("2020-10-02"), by = 1),rowMeans(do.call(cbind, Rt_vals_Reps))), type = "l")


Rt_vals_Reps[[1]]

plot(Rt_vals_Reps[[2]], type = "l")




# readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_deaths_index_log_sc.rds")

# pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_remove_include_4_5_heatmaps.pdf", height = 12, width = 18)
# Heatmaps1
# Heatmaps2
# Heatmaps3
# dev.off()



# Mod_Res2 <- readRDS("../Bonus Files/2022-03-15_09_pois_bin_bin_include_4_5.rds")
# Mod_Res3 <- readRDS("../Bonus Files/2022-03-15_09_pois_bin_bin_remove_4_5_full_durations.rds")
# Res_Figs <- lapply(X =1:3, FUN = Diagnostic_Plot_09, fit_Model_list = list(Test2,Test3,Test), IFRvals = readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[c(32,33,41),])
Res_Figs2 <- lapply(X =1:length(Mod_Res2), FUN = Diagnostic_Plot_09, fit_Model_list = Mod_Res2, IFRvals = Select_IFRs)
Res_Figs3 <- lapply(X =1:length(Mod_Res3), FUN = Diagnostic_Plot_09, fit_Model_list = Mod_Res3, IFRvals = Select_IFRs)


pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_include_4_5.pdf", height = 9, width = 9)
Res_Figs2
dev.off()

pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_remove_4_5_full_durations.pdf", height = 9, width = 9)
Res_Figs3
dev.off()


# Res_Figs[[1]]$Diagnostic
# Res_Figs[[5]]$Diagnostic
# Res_Figs[[25]]$Diagnostic
# Res_Figs[[51]]$Diagnostic


# Res_Figs[[1]]$lls





# pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_include_4_5_heatmaps.pdf", height = 12, width = 18)
# Heatmaps2
# dev.off()
#
# pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_remove_4_5_full_durations_heatmaps.pdf", height = 12, width = 18)
# Heatmaps3
# dev.off()






  # Res1$X1$pmcmc_results$chains$chain1$results$log_likelihood
  # Res1$X1$pmcmc_results$chains$chain1$results$log_prior

  ## Plot heatmap of Posteriors
  IFR_mat <- IFR_mat %>% mutate(Post_col_group = as.numeric(cut(round(AvPost),
                                                                breaks= c(-Inf,round(max(AvPost,na.rm = T))-500,
                                                                          round(max(AvPost,na.rm = T))-200,
                                                                          round(max(AvPost,na.rm = T))-100,
                                                                          round(max(AvPost,na.rm = T))-50,
                                                                          round(max(AvPost,na.rm = T))-20,
                                                                          round(max(AvPost,na.rm = T))-12,
                                                                          round(max(AvPost,na.rm = T))-8,
                                                                          round(max(AvPost,na.rm = T))-4,
                                                                          round(max(AvPost,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))


  ggplot(IFR_mat, aes(x = IFR_x, y = Slope_x, fill = Post_col_group)) + geom_tile() +
    geom_text(aes(label = round(AvPost), colour = (Post_col_group >= max(Post_col_group, na.rm=T))), size = 4) +
    scale_colour_manual(values = c("white", "black")) +
    ggtitle("Log Posterior") + xlab("xIFR") + ylab("xSlope") +
    labs(fill = "Mean Post") + theme(legend.position = "none") +
    xlim(0,2) + ylim(0,2) +
    scale_fill_viridis_c()



  lapply(1:length(Res_Figs), function(x){
    Res_Figs[[x]]$lls
  }


}


IFR_mat <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")
IFR_vec <- 1:nrow(IFR_mat) %in% readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds")
IFR_mat_fil <- IFR_mat[IFR_vec,]
rownames(IFR_mat_fil) <- 1:nrow(IFR_mat_fil)

get_Posterior <- function(model_fit){
  # Select all sampled posteriors
  PosC1 <- model_fit$pmcmc_results$chains$chain1$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(model_fit$replicate_parameters), value = T))))]
  PosC2 <- model_fit$pmcmc_results$chains$chain2$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(model_fit$replicate_parameters), value = T))))]
  PosC3 <- model_fit$pmcmc_results$chains$chain3$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(model_fit$replicate_parameters), value = T))))]
  # Average out posteriors
  mean(unlist(list(PosC1,PosC2,PosC3)))
}

# get_Posterior(Res1[[1]])
# get_Likelihood(Res3[[1]])

# tail(Res3$X1$pmcmc_results$chains$chain1$results$log_posterior)

IFR_mat$AvPost <- NA
IFR_mat$AvPost[IFR_vec] <- unlist(lapply(Mod_Res, get_Posterior))

# Res1$X1$pmcmc_results$chains$chain1$results$log_likelihood
# Res1$X1$pmcmc_results$chains$chain1$results$log_prior

## Plot heatmap of Posteriors
IFR_mat <- IFR_mat %>% mutate(Post_col_group = as.numeric(cut(round(AvPost),
                                                              breaks= c(-Inf,round(max(AvPost,na.rm = T))-500,
                                                                        round(max(AvPost,na.rm = T))-200,
                                                                        round(max(AvPost,na.rm = T))-100,
                                                                        round(max(AvPost,na.rm = T))-50,
                                                                        round(max(AvPost,na.rm = T))-20,
                                                                        round(max(AvPost,na.rm = T))-12,
                                                                        round(max(AvPost,na.rm = T))-8,
                                                                        round(max(AvPost,na.rm = T))-4,
                                                                        round(max(AvPost,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))


ggplot(IFR_mat, aes(x = IFR_x, y = Slope_x, fill = Post_col_group)) + geom_tile() +
  geom_text(aes(label = round(AvPost), colour = (Post_col_group >= max(Post_col_group, na.rm=T))), size = 4) +
  scale_colour_manual(values = c("white", "black")) +
  ggtitle("Log Posterior") + xlab("xIFR") + ylab("xSlope") +
  labs(fill = "Mean Post") + theme(legend.position = "none") +
  xlim(0,2) + ylim(0,2) +
  scale_fill_viridis_c()































p1 <- plot(Test, particle_fit = T) + theme(legend.position = "none")

readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")
Diagnostic_Plot_06(1, fit_Model_list = list(Tests_shorter_duration[[1]]), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds"), ll_type = "ll_pois")
Diagnostic_Plot_06b_Age_Date(1, fit_Model_list = list(Tests_shorter_duration[[3]]), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds"), ll_type = "ll_pois")


Testx3 <- readRDS(file = "../Bonus Files/2022-03-03_Testing_pois_bin_bin_x3.rds")
Diagnostic_Plot_06(1, fit_Model_list = list(Testx3[[1]]), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds"), ll_type = "ll_pois")

Testx3$X1$pmcmc_results$chains$chain2$results$log_likelihood
Testx3$X33$pmcmc_results$chains$chain1$results$log_likelihood
Testx3$X37$pmcmc_results$chains$chain1$results$log_likelihood

1046+ 156.5+7.4
906 + 150.9 + 21.5



index <- squire:::odin_index(Test$model)
Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                 to = as.Date("2020-10-02"),
                                 by = "week"),as.Date("2020-10-02"))
Mort_Deaths_Age_Week <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age.rds") %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  group_by(week, Age_group) %>%
  summarise(total_deaths = sum(total_deaths),
            date = head(date,1))


Mod_Age_Deaths_Lus <- apply(Test$output,3,function(x){

  AgeRes <- lapply(1:ncol(x[,index$S]),function(Age_Group){
    Ds_Lus <- diff(x[as.Date(rownames(x)) %in% Days_for_comparison, index$D][,Age_Group],na.rm = T)
    Ds_Mor <- Ds_Lus*0.8
    ll <- dpois(x = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_group==Age_Group,]$total_deaths, lambda = Ds_Mor + mean(Samples_age_ests[,Age_Group]*12/52), log = T)

    return(data.frame(Week_gr = 1:16,
                      Age_group = Age_Group,
                      Ds_Lus = Ds_Lus,
                      Ds_Mor = Ds_Mor,
                      Ds_Tot_Mor = Ds_Mor + mean(Samples_age_ests[,Age_Group]*12/52),
                      total_deaths = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_group==Age_Group,]$total_deaths,
                      ll = ll))
  })

  AgeRes <- do.call(rbind.data.frame, AgeRes)
  AgeRes
})

Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus %>%
  str2str::ld2a() %>%
  cbind(.[,c("Age_group","Week_gr","total_deaths"),1],
        data.frame(Mean = apply(.[,c("Ds_Lus","Ds_Mor","Ds_Tot_Mor","ll"),], MARGIN = 2, FUN = function(x){rowMeans(x)}),
                   Max = apply(.[,c("Ds_Lus","Ds_Mor","Ds_Tot_Mor","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = max)}),
                   Min = apply(.[,c("Ds_Lus","Ds_Mor","Ds_Tot_Mor","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = min)}))) %>%
  select(Age_group,Week_gr,total_deaths,
         Mean.Ds_Lus, Max.Ds_Lus, Min.Ds_Lus,
         Mean.Ds_Mor, Max.Ds_Mor, Min.Ds_Mor,
         Mean.Ds_Tot_Mor, Max.Ds_Tot_Mor, Min.Ds_Tot_Mor,
         Mean.ll, Max.ll, Min.ll) %>%
  arrange(Age_group, Week_gr) %>%
  filter(Week_gr != 4)
rownames(Mod_Age_Deaths_Lus_Av) <- NULL

## Plot by week
Mod_Age_Deaths_Lus_Av_Week <- merge(Mod_Age_Deaths_Lus_Av, Mort_Deaths_Age_Week[,c("week","Age_group","date")] %>% rename(Week_gr = week)) %>% group_by(Week_gr) %>%
  summarise(date = head(date,1)+3,#Week_gr = head(Week_gr,1),
            total_deaths = sum(total_deaths),
            Mean.Ds_Tot_Mor = sum(Mean.Ds_Tot_Mor),
            Max.Ds_Tot_Mor = sum(Max.Ds_Tot_Mor),
            Min.Ds_Tot_Mor = sum(Min.Ds_Tot_Mor)
            # Could add pcr, but this would need to be weighted by population size in each age group?
  )

p1 <- ggplot(Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
  # Modelled Positive deaths
  geom_line(aes(y=Mean.Ds_Tot_Mor),linetype="dashed") +
  geom_ribbon(aes(ymin=Min.Ds_Tot_Mor, ymax=Max.Ds_Tot_Mor), alpha=0.3) +
  # Modelled True deaths
  # geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
  # geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
  # Scaled positive deaths
  geom_point(aes(y = total_deaths)) +
  # Scaled True deaths
  # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
  xlab("Date") + ylab("Deaths") +
  xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
  ggtitle(paste("COVID-19 weekly deaths\n(Mortuary) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
  theme(plot.title = element_text(size = 10))

Age_groups.labs <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_group) %>%
  summarise(Mean.ll = sum(Mean.ll)) %>%
  mutate(lab = paste0("pois ll AG ",Age_group,": ", round(Mean.ll,1))) %>%
  select(lab) %>%
  unlist()
#
names(Age_groups.labs) <- 1:17


p2 <- ggplot(merge(Mod_Age_Deaths_Lus_Av, Mort_Deaths_Age_Week[,c("week","Age_group","date")] %>% rename(Week_gr = week)), aes(x = date)) +
  # Modelled Positive deaths
  geom_line(aes(y=Mean.Ds_Tot_Mor),linetype="dashed") +
  geom_ribbon(aes(ymin=Min.Ds_Tot_Mor, ymax=Max.Ds_Tot_Mor), alpha=0.3) +
  facet_wrap(vars(Age_group), labeller = labeller(Age_group = Age_groups.labs)) +
  # Modelled True deaths
  # geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
  # geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
  # Scaled positive deaths
  geom_point(aes(y = total_deaths)) +
  # Scaled True deaths
  # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
  xlab("Week") + ylab("Deaths") +
  # xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
  ggtitle(paste("COVID-19 weekly deaths\n(Lusaka) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
  theme(plot.title = element_text(size = 10))









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
