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

## Load data
PMP_data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_12_Post_Mortem_Complete_Date.rds")
data <- PMP_data %>% select(date, PosTests) %>% dplyr::rename(deaths = PosTests)

Comb_data <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")
# Comb_data <- merge(Comb_data, data.frame(Age_gr = 1:17, Bg_dr = as.vector(colMeans(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")))*12/52))

population <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
Weighted_Durs_Hosp <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
prob_non_severe_death_treatment <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Death_Matrix_log_sc.rds")
baseline_contact_matrix <- as.matrix(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_11_Nyanga_Mixing_Matrix.rds"))

dur_get_ox_survive <- Weighted_Durs_Hosp$Surv_Dur_Weighted/2
dur_get_ox_die <- Weighted_Durs_Hosp$Death_Dur_Weighted/2

## Create a dummy standardise file
## I want 100 rows with a value for each of the parameters:

# mcmc <- readRDS("../Bonus Files/2022-04-05_mcmc_baseline.rds")
mcmc <- readRDS("../Bonus Files/2022-04-25_Full_Baseline_MCMC_5e2_25e2.rds")

# library(drjacoby)
drjacoby::plot_par(mcmc, phase = "sampling")
# plot_par(mcmc, show = "U_5_Rate_Week1", phase = "sampling")
drjacoby::plot_par(mcmc, show = "U_5_Rate_Week4", phase = "burnin")
drjacoby::plot_par(mcmc, show = "RR16", phase = "sampling")
# plot_par(mcmc, show = "U_5_Rate_Week4", phase = "sampling")


mcmc_samples <-mcmc$output %>% filter(phase =="sampling")

# Of the 12500 samples, I need to get 500.
set.seed(6)
sample_index <- sample(x = 1:nrow(mcmc_samples), size = 500, replace = F)
mcmc_samples <- mcmc_samples[sample_index,]
# mcmc_samples[sample_index,]


## AG1 deaths in mortuary: Mort_deaths_mcmc: 2020_AG1
# This gets included in ncdeaths below.
AG1_2020_mcmc <- mcmc_samples[, c(paste0("U_5_Rate_Week",129:144))]

## AG1 Pre 2020 background death rate: Bg_dr_mcmc
AG1_pre2020_mcmc <- rowMeans(mcmc_samples[, c(paste0("U_5_Rate_Week",1:105))])

pre2020_mcmc <- lapply(1:nrow(mcmc_samples), function(x){
  data.frame(Age_gr = 1:17,
             Bg_dr = unlist(c(AG1_pre2020_mcmc[x], AG1_pre2020_mcmc[x] * mcmc_samples[x,paste0("RR",2:17)])))
  })
# rownames(pre2020_mcmc) <- 1:17
# colnames(pre2020_mcmc) <- 1:500
# pre2020_mcmc <- tibble::rownames_to_column(as.data.frame(pre2020_mcmc), var = "Age_gr")

## Non-covid deaths in the mortuary: 2020_AG1 * RR
Mort_ncd_mcmc <- lapply(1:nrow(AG1_2020_mcmc), function(x){
  mcmc_samples <- cbind(as.numeric(AG1_2020_mcmc[x,]),
                        as.numeric(AG1_2020_mcmc[x,]) %*% t(as.numeric(mcmc_samples[x,paste0("RR",2:17)])))
  rownames(mcmc_samples) <- 1:16
  colnames(mcmc_samples) <- 1:17
  mcmc_samples <- mcmc_samples %>% melt(value.name = "Mort_ncd_mcmc", varnames = c("Week_gr", "Age_gr"))

  return(mcmc_samples = mcmc_samples)
})


# Get the standardisation values
# ag_1_mcmc <- AG1_2020_mcmc %>% filter(Age_gr ==1) %>%
#   select(-Age_gr)


## Pre2020_drs
# rowMeans(mcmc_samples[,1:16])


# mcmc_samples <- mcmc_samples[, c(paste0("U_5_Rate_Week",129:144),paste0("RR",2:17))]

# plot(1:16, mcmc_samples[1,1:16], xlab = "Week", ylim = c(0,160), ylab = "Baseline deaths AG1")
# sapply(2:100, function(x){points(x = 1:16, y = mcmc_samples[x,1:16])})

# plot(1:16, mcmc_samples[1,17:32], xlab = "Week", ylim = c(0,2), ylab = "Baseline deaths AG1")
# sapply(2:100, function(x){points(x = 1:16, y = mcmc_samples[x,17:32])})

# Testdf <- matrix(c(rnorm(n = 100*16, mean = 30, sd = 10),rlnorm(n = 100*16, meanlog = 0, sdlog = 3)), nrow = 100,
#                  dimnames = list(NULL, c(paste0("Weekno",1:16),paste0("Age_gr",2:17))))

# Testdf[1,]
# Testdf[1,grep("RR",colnames(Testdf))]
# mcmc_samples_1 <- cbind(as.numeric(mcmc_samples[1,grep("U_5",colnames(mcmc_samples))]),
#            as.numeric(mcmc_samples[1,grep("U_5",colnames(mcmc_samples))]) %*% t(as.numeric(mcmc_samples[1,grep("RR",colnames(mcmc_samples))])))

# rownames(mcmc_samples_1) <- 1:16
# colnames(mcmc_samples_1) <- 1:17



# mcmc_samples_list <- lapply(1:nrow(mcmc_samples), function(x){
#   mcmc_samples <- cbind(as.numeric(mcmc_samples[x,grep("U_5",colnames(mcmc_samples))]),
#                           as.numeric(mcmc_samples[x,grep("U_5",colnames(mcmc_samples))]) %*% t(as.numeric(mcmc_samples[x,grep("RR",colnames(mcmc_samples))])))
#   rownames(mcmc_samples) <- 1:16
#   colnames(mcmc_samples) <- 1:17
#   return(mcmc_samples)
# })

## So I need to give it the combined data:
Sys.setenv(SQUIRE_PARALLEL_DEBUG = "TRUE")
# Sys.setenv(SQUIRE_PARALLEL_DEBUG = "")


## So the Mort_ncd_mcmc are the estimates for baseline deaths
# Mort_ncd_mcmc


dfj_mcmc_data <- lapply(1:length(Mort_ncd_mcmc) , function(x){
  merge(Mort_ncd_mcmc[x], pre2020_mcmc[[x]]) %>%
    mutate(ag1std = Mort_ncd_mcmc/Bg_dr)})

# profvis::profvis({
# system.time(

options(error=browser)

  Test2 <- cma::fit_spline_rt(prob_non_severe_death_treatment = prob_non_severe_death_treatment$X26,
                              data = data,
                              population = population,
                              baseline_contact_matrix = baseline_contact_matrix,
                              dur_get_ox_survive = dur_get_ox_survive,
                              dur_get_ox_die = dur_get_ox_die,
                              country = "Zambia",
                              reporting_fraction = 1,
                              n_mcmc = 1000,
                              replicates = 100,
                              rw_duration = 14,
                              hosp_beds = 1e10,
                              icu_beds = 1e10,
                              prob_severe = rep(0,17),
                              dur_R = Inf,
                              log_likelihood = cma:::calc_loglikelihood_10_pois_bin_bin_ag1std_agRR,
                              lld = "ll_pois_bin_include_4_5",
                              # lld = "ll_pois_bin",
                              # Combined PMP and Mort data
                              combined_data = list(Comb_data = Comb_data,
                                                   dfj_mcmc_data = dfj_mcmc_data),
                              # Combined prevalence data
                              comb_df = data.frame(date_start = as.Date("2020-07-04"), date_end = as.Date("2020-07-19"),
                                                   comb_pos = as.integer(0.091*332), samples = 332)
  )
# )
# saveRDS(Test2, "../Bonus Files/2022-04-20_Test_for_some_reason_I_think_the_10likelihood_test.rds")
## This took 20807 seconds

IFR_coefficients <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds") %>%
  mutate(Index = 1:nrow(.))

Select_Runs <- IFR_coefficients %>%
  filter(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_death_logical_log_sc.rds"),
         Slope_x %in% c(0.8,1,1.25)) %>%
  pull(Index)

Test2 <- readRDS("../Bonus Files/2022-04-20_Test_for_some_reason_I_think_the_10likelihood_test.rds")
test_plots <- cma::Diagnostic_Plot_09(fit_num = 1, fit_Model_list = list(Test2), IFRvals = IFR_coefficients[41,])
test_plots$Diagnostic
test_plots$Pois_fits
test_plots$lls


# IFR_mat <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")
# IFR_vec <- 1:nrow(IFR_mat) %in% readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds")
# IFR_mat_fil <- IFR_mat[IFR_vec,]
# rownames(IFR_mat_fil) <- 1:nrow(IFR_mat_fil)
# IFR_mat_fil <- IFR_mat_fil %>% select(IFR_x, Slope_x)

# which(1:nrow(IFR_mat) %in% as.numeric(rownames(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),])))

# readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),]

# as.numeric(gsub("X", "", names(Mod_Res1)))



# })
# })
# 259470...

# 17.309 for 100 runs, 10 reps, 2 integrate
# 40 for 100 runs, 10 reps, 10 integrate: 23 extra seconds for 8 more reps: 2.875
# 156.847 for .... 50 integrate: 138.5 seconds for 48 is about 3 seconds each: 2.8
# So at the
# 3*100 is 300 seconds: 5 minutes.
# 3*500 is 1500 seconds: 25 minutes
# >1002 for 500

Comb_data <- merge(Comb_data, data.frame(Age_gr = 1:17, Bg_dr = as.vector(colMeans(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")))*12/52))
system.time(
  Test2 <- cma::fit_spline_rt(prob_non_severe_death_treatment = prob_non_severe_death_treatment$X26,
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
                              # lld = "ll_pois_bin",
                              # Combined PMP and Mort data
                              combined_data = Comb_data,
                              # Combined prevalence data
                              comb_df = data.frame(date_start = as.Date("2020-07-04"), date_end = as.Date("2020-07-19"),
                                                   comb_pos = as.integer(0.091*332), samples = 332)
  )
)
## 14.042



# plot(Test, particle_fit = T)
Plots <- Diagnostic_Plot_09(fit_num = 1, fit_Model_list = list(Test), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[c(41),])



Plots <- Diagnostic_Plot_09(fit_num = 1, fit_Model_list = list(Test2,Test3,Test), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[c(32,33,41),])
# Plots2 <- Diagnostic_Plot_09(fit_num = 1, fit_Model_list = list(Test2), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[32,])
Plots$Diagnostic
Plots$Bin_fits
Plots$Pois_fits
Plots$Infection_rate

cowplot::plot_grid(Plots$Bin_fits,Plots$Pois_fits)
cowplot::plot_grid(Plots2$Bin_fits,Plots2$Pois_fits)



Plots2$Diagnostic
Plots2$Bin_fits
Plots2$Pois_fits
Plots2$Infection_rate


readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),][26,]
# Plots <- Diagnostic_Plot_08(fit_num = 1, fit_Model_list = list(Test), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[1,])

# Plots$Infection_rate

# Test$pmcmc_results$chains$chain1$results$log_posterior
# Test$pmcmc_results$chains$chain1$results$log_likelihood
# Diagnostic_Plot_06(1, fit_Model_list = list(Test), IFRvals = readRDS(file = "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds"), ll_type = "ll_pois")


Mod_Res <- readRDS("../Bonus Files/2022-03-14_09_pois_bin_bin_decline_correction.rds")
# Res_Figs <- lapply(X =1:3, FUN = Diagnostic_Plot_09, fit_Model_list = list(Test2,Test3,Test), IFRvals = readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[c(32,33,41),])
Res_Figs <- lapply(X =1:54, FUN = Diagnostic_Plot_09, fit_Model_list = Mod_Res, IFRvals = readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),])

pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_full.pdf", height = 9, width = 9)
Res_Figs
dev.off()

Res_Figs[[1]]$Diagnostic
Res_Figs[[5]]$Diagnostic
Res_Figs[[25]]$Diagnostic
Res_Figs[[51]]$Diagnostic


Res_Figs[[1]]$lls

# get_Posterior <- function(model_fit){
#   # Select all sampled posteriors
#   PosC1 <- model_fit$pmcmc_results$chains$chain1$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(model_fit$replicate_parameters), value = T))))]
#   PosC2 <- model_fit$pmcmc_results$chains$chain2$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(model_fit$replicate_parameters), value = T))))]
#   PosC3 <- model_fit$pmcmc_results$chains$chain3$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(model_fit$replicate_parameters), value = T))))]
#   # Average out posteriors
#   mean(unlist(list(PosC1,PosC2,PosC3)))
# }

# Plot_Heatmaps <- function(Mod_Res, Res_Figs){
#
#   IFR_mat <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")
#   IFR_vec <- 1:nrow(IFR_mat) %in% readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds")
#   IFR_mat_fil <- IFR_mat[IFR_vec,]
#   rownames(IFR_mat_fil) <- 1:nrow(IFR_mat_fil)
#   IFR_mat_fil <- IFR_mat_fil %>% select(IFR_x, Slope_x)
#
#
#   # IFR_mat$AvPost <- NA
#   IFR_mat$AvPost[IFR_vec] <- unlist(lapply(Mod_Res, get_Posterior))
#   IFR_mat$ll_bin[IFR_vec] <- unlist(lapply(Res_Figs, function(x){x$lls$ll_bin}))
#   IFR_mat$ll_pois[IFR_vec] <- unlist(lapply(Res_Figs, function(x){x$lls$ll_pois}))
#   IFR_mat$ll_pcr[IFR_vec] <- unlist(lapply(Res_Figs, function(x){x$lls$ll_pcr}))
#   IFR_mat$ll_sero[IFR_vec] <- unlist(lapply(Res_Figs, function(x){x$lls$ll_sero}))
#   IFR_mat$ll_comb[IFR_vec] <- unlist(lapply(Res_Figs, function(x){x$lls$ll_comb}))
#
#
#   IFR_mat <- IFR_mat %>% mutate(Post_col_group = as.numeric(cut(round(AvPost),
#                                                                 breaks = c(-Inf, round(max(AvPost,na.rm = T))- c(500,200,100,50,20,12,8,4,0)),
#                                                                 labels = as.character(c(1:9)))),
#                                 Bin_col_group = as.numeric(cut(round(ll_bin),
#                                                                 breaks = c(-Inf, round(max(ll_bin,na.rm = T))- c(500,200,100,50,20,12,8,4,0)),
#                                                                 labels = as.character(c(1:9)))),
#                                 Pois_col_group = as.numeric(cut(round(ll_pois),
#                                                                 breaks = c(-Inf, round(max(ll_pois,na.rm = T))- c(500,200,100,50,20,12,8,4,0)),
#                                                                 labels = as.character(c(1:9)))),
#                                 pcr_col_group = as.numeric(cut(round(ll_pcr),
#                                                                 breaks = c(-Inf, round(max(ll_pcr,na.rm = T))- c(500,200,100,50,20,12,8,4,0)),
#                                                                 labels = as.character(c(1:9)))),
#                                 sero_col_group = as.numeric(cut(round(ll_sero),
#                                                                 breaks = c(-Inf, round(max(ll_sero,na.rm = T))- c(500,200,100,50,20,12,8,4,0)),
#                                                                 labels = as.character(c(1:9)))),
#                                 comb_col_group = as.numeric(cut(round(ll_comb),
#                                                                 breaks = c(-Inf, round(max(ll_comb,na.rm = T))- c(500,200,100,50,20,12,8,4,0)),
#                                                                 labels = as.character(c(1:9)))))
#
#   p1_post <- ggplot(IFR_mat, aes(x = IFR_x, y = Slope_x, fill = Post_col_group)) + geom_tile() +
#     geom_text(aes(label = round(AvPost), colour = (Post_col_group >= max(Post_col_group, na.rm=T))), size = 4) +
#     scale_colour_manual(values = c("white", "black")) +
#     ggtitle("Log Posterior") + xlab("xIFR") + ylab("xSlope") +
#     labs(fill = "Mean Post") + theme(legend.position = "none") +
#     xlim(0,5) + ylim(0,5) +
#     scale_fill_viridis_c()
#
#
#   p2_bin <- ggplot(IFR_mat, aes(x = IFR_x, y = Slope_x, fill = Bin_col_group)) + geom_tile() +
#     geom_text(aes(label = round(ll_bin), colour = (Bin_col_group >= max(Bin_col_group, na.rm=T))), size = 4) +
#     scale_colour_manual(values = c("white", "black")) +
#     ggtitle("ll bin") + xlab("xIFR") + ylab("xSlope") +
#     labs(fill = "Mean ll bin") + theme(legend.position = "none") +
#     xlim(0,5) + ylim(0,5) +
#     scale_fill_viridis_c()
#
#   p3_pois <- ggplot(IFR_mat, aes(x = IFR_x, y = Slope_x, fill = Pois_col_group)) + geom_tile() +
#     geom_text(aes(label = round(ll_pois), colour = (Pois_col_group >= max(Pois_col_group, na.rm=T))), size = 4) +
#     scale_colour_manual(values = c("white", "black")) +
#     ggtitle("ll pois") + xlab("xIFR") + ylab("xSlope") +
#     labs(fill = "Mean ll pois") + theme(legend.position = "none") +
#     xlim(0,5) + ylim(0,5) +
#     scale_fill_viridis_c()
#
#   p4_pcr <- ggplot(IFR_mat, aes(x = IFR_x, y = Slope_x, fill = pcr_col_group)) + geom_tile() +
#     geom_text(aes(label = round(ll_pcr), colour = (pcr_col_group >= max(pcr_col_group, na.rm=T))), size = 4) +
#     scale_colour_manual(values = c("white", "black")) +
#     ggtitle("ll bin pcr") + xlab("xIFR") + ylab("xSlope") +
#     labs(fill = "Mean ll bin pcr") + theme(legend.position = "none") +
#     xlim(0,5) + ylim(0,5) +
#     scale_fill_viridis_c()
#
#   p5_sero <- ggplot(IFR_mat, aes(x = IFR_x, y = Slope_x, fill = sero_col_group)) + geom_tile() +
#     geom_text(aes(label = round(ll_sero), colour = (sero_col_group >= max(sero_col_group, na.rm=T))), size = 4) +
#     scale_colour_manual(values = c("white", "black")) +
#     ggtitle("ll bin sero") + xlab("xIFR") + ylab("xSlope") +
#     labs(fill = "Mean ll bin sero") + theme(legend.position = "none") +
#     xlim(0,5) + ylim(0,5) +
#     scale_fill_viridis_c()
#
#   p6_comb <- ggplot(IFR_mat, aes(x = IFR_x, y = Slope_x, fill = comb_col_group)) + geom_tile() +
#     geom_text(aes(label = round(ll_comb), colour = (comb_col_group >= max(comb_col_group, na.rm=T))), size = 4) +
#     scale_colour_manual(values = c("white", "black")) +
#     ggtitle("ll bin comb") + xlab("xIFR") + ylab("xSlope") +
#     labs(fill = "Mean ll bin comb") + theme(legend.position = "none") +
#     xlim(0,5) + ylim(0,5) +
#     scale_fill_viridis_c()
#
#   cowplot::plot_grid(p1_post, p2_bin, p3_pois, p4_pcr, p5_sero, p6_comb)
#   # return(list(p1_post, p2_bin, p3_pois, p4_pcr, p5_sero, p6_comb))
# }

Heatmaps <- Plot_Heatmaps(Mod_Res = Mod_Res, Res_Figs = Res_Figs)

pdf("analysis/figures/38_Model_Fit_Res_09_pois_bin_bin_full_heatmaps.pdf", height = 12, width = 18)
Heatmaps
dev.off()






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
