#### Paper Plan ###
# devtools::load_all("../../Squire/squire/squire.Rproj")
rm(list = ls())
devtools::load_all(".")
library(squire)
library(reshape2)
library(tidyverse)
#####################################################################
### 1. Fit model to official data and lancet values: Estimate rf. ###
#####################################################################
data <- readRDS(file = "analysis/data/Code-generated-data/00_01_Lusaka_Prov_Deaths.rds") # See 00 data code
pop_st_lu <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds") # See 00 data code
prob_death_tot_IFR_frac <- readRDS("analysis/data/Code-generated-data/00_03_Tot_Prob_Death_By_Age_Zam.rds") # See 00 data code
Weighted_Durs_Hosp <- readRDS("analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds") # See 00 data code
Lancet_Data <- readRDS("analysis/data/Code-generated-data/00_07_Lancet_Data.rds") # See 00 data code


fit_1 <- fit_spline_rt(data = data,
                       country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                       population = pop_st_lu,
                       reporting_fraction = 1,
                       reporting_fraction_bounds = c(0.25,0.1,1),
                       n_mcmc = 10000,
                       replicates = 100,
                       rw_duration = 14,
                       hosp_beds = 1e10,
                       icu_beds = 1e10,
                       prob_severe = rep(0,17),
                       prob_non_severe_death_treatment = prob_death_tot_IFR_frac,
                       dur_get_ox_survive =Weighted_Durs_Hosp$Surv_Dur_Weighted,
                       dur_get_ox_die =Weighted_Durs_Hosp$Death_Dur_Weighted,
                       dur_R = Inf,
                       sero_df_start = as.Date(c("2020-07-04")),#,"2020-07-18")),
                       sero_df_end = as.Date(c("2020-07-27")),#,"2020-08-10")),
                       sero_df_pos = as.numeric(as.integer(c(Lancet_Data$Sero_prev["val"]/100*Lancet_Data$Sero_prev["n"]))),#, 0.106*1952))), # See Table 2
                       sero_df_samples = Lancet_Data$Sero_prev["n"],#,1952),
                       pcr_df_start = as.Date(c("2020-07-04")),
                       pcr_df_end = as.Date(c("2020-07-27")),
                       pcr_df_pos = as.integer(c(Lancet_Data$PCR_prev["val"]/100*Lancet_Data$PCR_prev["n"])), # See Table 2
                       pcr_df_samples = Lancet_Data$PCR_prev["n"]
)

# par(mfrow = c(3,3))
#   plot(fit_1$pmcmc_results$chains[[1]]$results$rf, ylab = "rf - chain 1")
#   plot(fit_1$pmcmc_results$chains[[2]]$results$rf, ylab = "rf - chain 2")
#   plot(fit_1$pmcmc_results$chains[[3]]$results$rf, ylab = "rf - chain 3")
#   plot(fit_1$pmcmc_results$chains[[1]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 1")
#   plot(fit_1$pmcmc_results$chains[[2]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 2")
#   plot(fit_1$pmcmc_results$chains[[3]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 3")
#   plot(fit_1$pmcmc_results$chains[[1]]$results$rf,fit_1$pmcmc_results$chains[[1]]$results$log_likelihood, ylim = c(-400,-250), ylab = "ll - chain 1", xlab = "rf - chain 1")
#   plot(fit_1$pmcmc_results$chains[[2]]$results$rf,fit_1$pmcmc_results$chains[[2]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 2", xlab = "rf - chain 2")
#   plot(fit_1$pmcmc_results$chains[[3]]$results$rf,fit_1$pmcmc_results$chains[[3]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 3", xlab = "rf - chain 3")

# par(mfrow = c(1,1))

  # plot(fit_1$pmcmc_results$chains[[1]]$results$Rt_rw_16)

  # hist(fit_1$pmcmc_results$chains[[2]]$results$log_likelihood)

# sort(rownames(fit_1$replicate_parameters))
# mean(fit_1$pmcmc_results$chains[[1]]$results$rf)
# mean(fit_1$pmcmc_results$chains[[2]]$results$rf)
# mean(fit_1$pmcmc_results$chains[[3]]$results$rf)

  # hist(fit_1$replicate_parameters$rf)

# saveRDS(fit_1, file = "../Bonus Files/10_2021-10-21_fit.rds")
fit_1 <- readRDS(file = "../Bonus Files/p_01_Official_Data_Lancet_est_rf_resonable.rds")

Sero_Pcr_df_1 <- seroprev_df(fit_1)
PlotData_1 <- Summ_sero_pcr_data(Sero_Pcr_df_1)

p1 <-
  plot(fit_1, particle_fit = TRUE) +
  geom_bar(data = data, stat = "identity", aes(x=date, y=deaths), alpha = 0.6) +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit_1$pmcmc_results$inputs$data$deaths)/mean(fit_1$replicate_parameters$rf), yend = max(fit_1$pmcmc_results$inputs$data$deaths)/mean(fit_1$replicate_parameters$rf), alpha = 0.5, size = 1.5) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit_1$pmcmc_results$inputs$data$deaths)/mean(fit_1$replicate_parameters$rf), hjust = 0, label = "Gov. reported deaths") +
  annotate(geom = "point", x= as.Date("2020-03-02"), y = max(fit_1$pmcmc_results$inputs$data$deaths)*0.95/mean(fit_1$replicate_parameters$rf)) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit_1$pmcmc_results$inputs$data$deaths)*0.95/mean(fit_1$replicate_parameters$rf), hjust=0, label = "Gov. reported deaths (scaled)") +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit_1$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_1$replicate_parameters$rf), yend = max(fit_1$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_1$replicate_parameters$rf), col = "red", alpha = 0.5, size = 1.5) +
  annotate(geom = "rect", xmin=as.Date("2020-02-28"), xmax = as.Date("2020-03-05"), ymin = max(fit_1$pmcmc_results$inputs$data$deaths)*0.89/mean(fit_1$replicate_parameters$rf), ymax = max(fit_1$pmcmc_results$inputs$data$deaths)*0.91/mean(fit_1$replicate_parameters$rf), fill = NA, color = "black", linetype = 2) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit_1$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_1$replicate_parameters$rf), hjust = 0, label = "Model fit") +
  theme_bw() + theme(legend.position = "none")

p2 <- ggplot(PlotData_1, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  annotate("text", x=as.Date("2020-10-01"), y=max(tail(PlotData_1$max_pcr))*2, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$PCR_prev["val"])) +
  geom_errorbar(aes(ymin=Lancet_Data$PCR_prev["ci95l"],ymax=Lancet_Data$PCR_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$PCR_prev["val"], height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$PCR_prev["val"]+0.5, label="Mulenga et al.", hjust=0) +

  ylab(paste0("PCR+ % with ",100*round(mean(fit_1$replicate_parameters$rf),3),"% death reporting")) +
  xlab("Date") +
  theme_bw()

p3 <- ggplot(PlotData_1, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  annotate("text", x=as.Date("2020-10-01"), y=max(tail(PlotData_1$max_sero))*1.12, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$Sero_prev["val"])) +
  geom_errorbar(aes(ymin=Lancet_Data$Sero_prev["ci95l"],ymax=Lancet_Data$Sero_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$Sero_prev["val"], height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$Sero_prev["val"]+0.5, label="Mulenga et al.", hjust =0) +

  geom_point(aes(x= as.Date("2020-07-29"),y=Lancet_Data$Tot_cov_prev["val"]), color="darkblue") +
  geom_errorbar(aes(ymin=Lancet_Data$Tot_cov_prev["ci95l"],ymax=Lancet_Data$Tot_cov_prev["ci95h"],x=as.Date("2020-07-29"), width=10), color="darkblue") +
  geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=Lancet_Data$Tot_cov_prev["val"], height=0), color="darkblue") +
  annotate("text", x=as.Date("2020-07-29")+15, y=Lancet_Data$Tot_cov_prev["val"], label="Mulenga et al. \n (combined\n  measure)", color = "darkblue", hjust =0) +

  ylab(paste0("Sero+% with ",100*round(mean(fit_1$replicate_parameters$rf),3),"% death reporting")) +
  xlab("Date") +
  theme_bw()

pdf(file = "analysis/figures/10_1_Official_Reported_Deaths_Lusaka_Fit_est_rf_Mulenga_Data.pdf", height = 12, width = 4)
cowplot::plot_grid(p1,p2,p3,nrow = 3)
dev.off()
# Q: Should I start the 0 deaths earlier than the beginning of April? Later than currently?
# Q: Should I remove the 0 points from the graph? Does that look untidy?


