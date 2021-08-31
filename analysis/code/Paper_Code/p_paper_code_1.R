#### Paper Plan ###
# library(squire)
devtools::load_all("../../Squire/squire/squire.Rproj")
devtools::load_all(".")

#####################################################################
### 1. Fit model to official data and lancet values: Estimate rf. ###
#####################################################################
data <- readRDS(file = "analysis/data/Code-generated-data/00_01_Lusaka_Prov_Deaths.rds") # See 00 data code
pop_st_lu <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds") # See 00 data code
prob_death_tot_IFR_frac <- readRDS("analysis/data/Code-generated-data/00_03_Tot_Prob_Death_By_Age_Zam.rds") # See 00 data code
Weighted_Durs_Hosp <- readRDS("analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds") # See 00 data code

fit_1 <- fit_spline_rt(data = data,
                       country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                       population = pop_st_lu,
                       reporting_fraction = 1,
                       reporting_fraction_bounds = c(0.25,0.1,1),
                       n_mcmc = 1000,
                       replicates = 100,
                       rw_duration = 14,
                       hosp_beds = 1e10,
                       icu_beds = 1e10,
                       prob_severe = rep(0,17),
                       prob_non_severe_death_treatment = prob_death_tot_IFR_frac,
                       dur_get_ox_survive =Weighted_Durs_Hosp$Surv_Dur_Weighted,
                       dur_get_ox_die =Weighted_Durs_Hosp$Death_Dur_Weighted,
                       dur_R = Inf,
                       sero_df_start = as.Date(c("2020-07-04","2020-07-18")),
                       sero_df_end = as.Date(c("2020-07-27","2020-08-10")),
                       sero_df_pos = as.numeric(as.integer(c(0.021*2704, 0.106*1952))), # See SFig.3
                       sero_df_samples = c(2704,1952),
                       pcr_df_start = as.Date(c("2020-07-04")),
                       pcr_df_end = as.Date(c("2020-07-27")),
                       pcr_df_pos = as.integer(c(0.076*2990)), # See SFig.3
                       pcr_df_samples = c(2990)
)

par(mfrow = c(3,3))
  plot(fit_1$pmcmc_results$chains[[1]]$results$rf, ylab = "rf - chain 1")
  plot(fit_1$pmcmc_results$chains[[2]]$results$rf, ylab = "rf - chain 2")
  plot(fit_1$pmcmc_results$chains[[3]]$results$rf, ylab = "rf - chain 3")
  plot(fit_1$pmcmc_results$chains[[1]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 1")
  plot(fit_1$pmcmc_results$chains[[2]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 2")
  plot(fit_1$pmcmc_results$chains[[3]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 3")
  plot(fit_1$pmcmc_results$chains[[1]]$results$rf,fit_1$pmcmc_results$chains[[1]]$results$log_likelihood, ylim = c(-400,-250), ylab = "ll - chain 1", xlab = "rf - chain 1")
  plot(fit_1$pmcmc_results$chains[[2]]$results$rf,fit_1$pmcmc_results$chains[[2]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 2", xlab = "rf - chain 2")
  plot(fit_1$pmcmc_results$chains[[3]]$results$rf,fit_1$pmcmc_results$chains[[3]]$results$log_likelihood, ylim = c(-400,-200), ylab = "ll - chain 3", xlab = "rf - chain 3")
  plot(fit_1$pmcmc_results$chains[[1]]$results$rf,fit_1$pmcmc_results$chains[[1]]$results$log_posterior, ylim = c(-500,-400), ylab = "lpost - chain 1", xlab = "rf - chain 1")

  # plot(fit_1$pmcmc_results$chains[[1]]$results$Rt_rw_16)

  hist(fit_1$pmcmc_results$chains[[2]]$results$log_likelihood)

# sort(rownames(fit_1$replicate_parameters))
# mean(fit_1$pmcmc_results$chains[[1]]$results$rf)
# mean(fit_1$pmcmc_results$chains[[2]]$results$rf)
# mean(fit_1$pmcmc_results$chains[[3]]$results$rf)

  hist(fit_1$replicate_parameters$rf)

# saveRDS(fit_1, file = "analysis/results/weird_fit.rds")
# saveRDS(fit_1, file = "analysis/results/weird_fit_2.rds")
saveRDS(fit_1, file = "analysis/results/p_01_Official_Data_Lancet_est_rf_resonable.rds")

Sero_Pcr_df_1 <- seroprev_df(fit_1)
PlotData_1 <- Summ_sero_pcr_data(Sero_Pcr_df_1)

p1 <-
  plot(fit_1, particle_fit = TRUE) +
  geom_bar(data = data, stat = "identity", aes(x=date, y=deaths), alpha = 0.6) +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit$pmcmc_results$inputs$data$deaths)/mean(fit_1$replicate_parameters$rf), yend = max(fit$pmcmc_results$inputs$data$deaths)/mean(fit_1$replicate_parameters$rf), alpha = 0.5, size = 1.5) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit$pmcmc_results$inputs$data$deaths)/mean(fit_1$replicate_parameters$rf), hjust = 0, label = "Gov. reported deaths") +
  annotate(geom = "point", x= as.Date("2020-03-02"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.95/mean(fit_1$replicate_parameters$rf)) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.95/mean(fit_1$replicate_parameters$rf), hjust=0, label = "Gov. reported deaths (scaled)") +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_1$replicate_parameters$rf), yend = max(fit$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_1$replicate_parameters$rf), col = "red", alpha = 0.5, size = 1.5) +
  annotate(geom = "rect", xmin=as.Date("2020-02-28"), xmax = as.Date("2020-03-05"), ymin = max(fit$pmcmc_results$inputs$data$deaths)*0.89/mean(fit_1$replicate_parameters$rf), ymax = max(fit$pmcmc_results$inputs$data$deaths)*0.91/mean(fit_1$replicate_parameters$rf), fill = NA, color = "black", linetype = 2) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_1$replicate_parameters$rf), hjust = 0, label = "Model fit") +
  theme_bw() + theme(legend.position = "none")

p2 <- ggplot(PlotData_1, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  annotate("text", x=as.Date("2020-10-01"), y=max(tail(PlotData_1$max_pcr))*2, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=7.6)) +
  geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=7.6+0.5, label="Mulenga et al.", hjust=0) +

  ylab(paste0("PCR+ % with ",100*round(mean(fit_1$replicate_parameters$rf),3),"% death reporting")) +
  xlab("Date") +
  theme_bw()

p3 <- ggplot(PlotData_1, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  annotate("text", x=as.Date("2020-10-01"), y=max(tail(PlotData_1$max_sero))*1.12, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=2.1)) +
  geom_errorbar(aes(ymin=1.1,ymax=3.1,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=2.1+0.5, label="Mulenga et al.", hjust =0) +

  geom_point(aes(x= as.Date("2020-07-29"),y=10.6), color="darkblue") +
  geom_errorbar(aes(ymin=7.3,ymax=13.9,x=as.Date("2020-07-29"), width=10), color="darkblue") +
  geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=10.6, height=0), color="darkblue") +
  annotate("text", x=as.Date("2020-07-29")+15, y=10.6, label="Mulenga et al. \n (combined\n  measure)", color = "darkblue", hjust =0) +

  ylab(paste0("Sero+% with ",100*round(mean(fit_1$replicate_parameters$rf),3),"% death reporting")) +
  xlab("Date") +
  theme_bw()

pdf(file = "analysis/figures/p_1_Official_Reported_Deaths_Lusaka_Fit_est_rf_Mulenga_Data.pdf", height = 4, width = 13)
cowplot::plot_grid(p1,p2,p3,nrow = 1)
dev.off()
# Q: Should I start the 0 deaths earlier than the beginning of April? Later than currently?
# Q: Should I remove the 0 points from the graph? Does that look untidy?


#######################################################################################
### 2. Vary IFR over model fitted to BMJ data and lancet paper, holdling rf at 100% ###
#######################################################################################
## Bring in BMJ data:
BMJ <- readRDS("analysis/data/Code-generated-data/00_05b_BMJ_Data_DailyEsts.rds")
# Scale up
fit <- readRDS(file = "analysis/results/p_01_Official_Data_Lancet_est_rf_resonable.rds")
p_pos <- seroprev_df(fit) %>% filter(date>="2020-06-08" & date<="2020-09-27") %>%
  group_by(date) %>% summarise(pcr_perc_av = mean(pcr_perc)) %>% # average over replicates
  # mutate(TimeFrame = cut.Date(as.Date(date), breaks = as.Date(c("2020-06-08","2020-06-22","2020-07-06","2020-07-20","2020-08-03","2020-08-17","2020-08-31","2020-09-14","2020-09-28")), labels = 1:8, start.on.monday = T)) %>%
  # group_by(TimeFrame) %>% summarise(PCR = mean(pcr_perc_av)) %>%
  select(pcr_perc_av)

p_bg <-readRDS(file = "analysis/data/Code-generated-data/00_06_Crude_Mortality.rds") # per fortnight

r_maf <- (BMJ$q1_10x/p_pos - p_bg)/(1-p_bg)
r_maf_b <- (BMJ$q1_HighEst/p_pos - p_bg)/(1-p_bg)

pop_lu_prov <- sum(readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds"))
BMJ <- BMJ %>% mutate(TrueCovDeathsLus_10xEst = unlist(pop_lu_prov*r_maf*p_pos),
                      TrueCovDeathsLus_HighEst = unlist(pop_lu_prov*r_maf_b*p_pos))

BMJ_data_HighEst <- BMJ %>% select(date, TrueCovDeathsLus_HighEst) %>%
  rename(deaths = TrueCovDeathsLus_HighEst) %>%
  mutate(deaths = round(deaths))

# Fit model to this data over a range of IFRs
IFR_vec <- c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2)

fit_l_IFR <- lapply(1:length(IFR_vec), function(x){

  prob_death_tot_IFR_frac <- ifelse(prob_death_tot*IFR_vec[x]>1,1,prob_death_tot*IFR_vec[x])

  fit_spline_rt(data = BMJ_data_HighEst,
                country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                population = pop_st_lu,
                reporting_fraction = 1,
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
                sero_df_start = as.Date(c("2020-07-04","2020-07-18")),
                sero_df_end = as.Date(c("2020-07-27","2020-08-10")),
                sero_df_pos = as.numeric(as.integer(c(0.021*2704, 0.106*1952))), # See SFig.3
                sero_df_samples = c(2704,1952),
                pcr_df_start = as.Date(c("2020-07-04")),
                pcr_df_end = as.Date(c("2020-07-27")),
                pcr_df_pos = as.integer(c(0.076*2990)), # See SFig.3
                pcr_df_samples = c(2990)
  )
})

## save these results
# saveRDS(fit_l_IFR, file = "../Bonus Files/Fit_varying_IFR.rds")

# estimate pcr/serology across data:
sero_pcr_df_l_IFR <- lapply(X = fit_l_IFR, FUN = function(x){
  seroprev_df(x)})

# Simplify data
Simple_Plot_Data_l_IFR <- lapply(sero_pcr_df_l_IFR, Summ_sero_pcr_data)

### Calculate Log-likelihood function
log_lik_sero_fun <- function(sero_fit){
  sero_pred_1<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-15"), "sero_perc"])
  ll_sero_15 <- log(dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_pred_1))

  sero_pred_2<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-29"), "sero_perc"])
  ll_sero_29 <- log(dbinom(x = as.integer(0.106*1952), size = 1952, prob = sero_pred_2))

  pcr_pred_1<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-15"), "pcr_perc"])
  ll_pcr_15 <- log(dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_pred_1))

  return(list("sero_15"=ll_sero_15, "sero_29"=ll_sero_29, "pcr_15"=ll_pcr_15))
}

loglik_data <- function(fit_res){
  # browser()
  Ds <- diff(rowMeans(apply(fit_res$output[as.Date(rownames(fit_res$output)) %in% fit_res$pmcmc_results$inputs$data$date, odin_index(fit_res$model)$D,], MARGIN = 3, FUN = rowSums)))
  ll_deaths <- sum(ll_nbinom(data = fit_res$pmcmc_results$inputs$data$deaths[-1],
                             model = Ds,
                             phi = fit_res$pmcmc_results$inputs$pars_obs$phi_death,
                             k = fit_res$pmcmc_results$inputs$pars_obs$k_death,
                             exp_noise = fit_res$pmcmc_results$inputs$pars_obs$exp_noise))
  return(ll_deaths)
}

loglik_data(fit_l_IFR[[1]])

apply(fit_l_IFR[[1]]$output[as.Date(rownames(fit_l_IFR[[1]]$output)) %in% fit_l_IFR[[1]]$pmcmc_results$inputs$data$date, odin_index(fit_l_IFR[[1]]$model)$D,], MARGIN = 3, FUN = rowSums)

# Plot graphs
part_fit_plot <- function(fit){plot(fit, particle_fit = T) +
    annotate(geom = "point", x= as.Date("2020-05-03"), y = 48) +
    annotate(geom = "text", x=as.Date("2020-05-07"), y = 48, hjust=0, label = "BMJ data estimated deaths") +
    annotate(geom = "segment", x=as.Date("2020-05-01"), xend = as.Date("2020-05-05"), y = 45, yend = 45, col = "red", alpha = 0.7) +
    annotate(geom = "rect", xmin=as.Date("2020-05-01"), xmax = as.Date("2020-05-05"), ymin = 44.8, ymax = 45.2, fill = NA, color = "black", linetype = 2) +
    annotate(geom = "text", x=as.Date("2020-05-07"), y = 45, hjust = 0, label = "Model fit") +
    theme(legend.position = "none")
    }
pcr_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-15"),y=7.6)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=7.6+0.5, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=2, label="Model fit", alpha=0.8) +
    geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0)) +
    ylab(paste0("PCR % with ",100,"% death reporting")) +
    theme_bw()}
sero_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-15"),y=2.1)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=2.1+0.5, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=max(Summ_sero_fit$max_sero), label="Model fit", alpha=0.8, hjust=0, vjust = 2) +
    geom_errorbar(aes(ymin=1.1,ymax=3.1,x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0)) +
    geom_point(aes(x= as.Date("2020-07-29"),y=10.6), color="darkblue") +
    annotate("text", x=as.Date("2020-07-29")+15, y=10.6, label="Mulenga et al. \n (extrapolated \n from PCR)", color = "darkblue", hjust =0) +
    # geom_text(aes(label = "Mulenga et al. \n (extrapolated \n from PCR)", x= as.Date("2020-07-29")+15,y=0.106), color = "darkblue", hjust= 0) +
    geom_errorbar(aes(ymin=7.3,ymax=13.9,x=as.Date("2020-07-29"), width=10), color="darkblue") +
    geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=10.6, height=0), color="darkblue") +
    ylab(paste0("Sero+% with ",100,"% death reporting")) +
    annotate("text", x=as.Date("2020-05-01"), y=1.8, label=paste(""), color = "darkblue", hjust =0) +
    theme_bw()}


PlotsList <- lapply(seq(9), function(x){
  p1 <- part_fit_plot(fit_l_IFR[[x]])+ annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste("IFR x",IFR_vec[x]), vjust = 2) + annotate("text",x = as.Date("2020-05-01"),y = Inf, label=paste("LL\nDeath data:",round(loglik_data(fit_l_IFR[[x]]),2)), hjust = 0, vjust = 4)
  p2 <- pcr_fit_plot(Simple_Plot_Data_l_IFR[[x]]) + annotate("text",x = as.Date("2020-05-01"),y = Inf, label=paste("LL\nPCR 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$pcr_15,2)), hjust = 0, vjust = 1.5)
  p3 <- sero_fit_plot(Simple_Plot_Data_l_IFR[[x]]) + annotate("text",x = as.Date("2020-05-01"),y = Inf, label=paste("LL\nSero 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$sero_15,2),"\nSero 29th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$sero_29,2)), hjust = 0, vjust = 1.5)
  return(list(p1,p2,p3))
})

# Plot data
pdf(file = "analysis/figures/p_2_BMJ_Lancet_var_IFR.pdf", height = 15, width = 40)
cowplot::plot_grid(plotlist = unlist(PlotsList, recursive = F), nrow = 3,byrow = F)
dev.off()

### Plot LL varying IFR:
## Manipulate data for plotting
# Melt and plot
library(reshape2)
sero_pcr_df_l_IFR <- lapply(seq(length(sero_pcr_df_l_IFR)), function(x){sero_pcr_df_l_IFR[[x]] %>% mutate(IFR_val = IFR_vec[[x]])})
sero_pcr_df_l_IFR_com <- data.table::rbindlist(sero_pcr_df_l_IFR)

# generate vector of ll for death data
Death_LLs <- unlist(lapply(seq(9), function(x){
  Ds <- apply(X = apply(fit_l_IFR[[x]]$output[as.Date(rownames(fit_l_IFR[[x]]$output)) %in% fit_l_IFR[[x]]$pmcmc_results$inputs$data$date, odin_index(fit_l_IFR[[x]]$model)$D,], MARGIN = 3, FUN = rowSums),
        MARGIN = 2, FUN = diff)
  apply(Ds, MARGIN = 2, FUN = function(y){
    sum(ll_nbinom(data = fit_l_IFR[[x]]$pmcmc_results$inputs$data$deaths[-1],
                  model = y,
                  phi = fit_l_IFR[[x]]$pmcmc_results$inputs$pars_obs$phi_death,
                  k = fit_l_IFR[[x]]$pmcmc_results$inputs$pars_obs$k_death,
                  exp_noise = fit_l_IFR[[x]]$pmcmc_results$inputs$pars_obs$exp_noise))
  })
  }))


dd_l_IFR <- sero_pcr_df_l_IFR_com %>% filter(date %in% as.Date(c("2020-07-15","2020-07-29"))) %>%
  select(replicate, date, sero_perc, pcr_perc, IFR_val) %>%
  group_by(replicate,IFR_val) %>%
  mutate(sero_perc_2 = sero_perc[date==as.Date("2020-07-29")]) %>% ungroup() %>%
  filter(date==as.Date("2020-07-15")) %>% select(-date) %>%
  mutate(ll_sero_1 = log(dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_perc)),
         ll_sero_2 = log(dbinom(x = as.integer(0.106*1952), size = 1952, prob = sero_perc_2)),
         ll_pcr = log(dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_perc)),
         ll_deaths = Death_LLs) %>%
  mutate(ll_total = ll_sero_1 + ll_sero_2 + ll_pcr + ll_deaths) %>%
  select(-sero_perc,-sero_perc_2,-pcr_perc) %>%
  melt(data = ., id=c("IFR_val", "replicate"),value.name = "ll")


pdf(file = "analysis/figures/p_2b_BMJ_Lancet_var_IFR.pdf", height = 4, width = 10)
ggplot(filter(dd_l_IFR, IFR_val >0), aes(x=ll, y=as.factor(IFR_val), group=IFR_val)) +
  geom_boxplot(aes(fill=IFR_val)) +
  facet_wrap(. ~ variable, scales = "free_x", nrow=1, labeller = as_labeller(c(`ll_sero_1` = "Sero+ % (15-07-2020)", `ll_sero_2` = "Sero+ % (29-07-2020)", `ll_pcr` = "PCR+ % (15-07-2020)", `ll_deaths` = "BMJ Death data", `ll_total` = "Total"))) +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank(), panel.grid.minor = element_blank()) +
  xlab("log likelihood") + ylab("x IFR")
dev.off()


#######################################################################################
### 3. Vary IFR over model fitted to BMJ data and lancet paper, holdling rf at 100% ###
#######################################################################################
## Plot the proportion of total deaths in BMJ data that occur in each age group

BMJ_Deaths_by_Age_Covid_Neg <- c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2)
BMJ_Deaths_by_Age_Covid_Pos <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Tot <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2)

barplot(BMJ_Deaths_by_Age_Covid_Pos/BMJ_Deaths_by_Age_Tot)

## A line or bar plot with 2 lines/bar colors,
# 1 being the IFR from Brazeau et al
# and the other being the IFR we estimate to fit the profile of deaths by age.
# Something similarish to the following plot (S1 in https://science.sciencemag.org/content/371/6526/288)


# Brazeau report 34 IFR
Braz_IFR <- rbind(c(0.00, 0.00, 0.03),
      c(0.01, 0.00, 0.06),
      c(0.01, 0.00, 0.11),
      c(0.02, 0.00, 0.18),
      c(0.03, 0.00, 0.3),
      c(0.04, 0.00, 0.46),
      c(0.06, 0.01, 0.71),
      c(0.10, 0.01, 1.03),
      c(0.16, 0.02, 1.47),
      c(0.24, 0.03, 2.03),
      c(0.38, 0.05, 2.74),
      c(0.60, 0.10, 3.64),
      c(0.94, 0.18, 4.79),
      c(1.47, 0.35, 6.27),
      c(2.31, 0.65, 8.21),
      c(3.61, 1.21, 10.81),
      c(5.66, 2.23, 14.37),
      c(8.86, 4.06, 19.36),
      c(17.37, 9.7, 31.12))

## Fit log linear model through the Brazeau data.
plot(y = log(Braz_IFR[,1]), seq(2.5, 95, by = 5))
IFR_Coefs <- lm(log(Braz_IFR[-1,1]) ~ seq(7.5, 95, by = 5))$coefficients


## So what is the IFR based on the deaths that occurred?
# So I need the infection number in each age group and the death number in each age group.
DataFit <- readRDS(file = "analysis/results/p_01_Official_Data_Lancet_est_rf_resonable.rds")
Sero_Pcr_df_1 <- seroprev_df(DataFit)
PlotData_1 <- Summ_sero_pcr_data(Sero_Pcr_df_1)

PlotData_1

barplot(rbind(BMJ_Deaths_by_Age_Covid_Pos/BMJ_Deaths_by_Age_Tot, Braz_IFR[,1]), beside = T)
