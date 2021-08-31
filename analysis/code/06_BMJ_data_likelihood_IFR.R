### 2021-07-23
devtools::load_all(".")
library(tidyverse)
library(squire)

################################################################
# Step 1. What is the likelihood of the BMJ inferred death data?
################################################################

### - To do this use the death time series from the BMJ data (observed deaths multiplied by sampling effort and accounting for hospital being only 80% of all deaths)

# Load BMJ mortality estimated data - see 05_Compare_Off - w_bmj_deaths
data <- readRDS("analysis/data/Code-generated-data/BMJ_estimated_mortality_sampling.rds")

df_fitting <- data %>% select(Date, Est_Deaths_BMJ_Sam_HE) %>% rename(deaths = Est_Deaths_BMJ_Sam_HE, date = Date) %>%
  mutate(deaths = round(deaths))

### - Fit to this under 100% reporting and our current IFR pattern to work out likelihood of the sero/PCR data
# pop_st_lu <- readRDS("analysis/data/Lusaka_pop_2020_Open_Africa.rds") # See 02_zambia_lusaka_initial
# fit <- fit_spline_rt(data = df_fitting,
#                      country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
#                      population = pop_st_lu,
#                      reporting_fraction = 1,
#                      # reporting_fraction_bounds=c(0.25,0.1,1),
#                      n_mcmc = 10000,
#                      replicates = 100,
#                      rw_duration = 14,
#                      hosp_beds = 1e10,
#                      icu_beds = 1e10)

# Get Sero/PCR breakdown
# sero_fit <- seroprev_df(fit)

### Calculate Log-likelihood
log_lik_sero_fun <- function(sero_fit){
  sero_pred_1<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-15"), "sero_perc"])
  ll_sero_15 <- log(dbinom(x = as.integer(0.021*4258), size = 4258, prob = sero_pred_1))

  sero_pred_2<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-29"), "sero_perc"])
  ll_sero_29 <- log(dbinom(x = as.integer(0.106*4258), size = 4258, prob = sero_pred_2))
  return(list("sero_15"=ll_sero_15, "sero_29"=ll_sero_29))
}

# log_lik_sero_fun(sero_fit)

# Format sero data for plotting
# Summ_sero_fit <- Summ_sero_pcr_data(sero_fit)

# Plot graphs
part_fit_plot <- function(fit){plot(fit, particle_fit = T) +
    annotate(geom = "point", x= as.Date("2020-05-03"), y = 48) +
    annotate(geom = "text", x=as.Date("2020-05-07"), y = 48, hjust=0, label = "BMJ data estimated deaths") +
    annotate(geom = "segment", x=as.Date("2020-05-01"), xend = as.Date("2020-05-05"), y = 45, yend = 45, col = "red", alpha = 0.7) +
    annotate(geom = "rect", xmin=as.Date("2020-05-01"), xmax = as.Date("2020-05-05"), ymin = 44.8, ymax = 45.2, fill = NA, color = "black", linetype = 2) +
    annotate(geom = "text", x=as.Date("2020-05-07"), y = 45, hjust = 0, label = "Model fit") +
    theme(legend.position = "none")}
pcr_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-15"),y=0.076)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=0.076+0.005, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=0.02, label="Model fit", alpha=0.8) +
    geom_errorbar(aes(ymin=0.047,ymax=0.106,x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.076, height=0)) +
    ylab(paste0("PCR % with ",100,"% death reporting")) +
    theme_bw()}
sero_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-15"),y=0.021)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=0.021+0.005, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=0.18, label="Model fit", alpha=0.8, hjust=0) +
    geom_errorbar(aes(ymin=0.011,ymax=0.031,x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.021, height=0)) +
    geom_point(aes(x= as.Date("2020-07-29"),y=0.106), color="darkblue") +
    annotate("text", x=as.Date("2020-07-29")+15, y=0.106, label="Mulenga et al. \n (extrapolated \n from PCR)", color = "darkblue", hjust =0) +
    # geom_text(aes(label = "Mulenga et al. \n (extrapolated \n from PCR)", x= as.Date("2020-07-29")+15,y=0.106), color = "darkblue", hjust= 0) +
    geom_errorbar(aes(ymin=0.073,ymax=0.139,x=as.Date("2020-07-29"), width=10), color="darkblue") +
    geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=0.106, height=0), color="darkblue") +
    ylab(paste0("Sero+% with ",100,"% death reporting")) +
    annotate("text", x=as.Date("2020-05-01"), y=0.18, label=paste(""), color = "darkblue", hjust =0) +
    theme_bw()}

# pdf(file = "analysis/figures/06_01_PCR_Sero_BMJ_RevEng_Deaths_100pc_Reporting.pdf", height = 5)
# part_fit_plot(fit)
# pcr_fit_plot(Summ_sero_fit)
# sero_fit_plot(Summ_sero_fit)
# dev.off()


### - To put in our current IFR pattern:
# 1. prob_severe to 0
# 2. set prob_non_severe_death_treatment such that IFR = prob_non_severe_death_treatment * prob_hosp by age

prob_death_tot <- parameters_explicit_SEEIR("Zambia")$prob_severe * parameters_explicit_SEEIR("Zambia")$prob_severe_death_treatment +
  (1-parameters_explicit_SEEIR("Zambia")$prob_severe) * parameters_explicit_SEEIR("Zambia")$prob_non_severe_death_treatment
saveRDS(prob_death_tot, "analysis/data/Tot_Prob_Age_Death_Zam.rds")

# names(parameters_explicit_SEEIR("Zambia"))
# parameters_explicit_SEEIR("Zambia")$dur_get_ox_survive
# parameters_explicit_SEEIR("Zambia")$dur_get_ox_die
# Alternatively:
# (parameters_explicit_SEEIR("Zambia")$prob_hosp * (parameters_explicit_SEEIR("Zambia")$prob_severe * parameters_explicit_SEEIR("Zambia")$prob_severe_death_treatment +
#   (1-parameters_explicit_SEEIR("Zambia")$prob_severe) * parameters_explicit_SEEIR("Zambia")$prob_non_severe_death_treatment)) / parameters_explicit_SEEIR("Zambia")$prob_hosp

# fit_2 <- fit_spline_rt(data = df_fitting,
#                      country = "Zambia",
#                      population = pop_st_lu,
#                      reporting_fraction = 1,
#                      n_mcmc = 10000,
#                      replicates = 100,
#                      rw_duration = 14,
#                      hosp_beds = 1e10,
#                      icu_beds = 1e10,
#                      prob_severe = rep(0,17),
#                      prob_non_severe_death_treatment = prob_death_tot,
#                      dur_get_ox_survive =12,
#                      dur_get_ox_die =10,
#                      dur_R = Inf
#                      )

# fit_to_save <- fit_2
# fit_to_save$output <- NULL
# saveRDS(fit_to_save, "../fit_0_pr_sev.rds")
# r<- readRDS("../fit_0_pr_sev.rds")
# sero_fit_2 <- seroprev_df(fit_2)
# Summ_sero_fit_2 <- Summ_sero_pcr_data(sero_fit_2)



# Check it's working:
# plot(fit_2, var_select = c("IMVGetDie","IMVGetLive","IMVNotGetDie","IMVNotGetLive")) # there are none
# plot(fit, var_select = c("IMVGetDie","IMVGetLive","IMVNotGetDie","IMVNotGetLive")) # there are none
#
# plot(fit_2, var_select = c("IOxGetDie"))
# plot(fit_2, var_select = c("IOxGetLive"))
# plot(fit_2, var_select = c("IOxNotGetDie"))
# plot(fit_2, var_select = c("IOxNotGetLive"))
#
# plot(fit, var_select = c("infections"))
# plot(fit_2, var_select = c("infections"))
# plot(fit, var_select = c("hospital_occupancy"))
# plot(fit, var_select = c("ICU_occupancy"))
# plot(fit_2, var_select = c("ICU_occupancy"))


### - From the fit, generate a curve of seroprevalence and PCR prevalence over time and the numerical likelihood.
# pdf(file = "analysis/figures/06_02_PCR_Sero_BMJ_RevEng_Deaths_100pc_Reporting_0_Severe.pdf", height = 15, width = 12)
# cowplot::plot_grid(part_fit_plot(fit)+ annotate("text",x = as.Date("2020-09-15"),y = 48, label="Full Model"),
#                    part_fit_plot(fit_2)+ annotate("text",x = as.Date("2020-08-01"),y = 48, label="p.sev = 0,\np.n.sev.d.tr = IFR*p.hos", hjust = 0),
#                    pcr_fit_plot(Summ_sero_fit),pcr_fit_plot(Summ_sero_fit_2),
#                    sero_fit_plot(Summ_sero_fit)+ annotate("text",x = as.Date("2020-05-01"),y = 0.17, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_fit)$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_fit)$sero_29,2)), hjust = 0),
#                    sero_fit_plot(Summ_sero_fit_2)+ annotate("text",x = as.Date("2020-05-01"),y = 0.18, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_fit_2)$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_fit_2)$sero_29,2)), hjust = 0), nrow = 3)
# dev.off()


########################################################################################
# Step 2. In what direction does IFR need to change to better explain the serology data?
########################################################################################

# - Fit the death data with various IFRs to show how likelihood changes, e.g. explore an IFR that is 10x, 5x, 4x, 3x, 2x, 1x, 0.8x, 0.6x, 0.4x, 0.2x, 0.1x
IFR_vec <- c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2)


fit_l_IFR <- lapply(1:length(IFR_vec), function(x){

  prob_death_tot_IFR_frac <- ifelse(prob_death_tot*IFR_vec[x]>1,1,prob_death_tot*IFR_vec[x])

  fit_spline_rt(data = df_fitting,
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
                dur_get_ox_survive =12,
                dur_get_ox_die =10,
                dur_R = Inf
  )
})

# saveRDS(fit_l_IFR, file = "../fit_varying_IFR_2.rds")

sero_pcr_df_l_IFR <- lapply(X = fit_l_IFR, FUN = function(x){
  seroprev_df(x)})

Simple_Plot_Data_l_IFR <- lapply(sero_pcr_df_l_IFR, Summ_sero_pcr_data)

pdf(file = "analysis/figures/06_03_PCR_Sero_BMJ_RevEng_Deaths_100pc_Reporting_0_Severe_var_IFR_2.pdf", height = 15, width = 40)
cowplot::plot_grid(part_fit_plot(fit_l_IFR[[1]])+ annotate("text",x = as.Date("2020-09-15"),y = 48, label=paste("IFR x",IFR_vec[1])),
                   part_fit_plot(fit_l_IFR[[2]])+ annotate("text",x = as.Date("2020-09-15"),y = 48, label=paste("IFR x",IFR_vec[2])),
                   part_fit_plot(fit_l_IFR[[3]])+ annotate("text",x = as.Date("2020-09-15"),y = 48, label=paste("IFR x",IFR_vec[3])),
                   part_fit_plot(fit_l_IFR[[4]])+ annotate("text",x = as.Date("2020-09-15"),y = 48, label=paste("IFR x",IFR_vec[4])),
                   part_fit_plot(fit_l_IFR[[5]])+ annotate("text",x = as.Date("2020-09-15"),y = 48, label=paste("IFR x",IFR_vec[5])),
                   part_fit_plot(fit_l_IFR[[6]])+ annotate("text",x = as.Date("2020-09-15"),y = 48, label=paste("IFR x",IFR_vec[6])),
                   part_fit_plot(fit_l_IFR[[7]])+ annotate("text",x = as.Date("2020-09-15"),y = 48, label=paste("IFR x",IFR_vec[7])),
                   part_fit_plot(fit_l_IFR[[8]])+ annotate("text",x = as.Date("2020-09-15"),y = 48, label=paste("IFR x",IFR_vec[8])),
                   part_fit_plot(fit_l_IFR[[9]])+ annotate("text",x = as.Date("2020-09-15"),y = 48, label=paste("IFR x",IFR_vec[9])),
                   pcr_fit_plot(Simple_Plot_Data_l_IFR[[1]]), pcr_fit_plot(Simple_Plot_Data_l_IFR[[2]]), pcr_fit_plot(Simple_Plot_Data_l_IFR[[3]]),pcr_fit_plot(Simple_Plot_Data_l_IFR[[4]]),pcr_fit_plot(Simple_Plot_Data_l_IFR[[5]]),pcr_fit_plot(Simple_Plot_Data_l_IFR[[6]]),pcr_fit_plot(Simple_Plot_Data_l_IFR[[7]]),pcr_fit_plot(Simple_Plot_Data_l_IFR[[8]]),pcr_fit_plot(Simple_Plot_Data_l_IFR[[9]]),
                   sero_fit_plot(Simple_Plot_Data_l_IFR[[1]]) + annotate("text",x = as.Date("2020-05-01"),y = 0.17, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[1]])$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[1]])$sero_29,2)), hjust = 0),
                   sero_fit_plot(Simple_Plot_Data_l_IFR[[2]]) + annotate("text",x = as.Date("2020-05-01"),y = 0.17, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[2]])$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[2]])$sero_29,2)), hjust = 0),
                   sero_fit_plot(Simple_Plot_Data_l_IFR[[3]])+ annotate("text",x = as.Date("2020-05-01"),y = 0.18, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[3]])$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[3]])$sero_29,2)), hjust = 0),
                   sero_fit_plot(Simple_Plot_Data_l_IFR[[4]])+ annotate("text",x = as.Date("2020-05-01"),y = 0.18, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[4]])$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[4]])$sero_29,2)), hjust = 0),
                   sero_fit_plot(Simple_Plot_Data_l_IFR[[5]])+ annotate("text",x = as.Date("2020-05-01"),y = 0.18, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[5]])$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[5]])$sero_29,2)), hjust = 0),
                   sero_fit_plot(Simple_Plot_Data_l_IFR[[6]])+ annotate("text",x = as.Date("2020-05-01"),y = 0.18, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[6]])$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[6]])$sero_29,2)), hjust = 0),
                   sero_fit_plot(Simple_Plot_Data_l_IFR[[7]])+ annotate("text",x = as.Date("2020-05-01"),y = 0.18, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[7]])$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[7]])$sero_29,2)), hjust = 0),
                   sero_fit_plot(Simple_Plot_Data_l_IFR[[8]])+ annotate("text",x = as.Date("2020-05-01"),y = 0.18, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[8]])$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[8]])$sero_29,2)), hjust = 0),
                   sero_fit_plot(Simple_Plot_Data_l_IFR[[9]])+ annotate("text",x = as.Date("2020-05-01"),y = 0.18, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[9]])$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[9]])$sero_29,2)), hjust = 0),
                   nrow = 3)
dev.off()


## Manipulate data for plotting
# Melt and plot
library(reshape2)
sero_pcr_df_l_IFR <- lapply(1:length(sero_pcr_df_l_IFR), function(x){sero_pcr_df_l_IFR[[x]] %>% mutate(IFR_val = IFR_vec[[x]])})
sero_pcr_df_l_IFR_com <- data.table::rbindlist(sero_pcr_df_l_IFR)

dd_l_IFR <- sero_pcr_df_l_IFR_com %>% filter(date %in% as.Date(c("2020-07-15","2020-07-29"))) %>%
  select(replicate, date, sero_perc, pcr_perc, IFR_val) %>%
  group_by(replicate,IFR_val) %>%
  mutate(sero_perc_2 = sero_perc[date==as.Date("2020-07-29")]) %>% ungroup() %>%
  filter(date==as.Date("2020-07-15")) %>% select(-date) %>%
  mutate(ll_sero_1 = log(dbinom(x = as.integer(0.021*4258), size = 4258, prob = sero_perc)),
         ll_sero_2 = log(dbinom(x = as.integer(0.106*4258), size = 4258, prob = sero_perc_2)),
         ll_pcr = log(dbinom(x = as.integer(0.076*4258), size = 4258, prob = pcr_perc))) %>%
  mutate(ll_total = ll_sero_1 + ll_sero_2 + ll_pcr) %>%
  select(-sero_perc,-sero_perc_2,-pcr_perc) %>%
  melt(data = ., id=c("IFR_val", "replicate"),value.name = "ll")


pdf(file = "analysis/figures/06_04_PCR_Sero_BMJ_RevEng_Deaths_100pc_Reporting_0_Severe_var_IFR_ll.pdf", height = 4, width = 8)
ggplot(filter(dd_l_IFR, IFR_val >0.6), aes(x=ll, y=as.factor(IFR_val), group=IFR_val)) +
  geom_boxplot(aes(fill=IFR_val)) +
  facet_wrap(. ~ variable, scales = "free_x", nrow=1, labeller = as_labeller(c(`ll_sero_1` = "Sero+ % (15-07-2020)", `ll_sero_2` = "Sero+ % (29-07-2020)", `ll_pcr` = "PCR+ % (15-07-2020)", `ll_total` = "Total"))) +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank(), panel.grid.minor = element_blank()) +
  xlab("log likelihood") + ylab("x IFR")
dev.off()

# - Show these as a plot of likelihood against IFR multipliers
cowplot::plot_grid(part_fit_plot(fit_l_IFR[[1]]),part_fit_plot(fit_l_IFR[[2]]),part_fit_plot(fit_l_IFR[[3]]), nrow=1)


# - Estimate the correct IFR by fitting the IFR (or just identify this from the various IFR multiplier explored)


pdf(file = "analysis/figures/06_02_PCR_Sero_BMJ_RevEng_Deaths_100pc_Reporting_0_Severe.pdf", height = 15, width = 12)
cowplot::plot_grid(part_fit_plot(fit)+ annotate("text",x = as.Date("2020-09-15"),y = 48, label="Full Model"),
                   part_fit_plot(fit_2)+ annotate("text",x = as.Date("2020-08-01"),y = 48, label="p.sev = 0,\np.n.sev.d.tr = IFR*p.hos", hjust = 0),
                   pcr_fit_plot(Summ_sero_fit),pcr_fit_plot(Summ_sero_fit_2),
                   sero_fit_plot(Summ_sero_fit)+ annotate("text",x = as.Date("2020-05-01"),y = 0.17, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_fit)$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_fit)$sero_29,2)), hjust = 0),
                   sero_fit_plot(Summ_sero_fit_2)+ annotate("text",x = as.Date("2020-05-01"),y = 0.18, label=paste("LL\nSero 15th",round(log_lik_sero_fun(sero_fit_2)$sero_15,2),"\nSero 29th",round(log_lik_sero_fun(sero_fit_2)$sero_29,2)), hjust = 0), nrow = 3)
dev.off()


Step 3. Is the IFR shape and scale different?



  - Here we want to compare the different likelihoods with different shapes of IFR.
- To do this:
  1. Get the IFR  by age log linear equation from Report 34
2. Introduce 2 new parameters that alters the prob_non_severe_death_treatment by altering the gradient and intercept of the log linear relationship underlying the prob_non_severe_death_treatment.
3. Fit this by calculating the likelihood of the distribution of deaths by age.



Step 4. Given all of the above, fit to the offical reported COVID-19 deaths and infer reporting by comparing to serology and PCR prevalence
