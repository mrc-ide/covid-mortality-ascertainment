#######################################################################################
### 2. Vary IFR over model fitted to BMJ data and lancet paper, holdling rf at 100% ###
#######################################################################################
## Bring in BMJ data:
BMJ <- readRDS("analysis/data/Code-generated-data/00_05b_BMJ_Data_DailyEsts.rds")
fit_1 <- readRDS(file = "../Bonus Files/p_01_Official_Data_Lancet_est_rf_resonable.rds")

# Scale up
p_pos <- seroprev_df(fit_1) %>% filter(date>="2020-06-08" & date<="2020-09-27") %>%
  group_by(date) %>% summarise(pcr_perc_av = mean(pcr_perc)) %>% # average over replicates
  # mutate(TimeFrame = cut.Date(as.Date(date), breaks = as.Date(c("2020-06-08","2020-06-22","2020-07-06","2020-07-20","2020-08-03","2020-08-17","2020-08-31","2020-09-14","2020-09-28")), labels = 1:8, start.on.monday = T)) %>%
  # group_by(TimeFrame) %>% summarise(PCR = mean(pcr_perc_av)) %>%
  select(pcr_perc_av)

p_bg <-readRDS(file = "analysis/data/Code-generated-data/00_06_Crude_Mortality.rds") # per fortnight

# r_maf <- (BMJ$q1_10x/p_pos - p_bg)/(1-p_bg)
r_maf <- (BMJ$q1_HighEst/p_pos - p_bg)/(1-p_bg)
r_maf_Strict <- (BMJ$q1_HighEst_Strict/p_pos - p_bg)/(1-p_bg)

pop_lu_prov <- sum(readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds"))
BMJ <- BMJ %>% mutate(#TrueCovDeathsLus_10xEst = unlist(pop_lu_prov*r_maf*p_pos),
                      TrueCovDeathsLus_HighEst = unlist(pop_lu_prov*r_maf*p_pos),
                      TrueCovDeathsLus_HighEst_Strict = unlist(pop_lu_prov*r_maf_Strict*p_pos))

BMJ_data_HighEst <- BMJ %>% select(date, TrueCovDeathsLus_HighEst, TrueCovDeathsLus_HighEst_Strict, Est_Deaths_BMJ_Sam_HE, Est_Deaths_BMJ_Sam_HE_Strict) %>%
  # rename(deaths = TrueCovDeathsLus_HighEst) %>%
  mutate(TrueCovDeathsLus_HighEst = round(TrueCovDeathsLus_HighEst),
         TrueCovDeathsLus_HighEst_Strict = round(ifelse(TrueCovDeathsLus_HighEst_Strict<0,0,TrueCovDeathsLus_HighEst_Strict)),
         Est_Deaths_BMJ_Sam_HE = round(Est_Deaths_BMJ_Sam_HE),
         Est_Deaths_BMJ_Sam_HE_Strict = round(Est_Deaths_BMJ_Sam_HE_Strict),
         )
saveRDS(BMJ_data_HighEst, "analysis/data/Code-generated-data/00_05c_BMJ_Data_DailyEsts_HighEsts.rds")

# Fit model to this data over a range of IFRs
IFR_vec <- c(0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3, 3.2)

fit_l_IFR <- lapply(1:length(IFR_vec), function(x){

  prob_death_tot_IFR_frac <- ifelse(prob_death_tot_IFR_frac*IFR_vec[x]>1,1,prob_death_tot_IFR_frac*IFR_vec[x])

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
                dur_R = Inf
                # sero_df_start = as.Date(c("2020-07-04","2020-07-18")),
                # sero_df_end = as.Date(c("2020-07-27","2020-08-10")),
                # sero_df_pos = as.numeric(as.integer(c(0.021*2704, 0.106*1952))), # See SFig.3
                # sero_df_samples = c(2704,1952),
                # pcr_df_start = as.Date(c("2020-07-04")),
                # pcr_df_end = as.Date(c("2020-07-27")),
                # pcr_df_pos = as.integer(c(0.076*2990)), # See SFig.3
                # pcr_df_samples = c(2990)
  )
})

## save these results
# saveRDS(fit_l_IFR, file = "../Bonus Files/Fit_varying_IFR_fit_only_to_deaths.rds")
fit_l_IFR <- readRDS(file = "../Bonus Files/Fit_varying_IFR_fit_only_to_deaths.rds")

# estimate pcr/serology across data:
sero_pcr_df_l_IFR <- lapply(X = fit_l_IFR, FUN = function(x){
  seroprev_df(x)})

# Simplify data
Simple_Plot_Data_l_IFR <- lapply(sero_pcr_df_l_IFR, Summ_sero_pcr_data)

### Calculate Log-likelihood function
log_lik_sero_fun <- function(sero_fit){
  sero_pred_1<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-15"), "sero_perc"])
  ll_sero_15 <- log(dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_pred_1))

  # sero_pred_2<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-29"), "sero_perc"])
  # ll_sero_29 <- log(dbinom(x = as.integer(0.106*1952), size = 1952, prob = sero_pred_2))

  pcr_pred_1<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-15"), "pcr_perc"])
  ll_pcr_15 <- log(dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_pred_1))

  return(list("sero_15"=ll_sero_15, "pcr_15"=ll_pcr_15))#, "sero_29"=ll_sero_29))
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

# apply(fit_l_IFR[[1]]$output[as.Date(rownames(fit_l_IFR[[1]]$output)) %in% fit_l_IFR[[1]]$pmcmc_results$inputs$data$date, odin_index(fit_l_IFR[[1]]$model)$D,], MARGIN = 3, FUN = rowSums)

# Plot graphs
part_fit_plot <- function(fit){plot(fit, particle_fit = T) +
    annotate(geom = "point", x= as.Date("2020-04-18"), y = 50) +
    annotate(geom = "text", x=as.Date("2020-04-22"), y = 50, hjust=0, label = "BMJ data estimated deaths") +
    annotate(geom = "segment", x=as.Date("2020-04-15"), xend = as.Date("2020-04-20"), y = 50*0.92, yend = 50*0.92, col = "red", alpha = 0.7) +
    annotate(geom = "rect", xmin=as.Date("2020-04-15"), xmax = as.Date("2020-04-20"), ymin = 50*0.915, ymax = 50*0.925, fill = NA, color = "black", linetype = 2) +
    annotate(geom = "text", x=as.Date("2020-04-22"), y = 50*0.92, hjust = 0, label = "Model fit") +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 50)) +
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
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 15)) +
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
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 20)) +
    theme_bw()}


PlotsList <- lapply(c(1,4,5,14), function(x){#seq(length(fit_l_IFR)), function(x){
  p1 <- part_fit_plot(fit_l_IFR[[x]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste("IFR x",IFR_vec[x]), vjust = 2) #+ annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nDeath data:",round(loglik_data(fit_l_IFR[[x]]),2)), hjust = 0, vjust = 4)
  p2 <- pcr_fit_plot(Simple_Plot_Data_l_IFR[[x]]) + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL PCR:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$pcr_15,2)), hjust = 0, vjust = 1.5)
  p3 <- sero_fit_plot(Simple_Plot_Data_l_IFR[[x]]) + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL Sero:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$sero_15,2)), hjust = 0, vjust = 1.5)
  return(list(p1,p2,p3))
})

# Plot data
pdf(file = "analysis/figures/p_2_BMJ_Lancet_var_IFR.pdf", height = 12, width = 15)
cowplot::plot_grid(plotlist = unlist(PlotsList, recursive = F), nrow = 3,byrow = F)
dev.off()

### Plot LL varying IFR:
## Manipulate data for plotting
# Melt and plot

sero_pcr_df_l_IFR_IFRcol <- lapply(seq(length(sero_pcr_df_l_IFR)), function(x){sero_pcr_df_l_IFR[[x]] %>% mutate(IFR_val = IFR_vec[[x]])})
sero_pcr_df_l_IFR_com <- data.table::rbindlist(sero_pcr_df_l_IFR_IFRcol)

# generate vector of ll for death data
Death_LLs <- unlist(lapply(seq(10), function(x){
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
         # ll_sero_2 = log(dbinom(x = as.integer(0.106*1952), size = 1952, prob = sero_perc_2)),
         ll_pcr = log(dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_perc))) %>%
  # ll_deaths = Death_LLs) %>%
  mutate(ll_total = ll_sero_1 + ll_pcr) %>%
  select(-sero_perc,-sero_perc_2,-pcr_perc) %>%
  melt(data = ., id=c("IFR_val", "replicate"),value.name = "ll") %>%
  mutate(across(variable, factor, levels = c("ll_pcr","ll_sero_1","ll_total")))



pdf(file = "analysis/figures/p_2b_BMJ_Lancet_var_IFR.pdf", height = 4, width = 10)
ggplot(filter(dd_l_IFR, IFR_val >0), aes(x=ll, y=as.factor(IFR_val), group=IFR_val)) +
  geom_boxplot(aes(fill=IFR_val)) +
  facet_wrap(. ~ variable, scales = "free_x", nrow=1, labeller = as_labeller(c(`ll_sero_1` = "Sero+ % (15-07-2020)", `ll_sero_2` = "Sero+ % (29-07-2020)", `ll_pcr` = "PCR+ % (15-07-2020)", `ll_deaths` = "BMJ Death data", `ll_total` = "Total"))) +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank(), panel.grid.minor = element_blank()) +
  xlab("log likelihood") + ylab("x IFR")
dev.off()








Likelihoods_IFR_Age_structure<- sapply(1:length(fit_l_IFR), function(y){
  Tot_Deaths_by_Age_gr <- sapply(1:100, function(x){
    # browser()
    Deaths_reps_by_age <- tail(fit_l_IFR_slope[[y]]$output[,paste0("D[",1:17,"]"),x],1) - fit_l_IFR_slope[[y]]$output["2020-06-14",paste0("D[",1:17,"]"),x]
    # Deaths_reps_by_age/pop_st_lu
    # dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])),
    #           size = sum(BMJ_Deaths_by_Age_Covid_Pos),
    #           prob = Deaths_reps_by_age/pop_st_lu)
    # dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])),
    #           size = sum(BMJ_Deaths_by_Age_Covid_Pos),
    #           prob = Deaths_reps_by_age/sum(Deaths_reps_by_age))
    # dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos2[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos2[17:21])),
    #           size = sum(BMJ_Deaths_by_Age_Covid_Pos2),
    #           prob = Deaths_reps_by_age/pop_st_lu)
    dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos2[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos2[17:21])),
              size = sum(BMJ_Deaths_by_Age_Covid_Pos2),
              prob = Deaths_reps_by_age/sum(Deaths_reps_by_age))

  })
  # rowMeans(Tot_Deaths_by_Age_gr)
})

log(colMeans(Likelihoods_IFR_Age_structure))

Likelihoods_IFR_Age_structure[,1]/pop_st_lu/sum(pop_st_lu)

which.max(colMeans(log(Likelihoods_IFR_Age_structure)))
length(colMeans(log(Likelihoods_IFR_Age_structure)))

IFR_Age_LL <- IFR_mat %>% mutate(Age_LL =colMeans(log(Likelihoods_IFR_Age_structure))) %>%
  select(IFR_x, Slope_x, Age_LL)


IFR_mat[1,]
IFR_mat[10,]
IFR_mat[19,]

Deaths_reps_by_age <- sapply(1:100, function(x){tail(fit_l_IFR_slope[[9]]$output[,paste0("D[",1:17,"]"),x],1) - fit_l_IFR_slope[[9]]$output["2020-06-14",paste0("D[",1:17,"]"),x]})
Deaths_reps_by_age <- sapply(1:100, function(x){tail(fit_l_IFR_slope[[1]]$output[,paste0("D[",1:17,"]"),x],1) - fit_l_IFR_slope[[1]]$output["2020-06-14",paste0("D[",1:17,"]"),x]})

rmultinom(n = 1, size = 70, prob = Deaths_reps_by_age[,2]/sum(Deaths_reps_by_age[,2]))

rmultinom(n = 1, size = 888, prob = Deaths_reps_by_age[,2]/sum(Deaths_reps_by_age[,2]))
rmultinom(n = 1, size = 888, prob = Deaths_reps_by_age[,2]/pop_st_lu)

log(dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])),
              size = 70,
              prob = Deaths_reps_by_age[,2]/pop_st_lu))


apply(IFR_mat, MARGIN = 1, function(x){
  # browser()
  exp(x["Int_abs"] + x["Slope_abs"]*Age_groups) * pop_st_lu/sum(pop_st_lu)
})[,1]

Deaths_reps_by_age <- sapply(1:100, function(x){tail(fit_l_IFR_slope[[1]]$output[,paste0("D[",1:17,"]"),x],1) - fit_l_IFR_slope[[1]]$output["2020-06-14",paste0("D[",1:17,"]"),x]})
rowMeans(Deaths_reps_by_age)/sum(rowMeans(Deaths_reps_by_age))

plot(rowMeans(Deaths_reps_by_age)/sum(rowMeans(Deaths_reps_by_age)), apply(IFR_mat, MARGIN = 1, function(x){
  # browser()
  exp(x["Int_abs"] + x["Slope_abs"]*Age_groups) * pop_st_lu/sum(pop_st_lu)
})[,1]
)


Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr)

c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])))

dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])),
          size = sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))),
          prob = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr))


fit_l_IFR_slope[[1]]$output

Tot_Deaths_by_Age_gr <- sapply(1:17, function(x){
  mean(Off_data$output["2020-10-01",paste0("D[",x,"]"),]) - mean(Off_data$output["2020-06-14",paste0("D[",x,"]"),])
})

Tot_Deaths_by_Age_gr <- sapply(1:17, function(x){
  mean(fit_l_IFR_slope[[1]]$output["2020-10-01",paste0("D[",x,"]"),]) - mean(fit_l_IFR_slope[[1]]["2020-06-14",paste0("D[",x,"]"),])
})

