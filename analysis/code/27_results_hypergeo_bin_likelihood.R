rm(list = ls())
devtools::load_all()
library(squire)
library(ggplot2)
library(dplyr)
library(reshape2)

### Load data
# Dataset_Name <- "Hypergeometric_Bin_Likelihood_fixed"
Dataset_Name <- "Set_1_nbinom_week"
fit_Model <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-02-06_Set_1_nbin_weeks.rds")

IFR_mat <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")
IFR_vec <- 1:nrow(IFR_mat) %in% readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds")
IFR_mat_fil <- IFR_mat[IFR_vec,]
rownames(IFR_mat_fil) <- 1:nrow(IFR_mat_fil)


# So now I want to plot against those dimensions to see how that worked
# estimate pcr/serology across data:
sero_pcr_df_l_IFR_slope <- lapply(X = fit_Model, FUN = function(x){
  seroprev_df(x)})

# Simplify data
Simple_Plot_Data_l_IFR_slope <- lapply(sero_pcr_df_l_IFR_slope, Summ_sero_pcr_data)






#################################################
#################################################
# Plot graphs for deaths, pcr, sero, combined prevalence
#################################################
#################################################


PlotsList_1 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){return(NA)} else {
    # browser()
    y <- sum(IFR_vec[1:x])
    p1 <- part_fit_plot(fit_Model[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5) #+ annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nDeath data:",round(loglik_data(fit_l_IFR[[x]]),2)), hjust = 0, vjust = 4)
    return(p1)
  }
})

PlotsList_2 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){
    # browser;
    return(NULL)} else {
      # browser()
      y <- sum(IFR_vec[1:x])
      p2 <- pcr_fit_plot(Simple_Plot_Data_l_IFR_slope[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5)# + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nPCR 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$pcr_15,2)), hjust = 0, vjust = 1.5)
      return(p2)
    }
})

PlotsList_3 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){return(NA)} else {
    y <- sum(IFR_vec[1:x])
    p3 <- sero_fit_plot(Simple_Plot_Data_l_IFR_slope[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5)# + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nSero 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$sero_15,2)), hjust = 0, vjust = 1.5)
    return(p3)
  }
})

PlotsList_4 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){return(NA)} else {
    y <- sum(IFR_vec[1:x])
    p3 <- combined_fit_plot(Simple_Plot_Data_l_IFR_slope[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5)# + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nSero 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$sero_15,2)), hjust = 0, vjust = 1.5)
    return(p3)
  }
})


pdf(file = paste0("analysis/figures/27_1_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_1, nrow = 9,byrow = F)
dev.off()

pdf(file = paste0("analysis/figures/27_2_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_2, nrow = 9,byrow = F)
dev.off()

pdf(file = paste0("analysis/figures/27_3_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_3, nrow = 9,byrow = F)
dev.off()

pdf(file = paste0("analysis/figures/27_6_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_4, nrow = 9,byrow = F)
dev.off()



#################################################
#################################################
# Plot heatmaps for Likelihood and posterior
#################################################
#################################################


ModFits_LL <- lapply(fit_Model, function(x){
  # browser()
  LikC1 <- x$pmcmc_results$chains$chain1$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(x$replicate_parameters), value = T))))]
  LikC2 <- x$pmcmc_results$chains$chain2$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(x$replicate_parameters), value = T))))]
  LikC3 <- x$pmcmc_results$chains$chain3$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(x$replicate_parameters), value = T))))]

  return(mean(unlist(LikC1,LikC2,LikC3)))
})

Heatmap_Data_Model_Fit <- cbind(IFR_mat_fil, ModFits_LL = unlist(ModFits_LL)) %>%
  mutate(mean_gr = as.numeric(cut(round(ModFits_LL), breaks= c(-Inf,round(max(ModFits_LL,na.rm = T))-500,
                                                            round(max(ModFits_LL,na.rm = T))-200,
                                                            round(max(ModFits_LL,na.rm = T))-100,
                                                            round(max(ModFits_LL,na.rm = T))-50,
                                                            round(max(ModFits_LL,na.rm = T))-20,
                                                            round(max(ModFits_LL,na.rm = T))-12,
                                                            round(max(ModFits_LL,na.rm = T))-8,
                                                            round(max(ModFits_LL,na.rm = T))-4,
                                                            round(max(ModFits_LL,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))

p3 <- ggplot(Heatmap_Data_Model_Fit, aes(x = IFR_x, y = Slope_x, fill = as.numeric(ModFits_LL))) + geom_tile() +
  geom_text(aes(label = round(as.numeric(ModFits_LL)))) +#, colour = (as.numeric(LL_gr) >= max(as.numeric(LL_gr), na.rm=T))), size = 4) +
  # scale_colour_manual(values = c("white", "black")) +
  ggtitle("Model log likelihood") + xlab("xIFR") + ylab("xSlope") +
  labs(fill = "Mean ll") + theme(legend.position = "none") +
  scale_fill_viridis_c()


ModFits_Post <- lapply(fit_Model, function(x){
  # browser()
  LikC1 <- x$pmcmc_results$chains$chain1$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(x$replicate_parameters), value = T))))]
  LikC2 <- x$pmcmc_results$chains$chain2$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(x$replicate_parameters), value = T))))]
  LikC3 <- x$pmcmc_results$chains$chain3$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(x$replicate_parameters), value = T))))]

  return(mean(unlist(LikC1,LikC2,LikC3)))
})


Heatmap_Data_Model_Fit <- Heatmap_Data_Model_Fit  %>% mutate(ModFits_Post = as.numeric(unlist(ModFits_Post))) %>%
  mutate(mean_p_gr = as.numeric(cut(round(ModFits_Post), breaks= c(-Inf,round(max(ModFits_Post,na.rm = T))-500,
                                                            round(max(ModFits_Post,na.rm = T))-200,
                                                            round(max(ModFits_Post,na.rm = T))-100,
                                                            round(max(ModFits_Post,na.rm = T))-50,
                                                            round(max(ModFits_Post,na.rm = T))-20,
                                                            round(max(ModFits_Post,na.rm = T))-12,
                                                            round(max(ModFits_Post,na.rm = T))-8,
                                                            round(max(ModFits_Post,na.rm = T))-4,
                                                            round(max(ModFits_Post,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))

p4 <- ggplot(Heatmap_Data_Model_Fit, aes(x = IFR_x, y = Slope_x, fill = as.numeric(ModFits_Post))) + geom_tile() +
  geom_text(aes(label = round(as.numeric(ModFits_Post)))) +#, colour = (as.numeric(LL_gr) >= max(as.numeric(LL_gr), na.rm=T))), size = 4) +
  # scale_colour_manual(values = c("white", "black")) +
  ggtitle("Log posterior") + xlab("xIFR") + ylab("xSlope") +
  labs(fill = "Mean ll") + theme(legend.position = "none") +
  scale_fill_viridis_c()


Comb_data <- readRDS("analysis/data/Code-generated-data/00_08_Combined_mortuary_postmortem_data.rds")
Comb_data_gr <- Comb_data %>% group_by(Age_group) %>%
  summarise(total_deaths = sum(total_deaths),
            sampled_deaths = sum(sampled_deaths),
            CT_45_Either = sum(CT_45_Either)) %>%
  mutate(Est_Deaths = total_deaths * CT_45_Either/sampled_deaths)
index <- squire:::odin_index(fit_Model$X1$model)
# sum(apply(apply(fit_Model$X1$output[rownames(fit_Model$X1$output) %in% c("2020-06-14","2020-10-02"),index$D,], 1, rowMeans), 1, diff))
Mod_Age_Deaths <- lapply(fit_Model, function(x){
  tmp <- cbind(Comb_data_gr, Mod_Deaths = 0.8*apply(apply(x$output[rownames(fit_Model$X1$output) %in% c("2020-06-14","2020-10-02"),index$D,], 1, rowMeans), 1, diff))
  # browser()
  tmp$Mod_Est_Deaths <- tmp$sampled_deaths * tmp$Mod_Deaths/tmp$total_deaths
  tmp})

Comb_data_gr_date <- Comb_data %>% group_by(date) %>%
  summarise(total_deaths = sum(total_deaths),
            sampled_deaths = sum(sampled_deaths),
            CT_45_Either = sum(CT_45_Either)) %>%
  mutate(Est_Deaths = total_deaths * CT_45_Either/sampled_deaths)
index <- squire:::odin_index(fit_Model$X1$model)
sum(rowSums(apply(apply(fit_Model$X1$output[as.Date(rownames(fit_Model$X1$output)) %in% c(seq.Date(from = as.Date("2020-06-14"),
                                                                              to = as.Date("2020-10-02"),
                                                                              by = "week"),as.Date("2020-10-02")),index$D,], 1, rowMeans), 1, diff)))
Mod_Week_Deaths <- lapply(fit_Model, function(x){
  tmp <- cbind(Comb_data_gr_date, Mod_Deaths = 0.8*rowSums(apply(apply(x$output[as.Date(rownames(fit_Model$X1$output)) %in% c(seq.Date(from = as.Date("2020-06-14"),
                                                                                                                                             to = as.Date("2020-10-02"),
                                                                                                                                             by = "week"),as.Date("2020-10-02")),index$D,], 1, rowMeans), 1, diff)))
  # browser()
  tmp$Mod_Est_Deaths <- tmp$sampled_deaths * tmp$Mod_Deaths/tmp$total_deaths
  tmp})





## Plot likelihoods:
# First let's do the binomial one
comb_df <- fit_Model$X1$pmcmc_results$inputs$pars_obs$comb_df
comb_det <- fit_Model$X1$pmcmc_results$inputs$pars_obs$comb_det

comb_at_date <- function(date, symptoms, det, dates, N) {
  # browser()
  di <- which(dates == date)
  if(length(di) > 0) {
    to_sum <- tail(symptoms[seq_len(di)], length(det))
    min(sum(rev(to_sum)*head(det, length(to_sum)), na.rm=TRUE)/N, 0.99)
  } else {
    0
  }
}
# get infection incidence
LL_bin <- lapply(fit_Model, function(x){
  # browser()
  ll_reps <- apply(x$output, 3, function(out){
    # browser()
    infections_tmp <- c(0,rowSums(out[-nrow(out),index$S]-out[-1,index$S]))
    dates <- as.Date(rownames(out)[[1]]) + seq_len(nrow(out)) - 1L
    N <- sum(fit_Model$X1$parameters$population)
    comb_dates <- list(comb_df$date_end, comb_df$date_start, comb_df$date_start + as.integer((comb_df$date_end - comb_df$date_start)/2))
    unq_comb_dates <- unique(c(comb_df$date_end, comb_df$date_start, comb_df$date_start + as.integer((comb_df$date_end - comb_df$date_start)/2)))

    comb_model <- vapply(unq_comb_dates, comb_at_date, numeric(1), infections_tmp, comb_det, dates, N)
    comb_model_mat <- do.call(cbind,lapply(comb_dates, function(x) {comb_model[match(x, unq_comb_dates)]}))
    rowMeans(dbinom(comb_df$comb_pos, comb_df$samples, comb_model_mat, log = TRUE))
  })
  # out <- apply(x$output, 2, rowMeans)
  mean(ll_reps)
})

Heatmap_Data_Model_Fit <- Heatmap_Data_Model_Fit %>% mutate(LL_bin = as.numeric(LL_bin)) %>%
  mutate(LL_gr = cut(round(LL_bin), breaks= c(-Inf,round(max(LL_bin,na.rm = T))-500,
                                                            round(max(LL_bin,na.rm = T))-200,
                                                            round(max(LL_bin,na.rm = T))-100,
                                                            round(max(LL_bin,na.rm = T))-50,
                                                            round(max(LL_bin,na.rm = T))-20,
                                                            round(max(LL_bin,na.rm = T))-12,
                                                            round(max(LL_bin,na.rm = T))-8,
                                                            round(max(LL_bin,na.rm = T))-4,
                                                            round(max(LL_bin,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9")))



p1 <- ggplot(Heatmap_Data_Model_Fit, aes(x = IFR_x, y = Slope_x, fill = as.numeric(LL_bin))) + geom_tile() +
  geom_text(aes(label = round(as.numeric(LL_bin)))) +#, colour = (as.numeric(LL_gr) >= max(as.numeric(LL_gr), na.rm=T))), size = 4) +
  # scale_colour_manual(values = c("white", "black")) +
  ggtitle("Binomial log likelihood") + xlab("xIFR") + ylab("xSlope") +
  labs(fill = "Mean ll") + theme(legend.position = "none") +
  scale_fill_viridis_c()

### Now to get the other likelihood


### Actual data for comparison
# Dates_needed <- c(unique(Comb_data$date)-1,tail(unique(Comb_data$date)-1,1)+7)  ## These are the day ends I want.
# Date_sequence <- seq(from = min(as.Date(data$date))+1, to = as.Date(max(as.Date(Comb_data$date)))+7, by = 1) ## Create a date sequence to encompass everything, starting with day 1
# Days_for_comparison <- seq_along(Date_sequence)[Date_sequence %in% Dates_needed] # Day 1 is 2020-05-28: Day start = 1, Day end = 0/29? I need 2020-05-14 day end
# Days_for_comparison_b <- ifelse(Days_for_comparison>max(data$day_end),max(data$day_end),Days_for_comparison) # If any days are past the available days, lower the highest to the max day

LL_hypergeo <- lapply(fit_Model, function(x){

  ll_reps <- apply(x$output, 3, function(out){

  # browser()
  # out <- apply(x$output, 2, rowMeans)


    Comb_data <- fit_Model$X1$pmcmc_results$inputs$pars_obs$combined_data
    pcr_det <- fit_Model$X1$pmcmc_results$inputs$pars_obs$pcr_det


    Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                     to = as.Date("2020-10-02"),
                                     by = "week"),as.Date("2020-10-02"))

  ### Model Deaths
  Mod_Deaths_Age <- apply(out[as.Date(rownames(out)) %in% Days_for_comparison,index$D], 2, diff)
  colnames(Mod_Deaths_Age) <- 1:17
  rownames(Mod_Deaths_Age) <- 1:16
  Mod_Deaths_Age <- Mod_Deaths_Age %>%
    reshape2::melt(value.name = "Ds", varnames= c("Week_num","Age_gr")) # 16 time periods, 17 age groups.

  roll_func <- function(x, det) {
    l <- length(det)
    ret <- rep(0, length(x))
    for(i in seq_along(ret)) {
      to_sum <- tail(x[seq_len(i)], length(det))
      ret[i] <- sum(rev(to_sum)*head(det, length(to_sum)))
    }
    return(ret)
  }


  ### Model PCR: Number of Infections
  pcr_pos <- apply(out[,index$S],2,function(x){
    # browser()
    x <- as.integer(x)
    # x <- ifelse(is.na(x),max(x, na.rm = T), x)
    infs <- c(0,diff(max(x, na.rm = T)-x))
    pcr_positive <- roll_func(infs, pcr_det)
    pcr_perc <- pcr_positive/max(x, na.rm = T)
  })

  pcr_perc <- pcr_pos[as.Date(rownames(out[,index$S])) %in% (Days_for_comparison[-1]-3),] # This could be -3 or -4 to get Weds or Thurs.
  colnames(pcr_perc) <- 1:17 # Reshape
  pcr_perc <- pcr_perc %>% reshape2::melt(value.name = "pcr_perc", varnames= c("Week_num","Age_gr")) # 16 time periods, 17 age groups.


  ################################################
  ################################################

  # calculate ll for deaths
  frac_mort <- 0.8 # Mortuary captures 80% of deaths in Lusaka
  Mod_Deaths_Age <- Mod_Deaths_Age %>% dplyr::rename(age_group = "Age_gr", week_no = "Week_num")
  Comb_data <- Comb_data %>% dplyr::rename(age_group = "Age_group", week_no = "Week_gr")
  pcr_perc <- pcr_perc %>% dplyr::rename(age_group = "Age_gr", week_no = "Week_num")

  Mod_Deaths_Age <- Mod_Deaths_Age %>% merge(x = ., y = Comb_data) %>%
    merge(x = ., y = pcr_perc) %>%
    mutate(Mod_cd = Ds) %>%
    mutate(Mod_cd_Lus = Mod_cd*frac_mort) %>%
    mutate(tot_mort_deaths = total_deaths) %>%
    mutate(Mod_ncd = (total_deaths - Mod_cd_Lus)) %>% # non-covid deaths
    mutate(Mod_pos_ncd = Mod_ncd*pcr_perc) %>%
    mutate(Mod_tot_pos_mort = Mod_cd_Lus+Mod_pos_ncd)

  # browser()

  # Likelihood_data <- data.frame(Pos_Samples = Comb_data$CT_45_Either,
  #            Samples = Comb_data$sampled_deaths,
  #            Mod_Pos_deaths = round(covid_pos_mort),
  #            TotD_min_ModD = round(Comb_data$total_deaths - covid_pos_mort)
  #            )
  # browser()
  Likelihood_data <- Mod_Deaths_Age %>%
    mutate(Mod_tot_neg_mort = tot_mort_deaths-Mod_tot_pos_mort) %>%
    # mutate(Mod_tot_neg_mort = ifelse(Mod_tot_neg_mort<0, sampled_deaths-CT_45_Either, Mod_tot_neg_mort)) %>% # If the number of modelled covid deaths exceeds the total number of deaths (ie the number of non-covid deaths is negative), increase the modelled number of non-covid deaths to the minimum it needs to be: min number of covid deaths that we know occurred.
    # mutate(Mod_tot_pos_mort = ifelse(CT_45_Either>Mod_tot_pos_mort, CT_45_Either,Mod_tot_pos_mort)) %>% # If the number of modelled positive tests is below the number of positive tests we know occurred, increase this to the minimum value it needs to be
    # mutate(Mod_tot_neg_mort = ifelse((sampled_deaths - CT_45_Either)>Mod_tot_neg_mort, (sampled_deaths - CT_45_Either),Mod_tot_neg_mort)) %>% # If the number of modelled negative tests is below the number of negative tests we know occurred, increase this to the minimun value it needs to be
    mutate(ll = dhyper(x = CT_45_Either, m =  round(Mod_tot_pos_mort), n = round(Mod_tot_neg_mort), k = sampled_deaths, log = T))

  Likelihood_data <- Likelihood_data %>% mutate(ll = ifelse(is.nan(ll), 0, ll))
# browser()
  ll <-sum(Likelihood_data$ll)
  })
  print(mean(ll_reps))
  mean(ll_reps)
  # browser()
  # return(nrow(Likelihood_data[Likelihood_data$ll %in% c(-Inf, NaN),]))
})


Heatmap_Data_Model_Fit <- Heatmap_Data_Model_Fit %>% mutate(LL_hypergeo = as.numeric(LL_hypergeo)) %>%
  mutate(LL_hg_gr = cut(round(LL_hypergeo), breaks= c(-Inf,round(max(LL_hypergeo,na.rm = T))-500,
                                              round(max(LL_hypergeo,na.rm = T))-200,
                                              round(max(LL_hypergeo,na.rm = T))-100,
                                              round(max(LL_hypergeo,na.rm = T))-50,
                                              round(max(LL_hypergeo,na.rm = T))-20,
                                              round(max(LL_hypergeo,na.rm = T))-12,
                                              round(max(LL_hypergeo,na.rm = T))-8,
                                              round(max(LL_hypergeo,na.rm = T))-4,
                                              round(max(LL_hypergeo,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9")))


p2 <- ggplot(Heatmap_Data_Model_Fit, aes(x = IFR_x, y = Slope_x, fill = as.numeric(LL_hypergeo))) + geom_tile() +
  geom_text(aes(label = round(as.numeric(LL_hypergeo)), colour = (as.numeric(LL_hg_gr) >= max(as.numeric(LL_hg_gr), na.rm=T))), size = 4) +
  scale_colour_manual(values = c("white", "black")) +
  ggtitle("Hypergeometric log likelihood") + xlab("xIFR") + ylab("xSlope") +
  labs(fill = "Mean ll") + theme(legend.position = "none") +
  scale_fill_viridis_c()


pdf(file = "analysis/figures/27_8_Fits.pdf", width = 22)
cowplot::plot_grid(p1,p2,p3,p4, nrow = 1)
dev.off()




#########################################
### Plot other heatmaps: pcr, sero:
#########################################
## Calculate the multinomial likelihood for the age distribution
# Get the proportion of deaths from BMJ

BMJ_Deaths_by_Age_Covid_Neg <- c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2)
BMJ_Deaths_by_Age_Covid_Pos <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Covid_Pos2 <- c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Tot <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2)


Likelihoods_IFR_Age_structure<- sapply(1:length(fit_Model), function(y){
  Tot_Deaths_by_Age_gr <- sapply(1:100, function(x){

    Deaths_reps_by_age <- tail(fit_Model[[y]]$output[,paste0("D[",1:17,"]"),x],1) - fit_Model[[y]]$output["2020-06-14",paste0("D[",1:17,"]"),x]
    # print(Deaths_reps_by_age)
    if(is.na(Deaths_reps_by_age)){return(0)}
    # Deaths_reps_by_age/pop_st_lu
    if(Dataset_Name %in% c("Total_Covid_Strict","True_Covid_Strict")){
      dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos2[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos2[17:21])), # Strict ct cut off
                size = sum(BMJ_Deaths_by_Age_Covid_Pos2),
                prob = Deaths_reps_by_age)
    } else {
      dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])), # Not strict ct cut off
                size = sum(BMJ_Deaths_by_Age_Covid_Pos),
                prob = Deaths_reps_by_age)
    }

  })
  # rowMeans(Tot_Deaths_by_Age_gr)
})


IFR_Age_LL <- IFR_mat_fil %>% cbind(t(log(Likelihoods_IFR_Age_structure))) %>%
  melt(data = ., id = c("IFR_x","Slope_x","IFR_abs","Slope_abs","Int_abs"), variable = "replicate",value.name = "Age_ll") %>%
  rename(IFR_val = "IFR_x", Slope_val = "Slope_x") %>%
  select(IFR_val, Slope_val, replicate, Age_ll)


sero_pcr_df_l_IFR_slope <- lapply(seq(length(sero_pcr_df_l_IFR_slope)), function(x){sero_pcr_df_l_IFR_slope[[x]] %>% mutate(IFR_val = IFR_mat_fil$IFR_x[[x]],
                                                                                                                            Slope_val = IFR_mat_fil$Slope_x[[x]])})
sero_pcr_df_l_IFR_slope_com <- data.table::rbindlist(sero_pcr_df_l_IFR_slope)

dd_l_IFR_slope <- sero_pcr_df_l_IFR_slope_com %>% filter(date %in% as.Date(c("2020-07-15","2020-07-29"))) %>%
  select(replicate, date, sero_perc, pcr_perc, combined_perc, IFR_val, Slope_val) %>%
  group_by(replicate, IFR_val, Slope_val) %>%
  # mutate(sero_perc_2 = sero_perc[date==as.Date("2020-07-29")]) %>% ungroup() %>%
  filter(date==as.Date("2020-07-15")) %>% select(-date) %>%
  mutate(ll_sero_1 = log(dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_perc)),
         # ll_sero_2 = log(dbinom(x = as.integer(0.106*1952), size = 1952, prob = sero_perc_2)),
         ll_pcr = log(dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_perc)),
         ll_combined = dbinom(x = as.integer(0.091*332), size = as.integer(332), prob = combined_perc, log = T),
  ) %>%
  # ll_deaths = Death_LLs) %>%
  merge(IFR_Age_LL,by = c("replicate","IFR_val","Slope_val")) %>%
  mutate(ll_total = ll_sero_1 + ll_pcr) %>% #+ Age_ll) %>%
  select(-sero_perc,-pcr_perc) %>%
  melt(data = ., id=c("IFR_val","Slope_val", "replicate"),value.name = "ll")




HeatMapData1 <- dd_l_IFR_slope %>% filter(variable == "ll_sero_1") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup() %>%
  # mutate(mean_gr = cut(mean_ll, breaks= c(-Inf,-100,-50,-20,-10,-5,0), labels = c("1","2","3","4","5","6")))
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))


HeatMapData2 <- dd_l_IFR_slope %>% filter(variable == "ll_pcr") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup %>%
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))

HeatMapData3 <- dd_l_IFR_slope %>% filter(variable == "Age_ll") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup %>%
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))

HeatMapData4 <- dd_l_IFR_slope %>% filter(variable == "ll_total") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup() %>%
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))

HeatMapData5 <- dd_l_IFR_slope %>% filter(variable == "ll_combined") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup() %>%
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))


# pdf(file = "analysis/figures/p_3_BMJ_Lancet_var_IFR_slope_5.pdf", height = 6, width = 20)
# pdf(file = "analysis/figures/p_3_BMJ_Lancet_var_IFR_slope_5_All_Age_Deaths.pdf", height = 6, width = 20)
pdf(file = paste0("analysis/figures/23_5_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 6, width = 16)
cowplot::plot_grid(
  ggplot(HeatMapData2, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
    geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
    scale_colour_manual(values = c("white", "black")) +
    ggtitle("Log likelihood: PCR prevalence") + xlab("xIFR") + ylab("xSlope") +
    labs(fill = "Mean ll") + theme(legend.position = "none") +
    scale_fill_viridis_c(),
  ggplot(HeatMapData1, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
    geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
    scale_colour_manual(values = c("white", "black")) +
    ggtitle("Log likelihood: seroprevalence") + xlab("xIFR") + ylab("xSlope") +
    labs(fill = "Mean ll") + theme(legend.position = "none") +
    scale_fill_viridis_c(),
  # ggplot(HeatMapData3, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
  #   geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
  #   scale_colour_manual(values = c("white", "black")) +
  #   ggtitle("Log likelihood: deaths by age") + xlab("xIFR") + ylab("xSlope") +
  #   labs(fill = "Mean ll") + theme(legend.position = "none") +
  #   scale_fill_viridis_c(),
  ggplot(HeatMapData4, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
    # geom_text(aes(label = round(Tot_LL), colour = Tot_LL>max(Tot_LL)-2), size = 4) +
    geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
    scale_colour_manual(values = c("white", "black")) +
    ggtitle("Log likelihood: total") + xlab("xIFR") + ylab("xSlope") +
    labs(fill = "Mean ll") + theme(legend.position = "none") +
    scale_fill_viridis_c(),
  # ggplot(HeatMapData5, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
  #   # geom_text(aes(label = round(Tot_LL), colour = Tot_LL>max(Tot_LL)-2), size = 4) +
  #   geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
  #   scale_colour_manual(values = c("white", "black")) +
  #   ggtitle("Log likelihood: combined") + xlab("xIFR") + ylab("xSlope") +
  #   labs(fill = "Mean ll") + theme(legend.position = "none") +
  #   scale_fill_viridis_c(),
  nrow = 1)
dev.off()






























################################################
################################################
## Can I also plot the age-fits?
## What exactly am I trying to do here?
## I want to see how well the age distribution matches the actual age distribution
################################################
################################################


Date_Age_Mod_Death <- apply(apply(fit_Model$X1$output[as.Date(rownames(fit_Model$X1$output)) %in% c(seq.Date(from = as.Date("2020-06-14"),
                                                                            to = as.Date("2020-10-02"),
                                                                            by = "week"),as.Date("2020-10-02")),index$D,], 1, rowMeans), 1, diff)
colnames(Date_Age_Mod_Death) <- 1:17
rownames(Date_Age_Mod_Death) <- 1:16
Date_Age_Mod_Death_melt <- melt(Date_Age_Mod_Death, varnames = c("Week_gr","Age_group"), value.name = "Mod_Deaths")
# merge(Date_Age_Mod_Death_melt,Comb_data) %>%
#   mutate(ll = )

pcr_det <- fit_Model$X1$pmcmc_results$inputs$pars_obs$pcr_det

pcr_pos <- lapply(fit_Model, function(x){
  apply(x$output, MARGIN = 3, function(y){
    apply(y[,index$S],2,function(z){
      # browser()
      z <- as.integer(z)
      infs <- c(0,diff(max(z)-z))
      pcr_positive <- roll_func(infs, pcr_det)
      pcr_perc <- pcr_positive/max(z)})})})



pcr_perc <- pcr_pos[Days_for_comparison[-1]-3,] # This could be -3 or -4 to get Weds or Thurs.
colnames(pcr_perc) <- 1:17 # Reshape
pcr_perc <- pcr_perc %>% reshape2::melt(value.name = "pcr_perc", varnames= c("Week_num","Age_gr")) # 16 time periods, 17 age groups.


################################################
################################################

# calculate ll for deaths
# Mod_Deaths_Age <- Mod_Deaths_Age %>% dplyr::rename(age_group = "Age_gr", week_no = "Week_num")
# Comb_data <- Comb_data %>% dplyr::rename(age_group = "Age_group", week_no = "Week_gr")
# pcr_perc <- pcr_perc %>% dplyr::rename(age_group = "Age_gr", week_no = "Week_num")
#
# Mod_Deaths_Age <- Mod_Deaths_Age %>% merge(x = ., y = Comb_data) %>%
#   merge(x = ., y = pcr_perc) %>%
#   mutate(Mod_cd = Ds) %>%
#   mutate(Mod_cd_Lus = Mod_cd*frac_mort) %>%
#   mutate(tot_mort_deaths = total_deaths) %>%
#   mutate(Mod_ncd = (total_deaths - Mod_cd_Lus)) %>% # non-covid deaths
#   mutate(Mod_pos_ncd = Mod_ncd*pcr_perc) %>%
#   mutate(Mod_tot_pos_mort = Mod_cd_Lus+Mod_pos_ncd)
#
# # browser()
#
# # Likelihood_data <- data.frame(Pos_Samples = Comb_data$CT_45_Either,
# #            Samples = Comb_data$sampled_deaths,
# #            Mod_Pos_deaths = round(covid_pos_mort),
# #            TotD_min_ModD = round(Comb_data$total_deaths - covid_pos_mort)
# #            )
# # browser()
# Likelihood_data <- Mod_Deaths_Age %>%
#   mutate(Mod_tot_neg_mort = tot_mort_deaths-Mod_tot_pos_mort) %>%
#   mutate(Mod_tot_neg_mort = ifelse(Mod_tot_neg_mort<0, sampled_deaths-CT_45_Either, Mod_tot_neg_mort)) %>% # If the number of modelled covid deaths exceeds the total number of deaths (ie the number of non-covid deaths is negative), increase the modelled number of non-covid deaths to the minimum it needs to be: min number of covid deaths that we know occurred.
#   mutate(Mod_tot_pos_mort = ifelse(CT_45_Either>Mod_tot_pos_mort, CT_45_Either,Mod_tot_pos_mort)) %>% # If the number of modelled positive tests is below the number of positive tests we know occurred, increase this to the minimum value it needs to be
#   mutate(Mod_tot_neg_mort = ifelse((sampled_deaths - CT_45_Either)>Mod_tot_neg_mort, (sampled_deaths - CT_45_Either),Mod_tot_neg_mort)) %>% # If the number of modelled negative tests is below the number of negative tests we know occurred, increase this to the minimun value it needs to be
#   mutate(ll = dhyper(x = CT_45_Either, m =  as.integer(Mod_tot_pos_mort), n = as.integer(Mod_tot_neg_mort), k = sampled_deaths, log = T))
#
# ll <- Likelihood_data$ll
#
#
PlotsList_5 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){return(NA)} else {
    # browser()
    y <- sum(IFR_vec[1:x])
    p1 <- ggplot(data = as.data.frame(Mod_Age_Deaths[[y]]), aes(x = Age_group, y = Mod_Est_Deaths)) + geom_line() + #part_fit_plot(fit_Model[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5) #+ annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nDeath data:",round(loglik_data(fit_l_IFR[[x]]),2)), hjust = 0, vjust = 4)
    geom_point(aes(y = CT_45_Either))
      return(p1)
  }
})

pdf(file = paste0("analysis/figures/27_7_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_5, nrow = 9,byrow = F)
dev.off()

### Similarly, plot deaths by date:
PlotsList_9 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){return(NA)} else {
    # browser()
    y <- sum(IFR_vec[1:x])
    p1 <- ggplot(data = as.data.frame(Mod_Week_Deaths[[y]]), aes(x = date, y = Mod_Est_Deaths)) + geom_line() + #part_fit_plot(fit_Model[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5) #+ annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nDeath data:",round(loglik_data(fit_l_IFR[[x]]),2)), hjust = 0, vjust = 4)
      geom_point(aes(y = CT_45_Either))
    return(p1)
  }
})

pdf(file = paste0("analysis/figures/27_9_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_9, nrow = 9,byrow = F)
dev.off()






HeatMapData1 <- dd_l_IFR_slope %>% filter(variable == "ll_sero_1") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup() %>%
  # mutate(mean_gr = cut(mean_ll, breaks= c(-Inf,-100,-50,-20,-10,-5,0), labels = c("1","2","3","4","5","6")))
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))















# pdf(file = "analysis/figures/p_3_BMJ_Lancet_var_IFR_slope_4.pdf", height = 20, width = 15)
pdf(file = paste0("analysis/figures/12_4_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 20, width = 15)
ggplot(filter(dd_l_IFR_slope, IFR_val >0), aes(x=ll, y=as.factor(IFR_val), fill=as.factor(Slope_val))) +
  geom_boxplot() +
  facet_wrap(. ~ variable, scales = "free_x", nrow=1, labeller = as_labeller(c(`ll_sero_1` = "Sero+ % (15-07-2020)", `ll_pcr` = "PCR+ % (15-07-2020)", `ll_total` = "Total"))) +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank(), panel.grid.minor = element_blank()) +
  xlab("log likelihood") + ylab("x IFR")
dev.off()




sero_pcr_df_l_IFR_slope <- lapply(seq(length(sero_pcr_df_l_IFR_slope)), function(x){sero_pcr_df_l_IFR_slope[[x]] %>% mutate(IFR_val = IFR_mat_fil$IFR_x[[x]],
                                                                                                                            Slope_val = IFR_mat_fil$Slope_x[[x]])})
sero_pcr_df_l_IFR_slope_com <- data.table::rbindlist(sero_pcr_df_l_IFR_slope)



Likelihoods_IFR_Age_structure<- sapply(1:length(fit_Model), function(y){
  Tot_Deaths_by_Age_gr <- sapply(1:100, function(x){

    Deaths_reps_by_age <- tail(fit_Model[[y]]$output[,paste0("D[",1:17,"]"),x],1) - fit_Model[[y]]$output["2020-06-14",paste0("D[",1:17,"]"),x]
    # print(Deaths_reps_by_age)
    if(is.na(Deaths_reps_by_age)){return(0)}
    # Deaths_reps_by_age/pop_st_lu
    if(Dataset_Name %in% c("Total_Covid_Strict","True_Covid_Strict")){
      dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos2[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos2[17:21])), # Strict ct cut off
                size = sum(BMJ_Deaths_by_Age_Covid_Pos2),
                prob = Deaths_reps_by_age)
    } else {
      dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])), # Not strict ct cut off
                size = sum(BMJ_Deaths_by_Age_Covid_Pos),
                prob = Deaths_reps_by_age)
    }

  })
  # rowMeans(Tot_Deaths_by_Age_gr)
})


IFR_Age_LL <- IFR_mat_fil %>% cbind(t(log(Likelihoods_IFR_Age_structure))) %>%
  melt(data = ., id = c("IFR_x","Slope_x","IFR_abs","Slope_abs","Int_abs"), variable = "replicate",value.name = "Age_ll") %>%
  rename(IFR_val = "IFR_x", Slope_val = "Slope_x") %>%
  select(IFR_val, Slope_val, replicate, Age_ll)


dd_l_IFR_slope <- sero_pcr_df_l_IFR_slope_com %>% filter(date %in% as.Date(c("2020-07-15","2020-07-29"))) %>%
  select(replicate, date, sero_perc, pcr_perc, combined_perc, IFR_val, Slope_val) %>%
  group_by(replicate, IFR_val, Slope_val) %>%
  # mutate(sero_perc_2 = sero_perc[date==as.Date("2020-07-29")]) %>% ungroup() %>%
  filter(date==as.Date("2020-07-15")) %>% select(-date) %>%
  mutate(ll_sero_1 = log(dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_perc)),
         # ll_sero_2 = log(dbinom(x = as.integer(0.106*1952), size = 1952, prob = sero_perc_2)),
         ll_pcr = log(dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_perc)),
         ll_combined = dbinom(x = as.integer(0.091*0.631*1952), size = as.integer(0.631*1952), prob = combined_perc, log = T),
  ) %>%
  # ll_deaths = Death_LLs) %>%
  merge(IFR_Age_LL,by = c("replicate","IFR_val","Slope_val")) %>%
  mutate(ll_total = ll_sero_1 + ll_pcr) %>% #+ Age_ll) %>%
  select(-sero_perc,-pcr_perc) %>%
  melt(data = ., id=c("IFR_val","Slope_val", "replicate"),value.name = "ll")






fit_BMJ_mort <- fit_Model[[33]]
# Combined_Weeks <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$combined_data
# pcr_det <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$pcr_det
## Check that the likelihood calculations are the same as expected.

get_deaths_age_week <- function(fit_BMJ_mort){
  Combined_Weeks <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$combined_data
  # browser()
  Deaths <- squire::format_output(fit_BMJ_mort, c("D"), date_0 = max(fit_BMJ_mort$pmcmc_results$inputs$data$date), reduce_age = F) %>%
    group_by(replicate,age_group) %>%
    filter(date %in% c(min(date, na.rm = T),unique(Combined_Weeks$date)-1, max(date, na.rm = T))) %>%
    mutate(D = y,
           D_end = c(0,head(y,-1))) %>%
    mutate(D_weekly = D-D_end) %>%
    filter(!row_number() == 1) %>%
    group_by(age_group, replicate) %>%
    mutate(WeekNo = 1:16)

  return(Deaths)

}


# Deaths_by_age <- Deaths %>% group_by(replicate, age_group) %>%
#   summarise(Deaths = sum(D_weekly)) %>% ungroup() %>%
#   group_by(age_group) %>%
#   summarise(mean_Deaths = mean(Deaths),
#             max_Deaths = max(Deaths),
#             min_Deaths = min(Deaths))

# Deaths_by_week <- Deaths %>% group_by(replicate, date) %>%
#   summarise(Deaths = sum(D_weekly)) %>% ungroup() %>%
#   group_by(date) %>%
#   summarise(mean_Deaths = mean(Deaths),
#             max_Deaths = max(Deaths),
#             min_Deaths = min(Deaths))


get_expected_pcr_only <- function(fit_BMJ_mort){
  pcr_det <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$pcr_det
  Combined_Weeks <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$combined_data
  ## I want to get average infections broken down by week and age group
  date_0 <- max(fit_BMJ_mort$pmcmc_results$inputs$data$date)
  # Deaths_data <- squire::format_output(fit_BMJ_mort, c("D"), date_0 = max(fit_BMJ_mort$pmcmc_results$inputs$data$date), reduce_age = T)
  inf <- squire::format_output(fit_BMJ_mort, c("S"), date_0 = max(fit_BMJ_mort$pmcmc_results$inputs$data$date), reduce_age = F) %>%
    na.omit() %>%
    mutate(S = as.integer(.data$y)) %>%
    group_by(replicate, age_group) %>%
    mutate(infections = c(0, diff(max(.data$S)-.data$S))) %>%
    select(replicate, t, date, .data$S, .data$infections)
  # select(replicate, age_group, t, date, .data$S, .data$infections, D)

  inf <- inf %>%
    group_by(replicate,age_group) %>%
    na.omit() %>%
    mutate(pcr_positive = roll_func(.data$infections, pcr_det),
           pcr_perc = .data$pcr_positive/max(.data$S,na.rm = TRUE)) %>%
    ungroup %>%
    filter(date %in% c(unique(Combined_Weeks$date)+3)) %>%
    group_by(age_group, replicate) %>%
    mutate(WeekNo = 1:16)

  # pcr_dates <- inf %>% group_by(date,age_group) %>%
  #   summarise(mean_pcr = mean(pcr_perc),
  #             max_pcr = max(pcr_perc),
  #             min_pcr = min(pcr_perc))

  return(inf)
}

# get_expected_pcr_only(fit_Model[[1]])

recalculate_likelihood <- function(fit_BMJ_mort){

  Deaths <- get_deaths_age_week(fit_BMJ_mort)
  pcr_vals <- get_expected_pcr_only(fit_BMJ_mort)
  Combined_Weeks <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$combined_data

  Deaths_inf <- merge(x = pcr_vals, y = Deaths, by = c("age_group","replicate","WeekNo")) %>%
    mutate(D_weekly = replace_na(D_weekly, 0)) %>%
    select(age_group, replicate, WeekNo, date.y, pcr_perc, D_weekly)
  # browser()
  Non_cov_deaths_est <- Combined_Weeks %>% rename(age_group = "Age_group", WeekNo = "Week_gr") %>%
    merge(x = ., y = Deaths_inf, by = c("age_group","WeekNo")) %>%
    select(-date.y) %>%
    mutate(Mod_cd = D_weekly) %>%
    mutate(Mod_cd_mort = Mod_cd*0.8) %>%
    mutate(tot_mort_deaths = total_deaths) %>%
    mutate(Mod_ncd = (total_deaths - Mod_cd_mort)) %>% # non-covid deaths
    mutate(Mod_pos_ncd = Mod_ncd*pcr_perc) %>%
    mutate(Mod_tot_pos_mort = Mod_cd_mort+Mod_pos_ncd)

  Model_Data_Comp <- Non_cov_deaths_est %>% group_by(age_group, replicate) %>%
    summarise(Mod_cd = sum(Mod_cd),
              Mod_cd_mort = sum(Mod_cd_mort),
              tot_mort_deaths = sum(tot_mort_deaths),
              Mod_ncd = sum(Mod_ncd),
              Mod_pos_ncd = sum(Mod_pos_ncd),
              Mod_tot_pos_mort = sum(Mod_tot_pos_mort),
              sampled_deaths = sum(sampled_deaths),
              CT_45_Either = sum(CT_45_Either),
              # Mod_cov_deaths = sum(Mod_cov_deaths),
    ) %>%
    ungroup() %>% group_by(age_group) %>%
    summarise(Mod_cd = mean(Mod_cd),
              Mod_cd_mort = mean(Mod_cd_mort),
              tot_mort_deaths = mean(tot_mort_deaths),
              Mod_ncd = mean(Mod_ncd),
              Mod_pos_ncd = mean(Mod_pos_ncd),
              Mod_tot_pos_mort = mean(Mod_tot_pos_mort),
              sampled_deaths = mean(sampled_deaths),
              CT_45_Either = mean(CT_45_Either)
    ) %>%
    mutate(Mod_prop_pos = Mod_tot_pos_mort/tot_mort_deaths,
           Mort_prop_pos = CT_45_Either/sampled_deaths,
           Mod_Pos_Test = Mod_prop_pos*sampled_deaths)

  return(Model_Data_Comp)


  # Model_Data_Comp_Week <- Non_cov_deaths_est %>% group_by(age_group, WeekNo, replicate) %>%
  #   summarise(Mod_cd = sum(Mod_cd),
  #             Mod_cd_mort = sum(Mod_cd_mort),
  #             tot_mort_deaths = sum(tot_mort_deaths),
  #             Mod_ncd = sum(Mod_ncd),
  #             Mod_pos_ncd = sum(Mod_pos_ncd),
  #             Mod_tot_pos_mort = sum(Mod_tot_pos_mort),
  #             sampled_deaths = sum(sampled_deaths),
  #             CT_45_Either = sum(CT_45_Either),
  #             # Mod_cov_deaths = sum(Mod_cov_deaths),
  #   ) %>%
  #   ungroup() %>% group_by(age_group, WeekNo) %>%
  #   summarise(Mod_cd = mean(Mod_cd),
  #             Mod_cd_mort = mean(Mod_cd_mort),
  #             tot_mort_deaths = mean(tot_mort_deaths),
  #             Mod_ncd = mean(Mod_ncd),
  #             Mod_pos_ncd = mean(Mod_pos_ncd),
  #             Mod_tot_pos_mort = mean(Mod_tot_pos_mort),
  #             sampled_deaths = mean(sampled_deaths),
  #             CT_45_Either = mean(CT_45_Either)
  #   ) %>%
  #   mutate(Mod_prop_pos = Mod_tot_pos_mort/tot_mort_deaths,
  #          Mort_prop_pos = CT_45_Either/sampled_deaths,
  #          Mod_Pos_Test = Mod_prop_pos*sampled_deaths) %>%
  #   mutate(Too_Many_Deaths = ifelse(round(Mod_ncd + Mod_cd_mort) > tot_mort_deaths, TRUE, FALSE))
  #
  # return(Model_Data_Comp_Week)
}


fit_Model[[5]]$pmcmc_results$inputs$pars_obs$combined_data$total_deaths
Recreate_results <- lapply(fit_Model, recalculate_likelihood)

Res_fit_33 <- recalculate_likelihood(fit_Model[[33]])
Res_fit_32 <- recalculate_likelihood(fit_Model[[32]])
Res_fit_1 <- recalculate_likelihood(fit_Model[[1]])
Res_fit_5 <- recalculate_likelihood(fit_Model[[5]])



Recreate_results

fit_Model$X2$pmcmc_results$inputs$model_params$prob_non_severe_death_treatment

rownames(IFR_mat_fil) <- 1:65
IFR_mat_fil

fit_Model$X1$pmcmc_results$chains$chain1$results
plot(fit_Model$X4$replicate_parameterspmcmc_results$chains$chain1$results$log_posterior)

lapply(Recreate_results, function(x){any(x$Too_Many_Deaths)})

# "tot_mort_deaths"

cbind(fit_Model[[1]]$pmcmc_results$inputs$pars_obs$combined_data$total_deaths,
      rowSums(Recreate_results[[64]][,c("Mod_cd_mort", "Mod_ncd")]))


fit_BMJ_mort <- fit_Model[[1]]
Res_first_one <- recalculate_likelihood(fit_Model[[1]])
Res_first_one

Res_first_one$Too_Many_Deaths

head(Res_first_one)


Deaths_inf <- merge(x = inf, y = Deaths, by = c("age_group","replicate","WeekNo")) %>%
  select(age_group, replicate, WeekNo, date.y, pcr_perc, D_weekly)

Non_cov_deaths_est <- Combined_Weeks %>% rename(age_group = "Age_group", WeekNo = "Week_gr") %>%
  merge(x = ., y = Deaths_inf, by = c("age_group","WeekNo")) %>%
  select(-date.y) %>%
  mutate(Mod_cd = D_weekly) %>%
  mutate(Mod_cd_mort = Mod_cd*0.8) %>%
  mutate(tot_mort_deaths = total_deaths) %>%
  mutate(Mod_ncd = (total_deaths - Mod_cd_mort)) %>% # non-covid deaths
  mutate(Mod_pos_ncd = Mod_ncd*pcr_perc) %>%
  mutate(Mod_tot_pos_mort = Mod_cd_mort+Mod_pos_ncd)

Model_Data_Comp <- Non_cov_deaths_est %>% group_by(age_group, replicate) %>%
  summarise(Mod_cd = sum(Mod_cd),
            Mod_cd_Lus = sum(Mod_cd_Lus),
            tot_mort_deaths = sum(tot_mort_deaths),
            Mod_ncd = sum(Mod_ncd),
            Mod_pos_ncd = sum(Mod_pos_ncd),
            Mod_tot_pos_mort = sum(Mod_tot_pos_mort),
            sampled_deaths = sum(sampled_deaths),
            CT_45_Either = sum(CT_45_Either),
            # Mod_cov_deaths = sum(Mod_cov_deaths),
  ) %>%
  ungroup() %>% group_by(age_group) %>%
  summarise(Mod_cd = mean(Mod_cd),
            Mod_cd_Lus = mean(Mod_cd_Lus),
            tot_mort_deaths = mean(tot_mort_deaths),
            Mod_ncd = mean(Mod_ncd),
            Mod_pos_ncd = mean(Mod_pos_ncd),
            Mod_tot_pos_mort = mean(Mod_tot_pos_mort),
            sampled_deaths = mean(sampled_deaths),
            CT_45_Either = mean(CT_45_Either)
  ) %>%
  mutate(Mod_prop_pos = Mod_tot_pos_mort/tot_mort_deaths,
         Mort_prop_pos = CT_45_Either/sampled_deaths,
         Mod_Pos_Test = Mod_prop_pos*sampled_deaths)

colSums(Model_Data_Comp)

## But what about the break-down by age and week? Are there weeks that should be impossible?
Model_Data_Comp_Week <- Non_cov_deaths_est %>% group_by(age_group, WeekNo, replicate) %>%
  summarise(Mod_cd = sum(Mod_cd),
            Mod_cd_Lus = sum(Mod_cd_Lus),
            tot_mort_deaths = sum(tot_mort_deaths),
            Mod_ncd = sum(Mod_ncd),
            Mod_pos_ncd = sum(Mod_pos_ncd),
            Mod_tot_pos_mort = sum(Mod_tot_pos_mort),
            sampled_deaths = sum(sampled_deaths),
            CT_45_Either = sum(CT_45_Either),
            # Mod_cov_deaths = sum(Mod_cov_deaths),
  ) %>%
  ungroup() %>% group_by(age_group, WeekNo) %>%
  summarise(Mod_cd = mean(Mod_cd),
            Mod_cd_Lus = mean(Mod_cd_Lus),
            tot_mort_deaths = mean(tot_mort_deaths),
            Mod_ncd = mean(Mod_ncd),
            Mod_pos_ncd = mean(Mod_pos_ncd),
            Mod_tot_pos_mort = mean(Mod_tot_pos_mort),
            sampled_deaths = mean(sampled_deaths),
            CT_45_Either = mean(CT_45_Either)
  ) %>%
  mutate(Mod_prop_pos = Mod_tot_pos_mort/tot_mort_deaths,
         Mort_prop_pos = CT_45_Either/sampled_deaths,
         Mod_Pos_Test = Mod_prop_pos*sampled_deaths)


inf %>% group_by(age_group, WeekNo) %>%
  summarise(pcr_perc = mean(pcr_perc))


Combined_Weeks %>% group_by(Age_group) %>%
  summarise(total_deaths = sum(total_deaths),
            sampled_deaths = sum(sampled_deaths),
            CT_45_Either = sum(CT_45_Either))

Non_cov_deaths_est %>% filter(age_group ==1) group_by()


































































## plot the standard IFR against the best IFR we estimated
# lm(log(IFR_l$IFR_Age) ~ Age_groups)

MaxLL <- HeatMapData4[which.max(HeatMapData4$Tot_LL),]

plot(y = Age_groups*IFR_Coefs[2] + IFR_Coefs[1], x = Age_groups, type = "l",
     xlab = "Age", ylab = "log IFR")
lines(x = Age_groups,
      y = Age_groups*IFR_mat[IFR_mat$IFR_x == MaxLL$IFR_val & IFR_mat$Slope_x == MaxLL$Slope_val,"Slope_abs"] + IFR_mat[IFR_mat$IFR_x == MaxLL$IFR_val & IFR_mat$Slope_x == MaxLL$Slope_val,"Int_abs"],
      col = "darkblue", lty = 2)

plot(y = exp(Age_groups*IFR_Coefs[2] + IFR_Coefs[1]), x = Age_groups, type = "l",
     xlab = "Age", ylab = "log IFR")
lines(x = Age_groups,
      y = exp(Age_groups*IFR_mat[IFR_mat$IFR_x == MaxLL$IFR_val & IFR_mat$Slope_x == MaxLL$Slope_val,"Slope_abs"] + IFR_mat[IFR_mat$IFR_x == MaxLL$IFR_val & IFR_mat$Slope_x == MaxLL$Slope_val,"Int_abs"]),
      col = "darkblue", lty = 2)

#######################
## Now I want to plot a similar plot to the example plot I had:
#######################

IFR_Age <- readRDS(file = "analysis/data/Code-generated-data/00_03b_IFR_values.rds")
Age_vals <- seq(2.5, 82.5, by = 5)

#Plot 1: IFR curves
# What is the intercept when the IFR is 1.1?
library(dplyr)
library(tidyverse)
library(reshape2)

plot1_df <- data.frame(Age = seq(0,100,by = 0.1)) %>%
  mutate(IFR_1_1 = exp(Age * IFR_Coefs[2] + IFR_Coefs[1]),
         IFR_Best_1 = exp(Age * IFR_Coefs[2] + IFR_mat[round(IFR_mat$IFR_x,1)==1.4 & IFR_mat$Slope_x == 0.8,"Int_abs"]),
         IFR_Best_2 = exp(Age * IFR_Coefs[2] + IFR_mat[round(IFR_mat$IFR_x,1)==1.2 & IFR_mat$Slope_x == 0.8,"Int_abs"]),
  )

plot1_df_melt <- melt(plot1_df, id.vars = "Age") %>%
  mutate(IFR = recode(variable, "IFR_1_1" = "Standard IFR", "IFR_Best_1" = "1.4xIFR, 0.8xSlope", "IFR_Best_2" = "1.2xIFR, 0.8xSlope"))

p1 <- ggplot(data = plot1_df_melt, aes(x = Age, y = value, group = variable, col = IFR, linetype = IFR)) + geom_line() +
  scale_colour_manual(values = c("black", "darkred","darkblue")) +
  scale_linetype_manual(values = c(1,2,2)) +
  ylab("IFR") + xlim(0,85) + ylim(0,25) + labs(color='', linetype = '') #+
# geom_ribbon(data = subset(plot1_df_melt, variable == "IFR_0.6_1"),
#             aes(ymin = value, ymax = subset(plot1_df_melt, variable == "IFR_1.4_1")[,"value"]),
#             fill = "darkorange", alpha = 0.1) +
# geom_ribbon(data = subset(plot1_df_melt, variable == "IFR_1_0.9"),
#             aes(ymin = value, ymax = subset(plot1_df_melt, variable == "IFR_1_1.1")[,"value"]),
#             fill = "darkred", alpha = 0.1)

# Plot 2: Death curves
# fit_l_IFR_slope <- readRDS("../Bonus Files/fit_l_IFR_slope.RDS")

# Select the correct elements of the list
# IFR_vec <- c(0.4,0.6,0.8,0.9,1,1.1,1.2,1.4,1.6)
# Slope_vec <- seq(0.6, 1.4,by = 0.1)# * IFR_Coefs[2]
# IFR_mat <- expand.grid("IFR_x" = IFR_vec, "Slope_x" = Slope_vec)
IFR_mat_fil[IFR_mat_fil$IFR_x == 1 & IFR_mat_fil$Slope_x == 1 | round(IFR_mat_fil$IFR_x,1) == 1.4 & IFR_mat_fil$Slope_x == 0.8 | round(IFR_mat_fil$IFR_x,1) == 1.2 & IFR_mat_fil$Slope_x == 0.8, ]

names(fit_l_IFR_slope) <- apply(X = IFR_mat_fil, MARGIN = 1, function(x){paste0("IFR_",x[1],"_Slope",x[2])})

fit_subs <- fit_l_IFR_slope[IFR_mat_fil$IFR_x == 1 & IFR_mat_fil$Slope_x == 1 | round(IFR_mat_fil$IFR_x,1) == 1.4 & IFR_mat_fil$Slope_x == 0.8 | round(IFR_mat_fil$IFR_x,1) == 1.2 & IFR_mat_fil$Slope_x == 0.8]
fit_subs <- fit_subs[c(3,2,1)]

## For each element in the list plot the mean mortality fitted data
# rowMeans(apply(fit_subs[[1]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-1]-rowMeans(apply(fit_subs[[1]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-nrow(fit_subs[[1]]$output)]

colnames <- names(fit_subs)
Deaths_list <- lapply(1:length(fit_subs), function(x){
  Deaths_tmp <- rownames_to_column(data.frame(Deaths = rowMeans(apply(fit_subs[[x]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-1]-rowMeans(apply(fit_subs[[x]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-nrow(fit_subs[[x]]$output)])) %>%
    rename(Date = "rowname")
  names(Deaths_tmp)[2] <- colnames[x]
  Deaths_tmp
})

Deaths_df <- Deaths_list %>% reduce(full_join, by = "Date") %>%
  melt(id = "Date", variable = "IFR", value.name = "Deaths") %>%
  mutate(IFR = recode(IFR, "IFR_1_1" = "Standard IFR", "IFR_Best" = "Fitted IFR"))


fit_subs[[1]]$pmcmc_results$inputs$data

p2 <- ggplot(data = Deaths_df) +
  geom_line(aes(x = as.Date(Date), y = Deaths, group = IFR, col = IFR, linetype = IFR)) +
  scale_colour_manual(values = c("black","darkred","darkblue")) +
  scale_linetype_manual(values = c(1,2,2)) + #+ labs(color='', linetype = '') +
  xlab("Date") +
  geom_point(data = fit_subs[[1]]$pmcmc_results$inputs$data, aes(x = as.Date(date), y = deaths), alpha = 0.5, size = 0.5)
p2
# geom_ribbon(data = subset(Deaths_df, IFR == "IFR_1_Slope0.8"),
#             aes(ymin = Deaths, ymax = subset(Deaths_df, IFR == "IFR_1_Slope1.2")[,"Deaths"]),
#             fill = "darkred", alpha = 0.2) +
# geom_ribbon(data = subset(Deaths_df, IFR == "IFR_0.6_Slope1"),
#             aes(ymin = Deaths, ymax = subset(Deaths_df, IFR == "IFR_1.4_Slope1")[,"Deaths"]),
#             fill = "darkorange", alpha = 0.2)



## Plot 3: Plot the PCR/Sero curves
sero_pcr_df_l_IFR_slope <- lapply(X = fit_subs, FUN = function(x){
  seroprev_df(x)})

# Simplify data
Simple_Plot_Data_l_IFR_slope <- lapply(sero_pcr_df_l_IFR_slope, Summ_sero_pcr_data)

colnames <- names(Simple_Plot_Data_l_IFR_slope)
pcr_sero_list <- lapply(1:length(Simple_Plot_Data_l_IFR_slope), function(x){
  sero_pcr_tmp <- Simple_Plot_Data_l_IFR_slope[[x]] %>% select(date, mean_pcr, mean_sero)
  names(sero_pcr_tmp)[2] <- paste0("PCR_",colnames[x])
  names(sero_pcr_tmp)[3] <- paste0("sero_",colnames[x])
  sero_pcr_tmp
})


sero_df <- pcr_sero_list %>% reduce(full_join, by = "date") %>%
  select(date, paste0("sero_IFR_",c(1,1.4,1.2),"_Slope",c(1,0.8,0.8))) %>% arrange(date) %>%
  rename(IFR_1_Slope1 = "sero_IFR_1_Slope1",
         IFR_1.4_Slope0.8 = "sero_IFR_1.4_Slope0.8",
         IFR_1.2_Slope0.8 = "sero_IFR_1.2_Slope0.8") %>%
  melt(id = c("date"), variable = "IFR", value.name = "sero")

pcr_df <- pcr_sero_list %>% reduce(full_join, by = "date") %>%
  select(date, paste0("PCR_IFR_",c(1,1.4,1.2),"_Slope",c(1,0.8,0.8))) %>% arrange(date) %>%
  rename(IFR_1_Slope1 = "PCR_IFR_1_Slope1",
         IFR_1.4_Slope0.8 = "PCR_IFR_1.4_Slope0.8",
         IFR_1.2_Slope0.8 = "PCR_IFR_1.2_Slope0.8") %>%
  melt(id = c("date"), variable = "IFR", value.name = "pcr")

pcr_sero_df <- full_join(x = sero_df, y = pcr_df, by = c("date","IFR"))

p3 <- ggplot(pcr_sero_df, aes(x = date, y = pcr, group = IFR, col = IFR, linetype = IFR)) +
  geom_line() +
  scale_colour_manual(values = c("black","darkred","darkblue")) +
  scale_linetype_manual(values = c(1,2,2)) +# labs(color='', linetype = '') +
  geom_line(aes(y = sero/3)) +
  scale_y_continuous(
    name = "PCR+ prevalence", sec.axis = sec_axis(~.*3, name = "Seroprevalence"),
  ) + xlab("Date") +
  geom_point(aes(x= as.Date("2020-07-15"),y=7.6), col = "black", size = 0.9) +
  geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10), col = "black", width = 3, size = 0.3) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0), col = "black", size = 0.3, height = 0.2) +
  # annotate("text", x=as.Date("2020-07-15")+5, y=7.6+0.5, label="Mulenga et al.", hjust=0) +
  geom_point(aes(x= as.Date("2020-07-15"),y=2.1/3), col = "black", size = 0.9) +
  geom_errorbar(aes(ymin=1.1/3,ymax=3.1/3,x=as.Date("2020-07-15"), width=10), col = "black", width = 3, size = 0.3) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1/3, height=0), col = "black", size = 0.3, height = 0.2)+
  # annotate(geom = "text", x = "2020-06-01", y = 7.6,  label = "PCR") +
  annotate(geom = "text", x=as.Date("2020-07-19"), y = 8, hjust=0, label = "PCR+", fontface = "bold", size = 2.5) +
  annotate(geom = "text", x=as.Date("2020-07-19"), y = 1.1, hjust=0, label = "Sero+", fontface = "bold", size = 2.5)


# geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_1_Slope0.8"),
#             aes(ymin = sero/3, ymax = subset(pcr_sero_df, IFR == "IFR_1_Slope1.2")[,"sero"]/3),
#             fill = "darkred", alpha = 0.1) +
# geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_0.6_Slope1"),
#             aes(ymin = sero/3, ymax = subset(pcr_sero_df, IFR == "IFR_1.4_Slope1")[,"sero"]/3),
#             fill = "darkorange", alpha = 0.1) +
# geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_1_Slope0.8"),
#             aes(ymin = pcr, ymax = subset(pcr_sero_df, IFR == "IFR_1_Slope1.2")[,"pcr"]),
#             fill = "darkred", alpha = 0.1) +
# geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_0.6_Slope1"),
#             aes(ymin = pcr, ymax = subset(pcr_sero_df, IFR == "IFR_1.4_Slope1")[,"pcr"]),
#             fill = "darkorange", alpha = 0.1)
p3



# Plot 4: Plot the Death distribution by age of each:
# Can I also plot the expected points from the BMJ paper?
# Get the proportion of deaths from BMJ
BMJ_Deaths_by_Age_Covid_Neg <- c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2)
BMJ_Deaths_by_Age_Covid_Pos <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Covid_Pos2 <- c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Tot <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2)

Deaths_Age_List <- lapply(1:length(fit_subs), function(x){
  # browser()
  return(rowMeans(fit_subs[[x]]$output[nrow(fit_subs[[x]]$output),paste0("D[",1:17,"]"),]))
})
names(Deaths_Age_List) <- names(fit_subs)
Deaths_Age_df<-do.call(cbind.data.frame, Deaths_Age_List) %>%
  mutate(Age = Age_vals) %>%
  melt(id = "Age", variable = "IFR", value.name = "Deaths")

Est_Deaths <- data.frame(Age = Age_vals,
                         Deaths = c((BMJ_Deaths_by_Age_Covid_Pos2*10/0.8)[1:16],sum((BMJ_Deaths_by_Age_Covid_Pos2*10/0.8)[17:20])))

p4 <- ggplot(Deaths_Age_df, aes(x = Age)) +
  geom_line(aes(y = Deaths, group = IFR, col = IFR, linetype = IFR)) +
  scale_colour_manual(values = c("black","darkred","darkblue")) +
  scale_linetype_manual(values = c(1,2,2)) + labs(color='', linetype = '') +
  geom_point(data = Est_Deaths, aes(x = Age, y = Deaths), size = 0.5, alpha = 1)






# pdf(file = "analysis/figures/12_06_Best_Fit_Comp.pdf", height = 6, width = 8)
pdf(file = paste0("analysis/figures/12_06_Best_Fit_Comp_",Dataset_Name,".pdf"), height = 6, width = 8)
cowplot::plot_grid(p1 + theme_light() + theme(legend.position = "none"),
                   p2 + theme_light() +theme(legend.position = "none"),
                   cowplot::get_legend(p1 + theme_light() + labs(color='') + theme(legend.box.margin = margin(130,0,0,10))),
                   p3 + theme_light() +theme(legend.position = "none"),
                   p4 + theme_light() +theme(legend.position = "none"),
                   nrow = 2, rel_widths = c(4, 4, 2))
dev.off()

summary(cowplot::get_legend(p2))






lines(x = Age_groups,
      y = Age_groups*IFR_mat[2,"Slope_abs"] + IFR_mat[2,"Int_abs"],
      col = "darkred", lty = 2)
lines(x = Age_groups,
      y = Age_groups*IFR_mat[8,"Slope_abs"] + IFR_mat[8,"Int_abs"],
      col = "darkgreen", lty = 2)
lines(x = Age_groups,
      y = Age_groups*IFR_mat[10,"Slope_abs"] + IFR_mat[10,"Int_abs"],
      col = "darkgreen", lty = 2)

legend("topleft", legend = c("Brazeau IFR","1.2xIFR, 0.9xSlope","1.1xIFR, 1.2xSlope","1.4xIFR, 1.6xSlope"), col = c(1,"darkblue","darkred","darkgreen"), lty = c(1,2,2,2), bty = "n")





## Plot the proportion of total deaths in BMJ data that occur in each age group
BMJ_Deaths_by_Age_Covid_Neg <- c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2)
BMJ_Deaths_by_Age_Covid_Pos <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Tot <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2)

barplot(BMJ_Deaths_by_Age_Covid_Pos/BMJ_Deaths_by_Age_Tot)
barplot(BMJ_Deaths_by_Age_Covid_Pos/sum(BMJ_Deaths_by_Age_Covid_Pos),
        names = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-90","90-94","95-99",">99"), col="darkgrey", las = 2,
        ylab = "Proportion of Deaths")

## I also need the prevalence of death in each age group from the model we ran:

tail(Off_data$output)
str(Off_data$output)
colnames(Off_data$output)
plot(tail(Off_data$output[,"D[2]",],1))

tail(Off_data$output[,paste0("D[",1,"]"),],1)

names(Off_data$output[,"D[1]",1]) < "2020-10-1" & names(Off_data$output[,"D[1]",1]) > "2020-06-14"

Tot_Deaths_by_Age_gr <- sapply(1:17, function(x){
  mean(Off_data$output["2020-10-01",paste0("D[",x,"]"),]) - mean(Off_data$output["2020-06-14",paste0("D[",x,"]"),])
})

Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr)

c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])))

dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])),
          size = sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))),
          prob = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr))



dmultinom(x = round(100*c(0.002,0.004,0.01,0.012,0.014,0.019,0.03,0.04,0.05,0.05,0.05,0.05,0.07,0.09,0.1,0.13,0.2)),
          size = sum(round(100*c(0.002,0.004,0.01,0.012,0.014,0.019,0.03,0.04,0.05,0.05,0.05,0.05,0.07,0.09,0.1,0.13,0.2))),
          prob = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr))


plot(x = 1:17, y = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))), pch = 20, ylim = c(0,0.2), ylab = "Proportion Of Deaths")
points(x = 1:17, y = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr), col = 2, pch = 20)
legend("topleft", legend = c("BMJ","Model"), pch = 20, col = c(1,2))

plot(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))),
     y = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr), xlab = "BMJ Death Proportion", ylab = "Model Death Proportion",
     pch = 1:17, type = "n")
text(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))),
     y = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr))
abline(a = c(0,1))
