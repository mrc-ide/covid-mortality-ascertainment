####
# Mortality ascertainment
## What do I want to show here?
library(tidyverse)
# devtools::load_all()
# library(tidyr)
library(reshape2)
devtools::load_all()




# For each model fit:
# - Do the poisson fitting graph
# - Do a three panel: Age prev, Week prev, PCR+sero+combined
# - Heatmap

# ClusterModelFit <- readRDS("../Bonus Files/2022-04-27_Model_Fit_10_pois_bin_bin_int_over.rds")

IFR_coefficients <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds") %>%
  mutate(Index = 1:nrow(.))
#
Select_Runs <- IFR_coefficients %>%
  filter(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_03_Prob_death_logical_log_sc.rds"),
         Slope_x %in% c(0.8,1,1.25)) %>%
  pull(Index)


fit_Model <- Test2
# fit_Model <- readRDS("../Bonus Files/2022-04-27_Model_Fit_10_pois_bin_bin_int_over.rds")[[1]]
IFR_x <- IFR_coefficients$IFR_x[41]
IFR_slope <- IFR_coefficients$Slope_x[41]

LL_Res_1_1 <- get_ll_components(fit_Model = fit_Model, IFR_slope = IFR_slope, IFR_x = IFR_x)
# LL_Res_1_1 <- get_ll_components(fit_Model = fit_Model$X41, IFR_slope = IFR_slope, IFR_x = IFR_x)

LL_Res <- lapply(1:length(fit_Model), function(x){
  get_ll_components(fit_Model[[x]], IFR_coefficients$IFR_x[Select_Runs][x], IFR_coefficients$IFR_x[Select_Runs][x])
})

Heatmaps <- Plot_Heatmaps(Mod_Res = fit_Model, Res_Figs = LL_Res, Select_Runs = Select_Runs, Title = "")

doLL_Res[[1]]

get_ll_components <- function(fit_Model, IFR_slope, IFR_x){

  browser()

  pcr_det <- fit_Model$pmcmc_results$inputs$pars_obs$pcr_det
  comb_det <- fit_Model$pmcmc_results$inputs$pars_obs$comb_det
  index <- squire:::odin_index(fit_Model$model)
  Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                   to = as.Date("2020-10-02"),
                                   by = "week"),as.Date("2020-10-02"))
  frac_mort <- 0.8
  FullData <- fit_Model$pmcmc_results$inputs$pars_obs$combined_data$Comb_data
  dfj_mcmc_data <- fit_Model$pmcmc_results$inputs$pars_obs$combined_data$dfj_mcmc_data


  Mod_Age_Deaths_Lus_pcr <- fit_Model$output[,c(index$D,index$S),] %>%
    melt(varnames = c("date","var","Replicate"), value.name = "value") %>%
    mutate(Age_gr = as.numeric(gsub(".*?([0-9]+).*", '\\1', var)),
           var = substr(var, 1, 1),
           date = as.Date(date)) %>%
    dcast(... ~ var, value.var="value") %>%
    rename(Mod_cd_Lus = "D", Sus = "S") %>%
    group_by(Age_gr, Replicate) %>%
    replace_na(list(Mod_cd_Lus = 0)) %>%
    mutate(Sus = ifelse(is.na(Sus), max(Sus, na.rm=T), Sus)) %>%
    mutate(cum_infs = as.integer(max(Sus, na.rm = T)-Sus),
           infs = c(0, as.integer(diff(max(Sus, na.rm = T)-Sus))),
           pcr_pos = cma:::roll_func_10(infs, pcr_det),
           pcr_perc = pcr_pos/max(Sus,na.rm = T))

  # Attack_rates <- Mod_Age_Deaths_Lus_pcr %>% group_by(Age_gr) %>%
  #   summarise(max_sus = max(Sus)) %>%
  #   merge(Mod_Age_Deaths_Lus_pcr) %>% group_by(Age_gr, date) %>%
  #   summarise(cum_infs = median(cum_infs),
  #             attack_rate = cum_infs/max_sus)
  #
  # ggplot(Attack_rates, aes(x = date, y = attack_rate)) + geom_line() +
  #   facet_wrap(~Age_gr) +
  #   theme_minimal()
  #
  # ggplot(Attack_rates, aes(x = date, y = attack_rate)) + geom_line() +
  #   facet_wrap(~Age_gr) +
  #   theme_minimal()


  Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus_pcr %>%
    # filter(Replicate ==1) %>%
    filter(date > "2020-06-10") %>%
    mutate(Week_gr = as.numeric(cut.Date(date, breaks = "1 week", labels = 1:17))) %>%
    group_by(Age_gr, Week_gr, Replicate) %>%
    summarise(Mod_cd_Lus = max(Mod_cd_Lus),
              pcr_perc = pcr_perc[4],
              infs = max(infs)) %>%
    ungroup() %>% group_by(Age_gr, Replicate) %>%
    mutate(Mod_cd_Lus = c(0,diff(Mod_cd_Lus)),
           Mod_cd_80 = Mod_cd_Lus*frac_mort) %>%
    filter(Week_gr != 1) %>%
    mutate(Week_gr = Week_gr-1) %>%
    merge(FullData)

  # Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus %>%
    # arrange(Age_gr) %>% select(Week_gr, Age_gr, Mod_cd_Lus) %>%
    # rename(Mod_cd_Lus_b = "Mod_cd_Lus")

  # merge(Mod_Deaths_Age %>% select(Week_gr, Age_gr, Mod_cd_Lus), Mod_Age_Deaths_Lus)

  LL_Distributions <- lapply(1:length(fit_Model$pmcmc_results$inputs$pars_obs$combined_data$dfj_mcmc_data), function(y){

    tmp_df <- merge(Mod_Age_Deaths_Lus, fit_Model$pmcmc_results$inputs$pars_obs$combined_data$dfj_mcmc_data[[y]])
    tmp_df$Mod_cd_UTH <- tmp_df$Mod_cd_80*tmp_df$ag1std
    tmp_df$Mod_tot_ds_Mort <- tmp_df$Mod_cd_UTH + tmp_df$Mort_ncd_mcmc
    tmp_df$ll_pois <- dpois(x = tmp_df$Mort_deaths, lambda = tmp_df$Mod_tot_ds_Mort, log = T)

    tmp_df$Mod_ncd_UTH <- ifelse((tmp_df$Mort_deaths - tmp_df$Mod_cd_UTH)<0,0,(tmp_df$Mort_deaths - tmp_df$Mod_cd_UTH))
    tmp_df$Mod_pos_ds_UTH <- tmp_df$Mod_cd_UTH+tmp_df$Mod_ncd_UTH*tmp_df$pcr_perc # total positive deaths (covid and coincidental)
    tmp_df$Mod_tot_ds_UTH_unstd <- tmp_df$Mod_tot_ds_Mort/tmp_df$ag1std
    tmp_df$Pos_prev <- ifelse(tmp_df$Mod_pos_ds_UTH<=tmp_df$Mort_deaths,tmp_df$Mod_pos_ds_UTH/tmp_df$Mort_deaths, 0.999)
    tmp_df$ll_binom <- dbinom(tmp_df$PosTests, tmp_df$Samples, prob = tmp_df$Pos_prev)
    tmp_df <- tmp_df[c("Week_gr", "Age_gr","Replicate", "ll_pois", "ll_binom", "Mod_cd_Lus","infs","pcr_perc","Mod_cd_80","Mod_cd_UTH","Mod_tot_ds_Mort","Mod_tot_ds_UTH_unstd", "Mod_pos_ds_UTH","Mort_deaths","Pos_prev")]

  })


  str2str::ld2a(LL_Distributions) %>%
    pivot_longer() %>%
    group_by(Age_gr, Week_gr, Replicate) %>%
    summarise(av_ll_pois = mean(ll_pois),
              av_ll_bin = mean(ll_bin)
              )

  str2str::ld2a(LL_Distributions) %>%
    group_by(Replicate)

  exp(Brobdingnag::as.brob(sum(LL_Distributions[[1]]$ll_pois)))

  ##############################################################################
  ##############################################################################


  # LL_Distributions[[1]]

  ll_aw <- apply(str2str::ld2a(LL_Distributions), 2, rowMeans)
  ll_aw <- merge(ll_aw,fit_Model$pmcmc_results$inputs$pars_obs$combined_data$Comb_data)
  # ll_aw <- sum(dbinom(ll_aw$PosTests, ll_aw$Samples, prob = ll_aw$Pos_prev, log = T), log(ll_aw$ll_pois))

  ll_aw <- ll_aw %>% group_by(Replicate, Age_gr,Week_gr) %>%
    mutate(ll_bin = dbinom(PosTests, Samples, prob = Pos_prev)) %>%
    group_by(Age_gr,Week_gr) %>%

    # filter(Replicate==1) %>%
    # mutate(ll_pois = log(ll_pois),
           # ll_sum = ll_pois+ll_bin) %>% pull(ll_sum) %>% sum()

    summarise_at(c("ll_bin", "ll_pois", "Mod_cd_Lus", "Mod_pos_ds_UTH", "Mod_cd_80","Mod_cd_UTH","Mod_tot_ds_Mort","Mod_tot_ds_UTH_unstd","Mort_deaths","pcr_perc","infs","Pos_prev"), list(mean = mean, min = min, max = max), na.rm = TRUE) %>%
    mutate(ll_bin_mean = log(ll_bin_mean),
           ll_bin_min = log(ll_bin_min),
           ll_bin_max = log(ll_bin_max),
           ll_pois_mean = log(ll_pois_mean),
           ll_pois_min = log(ll_pois_min),
           ll_pois_max = log(ll_pois_max)) %>%
    merge(unique(Mod_Age_Deaths_Lus %>% select(Week_gr, date)))

  ll_aw %>% select(date, Age_gr, Week_gr, infs_mean, infs_max, infs_min)
  Pop_str <- readRDS(file = "analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
  Pop_str_df <- data.frame(Age_gr = 1:17, Pop_str = Pop_str)

  Attack_Rates <- merge(ll_aw, Pop_str_df) %>%
    mutate(attack_rate = infs_mean/Pop_str)

  ggplot(Attack_Rates, aes(x = date, y = attack_rate)) + geom_point() +
    facet_wrap(~Age_gr)


  Mod_Age_Deaths_Lus_Av_Week <- ll_aw %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
    merge(FullData) %>%
    group_by(Week_gr) %>%
    select(-date, -Age_gr) %>%
    summarise_all(sum, na.rm=T) %>%
    merge(unique(ll_aw[c("Week_gr","date")]))# %>%

  Mod_Age_Deaths_Lus_Av_Age <- ll_aw %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
    merge(FullData) %>%
    group_by(Age_gr) %>%
    select(-date, -Week_gr) %>%
    summarise_all(sum)


  ##############################################################################
  ##############################################################################




  ################################################
  ################################################
  sero_pcr <- seroprev_df(fit_Model)
  pcr_sero_data <- Summ_sero_pcr_data(sero_pcr)

  sero_pcr_ll <- sero_pcr %>% filter(as.Date(date) %in% seq.Date(from = as.Date("2020-07-04"), to = as.Date("2020-07-27"), by = 1)) %>%
    group_by(replicate) %>%
    summarise(pcr_perc = mean(pcr_perc),
              sero_perc = mean(sero_perc)) %>% ungroup() %>%
    mutate(ll_pcr = dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_perc),
           ll_sero = dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_perc)) %>%
    summarise(ll_pcr = log(mean(ll_pcr)),
              ll_sero = log(mean(ll_sero)))

  comb_ll <- sero_pcr %>% filter(as.Date(date) %in% seq.Date(from = as.Date("2020-07-04"), to = as.Date("2020-07-19"), by = 1)) %>%
    select(date, combined_perc, replicate) %>%
    group_by(replicate) %>%
    summarise(combined_perc = mean(combined_perc)) %>% ungroup() %>%
    mutate(ll_combined = dbinom(x = as.integer(0.091*332), size = as.integer(332), prob = combined_perc)) %>%
    summarise(ll_combined = log(mean(ll_combined)))
  ################################################
  ################################################



  lls <- list(
    ll_bin = sum(ll_aw$ll_bin_mean, na.rm = T),
    ll_pois = sum(ll_aw$ll_pois_mean, na.rm = T),
    ll_pcr = sero_pcr_ll$ll_pcr,
    ll_sero = sero_pcr_ll$ll_sero,
    ll_comb = comb_ll$ll_combined,
    ll_tot = sum(ll_aw$ll_bin_mean, ll_aw$ll_pois_mean, ll_aw$ll_comb_mean))



  # return(lls)

  ##############################################################################
  ##############################################################################

  ### Plots

  Age_groups.labs <- ll_aw %>% group_by(Age_gr) %>%
    summarise(ll_pois_mean = sum(ll_pois_mean)) %>%
    mutate(lab = paste0("pois ll AG ",Age_gr,": ", round(ll_pois_mean,1))) %>%
    mutate(lab = case_when(
      Age_gr==1 ~ paste0("Age: 0-4"),
      Age_gr==2 ~ paste0("Age: 5-9"),
      Age_gr==3 ~ paste0("Age: 10-14"),
      Age_gr==4 ~ paste0("Age: 15-19"),
      Age_gr==5 ~ paste0("Age: 20-24"),
      Age_gr==6 ~ paste0("Age: 25-29"),
      Age_gr==7 ~ paste0("Age: 30-34"),
      Age_gr==8 ~ paste0("Age: 35-39"),
      Age_gr==9 ~ paste0("Age: 40-44"),
      Age_gr==10 ~ paste0("Age: 45-49"),
      Age_gr==11 ~ paste0("Age: 50-54"),
      Age_gr==12 ~ paste0("Age: 55-59"),
      Age_gr==13 ~ paste0("Age: 60-64"),
      Age_gr==14 ~ paste0("Age: 65-69"),
      Age_gr==15 ~ paste0("Age: 70-74"),
      Age_gr==16 ~ paste0("Age: 75-79"),
      Age_gr==17 ~ paste0("Age: 80+"))) %>%
    select(lab) %>%
    unlist()
  names(Age_groups.labs) <- 1:17
#
  Poisson_Figure_uf <- ggplot(ll_aw, aes(x = Week_gr)) +
    # Modelled Positive deaths
    # geom_ribbon(aes(ymin=Mod_tot_ds_Mort_min, ymax=Mod_tot_ds_Mort_max), alpha=0.3) +
    facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs)) +
    # Modelled True deaths
    # geom_line(aes(y=Mod_tot_ds_UTH_unstd_mean, color = "Model fit (unstandardised)"),linetype="dashed", alpha = 0.5) +
    # geom_ribbon(aes(ymin=Mod_tot_ds_UTH_unstd_min, ymax=Mod_tot_ds_UTH_unstd_max), alpha=0.1) +
    # Scaled positive deaths
    geom_point(aes(y = Mort_deaths_mean, color = "Mortuary deaths")) +
    # Scaled True deaths
    xlab("Date") + ylab("Deaths") +
    ggtitle(paste0("COVID-19 weekly deaths (Mortuary)\n\n")) +#IFR x ", round(IFR_x,2),", Slope x ", round(IFR_slope,2),"\npois ll = ", round(sum(ll_aw$ll_pois_mean, na.rm = T),1))) +
    geom_point(data = ll_aw %>% filter(Week_gr %in% c(4,5)), aes(x = Week_gr, y = Mort_deaths_mean, color = "Questionable data points")) +
    theme(plot.title = element_text(size = 10),
          legend.position = c(1,0), legend.justification = c(1,0),
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name=NULL,
                       breaks=c("Mortuary deaths", "Questionable data points"),
                       values = c("black","red"),
                       # values=c('black', 'red'),
                       guide = guide_legend(override.aes = list(linetype = c(0,0),
                                                                shape = c(16,16),
                                                                alpha = c(1,1)))) +
    theme_minimal()
    # ggpubr::theme_pubr(legend = "bottom")
#

  Age_groups.labs <- ll_aw %>% group_by(Age_gr) %>%
    summarise(ll_pois_mean = sum(ll_pois_mean)) %>%
    mutate(lab = paste0("pois ll AG ",Age_gr,": ", round(ll_pois_mean,1))) %>%
    mutate(lab = case_when(
      Age_gr==1 ~ paste0("Age: 0-4; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==2 ~ paste0("Age: 5-9; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==3 ~ paste0("Age: 10-14; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==4 ~ paste0("Age: 15-19; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==5 ~ paste0("Age: 20-24; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==6 ~ paste0("Age: 25-29; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==7 ~ paste0("Age: 30-34; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==8 ~ paste0("Age: 35-39; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==9 ~ paste0("Age: 40-44; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==10 ~ paste0("Age: 45-49; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==11 ~ paste0("Age: 50-54; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==12 ~ paste0("Age: 55-59; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==13 ~ paste0("Age: 60-64; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==14 ~ paste0("Age: 65-69; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==15 ~ paste0("Age: 70-74; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==16 ~ paste0("Age: 75-79; Pois ll = ",round(ll_pois_mean,1)),
      Age_gr==17 ~ paste0("Age: 80+; Pois ll = ",round(ll_pois_mean,1)))) %>%
    select(lab) %>%
    unlist()
  names(Age_groups.labs) <- 1:17

  Age_groups.labs <-  ll_aw %>% group_by(Age_gr) %>% select(Age_gr) %>% unique() %>%
    mutate(Age_gr_lab = ifelse(Age_gr %in% 1:16, paste0("Age: ",Age_gr*5-5,"-",Age_gr*5-1),
                               "Age: 80+")) %>%
    ungroup() %>% arrange(Age_gr) %>% select(Age_gr_lab) %>%
    unlist()
  names(Age_groups.labs) <- 1:17
#
#


  Poisson_Figure <- ggplot(ll_aw, aes(x = Week_gr)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mod_tot_ds_Mort_mean, color = "Model fit")) +
    geom_ribbon(aes(ymin=Mod_tot_ds_Mort_min, ymax=Mod_tot_ds_Mort_max), alpha=0.3) +
    facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs)) +
    # Modelled True deaths
    geom_line(aes(y=Mod_tot_ds_UTH_unstd_mean, color = "Model fit (unstandardised)"),linetype="dashed", alpha = 0.5) +
    geom_ribbon(aes(ymin=Mod_tot_ds_UTH_unstd_min, ymax=Mod_tot_ds_UTH_unstd_max), alpha=0.1) +
    # Scaled positive deaths
    geom_point(aes(y = Mort_deaths_mean, color = "Mortuary deaths")) +
    # Scaled True deaths
    xlab("Date") + ylab("Deaths") +
    ggtitle(paste0("COVID-19 weekly deaths (Mortuary)\nIFR x ", round(IFR_x,2),", Slope x ", round(IFR_slope,2),"\npois ll = ", round(sum(ll_aw$ll_pois_mean, na.rm = T),1))) +
    geom_point(data = ll_aw %>% filter(Week_gr %in% c(4,5)), aes(x = Week_gr, y = Mort_deaths_mean, color = "Questionable data points")) +
    theme(plot.title = element_text(size = 10),
          legend.position = c(1,0), legend.justification = c(1,0),
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name=NULL,
                       breaks=c("Mortuary deaths", "Questionable data points", "Model fit", "Model fit (unstandardised)"),
                       values = c("black","red","black","black"),
                       # values=c('black', 'red'),
                       guide = guide_legend(override.aes = list(linetype = c(0,0,1,2),
                                                                shape = c(16,16,NA,NA),
                                                                alpha = c(1,1,1,0.5)))) +
    theme_minimal()


  Poisson_Figure <- ggplot(ll_aw, aes(x = date)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mod_tot_ds_Mort_mean, color = "Model fit")) +
    geom_ribbon(aes(ymin=Mod_tot_ds_Mort_min, ymax=Mod_tot_ds_Mort_max), alpha=0.3) +
    facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs)) +
    # Modelled True deaths
    geom_line(aes(y=Mod_tot_ds_UTH_unstd_mean, color = "Model fit (unstandardised)"),linetype="dashed", alpha = 0.5) +
    geom_ribbon(aes(ymin=Mod_tot_ds_UTH_unstd_min, ymax=Mod_tot_ds_UTH_unstd_max), alpha=0.1) +
    # Scaled positive deaths
    geom_point(aes(y = Mort_deaths_mean, color = "Mortuary deaths")) +
    # Scaled True deaths
    xlab("Date") + ylab("Deaths") +
    ggtitle(paste0("COVID-19 weekly deaths (Mortuary)")) +
    geom_point(data = ll_aw %>% filter(Week_gr %in% c(4,5)), aes(x = date, y = Mort_deaths_mean, color = "Mortuary surveillance disrupted?")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10),
          legend.position = c(1,0), legend.justification = c(1,0),
          legend.key = element_rect(fill = "white", linetype = 0)) +
    scale_color_manual(name=NULL,
                       breaks=c("Mortuary deaths", "Mortuary surveillance disrupted?", "Model fit", "Model fit (unstandardised)"),
                       values = c("black","red","black","black"),
                       # values=c('black', 'red'),
                       guide = guide_legend(override.aes = list(linetype = c(0,0,1,2),
                                                                shape = c(16,16,NA,NA),
                                                                alpha = c(1,1,1,0.5))))


  pdf("analysis/figures/42_Mortality_by_age.pdf")
  Poisson_Figure
  dev.off()

  tiff("analysis/figures/42_Mortality_by_age.tiff", res = 300, height = 7, width = 7, units = "in")
  Poisson_Figure
  dev.off()

    # ggpubr::theme_pubr(legend = "bottom")
#
#   ### Binomial figures
#
#
#   ## Unfitted data:
  Mod_Age_Deaths_Lus_Av_Week <- Mod_Age_Deaths_Lus_Av_Week %>% mutate(Unreliable_points_key = ifelse(Week_gr %in% 4:5, "Questionable data points", "Prevalence"))
  Week_prev_plot_uf <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date))+#, col = as.factor(Unreliable_points_key))) +
    # Prevalence on positive in sample
    geom_point(aes(y = 100*PosTests/Samples)) +
    geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
                      ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
    # xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    # xlim(as.Date("2020-06-10"),as.Date("2020-10-01")) +
    xlab("Date") + ylab("+ve %")  +
    ggtitle("COVID-19 weekly prevalence\nUTH mortuary") +
    scale_color_manual(name = NULL, values = c("black","red")) +
    theme(plot.title = element_text(size = 10),legend.position = "bottom") +

    # geom_point(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)), aes(x = date, y = PosTests/Samples), col = "red") +
    # geom_errorbar(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)),
                  # aes(ymin = Hmisc::binconf(PosTests,Samples)[,"Lower"],
                      # ymax = Hmisc::binconf(PosTests,Samples)[,"Upper"]), col = "red") +
    theme_minimal()
    # ggpubr::theme_pubr(legend = "bottom")
#

  Mod_Age_Deaths_Lus_Av_Age <- Mod_Age_Deaths_Lus_Av_Age %>% mutate(Age_gr_label = ifelse(Age_gr %in% 1:16,
                                                                                          paste0(Age_gr*5-5, "-",Age_gr*5-1),
                                                                                          "80+"))

  Age_prev_plot_uf <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr_label)) +
    # Prevalence on positive in sample
    geom_point(aes(y = 100*PosTests/Samples)) +
    geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
                      ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
    xlab("Age") + ylab("+ve %")  +
    ggtitle("COVID-19 prevalence by age\nUTH mortuary") +
    theme(plot.title = element_text(size = 10)) +
    theme_minimal() +
    scale_x_discrete(limits = c(paste0(1:16*5-5, "-",1:16*5-1),"80+")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
    # ggpubr::theme_pubr(legend = "bottom")

  pdf("analysis/figures/42_Prevalence_Data_BMJ.pdf", width = 10)
    cowplot::plot_grid(Week_prev_plot_uf + theme(legend.position = "none"), Age_prev_plot_uf, nrow = 1)
  dev.off()

  tiff("analysis/figures/42_Prevalence_Data_BMJ.tiff", width = 10, units = "in", height = 7, res = 300)
  cowplot::plot_grid(Week_prev_plot_uf + theme(legend.position = "none"), Age_prev_plot_uf, nrow = 1)
  dev.off()



  PCR_sero_comb_prev_plot_uf <- ggplot(pcr_sero_data, aes(x = date, y = mean_pcr)) +
    geom_point(aes(x= as.Date("2020-07-15"),y=7.6, color = "PCR % (Zambia)"), size = 2) +
    geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10,color = "PCR % (Zambia)")) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0,color = "PCR % (Zambia)")) +

    geom_point(aes(x= as.Date("2020-07-15"),y=2.1, color = "Sero % (Zambia)"), size = 2) +
    geom_errorbar(aes(ymin=1.1,ymax=3.1,x=as.Date("2020-07-15"), width=10, color = "Sero % (Zambia)")) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0, color = "Sero % (Zambia)")) +

    geom_point(aes(x= as.Date("2020-07-11"),y=9.1, color = "Combined % (Lusaka)"), size = 2) +
    geom_errorbar(aes(ymin=2.6,ymax=15.7,x=as.Date("2020-07-11"), width=10, color = "Combined % (Lusaka)")) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-19"),y=9.1, height=0, color = "Combined % (Lusaka)")) +

    ylab(paste0("+ve %")) +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) +# coord_cartesian(ylim=c(0, 15)) +
    ylim(0, 28) +
    xlab("Date")+
    theme_minimal() +
    # ggpubr::theme_pubr(legend = "bottom") +
    ggtitle(paste("COVID-19 prevalence\nLusaka/Zambia")) + #PCR binom ll =", round(sero_pcr_ll$ll_pcr,1),
                  # "\nSero binom ll =", round(sero_pcr_ll$ll_sero,1),
                  # "\nCombined binom ll =", round(comb_ll$ll_combined,1))) +
    # theme(plot.title = element_text(size = 10)) +
    scale_color_manual(name=NULL,
                       breaks=c("Combined % (Lusaka)","PCR % (Zambia)", "Sero % (Zambia)"),
                       values = c("black","darkgoldenrod1","chartreuse4"))
#
  # Prev_plot_uf <- cowplot::plot_grid(cowplot::plot_grid(Week_prev_plot_uf + theme(legend.position="none"), Age_prev_plot_uf, PCR_sero_comb_prev_plot_uf + theme(legend.position="none"), nrow = 1),
                                     # cowplot::plot_grid(get_legend(Week_prev_plot_uf + theme(legend.position = "bottom")), get_legend(PCR_sero_comb_prev_plot_uf), nrow = 1, rel_widths = c(2,1)),ncol = 1, rel_heights = c(1,0.2))
  Prev_plot_uf <- cowplot::plot_grid(Week_prev_plot_uf, Age_prev_plot_uf, PCR_sero_comb_prev_plot_uf + theme(legend.position="none"),
                                     NULL,NULL, ggpubr::get_legend(PCR_sero_comb_prev_plot_uf), ncol = 3, rel_heights = c(1,0.2))

#
#
#   # ## Graph 2:
  Week_prev_plot <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Prevalence on positive in sample
    geom_point(aes(y = 100*PosTests/Samples)) +#, col = as.factor(Unreliable_points_key))) +
    geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
                      ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
    geom_line(aes(y = 100*Mod_pos_ds_UTH_mean/Mort_deaths, col = "Total +ve deaths"),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Mod_pos_ds_UTH_min/Mort_deaths, ymax=100*Mod_pos_ds_UTH_max/Mort_deaths), alpha=0.3) +
    geom_line(aes(y = 100*Mod_cd_UTH_mean/Mort_deaths,col = "Causal deaths"),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Mod_cd_UTH_min/Mort_deaths, ymax=100*Mod_cd_UTH_max/Mort_deaths), alpha=0.3, fill = "darkred") +
    geom_line(aes(y = 100*(Mod_pos_ds_UTH_mean-Mod_cd_UTH_mean)/Mort_deaths, col = "Coincidental deaths"),linetype="dashed") +
    geom_ribbon(aes(ymin=100*(Mod_pos_ds_UTH_min-Mod_cd_UTH_min)/Mort_deaths, ymax=100*(Mod_pos_ds_UTH_max-Mod_cd_UTH_max)/Mort_deaths), alpha=0.3, fill = "darkblue") +
    # xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    xlab("Date") + ylab("+ve %")  +
    ggtitle("COVID-19 weekly prevalence\nUTH mortuary") +
    theme(plot.title = element_text(size = 10)) +
    # geom_point(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)), aes(x = date, y = PosTests/Samples), col = "red") +
    # geom_errorbar(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)),
                  # aes(ymin = Hmisc::binconf(PosTests,Samples)[,"Lower"],
                      # ymax = Hmisc::binconf(PosTests,Samples)[,"Upper"]), col = "red") +
    theme_minimal() +
      scale_color_manual(name = NULL, breaks = c("Total +ve deaths","Causal deaths","Coincidental deaths"), values = c("black","darkred","darkblue"))
    # ggpubr::theme_pubr(legend = "bottom")

  Age_prev_plot <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr_label)) +
    # Prevalence on positive in sample
    geom_point(aes(y = 100*PosTests/Samples)) +
    geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
                      ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
    geom_line(aes(y = 100*Mod_pos_ds_UTH_mean/Mort_deaths, col = "Total +ve deaths", group = 1),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Mod_pos_ds_UTH_min/Mort_deaths, ymax=100*Mod_pos_ds_UTH_max/Mort_deaths, group = 1), alpha=0.3) +
    geom_line(aes(y = 100*Mod_cd_UTH_mean/Mort_deaths,col = "Causal deaths", group = 1),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Mod_cd_UTH_min/Mort_deaths, ymax=100*Mod_cd_UTH_max/Mort_deaths, group = 1), alpha=0.3, fill = "darkred") +
    geom_line(aes(y = 100*(Mod_pos_ds_UTH_mean-Mod_cd_UTH_mean)/Mort_deaths, col = "Coincidental deaths", group = 1),linetype="dashed") +
    geom_ribbon(aes(ymin=100*(Mod_pos_ds_UTH_min-Mod_cd_UTH_min)/Mort_deaths, ymax=100*(Mod_pos_ds_UTH_max-Mod_cd_UTH_max)/Mort_deaths, group = 1), alpha=0.3, fill = "darkblue") +
    xlab("Age") + ylab("+ve %")  +
    ggtitle("COVID-19 prevalence by age\nUTH mortuary") +
    theme(plot.title = element_text(size = 10)) +
    theme_minimal() +
    scale_color_manual(name = NULL, breaks = c("Total +ve deaths","Causal deaths","Coincidental deaths"), values = c("black","darkred","darkblue")) +
    scale_x_discrete(limits = c(paste0(1:16*5-5, "-",1:16*5-1),"80+")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))


    # ggpubr::theme_pubr(legend = "bottom")
#
#
#
  PCR_sero_comb_prev_plot <- ggplot(pcr_sero_data, aes(x = date, y = mean_pcr)) +
    geom_line(aes(x=date, y=mean_pcr, color = "PCR % (Zambia)"),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3, fill = "darkgoldenrod1")+
    geom_point(aes(x= as.Date("2020-07-15"),y=7.6, color = "PCR % (Zambia)"), size = 2) +
    geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10,color = "PCR % (Zambia)")) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0,color = "PCR % (Zambia)")) +

    geom_line(aes(x=date, y=mean_sero, color = "Sero % (Zambia)"),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3, fill = "chartreuse4")+
    geom_point(aes(x= as.Date("2020-07-15"),y=2.1, color = "Sero % (Zambia)"), size = 2) +
    geom_errorbar(aes(ymin=1.1,ymax=3.1,x=as.Date("2020-07-15"), width=10, color = "Sero % (Zambia)")) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0, color = "Sero % (Zambia)")) +

    geom_line(aes(x=date, y=mean_combined, color = "Combined % (Lusaka)"),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_combined, ymax=max_combined), alpha=0.3, fill = "black")+
    geom_point(aes(x= as.Date("2020-07-11"),y=9.1, color = "Combined % (Lusaka)"), size = 2) +
    geom_errorbar(aes(ymin=2.6,ymax=15.7,x=as.Date("2020-07-11"), width=10, color = "Combined % (Lusaka)")) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-19"),y=9.1, height=0, color = "Combined % (Lusaka)")) +

    ylab(paste0("+ve %")) +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) +# coord_cartesian(ylim=c(0, 15)) +
    ylim(0, 28) +
    xlab("Date")+
    theme_minimal() +
    # ggpubr::theme_pubr(legend = "bottom") +
    ggtitle(paste("COVID-19 prevalence\nLusaka/Zambia")) +#\nPCR binom ll =", round(sero_pcr_ll$ll_pcr,1),
                  # "\nSero binom ll =", round(sero_pcr_ll$ll_sero,1),
                  # "\nCombined binom ll =", round(comb_ll$ll_combined,1)))
            # theme(plot.title = element_text(size = 10)) +
    scale_color_manual(name=NULL,
                       breaks=c("Combined % (Lusaka)","PCR % (Zambia)", "Sero % (Zambia)"),
                       values = c("black","darkgoldenrod1","chartreuse4"))
#
#
# Prev_plot <- cowplot::plot_grid(cowplot::plot_grid(Week_prev_plot + theme(legend.position="none"), Age_prev_plot, PCR_sero_comb_prev_plot + theme(legend.position="none"), nrow = 1),
#                                    cowplot::plot_grid(get_legend(Week_prev_plot + theme(legend.position = "bottom")), get_legend(PCR_sero_comb_prev_plot_uf), nrow = 1, rel_widths = c(2,1)),ncol = 1, rel_heights = c(1,0.2))
  Prev_plot <- cowplot::plot_grid(
    cowplot::plot_grid(Week_prev_plot + theme(legend.position = "none"), Age_prev_plot  + theme(legend.position = "none"), PCR_sero_comb_prev_plot + theme(legend.position="none"), nrow = 1),
    cowplot::plot_grid(ggpubr::get_legend(Week_prev_plot),ggpubr::get_legend(PCR_sero_comb_prev_plot_uf), rel_widths = c(2,1), nrow = 1),
    nrow = 2, rel_heights = c(1,0.2))



pdf("analysis/figures/42_Prevalences_data_only.pdf", width = 10, height = 4.5)
Prev_plot_uf
dev.off()

tiff("analysis/figures/42_Prevalences_data_only.tiff", width = 10, height = 4.5, units = "in", res = 300)
Prev_plot_uf
dev.off()

pdf("analysis/figures/42_Prevalences_data_and_fit.pdf", width = 10, height = 4.5)
Prev_plot
dev.off()

tiff("analysis/figures/42_Prevalences_data_and_fit.tiff", width = 10, height = 4.5, units = "in", res = 300)
Prev_plot
dev.off()


# Poisson_Figure_uf
# Poisson_Figure


# Prev_plot_uf
# Prev_plot

### Plotting excess mortality against the squire model predictions:
Pop_str <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
Pop_str_df <- data.frame(Age_gr = 1:17, Pop_str = Pop_str)
ExcessMort_mcmc <- readRDS("analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week.rds")

Age_groups.labs <- ExcessMort_mcmc %>% ungroup() %>% select(Age_gr) %>% unique() %>%
  mutate(Age_gr_labs = ifelse(Age_gr %in% 1:16,
                              paste0("Age: ", Age_gr*5-5,"-",Age_gr*5-1),
                              "Age: 80+")) %>%
  select(Age_gr_labs) %>%
  unlist()
names(Age_groups.labs) <- 1:17

## Add tot his true covid deaths
Plot_data_Excess_Pred <- ExcessMort_mcmc %>% merge(Pop_str_df) %>% merge(ll_aw) %>% filter(Age_gr!=1) %>%
  mutate(date = lubridate::floor_date(date, "week", 1))
pdf("analysis/figures/42_Excess_mortality_against_squire_predictions.pdf")
p_excess_comp<- ggplot(Plot_data_Excess_Pred, aes(x = date, y = excess_Median/Pop_str*1000)) + geom_line(aes(col = "Excess mortality")) +
  geom_ribbon(aes(ymin = excess_CI_low/Pop_str*1000, ymax = excess_CI_high/Pop_str*1000), alpha = 0.4) +
  facet_wrap(~Age_gr, labeller = labeller(Age_gr = Age_groups.labs)) +
  # geom_line(aes(y = Mod_cd_Lus_mean/Pop_str*1000, col = "Model output (Lusaka)"))  +
  # geom_ribbon(aes(ymin = Mod_cd_Lus_min/Pop_str*1000, ymax = Mod_cd_Lus_max/Pop_str*1000), alpha = 0.4, fill = "darkblue") +
  # geom_line(aes(y = Mod_cd_80_mean/Pop_str*1000), col = "darkred") +
  geom_line(aes(y = Mod_cd_UTH_mean/Pop_str*1000, col = "Modelled true c19 output (UTH)")) +
  geom_ribbon(aes(ymin = Mod_cd_UTH_min/Pop_str*1000, ymax = Mod_cd_UTH_max/Pop_str*1000), alpha = 0.4, fill = "darkred") +
  theme_minimal() +
  theme(legend.position="bottom") +
  ylab("Per capita deaths (/1000)") +
  xlab("Date") +
  scale_color_manual(name = NULL, breaks = c("Excess mortality","Model output (Lusaka)", "Modelled true c19 output (UTH)"), values = c("black", "darkblue", "darkred"))
dev.off()

tiff("analysis/figures/42_Excess_mortality_against_squire_predictions.tiff", width = 7, height = 7, units = "in", res = 300)
p_excess_comp
dev.off()


}




IFR_df <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds") %>%
  filter(IFR_x == 1 & Slope_x ==1 |
           IFR_x == 0.2 & Slope_x ==1 |
           IFR_x == 5 & Slope_x ==1|
           IFR_x == 1 & Slope_x ==0.2|
           IFR_x == 1 & Slope_x ==2.5)

curve(expr = IFR_df$Int_abs[4] + IFR_df$Slope_abs[4]*x, from = 0, 100)
curve(expr = IFR_df$Int_abs[2] + IFR_df$Slope_abs[2]*x, from = 0, 100, add = T)
curve(expr = IFR_df$Int_abs[3] + IFR_df$Slope_abs[3]*x, from = 0, 100, add = T)

IFR_df$tmp_var <- "Varying intercept (log10 scale)"
IFR_df$tmp_var2 <- "Varying intercept"
IFR_df$tmp_var3 <- "Varying slope (log10 scale)"
IFR_df$tmp_var4 <- "Varying slope"

p1_IFR <- ggplot(IFR_df) + xlim(0,90) + geom_function(fun = function(x) exp(IFR_df$Int_abs[2] + IFR_df$Slope_abs[2]*x), linetype = 2) +
  geom_function(fun = function(x) exp(IFR_df$Int_abs[3] + IFR_df$Slope_abs[3]*x)) +
  geom_function(fun = function(x) exp(IFR_df$Int_abs[4] + IFR_df$Slope_abs[4]*x), linetype = 2) +
  theme_minimal() + xlab("Age") + ylab("IFR") +
  scale_y_continuous(trans='log10')+
  # ggtitle("Varying intercept (log10 scale)") +
  geom_segment(aes(x = 89, y = 13, xend = 89, yend = 45),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.4) +
  geom_segment(aes(x = 89, y = 9, xend = 89, yend = 3),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.4) + facet_grid(. ~ tmp_var)

p2_IFR <- ggplot(IFR_df) + xlim(0,90) + geom_function(fun = function(x) exp(IFR_df$Int_abs[2] + IFR_df$Slope_abs[2]*x), linetype = 2) +
  geom_function(fun = function(x) exp(IFR_df$Int_abs[3] + IFR_df$Slope_abs[3]*x)) +
  geom_function(fun = function(x) exp(IFR_df$Int_abs[4] + IFR_df$Slope_abs[4]*x), linetype = 2) +
  theme_minimal() + xlab("Age") + ylab("IFR")+
  # ggtitle("Varying intercept")+
  geom_segment(aes(x = 89, y = 13, xend = 89, yend = 45),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.4) +
  geom_segment(aes(x = 89, y = 9, xend = 89, yend = 3),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.4) + facet_grid(. ~ tmp_var2)

p3_IFR <- ggplot(IFR_df) + xlim(0,90) + geom_function(fun = function(x) exp(IFR_df$Int_abs[1] + IFR_df$Slope_abs[1]*x), linetype = 2) +
  geom_function(fun = function(x) exp(IFR_df$Int_abs[3] + IFR_df$Slope_abs[3]*x)) +
  geom_function(fun = function(x) exp(IFR_df$Int_abs[5] + IFR_df$Slope_abs[5]*x), linetype = 2) +
  theme_minimal() + xlab("Age") + ylab("IFR")+
  scale_y_continuous(trans='log10')+
  # ggtitle("Varying slope (log10 scale)")+
  geom_segment(aes(x = 89, y = 13, xend = 89, yend = 80),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.4) +
  geom_segment(aes(x = 89, y = 9, xend = 89, yend = 0.5),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.4) + facet_grid(. ~ tmp_var3)

p4_IFR <- ggplot(IFR_df) + xlim(0,90) + geom_function(fun = function(x) exp(IFR_df$Int_abs[1] + IFR_df$Slope_abs[1]*x), linetype = 2) +
  geom_function(fun = function(x) exp(IFR_df$Int_abs[3] + IFR_df$Slope_abs[3]*x)) +
  geom_function(fun = function(x) exp(IFR_df$Int_abs[5] + IFR_df$Slope_abs[5]*x), linetype = 2) +
  theme_minimal() + xlab("Age") + ylab("IFR")+
  # ggtitle("Varying slope")+
  geom_segment(aes(x = 89, y = 13, xend = 89, yend = 80),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.4) +
  geom_segment(aes(x = 89, y = 9, xend = 89, yend = 1),
               arrow = arrow(length = unit(0.3, "cm")), size = 0.4) + facet_grid(. ~ tmp_var4)

pdf("analysis/figures/42_Varying_IFR_info.pdf")
cowplot::plot_grid(p2_IFR, p4_IFR, p1_IFR, p3_IFR)
dev.off()

tiff("analysis/figures/42_Varying_IFR_info.tiff", width = 7, height = 7, units = "in", res = 300)
cowplot::plot_grid(p2_IFR, p4_IFR, p1_IFR, p3_IFR)
dev.off()
