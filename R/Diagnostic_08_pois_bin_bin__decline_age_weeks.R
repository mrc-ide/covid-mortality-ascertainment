Diagnostic_Plot_08 <- function(fit_num, fit_Model_list, IFRvals, Deaths_by_week = T, Deaths_by_Age = T, Mulenga_Prev=T){

  fit_Model <- fit_Model_list[[fit_num]]
  # lld <- fit_Model$pmcmc_results$inputs$pars_obs$lld

  ##################################################################
  ##################################################################
  # Set 1: deaths by time
  ##################################################################
  ##################################################################
pcr_det <- fit_Model$pmcmc_results$inputs$pars_obs$pcr_det
  comb_det <- fit_Model$pmcmc_results$inputs$pars_obs$comb_det
  index <- squire:::odin_index(fit_Model$model)
  Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                   to = as.Date("2020-10-02"),
                                   by = "week"),as.Date("2020-10-02"))
  frac_mort <- 0.8

  FullData <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")
  FullData <- merge(FullData, data.frame(Age_gr = 1:17, Bg_dr = as.vector(colMeans(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")))*12/52))


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
  mutate(infs = c(0, as.integer(diff(max(Sus, na.rm = T)-Sus))),
         pcr_pos = roll_func(infs, pcr_det),
         pcr_perc = pcr_pos/max(Sus,na.rm = T))

Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus_pcr %>%
  filter(date > "2020-06-10") %>%
  mutate(Week_gr = cut.Date(date, breaks = "1 week", labels = 0:16)) %>%
  group_by(Age_gr, Week_gr, Replicate) %>%
  summarise(Mod_cd_Lus = max(Mod_cd_Lus),
            pcr_perc = pcr_perc[4]) %>%
  ungroup() %>% group_by(Age_gr, Replicate) %>%
  mutate(Mod_cd_Lus = c(0,diff(Mod_cd_Lus))) %>%
  filter(Week_gr != 0) %>%
  merge(FullData) %>%
  # group_by(Age_gr, Week_gr, Replicate) %>%
  mutate(Mod_cd_UTH = Mod_cd_Lus*frac_mort,
         Mod_ncd_UTH = Mort_deaths - Mod_cd_UTH,
         Mod_ncd_UTH = ifelse(Mod_ncd_UTH<0,0,Mod_ncd_UTH),
         Mod_pos_ncd_Lus = Mod_ncd_UTH*pcr_perc/frac_mort,
         Mod_pos_ds_Lus = Mod_pos_ncd_Lus + Mod_cd_Lus,
         Mod_tot_ds_Mort = Mod_cd_Lus*frac_mort + Bg_dr)


Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus %>% group_by(Week_gr, Replicate) %>%
  dplyr::filter(Age_gr ==1) %>% summarise(ag1std = Mort_deaths/(Mod_cd_Lus*frac_mort + Bg_dr)) %>%
  merge(Mod_Age_Deaths_Lus) %>%
  mutate(Pos_prev = ifelse(Mod_pos_ds_Lus/Mort_deaths>=1, 0.999999, Mod_pos_ds_Lus/Mort_deaths),
         Mod_tot_ds_Mort_sc = Mod_tot_ds_Mort*ag1std) %>%
  filter(Week_gr != 4) %>%
  mutate(ll_bin = dbinom(x = PosTests, size = Samples, prob = Pos_prev, log = T),
         ll_pois = dpois(x = Mort_deaths, lambda = Mod_tot_ds_Mort_sc, log = T)) %>%
  group_by(Age_gr, Week_gr, date,Mort_deaths,Samples,PosTests) %>%
  summarise_at(c("ll_bin", "ll_pois", "Mod_cd_Lus","Mod_pos_ds_Lus","Mod_pos_ncd_Lus","pcr_perc","Mod_tot_ds_Mort","Mod_tot_ds_Mort_sc"), list(mean = mean, min = min, max = max), na.rm = TRUE)


  ## Plot by week
  Mod_Age_Deaths_Lus_Av_Week <- Mod_Age_Deaths_Lus_Av %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
    group_by(Week_gr) %>%
    select(-date, -Age_gr) %>%
    summarise_all(sum) %>%
    merge(unique(Mod_Age_Deaths_Lus_Av[c("Week_gr","date")]))

  p1 <- ggplot(Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mod_pos_ds_Lus_mean),linetype="dashed") +
    geom_ribbon(aes(ymin=Mod_pos_ds_Lus_min, ymax=Mod_pos_ds_Lus_max), alpha=0.3) +
    # Modelled True deaths
    geom_line(aes(y=Mod_cd_Lus_mean),linetype="dashed") +
    geom_ribbon(aes(ymin=Mod_cd_Lus_min, ymax=Mod_cd_Lus_max), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = (Mort_deaths*PosTests/Samples)/frac_mort)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Date") + ylab("Deaths") +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle(paste("COVID-19 weekly deaths\n(Lusaka) \nbin ll = ", round(sum(Mod_Age_Deaths_Lus_Av$ll_bin_mean),1))) +
    theme(plot.title = element_text(size = 10))
  #
  #
  #
  # ## Graph 2:
  p2 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTests/Samples)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mod_pos_ds_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Mod_pos_ds_Lus_min*frac_mort/Mort_deaths, ymax=Mod_pos_ds_Lus_max*frac_mort/Mort_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mod_cd_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Mod_cd_Lus_min*frac_mort/Mort_deaths, ymax=Mod_cd_Lus_max*frac_mort/Mort_deaths), alpha=0.3, fill = "red") +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    xlab("Date") + ylab("Prevalence")  +
    ggtitle("COVID-19 weekly prevalence\n(UTH mortuary)") +
    theme(plot.title = element_text(size = 10))
  #
  # ## Of rmy benefit, let's plot at the test level
  p3 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTests)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mod_pos_ds_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Mod_pos_ds_Lus_min*frac_mort/Mort_deaths, ymax=Samples*Mod_pos_ds_Lus_max*frac_mort/Mort_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mod_cd_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Mod_cd_Lus_min*frac_mort/Mort_deaths, ymax=Samples*Mod_cd_Lus_max*frac_mort/Mort_deaths), alpha=0.3, fill = "red") +
    xlab("Date") + ylab("Positive tests")  +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle("COVID-19 weekly positive\ntests (UTH mortuary)") +
    theme(plot.title = element_text(size = 10))


  Mod_Age_Deaths_Lus_Av_Age <- Mod_Age_Deaths_Lus_Av %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
    group_by(Age_gr) %>%
    select(-date, -Week_gr) %>%
    summarise_all(sum) %>%
    merge(unique(Mod_Age_Deaths_Lus_Av[c("Week_gr","date")]))

  p4 <- ggplot(Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mod_pos_ds_Lus_mean),linetype="dashed") +
    geom_ribbon(aes(ymin=Mod_pos_ds_Lus_min, ymax=Mod_pos_ds_Lus_max), alpha=0.3) +
    # Modelled True deaths
    geom_line(aes(y=Mod_cd_Lus_mean),linetype="dashed") +
    geom_ribbon(aes(ymin=Mod_cd_Lus_min, ymax=Mod_cd_Lus_max), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = (Mort_deaths*PosTests/Samples)/frac_mort)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Age group") + ylab("Deaths") +
    ggtitle(paste("COVID-19 deaths by age\n(Lusaka)\nbin ll =", round(sum(Mod_Age_Deaths_Lus_Av$ll_bin_mean),1))) +
    theme(plot.title = element_text(size = 10))
  #
  #
  #
  # ## Graph 2:
  p5 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTests/Samples)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mod_pos_ds_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Mod_pos_ds_Lus_min*frac_mort/Mort_deaths, ymax=Mod_pos_ds_Lus_max*frac_mort/Mort_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mod_cd_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Mod_cd_Lus_min*frac_mort/Mort_deaths, ymax=Mod_cd_Lus_max*frac_mort/Mort_deaths), alpha=0.3, fill = "red") +
    xlab("Age group") + ylab("Prevalence")  +
    ggtitle("COVID-19 prevalence by age\n(UTH mortuary)") +
    theme(plot.title = element_text(size = 10))
  #
  # ## Of rmy benefit, let's plot at the test level
  p6 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTests)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mod_pos_ds_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Mod_pos_ds_Lus_min*frac_mort/Mort_deaths, ymax=Samples*Mod_pos_ds_Lus_max*frac_mort/Mort_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mod_cd_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Mod_cd_Lus_min*frac_mort/Mort_deaths, ymax=Samples*Mod_cd_Lus_max*frac_mort/Mort_deaths), alpha=0.3, fill = "red") +
    xlab("Age group") + ylab("Positive tests")  +
    ggtitle("COVID-19 positive tests by\nage (UTH mortuary)") +
    theme(plot.title = element_text(size = 10))


  ## Combined, sero, pcr
  sero_pcr <- seroprev_df(fit_Model)
  pcr_sero_data <- Summ_sero_pcr_data(sero_pcr)

  sero_pcr_ll <- sero_pcr %>% filter(as.Date(date) %in% as.Date(c("2020-07-15"))) %>%
    select(pcr_perc, sero_perc) %>%
    mutate(ll_pcr = dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_perc, log = T),
           ll_sero = dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_perc, log = T)
    ) %>%
    summarise(ll_pcr = mean(ll_pcr),
              ll_sero = mean(ll_sero))

  comb_ll <- sero_pcr %>% filter(as.Date(date) %in% as.Date(c("2020-07-19", "2020-07-04","2020-07-11"))) %>%
    select(date,combined_perc, replicate) %>%
    mutate(ll_combined = dbinom(x = as.integer(0.091*332), size = as.integer(332), prob = combined_perc, log = T)
    ) %>% group_by(replicate) %>%
    summarise(ll_combined = mean(ll_combined)) %>% ungroup() %>%
    summarise(ll_combined = mean(ll_combined))


  p7 <- pcr_fit_plot(pcr_sero_data) + ggtitle(paste("PCR prevalence\nbinom ll =", round(sero_pcr_ll$ll_pcr,1))) + theme(plot.title = element_text(size = 10))
  p8 <- sero_fit_plot(pcr_sero_data) + ggtitle(paste("Seroprevalence \nbinom ll =", round(sero_pcr_ll$ll_sero,1))) + theme(plot.title = element_text(size = 10))
  p9 <- combined_fit_plot(pcr_sero_data) + ggtitle(paste("Combined prevalence \nbinom ll =", round(comb_ll$ll_combined,1))) + theme(plot.title = element_text(size = 10))


  title_gg <- ggplot() +
    labs(title = paste0("IFR x ", IFRvals$IFR_x[fit_num],", Slope x ", IFRvals$Slope_x[fit_num]))
  # browser()
  Figure1 <- cowplot::plot_grid(title_gg, cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9), ncol = 1, rel_heights = c(0.05, 1))

  ####################
    Age_groups.labs <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_gr) %>%
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

    Figure2 <- ggplot(Mod_Age_Deaths_Lus_Av, aes(x = date)) +
      # Modelled Positive deaths
      geom_line(aes(y=Mod_tot_ds_Mort_sc_mean)) +
      geom_ribbon(aes(ymin=Mod_tot_ds_Mort_sc_min, ymax=Mod_tot_ds_Mort_sc_max), alpha=0.3) +
      facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs)) +
      # Modelled True deaths
      geom_line(aes(y=Mod_tot_ds_Mort_mean),linetype="dashed", alpha = 0.5) +
      geom_ribbon(aes(ymin=Mod_tot_ds_Mort_min, ymax=Mod_tot_ds_Mort_max), alpha=0.1) +

      # geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
      # geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
      # Scaled positive deaths
      geom_point(aes(y = Mort_deaths)) +
      # Scaled True deaths
      # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
      xlab("Date") + ylab("Deaths") +
      # xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
      ggtitle(paste0("IFR x ", IFRvals$IFR_x[fit_num],", Slope x ", IFRvals$Slope_x[fit_num],"\nCOVID-19 weekly deaths (Mortuary) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$ll_pois_mean),1))) +
      theme(plot.title = element_text(size = 10)) +
      geom_point(data = Mod_Age_Deaths_Lus_Av %>% filter(Week_gr==4), aes(x = date, y = Mort_deaths), col = "red")
# browser()



    Mod_Age_Deaths_Lus_Combined_Prev <- fit_Model$output[,c(index$S),] %>%
      melt(varnames = c("date","var","Replicate"), value.name = "Sus") %>%
      mutate(Age_gr = as.numeric(gsub(".*?([0-9]+).*", '\\1', var)),
             var = substr(var, 1, 1),
             date = as.Date(date)) %>%
      # dcast(... ~ var, value.var="value") %>%
      # rename(Sus = "S") %>%
      mutate(Age_gr_10 = ifelse(Age_gr>=15, 8, ceiling(Age_gr/2))) %>%
      group_by(Age_gr, Replicate) %>%
      # replace_na(list(Mod_cd_Lus = 0)) %>%
      mutate(Sus = ifelse(is.na(Sus), max(Sus, na.rm=T), Sus)) %>%
      mutate(infs = c(0, as.integer(diff(max(Sus, na.rm = T)-Sus))),
             comb_pos = roll_func(infs, comb_det),
             comb_perc = comb_pos/max(Sus,na.rm = T))


    Comb_prev_df <- Mod_Age_Deaths_Lus_Combined_Prev %>% group_by(date, Age_gr_10) %>%
      summarise(comb_perc_mean = mean(comb_perc),
                comb_perc_min = min(comb_perc),
                comb_perc_max = max(comb_perc))


Age_groups.labs_comb_prev <- Comb_prev_df %>% ungroup() %>% select(Age_gr_10) %>%
  unique() %>%
  # summarise(ll_pois_mean = sum(ll_pois_mean)) %>%
  # mutate(lab = paste0("pois ll AG ",Age_gr_10,": ", round(ll_pois_mean,1))) %>%
  mutate(lab = case_when(
    Age_gr_10==1 ~ paste0("Age: 0-9"),
    Age_gr_10==2 ~ paste0("Age: 10-19"),
    Age_gr_10==3 ~ paste0("Age: 20-29"),
    Age_gr_10==4 ~ paste0("Age: 30-39"),
    Age_gr_10==5 ~ paste0("Age: 40-49"),
    Age_gr_10==6 ~ paste0("Age: 50-59"),
    Age_gr_10==7 ~ paste0("Age: 60-69"),
    Age_gr_10==8 ~ paste0("Age: 70+"))) %>%
  select(lab) %>%
  unlist()
names(Age_groups.labs_comb_prev) <- 1:8
# browser()




Mulenga_Combined_Prevalence_Age <- readRDS("analysis/data/Code-generated-data/14_Mulenga_Combined_Prevalence_by_Age.rds") %>%
  mutate(Age_gr_10 = 1:8)

    Figure3 <- ggplot(merge(Comb_prev_df,Mulenga_Combined_Prevalence_Age), aes(x = date)) +
      geom_line(aes(y=comb_perc_mean*100)) +
      geom_ribbon(aes(ymin=comb_perc_min*100, ymax=comb_perc_min*100), alpha=0.3) +
      facet_wrap(vars(Age_gr_10), labeller = labeller(Age_gr_10 = Age_groups.labs_comb_prev)) +
      xlab("Date") + ylab("Combined prevalence %") +
      ggtitle(paste0("IFR x ", IFRvals$IFR_x[fit_num],", Slope x ", IFRvals$Slope_x[fit_num],"\nModelled COVID-19 Combined prevalence Lusaka")) +
      theme(plot.title = element_text(size = 10)) +
      geom_point(data = Mulenga_Combined_Prevalence_Age, aes(x= as.Date("2020-07-15"), y = Combined_Prev)) +
      geom_errorbar(data = Mulenga_Combined_Prevalence_Age, aes(ymin=LowerInt,ymax=HigherInt,x=as.Date("2020-07-15"), width=10)) +
      geom_errorbarh(mapping = aes(y = Combined_Prev, xmin=as.Date("2020-07-04"), xmax=as.Date("2020-07-27"),height = 0))

    return(list(Diagnostic = Figure1, Age_week = Figure2, Infection_rate = Figure3))


}
