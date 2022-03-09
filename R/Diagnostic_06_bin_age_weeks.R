## Diagnostic plots for likelihood 04: nbinom/pois weekly deaths, multinomial age, binomial

Diagnostic_Plot_06 <- function(fit_num, fit_Model_list, IFRvals, ll_type){

  fit_Model <- fit_Model_list[[fit_num]]

  pcr_det <- fit_Model$pmcmc_results$inputs$pars_obs$pcr_det
  index <- squire:::odin_index(fit_Model$model)
  Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                   to = as.Date("2020-10-02"),
                                   by = "week"),as.Date("2020-10-02"))
  # k_death <- fit_Model$pmcmc_results$inputs$pars_obs$k_death
  # WeekData <- fit_Model$pmcmc_results$inputs$pars_obs$combined_data_week

  FullData <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")

  ## Estimate pcr for each age group in each week
  Mod_Age_Deaths_Lus <- apply(fit_Model$output,3,function(x){

    AgeRes <- lapply(1:ncol(x[,index$S]),function(Age_Group){
      Sus <- x[,index$S][,Age_Group]
      Sus <- ifelse(is.na(Sus),0,Sus)
      infs <- c(0,as.integer(diff(max(Sus, na.rm = T)-Sus)))
      infs <- ifelse(infs<0, 0, infs)
      browser()
      pcr_positive <- roll_func(infs, pcr_det)
      pcr_perc <- pcr_positive/max(Sus, na.rm = T)
      pcr_perc <- pcr_perc[as.Date(names(Sus)) %in% (Days_for_comparison[-1]-3)]

      Ds_Lus <- diff(x[as.Date(rownames(x)) %in% Days_for_comparison, index$D][,Age_Group],na.rm = T)
      Ds_Mor <- Ds_Lus*0.8
      NcDs_mor <- filter(FullData, Age_gr == Age_Group)$Mort_deaths - Ds_Mor
      NcDs_mor <- ifelse(NcDs_mor<0,0,NcDs_mor)
      CoinDs_Lus <- NcDs_mor*pcr_perc/0.8
      Pos_Ds_Lus <- CoinDs_Lus + Ds_Lus
      # browser()
      return(data.frame(Week_gr = 1:16,
                        Age_gr = Age_Group,
                        Pos_Ds_Lus = Pos_Ds_Lus,
                        Ds_Lus = Ds_Lus,
                        CoinDs_Lus = CoinDs_Lus,
                        pcr_perc = pcr_perc))
    })

    AgeRes <- do.call(rbind.data.frame, AgeRes)
    AgeRes_Full <- merge(FullData,AgeRes) %>%
      mutate(PosPrev = ifelse(Pos_Ds_Lus/Mort_deaths>=1, 0.999999, Pos_Ds_Lus/Mort_deaths)) %>%
             # Err1 = PosPrev>1) %>%
      filter(Week_gr != 4) %>%
      mutate(ll = dbinom(x = PosTests, size = Samples, prob = PosPrev, log = T)) %>%
      select(-date)

    AgeRes_Full
  })
  # browser()
  Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus %>%
  str2str::ld2a() %>%
    cbind(.[,c("Age_group","Week_gr","total_deaths","Samples","PosTestNum"),1],
          data.frame(Mean = apply(.[,c("Pos_Ds_Lus","Ds_Lus","CoinDs_Lus","pcr_perc","PosPrev","ll"),], MARGIN = 2, FUN = function(x){rowMeans(x)}),
                       Max = apply(.[,c("Pos_Ds_Lus","Ds_Lus","CoinDs_Lus","pcr_perc","PosPrev","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = max)}),
                       Min = apply(.[,c("Pos_Ds_Lus","Ds_Lus","CoinDs_Lus","pcr_perc","PosPrev","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = min)}))) %>%
            select(Age_group,Week_gr,total_deaths,Samples,PosTestNum,
                   Mean.Pos_Ds_Lus, Max.Pos_Ds_Lus, Min.Pos_Ds_Lus,
                   Mean.Ds_Lus, Max.Ds_Lus, Min.Ds_Lus,
                   Mean.CoinDs_Lus, Max.CoinDs_Lus, Min.CoinDs_Lus,
                   Mean.pcr_perc, Max.pcr_perc, Min.pcr_perc,
                   Mean.ll, Max.ll, Min.ll) %>%
    arrange(Age_group, Week_gr)
  rownames(Mod_Age_Deaths_Lus_Av) <- NULL

  ## Plot by week
  Mod_Age_Deaths_Lus_Av_Week <- merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_group","Week_gr","date")]) %>% group_by(Week_gr) %>%
    summarise(date = head(date,1)+3,#Week_gr = head(Week_gr,1),
              total_deaths = sum(total_deaths),
              Samples = sum(Samples),
              PosTestNum = sum(PosTestNum),
              Mean.Pos_Ds_Lus = sum(Mean.Pos_Ds_Lus),
              Max.Pos_Ds_Lus = sum(Max.Pos_Ds_Lus),
              Min.Pos_Ds_Lus = sum(Min.Pos_Ds_Lus),
              Mean.Ds_Lus = sum(Mean.Ds_Lus),
              Max.Ds_Lus = sum(Max.Ds_Lus),
              Min.Ds_Lus = sum(Min.Ds_Lus),
              Mean.CoinDs_Lus = sum(Mean.CoinDs_Lus),
              Max.CoinDs_Lus = sum(Max.CoinDs_Lus),
              Min.CoinDs_Lus = sum(Min.CoinDs_Lus),
              # Could add pcr, but this would need to be weighted by population size in each age group?
              )
# browser()
  p1 <- ggplot(Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Pos_Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus, ymax=Max.Pos_Ds_Lus), alpha=0.3) +
    # Modelled True deaths
    geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = (total_deaths*PosTestNum/Samples)/0.8)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Date") + ylab("Deaths") +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle(paste("COVID-19 weekly deaths\n(Lusaka) \nbin ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
    theme(plot.title = element_text(size = 10))
  #
  #
  #
  # ## Graph 2:
  p2 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum/Samples)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mean.Pos_Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus*0.8/total_deaths, ymax=Max.Pos_Ds_Lus*0.8/total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mean.Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus*0.8/total_deaths, ymax=Max.Ds_Lus*0.8/total_deaths), alpha=0.3, fill = "red") +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    xlab("Date") + ylab("Prevalence")  +
    ggtitle("COVID-19 weekly prevalence\n(UTH mortuary)") +
    theme(plot.title = element_text(size = 10))
  #
  # ## Of rmy benefit, let's plot at the test level
  p3 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mean.Pos_Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Min.Pos_Ds_Lus*0.8/total_deaths, ymax=Samples*Max.Pos_Ds_Lus*0.8/total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mean.Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Min.Ds_Lus*0.8/total_deaths, ymax=Samples*Max.Ds_Lus*0.8/total_deaths), alpha=0.3, fill = "red") +
    xlab("Date") + ylab("Positive tests")  +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle("COVID-19 weekly positive\ntests (UTH mortuary)") +
    theme(plot.title = element_text(size = 10))



  ## Plot by Age
  Mod_Age_Deaths_Lus_Av_Age <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_group) %>%
    summarise(#Week_gr = head(Week_gr,1),
      total_deaths = sum(total_deaths),
      Samples = sum(Samples),
      PosTestNum = sum(PosTestNum),
      Mean.Pos_Ds_Lus = sum(Mean.Pos_Ds_Lus),
      Max.Pos_Ds_Lus = sum(Max.Pos_Ds_Lus),
      Min.Pos_Ds_Lus = sum(Min.Pos_Ds_Lus),
      Mean.Ds_Lus = sum(Mean.Ds_Lus),
      Max.Ds_Lus = sum(Max.Ds_Lus),
      Min.Ds_Lus = sum(Min.Ds_Lus),
      Mean.CoinDs_Lus = sum(Mean.CoinDs_Lus),
      Max.CoinDs_Lus = sum(Max.CoinDs_Lus),
      Min.CoinDs_Lus = sum(Min.CoinDs_Lus),
      # Could add pcr, but this would need to be weighted by population size in each age group?
    )

  p4 <- ggplot(Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_group)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Pos_Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus, ymax=Max.Pos_Ds_Lus), alpha=0.3) +
    # Modelled True deaths
    geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = (total_deaths*PosTestNum/Samples)/0.8)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Age group") + ylab("Deaths") +
    ggtitle(paste("COVID-19 deaths by age\n(Lusaka)\nbin ll =", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
    theme(plot.title = element_text(size = 10))
  #
  #
  #
  # ## Graph 2:
  p5 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_group)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum/Samples)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mean.Pos_Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus*0.8/total_deaths, ymax=Max.Pos_Ds_Lus*0.8/total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mean.Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus*0.8/total_deaths, ymax=Max.Ds_Lus*0.8/total_deaths), alpha=0.3, fill = "red") +
    xlab("Age group") + ylab("Prevalence")  +
    ggtitle("COVID-19 prevalence by age\n(UTH mortuary)") +
    theme(plot.title = element_text(size = 10))
  #
  # ## Of rmy benefit, let's plot at the test level
  p6 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_group)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mean.Pos_Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Min.Pos_Ds_Lus*0.8/total_deaths, ymax=Samples*Max.Pos_Ds_Lus*0.8/total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mean.Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Min.Ds_Lus*0.8/total_deaths, ymax=Samples*Max.Ds_Lus*0.8/total_deaths), alpha=0.3, fill = "red") +
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
  cowplot::plot_grid(title_gg, cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9), ncol = 1, rel_heights = c(0.05, 1))
}


Diagnostic_Plot_06b_Age_Date <- function(fit_num, fit_Model_list, IFRvals, ll_type){

  fit_Model <- fit_Model_list[[fit_num]]

  pcr_det <- fit_Model$pmcmc_results$inputs$pars_obs$pcr_det
  index <- squire:::odin_index(fit_Model$model)
  Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                   to = as.Date("2020-10-02"),
                                   by = "week"),as.Date("2020-10-02"))
  # k_death <- fit_Model$pmcmc_results$inputs$pars_obs$k_death
  # WeekData <- fit_Model$pmcmc_results$inputs$pars_obs$combined_data_week

  FullData <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")

  ## Estimate pcr for each age group in each week
  Mod_Age_Deaths_Lus <- apply(fit_Model$output,3,function(x){

    AgeRes <- lapply(1:ncol(x[,index$S]),function(Age_Group){
      Sus <- x[,index$S][,Age_Group]
      Sus <- ifelse(is.na(Sus),0,Sus)
      infs <- c(0,as.integer(diff(max(Sus, na.rm = T)-Sus)))
      infs <- ifelse(infs<0, 0, infs)
      pcr_positive <- roll_func(infs, pcr_det)
      pcr_perc <- pcr_positive/max(Sus, na.rm = T)
      pcr_perc <- pcr_perc[as.Date(names(Sus)) %in% (Days_for_comparison[-1]-3)]

      Ds_Lus <- diff(x[as.Date(rownames(x)) %in% Days_for_comparison, index$D][,Age_Group],na.rm = T)
      Ds_Mor <- Ds_Lus*0.8
      NcDs_mor <- filter(FullData, Age_group == Age_Group)$total_deaths - Ds_Mor
      NcDs_mor <- ifelse(NcDs_mor<0,0,NcDs_mor)
      CoinDs_Lus <- NcDs_mor*pcr_perc/0.8
      Pos_Ds_Lus <- CoinDs_Lus + Ds_Lus
      # browser()
      return(data.frame(Week_gr = 1:16,
                        Age_group = Age_Group,
                        Pos_Ds_Lus = Pos_Ds_Lus,
                        Ds_Lus = Ds_Lus,
                        CoinDs_Lus = CoinDs_Lus,
                        pcr_perc = pcr_perc))
    })

    AgeRes <- do.call(rbind.data.frame, AgeRes)
    AgeRes_Full <- merge(FullData,AgeRes) %>%
      mutate(PosPrev = ifelse(Pos_Ds_Lus/total_deaths>=1, 0.999999, Pos_Ds_Lus/total_deaths)) %>%
      # Err1 = PosPrev>1) %>%
      # filter(Err1 == T)
      mutate(ll = dbinom(x = PosTestNum, size = Samples, prob = PosPrev, log = T)) %>%
      select(-date)

    AgeRes_Full
  })
  # browser()
  Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus %>%
    str2str::ld2a() %>%
    cbind(.[,c("Age_group","Week_gr","total_deaths","Samples","PosTestNum"),1],
          data.frame(Mean = apply(.[,c("Pos_Ds_Lus","Ds_Lus","CoinDs_Lus","pcr_perc","PosPrev","ll"),], MARGIN = 2, FUN = function(x){rowMeans(x)}),
                     Max = apply(.[,c("Pos_Ds_Lus","Ds_Lus","CoinDs_Lus","pcr_perc","PosPrev","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = max)}),
                     Min = apply(.[,c("Pos_Ds_Lus","Ds_Lus","CoinDs_Lus","pcr_perc","PosPrev","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = min)}))) %>%
    select(Age_group,Week_gr,total_deaths,Samples,PosTestNum,
           Mean.Pos_Ds_Lus, Max.Pos_Ds_Lus, Min.Pos_Ds_Lus,
           Mean.Ds_Lus, Max.Ds_Lus, Min.Ds_Lus,
           Mean.CoinDs_Lus, Max.CoinDs_Lus, Min.CoinDs_Lus,
           Mean.pcr_perc, Max.pcr_perc, Min.pcr_perc,
           Mean.ll, Max.ll, Min.ll) %>%
    arrange(Age_group, Week_gr)
  rownames(Mod_Age_Deaths_Lus_Av) <- NULL



  Age_groups.labs <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_group) %>%
    summarise(Mean.ll = sum(Mean.ll)) %>%
    mutate(lab = paste0("bin ll AG ",Age_group,": ", round(Mean.ll,1))) %>%
    select(lab) %>%
    unlist()

  names(Age_groups.labs) <- 1:17

  Mod_Age_Deaths_Lus_Av %>% filter(Age_group ==1)
  Mod_Age_Deaths_Lus_Av %>% filter(Age_group ==2)
  Mod_Age_Deaths_Lus_Av %>% filter(Age_group ==3) %>% arrange(Week_gr)

  browser()

  Mod_Age_Deaths_Lus_Av <- merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_group","Week_gr","date")])

  p1 <- ggplot(Mod_Age_Deaths_Lus_Av, aes(x = date)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Pos_Ds_Lus),linetype="dashed") +
    facet_wrap(vars(Age_group), labeller = labeller(Age_group = Age_groups.labs)) +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus, ymax=Max.Pos_Ds_Lus), alpha=0.3) +
    # Modelled True deaths
    geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = (total_deaths*PosTestNum/Samples)/0.8)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Week") + ylab("Deaths") +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle(paste("COVID-19 weekly deaths\n(Lusaka) \nbin ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
    theme(plot.title = element_text(size = 10))

  Mod_Age_Deaths_Lus_Av %>%
    group_by(Age_group) %>%
    summarise(Samples = sum(Samples),
              total_deaths = sum(total_deaths)) %>%
    mutate(Sampling_perc = 100*Samples/total_deaths)

  # ## Graph 2:
  p2 <- ggplot(data = Mod_Age_Deaths_Lus_Av, aes(x = date)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum/Samples)) +
    facet_wrap(vars(Age_group), labeller = labeller(Age_group = Age_groups.labs)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mean.Pos_Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus*0.8/total_deaths, ymax=Max.Pos_Ds_Lus*0.8/total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mean.Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus*0.8/total_deaths, ymax=Max.Ds_Lus*0.8/total_deaths), alpha=0.3, fill = "red") +
    xlab("Week") + ylab("Prevalence")  +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle("COVID-19 weekly prevalence\n(UTH mortuary)") +
    theme(plot.title = element_text(size = 10)) +
    geom_hline(yintercept = 1, linetype = 2, colour = "darkblue")
  #
  # ## Of rmy benefit, let's plot at the test level
  p3 <- ggplot(data = Mod_Age_Deaths_Lus_Av, aes(x = date)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum)) +
    facet_wrap(vars(Age_group), labeller = labeller(Age_group = Age_groups.labs)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mean.Pos_Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Min.Pos_Ds_Lus*0.8/total_deaths, ymax=Samples*Max.Pos_Ds_Lus*0.8/total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mean.Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Min.Ds_Lus*0.8/total_deaths, ymax=Samples*Max.Ds_Lus*0.8/total_deaths), alpha=0.3, fill = "red") +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    xlab("Week") + ylab("Positive tests")  +
    ggtitle("COVID-19 weekly positive\ntests (UTH mortuary)") +
    theme(plot.title = element_text(size = 10)) +
    geom_point(aes(y = Samples), size = 0.5, col = "blue")



  ## Plot by Age
  Mod_Age_Deaths_Lus_Av_Age <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_group) %>%
    summarise(#Week_gr = head(Week_gr,1),
      total_deaths = sum(total_deaths),
      Samples = sum(Samples),
      PosTestNum = sum(PosTestNum),
      Mean.Pos_Ds_Lus = sum(Mean.Pos_Ds_Lus),
      Max.Pos_Ds_Lus = sum(Max.Pos_Ds_Lus),
      Min.Pos_Ds_Lus = sum(Min.Pos_Ds_Lus),
      Mean.Ds_Lus = sum(Mean.Ds_Lus),
      Max.Ds_Lus = sum(Max.Ds_Lus),
      Min.Ds_Lus = sum(Min.Ds_Lus),
      Mean.CoinDs_Lus = sum(Mean.CoinDs_Lus),
      Max.CoinDs_Lus = sum(Max.CoinDs_Lus),
      Min.CoinDs_Lus = sum(Min.CoinDs_Lus),
      # Could add pcr, but this would need to be weighted by population size in each age group?
    )

  p4 <- ggplot(Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_group)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Pos_Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus, ymax=Max.Pos_Ds_Lus), alpha=0.3) +
    # Modelled True deaths
    geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = (total_deaths*PosTestNum/Samples)/0.8)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Age group") + ylab("Deaths") +
    ggtitle(paste("COVID-19 deaths by age\n(Lusaka)\nbin ll =", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
    theme(plot.title = element_text(size = 10))
  #
  #
  #
  # ## Graph 2:
  p5 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_group)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum/Samples)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mean.Pos_Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus*0.8/total_deaths, ymax=Max.Pos_Ds_Lus*0.8/total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Mean.Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus*0.8/total_deaths, ymax=Max.Ds_Lus*0.8/total_deaths), alpha=0.3, fill = "red") +
    xlab("Age group") + ylab("Prevalence")  +
    ggtitle("COVID-19 prevalence by age\n(UTH mortuary)") +
    theme(plot.title = element_text(size = 10))
  #
  # ## Of rmy benefit, let's plot at the test level
  p6 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_group)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mean.Pos_Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Min.Pos_Ds_Lus*0.8/total_deaths, ymax=Samples*Max.Pos_Ds_Lus*0.8/total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(aes(y = Samples*Mean.Ds_Lus*0.8/total_deaths),linetype="dashed") +
    geom_ribbon(aes(ymin=Samples*Min.Ds_Lus*0.8/total_deaths, ymax=Samples*Max.Ds_Lus*0.8/total_deaths), alpha=0.3, fill = "red") +
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
  cowplot::plot_grid(title_gg, cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9), ncol = 1, rel_heights = c(0.05, 1))
}
