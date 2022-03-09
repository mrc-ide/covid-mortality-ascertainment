rm(list = ls())
devtools::load_all()
library(squire)
library(ggplot2)
library(dplyr)
library(reshape2)

### Dashboard results of fit
## Graph 1: Modelled covid deaths Lusaka, modelled covid +ve deaths Lusaka,
##        scaled covid +ve deaths Lusaka, scaled covid +ve deaths Lusaka


## Check likelihood
# fit_Model_list <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-02-06_Set_1_nbin_weeks.rds")
fit_Model_list <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-02-07_Set_2_pois_weeks.rds")
# fit_Model_list <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-02-07_Set_3_nb_weeks_uth.rds")
# fit_Model_Test <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-02-07_Set_3_nb_weeks_uth.rds")[[1]]

# LikC1 <- fit_Model_Test$pmcmc_results$chains$chain1$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(fit_Model_Test$replicate_parameters), value = T))))]
# LikC2 <- fit_Model_Test$pmcmc_results$chains$chain2$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(fit_Model_Test$replicate_parameters), value = T))))]
# LikC3 <- fit_Model_Test$pmcmc_results$chains$chain3$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(fit_Model_Test$replicate_parameters), value = T))))]

# mean(unlist(list(LikC1,LikC2,LikC3)))

## Check posterior
# PosC1 <- fit_Model_Test$pmcmc_results$chains$chain1$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(fit_Model_Test$replicate_parameters), value = T))))]
# PosC2 <- fit_Model_Test$pmcmc_results$chains$chain2$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(fit_Model_Test$replicate_parameters), value = T))))]
# PosC3 <- fit_Model_Test$pmcmc_results$chains$chain3$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(fit_Model_Test$replicate_parameters), value = T))))]

# mean(unlist(list(PosC1,PosC2,PosC3)))


## While we're here, let's check the priors
# PrC1 <- fit_Model_Test$pmcmc_results$chains$chain1$results$log_prior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(fit_Model_Test$replicate_parameters), value = T))))]
# PrC2 <- fit_Model_Test$pmcmc_results$chains$chain2$results$log_prior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(fit_Model_Test$replicate_parameters), value = T))))]
# PrC3 <- fit_Model_Test$pmcmc_results$chains$chain3$results$log_prior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(fit_Model_Test$replicate_parameters), value = T))))]
# mean(unlist(list(PrC1,PrC2,PrC3)))


Diagnostic_Plot <- function(fit_num, fit_Model_list, IFRvals){

  # browser()

  fit_Model <- fit_Model_list[[fit_num]]

  pcr_det <- fit_Model$pmcmc_results$inputs$pars_obs$pcr_det
  index <- squire:::odin_index(fit_Model$model)
  Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                   to = as.Date("2020-10-02"),
                                   by = "week"),as.Date("2020-10-02"))
  k_death <- fit_Model$pmcmc_results$inputs$pars_obs$k_death
  WeekData <- fit_Model$pmcmc_results$inputs$pars_obs$combined_data_week

  Mod_Weekly_Deaths_Lus <- apply(fit_Model$output,3,function(x){
    # browser()
    Sus <- rowSums(x[, index$S],na.rm = T)
    infs <- c(0,as.integer(diff(max(Sus, na.rm=T)-Sus)))
    infs <- ifelse(infs<0, 0, infs)
    pcr_positive <- roll_func(infs, pcr_det)
    pcr_perc <- pcr_positive/max(Sus, na.rm = T)
    # browser()
    pcr_perc <- pcr_perc[as.Date(names(Sus)) %in% (Days_for_comparison[-1]-3)]

    Ds_Lus <- diff(rowSums(x[as.Date(rownames(x)) %in% Days_for_comparison, index$D],na.rm = T))
    Ds_Mor <- Ds_Lus*0.8
    NcDs_mor <- WeekData$deaths - Ds_Mor
    CoinDs_Lus <- NcDs_mor*pcr_perc/0.8
    Pos_Ds_Lus <- CoinDs_Lus + Ds_Lus
    # ll_nb <- sum(dnbinom(x = WeekData$PosTestNum, size = k_death, mu = Pos_Ds_Lus*0.8*WeekData$Samples/WeekData$deaths, log=T))
    ll_pois <- sum(dpois(x = WeekData$PosTestNum, lambda = Pos_Ds_Lus*0.8*WeekData$Samples/WeekData$deaths, log=T))
    return(data.frame(week = 1:16,
                      Pos_Ds_Lus = Pos_Ds_Lus,
                      Ds_Lus = Ds_Lus,
                      CoinDs_Lus = CoinDs_Lus,
                      pcr_perc = pcr_perc,
                      ll = ll_pois))
  }) %>% str2str::ld2a() %>%
    data.frame(week = 1:16,
               Mean = apply(., MARGIN = 2, FUN = function(x){rowMeans(x)}),
               Max = apply(., MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = max)}),
               Min = apply(., MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = min)})) %>%
    select(week,Mean.Pos_Ds_Lus, Max.Pos_Ds_Lus, Min.Pos_Ds_Lus,
           Mean.Ds_Lus, Max.Ds_Lus, Min.Ds_Lus,
           Mean.CoinDs_Lus, Max.CoinDs_Lus, Min.CoinDs_Lus,
           Mean.pcr_perc, Max.pcr_perc, Min.pcr_perc,
           Mean.ll, Max.ll, Min.ll
    )

  WeekData <- WeekData %>% mutate(PosDeathsHosp = PosTestNum/Samples * deaths,
                                  PosDeathsLus = PosDeathsHosp/0.8,
                                  TrueDeathsHosp = (PosDeathsHosp - Mod_Weekly_Deaths_Lus$Mean.pcr_perc*deaths)/(1 - Mod_Weekly_Deaths_Lus$Mean.pcr_perc),
                                  TrueDeathsLus = TrueDeathsHosp/0.8)



  p1 <- ggplot(Mod_Weekly_Deaths_Lus, aes(x = week)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Pos_Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus, ymax=Max.Pos_Ds_Lus), alpha=0.3) +
    # Modelled True deaths
    geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(data = WeekData, aes(y = PosDeathsLus)) +
    # Scaled True deaths
    geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Week") + ylab("Deaths") +
    ggtitle(paste("COVID-19 weekly deaths\n(Lusaka) \nnbin ll = ", round(Mod_Weekly_Deaths_Lus$Mean.ll[1]),1)) +
    theme(plot.title = element_text(size = 10))



  ## Graph 2:
  p2 <- ggplot(data = WeekData, aes(x = week)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum/Samples)) +
    # Prevalence of True positive in sample
    geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(data = Mod_Weekly_Deaths_Lus, aes(y = Mean.Pos_Ds_Lus*0.8/WeekData$deaths),linetype="dashed") +
    geom_ribbon(data = Mod_Weekly_Deaths_Lus, aes(ymin=Min.Pos_Ds_Lus*0.8/WeekData$deaths, ymax=Max.Pos_Ds_Lus*0.8/WeekData$deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(data = Mod_Weekly_Deaths_Lus, aes(y = Mean.Ds_Lus*0.8/WeekData$deaths),linetype="dashed") +
    geom_ribbon(data = Mod_Weekly_Deaths_Lus, aes(ymin=Min.Ds_Lus*0.8/WeekData$deaths, ymax=Max.Ds_Lus*0.8/WeekData$deaths), alpha=0.3, fill = "red") +
    xlab("Week") + ylab("Prevalence")  +
    ggtitle("COVID-19 weekly prevalence\n(UTH mortuary)") +
    theme(plot.title = element_text(size = 10))

  ## Of rmy benefit, let's plot at the test level
  p2b <- ggplot(data = WeekData, aes(x = week)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum)) +
    # Prevalence of True positive in sample
    geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(data = Mod_Weekly_Deaths_Lus, aes(y = WeekData$Samples*Mean.Pos_Ds_Lus*0.8/WeekData$deaths),linetype="dashed") +
    geom_ribbon(data = Mod_Weekly_Deaths_Lus, aes(ymin=WeekData$Samples*Min.Pos_Ds_Lus*0.8/WeekData$deaths, ymax=WeekData$Samples*Max.Pos_Ds_Lus*0.8/WeekData$deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(data = Mod_Weekly_Deaths_Lus, aes(y = WeekData$Samples*Mean.Ds_Lus*0.8/WeekData$deaths),linetype="dashed") +
    geom_ribbon(data = Mod_Weekly_Deaths_Lus, aes(ymin=WeekData$Samples*Min.Ds_Lus*0.8/WeekData$deaths, ymax=WeekData$Samples*Max.Ds_Lus*0.8/WeekData$deaths), alpha=0.3, fill = "red") +
    xlab("Week") + ylab("Positive tests")  +
    ggtitle("COVID-19 weekly positive\ntests (UTH mortuary)") +
    theme(plot.title = element_text(size = 10))


  # Graph 3: Age distribution

  # FullData <- fit_Model$pmcmc_results$inputs$pars_obs$combined_data
  FullData <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")

  AgeData <- FullData %>% group_by(Age_group) %>%
    summarise(total_deaths = sum(total_deaths),
              Samples = sum(Samples),
              PosTestNum = sum(PosTestNum)) %>%
    mutate(PosDeathsLus = (PosTestNum/Samples*total_deaths)/0.8)

  Mod_Age_Deaths_Lus <- apply(fit_Model$output,3,function(x){

    AgeRes <- sapply(1:ncol(x[,index$S]),function(Age_Group){
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
      return(data.frame(Pos_Ds_Lus = sum(Pos_Ds_Lus),
                        Ds_Lus = sum(Ds_Lus),
                        CoinDs_Lus = sum(CoinDs_Lus)))
    })
    AgeRes<- t(AgeRes)
    rownames(AgeRes) <- 1:17
    AgeRes <- data.frame(AgeRes)
    AgeRes$ll_mult <- dmultinom(x = AgeData$PosTestNum, prob = unlist(AgeRes$Pos_Ds_Lus)*0.8*AgeData$Samples/AgeData$total_deaths, log=T)
    AgeRes
  })

  Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus %>% str2str::ld2a() %>%
    data.frame(Age_group = 1:17,
               Mean = apply(., MARGIN = 2, rowMeans),
               Max = apply(., MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = max)}),
               Min = apply(., MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = min)})) %>%
    select(Age_group, Mean.Pos_Ds_Lus, Max.Pos_Ds_Lus, Min.Pos_Ds_Lus,
           Mean.Ds_Lus, Max.Ds_Lus, Min.Ds_Lus,
           Mean.CoinDs_Lus, Max.CoinDs_Lus, Min.CoinDs_Lus,
           Mean.ll_mult, Max.ll_mult, Min.ll_mult)


  p3 <- ggplot(Mod_Age_Deaths_Lus, aes(x = Age_group)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Pos_Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Pos_Ds_Lus, ymax=Max.Pos_Ds_Lus), alpha=0.3) +
    # Modelled True deaths
    geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(data = AgeData, aes(y = PosDeathsLus)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Age group") + ylab("Deaths") +
    ggtitle(paste("COVID-19 deaths by age\n(Lusaka)\nmultinom ll =", round(Mod_Age_Deaths_Lus$Mean.ll_mult[1],1))) +
    theme(plot.title = element_text(size = 10))


  p4 <- ggplot(data = AgeData, aes(x = Age_group)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum/Samples)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(data = Mod_Age_Deaths_Lus, aes(y = Mean.Pos_Ds_Lus*0.8/AgeData$total_deaths),linetype="dashed") +
    geom_ribbon(data = Mod_Age_Deaths_Lus, aes(ymin=Min.Pos_Ds_Lus*0.8/AgeData$total_deaths, ymax=Max.Pos_Ds_Lus*0.8/AgeData$total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(data = Mod_Age_Deaths_Lus, aes(y = Mean.Ds_Lus*0.8/AgeData$total_deaths),linetype="dashed") +
    geom_ribbon(data = Mod_Age_Deaths_Lus, aes(ymin=Min.Ds_Lus*0.8/AgeData$total_deaths, ymax=Max.Ds_Lus*0.8/AgeData$total_deaths), alpha=0.3, fill = "red") +
    xlab("Age group") + ylab("Prevalence")  +
    ggtitle("COVID-19 prevalence by age\n(UTH mortuary)") +
    theme(plot.title = element_text(size = 10))

  ## Again, for my benefit, let's plot at the test level
  p4b <- ggplot(data = AgeData, aes(x = Age_group)) +
    # Prevalence on positive in sample
    geom_point(aes(y = PosTestNum)) +
    # Prevalence of True positive in sample
    # geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
    # Modelled Positive Deaths (hospital)/Total hospital deaths
    geom_line(data = Mod_Age_Deaths_Lus, aes(y = AgeData$Samples*Mean.Pos_Ds_Lus*0.8/AgeData$total_deaths),linetype="dashed") +
    geom_ribbon(data = Mod_Age_Deaths_Lus, aes(ymin = AgeData$Samples*Min.Pos_Ds_Lus*0.8/AgeData$total_deaths, ymax=AgeData$Samples*Max.Pos_Ds_Lus*0.8/AgeData$total_deaths), alpha=0.3) +
    # Modelled True Deaths (hospital)/Total hospital deaths
    geom_line(data = Mod_Age_Deaths_Lus, aes(y = AgeData$Samples*Mean.Ds_Lus*0.8/AgeData$total_deaths),linetype="dashed") +
    geom_ribbon(data = Mod_Age_Deaths_Lus, aes(ymin=AgeData$Samples*Min.Ds_Lus*0.8/AgeData$total_deaths, ymax=AgeData$Samples*Max.Ds_Lus*0.8/AgeData$total_deaths), alpha=0.3, fill = "red") +
    xlab("Age group") + ylab("Positive tests")  +
    ggtitle("COVID-19 positive tests by\nage (UTH mortuary)") +
    theme(plot.title = element_text(size = 10))



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



  p5 <- pcr_fit_plot(pcr_sero_data) + ggtitle(paste("PCR prevalence\nbinom ll =", round(sero_pcr_ll$ll_pcr,1))) + theme(plot.title = element_text(size = 10))
  p6 <- sero_fit_plot(pcr_sero_data) + ggtitle(paste("Seroprevalence \nbinom ll =", round(sero_pcr_ll$ll_sero,1))) + theme(plot.title = element_text(size = 10))
  p7 <- combined_fit_plot(pcr_sero_data) + ggtitle(paste("Combined prevalence \nbinom ll =", round(comb_ll$ll_combined,1))) + theme(plot.title = element_text(size = 10))


  title_gg <- ggplot() +
    labs(title = paste0("IFR x ", IFRvals$IFR_x[fit_num],", Slope x ", IFRvals$Slope_x[fit_num]))
# browser()
  cowplot::plot_grid(title_gg, cowplot::plot_grid(p1,p2,p2b,p3,p4,p4b,p5,p6,p7), ncol = 1, rel_heights = c(0.05, 1))
}


# fit_Model_Test <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-02-06_Set_1_nbin_weeks.rds")
IFRvals_fil <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds"),]
# IFR_x <- IFRvals$IFR_x; IFR_slope <- IFRvals$Slope_x
# readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds")

# Diagnostic_Plot(fit_num = 2, fit_Model_list = fit_Model_list, IFRvals = IFRvals_fil)

pdf("analysis/figures/31_01_Diagnostic_plots_Set_2.pdf")
lapply(X = 1:length(fit_Model_list), FUN = Diagnostic_Plot, fit_Model_list = fit_Model_list, IFRvals = IFRvals_fil)
# Diagnostic_Plot(fit_Model = fit_Model_Test)
dev.off()
