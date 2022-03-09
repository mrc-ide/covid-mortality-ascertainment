rm(list = ls())
library(dplyr)
library(ggplot2)
devtools::load_all()

Res1 <- readRDS("../Bonus Files/2022-03-02_Testing_lower_duration_x3.rds")

Res1 <- readRDS("../Bonus Files/2022-03-03_07_pois_bin_bin_just_pois.rds")
Res1 <- readRDS("../Bonus Files/2022-03-03_07_pois_bin_bin.rds")
Dataset_Name <- "07_pois_bin_bin_just_pois"
# Dataset_Name <- "07_pois_bin_bin"


## IFR List
IFR_mat <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")
IFR_vec <- 1:nrow(IFR_mat) %in% readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds")
IFR_mat_fil <- IFR_mat[IFR_vec,]
rownames(IFR_mat_fil) <- 1:nrow(IFR_mat_fil)


get_Likelihood <- function(model_fit){
  # Select all sampled posteriors
  LikC1 <- model_fit$pmcmc_results$chains$chain1$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(model_fit$replicate_parameters), value = T))))]
  LikC2 <- model_fit$pmcmc_results$chains$chain2$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(model_fit$replicate_parameters), value = T))))]
  LikC3 <- model_fit$pmcmc_results$chains$chain3$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(model_fit$replicate_parameters), value = T))))]
  # Average out posteriors
  mean(unlist(list(LikC1,LikC2,LikC3)))
}

get_Posterior <- function(model_fit){
    # Select all sampled posteriors
    PosC1 <- model_fit$pmcmc_results$chains$chain1$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(model_fit$replicate_parameters), value = T))))]
    PosC2 <- model_fit$pmcmc_results$chains$chain2$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(model_fit$replicate_parameters), value = T))))]
    PosC3 <- model_fit$pmcmc_results$chains$chain3$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(model_fit$replicate_parameters), value = T))))]
    # Average out posteriors
    mean(unlist(list(PosC1,PosC2,PosC3)))
}

IFR_mat$AvPost <- NA
IFR_mat$AvPost[IFR_vec] <- unlist(lapply(Res1, get_Posterior))

IFR_mat$AvLike <- NA
IFR_mat$AvLike[IFR_vec] <- unlist(lapply(Res1, get_Likelihood))

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
  scale_fill_viridis_c()

## Check fit
# Plot modelled bin by age week
Bin_plots <- function(model_fit){

  pcr_det <- model_fit$pmcmc_results$inputs$pars_obs$pcr_det
  index <- squire:::odin_index(model_fit$model)
  Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                   to = as.Date("2020-10-02"),
                                   by = "week"),as.Date("2020-10-02"))

  FullData <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")

  ## Estimate pcr for each age group in each week
  Mod_Age_Deaths_Lus <- apply(model_fit$output,3,function(x){

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
      filter(Week_gr != 4) %>%
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

  return(list(p1,p2,p3))

}

Bin_plots(Res1[[1]])


# Plot modelled pois by age week
Pois_Fit_Plots <- function(model_fit){
  index <- squire:::odin_index(model_fit$model)

  Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                   to = as.Date("2020-10-02"),
                                   by = "week"),as.Date("2020-10-02"))

  Mort_Deaths_Age_Week <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age.rds") %>%
    mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
    group_by(week, Age_group) %>%
    summarise(total_deaths = sum(total_deaths),
              date = head(date,1))

  Samples_age_ests <- readRDS("analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")

  Mod_Age_Deaths_Lus <- apply(model_fit$output,3,function(x){

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
  # browser()
  # p1 <- ggplot(Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
  #   # Modelled Positive deaths
  #   geom_line(aes(y=Mean.Ds_Tot_Mor),linetype="dashed") +
  #   geom_ribbon(aes(ymin=Min.Ds_Tot_Mor, ymax=Max.Ds_Tot_Mor), alpha=0.3) +
  #   # Modelled True deaths
  #   # geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
  #   # geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
  #   # Scaled positive deaths
  #   geom_point(aes(y = total_deaths)) +
  #   # Scaled True deaths
  #   # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
  #   xlab("Date") + ylab("Deaths") +
  #   xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
  #   ggtitle(paste("COVID-19 weekly deaths\n(Mortuary) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
  #   theme(plot.title = element_text(size = 10))

  # Age_groups.labs <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_group) %>%
  #   summarise(Mean.ll = sum(Mean.ll)) %>%
  #   mutate(lab = paste0("pois ll AG ",Age_group,": ", round(Mean.ll,1))) %>%
  #   select(lab) %>%
  #   unlist()
  # #
  # names(Age_groups.labs) <- 1:17

  Age_groups.labs <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_group) %>%
    summarise(Mean.ll = sum(Mean.ll)) %>%
    mutate(lab = paste0("pois ll AG ",Age_group,": ", round(Mean.ll,1))) %>%
    mutate(lab = case_when(
      Age_group==1 ~ paste0("Age: 0-4; Pois ll = ",round(Mean.ll,1)),
      Age_group==2 ~ paste0("Age: 5-9; Pois ll = ",round(Mean.ll,1)),
      Age_group==3 ~ paste0("Age: 10-14; Pois ll = ",round(Mean.ll,1)),
      Age_group==4 ~ paste0("Age: 15-19; Pois ll = ",round(Mean.ll,1)),
      Age_group==5 ~ paste0("Age: 20-24; Pois ll = ",round(Mean.ll,1)),
      Age_group==6 ~ paste0("Age: 25-29; Pois ll = ",round(Mean.ll,1)),
      Age_group==7 ~ paste0("Age: 30-34; Pois ll = ",round(Mean.ll,1)),
      Age_group==8 ~ paste0("Age: 35-39; Pois ll = ",round(Mean.ll,1)),
      Age_group==9 ~ paste0("Age: 40-44; Pois ll = ",round(Mean.ll,1)),
      Age_group==10 ~ paste0("Age: 45-49; Pois ll = ",round(Mean.ll,1)),
      Age_group==11 ~ paste0("Age: 50-54; Pois ll = ",round(Mean.ll,1)),
      Age_group==12 ~ paste0("Age: 55-59; Pois ll = ",round(Mean.ll,1)),
      Age_group==13 ~ paste0("Age: 60-64; Pois ll = ",round(Mean.ll,1)),
      Age_group==14 ~ paste0("Age: 65-69; Pois ll = ",round(Mean.ll,1)),
      Age_group==15 ~ paste0("Age: 70-74; Pois ll = ",round(Mean.ll,1)),
      Age_group==16 ~ paste0("Age: 75-79; Pois ll = ",round(Mean.ll,1)),
      Age_group==17 ~ paste0("Age: 80+; Pois ll = ",round(Mean.ll,1)))) %>%
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
    ggtitle(paste("COVID-19 weekly deaths (Mortuary) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
    theme(plot.title = element_text(size = 10)) +
    geom_point(data = Mort_Deaths_Age_Week %>% filter(week==4), aes(x = date, y = total_deaths), col = "red")

  return(list(p2))}


pdf(file = "analysis/figures/37_PoisFits_2.pdf", width = 20, height = 10)
lapply(1:length(Res1), function(x){
  Pois_Fit_Plots(Res1[[x]])[[1]] + xlab(paste0(IFR_mat_fil[x,"IFR_x"]," x IFR; ",IFR_mat_fil[x,"Slope_x"]," x Slope"))
})
dev.off()

IFR_mat_fil %>% tibble::rownames_to_column() %>% filter(IFR_x ==1) %>% select(rowname)

# pdf(file = "analysis/figures/37_PoisFits_1xSlope.pdf")
# lapply(IFR_mat_fil %>% tibble::rownames_to_column() %>% filter(IFR_x ==1) %>% pull(rowname) %>% as.numeric(), function(x){
#   Pois_Fit_Plots(Res1[[x]])[[2]] + xlab(paste0(IFR_mat_fil[x,"IFR_x"]," x IFR; ",IFR_mat_fil[x,"Slope_x"]," x Slope"))
# })
# dev.off()

pdf(file = "analysis/figures/37_PoisFits_selected_earlier_ones_2.pdf", width = 9, height = 8)
lapply(c(1,33,37), function(x){
  Pois_Fit_Plots(Res1[[x]])[[1]] + xlab(paste0(IFR_mat_fil[x,"IFR_x"]," x IFR; ",IFR_mat_fil[x,"Slope_x"]," x Slope"))
})
dev.off()



# Plot modelled combined prevalence
PrevPlots <- function(model_fit){
  sero_pcr <- seroprev_df(model_fit)
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

  return(list(p5,p6,p7))
}

Res1_minus_null <- Res1
Res1_minus_null$X46 <- NULL

pdf("analysis/figures/37_Prev_Plots_2.pdf")
lapply(Res1_minus_null, function(x){PrevPlots(x)[[3]] + xlab(paste0(IFR_mat_fil[c(1,33,37),"IFR_x"][x]," x IFR; ",IFR_mat_fil[c(1,33,37),"Slope_x"][x]," x Slope"))})
dev.off()


pdf(file = "analysis/figures/37_Prev_Plots_selected.pdf", width = 5, height = 4)
lapply(c(1,33,37,72), function(x){
  PrevPlots(Res1[[x]])[[3]] + xlab(paste0(IFR_mat_fil[x,"IFR_x"]," x IFR; ",IFR_mat_fil[x,"Slope_x"]," x Slope"))
})
dev.off()

pdf(file = "analysis/figures/37_Prev_Plots_selected_earlier.pdf", width = 5, height = 4)
lapply(1:3, function(x){
  PrevPlots(Res1[[x]])[[3]] + xlab(paste0(IFR_mat_fil[c(1,33,37),"IFR_x"][x]," x IFR; ",IFR_mat_fil[c(1,33,37),"Slope_x"][x]," x Slope"))
})
dev.off()


Diagnostic_Plot_06(fit_Model_list = list(Res1[[37]]), fit_num = 1, IFRvals = IFR_mat_fil[37,], ll_type = "ll_pois")


































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



  p5 <- pcr_fit_plot(pcr_sero_data) + ggtitle(paste("PCR prevalence\nbinom ll =", round(sero_pcr_ll$ll_pcr),1)) + theme(plot.title = element_text(size = 10))
  p6 <- sero_fit_plot(pcr_sero_data) + ggtitle(paste("Seroprevalence \nbinom ll =", round(sero_pcr_ll$ll_sero),1)) + theme(plot.title = element_text(size = 10))
  p7 <- combined_fit_plot(pcr_sero_data) + ggtitle(paste("Combined prevalence \nbinom ll =", round(comb_ll$ll_combined),1)) + theme(plot.title = element_text(size = 10))


  title_gg <- ggplot() +
    labs(title = paste0("IFR x ", IFRvals$IFR_x[fit_num],", Slope x ", IFRvals$Slope_x[fit_num]))
  # browser()
  cowplot::plot_grid(title_gg, cowplot::plot_grid(p1,p2,p2b,p3,p4,p4b,p5,p6,p7), ncol = 1, rel_heights = c(0.05, 1))
}


# fit_Model_Test <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-02-06_Set_1_nbin_weeks.rds")
IFRvals_fil <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds"),]
# IFR_x <- IFRvals$IFR_x; IFR_slope <- IFRvals$Slope_x
# readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds")

Diagnostic_Plot(fit_num = 2, fit_Model_list = fit_Model_list, IFRvals = IFRvals_fil)







