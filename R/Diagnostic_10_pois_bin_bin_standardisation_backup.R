Diagnostic_Plot_10 <- function(fit_num, fit_Model_list, IFRvals,
                               Return_likelihoods_only = F,
                               Return_data_only_plots = F,
                               full_drjacoby_fit_data = NULL,
                               filter_set = NULL
                               ){
  gc();print(fit_num)
  fit_Model <- fit_Model_list[[fit_num]]
  if(is.null(fit_Model)){return(NA)}

  ################################################
  ################################################
  ### First, load inputs
  pars_obs <- fit_Model$pmcmc_results$inputs$pars_obs
  pcr_det <- pars_obs$pcr_det
  pcr_det_PM <- pars_obs$pcr_det_PM
  if(is.null(pcr_det_PM)){pcr_det_PM <- pars_obs$pcr_det}
  index <- squire:::odin_index(fit_Model$model)
  FullData <- pars_obs$combined_data$Comb_data
  dfj_mcmc_data <- pars_obs$combined_data$dfj_mcmc_data

  ################################################
  ################################################
  ## Get Susceptibles, Infections, Cumulative infections, pcr_positive and pcr_perc
  Mod_Age_Deaths_Lus_pcr <- fit_Model$output[,c(index$D,index$S),] %>%
    melt(varnames = c("date","var","Replicate"), value.name = "value") %>%
    mutate(Age_gr = as.numeric(gsub(".*?([0-9]+).*", '\\1', var)),
           var = substr(var, 1, 1),
           date = as.Date(date)) %>%
    dcast(... ~ var, value.var="value") %>%
    dplyr::rename(Mod_cd_Lus = "D", Sus = "S") %>%
    group_by(Age_gr, Replicate) %>%
    tidyr::replace_na(list(Mod_cd_Lus = 0)) %>%
    mutate(Sus = ifelse(is.na(Sus), max(Sus, na.rm=T), Sus)) %>%
    mutate(cum_infs = max(Sus, na.rm = T)-Sus,
           infs = c(0, diff(max(Sus, na.rm = T)-Sus)),
           pcr_pos = cma:::roll_func_10(infs, pcr_det_PM),
           pcr_perc = pcr_pos/max(Sus,na.rm = T))

  Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus_pcr %>%
    filter(date > "2020-06-10") %>%
    mutate(Week_gr = as.numeric(cut.Date(date, breaks = "1 week", labels = 1:17))) %>%
    group_by(Age_gr, Week_gr, Replicate) %>%
    summarise(Mod_cd_Lus = max(Mod_cd_Lus),
              pcr_perc = pcr_perc[3], # Wednesday - middle of the study week
              infs = sum(infs)) %>%
    ungroup() %>% group_by(Age_gr, Replicate) %>%
    mutate(Mod_cd_Lus = c(0,diff(Mod_cd_Lus))) %>% #,
    filter(Week_gr != 1) %>%
    mutate(Week_gr = Week_gr-1) %>%
    merge(FullData)


  ################################################
  ################################################
  ### Calculate Likelihoods
  ## Integrate over mcmc samples
  LL_Distributions_diag <- lapply(1:length(full_drjacoby_fit_data), function(y){
    tmp_df <- merge(Mod_Age_Deaths_Lus, full_drjacoby_fit_data[[y]]) %>%
      arrange(Week_gr, Age_gr)

    # Calculate the poisson likelihood of burial registrations from Modelled Lusaka Covid deaths (factored by the proportional weekly changes in <5)
    # Added to the mcmc estimates for Lusaka level baseline deaths (these are already factored by the weekly changes based on <5s)
    tmp_df$Mod_tot_ds_Lus <- tmp_df$Mod_cd_Lus + tmp_df$Mort_ncd_mcmc/tmp_df$ag1std
    tmp_df$Mod_tot_ds_bur_regs <- tmp_df$Mod_cd_Lus*tmp_df$ag1std + tmp_df$Mort_ncd_mcmc
    tmp_df$l_pois <- dpois(x = tmp_df$Bur_regs, lambda = tmp_df$Mod_tot_ds_bur_regs, log = F)

    tmp_df$Est_Lus_ds <- tmp_df$Bur_regs/tmp_df$ag1std

    tmp_df$Mod_ncd_Lus <- tmp_df$Est_Lus_ds - tmp_df$Mod_cd_Lus
    tmp_df$Mod_ncd_Lus <- ifelse(tmp_df$Mod_ncd_Lus<0,0,tmp_df$Mod_ncd_Lus)
    tmp_df$Mod_pos_ds_Lus <- tmp_df$Mod_cd_Lus+tmp_df$Mod_ncd_Lus*tmp_df$pcr_perc # total positive deaths (covid and coincidental)
    tmp_df$Pos_prev <- ifelse(tmp_df$Mod_pos_ds_Lus<=tmp_df$Est_Lus_ds,tmp_df$Mod_pos_ds_Lus/tmp_df$Est_Lus_ds, 0.999)
    tmp_df$l_binom <- dbinom(tmp_df$PosTests, tmp_df$Samples, prob = tmp_df$Pos_prev, log = F)

    tmp_df$Sample_no <- y
    tmp_df <- tmp_df[c("Sample_no", "Week_gr", "Age_gr","Replicate", "l_pois", "l_binom",
                       "Mod_cd_Lus","infs","pcr_perc",
                       "Mod_tot_ds_bur_regs","Mod_tot_ds_Lus","Mod_pos_ds_Lus",
                       "Bur_regs","Est_Lus_ds","Pos_prev","Mort_ncd_mcmc")]
  })

  Res_2d <- data.table::rbindlist(LL_Distributions_diag)
  rm(LL_Distributions_diag)
  gc()

  if(is.null(filter_set)){Res_2d <- Res_2d}
  # if(pars_obs$lld =="full_data"){Res_2d <- Res_2d}
  # if(pars_obs$lld =="remove_weeks_4_and_5"){Res_2d <- Res_2d %>% dplyr::filter(Week_gr != 4,
  #                                                                                  Week_gr != 5)}
  # if(pars_obs$lld =="remove_under_5s"){Res_2d <- Res_2d %>% dplyr::filter(Age_gr != 1)}
  if(filter_set =="remove_weeks_4_and_5_and_age_under_5s"){Res_2d_Fil <- Res_2d %>% dplyr::filter(Age_gr != 1,
                                                                                                    Week_gr != 4,
                                                                                                    Week_gr != 5)}
  # Get overall ll for poisson and bin
  ll_ov_pois_bin <- Res_2d_Fil %>% group_by(Replicate, Sample_no) %>%
    summarise(ll_pois_prod = log(Brobdingnag::prod(Brobdingnag::as.brob(l_pois))),
              l_binom_prod = prod(l_binom)) %>%
    ## Then take the mean of everything?
    ungroup() %>%
    summarise(ll_pois_mean_overall = log(Brobdingnag::sum(exp(Brobdingnag::as.brob(ll_pois_prod)))/length(ll_pois_prod)),
              ll_bin_mean_overall = log(mean(l_binom_prod)))


  ################################################
  ################################################
  ## pcr/sero prevalence likelihoods
  sero_pcr <- seroprev_df(fit_Model)
  sero_pcr_ll <- sero_pcr %>% filter(as.Date(date) %in% seq.Date(from = pars_obs$sero_df$date_start, to = pars_obs$sero_df$date_end, by = 1)) %>%
    group_by(replicate) %>%
    summarise(pcr_perc = mean(pcr_perc),
              sero_perc = mean(sero_perc)) %>% ungroup() %>%
    mutate(ll_pcr = dbinom(x = pars_obs$pcr_df$pos_tests, size = pars_obs$pcr_df$samples, prob = pcr_perc),
           ll_sero = dbinom(x = pars_obs$sero_df$pos_tests, size = pars_obs$sero_df$samples, prob = sero_perc)) %>%
    summarise(ll_pcr = log(mean(ll_pcr)),
              ll_sero = log(mean(ll_sero)))

  ################################################
  ################################################
  lls <- list(
    ll_bin = ll_ov_pois_bin$ll_bin_mean_overall,
    ll_pois = ll_ov_pois_bin$ll_pois_mean_overall,
    ll_pcr = sero_pcr_ll$ll_pcr,
    ll_sero = sero_pcr_ll$ll_sero,
    ll_tot = sum(ll_ov_pois_bin$ll_bin_mean_overall, ll_ov_pois_bin$ll_pois_mean_overall, sero_pcr_ll$ll_pcr, sero_pcr_ll$ll_sero))


  if(Return_likelihoods_only){return(lls)}


  ################################################
  ################################################

  # Calculate the mean for all other variables
  ll_aw <- Res_2d %>% mutate(Mod_coin_ds_Lus = Mod_pos_ds_Lus - Mod_cd_Lus) %>%
    group_by(Age_gr, Week_gr) %>%
    summarise_at(c("l_binom", "Mod_cd_Lus", "Mod_pos_ds_Lus","Bur_regs","Est_Lus_ds","pcr_perc","infs",
                   "Pos_prev","Mod_coin_ds_Lus","Mod_tot_ds_bur_regs","Mod_tot_ds_Lus","Mort_ncd_mcmc"), list(median = median, ci = bayestestR::ci), na.rm = TRUE) %>%
    merge(unique(Mod_Age_Deaths_Lus %>% select(Week_gr, date)))

  ll_week <- Res_2d %>% select(Sample_no, Week_gr, Age_gr, Replicate, Mod_pos_ds_Lus, Mod_cd_Lus, Est_Lus_ds, Mod_tot_ds_bur_regs, Mod_tot_ds_Lus, Mort_ncd_mcmc) %>%
    group_by(Sample_no, Week_gr, Replicate) %>%
    summarise(Mod_pos_ds_Lus = sum(Mod_pos_ds_Lus),
              Mod_cd_Lus = sum(Mod_cd_Lus),
              Est_Lus_ds = sum(Est_Lus_ds),
              Mod_tot_ds_bur_regs = sum(Mod_tot_ds_bur_regs),
              Mod_tot_ds_Lus = sum(Mod_tot_ds_Lus),
              Mort_ncd_mcmc = sum(Mort_ncd_mcmc)
    ) %>% ungroup() %>%
    mutate(Mod_coin_ds_Lus = Mod_pos_ds_Lus - Mod_cd_Lus,
           Pos_prev = Mod_pos_ds_Lus/Est_Lus_ds,
           Pos_prev_causal = Mod_cd_Lus/Est_Lus_ds,
           Pos_prev_coin = Mod_coin_ds_Lus/Est_Lus_ds
    ) %>%
    group_by(Week_gr) %>%
    summarise_at(c("Mod_cd_Lus", "Mod_pos_ds_Lus","Est_Lus_ds","Mod_coin_ds_Lus",
                   "Pos_prev", "Pos_prev_causal", "Pos_prev_coin", "Mod_tot_ds_bur_regs","Mod_tot_ds_Lus","Mort_ncd_mcmc"), list(median = median, ci = bayestestR::ci), na.rm = TRUE) %>%
    merge(unique(Mod_Age_Deaths_Lus %>% select(Week_gr, date)))

  # Group by week
  Mod_Age_Deaths_Lus_Av_Week <- ll_week %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
    merge(FullData %>%
            group_by(Week_gr) %>%
            select(-date, -Age_gr) %>%
            summarise_all(sum, na.rm=T), all = T)


  ll_age <- Res_2d %>% select(Sample_no, Week_gr, Age_gr, Replicate, Mod_pos_ds_Lus, Mod_cd_Lus, Est_Lus_ds, Mod_tot_ds_bur_regs, Mod_tot_ds_Lus, Mort_ncd_mcmc) %>%
    group_by(Sample_no, Age_gr, Replicate) %>%
    summarise(Mod_pos_ds_Lus = sum(Mod_pos_ds_Lus),
              Mod_cd_Lus = sum(Mod_cd_Lus),
              Est_Lus_ds = sum(Est_Lus_ds),
              Mod_tot_ds_bur_regs = sum(Mod_tot_ds_bur_regs),
              Mod_tot_ds_Lus = sum(Mod_tot_ds_Lus),
              Mort_ncd_mcmc = sum(Mort_ncd_mcmc)
    ) %>% ungroup() %>%
    mutate(Mod_coin_ds_Lus = Mod_pos_ds_Lus - Mod_cd_Lus,
           Pos_prev = Mod_pos_ds_Lus/Est_Lus_ds,
           Pos_prev_causal = Mod_cd_Lus/Est_Lus_ds,
           Pos_prev_coin = Mod_coin_ds_Lus/Est_Lus_ds
    ) %>%
    group_by(Age_gr) %>%
    summarise_at(c("Mod_cd_Lus", "Mod_pos_ds_Lus","Est_Lus_ds","Mod_coin_ds_Lus",
                   "Pos_prev", "Pos_prev_causal", "Pos_prev_coin", "Mod_tot_ds_bur_regs", "Mod_tot_ds_Lus","Mort_ncd_mcmc"), list(median = median, ci = bayestestR::ci), na.rm = TRUE) %>%
    merge(unique(Mod_Age_Deaths_Lus %>% select(Age_gr)))


  Mod_Age_Deaths_Lus_Av_Age <- ll_age %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
    merge(FullData %>%
            group_by(Age_gr) %>%
            select(-date, -Week_gr) %>%
            summarise_all(sum, na.rm=T), all = T) %>%
    mutate(Age_gr_label = ifelse(Age_gr %in% 1:16,
                                 paste0(Age_gr*5-5, "-",Age_gr*5-1),
                                 "80+"))




  ################################################
  ################################################
  ## Plot data first  ##
  ################################################
  ################################################
  # Res_2d <- Res_2d
  # Facet labels: Simple
  Age_groups.labs_Simple <-  Mod_Age_Deaths_Lus_pcr %>% group_by(Age_gr) %>% select(Age_gr) %>% unique() %>%
    mutate(Age_gr_lab = ifelse(Age_gr %in% 1:16, paste0("Age: ",Age_gr*5-5,"-",Age_gr*5-1),
                               "Age: 80+")) %>%
    ungroup() %>% arrange(Age_gr) %>% select(Age_gr_lab) %>%
    unlist()
  names(Age_groups.labs_Simple) <- 1:17



  ################################################
  # Burial Registration Plot
  # Calculate the mean pois for each age and week
  ll_aw_pois <- Res_2d %>% group_by(Age_gr, Week_gr) %>%
    summarise(ll_pois_mean_age_week = log(Brobdingnag::sum(Brobdingnag::as.brob(l_pois))/length(l_pois)),
              ll_pois_min_age_week = log(min(l_pois)),
              ll_pois_max_age_week = log(max(l_pois))
    ) %>% merge(FullData, all = T)

  # Facet labels: Including Pois ll
  Age_groups.labs_pois <- ll_aw_pois %>% group_by(Age_gr) %>%
    summarise(ll_pois_mean = sum(ll_pois_mean_age_week)) %>%
    mutate(Age_gr_lab = paste0(Age_groups.labs_Simple[Age_gr])) %>%
    # mutate(Age_gr_lab = paste0(Age_groups.labs_Simple[Age_gr],"; Pois ll = ",round(ll_pois_mean[Age_gr],1))) %>%
    select(Age_gr_lab) %>%
    unlist()
  names(Age_groups.labs_pois) <- 1:17

  # Poisson_Figure <- ggplot(ll_aw, aes(x = date)) +
    # facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs_pois)) +
    # geom_point(aes(y = Bur_regs_mean, color = "Burial registrations")) +
    # xlab("Date") + ylab("Deaths") +
    # ggtitle(
      # paste0(ifelse(Return_data_only_plots,"",
                    # paste0("IFR x", round(IFRvals$IFR_x[fit_num],2),
                           # ", Slope x", round(IFRvals$Slope_x[fit_num],2))),
                           # "\nLusaka weekly burial registrations")) +
    # theme_minimal() +
    # theme(plot.title = element_text(size = 10),
          # legend.position = c(1,0), legend.justification = c(1,0),
          # legend.key = element_rect(fill = "white", linetype = 0))

  Poisson_Figure <- ggplot(ll_aw, aes(x = date)) +
    # facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs_pois)) +
    geom_point(data = Mod_Age_Deaths_Lus_Av_Week, aes(y = Bur_regs)) +#, color = "Burial registrations")) +
    xlab("Date") + ylab("Deaths") +
    # ggtitle(
    # paste0(ifelse(Return_data_only_plots,"",
    # paste0("IFR x", round(IFRvals$IFR_x[fit_num],2),
    # ", Slope x", round(IFRvals$Slope_x[fit_num],2))),
    # "\nLusaka weekly burial registrations")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10),
          legend.position = c(1,0), legend.justification = c(1,0),
          legend.key = element_rect(fill = "white", linetype = 0))


  ## Summarise by age and week
  Poisson_Figure_weeks <- ggplot(Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs_pois)) +
    geom_point(aes(y = Bur_regs, color = "Burial registrations")) +
    xlab("Date") + ylab("Deaths") +
    # ggtitle(
      # paste0(ifelse(Return_data_only_plots,"",
                    # paste0("IFR x", round(IFRvals$IFR_x[fit_num],2),
                           # ", Slope x", round(IFRvals$Slope_x[fit_num],2))),"\n",
                      # "Lusaka weekly burial registrations"))) +
    theme_minimal() +
    theme(#plot.title = element_text(size = 10),
          # legend.position = c(1,0), legend.justification = c(1,0),
          legend.key = element_rect(fill = "white", linetype = 0))

  Mod_Age_Deaths_Lus_Av_Age$Age_gr_label <- factor(Mod_Age_Deaths_Lus_Av_Age$Age_gr_label, levels = Mod_Age_Deaths_Lus_Av_Age$Age_gr_label)


  Poisson_Figure_age <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr_label)) +
    # facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs_pois)) +
    geom_point(aes(y = Bur_regs, color = "Burial registrations")) +
    xlab("Age") + ylab("Deaths") +
    # ggtitle(
      # paste0(ifelse(Return_data_only_plots,"",
                    # paste0("IFR x", round(IFRvals$IFR_x[fit_num],2),
                           # ", Slope x", round(IFRvals$Slope_x[fit_num],2))),"\n",
             # "Lusaka burial registrations by age"))) +
    theme_minimal() +
    theme(#plot.title = element_text(size = 10),
          legend.position = c(1,0), legend.justification = c(1,0),
          legend.key = element_rect(fill = "white", linetype = 0),
          axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))




  ################################################
  ## Plot 6: Weekly prevalence fit
  Week_prev_plot <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Prevalence on positive in sample
    geom_point(aes(y = 100*PosTests/Samples)) +#, col = as.factor(Unreliable_points_key))) +
    geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
                      ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
    coord_cartesian(ylim = c(0,100)) +
    xlab("Date") + ylab("+ve %")  +
    # ggtitle(paste0(ifelse(Return_data_only_plots,"",
                          # paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),
                                 # ", Slope x ", round(IFRvals$Slope_x[fit_num],2))),"\n",
                          # "Post-mortem weekly PCR prev. \nUTH mortuary"))) +
    theme(plot.title = element_text(size = 10)) +
    theme_minimal()
  # ggpubr::theme_pubr(legend = "bottom")


  ## Plot 7: Age group prevalence fits
  Age_prev_plot <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr_label)) +
    # Prevalence on positive in sample
    geom_point(aes(y = 100*PosTests/Samples)) +
    geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
                      ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
    xlab("Age") + ylab("+ve %")  +
    # ggtitle("Post-mortem PCR prev. by age\nUTH mortuary") +
    theme(plot.title = element_text(size = 10)) +
    theme_minimal() +
    # scale_color_manual(name = NULL, breaks = c("Total +ve deaths","Causal deaths","Coincidental deaths"), values = c("black","darkred","darkblue")) +
    scale_x_discrete(limits = c(paste0(1:16*5-5, "-",1:16*5-1),"80+")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))

  pcr_sero_data <- Summ_sero_pcr_data(sero_pcr)
  PCR_sero_prev_plot <- ggplot(pcr_sero_data, aes(x = date, y = mean_pcr)) +
    geom_point(aes(x= as.Date(pars_obs$pcr_df$date_start) + 0.5*(as.Date(pars_obs$pcr_df$date_end)-as.Date(pars_obs$pcr_df$date_start)),
                   y=100*pars_obs$pcr_df$pos_tests/pars_obs$pcr_df$samples, color = "PCR % (Lusaka)"),  size = 2) +
    geom_errorbar(aes(ymin=100*Hmisc::binconf(pars_obs$pcr_df$pos_tests,pars_obs$pcr_df$samples)[,"Lower"],
                      ymax=100*Hmisc::binconf(pars_obs$pcr_df$pos_tests,pars_obs$pcr_df$samples)[,"Upper"],
                      x=as.Date(pars_obs$pcr_df$date_start) + 0.5*(as.Date(pars_obs$pcr_df$date_end)-as.Date(pars_obs$pcr_df$date_start)),
                      width=10,color = "PCR % (Lusaka)")) +
    geom_errorbarh(aes(xmin=as.Date(pars_obs$pcr_df$date_start),xmax=as.Date(pars_obs$pcr_df$date_end),y=100*pars_obs$pcr_df$pos_tests/pars_obs$pcr_df$samples, height=0,color = "PCR % (Lusaka)")) +

    geom_point(aes(x= as.Date(pars_obs$sero_df$date_start) + 0.5*(as.Date(pars_obs$sero_df$date_end)-as.Date(pars_obs$sero_df$date_start)),
                   y=100*pars_obs$sero_df$pos_tests/pars_obs$sero_df$samples, color = "Sero % (Lusaka)"), size = 2) +
    geom_errorbar(aes(ymin=100*Hmisc::binconf(pars_obs$sero_df$pos_tests,pars_obs$sero_df$samples)[,"Lower"],
                      ymax=100*Hmisc::binconf(pars_obs$sero_df$pos_tests,pars_obs$sero_df$samples)[,"Upper"],
                      x=as.Date(pars_obs$sero_df$date_start) + 0.5*(as.Date(pars_obs$sero_df$date_end)-as.Date(pars_obs$sero_df$date_start)),
                      width=10,color = "Sero % (Lusaka)")) +
    geom_errorbarh(aes(xmin=as.Date(pars_obs$pcr_df$date_start),xmax=as.Date(pars_obs$pcr_df$date_end),y=100*pars_obs$sero_df$pos_tests/pars_obs$sero_df$samples, height=0,color = "Sero % (Lusaka)")) +
    ylab(paste0("+ve %")) +
    coord_cartesian(xlim = c(as.Date("2020-04-15"), as.Date("2020-10-01")),
                    ylim = c(0, 28)) +
    xlab("Date")+
    theme_minimal() +
    # ggtitle(paste("\nPopulation prevalence\nLusaka")) +
    scale_color_manual(name=NULL,
                       breaks = c("PCR % (Lusaka)", "Sero % (Lusaka)"),
                       values = c("darkgoldenrod2","chartreuse4"))





  if(Return_data_only_plots){

    Poisson_Figure <- Poisson_Figure + scale_color_manual(name=NULL,
                                                          breaks = "Burial registrations",
                                                          values = "black",
                                                          guide = guide_legend(override.aes = list(linetype = 0,shape = 16,alpha = 1)))

    Prev_plot <- cowplot::plot_grid(
      cowplot::plot_grid(Week_prev_plot + theme(legend.position = "none"), Age_prev_plot  + theme(legend.position = "none"), PCR_sero_prev_plot + theme(legend.position="none"), nrow = 1),
      cowplot::plot_grid(ggpubr::get_legend(Week_prev_plot),ggpubr::get_legend(PCR_sero_prev_plot), rel_widths = c(2,1), nrow = 1),
      nrow = 2, rel_heights = c(1,0.2))
    return(list(Poisson_Figure,Prev_plot))
  }


  # Poisson_Figure <- Poisson_Figure +
  #   geom_line(aes(y=Mod_tot_ds_bur_regs_mean, color = "Model fit")) +
  #   geom_ribbon(aes(ymin=Mod_tot_ds_bur_regs_ci$CI_low, ymax=Mod_tot_ds_bur_regs_ci$CI_high), alpha=0.3) +
  #   geom_line(aes(y=Mod_tot_ds_Lus_mean, color = "Model fit (Lusaka)"),linetype="dashed", alpha = 0.5) +
  #   geom_ribbon(aes(ymin=Mod_tot_ds_Lus_ci$CI_low, ymax=Mod_tot_ds_Lus_ci$CI_high), alpha=0.1) +
  #   scale_color_manual(name=NULL,
  #                      breaks = c("Burial registrations", "Model fit", "Model fit (Lusaka)"),
  #                      values = c("black","black","black"),
  #                      guide = guide_legend(override.aes = list(linetype = c(0,1,2),
  #                                                               shape = c(16,NA,NA),
  #                                                               alpha = c(1,1,0.5)))) +
  #   guides(colour = guide_legend(nrow = 1))

  Poisson_Figure <- Poisson_Figure +
    # geom_line(aes(y=Mod_tot_ds_bur_regs_mean)) +
    geom_area(aes(y=Mod_tot_ds_bur_regs_median, fill = as.factor(Age_gr)), position = "stack") +
    geom_point(data = Mod_Age_Deaths_Lus_Av_Week, aes(y = Bur_regs)) +#, color = "Burial registrations")) +
    viridis::scale_fill_viridis(discrete = T, option = "H") +
    viridis::scale_color_viridis(discrete = T, option = "H") +
    # geom_ribbon(aes(ymin=Mod_tot_ds_bur_regs_ci$CI_low, ymax=Mod_tot_ds_bur_regs_ci$CI_high, fill = as.factor(Age_gr)), alpha=0.2) +
    geom_line(data = Mod_Age_Deaths_Lus_Av_Week, aes(y=Mod_tot_ds_Lus_median),linetype="dashed", alpha = 0.5) +
    geom_line(aes(y=Mod_cd_Lus_median+Mort_ncd_mcmc_median, col = as.factor(Age_gr)),linetype="dashed", alpha = 0.5) +
    geom_ribbon(aes(ymin=Mod_tot_ds_Lus_ci$CI_low, ymax=Mod_tot_ds_Lus_ci$CI_high), alpha=0.1) +
    scale_color_manual(name=NULL,
                       breaks = c("Burial registrations", "Model fit", "Model fit (Lusaka)"),
                       values = c("black","black","black"),
                       guide = guide_legend(override.aes = list(linetype = c(0,1,2),
                                                                shape = c(16,NA,NA),
                                                                alpha = c(1,1,0.5)))) +
    guides(colour = guide_legend(nrow = 1))

# browser()
  Poisson_Figure_weeks <- Poisson_Figure_weeks +
    geom_line(aes(y=Mod_tot_ds_bur_regs_median, color = "Model fit")) +
    geom_ribbon(aes(ymin=Mod_tot_ds_bur_regs_ci$CI_low, ymax=Mod_tot_ds_bur_regs_ci$CI_high), alpha=0.3) +
    geom_line(aes(y=Mod_tot_ds_Lus_median, color = "Model fit (Lusaka)"),linetype="dashed", alpha = 0.5) +
    geom_ribbon(aes(ymin=Mod_tot_ds_Lus_ci$CI_low, ymax=Mod_tot_ds_Lus_ci$CI_high), alpha=0.1) +
    scale_color_manual(name=NULL,
                       breaks = c("Burial registrations", "Model fit", "Model fit (Lusaka)"),
                       values = c("black","black","black"),
                       guide = guide_legend(override.aes = list(linetype = c(0,1,2),
                                                                shape = c(16,NA,NA),
                                                                alpha = c(1,1,0.5)),
                                            nrow = 1))

  Poisson_Figure_age <- Poisson_Figure_age +
    geom_line(aes(y=Mod_tot_ds_bur_regs_median, color = "Model fit", group = 1)) +
    geom_ribbon(aes(ymin=Mod_tot_ds_bur_regs_ci$CI_low, ymax=Mod_tot_ds_bur_regs_ci$CI_high, group = 1), alpha=0.3) +
    geom_line(aes(y=Mod_tot_ds_Lus_median, color = "Model fit (Lusaka)", group = 1),linetype="dashed", alpha = 0.5) +
    geom_ribbon(aes(ymin=Mod_tot_ds_Lus_ci$CI_low, ymax=Mod_tot_ds_Lus_ci$CI_high, group = 1), alpha=0.1) +
    scale_color_manual(name=NULL,
                       breaks = c("Burial registrations", "Model fit", "Model fit (Lusaka)"),
                       values = c("black","black","black"),
                       guide = guide_legend(override.aes = list(linetype = c(0,1,2),
                                                                shape = c(16,NA,NA),
                                                                alpha = c(1,1,0.5)),
                                            nrow = 1)) +
    guides(colour = guide_legend(nrow = 1))


  Week_prev_plot <- Week_prev_plot +
    geom_line(aes(y = 100*Pos_prev_coin_median, col = "Coincidental deaths"),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Pos_prev_coin_ci$CI_low, ymax=100*Pos_prev_coin_ci$CI_high), alpha=0.3, fill = "darkblue") +
    geom_line(aes(y = 100*Pos_prev_causal_median,col = "Causal deaths"),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Pos_prev_causal_ci$CI_low, ymax=100*Pos_prev_causal_ci$CI_high), alpha=0.3, fill = "darkred") +
    geom_line(aes(y = 100*Pos_prev_median, col = "Total +ve deaths"),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Pos_prev_ci$CI_low, ymax=100*Pos_prev_ci$CI_high), alpha=0.3) +
    scale_color_manual(name = NULL, breaks = c("Total +ve deaths","Causal deaths","Coincidental deaths"),
                       values = c("black","darkred","darkblue")) +
    guides(colour = guide_legend(nrow = 1))

  Age_prev_plot <- Age_prev_plot +
    geom_line(aes(y = 100*Pos_prev_coin_median, col = "Coincidental deaths", group = 1),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Pos_prev_coin_ci$CI_low, ymax=100*Pos_prev_coin_ci$CI_high, group = 1), alpha=0.3, fill = "darkblue") +
    geom_line(aes(y = 100*Pos_prev_causal_median,col = "Causal deaths", group = 1),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Pos_prev_causal_ci$CI_low, ymax=100*Pos_prev_causal_ci$CI_high, group = 1), alpha=0.3, fill = "darkred") +
    geom_line(aes(y = 100*Pos_prev_median, col = "Total +ve deaths", group = 1),linetype="dashed") +
    geom_ribbon(aes(ymin=100*Pos_prev_ci$CI_low, ymax=100*Pos_prev_ci$CI_high, group = 1), alpha=0.3) +
    scale_color_manual(name = NULL, breaks = c("Total +ve deaths","Causal deaths","Coincidental deaths"),
                       values = c("black","darkred","darkblue")) +
    guides(colour = guide_legend(nrow = 1))

  PCR_sero_prev_plot <- PCR_sero_prev_plot +
    geom_line(aes(x=date, y=median_pcr, color = "PCR % (Lusaka)"),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=ci_low_pcr, ymax=ci_high_pcr), alpha=0.3, fill = "darkgoldenrod1")+
    geom_line(aes(x=date, y=median_sero, color = "Sero % (Lusaka)"),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=ci_low_sero, ymax=ci_high_sero), alpha=0.3, fill = "chartreuse4") +
    guides(colour = guide_legend(nrow = 1))

  # Prev_plot <- cowplot::plot_grid(
  #   cowplot::plot_grid(Week_prev_plot + theme(legend.position = "none"), Age_prev_plot  + theme(legend.position = "none"), PCR_sero_prev_plot + theme(legend.position="none"), nrow = 1),
  #   cowplot::plot_grid(ggpubr::get_legend(Week_prev_plot),ggpubr::get_legend(PCR_sero_prev_plot), rel_widths = c(2,1), nrow = 1),
  #   nrow = 2, rel_heights = c(1,0.2))


  # Burial_Plots <- cowplot::plot_grid(
  #   cowplot::plot_grid(Poisson_Figure_weeks + theme(legend.position = "none"), Poisson_Figure_age  + theme(legend.position = "none"), p_Rt_Reff_a+ theme(legend.position = "none"), nrow = 1),
  #   cowplot::plot_grid(ggpubr::get_legend(Poisson_Figure_weeks),ggpubr::get_legend(p_Rt_Reff_a), rel_widths = c(2,1), nrow = 1),
  #   nrow = 2, rel_heights = c(1,0.2))
  #


  ###############################################################
  ###############################################################
  ## Extra Plots
  ## Plot 1: PCR perc by age
  # PCR_perc <- Mod_Age_Deaths_Lus_pcr %>% group_by(Age_gr, date) %>%
    # summarise(pcr_perc = median(pcr_perc))


  # Plot
  # p_pcr_perc_age <- ggplot(PCR_perc, aes(x = date, y = pcr_perc, col = as.factor(Age_gr))) + geom_line() +
  #   # facet_wrap(~Age_gr, labeller = labeller(Age_gr = Age_groups.labs_Simple)) +
  #   theme_minimal() +
  #   xlab("Date") + ylab("+ve %") +
  #   # ggtitle(paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),
  #   #                ", Slope x ", round(IFRvals$Slope_x[fit_num],2),
  #   #                "\nModelled PCR % by age group (mid-week)")) +
  #   # ggtitle(paste0("Modelled PCR %")) +
  #   viridis::scale_color_viridis(discrete = T, labels = c(Age_groups.labs_Simple)) +
  #   theme(legend.title = element_blank())
  # # scale_color_manual()


  ###############################################################
  ###############################################################
  ## Plot 2: Rt and Reff
  p_Rt_Reff_a <- rt_plot_immunity(fit_Model)$plot +
    scale_fill_manual("",values = c("Reff" = "#48996b","Rt" ="#3f8da7"), labels = c(expression("R"[eff]),expression("R"[t]))) +
    guides(fill = guide_legend(nrow = 1))
    # ggtitle(label = expression("R"[t]*" and R"[eff]))

  # p_Rt_Reff_b <- fit_Model$replicate_parameters[,paste0("Rt_rw_",1:10)] %>%
  #   melt(id.vars = NULL) %>%
  #   ggplot() +
  #   facet_wrap(~variable, ncol = 2) +
  #   geom_histogram(aes(x = value), bins = 30) +
  #   theme_minimal() +
  #   geom_vline(xintercept = 0, linetype = "dashed") +
  #   theme(strip.text.x = element_text(size = 6),
  #         axis.text=element_text(size=6))

  # p_Rt_Reff <- cowplot::plot_grid(p_Rt_Reff_a, p_Rt_Reff_b, rel_widths = c(1, 0.6))




  ################################################
  ## Plot 9: Excess Mortality Plot against squire predictions
  Pop_str <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
  Pop_str_df <- data.frame(Age_gr = 1:17, Pop_str = Pop_str)
  # ExcessMort_mcmc <- readRDS("analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week.rds")
  ExcessMort_mcmc <- readRDS("analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week_Checked.rds")

  ## Add this to true covid deaths
  Plot_data_Excess_Pred <- ExcessMort_mcmc %>% merge(Pop_str_df) %>% merge(ll_aw) %>% filter(Age_gr!=1) %>%
    mutate(date = lubridate::floor_date(date, "week", 1))
  p_excess_comp <- ggplot(Plot_data_Excess_Pred, aes(x = date, y = excess_Median/Pop_str*1000)) + geom_line(aes(col = "Excess mortality")) +
    geom_ribbon(aes(ymin = excess_CI_low/Pop_str*1000, ymax = excess_CI_high/Pop_str*1000), alpha = 0.4) +
    facet_wrap(~Age_gr, labeller = labeller(Age_gr = Age_groups.labs_Simple[-1])) +
    # geom_line(aes(y = Mod_cd_Lus_median/Pop_str*1000, col = "Model output (Lusaka)"))  +
    # geom_ribbon(aes(ymin = Mod_cd_Lus_min/Pop_str*1000, ymax = Mod_cd_Lus_max/Pop_str*1000), alpha = 0.4, fill = "darkblue") +
    # geom_line(aes(y = Mod_cd_80_median/Pop_str*1000), col = "darkred") +
    geom_line(aes(y = Mod_cd_Lus_median/Pop_str*1000, col = "Modelled true c19 output (UTH)")) +
    geom_ribbon(aes(ymin = Mod_cd_Lus_ci$CI_low/Pop_str*1000, ymax = Mod_cd_Lus_ci$CI_high/Pop_str*1000), alpha = 0.4, fill = "darkred") +
    theme_minimal() +
    theme(legend.position="bottom") +
    ylab("Per capita deaths (/1000)") +
    xlab("Date") +
    # ggtitle(ifelse(is.numeric(IFRvals$IFR_x),
    #                paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),
    #                       ", Slope x ", round(IFRvals$Slope_x[fit_num],2),
    #                       "\nCOVID-19 excess mortality: Comparison of MCMC results with squire results"),
    #                paste0(IFRvals$IFR_x[fit_num],"; ",IFRvals$Slope_x[fit_num],
    #                       "\nCOVID-19 excess mortality: Comparison of MCMC results with squire results")))+
    scale_color_manual(name = NULL, breaks = c("Excess mortality","Model output (Lusaka)", "Modelled true c19 output (UTH)"), values = c("black", "darkblue", "darkred"))


  # PCR_Combined_plots <- cowplot::plot_grid(p_Rt_Reff +ggtitle("Reff and Rt"), p_pcr_perc_age  + guides(col=guide_legend(ncol=2)),
  #                                          ncol = 2, rel_widths = c(1,0.8))


  # Full_Prev_Plot <-  cowplot::plot_grid(Prev_plot,
  #                                       PCR_Combined_plots,
  #                                       nrow = 2, rel_heights = c(1.7,1.1))
# browser()
  rm(list = ls()[!ls() %in% c("lls","Full_Prev_Plot","Poisson_Figure","Poisson_Figure_weeks","Poisson_Figure_age",
                              "Week_prev_plot","Age_prev_plot","PCR_sero_prev_plot","p_excess_comp","p_Rt_Reff_a","pars_obs")])
  gc()
# browser()
  return(list(lls = lls,
              Poisson_Figure = Poisson_Figure,
              Poisson_Figure_weeks = Poisson_Figure_weeks,
              Poisson_Figure_age = Poisson_Figure_age,
              Week_prev_plot = Week_prev_plot,
              Age_prev_plot = Age_prev_plot,
              PCR_sero_prev_plot = PCR_sero_prev_plot,
              p_excess_comp = p_excess_comp,
              p_Rt_Reff_a = p_Rt_Reff_a))
}

# Roll function
roll_func_10 <- function(x, det) {
  ret <- rep(0, length(x))
  for(i in seq_along(ret)) {
    to_sum <- x[seq_len(i)][max(1,i-length(det)+1):i]
    ret[i] <- sum(rev(to_sum)*det[seq_along(to_sum)])
  }
  return(ret)
}


## Prev function
ll_prev_func <- function(df, det, Incidence, out, index, model_params = model_params){

  Sus <- rowSums(out[,index$S])
  Infs <- c(0,as.integer(diff(max(Sus)-Sus)))

  if(Incidence == "Infections"){
    positives <- roll_func_10(Infs, det)
  }
  if(Incidence == "Symptoms"){
    Symps <- rowSums(out[,index$E2]) * model_params$gamma_E
    positives <- roll_func_10(Symps, det)
  }

  percs <- positives/max(Sus)

  test_dates <- seq.Date(from = df$date_start, to = df$date_end, by = 1)
  perc_dates <- percs[as.Date(rownames(out)) %in% test_dates]

  ll <- dbinom(df$pos_tests, df$samples, mean(perc_dates), log = TRUE)

  return(ll)
}
