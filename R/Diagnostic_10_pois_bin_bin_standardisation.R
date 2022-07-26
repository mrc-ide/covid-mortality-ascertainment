# Get_the_replicate_likelihood_values <- function(fit_num, fit_Model_list){
#
#   gc();print(fit_num)
#   fit_Model <- fit_Model_list[[fit_num]]
#
#   ################################################
#   ################################################
#   ### First, load inputs
#   pars_obs <- fit_Model$pmcmc_results$inputs$pars_obs
#   # pcr_det <- pars_obs$pcr_det
#   pcr_det_PM <- pars_obs$pcr_det_PM
#   comb_det <- pars_obs$comb_det
#   index <- squire:::odin_index(fit_Model$model)
#   frac_mort <- 0.8
#   FullData <- pars_obs$combined_data$Comb_data
#   dfj_mcmc_data <- pars_obs$combined_data$dfj_mcmc_data
#
#
#   ################################################
#   ################################################
#   ## Get Susceptibles, Infections, Cumulative infections, pcr_positive and pcr_perc
#   Mod_Age_Deaths_Lus_pcr <- fit_Model$output[,c(index$D,index$S),] %>%
#     melt(varnames = c("date","var","Replicate"), value.name = "value") %>%
#     mutate(Age_gr = as.numeric(gsub(".*?([0-9]+).*", '\\1', var)),
#            var = substr(var, 1, 1),
#            date = as.Date(date)) %>%
#     dcast(... ~ var, value.var="value") %>%
#     dplyr::rename(Mod_cd_Lus = "D", Sus = "S") %>%
#     group_by(Age_gr, Replicate) %>%
#     tidyr::replace_na(list(Mod_cd_Lus = 0)) %>%
#     mutate(Sus = ifelse(is.na(Sus), max(Sus, na.rm=T), Sus)) %>%
#     mutate(cum_infs = as.integer(max(Sus, na.rm = T)-Sus),
#            infs = c(0, as.integer(diff(max(Sus, na.rm = T)-Sus))),
#            pcr_pos = cma:::roll_func_10(infs, pcr_det_PM),
#            pcr_perc = pcr_pos/max(Sus,na.rm = T))
#
#   Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus_pcr %>%
#     filter(date > "2020-06-10") %>%
#     mutate(Week_gr = as.numeric(cut.Date(date, breaks = "1 week", labels = 1:17))) %>%
#     group_by(Age_gr, Week_gr, Replicate) %>%
#     summarise(Mod_cd_Lus = max(Mod_cd_Lus),
#               pcr_perc = pcr_perc[3], # Wednesday - middle of the study week
#               infs = max(infs)) %>%
#     ungroup() %>% group_by(Age_gr, Replicate) %>%
#     mutate(Mod_cd_Lus = c(0,diff(Mod_cd_Lus)),
#            Mod_cd_frac = Mod_cd_Lus*frac_mort) %>%
#     filter(Week_gr != 1) %>%
#     mutate(Week_gr = Week_gr-1) %>%
#     merge(FullData)
#
#   if(obs_params$lld =="remove_weeks_4_and_5"){Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus %>% dplyr::filter(Week_gr != 4,
#                                                                                                  Week_gr != 5)}
#   if(obs_params$lld =="remove_under_5s"){Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus %>% dplyr::filter(Age_gr != 1)}
#   if(obs_params$lld =="remove_weeks_4_and_5_and_age_under_5s"){Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus %>% dplyr::filter(Age_gr != 1,
#                                                                                                                   Week_gr != 4,
#                                                                                                                   Week_gr != 5)}
#
#   ################################################
#   ################################################
#   ### Calculate Likelihoods
#   ## Integrate over
#   LL_Distributions_diag <- lapply(1:length(fit_Model$pmcmc_results$inputs$pars_obs$combined_data$dfj_mcmc_data), function(y){
#     tmp_df <- merge(Mod_Age_Deaths_Lus, fit_Model$pmcmc_results$inputs$pars_obs$combined_data$dfj_mcmc_data[[y]]) %>%
#       arrange(Week_gr, Age_gr)
#     tmp_df$Mod_cd_UTH <- tmp_df$Mod_cd_80*tmp_df$ag1std
#     tmp_df$Mod_tot_ds_Mort <- tmp_df$Mod_cd_UTH + tmp_df$Mort_ncd_mcmc
#     tmp_df$l_pois <- dpois(x = tmp_df$Mort_deaths, lambda = tmp_df$Mod_tot_ds_Mort, log = F)
#
#     tmp_df$Mod_ncd_UTH <- ifelse((tmp_df$Mort_deaths - tmp_df$Mod_cd_UTH)<0,0,(tmp_df$Mort_deaths - tmp_df$Mod_cd_UTH))
#     tmp_df$Mod_pos_ds_UTH <- tmp_df$Mod_cd_UTH+tmp_df$Mod_ncd_UTH*tmp_df$pcr_perc # total positive deaths (covid and coincidental)
#     tmp_df$Mod_tot_ds_UTH_unstd <- tmp_df$Mod_tot_ds_Mort/tmp_df$ag1std
#     tmp_df$Pos_prev <- ifelse(tmp_df$Mod_pos_ds_UTH<=tmp_df$Mort_deaths,tmp_df$Mod_pos_ds_UTH/tmp_df$Mort_deaths, 0.999)
#     tmp_df$l_binom <- dbinom(tmp_df$PosTests, tmp_df$Samples, prob = tmp_df$Pos_prev, log = F)
#     tmp_df$Sample_no <- y
#     tmp_df <- tmp_df[c("Sample_no", "Week_gr", "Age_gr","Replicate", "l_pois", "l_binom", "Mod_cd_Lus","infs","pcr_perc","Mod_cd_80","Mod_cd_UTH","Mod_tot_ds_Mort","Mod_tot_ds_UTH_unstd", "Mod_pos_ds_UTH","Mort_deaths","Pos_prev")]
#   })
#
#   Res_2d <- data.table::rbindlist(LL_Distributions_diag)
#   rm(LL_Distributions_diag)
#   gc()
#
#   # Get overall ll for poisson and bin
#   ll_ov_pois_bin <- Res_2d %>% group_by(Replicate, Sample_no) %>%
#     summarise(ll_pois_prod = log(Brobdingnag::prod(Brobdingnag::as.brob(l_pois))),
#               l_binom_prod = prod(l_binom)) %>%
#     ## Then take the mean of everything?
#     ungroup() %>% group_by(Replicate) %>%
#     summarise(ll_pois_mean_overall = log(Brobdingnag::sum(exp(Brobdingnag::as.brob(ll_pois_prod)))/length(ll_pois_prod)),
#               ll_bin_mean_overall = log(mean(l_binom_prod)))
#
#
#   ################################################
#   ################################################
#   ## pcr/sero/combined Prevalence
#   sero_pcr <- seroprev_df(fit_Model)
#   # sero_pcr <- seroprev_df(fit_Model) %>% filter(replicate ==1)
#
#   sero_pcr_ll <- sero_pcr %>% filter(as.Date(date) %in% seq.Date(from = as.Date("2020-07-04"), to = as.Date("2020-07-27"), by = 1)) %>%
#     group_by(replicate) %>%
#     summarise(pcr_perc = mean(pcr_perc),
#               sero_perc = mean(sero_perc)) %>% #ungroup() %>%
#     mutate(ll_pcr = dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_perc),
#            ll_sero = dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_perc)) %>%
#     mutate(ll_pcr = log(ll_pcr),
#               ll_sero = log(ll_sero))
#
#   comb_ll <- sero_pcr %>% filter(as.Date(date) %in% seq.Date(from = fit_Model$pmcmc_results$inputs$pars_obs$comb_df$date_start,
#                                                              to = fit_Model$pmcmc_results$inputs$pars_obs$comb_df$date_end, by = 1)) %>%
#     select(date, combined_perc, replicate) %>%
#     group_by(replicate) %>%
#     summarise(combined_perc = mean(combined_perc)) %>% #ungroup() %>%
#     mutate(ll_combined = dbinom(x = as.integer(0.091*332), size = as.integer(332), prob = combined_perc, log = F)) %>%
#     mutate(ll_combined = log(ll_combined))
#
#
#   ################################################
#   ################################################
# # browser()
#   lls <- list(
#     ll_bin = ll_ov_pois_bin$ll_bin_mean_overall,
#     ll_pois = ll_ov_pois_bin$ll_pois_mean_overall,
#     ll_pcr = sero_pcr_ll$ll_pcr,
#     ll_sero = sero_pcr_ll$ll_sero,
#     ll_comb = comb_ll$ll_combined,
#     ll_tot = ll_ov_pois_bin$ll_bin_mean_overall + ll_ov_pois_bin$ll_pois_mean_overall + comb_ll$ll_combined)
#
#   rm(list = ls()[!ls() %in% c("lls")])
#   gc()
#
#   return(lls)
# }
#
#
# ################################################
# ################################################
# ################################################
# ################################################
# ################################################
# ################################################
# ################################################
# ################################################
# ################################################
# ################################################
# ################################################
# ################################################
#
# Diagnostic_Plot_10 <- function(fit_num, fit_Model_list, IFRvals, Deaths_by_week = T, Deaths_by_Age = T, Mulenga_Prev=T, Data_only_plots = F){
#   gc();print(fit_num)
#   fit_Model <- fit_Model_list[[fit_num]]
# browser()
#   ################################################
#   ################################################
#   ### First, load inputs
#   pars_obs <- fit_Model$pmcmc_results$inputs$pars_obs
#   # pcr_det <- pars_obs$pcr_det
#   pcr_det_PM <- pars_obs$pcr_det_PM
#   if(is.null(pcr_det_PM)){pcr_det_PM <- pars_obs$pcr_det}
#   # comb_det <- pars_obs$comb_det
#   index <- squire:::odin_index(fit_Model$model)
#   frac_mort <- pars_obs$frac_mort
#   frac_mort <- 0.8
#   FullData <- pars_obs$combined_data$Comb_data
#   dfj_mcmc_data <- pars_obs$combined_data$dfj_mcmc_data
#
#   # fit_Model$pmcmc_results$inputs$pars_obs$
#
#   ################################################
#   ################################################
#   ## Get Susceptibles, Infections, Cumulative infections, pcr_positive and pcr_perc
#   Mod_Age_Deaths_Lus_pcr <- fit_Model$output[,c(index$D,index$S),] %>%
#     melt(varnames = c("date","var","Replicate"), value.name = "value") %>%
#     mutate(Age_gr = as.numeric(gsub(".*?([0-9]+).*", '\\1', var)),
#            var = substr(var, 1, 1),
#            date = as.Date(date)) %>%
#     dcast(... ~ var, value.var="value") %>%
#     dplyr::rename(Mod_cd_Lus = "D", Sus = "S") %>%
#     group_by(Age_gr, Replicate) %>%
#     tidyr::replace_na(list(Mod_cd_Lus = 0)) %>%
#     mutate(Sus = ifelse(is.na(Sus), max(Sus, na.rm=T), Sus)) %>%
#     mutate(cum_infs = as.integer(max(Sus, na.rm = T)-Sus),
#            infs = c(0, as.integer(diff(max(Sus, na.rm = T)-Sus))),
#            pcr_pos = cma:::roll_func_10(infs, pcr_det_PM),
#            pcr_perc = pcr_pos/max(Sus,na.rm = T))
#
#   Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus_pcr %>%
#     filter(date > "2020-06-10") %>%
#     mutate(Week_gr = as.numeric(cut.Date(date, breaks = "1 week", labels = 1:17))) %>%
#     group_by(Age_gr, Week_gr, Replicate) %>%
#     summarise(Mod_cd_Lus = max(Mod_cd_Lus),
#               pcr_perc = pcr_perc[3], # Wednesday - middle of the study week
#               infs = max(infs)) %>%
#     ungroup() %>% group_by(Age_gr, Replicate) %>%
#     mutate(Mod_cd_Lus = c(0,diff(Mod_cd_Lus)),# %>% #,
#            Mod_cd_frac = Mod_cd_Lus*frac_mort) %>%
#     filter(Week_gr != 1) %>%
#     mutate(Week_gr = Week_gr-1) %>%
#     merge(FullData)
#
#   ################################################
#   ################################################
#   ### Calculate Likelihoods
#   # Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus %>% filter(Replicate ==1)
#   ## Integrate over
#   LL_Distributions_diag <- lapply(1:length(fit_Model$pmcmc_results$inputs$pars_obs$combined_data$dfj_mcmc_data), function(y){
#     tmp_df <- merge(Mod_Age_Deaths_Lus, fit_Model$pmcmc_results$inputs$pars_obs$combined_data$dfj_mcmc_data[[y]]) %>%
#       arrange(Week_gr, Age_gr) %>%
#       rename(Mort_deaths = Bur_regs)
#
#     # Calculate the poisson likelihood of burial registrations from Modelled Lusaka Covid deaths (factored by the proportional weekly changes in <5)
#     # Added to the mcmc estimates for Lusaka level baseline deaths (these are already factored by the weekly changes based on <5s)
#     # tmp_df$Mod_tot_ds_Lus <- tmp_df$Mod_cd_Lus + tmp_df$Mort_ncd_mcmc/tmp_df$ag1std
#     # tmp_df$Mod_tot_ds_bur_regs <- tmp_df$Mod_cd_Lus*tmp_df$ag1std + tmp_df$Mort_ncd_mcmc
#     # tmp_df$l_pois <- dpois(x = tmp_df$Bur_regs, lambda = tmp_df$Mod_tot_ds_bur_regs, log = F)
#     tmp_df$Mod_cd_UTH <- tmp_df$Mod_cd_frac*tmp_df$ag1std
#     tmp_df$Mod_tot_ds_Mort <- tmp_df$Mod_cd_UTH + tmp_df$Mort_ncd_mcmc
#     tmp_df$l_pois <- dpois(x = tmp_df$Mort_deaths, lambda = tmp_df$Mod_tot_ds_Mort, log = F)
#
#     # tmp_df$Est_Lus_ds <- tmp_df$Bur_regs/tmp_df$ag1std
#
#     # tmp_df$Mod_ncd_Lus <- tmp_df$Est_Lus_ds - tmp_df$Mod_cd_Lus
#     # tmp_df$Mod_ncd_Lus <- ifelse(tmp_df$Mod_ncd_Lus<0,0,tmp_df$Mod_ncd_Lus)
#     # tmp_df$Mod_pos_ds_Lus <- tmp_df$Mod_cd_Lus+tmp_df$Mod_ncd_Lus*tmp_df$pcr_perc # total positive deaths (covid and coincidental)
#     # tmp_df$Pos_prev <- ifelse(tmp_df$Mod_pos_ds_Lus<=tmp_df$Est_Lus_ds,tmp_df$Mod_pos_ds_Lus/tmp_df$Est_Lus_ds, 0.999)
#     # tmp_df$l_binom <- dbinom(tmp_df$PosTests, tmp_df$Samples, prob = tmp_df$Pos_prev, log = F)
#
#
#     # tmp_df$Mod_cd_UTH <- tmp_df$Mod_cd_frac*tmp_df$ag1std
#
#     # tmp_df$Mod_ncd_UTH <- ifelse((tmp_df$Bur_regs*frac_mort - tmp_df$Mod_cd_UTH)<0,0,(tmp_df$Bur_regs*frac_mort - tmp_df$Mod_cd_UTH))
#     # tmp_df$Mod_pos_ds_UTH <- tmp_df$Mod_cd_UTH+tmp_df$Mod_ncd_UTH*tmp_df$pcr_perc # total positive deaths (covid and coincidental)
#     # tmp_df$Mod_pos_ds_frac <- tmp_df$Mod_pos_ds_UTH/tmp_df$ag1std
#     # tmp_df$Pos_prev <- ifelse(tmp_df$Mod_pos_ds_UTH<=tmp_df$Burial_Regs*frac_mort,tmp_df$Mod_pos_ds_UTH/tmp_df$Burial_Regs, 0.999)
#     # tmp_df$l_binom <- dbinom(tmp_df$PosTests, tmp_df$Samples, prob = tmp_df$Pos_prev, log = F)
#
#     tmp_df$Mod_ncd_UTH <- ifelse((tmp_df$Mort_deaths - tmp_df$Mod_cd_UTH)<0,0,(tmp_df$Mort_deaths - tmp_df$Mod_cd_UTH))
#     tmp_df$Mod_pos_ds_UTH <- tmp_df$Mod_cd_UTH+tmp_df$Mod_ncd_UTH*tmp_df$pcr_perc # total positive deaths (covid and coincidental)
#     tmp_df$Mod_tot_ds_UTH_unstd <- tmp_df$Mod_tot_ds_Mort/tmp_df$ag1std
#     tmp_df$Pos_prev <- ifelse(tmp_df$Mod_pos_ds_UTH<=tmp_df$Mort_deaths,tmp_df$Mod_pos_ds_UTH/tmp_df$Mort_deaths, 0.999)
#     tmp_df$l_binom <- dbinom(tmp_df$PosTests, tmp_df$Samples, prob = tmp_df$Pos_prev, log = F)
#     tmp_df$Sample_no <- y
#     tmp_df <- tmp_df[c("Sample_no", "Week_gr", "Age_gr","Replicate", "l_pois", "l_binom",
#                        "Mod_cd_Lus","Mod_cd_frac","Mod_cd_UTH",
#                        "infs","pcr_perc",
#                        "Mod_tot_ds_Mort","Mod_tot_ds_UTH_unstd","Mort_deaths",
#                        "Mod_pos_ds_UTH","Pos_prev")]
#     # tmp_df <- tmp_df[c("Sample_no", "Week_gr", "Age_gr","Replicate", "l_pois", "l_binom",
#                        # "Mod_cd_Lus","infs","pcr_perc",
#                        # "Mod_tot_ds_bur_regs","Mod_tot_ds_Lus","Mod_pos_ds_Lus",
#                        # "Bur_regs","Est_Lus_ds","Pos_prev")]
#   })
# browser()
#   Res_2d <- data.table::rbindlist(LL_Distributions_diag)
#   rm(LL_Distributions_diag)
#   gc()
#
#   if(pars_obs$lld ==""){Res_2d_fil <- Res_2d}
#   if(pars_obs$lld =="full_data"){Res_2d_fil <- Res_2d}
#   if(pars_obs$lld =="remove_weeks_4_and_5"){Res_2d_fil <- Res_2d %>% dplyr::filter(Week_gr != 4,
#                                                                                Week_gr != 5)}
#   if(pars_obs$lld =="remove_under_5s"){Res_2d_fil <- Res_2d %>% dplyr::filter(Age_gr != 1)}
#   if(pars_obs$lld =="remove_weeks_4_and_5_and_age_under_5s"){Res_2d_fil <- Res_2d %>% dplyr::filter(Age_gr != 1,
#                                                                                                 Week_gr != 4,
#                                                                                                 Week_gr != 5)}
#
#
#   # Get overall ll for poisson and bin
#   ll_ov_pois_bin <- Res_2d_fil %>% group_by(Replicate, Sample_no) %>%
#     summarise(ll_pois_prod = log(Brobdingnag::prod(Brobdingnag::as.brob(l_pois))),
#               l_binom_prod = prod(l_binom)) %>%
#     ## Then take the mean of everything?
#     ungroup() %>%
#     summarise(ll_pois_mean_overall = log(Brobdingnag::sum(exp(Brobdingnag::as.brob(ll_pois_prod)))/length(ll_pois_prod)),
#               ll_bin_mean_overall = log(mean(l_binom_prod)))
#
#
#   ################################################
#   ################################################
#   ## pcr/sero/combined Prevalence
#   sero_pcr <- seroprev_df(fit_Model)
#
#
#   # pars_obs$sero_df$samples
#
#   sero_pcr_ll <- sero_pcr %>% filter(as.Date(date) %in% seq.Date(from = pars_obs$sero_df$date_start, to = pars_obs$sero_df$date_end, by = 1)) %>%
#     group_by(replicate) %>%
#     summarise(pcr_perc = mean(pcr_perc),
#               sero_perc = mean(sero_perc)) %>% ungroup() %>%
#     mutate(ll_pcr = dbinom(x = pars_obs$pcr_df$pos_tests, size = pars_obs$pcr_df$samples, prob = pcr_perc),
#            ll_sero = dbinom(x = pars_obs$sero_df$pos_tests, size = pars_obs$sero_df$samples, prob = sero_perc)) %>%
#     summarise(ll_pcr = log(mean(ll_pcr)),
#               ll_sero = log(mean(ll_sero)))
#
#   # comb_ll <- sero_pcr %>% filter(as.Date(date) %in% seq.Date(from = fit_Model$pmcmc_results$inputs$pars_obs$comb_df$date_start,
#                                                              # to = fit_Model$pmcmc_results$inputs$pars_obs$comb_df$date_end, by = 1)) %>%
#     # select(date, combined_perc, replicate) %>%
#     # group_by(replicate) %>%
#     # summarise(combined_perc = mean(combined_perc)) %>% ungroup() %>%
#     # mutate(ll_combined = dbinom(x = as.integer(0.091*332), size = as.integer(332), prob = combined_perc, log = F)) %>%
#     # summarise(ll_combined = log(mean(ll_combined)))
#
#
#   ################################################
#   ################################################
#
#   lls <- list(
#     ll_bin = ll_ov_pois_bin$ll_bin_mean_overall,
#     ll_pois = ll_ov_pois_bin$ll_pois_mean_overall,
#     ll_pcr = sero_pcr_ll$ll_pcr,
#     ll_sero = sero_pcr_ll$ll_sero,
#     # ll_comb = comb_ll$ll_combined,
#     ll_tot = sum(ll_ov_pois_bin$ll_bin_mean_overall, ll_ov_pois_bin$ll_pois_mean_overall, sero_pcr_ll$ll_pcr, sero_pcr_ll$ll_sero))
#
#   ##############################################################################
#   ##############################################################################
#
#   ################################################
#   ################################################
#
#   ################################################
#   ################################################
#   ### Plots:
# # browser()
#   ################################################
#   ## Plot 1: PCR perc by age
#   PCR_perc <- Mod_Age_Deaths_Lus_pcr %>% group_by(Age_gr, date) %>%
#     summarise(pcr_perc = median(pcr_perc))
#
#   # Facet labels: Simple
#   Age_groups.labs_Simple <-  Mod_Age_Deaths_Lus_pcr %>% group_by(Age_gr) %>% select(Age_gr) %>% unique() %>%
#     mutate(Age_gr_lab = ifelse(Age_gr %in% 1:16, paste0("Age: ",Age_gr*5-5,"-",Age_gr*5-1),
#                                "Age: 80+")) %>%
#     ungroup() %>% arrange(Age_gr) %>% select(Age_gr_lab) %>%
#     unlist()
#   names(Age_groups.labs_Simple) <- 1:17
#
#   # Plot
#   p_pcr_perc_age <- ggplot(PCR_perc, aes(x = date, y = pcr_perc, col = as.factor(Age_gr))) + geom_line() +
#     # facet_wrap(~Age_gr, labeller = labeller(Age_gr = Age_groups.labs_Simple)) +
#     theme_minimal() +
#     xlab("Date") + ylab("+ve %") +
#     # ggtitle(paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),
#     #                ", Slope x ", round(IFRvals$Slope_x[fit_num],2),
#     #                "\nModelled PCR % by age group (mid-week)")) +
#     ggtitle(paste0("Modelled PCR %")) +
#     viridis::scale_color_viridis(discrete = T, labels = c(Age_groups.labs_Simple)) +
#     theme(legend.title = element_blank())
#     # scale_color_manual()
#
#
#   ################################################
#   ## Plot 2: Rt and Reff
#   p_Rt_Reff_a <- rt_plot_immunity(fit_Model)$plot +
#     scale_fill_manual("",values = c("Reff" = "#48996b","Rt" ="#3f8da7"))
#
#   p_Rt_Reff_b <- fit_Model$replicate_parameters[,paste0("Rt_rw_",1:10)] %>%
#     melt(id.vars = NULL) %>%
#     ggplot() +
#     facet_wrap(~variable, ncol = 2) +
#     geom_histogram(aes(x = value), bins = 30) +
#     theme_minimal() +
#     geom_vline(xintercept = 0, linetype = "dashed") +
#     theme(strip.text.x = element_text(size = 6),
#           axis.text=element_text(size=6))
#
#   p_Rt_Reff <- cowplot::plot_grid(p_Rt_Reff_a, p_Rt_Reff_b, rel_widths = c(1, 0.6))
#
#
#   ################################################
#   ## Plot 3: combined prevalence by wider age groups to compare with Mulenga data
#   # Mod_Age_Deaths_Lus_Combined_Prev <- fit_Model$output[,c(index$S),] %>%
#   #   melt(varnames = c("date","var","Replicate"), value.name = "Sus") %>%
#   #   mutate(Age_gr = as.numeric(gsub(".*?([0-9]+).*", '\\1', var)),
#   #          var = substr(var, 1, 1),
#   #          date = as.Date(date)) %>%
#   #   mutate(Age_gr_10 = ifelse(Age_gr>=15, 8, ceiling(Age_gr/2))) %>%
#   #   group_by(Age_gr, Replicate) %>%
#   #   mutate(Sus = ifelse(is.na(Sus), max(Sus, na.rm=T), Sus)) %>%
#   #   mutate(infs = c(0, as.integer(diff(max(Sus, na.rm = T)-Sus))),
#   #          comb_pos = roll_func(infs, comb_det),
#   #          comb_perc = comb_pos/max(Sus,na.rm = T))
#
#   # Comb_prev_df <- Mod_Age_Deaths_Lus_Combined_Prev %>% group_by(date, Age_gr_10) %>%
#   #   summarise(comb_perc_mean = mean(comb_perc),
#   #             comb_perc_max = max(comb_perc),
#   #             comb_perc_min = min(comb_perc))
#
#   # Mulenga_Combined_Prevalence_Age <- readRDS("analysis/data/Code-generated-data/14_Mulenga_Combined_Prevalence_by_Age.rds") %>%
#   #   mutate(Age_gr_10 = 1:8)
#
#   # Facet labels: Mulenga groups of 10 years
#   # Age_groups.labs_comb_prev <- Comb_prev_df %>% ungroup() %>% select(Age_gr_10) %>% unique() %>%
#   #   mutate(Age_gr_10_lab = ifelse(Age_gr_10 %in% 1:7, paste0("Age: ", Age_gr_10*10-10,"-",Age_gr_10*10-1),
#   #                                 "Age: 70+")) %>%
#   #   ungroup() %>% arrange(Age_gr_10) %>% select(Age_gr_10_lab) %>%
#   #   unlist()
#   # names(Age_groups.labs_comb_prev) <- 1:8
#
#   # Plot
#   # p_combined_prevalence_comp <- ggplot(merge(Comb_prev_df,Mulenga_Combined_Prevalence_Age), aes(x = date)) +
#   #   geom_line(aes(y=comb_perc_mean*100)) +
#   #   geom_ribbon(aes(ymin=comb_perc_min*100, ymax=comb_perc_max*100), alpha=0.3) +
#   #   facet_wrap(vars(Age_gr_10), labeller = labeller(Age_gr_10 = Age_groups.labs_comb_prev)) +
#   #   xlab("Date") + ylab("Combined prevalence %") +
#   #   ggtitle(paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),", Slope x ", round(IFRvals$Slope_x[fit_num],2),"\nModelled COVID-19 Combined prevalence Lusaka")) +
#   #   theme(plot.title = element_text(size = 10)) +
#   #   geom_point(data = Mulenga_Combined_Prevalence_Age, aes(x= as.Date("2020-07-15"), y = Combined_Prev)) +
#   #   geom_errorbar(data = Mulenga_Combined_Prevalence_Age, aes(ymin=LowerInt,ymax=HigherInt,x=as.Date("2020-07-15"), width=10)) +
#   #   geom_errorbarh(mapping = aes(y = Combined_Prev, xmin=as.Date("2020-07-04"), xmax=as.Date("2020-07-27"),height = 0)) +
#   #   theme_minimal()
#
#   # p_combined_prevalence_comp_full <- ggplot(merge(Comb_prev_df,Mulenga_Combined_Prevalence_Age %>%tibble::rownames_to_column(var = "Age_group_lab")),
#   #                                           aes(x = date, col = Age_group_lab, fill = Age_group_lab)) +
#   #   annotate("rect", xmin = as.Date("2020-07-04"), xmax = as.Date("2020-07-27"), ymin = -Inf, ymax = Inf, alpha = 0.3, fill= "lightblue") +
#   #   geom_line(aes(y=comb_perc_mean*100)) +
#   #   xlab("Date") + ylab("+ve %") +
#   #   # ggtitle(paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),", Slope x ", round(IFRvals$Slope_x[fit_num],2),"\nModelled COVID-19 Combined prevalence Lusaka")) +
#   #   ggtitle(paste0("Modelled COVID-19 combined prevalence")) +
#   #   theme_minimal() +
#   #   viridis::scale_fill_viridis(discrete = T, name = "Age") +
#   #   viridis::scale_colour_viridis(discrete = T, name = "Age")
#   # theme(#plot.title = element_text(size = 10),
#   # legend.position = "none")
#
#
#   # p_combined_prevalence_comp_by_age <- ggplot(merge(Comb_prev_df,Mulenga_Combined_Prevalence_Age %>%tibble::rownames_to_column(var = "Age_group_lab")),
#   #                                             aes(x = date, col = Age_group_lab, fill = Age_group_lab)) +
#   #   geom_line(aes(y=comb_perc_mean*100)) +
#   #   geom_ribbon(aes(ymin=comb_perc_min*100, ymax=comb_perc_max*100), alpha=0.2) +
#   #   facet_wrap(vars(Age_group_lab), labeller = labeller(Age_gr_10 = Age_groups.labs_comb_prev), nrow = 1) +
#   #   xlab("Date") + ylab("+ve %") +
#   #   # ggtitle(paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),", Slope x ", round(IFRvals$Slope_x[fit_num],2),"\nModelled COVID-19 Combined prevalence Lusaka")) +
#   #   ggtitle(paste0("Modelled combined prevalence, Lusaka/Zambia")) +
#   #   theme(plot.title = element_text(size = 10)) +
#   #   geom_point(data = Mulenga_Combined_Prevalence_Age %>% tibble::rownames_to_column(var = "Age_group_lab"), aes(x= as.Date("2020-07-15"), y = Combined_Prev)) +
#   #   geom_errorbar(data = Mulenga_Combined_Prevalence_Age %>% tibble::rownames_to_column(var = "Age_group_lab"), aes(ymin=LowerInt,ymax=HigherInt,x=as.Date("2020-07-15"), width=10)) +
#   #   geom_errorbarh(mapping = aes(y = Combined_Prev, xmin=as.Date("2020-07-04"), xmax=as.Date("2020-07-27"),height = 0)) +
#   #   theme_minimal() +
#   #   # coord_cartesian(xlim=c(as.Date("2020-07-04"),as.Date("2020-07-27"))) +
#   #   viridis::scale_fill_viridis(discrete = T, name = "Age") +
#   #   viridis::scale_colour_viridis(discrete = T, name = "Age") +
#   #   # scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks") +
#   #   theme(#plot.title = element_text(size = 10),
#   #     legend.position = "none")
#
#
#
#
#
#   ################################################
#   ################################################
#   # Calculate the mean pois for each age and week
#   ll_aw_pois <- Res_2d %>% group_by(Age_gr, Week_gr) %>%
#     summarise(ll_pois_mean_age_week = log(Brobdingnag::sum(Brobdingnag::as.brob(l_pois))/length(l_pois)),
#               ll_pois_min_age_week = log(min(l_pois)),
#               ll_pois_max_age_week = log(max(l_pois))
#     )
#
#   # Calculate the mean for all other variables
#   ll_aw <- Res_2d %>% mutate(Mod_coin_ds_Lus = Mod_pos_ds_UTH - Mod_cd_UTH) %>%
#     group_by(Age_gr, Week_gr) %>%
#     # summarise_at(c("l_binom", "Mod_cd_Lus", "Mod_pos_ds_Lus","Bur_regs","Est_Lus_ds","pcr_perc","infs",
#     #                "Pos_prev","Mod_coin_ds_Lus","Mod_tot_ds_bur_regs","Mod_tot_ds_Lus"), list(mean = mean, min = min, max = max), na.rm = TRUE) %>%
#     summarise_at(c("l_binom",
#                    "Mod_cd_Lus","Mod_cd_frac","Mod_cd_UTH",
#                    "Mod_pos_ds_UTH","Mort_deaths",
#                    "pcr_perc","infs","Pos_prev","Mod_tot_ds_Mort","Mod_tot_ds_UTH_unstd"), list(mean = mean, min = min, max = max), na.rm = TRUE) %>%
#     merge(unique(Mod_Age_Deaths_Lus %>% select(Week_gr, date)))
#
#   ################################################
#   ################################################
#
#   ################################################
#   ## Plot 4: Poisson fits
#
#   # Facet labels: Including Pois ll
#   Age_groups.labs_pois <- ll_aw_pois %>% group_by(Age_gr) %>%
#     summarise(ll_pois_mean = sum(ll_pois_mean_age_week)) %>%
#     mutate(Age_gr_lab = paste0(Age_groups.labs_Simple[Age_gr],"; Pois ll = ",round(ll_pois_mean[Age_gr],1))) %>%
#     select(Age_gr_lab) %>%
#     unlist()
#   names(Age_groups.labs_pois) <- 1:17
#
#   # Plot
#   Poisson_Figure <- ggplot(ll_aw, aes(x = date)) +
#     # Modelled Positive deaths
#     geom_line(aes(y=Mod_tot_ds_bur_regs_mean, color = "Model fit")) +
#     geom_ribbon(aes(ymin=Mod_tot_ds_bur_regs_min, ymax=Mod_tot_ds_bur_regs_max), alpha=0.3) +
#     facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs_pois)) +
#     # Modelled True deaths
#     geom_line(aes(y=Mod_tot_ds_Lus_mean, color = "Model fit (Lusaka)"),linetype="dashed", alpha = 0.5) +
#     geom_ribbon(aes(ymin=Mod_tot_ds_Lus_min, ymax=Mod_tot_ds_Lus_max), alpha=0.1) +
#     # Scaled positive deaths
#     geom_point(aes(y = Bur_regs_mean, color = "Burial registrations")) +
#     # Scaled True deaths
#     xlab("Date") + ylab("Deaths") +
#     ggtitle(#ifelse(is.numeric(IFRvals$IFR_x),
#                    paste0("IFR x", round(IFRvals$IFR_x[fit_num],2),
#                                                     ", Slope x", round(IFRvals$Slope_x[fit_num],2),
#                                                     "\nLusaka weekly burial registrations")) +
#                    # paste0(IFRvals$IFR_x[fit_num],"; ",IFRvals$Slope_x[fit_num],
#                           # "\nCOVID-19 weekly deaths (Mortuary)"))) +
#     # geom_point(data = ll_aw %>% filter(Week_gr %in% c(4,5)), aes(x = date, y = Bur_regs_mean, color = "Mortuary surveillance disrupted?")) +
#     theme_minimal() +
#     theme(plot.title = element_text(size = 10),
#           legend.position = c(1,0), legend.justification = c(1,0),
#           legend.key = element_rect(fill = "white", linetype = 0)) +
#     scale_color_manual(name=NULL,
#                        breaks=c("Burial registrations", "Model fit", "Model fit (Lusaka)"),
#                        values = c("black","black","black"),
#                        # values=c('black', 'red'),
#                        guide = guide_legend(override.aes = list(linetype = c(0,1,2),
#                                                                 shape = c(16,NA,NA),
#                                                                 alpha = c(1,1,0.5))))
#
#
#   ################################################
#   ## Plot 6: Weekly prevalence fit
#
#   # Group by week
#   Mod_Age_Deaths_Lus_Av_Week <- ll_aw %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
#     merge(FullData) %>%
#     group_by(Week_gr) %>%
#     select(-date, -Age_gr) %>%
#     summarise_all(sum, na.rm=T) %>%
#     merge(unique(ll_aw[c("Week_gr","date")])) %>%
#     rename(Mort_deaths = Bur_regs)
#
#   # Note questionable data points
#   # Mod_Age_Deaths_Lus_Av_Week <- Mod_Age_Deaths_Lus_Av_Week %>% mutate(Unreliable_points_key = ifelse(Week_gr %in% 4:5, "Questionable data points", "Prevalence"))
#
#   # plot week plot
#   Week_prev_plot <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
#     # Prevalence on positive in sample
#     geom_point(aes(y = 100*PosTests/Samples)) +#, col = as.factor(Unreliable_points_key))) +
#     geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
#                       ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
#     # geom_line(aes(y = 100*Pos_prev_coin_mean, col = "Coincidental deaths"),linetype="dashed") +
#     # geom_ribbon(aes(ymin=100*Pos_prev_coin_min, ymax=100*Pos_prev_coin_max), alpha=0.2, fill = "darkblue") +
#     # geom_line(aes(y = 100*Pos_prev_causal_mean,col = "Causal deaths"),linetype="dashed") +
#     # geom_ribbon(aes(ymin=100*Pos_prev_causal_min, ymax=100*Pos_prev_causal_max), alpha=0.3, fill = "darkred") +
#     geom_line(aes(y = 100*Mod_pos_ds_UTH_mean/Mort_deaths, col = "Total +ve deaths"),linetype="dashed") +
#     geom_ribbon(aes(ymin=100*Mod_pos_ds_UTH_min/Mort_deaths, ymax=100*Mod_pos_ds_UTH_max/Mort_deaths), alpha=0.3) +
#     # xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
#     coord_cartesian(ylim = c(0,100)) +
#     xlab("Date") + ylab("+ve %")  +
#     ggtitle(ifelse(is.numeric(IFRvals$IFR_x),
#                    paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),
#                           ", Slope x ", round(IFRvals$Slope_x[fit_num],2),
#                           "\nCOVID-19 weekly prevalence\nUTH mortuary"),
#               paste0(IFRvals$IFR_x[fit_num],"; ",IFRvals$Slope_x[fit_num],
#                           "\nCOVID-19 weekly prevalence\nUTH mortuary"))) +
#     theme(plot.title = element_text(size = 10)) +
#     # geom_point(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)), aes(x = date, y = PosTests/Samples), col = "red") +
#     # geom_errorbar(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)),
#     # aes(ymin = Hmisc::binconf(PosTests,Samples)[,"Lower"],
#     # ymax = Hmisc::binconf(PosTests,Samples)[,"Upper"]), col = "red") +
#     theme_minimal() +
#     scale_color_manual(name = NULL, breaks = c("Total +ve deaths","Causal deaths","Coincidental deaths"), values = c("black","darkred","darkblue"))
#   # ggpubr::theme_pubr(legend = "bottom")
#
#
#   ################################################
#   ## Plot 7: Age group prevalence fits
#
#   # Group by Age
#   # Mod_Age_Deaths_Lus_Av_Week <- ll_aw %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
#   #   merge(FullData) %>%
#   #   group_by(Week_gr) %>%
#   #   select(-date, -Age_gr) %>%
#   #   summarise_all(sum, na.rm=T) %>%
#   #   merge(unique(ll_aw[c("Week_gr","date")]))# %>%
#
#   Mod_Age_Deaths_Lus_Av_Age <- ll_aw %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
#     merge(FullData) %>%
#     group_by(Age_gr) %>%
#     select(-date, -Week_gr) %>%
#     summarise_all(sum, na.rm=T) %>%
#     rename(Mort_deaths = Bur_regs)
#
#   Mod_Age_Deaths_Lus_Av_Age <- Mod_Age_Deaths_Lus_Av_Age %>% mutate(Age_gr_label = ifelse(Age_gr %in% 1:16,
#                                                                                           paste0(Age_gr*5-5, "-",Age_gr*5-1),
#                                                                                           "80+"))
#
#   # plot age plot
#   Age_prev_plot <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr_label)) +
#     # Prevalence on positive in sample
#     geom_point(aes(y = 100*PosTests/Samples)) +
#     geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
#                       ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
#     # geom_line(aes(y = 100*(Mod_coin_ds_Lus_mean)/Est_Lus_ds_mean, col = "Coincidental deaths", group = 1),linetype="dashed") +
#     # geom_ribbon(aes(ymin=100*(Mod_coin_ds_Lus_min)/Est_Lus_ds_mean, ymax=100*(Mod_coin_ds_Lus_max)/Est_Lus_ds_mean, group = 1), alpha=0.2, fill = "darkblue") +
#     # geom_line(aes(y = 100*Mod_cd_Lus_mean/Est_Lus_ds_mean,col = "Causal deaths", group = 1),linetype="dashed") +
#     # geom_ribbon(aes(ymin=100*Mod_cd_Lus_min/Est_Lus_ds_mean, ymax=100*Mod_cd_Lus_max/Est_Lus_ds_mean, group = 1), alpha=0.3, fill = "darkred") +
#     geom_line(aes(y = 100*Mod_pos_ds_UTH_mean/Mort_deaths, group = 1, col = "Total +ve deaths"),linetype="dashed") +
#     geom_ribbon(aes(ymin=100*Mod_pos_ds_UTH_min/Mort_deaths, ymax=100*Mod_pos_ds_UTH_max/Mort_deaths), alpha=0.3) +
#     xlab("Age") + ylab("+ve %")  +
#     ggtitle("\nCOVID-19 prevalence by age\nLus mortuary") +
#     theme(plot.title = element_text(size = 10)) +
#     theme_minimal() +
#     scale_color_manual(name = NULL, breaks = c("Total +ve deaths","Causal deaths","Coincidental deaths"), values = c("black","darkred","darkblue")) +
#     scale_x_discrete(limits = c(paste0(1:16*5-5, "-",1:16*5-1),"80+")) +
#     theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
#
# # browser()
#   ################################################
#   ## Plot 8: PCR/Sero/Combined prevalence fits
#   pcr_sero_data <- Summ_sero_pcr_data(sero_pcr)
#   PCR_sero_comb_prev_plot <- ggplot(pcr_sero_data, aes(x = date, y = mean_pcr)) +
#     geom_line(aes(x=date, y=mean_pcr, color = "PCR % (Zambia)"),linetype="dashed") +
#     geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3, fill = "darkgoldenrod1")+
#     geom_point(aes(x= as.Date("2020-07-15"),y=7.6, color = "PCR % (Zambia)"), size = 2) +
#     geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10,color = "PCR % (Zambia)")) +
#     geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0,color = "PCR % (Zambia)")) +
#
#     geom_line(aes(x=date, y=mean_sero, color = "Sero % (Zambia)"),linetype="dashed") +
#     geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3, fill = "chartreuse4")+
#     geom_point(aes(x= as.Date("2020-07-15"),y=2.1, color = "Sero % (Zambia)"), size = 2) +
#     geom_errorbar(aes(ymin=1.1,ymax=3.1,x=as.Date("2020-07-15"), width=10, color = "Sero % (Zambia)")) +
#     geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0, color = "Sero % (Zambia)")) +
#
#     geom_line(aes(x=date, y=mean_combined, color = "Combined % (Lusaka)"),linetype="dashed") +
#     geom_ribbon(aes(x=date,ymin=min_combined, ymax=max_combined), alpha=0.3, fill = "black")+
#     geom_point(aes(x= as.Date("2020-07-11"),y=9.1, color = "Combined % (Lusaka)"), size = 2) +
#     geom_errorbar(aes(ymin=2.6,ymax=15.7,x=as.Date("2020-07-11"), width=10, color = "Combined % (Lusaka)")) +
#     geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-19"),y=9.1, height=0, color = "Combined % (Lusaka)")) +
#
#     ylab(paste0("+ve %")) +
#     coord_cartesian(xlim = c(as.Date("2020-04-15"), as.Date("2020-10-01")),
#                     ylim = c(0, 28)) +
#     # xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) +# coord_cartesian(ylim=c(0, 15)) +
#     # ylim(0, 28) +
#     xlab("Date")+
#     theme_minimal() +
#     # ggpubr::theme_pubr(legend = "bottom") +
#     ggtitle(paste("\nCOVID-19 prevalence\nLusaka/Zambia")) +#\nPCR binom ll =", round(sero_pcr_ll$ll_pcr,1),
#     # "\nSero binom ll =", round(sero_pcr_ll$ll_sero,1),
#     # "\nCombined binom ll =", round(comb_ll$ll_combined,1)))
#     # theme(plot.title = element_text(size = 10)) +
#     scale_color_manual(name=NULL,
#                        breaks=c("Combined % (Lusaka)","PCR % (Zambia)", "Sero % (Zambia)"),
#                        values = c("black","darkgoldenrod1","chartreuse4"))
#   #
#   #
#   # Prev_plot <- cowplot::plot_grid(
#     # cowplot::plot_grid(Week_prev_plot + theme(legend.position="none"), Age_prev_plot, PCR_sero_comb_prev_plot + theme(legend.position="none"), nrow = 1),
#     # cowplot::plot_grid(get_legend(Week_prev_plot + theme(legend.position = "bottom")), get_legend(PCR_sero_comb_prev_plot_uf), nrow = 1,
#                        # rel_widths = c(2,1)),ncol = 1, rel_heights = c(1,0.2))
#   Prev_plot <- cowplot::plot_grid(
#     cowplot::plot_grid(Week_prev_plot + theme(legend.position = "none"), Age_prev_plot  + theme(legend.position = "none"), PCR_sero_comb_prev_plot + theme(legend.position="none"), nrow = 1),
#     cowplot::plot_grid(ggpubr::get_legend(Week_prev_plot),ggpubr::get_legend(PCR_sero_comb_prev_plot), rel_widths = c(2,1), nrow = 1),
#     nrow = 2, rel_heights = c(1,0.2))
#
#
#   ################################################
#   ## Plot 9: Excess Mortality Plot against squire predictions
#   Pop_str <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
#   Pop_str_df <- data.frame(Age_gr = 1:17, Pop_str = Pop_str)
#   # ExcessMort_mcmc <- readRDS("analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week.rds")
#   ExcessMort_mcmc <- readRDS("analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week_Checked.rds")
#
#   ## Add this to true covid deaths
#   Plot_data_Excess_Pred <- ExcessMort_mcmc %>% merge(Pop_str_df) %>% merge(ll_aw) %>% filter(Age_gr!=1) %>%
#     mutate(date = lubridate::floor_date(date, "week", 1))
#   p_excess_comp <- ggplot(Plot_data_Excess_Pred, aes(x = date, y = excess_Median/Pop_str*1000)) + geom_line(aes(col = "Excess mortality")) +
#     geom_ribbon(aes(ymin = excess_CI_low/Pop_str*1000, ymax = excess_CI_high/Pop_str*1000), alpha = 0.4) +
#     facet_wrap(~Age_gr, labeller = labeller(Age_gr = Age_groups.labs_Simple[-1])) +
#     # geom_line(aes(y = Mod_cd_Lus_mean/Pop_str*1000, col = "Model output (Lusaka)"))  +
#     # geom_ribbon(aes(ymin = Mod_cd_Lus_min/Pop_str*1000, ymax = Mod_cd_Lus_max/Pop_str*1000), alpha = 0.4, fill = "darkblue") +
#     # geom_line(aes(y = Mod_cd_80_mean/Pop_str*1000), col = "darkred") +
#     geom_line(aes(y = Mod_cd_UTH_mean/Pop_str*1000, col = "Modelled true c19 output (UTH)")) +
#     geom_ribbon(aes(ymin = Mod_cd_UTH_min/Pop_str*1000, ymax = Mod_cd_UTH_max/Pop_str*1000), alpha = 0.4, fill = "darkred") +
#     theme_minimal() +
#     theme(legend.position="bottom") +
#     ylab("Per capita deaths (/1000)") +
#     xlab("Date") +
#     ggtitle(ifelse(is.numeric(IFRvals$IFR_x),
#                    paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),
#                           ", Slope x ", round(IFRvals$Slope_x[fit_num],2),
#                           "\nCOVID-19 excess mortality: Comparison of MCMC results with squire results"),
#                    paste0(IFRvals$IFR_x[fit_num],"; ",IFRvals$Slope_x[fit_num],
#                           "\nCOVID-19 excess mortality: Comparison of MCMC results with squire results")))+
#
#
#
#     scale_color_manual(name = NULL, breaks = c("Excess mortality","Model output (Lusaka)", "Modelled true c19 output (UTH)"), values = c("black", "darkblue", "darkred"))
#   # dev.off()
#
#   PCR_Combined_plots <- cowplot::plot_grid(p_Rt_Reff +ggtitle("Reff and Rt"), p_pcr_perc_age  + guides(col=guide_legend(ncol=2)),
#                                            #p_combined_prevalence_comp_full, #+ guides(fill=guide_legend(ncol=2)),
#                                            # p_combined_prevalence_comp_by_age,
#                                            ncol = 2, rel_widths = c(1,1))
#
#
#   Full_Prev_Plot <-  cowplot::plot_grid(Prev_plot,
#                                         PCR_Combined_plots,
#                                         p_combined_prevalence_comp_by_age,
#                                         nrow = 3, rel_heights = c(1.7,1.1,1))
#
#   # pdf("analysis/figures/Test_plot.pdf", height = 11, width = 12)
#   # Full_Prev_Plot
#   # dev.off()
#   #
#   # browser()
#
#   #########################
#   #### Data only plots: ###
#   #########################
#
#   if(Data_only_plots ==T){
#
#   Week_prev_plot_data <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
#     # Prevalence on positive in sample
#     geom_point(aes(y = 100*PosTests/Samples)) +#, col = as.factor(Unreliable_points_key))) +
#     geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
#                       ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
#     xlab("Date") + ylab("+ve %")  +
#     ggtitle(ifelse(is.numeric(IFRvals$IFR_x),
#                    paste0(#"IFR x ", round(IFRvals$IFR_x[fit_num],2),
#                           #", Slope x ", round(IFRvals$Slope_x[fit_num],2),
#                           "\nCOVID-19 weekly prevalence\nUTH mortuary"),
#                    paste0(IFRvals$IFR_x[fit_num],"; ",IFRvals$Slope_x[fit_num],
#                           "\nCOVID-19 weekly prevalence\nUTH mortuary"))) +
#     theme(plot.title = element_text(size = 10)) +
#     theme_minimal() +
#     scale_color_manual(name = NULL, breaks = c("Total +ve deaths","Causal deaths","Coincidental deaths"), values = c("black","darkred","darkblue"))
#
#   Age_prev_plot_data <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr_label)) +
#     # Prevalence on positive in sample
#     geom_point(aes(y = 100*PosTests/Samples)) +
#     geom_errorbar(aes(ymin = 100*Hmisc::binconf(PosTests,Samples)[,"Lower"],
#                       ymax = 100*Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
#     xlab("Age") + ylab("+ve %")  +
#     ggtitle("\nCOVID-19 prevalence by age\nUTH mortuary") +
#     theme(plot.title = element_text(size = 10)) +
#     theme_minimal() +
#     scale_color_manual(name = NULL, breaks = c("Total +ve deaths","Causal deaths","Coincidental deaths"), values = c("black","darkred","darkblue")) +
#     scale_x_discrete(limits = c(paste0(1:16*5-5, "-",1:16*5-1),"80+")) +
#     theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
#
#     PCR_sero_comb_prev_plot_data <- ggplot(pcr_sero_data, aes(x = date, y = mean_pcr)) +
#       geom_point(aes(x= as.Date("2020-07-15"),y=7.6, color = "PCR % (Zambia)"), size = 2) +
#       geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10,color = "PCR % (Zambia)")) +
#       geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0,color = "PCR % (Zambia)")) +
#
#       geom_point(aes(x= as.Date("2020-07-15"),y=2.1, color = "Sero % (Zambia)"), size = 2) +
#       geom_errorbar(aes(ymin=1.1,ymax=3.1,x=as.Date("2020-07-15"), width=10, color = "Sero % (Zambia)")) +
#       geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0, color = "Sero % (Zambia)")) +
#
#       geom_point(aes(x= as.Date("2020-07-11"),y=9.1, color = "Combined % (Lusaka)"), size = 2) +
#       geom_errorbar(aes(ymin=2.6,ymax=15.7,x=as.Date("2020-07-11"), width=10, color = "Combined % (Lusaka)")) +
#       geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-19"),y=9.1, height=0, color = "Combined % (Lusaka)")) +
#
#       ylab(paste0("+ve %")) +
#       coord_cartesian(xlim = c(as.Date("2020-04-15"), as.Date("2020-10-01")),
#                       ylim = c(0, 28)) +
#       xlab("Date")+
#       theme_minimal() +
#       ggtitle(paste("\nCOVID-19 prevalence\nLusaka/Zambia")) +
#       scale_color_manual(name=NULL,
#                          breaks=c("Combined % (Lusaka)","PCR % (Zambia)", "Sero % (Zambia)"),
#                          values = c("black","darkgoldenrod1","chartreuse4"))
#
#
#     Prev_plot_data <- cowplot::plot_grid(
#       cowplot::plot_grid(Week_prev_plot_data + theme(legend.position = "none"), Age_prev_plot_data  + theme(legend.position = "none"), PCR_sero_comb_prev_plot_data + theme(legend.position="none"), nrow = 1),
#       cowplot::plot_grid(ggpubr::get_legend(Week_prev_plot_data),ggpubr::get_legend(PCR_sero_comb_prev_plot_data), rel_widths = c(2,1), nrow = 1),
#       nrow = 2, rel_heights = c(1,0.2))
#
#     Prev_plot <- cowplot::plot_grid(
#       cowplot::plot_grid(Week_prev_plot + theme(legend.position = "none"), Age_prev_plot  + theme(legend.position = "none"), PCR_sero_comb_prev_plot + theme(legend.position="none"), nrow = 1),
#       cowplot::plot_grid(ggpubr::get_legend(Week_prev_plot),ggpubr::get_legend(PCR_sero_comb_prev_plot), rel_widths = c(2,1), nrow = 1),
#       nrow = 2, rel_heights = c(1,0.2))
#
#     # browser()
#
#
#     return(list(Prev_plot = Prev_plot,
#                 Prev_plot_data = Prev_plot_data))
#
#   }
#
#
#
#   rm(list = ls()[!ls() %in% c("lls","Full_Prev_Plot","Poisson_Figure","p_excess_comp")])
#   gc()
# # browser()
#
#
#
#
# # Full_Prev_Plot <-  cowplot::plot_grid(Prev_plot,
# #                    cowplot::plot_grid(p_combined_prevalence_comp, p_Rt_Reff$plot, ncol = 2, rel_widths = c(1,0.5)),
# #                    nrow = 2, rel_heights = c(0.8,1))
#
#   # PCR_Combined_plots <- cowplot::plot_grid(p_Rt_Reff +ggtitle("Reff and Rt"), p_pcr_perc_age  + guides(col=guide_legend(ncol=2)),
#   #                                          p_combined_prevalence_comp_full, #+ guides(fill=guide_legend(ncol=2)),
#   #                                          p_combined_prevalence_comp_by_age,
#   #                                          ncol = 2, rel_widths = c(1,1))
#
#
#   return(list(lls = lls,
#               Full_Prev_Plot = Full_Prev_Plot,
#               # pcr_perc_age = p_pcr_perc_age,
#               # Rt_Reff = p_Rt_Reff$plot,
#               # combined_prevalence_comp = p_combined_prevalence_comp,
#               Poisson_Figure = Poisson_Figure,
#               # Prev_plot = Prev_plot,
#               p_excess_comp = p_excess_comp))
#
#
# }
#
#
# # Roll function
# roll_func_10 <- function(x, det) {
#   ret <- rep(0, length(x))
#   for(i in seq_along(ret)) {
#     to_sum <- x[seq_len(i)][max(1,i-length(det)+1):i]
#     ret[i] <- sum(rev(to_sum)*det[seq_along(to_sum)])
#   }
#   return(ret)
# }
#
#
# ## Prev function
# ll_prev_func <- function(df, det, Incidence, out, index, model_params = model_params){
#
#   Sus <- rowSums(out[,index$S])
#   Infs <- c(0,as.integer(diff(max(Sus)-Sus)))
#
#   if(Incidence == "Infections"){
#     positives <- roll_func_10(Infs, det)
#   }
#   if(Incidence == "Symptoms"){
#     Symps <- rowSums(out[,index$E2]) * model_params$gamma_E
#     positives <- roll_func_10(Symps, det)
#   }
#
#   percs <- positives/max(Sus)
#
#   test_dates <- seq.Date(from = df$date_start, to = df$date_end, by = 1)
#   perc_dates <- percs[as.Date(rownames(out)) %in% test_dates]
#
#   ll <- dbinom(df$pos_tests, df$samples, mean(perc_dates), log = TRUE)
#
#   return(ll)
# }
