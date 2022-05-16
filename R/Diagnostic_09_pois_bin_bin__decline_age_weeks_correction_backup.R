# Diagnostic_Plot_09 <- function(fit_num, fit_Model_list, IFRvals, Deaths_by_week = T, Deaths_by_Age = T, Mulenga_Prev=T){
#
#   fit_Model <- fit_Model_list[[fit_num]]
#   # lld <- fit_Model$pmcmc_results$inputs$pars_obs$lld
#
#   ##################################################################
#   ##################################################################
#   # Set 1: deaths by time
#   ##################################################################
#   ##################################################################
#   pcr_det <- fit_Model$pmcmc_results$inputs$pars_obs$pcr_det
#   comb_det <- fit_Model$pmcmc_results$inputs$pars_obs$comb_det
#   index <- squire:::odin_index(fit_Model$model)
#   Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
#                                    to = as.Date("2020-10-02"),
#                                    by = "week"),as.Date("2020-10-02"))
#   frac_mort <- 0.8
#
#   FullData <- readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")
#   FullData <- merge(FullData, data.frame(Age_gr = 1:17, Bg_dr = as.vector(colMeans(readRDS("~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")))*12/52))
#
#   Mod_Age_Deaths_Lus_pcr <- fit_Model$output[,c(index$D,index$S),] %>%
#     melt(varnames = c("date","var","Replicate"), value.name = "value") %>%
#     mutate(Age_gr = as.numeric(gsub(".*?([0-9]+).*", '\\1', var)),
#            var = substr(var, 1, 1),
#            date = as.Date(date)) %>%
#     dcast(... ~ var, value.var="value") %>%
#     rename(Mod_cd_Lus = "D", Sus = "S") %>%
#     group_by(Age_gr, Replicate) %>%
#     replace_na(list(Mod_cd_Lus = 0)) %>%
#     mutate(Sus = ifelse(is.na(Sus), max(Sus, na.rm=T), Sus)) %>%
#     mutate(infs = c(0, as.integer(diff(max(Sus, na.rm = T)-Sus))),
#            pcr_pos = roll_func(infs, pcr_det),
#            pcr_perc = pcr_pos/max(Sus,na.rm = T))
#
#   Mod_Age_Deaths_Lus <- Mod_Age_Deaths_Lus_pcr %>%
#     filter(date > "2020-06-10") %>%
#     mutate(Week_gr = cut.Date(date, breaks = "1 week", labels = 0:16)) %>%
#     group_by(Age_gr, Week_gr, Replicate) %>%
#     summarise(Mod_cd_Lus = max(Mod_cd_Lus),
#               pcr_perc = pcr_perc[4]) %>%
#     ungroup() %>% group_by(Age_gr, Replicate) %>%
#     mutate(Mod_cd_Lus = c(0,diff(Mod_cd_Lus))) %>%
#     filter(Week_gr != 0) %>%
#     merge(FullData)
#
#   ## Get the age standardisation:
#   Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus %>%
#     group_by(Week_gr, Replicate) %>%
#     dplyr::filter(Age_gr ==1) %>% summarise(ag1std = Mort_deaths/(Mod_cd_Lus*frac_mort + Bg_dr)) %>%
#     merge(Mod_Age_Deaths_Lus) %>%
#
#     # group_by(Age_gr, Week_gr, Replicate) %>%
#     mutate(Mod_cd_UTH = Mod_cd_Lus*frac_mort*ag1std,
#            Mod_ncd_UTH = Mort_deaths - Mod_cd_UTH,
#            Mod_ncd_UTH = ifelse(Mod_ncd_UTH<0,0,Mod_ncd_UTH),
#            Mod_pos_ncd_UTH = Mod_ncd_UTH*pcr_perc,
#            Mod_pos_ds_UTH = Mod_pos_ncd_UTH + Mod_cd_UTH,
#            # Mod_pos_ncd_Lus = Mod_ncd_UTH*pcr_perc/frac_mort,
#            # Mod_pos_ds_Lus = Mod_pos_ncd_Lus + Mod_cd_Lus,
#            Mod_tot_ds_Mort = Mod_cd_UTH + Bg_dr*ag1std,
#            Mod_tot_ds_UTH_unstd = Mod_tot_ds_Mort/ag1std) %>%
#
#
#     # Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus %>% group_by(Week_gr, Replicate) %>%
#     # dplyr::filter(Age_gr ==1) %>% summarise(ag1std = Mort_deaths/(Mod_cd_Lus*frac_mort + Bg_dr)) %>%
#     # merge(Mod_Age_Deaths_Lus) %>%
#     mutate(Pos_prev = ifelse(Mod_pos_ds_UTH/Mort_deaths>=1, 0.999, Mod_pos_ds_UTH/Mort_deaths))
#            # Mod_tot_ds_Mort_sc = Mod_tot_ds_Mort*ag1std)
#   if(!(fit_Model$pmcmc_results$inputs$pars_obs$lld %in% c("ll_pois_bin_include_4_5",
#                                                           "ll_pois_bin_remove_4_5",
#                                                           "ll_pois_bin_remove_4_5_from_pois_only",
#                                                           "ll_pois_bin_include_4_5_remove_youngest_age_group"))){
#     stop("LL for deaths not recognised.")
#   }
#
#   if(fit_Model$pmcmc_results$inputs$pars_obs$lld =="ll_pois_bin_include_4_5"){
#     Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus_Av %>%
#       # filter(Age_gr ==1, Replicate ==1)
#       mutate(ll_bin = dbinom(x = PosTests, size = Samples, prob = Pos_prev, log = T),
#              ll_pois = dpois(x = Mort_deaths, lambda = Mod_tot_ds_Mort, log = T))
#   }
#
#   if(fit_Model$pmcmc_results$inputs$pars_obs$lld =="ll_pois_bin_remove_4_5"){
#     Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus_Av %>%
#       # filter(Age_gr ==1, Replicate ==1)
#       mutate(ll_bin = ifelse(Week_gr %in% c(4,5), 0, dbinom(x = PosTests, size = Samples, prob = Pos_prev, log = T)),
#              ll_pois = ifelse(Week_gr %in% c(4,5), 0, dpois(x = Mort_deaths, lambda = Mod_tot_ds_Mort, log = T)))
#   }
#
#   if(fit_Model$pmcmc_results$inputs$pars_obs$lld =="ll_pois_bin_remove_4_5_from_pois_only"){
#     Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus_Av %>%
#       # filter(Age_gr ==1, Replicate ==1)
#       mutate(ll_bin = dbinom(x = PosTests, size = Samples, prob = Pos_prev, log = T),
#              ll_pois = ifelse(Week_gr %in% c(4,5), 0, dpois(x = Mort_deaths, lambda = Mod_tot_ds_Mort, log = T)))
#   }
#
#   if(fit_Model$pmcmc_results$inputs$pars_obs$lld =="ll_pois_bin_include_4_5_remove_youngest_age_group"){
#     Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus_Av %>%
#       # filter(Age_gr ==1, Replicate ==1)
#       mutate(ll_bin = ifelse(Age_gr %in% c(1), 0, dbinom(x = PosTests, size = Samples, prob = Pos_prev, log = T)),
#              ll_pois = ifelse(Age_gr %in% c(1), 0, dpois(x = Mort_deaths, lambda = Mod_tot_ds_Mort, log = T)))
#   }
#
#
#
#   Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus_Av %>%
#     group_by(Age_gr, Week_gr, date,Mort_deaths,Samples,PosTests) %>%
#     summarise_at(c("ll_bin", "ll_pois", "Mod_cd_Lus", "Mod_pos_ds_UTH", "Mod_pos_ncd_UTH", "Mod_cd_UTH","pcr_perc","Mod_tot_ds_Mort","Mod_tot_ds_UTH_unstd","Pos_prev"), list(mean = mean, min = min, max = max), na.rm = TRUE)
#
#
#
#     # filter(Age_gr ==2, Replicate ==1)
#
#   ## Plot by week
#   Mod_Age_Deaths_Lus_Av_Week <- Mod_Age_Deaths_Lus_Av %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
#     group_by(Week_gr) %>%
#     select(-date, -Age_gr) %>%
#     summarise_all(sum) %>%
#     merge(unique(Mod_Age_Deaths_Lus_Av[c("Week_gr","date")]))# %>%
#     # mutate(PointCol = ifelse(Week_gr %in% c(4,5), "red","black"))
# # browser()
#
# # if(fit_Model$pmcmc_results$inputs$pars_obs$lld == "ll_pois_bin_remove_4_5"){
# #   Plot_data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr != 4, Week_gr != 5)} else {Plot_data = Mod_Age_Deaths_Lus_Av_Week}
#
#   Lab1 <- "\nModelled total\n(inc. coincidental)\nmortuary\ndeaths/prev/tests\n"
#   Lab2 <- "Modelled true\nmortuary\ndeaths/prev/tests"
#   Lab3 <- "\nLeft:\nestimated cvd+ve\nmortuary deaths\n\nCentre:\nmortuary cvd+ve\nprevalence\n\nRight:\ncvd+ve tests\n\n"
#   Lab4 <- "Questionable\ndata points"
#
#   p1 <- ggplot(Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
#     # Modelled Positive deaths
#     geom_line(aes(y=Mod_pos_ds_UTH_mean, color = Lab1),linetype="dashed") +
#     geom_ribbon(aes(ymin=Mod_pos_ds_UTH_min, ymax=Mod_pos_ds_UTH_max), alpha=0.3) +
#     # Modelled True deaths
#     geom_line(aes(y=Mod_cd_UTH_mean, color = Lab2),linetype="dashed") +
#     geom_ribbon(aes(ymin=Mod_cd_UTH_min, ymax=Mod_cd_UTH_max), alpha=0.3, fill = "red") +
#     # Scaled positive deaths
#     geom_point(aes(y = (Mort_deaths*PosTests/Samples), alpha = Lab3)) +
#     geom_point(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)), aes(x = date, y = (Mort_deaths*PosTests/Samples), alpha = Lab4), col = "red") +
#     # Scaled True deaths
#     # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
#     xlab("Date") + ylab("Deaths") +
#     xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
#     ggtitle(paste("COVID-19 weekly deaths\n(UTH Mortuary) \nbin ll = ", round(sum(Mod_Age_Deaths_Lus_Av$ll_bin_mean),1))) +
#     theme(plot.title = element_text(size = 10),
#           legend.key = element_rect(fill = "white"))+
#     # guides(color = guide_legend(order = 2),
#            # alpha = guide_legend(order = 1)) +
#     scale_color_manual(name=NULL,
#                        breaks=c(Lab1,Lab2),
#                        values=c("black","black"),
#                        guide = guide_legend(override.aes = list(color = c("black","red")))) +
#
#     scale_alpha_manual(name=NULL,
#                        breaks=c(Lab3, Lab4),
#                        values = c(1,1),
#                        # values=c('black', 'red'),
#                        guide = guide_legend(override.aes = list(linetype = c(0,0),
#                                                                 shape = c(16,16),
#                                                                 color = c('black', 'red')),
#                                             order = 1))
#                        # guide = guide_legend(override.aes = list(linetype = c(2,2),
#                                                                 # shape = c(NA,NA))))
#     # scale_fill_manual(name='Second',
#       # breaks=c('Total', 'True'),
#       # values=c('black', 'darkred'))
#
#     # scale_color_(breaks=c('Data','spurious'),
#     #                   values=c("black","red"))
#     #
#   #
#   #
#   #
#   # ## Graph 2:
#   p2 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
#     # Prevalence on positive in sample
#     geom_point(aes(y = PosTests/Samples)) +
#     # geom_errorbar(aes(ymin = PosTests/Samples - qnorm(1-0.05/2)*sqrt((1/100)*(PosTests/Samples) *(1 - PosTests/Samples)),
#     #                   ymax = PosTests/Samples + qnorm(1-0.05/2)*sqrt((1/100)*(PosTests/Samples) *(1 - PosTests/Samples)))) +
#     geom_errorbar(aes(ymin = Hmisc::binconf(PosTests,Samples)[,"Lower"],
#                       ymax = Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
#     # Prevalence of True positive in sample
#     # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
#     # Modelled Positive Deaths (hospital)/Total hospital deaths
#     # geom_line(aes(y = Mod_pos_ds_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
#     geom_line(aes(y = Mod_pos_ds_UTH_mean/Mort_deaths),linetype="dashed") +
#     geom_ribbon(aes(ymin=Mod_pos_ds_UTH_min/Mort_deaths, ymax=Mod_pos_ds_UTH_max/Mort_deaths), alpha=0.3) +
#     # Modelled True Deaths (hospital)/Total hospital deaths
#     geom_line(aes(y = Mod_cd_UTH_mean/Mort_deaths),linetype="dashed") +
#     geom_ribbon(aes(ymin=Mod_cd_UTH_min/Mort_deaths, ymax=Mod_cd_UTH_max/Mort_deaths), alpha=0.3, fill = "red") +
#     xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
#     xlab("Date") + ylab("Prevalence")  +
#     ggtitle("COVID-19 weekly prevalence\n(UTH mortuary)") +
#     theme(plot.title = element_text(size = 10)) +
#     geom_point(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)), aes(x = date, y = PosTests/Samples), col = "red") +
#     geom_errorbar(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)),
#                   aes(ymin = Hmisc::binconf(PosTests,Samples)[,"Lower"],
#                       ymax = Hmisc::binconf(PosTests,Samples)[,"Upper"]), col = "red")
#
#
#
#   #
#   # ## Of rmy benefit, let's plot at the test level
#   p3 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
#     # Prevalence on positive in sample
#     geom_point(aes(y = PosTests)) +
#     # Prevalence of True positive in sample
#     # geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
#     # Modelled Positive Deaths (hospital)/Total hospital deaths
#     geom_line(aes(y = Samples*Mod_pos_ds_UTH_mean/Mort_deaths),linetype="dashed") +
#     geom_ribbon(aes(ymin=Samples*Mod_pos_ds_UTH_min/Mort_deaths, ymax=Samples*Mod_pos_ds_UTH_max/Mort_deaths), alpha=0.3) +
#     # Modelled True Deaths (hospital)/Total hospital deaths
#     geom_line(aes(y = Samples*Mod_cd_UTH_mean/Mort_deaths),linetype="dashed") +
#     geom_ribbon(aes(ymin=Samples*Mod_cd_UTH_min/Mort_deaths, ymax=Samples*Mod_cd_UTH_max/Mort_deaths), alpha=0.3, fill = "red") +
#     xlab("Date") + ylab("Positive tests")  +
#     xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
#     ggtitle("COVID-19 weekly positive\ntests (UTH mortuary)") +
#     theme(plot.title = element_text(size = 10)) +
#     geom_point(data = Mod_Age_Deaths_Lus_Av_Week %>% filter(Week_gr %in% c(4,5)), aes(x = date, y = PosTests), col = "red")
#
#
#
#   Mod_Age_Deaths_Lus_Av_Age <- Mod_Age_Deaths_Lus_Av %>% #merge(Mod_Age_Deaths_Lus_Av, FullData[,c("Age_gr","Week_gr","date")]) %>%
#     group_by(Age_gr) %>%
#     select(-date, -Week_gr) %>%
#     summarise_all(sum) %>%
#     merge(unique(Mod_Age_Deaths_Lus_Av[c("Week_gr","date")]))
#
#   p4 <- ggplot(Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr)) +
#     # Modelled Positive deaths
#     geom_line(aes(y=Mod_pos_ds_UTH_mean),linetype="dashed") +
#     geom_ribbon(aes(ymin=Mod_pos_ds_UTH_min, ymax=Mod_pos_ds_UTH_max), alpha=0.3) +
#     # Modelled True deaths
#     geom_line(aes(y=Mod_cd_UTH_mean),linetype="dashed") +
#     geom_ribbon(aes(ymin=Mod_cd_UTH_min, ymax=Mod_cd_UTH_max), alpha=0.3, fill = "red") +
#     # Scaled positive deaths
#     geom_point(aes(y = (Mort_deaths*PosTests/Samples))) +
#     # Scaled True deaths
#     # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
#     xlab("Age group") + ylab("Deaths") +
#     ggtitle(paste("COVID-19 deaths by age\n(UTH Mortuary)\nbin ll =", round(sum(Mod_Age_Deaths_Lus_Av$ll_bin_mean),1))) +
#     theme(plot.title = element_text(size = 10))
#   #
#   #
#   #
#   # ## Graph 2:
#   p5 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr)) +
#     # Prevalence on positive in sample
#     geom_point(aes(y = PosTests/Samples)) +
#     geom_errorbar(aes(ymin = Hmisc::binconf(PosTests,Samples)[,"Lower"],
#                       ymax = Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
#     # Prevalence of True positive in sample
#     # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
#     # Modelled Positive Deaths (hospital)/Total hospital deaths
#     geom_line(aes(y = Mod_pos_ds_UTH_mean/Mort_deaths),linetype="dashed") +
#     geom_ribbon(aes(ymin=Mod_pos_ds_UTH_min/Mort_deaths, ymax=Mod_pos_ds_UTH_max/Mort_deaths), alpha=0.3) +
#     # Modelled True Deaths (hospital)/Total hospital deaths
#     geom_line(aes(y = Mod_cd_UTH_mean/Mort_deaths),linetype="dashed") +
#     geom_ribbon(aes(ymin=Mod_cd_UTH_min/Mort_deaths, ymax=Mod_cd_UTH_max/Mort_deaths), alpha=0.3, fill = "red") +
#     xlab("Age group") + ylab("Prevalence")  +
#     ggtitle("COVID-19 prevalence by age\n(UTH mortuary)") +
#     theme(plot.title = element_text(size = 10))
#   #
#   # ## Of rmy benefit, let's plot at the test level
#   p6 <- ggplot(data = Mod_Age_Deaths_Lus_Av_Age, aes(x = Age_gr)) +
#     # Prevalence on positive in sample
#     geom_point(aes(y = PosTests)) +
#     # Prevalence of True positive in sample
#     # geom_point(aes(y = Samples*TrueDeathsHosp/deaths), col = "brown", shape = 1) +
#     # Modelled Positive Deaths (hospital)/Total hospital deaths
#     geom_line(aes(y = Samples*Mod_pos_ds_UTH_mean/Mort_deaths),linetype="dashed") +
#     geom_ribbon(aes(ymin=Samples*Mod_pos_ds_UTH_min/Mort_deaths, ymax=Samples*Mod_pos_ds_UTH_max/Mort_deaths), alpha=0.3) +
#     # Modelled True Deaths (hospital)/Total hospital deaths
#     geom_line(aes(y = Samples*Mod_cd_UTH_mean/Mort_deaths),linetype="dashed") +
#     geom_ribbon(aes(ymin=Samples*Mod_cd_UTH_min/Mort_deaths, ymax=Samples*Mod_cd_UTH_max/Mort_deaths), alpha=0.3, fill = "red") +
#     xlab("Age group") + ylab("Positive tests")  +
#     ggtitle("COVID-19 positive tests by\nage (UTH mortuary)") +
#     theme(plot.title = element_text(size = 10))
#
#
#   ## Combined, sero, pcr
#   sero_pcr <- seroprev_df(fit_Model)
#   pcr_sero_data <- Summ_sero_pcr_data(sero_pcr)
#
#   sero_pcr_ll <- sero_pcr %>% filter(as.Date(date) %in% as.Date(c("2020-07-15"))) %>%
#     select(pcr_perc, sero_perc) %>%
#     mutate(ll_pcr = dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_perc, log = T),
#            ll_sero = dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_perc, log = T)
#     ) %>%
#     summarise(ll_pcr = mean(ll_pcr),
#               ll_sero = mean(ll_sero))
#
#   comb_ll <- sero_pcr %>% filter(as.Date(date) %in% as.Date(c("2020-07-19", "2020-07-04","2020-07-11"))) %>%
#     select(date,combined_perc, replicate) %>%
#     mutate(ll_combined = dbinom(x = as.integer(0.091*332), size = as.integer(332), prob = combined_perc, log = T)
#     ) %>% group_by(replicate) %>%
#     summarise(ll_combined = mean(ll_combined)) %>% ungroup() %>%
#     summarise(ll_combined = mean(ll_combined))
# # browser()
#   # comb_ll <- sero_pcr %>% filter(as.Date(date) %in% seq.Date(from = as.Date("2020-07-04", to = "2020-07-19", by = 1))) %>%
#   #   select(date,combined_perc, replicate) %>%
#   #   mutate(ll_combined = dbinom(x = as.integer(0.091*332), size = as.integer(332), prob = combined_perc, log = T)
#   #   ) %>% group_by(replicate) %>%
#   #   summarise(ll_combined = mean(ll_combined)) %>% ungroup() %>%
#   #   summarise(ll_combined = mean(ll_combined))
#
#   p7 <- pcr_fit_plot(pcr_sero_data) + ggtitle(paste("PCR prevalence\nbinom ll =", round(sero_pcr_ll$ll_pcr,1))) + theme(plot.title = element_text(size = 10)) +
#     scale_color_manual(name=NULL,
#                        breaks=c("\nData from\nMulenga et al.\n", "Model fit"),
#                        values = c("black","black"),
#                        # values=c('black', 'red'),
#                        guide = guide_legend(override.aes = list(linetype = c(0,2),
#                                                                 shape = c(16,NA))))
#
#   p8 <- sero_fit_plot(pcr_sero_data) + ggtitle(paste("Seroprevalence \nbinom ll =", round(sero_pcr_ll$ll_sero,1))) + theme(plot.title = element_text(size = 10))
#   p9 <- combined_fit_plot(pcr_sero_data) + ggtitle(paste("Combined prevalence \nbinom ll =", round(comb_ll$ll_combined,1))) + theme(plot.title = element_text(size = 10))
# # browser()
#
#   title_gg <- ggplot() +
#     labs(title = paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),", Slope x ", round(IFRvals$Slope_x[fit_num],2)))
#   # browser()
#   # Figure1 <- cowplot::plot_grid(title_gg,
#                                 # cowplot::plot_grid(p1+theme(legend.position="none"),p2,p3,p4,p5,p6,p7,p8,p9), ncol = 1, rel_heights = c(0.05, 1))
#
#   Figure1 <- cowplot::plot_grid(title_gg,
#                                 cowplot::plot_grid(cowplot::plot_grid(p1+theme(legend.position="none"),p2,p3,p4,p5,p6, nrow = 2),cowplot::get_legend(p1), nrow = 1, rel_widths = c(1, 0.2)),
#                                 cowplot::plot_grid(p7+theme(legend.position="none"),p8,p9, cowplot::get_legend(p7), nrow = 1, rel_widths = c(1/3,1/3,1/3,0.2)), ncol = 1, rel_heights = c(0.05, 2/3,1/3))
#
#
#   # Age_groups.labs.bin <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_gr) %>%
#   #   summarise(ll_bin_mean = sum(ll_bin_mean)) %>%
#   #   mutate(lab = paste0("bin ll AG ",Age_gr,": ", round(ll_bin_mean,1))) %>%
#   #   mutate(lab = case_when(
#   #     Age_gr==1 ~ paste0("Age: 0-4; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==2 ~ paste0("Age: 5-9; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==3 ~ paste0("Age: 10-14; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==4 ~ paste0("Age: 15-19; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==5 ~ paste0("Age: 20-24; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==6 ~ paste0("Age: 25-29; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==7 ~ paste0("Age: 30-34; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==8 ~ paste0("Age: 35-39; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==9 ~ paste0("Age: 40-44; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==10 ~ paste0("Age: 45-49; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==11 ~ paste0("Age: 50-54; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==12 ~ paste0("Age: 55-59; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==13 ~ paste0("Age: 60-64; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==14 ~ paste0("Age: 65-69; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==15 ~ paste0("Age: 70-74; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==16 ~ paste0("Age: 75-79; Bin ll = ",round(ll_bin_mean,1)),
#   #     Age_gr==17 ~ paste0("Age: 80+; Bin ll = ",round(ll_bin_mean,1)))) %>%
#   #   select(lab) %>%
#   #   unlist()
#   # names(Age_groups.labs.bin) <- 1:17
#   #
#   # ### Plot bin fits by age and week.
#   # Figure2 <- ggplot(data = Mod_Age_Deaths_Lus_Av, aes(x = date)) +
#   #   # Prevalence on positive in sample
#   #   # geom_text(aes(y = PosTests/Samples+0.1, label=as.character(Samples)), size = 3) +
#   #   geom_point(aes(y = PosTests/Samples), size = 0.5) +
#   #   geom_errorbar(aes(ymin = Hmisc::binconf(PosTests,Samples)[,"Lower"],
#   #                     ymax = Hmisc::binconf(PosTests,Samples)[,"Upper"])) +
#   #   facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs.bin)) +
#   #   # Prevalence of True positive in sample
#   #   # geom_point(aes(y = TrueDeathsHosp/deaths), col = "brown", shape = 1) +
#   #   # Modelled Positive Deaths (hospital)/Total hospital deaths
#   #   # geom_line(aes(y = Mod_pos_ds_Lus_mean*frac_mort/Mort_deaths),linetype="dashed") +
#   #   geom_line(aes(y = Mod_pos_ds_UTH_mean/Mort_deaths),linetype="dashed") +
#   #   geom_ribbon(aes(ymin=Mod_pos_ds_UTH_min/Mort_deaths, ymax=Mod_pos_ds_UTH_max/Mort_deaths), alpha=0.3) +
#   #   # Modelled True Deaths (hospital)/Total hospital deaths
#   #   geom_line(aes(y = Mod_cd_UTH_mean/Mort_deaths),linetype="dashed") +
#   #   geom_ribbon(aes(ymin=Mod_cd_UTH_min/Mort_deaths, ymax=Mod_cd_UTH_max/Mort_deaths), alpha=0.3, fill = "red") +
#   #   # xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
#   #   xlab("Date") + ylab("Prevalence")  +
#   #   ggtitle(paste0("IFR x ", IFRvals$IFR_x[fit_num],", Slope x ", IFRvals$Slope_x[fit_num],"\nCOVID-19 weekly prevalence (Mortuary) \nbin ll = ", round(sum(Mod_Age_Deaths_Lus_Av$ll_bin_mean),1))) +
#   #   theme(plot.title = element_text(size = 10))
#
#   ####################
#   Age_groups.labs <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_gr) %>%
#     summarise(ll_pois_mean = sum(ll_pois_mean)) %>%
#     mutate(lab = paste0("pois ll AG ",Age_gr,": ", round(ll_pois_mean,1))) %>%
#     mutate(lab = case_when(
#       Age_gr==1 ~ paste0("Age: 0-4; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==2 ~ paste0("Age: 5-9; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==3 ~ paste0("Age: 10-14; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==4 ~ paste0("Age: 15-19; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==5 ~ paste0("Age: 20-24; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==6 ~ paste0("Age: 25-29; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==7 ~ paste0("Age: 30-34; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==8 ~ paste0("Age: 35-39; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==9 ~ paste0("Age: 40-44; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==10 ~ paste0("Age: 45-49; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==11 ~ paste0("Age: 50-54; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==12 ~ paste0("Age: 55-59; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==13 ~ paste0("Age: 60-64; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==14 ~ paste0("Age: 65-69; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==15 ~ paste0("Age: 70-74; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==16 ~ paste0("Age: 75-79; Pois ll = ",round(ll_pois_mean,1)),
#       Age_gr==17 ~ paste0("Age: 80+; Pois ll = ",round(ll_pois_mean,1)))) %>%
#     select(lab) %>%
#     unlist()
#   names(Age_groups.labs) <- 1:17
# # browser()Plot_data
#
#   # if(fit_Model$pmcmc_results$inputs$pars_obs$lld == "ll_pois_bin_remove_4_5"){
#   #   Plot_data2 = Mod_Age_Deaths_Lus_Av %>% filter(Week_gr != 4, Week_gr != 5)} else {Plot_data2 = Mod_Age_Deaths_Lus_Av}
# # browser()
#   Figure3 <- ggplot(Mod_Age_Deaths_Lus_Av, aes(x = date)) +
#     # Modelled Positive deaths
#     geom_line(aes(y=Mod_tot_ds_Mort_mean, color = "Model fit")) +
#     geom_ribbon(aes(ymin=Mod_tot_ds_Mort_min, ymax=Mod_tot_ds_Mort_max), alpha=0.3) +
#     facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_groups.labs)) +
#     # Modelled True deaths
#     geom_line(aes(y=Mod_tot_ds_UTH_unstd_mean, color = "Model fit (unstandardised)"),linetype="dashed", alpha = 0.5) +
#     geom_ribbon(aes(ymin=Mod_tot_ds_UTH_unstd_min, ymax=Mod_tot_ds_UTH_unstd_max), alpha=0.1) +
#     # geom_line(aes(y=Mod_cd_Lus_mean, color = "Model fit (unscaled)"),linetype="dashed", alpha = 0.5) +
#     # geom_ribbon(aes(ymin=Mod_cd_Lus_min, ymax=Mod_cd_Lus_min), alpha=0.1) +
#     # geom_line(aes(y=Mod_cd_Lus_mean, color = "Model fit (unscaled)2"),linetype="dashed", alpha = 0.5) +
#
#     # geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
#     # geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
#     # Scaled positive deaths
#     geom_point(aes(y = Mort_deaths, color = "Mortuary deaths")) +
#     # Scaled True deaths
#     # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
#     xlab("Date") + ylab("Deaths") +
#     # xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
#     ggtitle(paste0("IFR x ", round(IFRvals$IFR_x[fit_num],2),", Slope x ", round(IFRvals$Slope_x[fit_num],2),"\nCOVID-19 weekly deaths (Mortuary) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$ll_pois_mean),1))) +
#     geom_point(data = Mod_Age_Deaths_Lus_Av %>% filter(Week_gr %in% c(4,5)), aes(x = date, y = Mort_deaths, color = "Questionable data points")) +
#     theme(plot.title = element_text(size = 10),
#           legend.position = c(1,0), legend.justification = c(1,0),
#           legend.key = element_rect(fill = "white")) +
#     scale_color_manual(name=NULL,
#                        breaks=c("Mortuary deaths", "Questionable data points", "Model fit", "Model fit (unstandardised)"),
#                        values = c("black","red","black","black"),
#                        # values=c('black', 'red'),
#                        guide = guide_legend(override.aes = list(linetype = c(0,0,1,2),
#                                                                 shape = c(16,16,NA,NA),
#                                                                 alpha = c(1,1,1,0.5))))
#
#
#
#   # browser()
#
#
#
#   # Mod_Age_Deaths_Lus_Combined_Prev <- fit_Model$output[,c(index$S),] %>%
#   #   melt(varnames = c("date","var","Replicate"), value.name = "Sus") %>%
#   #   mutate(Age_gr = as.numeric(gsub(".*?([0-9]+).*", '\\1', var)),
#   #          var = substr(var, 1, 1),
#   #          date = as.Date(date)) %>%
#   #   # dcast(... ~ var, value.var="value") %>%
#   #   # rename(Sus = "S") %>%
#   #   mutate(Age_gr_10 = ifelse(Age_gr>=15, 8, ceiling(Age_gr/2))) %>%
#   #   group_by(Age_gr, Replicate) %>%
#   #   # replace_na(list(Mod_cd_Lus = 0)) %>%
#   #   mutate(Sus = ifelse(is.na(Sus), max(Sus, na.rm=T), Sus)) %>%
#   #   mutate(infs = c(0, as.integer(diff(max(Sus, na.rm = T)-Sus))),
#   #          comb_pos = roll_func(infs, comb_det),
#   #          comb_perc = comb_pos/max(Sus,na.rm = T))
#   #
#   #
#   # Comb_prev_df <- Mod_Age_Deaths_Lus_Combined_Prev %>% group_by(date, Age_gr_10) %>%
#   #   summarise(comb_perc_mean = mean(comb_perc),
#   #             comb_perc_min = min(comb_perc))
#
#
#   # Age_groups.labs_comb_prev <- Comb_prev_df %>% ungroup() %>% select(Age_gr_10) %>%
#   #   unique() %>%
#   #   # summarise(ll_pois_mean = sum(ll_pois_mean)) %>%
#   #   # mutate(lab = paste0("pois ll AG ",Age_gr_10,": ", round(ll_pois_mean,1))) %>%
#   #   mutate(lab = case_when(
#   #     Age_gr_10==1 ~ paste0("Age: 0-9"),
#   #     Age_gr_10==2 ~ paste0("Age: 10-19"),
#   #     Age_gr_10==3 ~ paste0("Age: 20-29"),
#   #     Age_gr_10==4 ~ paste0("Age: 30-39"),
#   #     Age_gr_10==5 ~ paste0("Age: 40-49"),
#   #     Age_gr_10==6 ~ paste0("Age: 50-59"),
#   #     Age_gr_10==7 ~ paste0("Age: 60-69"),
#   #     Age_gr_10==8 ~ paste0("Age: 70+"))) %>%
#   #   select(lab) %>%
#   #   unlist()
#   # names(Age_groups.labs_comb_prev) <- 1:8
#   #
#   #
#   # Mulenga_Combined_Prevalence_Age <- readRDS("analysis/data/Code-generated-data/14_Mulenga_Combined_Prevalence_by_Age.rds") %>%
#   #   mutate(Age_gr_10 = 1:8)
#   #
#   # Figure4 <- ggplot(merge(Comb_prev_df,Mulenga_Combined_Prevalence_Age), aes(x = date)) +
#   #   geom_line(aes(y=comb_perc_mean*100)) +
#   #   geom_ribbon(aes(ymin=comb_perc_min*100, ymax=comb_perc_min*100), alpha=0.3) +
#   #   facet_wrap(vars(Age_gr_10), labeller = labeller(Age_gr_10 = Age_groups.labs_comb_prev)) +
#   #   xlab("Date") + ylab("Combined prevalence %") +
#   #   ggtitle(paste0("IFR x ", IFRvals$IFR_x[fit_num],", Slope x ", IFRvals$Slope_x[fit_num],"\nModelled COVID-19 Combined prevalence Lusaka")) +
#   #   theme(plot.title = element_text(size = 10)) +
#   #   geom_point(data = Mulenga_Combined_Prevalence_Age, aes(x= as.Date("2020-07-15"), y = Combined_Prev)) +
#   #   geom_errorbar(data = Mulenga_Combined_Prevalence_Age, aes(ymin=LowerInt,ymax=HigherInt,x=as.Date("2020-07-15"), width=10)) +
#   #   geom_errorbarh(mapping = aes(y = Combined_Prev, xmin=as.Date("2020-07-04"), xmax=as.Date("2020-07-27"),height = 0))
#
#
#   lls <- list(
#     ll_bin = round(sum(Mod_Age_Deaths_Lus_Av$ll_bin_mean),1),
#     ll_pois = round(sum(Mod_Age_Deaths_Lus_Av$ll_pois_mean),1),
#     ll_pcr = sero_pcr_ll$ll_pcr,
#     ll_sero = sero_pcr_ll$ll_sero,
#     ll_comb = comb_ll$ll_combined)
#
#
#
#   return(list(Diagnostic = Figure1, #Bin_fits = Figure2,
#               Pois_fits = Figure3, #Infection_rate = Figure4,
#               lls = lls))
#
#
# }
