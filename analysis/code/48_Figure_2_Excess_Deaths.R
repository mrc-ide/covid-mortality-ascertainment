## get a basic estimate of what the figure 2 might look like:
library(dplyr)
library(ggplot2)
library(tidyr)

## Get median all cause mortality for 2018-2019:
# Burial_df <- readRDS("analysis/data/Code-generated-data/00_07_Burial_registrations_by_week_2017_to_2021.rds")
Burial_df <- readRDS("analysis/data/Code-generated-data/00_07_Burial_registrations_2017_2021.rds") %>%
  # group_by(Age_gr_fig_2, Week_st) %>%
  group_by(Age_gr_fig_2b, Week_st) %>%
  summarise(Total_deaths = n()) %>%
  ungroup() %>% complete(Age_gr_fig_2b, Week_st, fill = list(Total_deaths = 0))

Burs_2018_2019 <- Burial_df %>%
  filter(Week_st >=as.Date("2018-01-01"),
         Week_st <as.Date("2019-12-25"))

Burs_2018_2021 <- Burial_df %>%
  filter(Week_st >=as.Date("2018-01-01"),
         Week_st <as.Date("2021-06-12"))


Median_Baseline <- Burs_2018_2019 %>% group_by(Age_gr_fig_2b) %>%
  summarise(Median_Baseline = median(Total_deaths),
            Baseline_ci_low = bayestestR::ci(Total_deaths)$CI_low,
            Baseline_ci_high = bayestestR::ci(Total_deaths)$CI_high)

Burs_Excess <- Burs_2018_2021 %>%
  merge(Median_Baseline) %>%
  mutate(Excess_burs = Total_deaths - Median_Baseline,
         Excess_burs_ci_low = Total_deaths - Baseline_ci_low,
         Excess_burs_ci_high = Total_deaths - Baseline_ci_high,
         Age_gr_fig_2b = as.factor(Age_gr_fig_2b))

Median_Baseline_total <- Burs_2018_2019 %>% group_by(Week_st) %>% summarise(Total_deaths = sum(Total_deaths)) %>%
  summarise(Median_Baseline = median(Total_deaths),
            Baseline_ci_low = bayestestR::ci(Total_deaths)$CI_low,
            Baseline_ci_high = bayestestR::ci(Total_deaths)$CI_high)


Burs_Excess_total <- Burs_2018_2021 %>%
  group_by(Week_st) %>% summarise(Total_deaths = sum(Total_deaths)) %>%
  merge(Median_Baseline_total) %>%
  mutate(Excess_burs = Total_deaths - Median_Baseline,
         Excess_burs_ci_low = Total_deaths - Baseline_ci_low,
         Excess_burs_ci_high = Total_deaths - Baseline_ci_high)


Age_gr_fig_2b.labs <- c(paste0(c(0,5, 15,25, 40,50,60),"-",c(5, 15,25,40,50,60,70)),"70+")
names(Age_gr_fig_2b.labs) <- 1:8

p1 <- ggplot(Burs_Excess, aes(x = Week_st, y = Total_deaths, color = Age_gr_fig_2b)) +
  geom_line() +
  facet_wrap(~Age_gr_fig_2b, ncol = 1, labeller = labeller(Age_gr_fig_2b = Age_gr_fig_2b.labs)) +
  # geom_ribbon(aes(ymin = Excess_burs_ci_low, ymax = Excess_burs_ci_high, fill = as.factor(Age_gr_fig_2)), alpha = 0.1) +
  theme_minimal() +
  xlab("") + ylab("Burial registrations compared with Jan 2018-Dec 2019 median") +
  viridis::scale_color_viridis("Age group", discrete = T, option = "H",
                               # labels = c(0,seq(5,75,by = 10),Inf)
                               labels = c(paste0(c(0,5, 15,25, 40,50,60),"-",c(5, 15,25,40,50,60,70)),"70+")
                               ) +
  # coord_cartesian(ylim = c(-60,100)) +
  coord_cartesian(ylim = c(0,100)) +
  # geom_hline(yintercept = 0, linetype = 2)
  geom_hline(aes(yintercept = Median_Baseline), linetype = 2) +
  scale_x_date(expand = c(0,0)) +
  ggtitle("A")

#### Plot comparable results for model output...
# ExcessMort_df <- readRDS(file = "analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week_checked_extended_with_Feb.rds") %>%
#   mutate(Age_gr_fig_2b = case_when(Age_gr == 1 ~ 1,
#                                Age_gr %in% 2:3 ~ 2,
#                                Age_gr %in% 4:5 ~ 3,
#                                Age_gr %in% 6:8 ~ 4,
#                                Age_gr %in% 9:10 ~ 5,
#                                Age_gr %in% 11:12 ~ 6,
#                                Age_gr %in% 13:14 ~ 7,
#                                Age_gr %in% 15:17 ~ 8)) %>%
#   filter(Week_gr != 181) %>%
#   select(-Age_gr) %>%
#   group_by(Age_gr_fig_2b, Week_gr) %>%
#   summarise_all(sum) %>%
#   mutate(Week_gr = Week_gr +104) %>%
#   merge(data.frame(Week_gr = 105:180, Week_st = as.Date(c(Burs_2018_2021 %>% pull(Week_st) %>% unique() %>% sort())[105:180])),all =T)


### mcmc results
# mcmc <- readRDS("../Bonus Files/2022-05-24_Baseline_Mortality_mcmc_Gamma_Prior_inc_Feb_2021.rds")
mcmc <- readRDS("../Bonus Files/2022-08-19_17_Baseline_Mortality_mcmc_Gamma_Prior_inc_Feb_2021.rds")
mcmc_samples <-mcmc$output %>% filter(phase =="sampling")
AG1_2020_mcmc <- mcmc_samples[, c(paste0("Week_rate_0to5_",c(1:180)))]
AG1_mcmc <- mcmc_samples[, c(paste0("Week_rate_0to5_",c(1:180)))]

## AG1 Pre 2020 background death rate: Bg_dr_mcmc
# Mort_deaths <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age_weeks.rds")


# Mort_deaths <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age_weeks_2020_plus.rds")


## Non-covid deaths in the mortuary: 2020_AG1 * RR
# Mort_ncd_mcmc
# Mort_excess_deaths <- lapply(1:nrow(AG1_mcmc), function(x){
#   # browser()
#   mcmc_samples <- cbind(as.numeric(AG1_mcmc[x,]),
#                         as.numeric(AG1_mcmc[x,]) %*% t(as.numeric(mcmc_samples[x,paste0("RR",2:17)])))
#   rownames(mcmc_samples) <- 1:104
#   colnames(mcmc_samples) <- 1:17
#   mcmc_samples <- mcmc_samples %>% reshape2::melt(value.name = "Mort_ncd_mcmc", varnames = c("Week_gr", "Age_gr"))
#   excess_deaths <- merge(mcmc_samples,Mort_deaths, all = T) %>%
#     mutate(excess = deaths - Mort_ncd_mcmc)
#
#   return(excess_deaths)
# })

Burial_df <- readRDS("analysis/data/Code-generated-data/00_07_Burial_registrations_2017_2021.rds") %>%
  # group_by(Age_gr_fig_2, Week_st) %>%
  group_by(Age_gr, Week_st) %>%
  summarise(Total_deaths = n()) %>%
  ungroup() %>% complete(Age_gr, Week_st, fill = list(Total_deaths = 0))

Burs_2018_2021 <- Burial_df %>%
  filter(Week_st >=as.Date("2018-01-01"),
         Week_st <as.Date("2021-06-12"))

Burs_2020_2021 <- Burial_df %>%
  filter(Week_st >=as.Date("2019-12-30"),
         Week_st <as.Date("2021-06-12"))

Burs_2018_2021_Total <- Burs_2018_2021 %>% group_by(Week_st) %>%
  summarise(Total_deaths = sum(Total_deaths))

Ag1std <- Burs_2018_2021 %>%
  rename("Ag1std" = Total_deaths) %>%
  filter(Age_gr ==1) %>%
  mutate(Week_gr = 1:180)

Dates_df <- Ag1std %>% select(Week_st, Week_gr)

AG1_pre2020_mcmc <- mcmc_samples[, c(paste0("Week_rate_0to5_",1:104))]
U5_Baseline_2018_2019 <- rowMeans(AG1_pre2020_mcmc)
WeeklyStandardise <- apply(AG1_mcmc, 2, function(x){x/U5_Baseline_2018_2019})
# colnames(WeeklyStandardise) <- 1:16
colnames(WeeklyStandardise) <- 1:180
WeeklyStandardise <- WeeklyStandardise %>%
  reshape2::melt(value.name = "Standard", varnames = c("list_names","Week_gr"))

Pop_Str <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Str_2020_imp_ests.rds")

Mort_excess_deaths <- lapply(1:17, function(y){
  lapply(1:180, function(x){
    if(y == 1){tmp_df <- AG1_mcmc[,paste0("Week_rate_0to5_",x)]} else {tmp_df <- AG1_mcmc[,paste0("Week_rate_0to5_",x)] * mcmc_samples[,paste0("RR",y)]}
    # tmp_df_pc <- 1000*tmp_df/Pop_Str[y]
    Excess <- merge(Burs_2018_2021, Dates_df) %>% filter(Week_gr == x, Age_gr == y) %>% pull(Total_deaths) - tmp_df
    Excess_pc <- 1000*Excess/Pop_Str[y]
    Excess_std <- Excess/(WeeklyStandardise %>% filter(Week_gr == x) %>% pull(Standard))
    Excess_std_pc <- 1000*Excess_std/Pop_Str[y]
    Tot <- Excess + tmp_df
    Tot_pc <- 1000*Excess/Pop_Str[y]
    Tot_std <- Excess_std + tmp_df/(WeeklyStandardise %>% filter(Week_gr == x) %>% pull(Standard))
    Tot_std_pc <- 1000*Excess_std/Pop_Str[y]

    # Excess_std_cum <- Excess/(WeeklyStandardise %>% filter(Week_gr == x) %>% pull(Standard))
    return(data.frame(Week_gr = x, Age_gr = y,
                    Median = median(tmp_df), CI_low = bayestestR::ci(tmp_df)$CI_low, CI_high = bayestestR::ci(tmp_df)$CI_high,
                    # pc_Median = median(tmp_df_pc), pc_CI_low = bayestestR::ci(tmp_df_pc)$CI_low, pc_CI_high = bayestestR::ci(tmp_df_pc)$CI_high,
                    Excess_Median = median(Excess), Excess_CI_low = bayestestR::ci(Excess)$CI_low, Excess_CI_high = bayestestR::ci(Excess)$CI_high,
                    pc_Excess_Median = median(Excess_pc), pc_Excess_CI_low = bayestestR::ci(Excess_pc)$CI_low, pc_Excess_CI_high = bayestestR::ci(Excess_pc)$CI_high,
                    Excess_std_Median = median(Excess_std), Excess_std_CI_low = bayestestR::ci(Excess_std)$CI_low, Excess_std_CI_high = bayestestR::ci(Excess_std)$CI_high,
                    pc_Excess_std_Median = median(Excess_std_pc), pc_Excess_std_CI_low = bayestestR::ci(Excess_std_pc)$CI_low, pc_Excess_std_CI_high = bayestestR::ci(Excess_std_pc)$CI_high,
                    Tot_Median = median(Tot), Tot_CI_low = bayestestR::ci(Tot)$CI_low, Tot_CI_high = bayestestR::ci(Tot)$CI_high,
                    pc_Tot_Median = median(Tot_pc), pc_Tot_CI_low = bayestestR::ci(Tot_pc)$CI_low, pc_Tot_CI_high = bayestestR::ci(Tot_pc)$CI_high,
                    Tot_std_Median = median(Tot_std), Tot_std_CI_low = bayestestR::ci(Tot_std)$CI_low, Tot_std_CI_high = bayestestR::ci(Tot_std)$CI_high,
                    pc_Tot_std_Median = median(Tot_std_pc), pc_Tot_std_CI_low = bayestestR::ci(Tot_std_pc)$CI_low, pc_Tot_std_CI_high = bayestestR::ci(Tot_std_pc)$CI_high))
  }) %>% str2str::ld2d() %>% select(-row_names)
}) %>% str2str::ld2d() %>% select(-row_names, -list_names)


Mort_excess_deaths_cum <- lapply(1:17, function(y){
  Test_df <- lapply(sample(1:nrow(AG1_mcmc),1000), function(x){
    # if(y == 2){browser()}
    if(y == 1){tmp_df <- AG1_mcmc[x,105:180]} else {tmp_df <- AG1_mcmc[x,105:180] * mcmc_samples[x,paste0("RR",y)]}
    Excess <- merge(Burs_2020_2021, Dates_df) %>% filter(Age_gr == y) %>% pull(Total_deaths) - tmp_df
    Excess_std <- Excess/(WeeklyStandardise %>% filter(list_names == x, Week_gr >104) %>% pull(Standard))
    Excess_std_cum <- cumsum(x = unlist(Excess_std))
    return(Excess_std_cum)
  })
  Test_df <- Test_df %>% str2str::lv2d(along = 2) %>% as_tibble() %>% rowwise() %>% summarise(median = median(c_across()),
                                                                                   CI_low = bayestestR::ci(c_across())$CI_low,
                                                                                   CI_high = bayestestR::ci(c_across())$CI_high) %>%
    mutate(Age_gr = y,
           Week_gr = 105:180)


}) %>% str2str::ld2d() %>% select(-row_names, -list_names)


##################################################################
ExcessMort_df <- Mort_excess_deaths %>%
  merge(merge(Burs_2018_2021,Dates_df)) %>%
  mutate(Age_gr_fig_2b = case_when(Age_gr == 1 ~ 1,
                               Age_gr %in% 2:3 ~ 2,
                               Age_gr %in% 4:5 ~ 3,
                               Age_gr %in% 6:8 ~ 4,
                               Age_gr %in% 9:10 ~ 5,
                               Age_gr %in% 11:12 ~ 6,
                               Age_gr %in% 13:14 ~ 7,
                               Age_gr %in% 15:17 ~ 8)) %>%
  # filter(Week_gr != 181) %>%
  select(-Age_gr, -Week_st) %>%
  group_by(Age_gr_fig_2b, Week_gr) %>%
  summarise_all(sum) %>%
  merge(Dates_df) %>%
  mutate(Age_gr_fig_2b = as.factor(Age_gr_fig_2b))


p2 <- ggplot(ExcessMort_df, aes(x = Week_st, y = Median, color = Age_gr_fig_2b, fill = Age_gr_fig_2b)) +
  geom_line() +
  geom_point(aes(y = Total_deaths), col = "black", size = 0.3) +
  facet_wrap(~Age_gr_fig_2b, ncol = 1, labeller = labeller(Age_gr_fig_2b = Age_gr_fig_2b.labs)) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.4, color = NA) +
  theme_minimal() +
  xlab("") + ylab("Burial registrations with model fit and predictions") +
  viridis::scale_color_viridis("Age group", discrete = T, option = "H",
                               # labels = c(0,seq(5,75,by = 10),Inf)
                               labels = c(paste0(c(0, 5, 15,25, 40,50,60),"-",c(5,15,25,40,50,60,70)),"70+")
  ) +
  viridis::scale_fill_viridis("Age group", discrete = T, option = "H",
                              # labels = c(0,seq(5,75,by = 10),Inf)
                              labels = c(paste0(c(0, 5, 15,25, 40,50,60),"-",c(5,15,25,40,50,60,70)),"70+")
  ) +
  coord_cartesian(ylim = c(0,100)) +
  scale_x_date(expand = c(0,0)) +
  ggtitle("B")




Mort_excess_deaths_total <- lapply(1:180, function(x){
  tmp_df <- lapply(1:17, function(y){
    if(y == 1){tmp_df <- AG1_mcmc[,paste0("Week_rate_0to5_",x)]} else {tmp_df <- AG1_mcmc[,paste0("Week_rate_0to5_",x)] * mcmc_samples[,paste0("RR",y)]}
    return(tmp_df)
  })

  tmp_sum <- tmp_df %>% str2str::lv2d(along = 2) %>% rowSums()
  tmp_excess <- merge(Burs_2018_2021_Total, Dates_df) %>% filter(Week_gr == x) %>% pull(Total_deaths) - tmp_sum
  tmp_excess_std <- tmp_excess/(WeeklyStandardise %>% filter(Week_gr == x) %>% pull(Standard))
  tmp_sum_pc <- 1000*tmp_sum/sum(Pop_Str)
  tmp_excess_pc <- 1000*tmp_excess/sum(Pop_Str)
  tmp_excess_std_pc <- 1000*tmp_excess_std/sum(Pop_Str)



  return(data.frame(Week_gr = x,
                    Median = median(tmp_sum), CI_low = bayestestR::ci(tmp_sum)$CI_low, CI_high = bayestestR::ci(tmp_sum)$CI_high,
                    pc_Median = median(tmp_sum_pc), pc_CI_low = bayestestR::ci(tmp_sum_pc)$CI_low, pc_CI_high = bayestestR::ci(tmp_sum_pc)$CI_high,
                    Excess_Median = median(tmp_excess), Excess_CI_low = bayestestR::ci(tmp_excess)$CI_low, Excess_CI_high = bayestestR::ci(tmp_excess)$CI_high,
                    pc_Excess_Median = median(tmp_excess_pc), pc_Excess_CI_low = bayestestR::ci(tmp_excess_pc)$CI_low, pc_Excess_CI_high = bayestestR::ci(tmp_excess_pc)$CI_high,
                    Excess_std_Median = median(tmp_excess_std), Excess_std_CI_low = bayestestR::ci(tmp_excess_std)$CI_low, Excess_std_CI_high = bayestestR::ci(tmp_excess_std)$CI_high,
                    pc_Excess_std_Median = median(tmp_excess_std_pc), pc_Excess_std_CI_low = bayestestR::ci(tmp_excess_std_pc)$CI_low, pc_Excess_std_CI_high = bayestestR::ci(tmp_excess_std_pc)$CI_high))
}) %>% str2str::ld2d() %>% select(-row_names, -list_names)

WeeklyStandardise_sum <- WeeklyStandardise %>% ungroup() %>%group_by(Week_gr) %>% summarise(
  median = median(Standard),
  ci_low = bayestestR::ci(Standard)$CI_low,
  ci_high = bayestestR::ci(Standard)$CI_high
) %>% merge(Dates_df) %>%
  filter(Week_gr > 104)





# ExcessMort_df_plot_3a <- rbind(ExcessMort_df %>% select(-Total_deaths, -Pop_str), Mort_excess_deaths_total %>% mutate(Age_gr_fig_2b = "Total") %>% merge(Dates_df))

Age_gr_fig_2b.labs_total <- Age_gr_fig_2b.labs
Age_gr_fig_2b.labs_total <- c("Total",Age_gr_fig_2b.labs_total)
names(Age_gr_fig_2b.labs_total)[1] <- "Total"
ExcessMort_df_plot_3a$Age_gr_fig_2b <- factor(ExcessMort_df_plot_3a$Age_gr_fig_2b,
                                              levels = c("Total",1:8))

p3a <- ggplot(ExcessMort_df_plot_3a %>% filter(Week_gr >104, Age_gr_fig_2b != 1), aes(x = Week_st, y = Excess_Median, color = Age_gr_fig_2b, fill = Age_gr_fig_2b)) +
  geom_line() +
  facet_wrap(~Age_gr_fig_2b, ncol = 1, labeller = labeller(Age_gr_fig_2b = Age_gr_fig_2b.labs_total), scales = "free_y") +
  geom_ribbon(aes(ymin = Excess_CI_low, ymax = Excess_CI_high, fill = Age_gr_fig_2b), alpha = 0.4, color = NA) +
  theme_minimal() +
  xlab("") + ylab("Burial registrations minus model predictions") +
  viridis::scale_color_viridis("Age group", discrete = T, option = "H",
                               # labels = c(0,seq(5,75,by = 10),Inf)
                               labels = c("Total",paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")) +
  viridis::scale_fill_viridis("Age group", discrete = T, option = "H",
                              # labels = c(0,seq(5,75,by = 10),Inf)
                              labels = c("Total",paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")) +
  # coord_cartesian(ylim = c(-30,200)) +
  scale_y_continuous(limits = c(-30,100)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_date(position = "bottom", breaks = seq(as.Date("2020-01-01"), as.Date("2021-07-01"), by="6 months"), date_labels = "%b-%Y", expand = c(0,0)) +
  ggh4x::facetted_pos_scales(y = list(Age_gr_fig_2b == "Total" ~ scale_y_continuous(limits = c(-200,200)),
                                      Age_gr_fig_2b %in% as.character(2:8) ~ scale_y_continuous(limits = c(-30,100))))


p4a <- ggplot(merge(ExcessMort_df_plot_3a,data.frame(WeeklyStandardise_sum, Age_gr_fig_2b="Total"), all.x = T) %>% filter(Week_gr >104, Age_gr_fig_2b != 1), aes(x = Week_st)) +
  geom_line(aes(y = Excess_Median), color = "black", linetype = 2) +
  geom_ribbon(aes(ymin = Excess_CI_low, ymax = Excess_CI_high), alpha = 0.4, color = NA, fill = "black") +
  geom_line(aes(y = Excess_std_Median, col = Age_gr_fig_2b)) +
  geom_ribbon(aes(ymin = Excess_std_CI_low, ymax = Excess_std_CI_high, fill = Age_gr_fig_2b), alpha = 0.4, color = NA) +
  facet_wrap(~Age_gr_fig_2b, ncol = 1, labeller = labeller(Age_gr_fig_2b = Age_gr_fig_2b.labs_total), scales = "free_y") +
  theme_minimal() +
  xlab("") + ylab("Burial registrations minus scaled model predictions") +
  viridis::scale_color_viridis("Age group", discrete = T, option = "H",
                               # labels = c(0,seq(5,75,by = 10),Inf)
                               labels = c("Total",paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")) +
  viridis::scale_fill_viridis("Age group", discrete = T, option = "H",
                              # labels = c(0,seq(5,75,by = 10),Inf)
                              labels = c("Total",paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")) +
  # ylim(c(-60,200)) +
  scale_y_continuous(limits = c(-30,100)) +
  # coord_cartesian(ylim = c(-50,150)) +
  geom_line(data = subset(merge(ExcessMort_df_plot_3a,data.frame(WeeklyStandardise_sum, Age_gr_fig_2b="Total"), all.x = T) %>% filter(Week_gr >104, Age_gr_fig_2b != 1), Age_gr_fig_2b=="Total"),
            aes(x = Week_st, y = (median-1)/0.005), linetype = 1, color = "red", size = 1) +
  geom_ribbon(data = subset(merge(ExcessMort_df_plot_3a,data.frame(WeeklyStandardise_sum, Age_gr_fig_2b="Total"), all.x = T) %>% filter(Week_gr >104, Age_gr_fig_2b != 1), Age_gr_fig_2b=="Total"),
              aes(x = Week_st, ymin = (ci_low-1)/0.005, ymax = (ci_high-1)/0.005), linetype = 1, fill = "red", color = NA, size = 1, alpha = 0.4) +
  ggh4x::facetted_pos_scales(y = list(Age_gr_fig_2b == "Total" ~ scale_y_continuous(limits = c(-200,500),
                                                                                    sec.axis = sec_axis(~.*0.005+1, name="Standardisation", labels = c(0,1,2), breaks = c(0,1,2))),
                                      Age_gr_fig_2b %in% as.character(2:8) ~ scale_y_continuous(limits = c(-30,100)))) +
  scale_x_date(position = "bottom", breaks = seq(as.Date("2020-01-01"), as.Date("2021-07-01"), by="6 months"), date_labels = "%b-%Y", expand = c(0,0)) +
  theme(axis.title.y.right = element_text(hjust = 0)) +
  geom_hline(yintercept = 0, linetype = 2)

p5_cum <- ggplot(merge(ExcessMort_df_plot_3a,data.frame(WeeklyStandardise_sum, Age_gr_fig_2b="Total"), all.x = T) %>% filter(Week_gr >104, Age_gr_fig_2b != 1), aes(x = Week_st)) +
  geom_line(aes(y = Excess_Median), color = "black", linetype = 2) +
  geom_ribbon(aes(ymin = Excess_CI_low, ymax = Excess_CI_high), alpha = 0.4, color = NA, fill = "black") +
  geom_line(aes(y = Excess_std_Median, col = Age_gr_fig_2b)) +
  geom_ribbon(aes(ymin = Excess_std_CI_low, ymax = Excess_std_CI_high, fill = Age_gr_fig_2b), alpha = 0.4, color = NA) +
  facet_wrap(~Age_gr_fig_2b, ncol = 1, labeller = labeller(Age_gr_fig_2b = Age_gr_fig_2b.labs_total), scales = "free_y") +
  theme_minimal() +
  xlab("") + ylab("Burial registrations minus scaled model predictions") +
  viridis::scale_color_viridis("Age group", discrete = T, option = "H",
                               # labels = c(0,seq(5,75,by = 10),Inf)
                               labels = c("Total",paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")) +
  viridis::scale_fill_viridis("Age group", discrete = T, option = "H",
                              # labels = c(0,seq(5,75,by = 10),Inf)
                              labels = c("Total",paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")) +
  # ylim(c(-60,200)) +
  scale_y_continuous(limits = c(-30,100)) +
  # coord_cartesian(ylim = c(-50,150)) +
  geom_line(data = subset(merge(ExcessMort_df_plot_3a,data.frame(WeeklyStandardise_sum, Age_gr_fig_2b="Total"), all.x = T) %>% filter(Week_gr >104, Age_gr_fig_2b != 1), Age_gr_fig_2b=="Total"),
            aes(x = Week_st, y = (median-1)/0.005), linetype = 1, color = "red", size = 1) +
  geom_ribbon(data = subset(merge(ExcessMort_df_plot_3a,data.frame(WeeklyStandardise_sum, Age_gr_fig_2b="Total"), all.x = T) %>% filter(Week_gr >104, Age_gr_fig_2b != 1), Age_gr_fig_2b=="Total"),
              aes(x = Week_st, ymin = (ci_low-1)/0.005, ymax = (ci_high-1)/0.005), linetype = 1, fill = "red", color = NA, size = 1, alpha = 0.4) +
  ggh4x::facetted_pos_scales(y = list(Age_gr_fig_2b == "Total" ~ scale_y_continuous(limits = c(-200,500),
                                                                                    sec.axis = sec_axis(~.*0.005+1, name="Standardisation", labels = c(0,1,2), breaks = c(0,1,2))),
                                      Age_gr_fig_2b %in% as.character(2:8) ~ scale_y_continuous(limits = c(-30,100)))) +
  scale_x_date(position = "bottom", breaks = seq(as.Date("2020-01-01"), as.Date("2021-07-01"), by="6 months"), date_labels = "%b-%Y", expand = c(0,0)) +
  theme(axis.title.y.right = element_text(hjust = 0)) +
  geom_hline(yintercept = 0, linetype = 2)



pdf("analysis/figures/48_Figure_2_Excess_Mortality.pdf", width = 12, height = 10)
cowplot::plot_grid(p1 + theme(legend.position = "none"),
                   p2 + theme(legend.position = "none"),
                   p3a + theme(legend.position = "none"),
                   p4a + theme(legend.position = "none"), nrow = 1, rel_widths = c(1,1,1,1.1))
dev.off()

tiff("analysis/figures/48_Figure_2_Excess_Mortality.tiff", width = 12, height = 10, units = "in", res = 300)
cowplot::plot_grid(p1 + theme(legend.position = "none"),
                   p2 + theme(legend.position = "none"),
                   p3a + theme(legend.position = "none"),
                   p4a + theme(legend.position = "none"), nrow = 1)
dev.off()


###########
# Version 2
p3b <- ggplot(ExcessMort_df_plot_3a %>% filter(Week_gr >104, Age_gr_fig_2b != "Total") %>% mutate(Excess_Median = ifelse(Age_gr_fig_2b==1, 0, Excess_Median),
                                                                                                  Excess_CI_low = ifelse(Age_gr_fig_2b==1, 0, Excess_CI_low),
                                                                                                  Excess_CI_high = ifelse(Age_gr_fig_2b==1, 0, Excess_CI_high)), aes(x = Week_st)) +
  geom_line(aes(y = Excess_Median, color = Age_gr_fig_2b), linetype = 1) +
  geom_ribbon(aes(ymin = Excess_CI_low, ymax = Excess_CI_high, fill = Age_gr_fig_2b), alpha = 0.4, color = NA) +
  # geom_line(aes(y = Excess_std_Median, col = Age_gr_fig_2b)) +
  # geom_ribbon(aes(ymin = Excess_std_CI_low, ymax = Excess_std_CI_high, fill = Age_gr_fig_2b), alpha = 0.4, color = NA) +
  facet_wrap(~Age_gr_fig_2b, ncol = 1, labeller = labeller(Age_gr_fig_2b = Age_gr_fig_2b.labs), scales = "free_y") +
  theme_minimal() +
  xlab("") + ylab("Burial registrations - pre-pandemic model predictions") +
  viridis::scale_color_viridis("Age group", discrete = T, option = "H",
                               labels = c(paste0(c(0,5,15,25,40,50,60),"-",c(5,15,25,40,50,60,70)),"70+")) +
  viridis::scale_fill_viridis("Age group", discrete = T, option = "H",
                              labels = c(paste0(c(0,5,15,25,40,50,60),"-",c(5,15,25,40,50,60,70)),"70+")) +
  scale_y_continuous(limits = c(-30,100)) +
  ggh4x::facetted_pos_scales(y = list(Age_gr_fig_2b %in% as.character(1:8) ~ scale_y_continuous(limits = c(-30,100)))) +
  scale_x_date(position = "bottom", breaks = seq(as.Date("2020-01-01"), as.Date("2021-07-01"), by="6 months"), date_labels = "%m-%Y", expand = c(0,0)) +
  geom_hline(yintercept = 0, linetype = 2) +
  ggtitle("C")

# Pop_Str <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Str_2020_imp_ests.rds")
#
# ExcessMort_df_plot_3a <- ExcessMort_df_plot_3a %>% merge(data.frame(Age_gr_fig_2b = c("Total", 1:8),
#                                                                     Pop_str = c(Pop_Str[1],sum(Pop_Str[2:3]),sum(Pop_Str[4:5]),sum(Pop_Str[6:8]),sum(Pop_Str[9:10]),sum(Pop_Str[11:12]),sum(Pop_Str[13:14]),sum(Pop_Str[14:15]),sum(Pop_Str[16:17])))) %>%
#   mutate(Per_capita_unscaled_median = 1000*Excess_Median/Pop_str,
#          Per_capita_unscaled_CI_low = 1000*Excess_CI_low/Pop_str,
#          Per_capita_unscaled_high = 1000*Excess_CI_high/Pop_str)

p4b <- ggplot(ExcessMort_df %>% filter(Week_gr >104, Age_gr_fig_2b != "Total"), aes(x = Week_st)) +
  # geom_line(aes(y = Tot_Median, color = Age_gr_fig_2b), linetype = 1) +
  # geom_ribbon(aes(ymin = Tot_CI_low, ymax = Tot_std_CI_high, fill = Age_gr_fig_2b), alpha = 0.4, color = NA) +
  # geom_line(aes(y = Tot_std_Median, color = Age_gr_fig_2b), linetype = 1) +
  # geom_ribbon(aes(ymin = Tot_std_CI_low, ymax = Tot_std_CI_high, fill = Age_gr_fig_2b), alpha = 0.4, color = NA) +
  geom_line(aes(y = pc_Excess_Median, color = Age_gr_fig_2b), linetype = 1) +
  geom_ribbon(aes(ymin = pc_Excess_CI_low, ymax = pc_Excess_CI_high, fill = Age_gr_fig_2b), alpha = 0.4, color = NA) +
  # geom_line(aes(y = Excess_std_Median, col = Age_gr_fig_2b)) +
  # geom_ribbon(aes(ymin = Excess_std_CI_low, ymax = Excess_std_CI_high, fill = Age_gr_fig_2b), alpha = 0.4, color = NA) +
  facet_wrap(~Age_gr_fig_2b, ncol = 1, labeller = labeller(Age_gr_fig_2b = Age_gr_fig_2b.labs)) +
  theme_minimal() +
  xlab("") + ylab("Burial registrations - pre-pandemic model predictions, per capita (/1000)") +
  viridis::scale_color_viridis("Age group", discrete = T, option = "H",
                               labels = c(paste0(c(0,5,15,25,40,50,60),"-",c(5,15,25,40,50,60,70)),"70+")) +
  viridis::scale_fill_viridis("Age group", discrete = T, option = "H",
                              labels = c(paste0(c(0,5,15,25,40,50,60),"-",c(5,15,25,40,50,60,70)),"70+")) +
  # scale_y_continuous(limits = c(-30,100)) +
  # ggh4x::facetted_pos_scales(y = list(Age_gr_fig_2b %in% as.character(1:8) ~ scale_y_continuous(limits = c(-30,100)))) +
  scale_x_date(position = "bottom", breaks = seq(as.Date("2020-01-01"), as.Date("2021-07-01"), by="6 months"), date_labels = "%m-%Y", expand = c(0,0)) +
  # geom_hline(yintercept = 0, linetype = 2) +
  ggtitle("D") +
  # scale_y_continuous(trans = "log10")
  geom_hline(aes(yintercept = 0))
  # geom_hline(data = Median_Baseline %>% merge(data.frame(Age_gr_fig_2b = 1:8, Pop_str = unlist(lapply(list(1,2:3,4:5,6:8,9:10,11:12,13:14,15:17), function(x){sum(Pop_Str[x])})))) %>%
               # mutate(Baseline_median_pc = 1000*Median_Baseline/Pop_str),
             # aes(yintercept = Median_Baseline))
  # coord_cartesian(ylim = c(0,100))

# cowplot::plot_grid(p4b_test1,p4b_test2)



pB1 <- ggplot(Burs_Excess_total %>% merge(Dates_df), aes(x = Week_st, y = Total_deaths)) +
  geom_line() +
  theme_minimal() +
  xlab("") + ylab("") +
  ggtitle("Total") +
  # coord_cartesian(ylim = c(-60,100)) +
  geom_hline(yintercept = Median_Baseline_total$Median_Baseline, linetype = 2) +
  scale_x_date(expand = c(0,0)) +
  theme(title = element_text(size = 8),
        plot.title = element_text(hjust = 0.5))
        # axis.text.x=element_blank())



coeff <- 0.005
pB2 <- ggplot(Mort_excess_deaths_total_meds %>% merge(Dates_df) %>% filter(Week_gr >104), aes(x = Week_st, color = Type)) +
  geom_line(aes(y = Median, color = Type), linetype = 1) +
  geom_ribbon(data = merge(Mort_excess_deaths_total_CIs_low,Mort_excess_deaths_total_CIs_high), aes(ymin = CI_low, ymax = CI_high, fill = Type), alpha = 0.4, col = NA) +
  # geom_line(aes(y = Excess_std_Median, color = "Scaled")) +
  # geom_ribbon(aes(ymin = Excess_std_CI_low, ymax = Excess_std_CI_high), alpha = 0.4, fill = "darkblue") +
  # geom_line(data = WeeklyStandardise_sum, aes(y = (median-1)/coeff, color = "Scaling factor"), linetype = 3) +
  # geom_ribbon(data = WeeklyStandardise_sum, aes(y = (median-1)/coeff, ymin = (ci_low-1)/coeff, ymax = (ci_high-1)/coeff), fill = "red", alpha = 0.4, size = 1) +
  scale_y_continuous(name = "Burial registrations -\npre-pandemic model predictions",
                     sec.axis = sec_axis(~.*coeff+1, name="Standardisation", labels = c(0,1,2), breaks = c(0,1,2))) +
  theme_minimal() +
  xlab("") + #ylab("Deaths relative to Jan 2018-Dec 2019 <5 patterns") +
  coord_cartesian(ylim = c(-200,500)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_date(expand = c(0,0)) +
  ggtitle("E") +
  scale_color_manual(labels = c("Unscaled","Scaled","Scaling factor"), values = c("black","darkblue","red")) +
  scale_fill_manual(labels = c("Unscaled","Scaled","Scaling factor"), values = c("black","darkblue","red"))
                     # guide = guide_legend(override.aes = list(#breaks = c("Scaled","Unscaled","Scaling factor"),
                                                              # color = c("darkblue","black","red"),
                                                              # fill = c("darkblue","black","red"), alpha = 0.4)))
# theme(title = element_text(size = 8),
          # plot.title = element_text(hjust = 0.5))

WeeklyStandardise_sum_plot <- WeeklyStandardise_sum %>% mutate(median = (median-1)/coeff,
                                                               ci_low = (ci_low-1)/coeff,
                                                               ci_high = (ci_high-1)/coeff)

Mort_excess_deaths_total_meds <- Mort_excess_deaths_total %>% select(Week_gr, Excess_Median, Excess_std_Median) %>% rename(Excess = "Excess_Median", Excess_std = "Excess_std_Median") %>%
  merge(WeeklyStandardise_sum_plot %>% rename(Std = "median") %>% select(Week_gr, Std)) %>% pivot_longer(cols = c("Excess","Excess_std","Std"),names_to = "Type", values_to = "Median") %>%
  merge(Dates_df)
Mort_excess_deaths_total_CIs_low <- Mort_excess_deaths_total %>% select(Week_gr, Excess_CI_low, Excess_std_CI_low) %>% rename(Excess = "Excess_CI_low", Excess_std = "Excess_std_CI_low") %>%
  merge(WeeklyStandardise_sum_plot %>% rename(Std = "ci_low") %>% select(Week_gr, Std)) %>% pivot_longer(cols = c("Excess","Excess_std","Std"),names_to = "Type", values_to = "CI_low") %>%
  merge(Dates_df)
Mort_excess_deaths_total_CIs_high <- Mort_excess_deaths_total %>% select(Week_gr, Excess_CI_high, Excess_std_CI_high) %>% rename(Excess = "Excess_CI_high", Excess_std = "Excess_std_CI_high") %>%
  merge(WeeklyStandardise_sum_plot %>% rename(Std = "ci_high") %>% select(Week_gr, Std)) %>% pivot_longer(cols = c("Excess","Excess_std","Std"),names_to = "Type", values_to = "CI_high") %>%
  merge(Dates_df)

merge(Mort_excess_deaths_total_CIs_low,Mort_excess_deaths_total_CIs_high)

WeeklyStandardise_sum
#stringr::str_wrap("Scaling process", width = 50)



Mort_excess_deaths_cum_total <- lapply(sample(1:nrow(AG1_mcmc),1000), function(x){
    tmp_df <- lapply(1:17, function(y){
      if(y == 1){tmp_df <- AG1_mcmc[x,105:180]} else {tmp_df <- AG1_mcmc[x,105:180] * mcmc_samples[,paste0("RR",y)]}
      return(unlist(tmp_df))
    })
    tmp_sum <- tmp_df %>% str2str::lv2d(along = 2) %>% rowSums()

    Excess <- merge(Burs_2018_2021_Total[105:180,], Dates_df) %>% pull(Total_deaths) - tmp_sum
    Excess_std <- Excess/(WeeklyStandardise %>% filter(list_names == x, Week_gr > 104) %>% pull(Standard))
    Excess_std_cum <- cumsum(x = unlist(Excess_std))
    return(Excess_std_cum)
  }) %>% str2str::lv2d(along = 2) %>% as_tibble() %>% rowwise() %>% summarise(median = median(c_across()),
                                                                                              CI_low = bayestestR::ci(c_across())$CI_low,
                                                                                              CI_high = bayestestR::ci(c_across())$CI_high) %>%
  mutate(Week_gr = 105:180)

pB3 <- ggplot(Mort_excess_deaths_cum_total %>% merge(Dates_df), aes(x = Week_st)) +
  geom_line(aes(y = median)) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.4) +
  ggtitle("F") +
  # ggtitle("Total cumulative burial registrations") +
  scale_y_continuous(name = stringr::str_wrap("Cumulative age-distribution driven excess mortality estimate", width = 40)) +
  theme_minimal() +
  xlab("") + #ylab("Deaths relative to Jan 2018-Dec 2019 <5 patterns") +
  # coord_cartesian(ylim = c(-100,500)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_date(expand = c(0,0))
  # theme(title = element_text(size = 8))
        #plot.title = element_text(hjust = 0.5))


pdf("analysis/figures/48_Figure_2_Excess_Mortality_version2.pdf", width = 12, height = 12)
cowplot::plot_grid(cowplot::plot_grid(p1 + theme(legend.position = "none",
                                                 plot.title = element_text(vjust = -3, hjust = -0.09)),
                                      p2 + theme(legend.position = "none",
                                                 plot.title = element_text(vjust = -3, hjust = -0.09)),
                                      p3b + theme(legend.position = "none",
                                                  plot.title = element_text(vjust = -3, hjust = -0.09)),
                                      p4b + theme(legend.position = "none",
                                                  plot.title = element_text(vjust = -3, hjust = -0.09)), nrow = 1),
                   cowplot::plot_grid(pB2,pB3, nrow = 1), nrow = 2, rel_heights = c(7,2))
dev.off()

tiff("analysis/figures/48_Figure_2_Excess_Mortality_version2.tiff", width = 12, height = 12, units = "in", res = 150)
cowplot::plot_grid(cowplot::plot_grid(p1 + theme(legend.position = "none",
                                                 plot.title = element_text(vjust = -3, hjust = -0.09)),
                                      p2 + theme(legend.position = "none",
                                                 plot.title = element_text(vjust = -3, hjust = -0.09)),
                                      p3b + theme(legend.position = "none",
                                                  plot.title = element_text(vjust = -3, hjust = -0.09)),
                                      p4b + theme(legend.position = "none",
                                                  plot.title = element_text(vjust = -3, hjust = -0.09)), nrow = 1),
                   cowplot::plot_grid(pB2,pB3, nrow = 1), nrow = 2, rel_heights = c(7,2))
dev.off()


g1 <- ggplotGrob(p1 + theme(legend.position = "none",
                            # axis.title.y = element_text(hjust = 0.9),
                            axis.text.x=element_blank()))
gB1 <- ggplotGrob(pB1)#

# g3 <- ggplotGrob(p3)#
# g4 <- ggplotGrob(p4)#

# set the same widths for both blots
library(grid)
g1$widths <- unit.pmax(g1$widths, gB1$widths)
gB1$widths <- unit.pmax(g1$widths, gB1$widths)

gleft <- rbind(g1,gB1, size="first") # stack the two plots

g2 <- ggplotGrob(p2 + theme(legend.position = "none"))
                            # axis.title.y = element_text(hjust = 0.7)))
gB2 <- ggplotGrob(pB2)#

g2$widths <- unit.pmax(g2$widths, gB2$widths)
gB2$widths <- unit.pmax(g2$widths, gB2$widths)

gcent <- rbind(g2,gB2, size="first") # stack the two plots

g3 <- ggplotGrob(p3b + theme(legend.position = "none"))
gB3 <- ggplotGrob(pB3)#

g3$widths <- unit.pmax(g3$widths, gB3$widths)
gB3$widths <- unit.pmax(g3$widths, gB3$widths)

gright <- rbind(g3,gB3, size="first") # stack the two plots

g <- cbind(gleft,gcent,gright)

grid.newpage()
grid.draw(g)
# grid.draw(gleft)
# grid.draw(gcent)




pdf("analysis/figures/48_Figure_2_Excess_Mortality_version2.pdf", width = 10, height = 12)
grid.newpage()
grid.draw(g)
dev.off()

tiff("analysis/figures/48_Figure_2_Excess_Mortality_version2.tiff", width = 10, height = 12, units = "in", res = 300)
grid.newpage()
grid.draw(g)
dev.off()






pdf(file = "analysis/figures/48_Figure_2_Excess_Mortality.pdf", width = 8, height = 10)
cowplot::plot_grid(p1 + theme(legend.position = "none"),
                   p2 + theme(legend.position = "none"),
                   p3 + theme(legend.position = "none"),
                   p4 + theme(legend.position = "none"), nrow = 1)
dev.off()

tiff(file = "analysis/figures/48_Figure_2_Excess_Mortality.tiff", height = 10,units = "in", res = 300, width = 8)
cowplot::plot_grid(p1 + theme(legend.position = "none"),
                   p2 + theme(legend.position = "none"),
                   p3 + theme(legend.position = "none"),
                   p4 + theme(legend.position = "none"), nrow = 1)
dev.off()


# cowplot::plot_grid(p1 + theme(legend.position = "none"),
#                    p2 + theme(legend.position = "none"),
#                    p3 + theme(legend.position = "none"),
#                    p4 + theme(legend.position = "none"), nrow = 1)
