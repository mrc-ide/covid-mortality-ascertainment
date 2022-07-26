## Excess deaths by age and time:
rm(list = ls())
library(dplyr)
library(ggplot2)
# library(tidyverse)

# 1. I need the baseline estimates
# mcmc <- readRDS("../Bonus Files/2022-04-05_mcmc_baseline.rds")
mcmc <- readRDS("../Bonus Files/2022-05-24_Baseline_Mortality_mcmc_Gamma_Prior_inc_Feb_2021.rds")
# mcmc <- readRDS("../Bonus Files/2022-04-22_mcmc_LOOCV_1.rds")
mcmc_samples <-mcmc$output %>% filter(phase =="sampling")

plot_par(mcmc, show = "RR2", phase = "sampling")
plot_par(mcmc, show = "U_5_Rate_Week14", phase = "sampling")

## AG1 deaths in mortuary: Mort_deaths_mcmc: 2020_AG1
# This gets included in ncdeaths below.
# AG1_2020_mcmc <- mcmc_samples[, c(paste0("U_5_Rate_Week",129:144))]
AG1_2020_mcmc <- mcmc_samples[, c(paste0("U_5_Rate_Week_",c(105:181)))]

## AG1 Pre 2020 background death rate: Bg_dr_mcmc
# Mort_deaths <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age_weeks.rds")
Mort_deaths <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age_weeks_2020_plus.rds")
Ag1std <- Mort_deaths %>% filter(Age_gr ==1) %>% select(-Age_gr) %>%
  rename("Ag1std" = deaths)


# median(AG1_2020_mcmc[,4]) * colMeans(mcmc_samples[,paste0("RR",2:17)])
# median(AG1_2020_mcmc[,5]) * colMeans(mcmc_samples[,paste0("RR",2:17)])
# median(AG1_2020_mcmc[,6]) * colMeans(mcmc_samples[,paste0("RR",2:17)])

# cbind(Mort_deaths %>% filter(Week_gr ==6, Age_gr !=1),
#       mean(AG1_2020_mcmc[,6]) * colMeans(mcmc_samples[,paste0("RR",2:17)]),
#       Mort_deaths %>% filter(Week_gr ==5, Age_gr !=1),
#       mean(AG1_2020_mcmc[,5]) * colMeans(mcmc_samples[,paste0("RR",2:17)]))

## Non-covid deaths in the mortuary: 2020_AG1 * RR
# Mort_ncd_mcmc
Mort_excess_deaths <- lapply(1:nrow(AG1_2020_mcmc), function(x){
  mcmc_samples <- cbind(as.numeric(AG1_2020_mcmc[x,]),
                        as.numeric(AG1_2020_mcmc[x,]) %*% t(as.numeric(mcmc_samples[x,paste0("RR",2:17)])))
  # rownames(mcmc_samples) <- 1:16
  rownames(mcmc_samples) <- c(105:181)-104
  colnames(mcmc_samples) <- 1:17
  # browser()
  mcmc_samples <- mcmc_samples %>% reshape2::melt(value.name = "Mort_ncd_mcmc", varnames = c("Week_gr", "Age_gr"))
    # dplyr::pull(Mort_ncd_mcmc)
  # browser()
  excess_deaths <- merge(mcmc_samples,Mort_deaths, all = T) %>%
    mutate(excess = deaths - Mort_ncd_mcmc)
  return(excess_deaths)
}) %>% str2str::ld2d() %>%
  select(-row_names)

# Mort_excess_deaths %>% filter(Week_gr ==4, Age_gr ==16)

## Divide by pop size for Lusaka
## Multiply by 1000 to get per-capita deaths per 1000
Pop_str <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Str_2020_imp_ests.rds")
Pop_str_df <- data.frame(Age_gr = 1:17, Pop_str = Pop_str)

## mean(under5_2018_to_2019)/under_5_rate/0.8

# Mort_ncd_mcmc need to be scaled by under 5 rate.
# So we have under 5 rate from weeks
AG1_pre2020_mcmc <- mcmc_samples[, c(paste0("U_5_Rate_Week_",1:104))]

U5_Baseline_2018_2019 <- rowMeans(AG1_pre2020_mcmc)
WeeklyStandardise <- apply(AG1_2020_mcmc, 2, function(x){x/U5_Baseline_2018_2019})
# colnames(WeeklyStandardise) <- 1:16
colnames(WeeklyStandardise) <- c(105:181)-104
WeeklyStandardise <- WeeklyStandardise %>%
  reshape2::melt(value.name = "Standard", varnames = c("list_names","Week_gr"))


##################################################################
# median(WeeklyStandardise %>% filter(Week_gr ==4) %>% pull(Standard))
# median(WeeklyStandardise %>% filter(Week_gr ==5) %>% pull(Standard))

WeeklyStandardise %>% group_by(Week_gr) %>%
  summarise(Median = median(Standard),
            CI_low = bayestestR::ci(Standard)$CI_low,
            CI_high = bayestestR::ci(Standard)$CI_high) %>%
  ggplot(aes(x = Week_gr, y = Median)) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  xlab("Week of study") + ylab("Standardisation coefficient")

WeeklyStandardise %>%group_by(Week_gr) %>%
  summarise(min(Standard),
            median(Standard),
            max(Standard))
##################################################################

hist(U5_Baseline_2018_2019)
hist(WeeklyStandardise %>% select(-list_names) %>% filter(Week_gr ==5))



##################################################################
##################################################################
# Mort_excess_deaths %>% filter(Week_gr %in% c(4,5), Age_gr %in% c(1,16)) %>% group_by(Age_gr, Week_gr) %>%
#   summarise(deaths = mean(deaths),
#             Mort_ncd_mcmc = mean(Mort_ncd_mcmc),
#             excess = median(excess))
#
# Mort_excess_deaths %>% filter(Week_gr == 4, Age_gr == 16)
#
# Mort_excess_deaths %>% filter(Week_gr ==5) %>% group_by(Age_gr) %>%
#   summarise(deaths = mean(deaths),
#             Mort_ncd_mcmc = mean(Mort_ncd_mcmc),
#             excess = mean(excess))
##################################################################
##################################################################

# WeeklyStandardise %>% select(-list_names) %>% group_by(Week_gr) %>% summarise(mean = mean(Standard))
# WeeklyStandardise %>% select(-list_names) %>% filter(Week_gr ==2) %>% pull(Standard) %>% mean()
# WeeklyStandardise %>% select(-list_names) %>% filter(Week_gr ==4) %>% pull(Standard) %>% mean()
# WeeklyStandardise %>% select(-list_names) %>% filter(Week_gr ==5) %>% pull(Standard) %>% mean()
#
# Mort_excess_deaths %>% filter(Age_gr == 1)
# Mort_excess_deaths %>% filter(Age_gr == 1, Week_gr == 4)
# Mort_excess_deaths %>% filter(Age_gr == 1, Week_gr == 4)
# Mort_excess_deaths %>% filter(Age_gr == 1, Week_gr == 5)
# Mort_excess_deaths %>% filter(Age_gr == 16, Week_gr == 5)
# Mort_excess_deaths %>% filter(Age_gr == 1, Week_gr == 6)

Plot_df_Total <- Mort_excess_deaths %>% #filter(Week_gr ==5, Age_gr == 11) %>%
  # complete()
  merge(WeeklyStandardise) %>% mutate(excess_sc = excess/Standard) %>%#
                                      #excess_sc_Lus = excess_sc/0.8)%>%
  group_by(Week_gr, Age_gr) %>%
  # across()
  summarise(across(starts_with("excess"), list(Median = ~median(.x),
                                               CI_low = ~ bayestestR::ci(.x)$CI_low,
                                               CI_high = ~ bayestestR::ci(.x)$CI_high)))

# saveRDS(Plot_df_Total, file = "analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week.rds")
# saveRDS(Plot_df_Total, file = "analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week_Checked.rds")
# saveRDS(Plot_df_Total, file = "analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week_checked_extended_with_Feb.rds")
Plot_df_Total <- readRDS(file = "analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week_checked_extended_with_Feb.rds")
# merge(Pop_str_df) %>%
  # mutate(across(starts_with("excess"), ~.x*1000/Pop_str))

Excess_Deaths <- Plot_df_Total %>% merge(Dates_for_groups) %>%
  ungroup() %>% group_by(date_st) %>% summarise(
    Excess_Mort = sum(excess_Median),
    Excess_Mort_Scaled = sum(excess_sc_Median)) %>%
  reshape2::melt(id = "date_st") %>%
  ggplot(aes(x = date_st, y = value, group = variable, fill = variable)) +
  geom_bar(width = 4, stat = "identity", position = "dodge") +
  scale_fill_manual(name = NULL,
                    values = c("black","firebrick3"),
                    labels=c("Initial estimates", "Standardised")) +
  theme_minimal() +
  xlab("Date") +ylab("Excess Deaths")

tiff(filename = "analysis/figures/00_40_Total_Excess_Deaths_Weekly.tiff", res = 300, width = 8, height = 5, units = "in")
Excess_Deaths
dev.off()

Plot_df_Total %>% merge(Dates_for_groups) %>%
  filter(date_st >="2020-06-15",
         date_st <"2020-10-01") %>% ungroup() %>%
  summarise(sum(excess_Median),
            sum(excess_sc_Median))

Plot_df_Total %>% merge(Dates_for_groups) %>%
  filter(date_st >="2020-12-15",
         date_st <"2021-03-01") %>% ungroup() %>%
  summarise(sum(excess_Median),
            sum(excess_sc_Median))


Plot_df <- Mort_excess_deaths %>%
  # ungroup()
  # complete() %>%#filter(Week_gr ==5, Age_gr == 11) %>%
  merge(WeeklyStandardise) %>% mutate(excess_sc = excess/Standard)%>%
  group_by(Week_gr, Age_gr) %>%
  # across()
  summarise(across(starts_with("excess"), list(Median = ~median(.x),
                                               CI_low = ~ bayestestR::ci(.x)$CI_low,
                                               CI_high = ~ bayestestR::ci(.x)$CI_high))) %>%
  merge(Pop_str_df) %>%
  mutate(across(starts_with("excess"), ~.x*1000/Pop_str))
         # CI_low_pc = 1000*CI_low/Pop_str,
         # CI_high_pc = 1000*CI_high/Pop_str)

saveRDS(Plot_df, file = "analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week_checked_extended_per1000_with_Feb.rds")

# dose.labs <- c("D0.5", "D1", "D2")
# names(dose.labs) <- c("0.5", "1", "2")


# plot(Mort_excess_deaths %>% #filter(Week_gr ==5, Age_gr == 11) %>%
  # merge(WeeklyStandardise) %>% mutate(excess_sc = excess/Standard,
                                      # excess_sc_Lus = excess_sc/0.8) %>%
  # group_by(list_names, Age_gr) %>% summarise(sum(excess_sc_Lus)/sum(Pop_str)*1000))

# Plot_df %>% filter(Age_gr ==17) %>% summarise(sum(excess_sc_Lus_Median))
plot(Plot_df %>% filter(Age_gr ==17) %>% pull(excess_sc_Lus_Median))

3*2419900/290810000*100

Age_groups.labs <- c(paste0("Age: ", c("0-4","5-9","10-14","15-29","20-24","25-29",
                                       "30-34","35-39","40-44","45-49","50-54","55-59",
                                       "60-64","65-69","70-74","75-79","80+")))
names(Age_groups.labs) <- 1:17

library(ggpubr)
# ggplot(data = Plot_df %>% filter(Age_gr !=1), aes(x = Week_gr))  + geom_line(aes(y = Median)) +
#   facet_wrap(~Age_gr, labeller = labeller(Age_gr = Age_groups.labs)) +
#   geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.5, fill = "darkred") +
#   xlab("Week of study") + ylab("Excess mortality per capita") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_pubr()
# Dates_for_groups <- readRDS(file = "analysis/data/Code-generated-data/00_07_Mortuary_data.rds") %>%
#   filter(date < as.Date("2020-10-05")) %>%
#   mutate(date = lubridate::floor_date(date, "week", 1)) %>% group_by(date) %>% summarise(date = date[1]) %>%
#   mutate(Week_gr = 1:16)

Dates_for_groups <- readRDS(file = "analysis/data/Code-generated-data/00_07_Mortuary_data_age_weeks_2020_plus.rds") %>%
  select(Week_gr, date_st) %>%
  unique()


Plot_df <- readRDS(file = "analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week_checked_extended_per1000_with_Feb.rds")
Check_the_plot_data <- Plot_df %>% filter(Age_gr !=1) %>%
  merge(Dates_for_groups)
# Dates_to_add <- data.frame(Week_gr = c(58:61), date_st = seq.Date(from = as.Date("2021-02-02"), by = "week", length.out = 4))
# Check_the_plot_data <- Check_the_plot_data #%>%
  # rbind(data.frame(expand.grid(Week_gr = c(58:61),Age_gr = 2:17) %>%# merge(Dates_to_add),
                   # excess_Median = NA, excess_CI_low = NA, excess_CI_high = NA, excess_sc_Median = NA,
                   # excess_sc_CI_low = NA, excess_sc_CI_high = NA, excess_sc_Lus_Median = NA,
                   # excess_sc_Lus_CI_low = NA, excess_sc_Lus_CI_high = NA, Pop_str = NA))



p1 <- ggplot(data = Check_the_plot_data, aes(x = date_st))  +
  # ylim(c(-1,8)) +
  # annotate(geom = "rect", xmin = as.Date("2020-06-15"), xmax = as.Date("2020-09-05"), ymin = 0, ymax = Inf,
           # fill = "blue", alpha = 0.2) +
  # annotate(geom = "rect", xmin = as.Date("2020-12-01"), xmax = as.Date("2021-02-15"), ymin = 0, ymax = Inf,
           # fill = "blue", alpha = 0.2) +
  # annotate(geom = "rect", xmin = as.Date("2021-05-01"), xmax = as.Date("2021-07-01"), ymin = 0, ymax = Inf,
           # fill = "blue", alpha = 0.2) +
  coord_cartesian(ylim = c(-2, 8)) +
  facet_wrap(~Age_gr, labeller = labeller(Age_gr = Age_groups.labs)) +
  geom_ribbon(aes(ymin = excess_CI_low, ymax = excess_CI_high), linetype = 2, col = "black", alpha = 0.4) +
  geom_ribbon(aes(ymin = excess_sc_CI_low, ymax = excess_sc_CI_high), linetype = 2, col = "firebrick3", alpha = 0.2, fill = "firebrick3") +
  # geom_ribbon(aes(ymin = excess_sc_Lus_CI_low, ymax = excess_sc_Lus_CI_high), linetype = 2, col = "orange", fill = "yellow", alpha = 0.2) +
  xlab("Date") + ylab("Excess mortality per capita (/1000)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # geom_line(aes(y = excess_sc_Lus_Median, color = "Excess deaths (Lusaka)")) +
  geom_line(aes(y = excess_sc_Median, color = "Excess deaths (standardised)")) +
  geom_line(aes(y = excess_Median, color = "Excess deaths")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(name = NULL,
                       values = c("Excess deaths" = "black",
                                  "Excess deaths (standardised)" = "firebrick3"
                                  # "Excess deaths (Lusaka)" = "orange"
                                  )) +
  theme(axis.text.x=element_text(angle = 40, hjust = 1))

pdf(file = "analysis/figures/40_02_2020_study_period_excess_mortality_extended.pdf", height = 5, width = 8)
p1
dev.off()

tiff(file = "analysis/figures/40_02_2020_study_period_excess_mortality_extended.tiff", height = 5, width = 8, res=300, units = "in")
p1
dev.off()

pdf(file = "analysis/figures/40_02_2020_study_period_excess_mortality_study_period.pdf", height = 8, width = 8)
p1 + xlim(as.Date(c("2020-06-01", "2020-10-15")))
dev.off()

tiff(file = "analysis/figures/40_02_2020_study_period_excess_mortality_study_period.tiff", height = 8, width = 8, res=300, units = "in")
p1 + xlim(as.Date(c("2020-06-01", "2020-10-15")))
dev.off()



# sum(Plot_df$excess_Median)
# sum(Plot_df$excess_sc_Median)
# sum(Plot_df$excess_sc_Lus_Median)
# sum(Mort_deaths$deaths)

# sum(Plot_df$excess_Median)
# sum(Plot_df$excess_sc_Median)
# sum(Plot_df$excess_sc_Lus_Median)

Plot_df %>% group_by(Age_gr) %>% summarise(excess_age_per_1000 = sum(excess_sc_Lus_Median))

## What happened to week 5?
# So week 5 has about 45 deaths in AG1, which gives is about 0.8 in terms of relative to the baseline.
# That means that when we take the excess deaths of around 0, it's still around 0.
# So, follow up question is: Is the excess mortality value correct?
## That estimate comes from those values
## Do I need to multiply the baseline estimates by the weekly... yes I do.


## Three lines:
# 1: Adjust upwards accounting for drop off in overall mort attendance.
# Upweight by Lusaka


### 2018-2019 mortality
### All cause mortality
UTH_Mortality_Total <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv") %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(date >= "2018-01-01" & date < "2020-01-06", age_years !=".", dod != ".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F),
         Week_gr = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  tidyr::complete(Age_gr, Week_gr, fill = list(Mort_deaths = 0)) %>%
  group_by(Week_gr,Age_gr) %>%
  summarise(Mort_deaths = length(date)) %>%
  ungroup %>%
  tidyr::complete(Week_gr, Age_gr, fill = list(Mort_deaths = 0))
# filter(date >= "2020-06-15" & date <= "2020-11-01")


pre2020_mcmc
# rpois(n = 1, )

pre2020_mcmc <- lapply(1:nrow(mcmc_samples), function(x){
  browser()

  # rpois(n = 1, lambda = mcmc_samples[1,] %>% select(-chain,-phase,-iteration,-logprior,-loglikelihood))
  AG1_pre2020_mcmc[x,] <- rpois(n = length(AG1_pre2020_mcmc[x,]), lambda = as.numeric(AG1_pre2020_mcmc[x,]))



  Ests_tmp <- cbind(as.numeric(AG1_pre2020_mcmc[x,]),as.numeric(AG1_pre2020_mcmc[x,]) %*% t(as.numeric(mcmc_samples[x,paste0("RR",2:17)])))
  colnames(Ests_tmp) <- 1:17
  rownames(Ests_tmp) <- 1:105
  Ests_tmp <- Ests_tmp %>%
    reshape2::melt(value.name = "All_cause_deaths", varnames = c("Week_gr", "Age_gr"))
}) %>% str2str::ld2d() %>%
  group_by(Week_gr, Age_gr) %>%
  summarise(Median = median(All_cause_deaths),
            CI = bayestestR::ci(All_cause_deaths)$CI,
            CI_low = bayestestR::ci(All_cause_deaths)$CI_low,
            CI_high = bayestestR::ci(All_cause_deaths)$CI_high)

p2 <- ggplot(data = UTH_Mortality_Total, aes(x = as.numeric(Week_gr), y = Mort_deaths)) +
  geom_point(aes(alpha = "Mortuary deaths")) +
  geom_line(data = pre2020_mcmc, aes(x = as.numeric(Week_gr), y = Median, alpha = "MCMC ests. median"), col = "black", linetype = 2, inherit.aes = F) +
  geom_ribbon(data = pre2020_mcmc, aes(x = as.numeric(Week_gr), ymin = CI_low, ymax = CI_high, alpha = "MCMC ests. median +/- 95% CI"), fill ="darkred", inherit.aes = F) +
  facet_wrap(~Age_gr, ncol = 3, scales = "free_y", labeller = labeller(Age_gr = Age_groups.labs)) +
  xlab("Weeks (2018-2019)") + ylab("All cause deaths") +
  # scale_alpha_manual(name=NULL,
                     # values = c("Mortuary deaths" = 1,
                                # "MCMC ests. median +/- 95% CI" = 1
                                # ),
                     # guide = guide_legend(override.aes = list(linetype =c("blank","dashed"),
                                                              # shape = c(16, NA),
                                                              # fill = c(NA, "darkred"),
                                                              # alpha = c(1,0.5)))) #+
  # scale_linetype_manual(name=NULL,
                     # breaks = c("MCMC ests. median +/- 95% CI"),
                     # values = c("MCMC ests. median +/- 95% CI" = 1))+
  scale_alpha_manual(name = NULL,
                    # breaks = c(),
                    values = c("Mortuary deaths" = 1, "MCMC ests. median +/- 95% CI" = 0.5),
                    guide = guide_legend(override.aes = list(
                      linetype =c("blank","solid"),
                      color=c("black","black"),
                      shape = c(16, NA),
                      fill = c(NA, "darkred"),
                      alpha = c(1,0.5)))) +
  # )
  # scale_alpha_manual(name = NULL,
  #                   # breaks = c("MCMC ests. median +/- 95% CI"),
  #                   values = c("MCMC ests. median +/- 95% CI" = 0.5)) +
  theme_pubr(legend = c(5/6,0.07)) +
  theme(legend.text = element_text(size=12),
        )
  # theme(theme.key.size = 1.5)
# theme(#plot.title = element_text(size = 10),
  #   legend.position = c(1,0), legend.justification = c(1,0),
  #   legend.key = element_rect(fill = "white")) +


  # scale_alpha_manual(#plot.title = element_text(size = 10),
# legend.position = c(1,0), legend.justification = c(1,0),
# legend.key = element_rect(fill = "white")) +




pdf(file = "analysis/figures/40_01_2018-2019_All_cause_mortality_mcmc_ests.pdf", width = 12, height = 15)
p2
dev.off()




### CIs and Median for credible interval.

### Change in expected rate of mortality
## (True deaths - non-covid)/non-covid
p3 <- ggplot(data = Mort_excess_deaths %>% mutate(Prop = excess/Mort_ncd_mcmc*100) %>%
         group_by(Age_gr, Week_gr) %>%
         summarise(Median = median(Prop),
                   CI_low = bayestestR::ci(Prop)$CI_low,
                   CI_high = bayestestR::ci(Prop)$CI_high),
       aes(x = Week_gr, y = Median)) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), fill = "darkred", alpha = 0.5) +
  facet_wrap(~Age_gr) +
  xlab("Week of study") +
  ylab("Excess mortality as percentage of non-COVID mortality") +
  theme_pubr()


pdf(file = "analysis/figures/40_03_2020_study_period_exc_mort_prop.pdf")
p3
dev.off()


pre2020_mcmc








Mort_excess_deaths

ggplot(data = Mort_excess_deaths) + geom_point(aes(y = excess)





Mort_ncd_mcmc

str2str::ld2d(Mort_ncd_mcmc)
tidyr::pivot_wider(str2str::ld2d(Mort_ncd_mcmc), names_from = list_names, values_from = Mort_ncd_mcmc)

## Then I need to take the actual mortuary values
# Mort_deaths <- readRDS(file = "analysis/data/Code-generated-data/00_07_Mortuary_data_age_weeks.rds")
