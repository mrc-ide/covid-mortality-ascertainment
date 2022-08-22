## Excess deaths by age and time using 5-15 data:
rm(list = ls())
library(dplyr)
library(ggplot2)
library(drjacoby)
# library(tidyverse)


mcmc <- readRDS("../Bonus Files/2022-08-11_Baseline_Mortality_mcmc_515.rds")
mcmc_samples <-mcmc$output %>% filter(phase =="sampling")
AG1_2020_mcmc <- mcmc_samples[, c(paste0("Week_rate_5to15_",c(105:180)))]
AG1_mcmc <- mcmc_samples[, c(paste0("Week_rate_5to15_",c(1:180)))]

Burial_df <- readRDS("analysis/data/Code-generated-data/00_07_Burial_registrations_2017_2021.rds") %>%
  # group_by(Age_gr_fig_2, Week_st) %>%
  group_by(Age_gr_515, Week_st) %>%
  filter(!is.na(Age_gr_515)) %>%
  summarise(Total_deaths = n()) %>%
  ungroup() %>% complete(Age_gr_515, Week_st, fill = list(Total_deaths = 0))

Burs_2018_2021 <- Burial_df %>%
  filter(Week_st >=as.Date("2018-01-01"),
         Week_st <as.Date("2021-06-12"))

Ag1std <- Burs_2018_2021 %>%
  rename("Ag1std" = Total_deaths) %>%
  filter(Age_gr_515 ==1) %>%
  mutate(Week_gr = 1:180)

Dates_df <- Ag1std %>% select(Week_st, Week_gr)

AG1_pre2020_mcmc <- mcmc_samples[, c(paste0("Week_rate_5to15_",1:104))]
U515_Baseline_2018_2019 <- rowMeans(AG1_pre2020_mcmc)
WeeklyStandardise <- apply(AG1_mcmc, 2, function(x){x/U515_Baseline_2018_2019})
# colnames(WeeklyStandardise) <- 1:16
colnames(WeeklyStandardise) <- 1:180
WeeklyStandardise <- WeeklyStandardise %>%
  reshape2::melt(value.name = "Standard", varnames = c("list_names","Week_gr"))


Mort_excess_deaths <- lapply(1:180, function(x){
  lapply(1:15, function(y){
    if(y == 1){tmp_df <- AG1_mcmc[,paste0("Week_rate_5to15_",x)]} else {tmp_df <- AG1_mcmc[,paste0("Week_rate_5to15_",x)] * mcmc_samples[,paste0("RR",y)]}
    Excess <- merge(Burs_2018_2021, Dates_df) %>% filter(Week_gr == x, Age_gr_515 == y) %>% pull(Total_deaths) - tmp_df
    Excess_std <- Excess/(WeeklyStandardise %>% filter(Week_gr == x) %>% pull(Standard))
    return(data.frame(Week_gr = x, Age_gr_515 = y,
                      Median = median(tmp_df), CI_low = bayestestR::ci(tmp_df)$CI_low, CI_high = bayestestR::ci(tmp_df)$CI_high,
                      Excess_Median = median(Excess), Excess_CI_low = bayestestR::ci(Excess)$CI_low, Excess_CI_high = bayestestR::ci(Excess)$CI_high,
                      Excess_std_Median = median(Excess_std), Excess_std_CI_low = bayestestR::ci(Excess_std)$CI_low, Excess_std_CI_high = bayestestR::ci(Excess_std)$CI_high))
  }) %>% str2str::ld2d() %>% select(-row_names)
}) %>% str2str::ld2d() %>% select(-row_names, -list_names)


##################################################################
ExcessMort_df <- Mort_excess_deaths %>% #filter(Week_gr ==5, Age_gr == 11) %>%
  # merge(WeeklyStandardise) %>% mutate(excess_sc = excess/Standard) %>%#
  # group_by(Week_gr, Age_gr) %>%
  # summarise(across(starts_with("excess"), list(Median = ~median(.x),
  # CI_low = ~ bayestestR::ci(.x)$CI_low,
  # CI_high = ~ bayestestR::ci(.x)$CI_high))) %>%
  mutate(Age_gr_515_plot = case_when(Age_gr_515 == 1 ~ 1,
                                Age_gr_515 %in% 2:3 ~ 2,
                                Age_gr_515 %in% 4:5 ~ 3,
                                Age_gr_515 %in% 6:8 ~ 4,
                                Age_gr_515 %in% 9:10 ~ 5,
                                Age_gr_515 %in% 11:12 ~ 6,
                                Age_gr_515 %in% 13:15 ~ 7)) %>%
  # filter(Week_gr != 181) %>%
  select(-Age_gr_515) %>%
  group_by(Age_gr_515_plot, Week_gr) %>%
  summarise_all(sum) #%>%
# mutate(Week_gr = Week_gr +104) %>%
# merge(data.frame(Week_gr = 1:180, Week_st = as.Date(c(Burs_2018_2021 %>% pull(Week_st) %>% unique() %>% sort())[105:180])),all =T)


# Dates_df <- data.frame(Week_gr = 1:180, Week_st = Burs_2018_2021 %>%select(Week_st))
ExcessMort_df <- ExcessMort_df %>% mutate(Age_gr_515_plot = as.factor(Age_gr_515_plot))

Age_gr_515.labs <- c(paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")
names(Age_gr_515.labs) <- 1:7

p1 <- ggplot(ExcessMort_df %>% merge(Dates_df), aes(x = Week_st, y = Excess_Median, color = Age_gr_515_plot, fill = Age_gr_515_plot)) +
  geom_line() +
  facet_wrap(~Age_gr_515_plot, ncol = 1, labeller = labeller(Age_gr_515_plot = Age_gr_515.labs)) +
  geom_ribbon(aes(ymin = Excess_CI_low, ymax = Excess_CI_high), alpha = 0.4, color = NA) +
  theme_minimal() +
  xlab("Date") + ylab("Total excess Mortality") +
  viridis::scale_color_viridis("Age group", discrete = T, option = "H",
                               # labels = c(0,seq(5,75,by = 10),Inf)
                               labels = c(paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")
  ) +
  viridis::scale_fill_viridis("Age group", discrete = T, option = "H",
                               # labels = c(0,seq(5,75,by = 10),Inf)
                               labels = c(paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")
  ) +
  coord_cartesian(ylim = c(-60,150))



p2 <- ggplot(ExcessMort_df %>% merge(Dates_df), aes(x = Week_st, y = Excess_std_Median, color = Age_gr_515_plot, fill = Age_gr_515_plot)) +
  geom_line() +
  facet_wrap(~Age_gr_515_plot, ncol = 1, labeller = labeller(Age_gr_515_plot = Age_gr_515.labs)) +
  geom_ribbon(aes(ymin = Excess_std_CI_low, ymax = Excess_std_CI_high, fill = Age_gr_515_plot), alpha = 0.4, color = NA) +
  theme_minimal() +
  xlab("Date") + ylab("Total excess Mortality") +
  viridis::scale_color_viridis("Age group", discrete = T, option = "H",
                               # labels = c(0,seq(5,75,by = 10),Inf)
                               labels = c(paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")) +
  viridis::scale_fill_viridis("Age group", discrete = T, option = "H",
                              # labels = c(0,seq(5,75,by = 10),Inf)
                              labels = c(paste0(c(5, 15,25, 40,50,60),"-",c(15,25,40,50,60,70)),"70+")) +
  coord_cartesian(ylim = c(-60,150))


cowplot::plot_grid(p1 + theme(legend.position = "none"), p2 + theme(legend.position = "none"), nrow = 1)
