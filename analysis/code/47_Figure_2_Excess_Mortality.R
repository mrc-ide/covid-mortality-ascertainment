library(drjacoby)
library(dplyr)
library(tidyr)

## Format inputs
# Data
UTH_Mortality_Total <- read.csv(file = "~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records_v2.csv")

## For my first fit: Don't structure by age. Just take


# UTH_Mortality_Total%>%   mutate(date = as.Date(dod, "%m/%d/%y")) %>%
# filter(date >= "2018-08-13", date <= "2018-08-19")

weekly_deaths_list <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2021-06-21") %>% #, age_years !=".") %>%
  mutate(#Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1),
    week_num = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = F)) %>%
  group_by(week_num) %>%
  # group_by(Age_gr, week) %>%
  summarise(total_deaths = n(),
            week_start = unique(week_start)) %>% ungroup() %>%
  mutate(month = as.numeric(format(week_start, "%m")))



#### FIT 1: Single baseline estiamte for 2018-2019
data_list <- list(weekly_deaths_list = weekly_deaths_list)
parms_list <- define_params(name = c("Baseline"),
                            min = c(0),
                            max = c(600))

# define log-likelihood function
r_loglike <- function(params, data, misc) {

  data_deaths <- data
  baseline_rate <- as.numeric(params)

  ret <- 0
  ret <- ret + sum(dpois(x = data_deaths$total_deaths[1:104], lambda = baseline_rate, log=TRUE))
  return(ret)
}

## define prior
r_logprior <- function(params, misc) {

  baseline_rate <- as.numeric(params)

  ret <- 0
  ret <- ret + sum(dgamma(x = baseline_rate, shape = 0.1, rate = 0.1, log = T))
  return(ret)
}


MCMC_fit_1 <- drjacoby::run_mcmc(data = weekly_deaths_list,
                               df_params = parms_list,
                               loglike = r_loglike,
                               logprior = r_logprior,
                               burnin = 5e2,
                               samples = 2.5e3,
                               pb_markdown = TRUE,
                               chains = 5)

MCMC_fit_1

plot_par(MCMC_fit_1, phase = "burnin")
plot_par(MCMC_fit_1, phase = "sampling")

MCMC_fit_1$output[MCMC_fit_1$output$phase =="sampling","Baseline"]

Predictions_1 <- lapply(1:sum(MCMC_fit_1$output$phase =="sampling"), function(x){
  Baseline <- MCMC_fit_1$output[MCMC_fit_1$output$phase =="sampling","Baseline"][x]
  weekly_deaths_list %>%
    mutate(Predictions = total_deaths - Baseline,
           Sample = x)
})

Predictions_summary_1 <- data.table::rbindlist(Predictions_1) %>%
  group_by(week_start) %>%
  summarise_at(c("Predictions"), list(median = median, ci = bayestestR::ci), na.rm = TRUE)

Predictions_summary_1 %>%
  ggplot(aes(x = week_start, y = median)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_ribbon(aes(ymin = ci$CI_low, ymax = ci$CI_high)) +
  geom_hline(aes(yintercept = 0))


####################################################
####################################################
## FIT 2: Monthly baseline estiamtes for 2018-2019
parms_list <- define_params(name = c(paste0("Months_",1:12)),
                            min = c(rep(0,12)),
                            max = c(rep(600,12)))

# define log-likelihood function
r_loglike <- function(params, data, misc) {

  data_deaths <- data
  month_rate <- as.numeric(params)

  ret <- 0
  ret <- ret + sum(dpois(x = data_deaths$total_deaths[1:104], lambda = month_rate[data$month[1:104]], log=TRUE))
  return(ret)
}


## define prior
r_logprior <- function(params, misc) {

  month_rate <- as.numeric(params)

  ret <- 0
  ret <- ret + sum(dgamma(x = month_rate, shape = 0.1, rate = 0.1, log = T))
  return(ret)
}

MCMC_fit_2 <- drjacoby::run_mcmc(data = weekly_deaths_list,
                           df_params = parms_list,
                           loglike = r_loglike,
                           logprior = r_logprior,
                           burnin = 5e2,
                           samples = 2.5e3,
                           pb_markdown = TRUE,
                           chains = 5)

plot_par(MCMC_fit_2, phase = "burnin")
plot_par(MCMC_fit_2, phase = "sampling")

Predictions_2 <- lapply(1:sum(MCMC_fit_2$output$phase =="sampling"), function(x){

  Baselines <- cbind(month = 1:12,
                     Baselines = unlist(MCMC_fit_2$output[MCMC_fit_2$output$phase =="sampling",paste0("Months_",1:12)][x,]))

  weekly_deaths_list %>%
    merge(Baselines, all = T) %>%
    mutate(Predictions = total_deaths - Baselines,
           Sample = x)
})

## Get medians/CI intervals
Predictions_summary_2 <- data.table::rbindlist(Predictions_2) %>%
  group_by(week_start) %>%
  summarise_at(c("Predictions"), list(median = median, ci = bayestestR::ci), na.rm = TRUE)


Predictions_summary_2 %>%
  ggplot(aes(x = week_start, y = median)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_ribbon(aes(ymin = ci$CI_low, ymax = ci$CI_high)) +
  geom_line(data = Predictions_summary_1, col = "red") +
  geom_ribbon(data = Predictions_summary_1, aes(ymin = ci$CI_low, ymax = ci$CI_high), col = "red") +
  geom_hline(aes(yintercept = 0))


####################################################
####################################################
## FIT 3: Single baseline estimate, age-structured for 2018-2019
age_weekly_deaths <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".", age_years != ".") %>%
  filter(date >= "2018-01-01",date < "2021-06-21") %>% #, age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1),
    week_num = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = F)) %>%
  group_by(Age_gr, week_num) %>%
  summarise(total_deaths = n(),
            week_start = unique(week_start)) %>% ungroup() %>%
  mutate(month = as.numeric(format(week_start, "%m")))

age_weekly_deaths_list <- age_weekly_deaths %>%
  filter(week_num <105) %>% #, age_years !=".") %>%
  # select(-Age_gr) %>%
  pivot_wider(names_from = c(Age_gr), values_from = total_deaths, values_fill = 0, names_prefix = "Age_") %>%
  as.data.frame()




#### FIT 1: Single baseline estiamte for 2018-2019
# data_list <- list(weekly_deaths_list = age_weekly_deaths_list)
parms_list <- define_params(name = c(paste0("Baseline_",1:17)),
                            min = c(rep(0,17)),
                            max = c(rep(600, 17)))


# define log-likelihood function
r_loglike <- function(params, data, misc) {
  # browser()

  ret <- 0
  for(i in 1:length(params)){
    ret <- ret + sum(dpois(x = data[,i], lambda = params[i], log=TRUE))
  }
  return(ret)
}

## define prior
r_logprior <- function(params, misc) {

  baseline_rates <- as.numeric(params)

  ret <- 0
  ret <- ret + sum(dgamma(x = baseline_rates, shape = 0.1, rate = 0.1, log = T))
  return(ret)
}

MCMC_fit_3 <- drjacoby::run_mcmc(data = age_weekly_deaths_list %>% select(-week_start,-month, -week_num),
                                 df_params = parms_list,
                                 loglike = r_loglike,
                                 logprior = r_logprior,
                                 burnin = 5e2,
                                 samples = 2.5e3,
                                 pb_markdown = TRUE,
                                 chains = 5)

plot_par(MCMC_fit_3, phase = "burnin")
plot_par(MCMC_fit_3, phase = "sampling")

Predictions_3 <- lapply(1:sum(MCMC_fit_3$output$phase =="sampling"), function(x){
  # browser()
  Baselines <- cbind(Age_gr = 1:17,
        Baselines = unlist(MCMC_fit_3$output[MCMC_fit_3$output$phase =="sampling",paste0("Baseline_",1:17)][x,]))


  age_weekly_deaths %>%
    merge(Baselines, all = T) %>%
    mutate(Predictions = total_deaths - Baselines,
           Sample = x)
})

### Let's do a per capita plot
Lus_Pop <- cbind(Age_gr = 1:17,
                 Pop_size = readRDS(file = "analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Str_2020_imp_ests.rds"))

## Get medians/CI intervals
Predictions_summary_3 <- data.table::rbindlist(Predictions_3) %>%
  group_by(Age_gr, week_start) %>%
  summarise_at(c("Predictions"), list(median = median, ci = bayestestR::ci), na.rm = TRUE) %>%
  merge(Lus_Pop) %>%
  mutate(median_pc = median/Pop_size)

Date_df <- data.frame(Week_gr = 1:77, week_start = Predictions_summary_1$week_start[105:181])

ggplot(Predictions_summary_3 %>% filter(Age_gr %in% c(1,17)), aes(x = week_start, y = median, group = Age_gr, color = Age_gr, fill = Age_gr)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  geom_line(data = Predictions_summary_3 %>% group_by(week_start) %>% summarise(week_start = unique(week_start),
                                                                            median = sum(median)), aes(x = week_start, y = median), inherit.aes = F) +
  # geom_ribbon(aes(ymin = ci$CI_low, ymax = ci$CI_high), alpha = 0.2) +
  geom_line(data = Predictions_summary_1, aes(x = week_start, y = median), col = "red", inherit.aes = F) +
  # geom_ribbon(data = Predictions_summary_1, aes(x = week_start, ymin = ci$CI_low, ymax = ci$CI_high), col = "red", inherit.aes = F, alpha = 0.2) +
  geom_line(data = Predictions_summary_2, aes(x = week_start, y = median), col = "blue", inherit.aes = F) +
  # geom_ribbon(data = Predictions_summary_2, aes(x = week_start, ymin = ci$CI_low, ymax = ci$CI_high), col = "blue", inherit.aes = F, alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_minimal()  +
  geom_line(data = Plot_df %>% filter(Age_gr %in% c(1,17)) %>% merge(Date_df), aes(x = as.Date(week_start), y = as.numeric(excess_Median), group = Age_gr), col = "green", inherit.aes = F)

Plot_df <- readRDS(file = "analysis/data/Code-generated-data/40_Excess_mortality_total_deaths_by_age_week_checked_extended_with_Feb.rds")

ggplot(Predictions_summary_3 %>% filter(Age_gr %in% c(1,17)), aes(x = week_start, y = median_pc, group = Age_gr, color = Age_gr, fill = Age_gr)) +
  # geom_bar(stat = "identity") +
  geom_line() +
  # geom_line(data = Predictions_summary_3 %>% group_by(week_start) %>% summarise(week_start = unique(week_start),
                                                                                # median = sum(median)), aes(x = week_start, y = median), inherit.aes = F) +
  # geom_ribbon(aes(ymin = ci$CI_low, ymax = ci$CI_high), alpha = 0.2) +
  geom_line(data = Predictions_summary_1, aes(x = week_start, y = median/sum(Lus_Pop[,2])), col = "red", inherit.aes = F) +
  # geom_ribbon(data = Predictions_summary_1, aes(x = week_start, ymin = ci$CI_low, ymax = ci$CI_high), col = "red", inherit.aes = F, alpha = 0.2) +
  geom_line(data = Predictions_summary_2, aes(x = week_start, y = median/sum(Lus_Pop[,2])), col = "blue", inherit.aes = F) +
  # geom_ribbon(data = Predictions_summary_2, aes(x = week_start, ymin = ci$CI_low, ymax = ci$CI_high), col = "blue", inherit.aes = F, alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_minimal()

