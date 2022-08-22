## So first thing: I want to run an mcmc fitting to each week, total mortality:
library(drjacoby)
library(dplyr)
library(tidyr)
library(ggplot2)

## Format inputs
# Data
UTH_Mortality_Total <- read.csv(file = "~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records_v2.csv")

weekly_deaths_list <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2021-06-21") %>% #, age_years !=".") %>%
  mutate(#Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1),
    week_num = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = F)) %>%
  group_by(week_num) %>%
  summarise(total_deaths = n(),
            week_start = unique(week_start)) %>% ungroup() %>%
  mutate(month = as.numeric(format(week_start, "%m"))) %>%
  filter(week_num <105)



data_list <- list(weekly_deaths_list = weekly_deaths_list)

# parms_list <- define_params(name = c(paste0("Week_no_",1:104),paste0("Month_no_",1:12),"tau_week"),
#                             min = c(rep(0,104),rep(0,12),0),
#                             max = c(rep(10,104),rep(600,12),100))

parms_list <- define_params(name = c(paste0("Week_no_",1:104),"Baseline",paste0("Month_no_",2:12),"tau_week"),
                            min = c(rep(0,104),rep(0,12),0),
                            max = c(rep(10,104),rep(600,12),100))


# define log-likelihood function
r_loglike <- function(params, data, misc) {
  data_deaths <- data
  rates <- as.numeric(params)

  # rates 1:104 weekly
  # rates 105:116: months
  # rate 117: tau_week

  ret <- 0
  ret <- ret + sum(dpois(x = data_deaths$total_deaths[1:104], lambda = rates[105:116][data_deaths$month[1:104]] * rates[1:104], log=T))
  ret <- ret + sum(dlnorm(x = rates[1:104], meanlog = 1, sd = rates[117], log = T))
  return(ret)
}

## define prior
r_logprior <- function(params, misc) {
  rates <- as.numeric(params)

  ret <- 0
  ret <- ret + sum(dgamma(x = rates[105:116], shape = 0.01, rate = 0.01, log = T))
  # ret <- ret + sum(dlnorm(x = rates[117], meanlog = 1, sd = 100, log = T))
  return(ret)
}


MCMC_fit_fe_months_re_weeks <- drjacoby::run_mcmc(data = weekly_deaths_list,
                                          df_params = parms_list,
                                          loglike = r_loglike,
                                          logprior = r_logprior,
                                          burnin = 2.5e3,
                                          samples = 5e4,
                                          pb_markdown = TRUE,
                                          chains = 5)

###########################
###########################
###########################




plot_par(MCMC_fit_fe_months_re_weeks, phase = "burnin")
plot_par(MCMC_fit_fe_months_re_weeks, phase = "sampling", show = "Week_no_1")
plot_par(MCMC_fit_fe_months_re_weeks, phase = "burnin", show = "Month_no_4")
plot_par(MCMC_fit_fe_months_re_weeks, phase = "sampling", show = "Month_no_1")
plot_credible(MCMC_fit_fe_months_re_weeks)


## I have to add together the week numbers and the month
CIs_Weeks <- as.data.frame(t(sapply(1:104, function(x){
  Lambdas <- MCMC_fit_fe_months_re_weeks$output[MCMC_fit_fe_months_re_weeks$output$phase =="sampling",paste0("Week_no_",x)] *
    MCMC_fit_fe_months_re_weeks$output[MCMC_fit_fe_months_re_weeks$output$phase =="sampling",paste0("Month_no_",weekly_deaths_list$month[weekly_deaths_list$week_num==x])]
  Draws <- sapply(Lambdas, function(y){rpois(n = 1, lambda = y)})
  unlist(bayestestR::ci(Draws))
})))

CIs_Months <- as.data.frame(t(sapply(1:12, function(x){
  Lambdas <- MCMC_fit_fe_months_re_weeks$output[MCMC_fit_fe_months_re_weeks$output$phase =="sampling",paste0("Month_no_",x)]
  Draws <- sapply(Lambdas, function(y){rpois(n = 1, lambda = y)})
  unlist(bayestestR::ci(Draws))
})))



## Let's plot with real data to check this:
ggplot(weekly_deaths_list, aes(x = week_start, y = total_deaths, col = as.factor(month))) +
  geom_point() +
  theme_minimal() +
  geom_errorbar(data = CIs_Weeks, aes(x = weekly_deaths_list$week_start, ymin = CI_low, ymax = CI_high, col = as.factor(weekly_deaths_list$month[1:104])), inherit.aes = F) +
  ylab("Burial registrations") + xlab("Week number") +
  viridis::scale_color_viridis("Month number", discrete = T, option = "H") #+
  # geom_errorbar(data = CIs_Months, aes(x = 1:12 +104, ymin = CI_low, ymax = CI_high), col = viridis::turbo(n = 12), inherit.aes = F)


###########################
###########################
###########################


data_list <- list(weekly_deaths_list = weekly_deaths_list)

parms_list <- define_params(name = c(paste0("Week_no_",1:104),"Baseline","tau_week"),
                            min = c(rep(0,104),rep(0,1),0),
                            max = c(rep(5,104),rep(600,1),100))


# define log-likelihood function
r_loglike <- function(params, data, misc) {
  data_deaths <- data
  rates <- as.numeric(params)
  # browser()
  # rates 1:104 weekly
  # rates 105 baseline
  # rate 106 tau_week

  ret <- 0
  ret <- ret + sum(dpois(x = data_deaths$total_deaths[1:104], lambda = rates[105] * rates[1:104], log=T))
  ret <- ret + sum(dlnorm(x = rates[1:104], meanlog = 0, sd = rates[106], log = T))
  return(ret)
}

## define prior
r_logprior <- function(params, misc) {
  rates <- as.numeric(params)

  ret <- 0
  ret <- ret + sum(dgamma(x = rates[105], shape = 0.01, rate = 0.01, log = T))
  # ret <- ret + sum(dlnorm(x = rates[117], meanlog = 1, sd = 100, log = T))
  return(ret)
}


MCMC_fit_fe_months_re_weeks_test <- drjacoby::run_mcmc(data = weekly_deaths_list,
                                                  df_params = parms_list,
                                                  loglike = r_loglike,
                                                  logprior = r_logprior,
                                                  burnin = 5e4,
                                                  samples = 2.5e5,
                                                  pb_markdown = TRUE,
                                                  chains = 5)


plot_credible(MCMC_fit_fe_months_re_weeks_test)
plot_credible(MCMC_fit_fe_months_re_weeks_test, show = paste0("Week_no_",1:104))
plot_par(MCMC_fit_fe_months_re_weeks_test, phase = "burnin", show = "Baseline")
plot_par(MCMC_fit_fe_months_re_weeks_test, phase = "sampling", show = "Baseline")
plot_par(MCMC_fit_fe_months_re_weeks_test, phase = "burnin", show = "Week_no_1")
plot_par(MCMC_fit_fe_months_re_weeks_test, phase = "sampling", show = "Week_no_1")
plot_par(MCMC_fit_fe_months_re_weeks_test, phase = "sampling", show = "Week_no_2")
plot_par(MCMC_fit_fe_months_re_weeks_test, phase = "sampling", show = "Week_no_5")
plot_par(MCMC_fit_fe_months_re_weeks_test, phase = "sampling", show = "Week_no_18")
plot_par(MCMC_fit_fe_months_re_weeks_test, phase = "sampling", show = "tau_week")


Baselines_preds_mcmc <- sapply(MCMC_fit_fe_months_re_weeks_test$output[MCMC_fit_fe_months_re_weeks_test$output$phase =="sampling",c("Baseline")],
       rpois, n = 1)

re_preds_mcmc <- sapply(MCMC_fit_fe_months_re_weeks_test$output[MCMC_fit_fe_months_re_weeks_test$output$phase =="sampling",c("tau_week")],
       rlnorm, n = 1, meanlog = 0)


bayestestR::ci(re_preds_mcmc)
bayestestR::ci(Baselines_preds_mcmc)
bayestestR::ci(Baselines_preds_mcmc*re_preds_mcmc)
boxplot(Baselines_preds_mcmc*re_preds_mcmc, log = "y")
bayestestR::ci(Baselines_preds_mcmc*re_preds_mcmc)[2]$CI_low
bayestestR::ci(Baselines_preds_mcmc*re_preds_mcmc)[3]$CI_high
median(Baselines_preds_mcmc*re_preds_mcmc)
#

## So then we take the true values:
All_registrations <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2021-06-21") %>% #, age_years !=".") %>%
  mutate(#Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
    week_start = lubridate::floor_date(date, unit = "week", week_start = 1),
    week_num = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = F)) %>%
  group_by(week_num) %>%
  summarise(total_deaths = n(),
            week_start = unique(week_start)) %>% ungroup() %>%
  mutate(month = as.numeric(format(week_start, "%m")))


All_registrations <- All_registrations %>% mutate(
  Excess_deaths_median = total_deaths - median(Baselines_preds_mcmc*re_preds_mcmc),
                             Excess_deaths_low95 = bayestestR::ci(Baselines_preds_mcmc*re_preds_mcmc)[2]$CI_low,
                             Excess_deaths_high95 = bayestestR::ci(Baselines_preds_mcmc*re_preds_mcmc)[3]$CI_high)

ggplot(All_registrations, aes(x = week_start, y = Excess_deaths_median)) +
  geom_line()

ggplot(All_registrations, aes(x = week_start, y = Excess_deaths_median)) +
  geom_line() +
  geom_ribbon(aes(ymin = Excess_deaths_low95, ))

