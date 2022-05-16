rm(list = ls())
# devtools::install_github("mrc-ide/drjacoby")
library(drjacoby)
library(dplyr)
library(tidyr)



## Format inputs
# Data
UTH_Mortality_Total <- read.csv(file = "~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv")

weekly_deaths_list_5_plus <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2020-01-06", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
         week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = c(1:105))) %>%
  group_by(Age_gr, week) %>%
  summarise(total_deaths = length(date)) %>% ungroup() %>%
  complete(., Age_gr, week, fill = list(total_deaths = 0)) %>%
  arrange(Age_gr, week) %>%
  filter(Age_gr !=1) %>%
  ungroup() %>%
  pivot_wider(names_from = c(week), values_from = total_deaths, values_fill = 0) %>%
  tibble::column_to_rownames(var="Age_gr")

Under_5s_deaths <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2021-01-01", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F),
         year = as.numeric(format(date,"%Y")),
         week = as.numeric(cut.Date(date, breaks = "1 week", start.on.monday = T, labels = F))) %>%
  complete(expand(., week, Age_gr)) %>%
  ungroup() %>% group_by(Age_gr, week) %>%
  filter(Age_gr ==1) %>%
  summarise(total_deaths = as.numeric(sum(!is.na(date))), st_date = min(date)) %>%
  ungroup() %>%
  select(-Age_gr, -st_date) %>%
  arrange(week)

data_list <- list(Under_5s_deaths = Under_5s_deaths, weekly_deaths_list_5_plus = weekly_deaths_list_5_plus)

#### Parameters

age_cats <- 17

# define parameters for each of the age_rates
df_params <- define_params(name = c(paste0("U_5_Rate_Week",1:nrow(Under_5s_deaths)), paste0("RR",2:age_cats)),
                                     min =c(rep(0,nrow(Under_5s_deaths)), rep(0,age_cats-1)), max = c(rep(150,nrow(Under_5s_deaths)),rep(10,age_cats-1)))

# define log-likelihood function
r_loglike <- function(params, data, misc) {

  # Split data
  data_u5 <- data$Under_5s_deaths
  data_5p <- data$weekly_deaths_list_5_plus

  # Split parameters: under 5 rate and age category variables
  u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:157)])
  rel_rates <- as.numeric(params[paste0("RR",2:age_cats)])

  ret<-0

  for(i in 1:ncol(data_u5)){
    ret <- ret + dpois(x = data_u5$total_deaths[i], lambda = u5_w_rates[i], log=TRUE)
    if(data_u5$year[i] != 2020){
      for(j in 1:(age_cats-1)){
        ret <- ret + dpois(x = data_5p[j,i], lambda = rel_rates[j]*u5_w_rates[i], log=TRUE)
      }
    }
  }
  return(ret)
}

## define prior
r_logprior <- function(params, misc) {

  # extract parameter values
  u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:157)])
  rel_rates <- as.numeric(params[paste0("RR",2:age_cats)])

  # calculate log-prior
  ret <- 0

  # Add a prior for each of the age group relative risks
  for(i in 1:length(rel_rates)){
    ret <- ret + dlnorm(x = rel_rates[i], meanlog = 1, sdlog = 10, log = T)
  }

  # return
  return(ret)
}

mcmc <- drjacoby::run_mcmc(data = data_list,
                           df_params = df_params,
                           loglike = r_loglike,
                           logprior = r_logprior,
                           burnin = 1e4,
                           samples = 5e4,
                           pb_markdown = TRUE,
                           chains = 5)

# saveRDS(mcmc, "../Bonus Files/2022-03-31_mcmc_RR_U5RW.rds")
mcmc <- readRDS("../Bonus Files/2022-04-07_mcmc_baseline.rds")

## Plot estimates onto the data
## Plot Weekly Age 1 for all years:
# ggplot(Under_5s_deaths, aes(x = week, y = total_deaths)) + geom_point()
# plot(Under_5s_deaths$week, Under_5s_deaths$total_deaths, pch = 20)

U5_weeks <- t(rbind(1:157,apply(mcmc$output[mcmc$output$phase=="sampling",grepl(x = names(mcmc$output), pattern = "U_5")], 2, function(x){
  return(c(min(x),max(x)))
})))
colnames(U5_weeks) <- c("week", "min", "max")
U5_weeks <- as.data.frame(U5_weeks)

library(ggplot2)
ggplot() + geom_point(data = Under_5s_deaths, aes(x = week, y = total_deaths)) +
  geom_errorbar(data = U5_weeks, aes(x = week, ymin = min, ymax = max)) +
  xlab("Week") + ylab("AG1 Deaths")


## Plot the Over 5 data.
# For each age group...?
# melt(weekly_deaths_list_5_plus, variable = "week", value = "deaths")

weekly_deaths_all_ages <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2020-01-06", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
         week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = c(1:105))) %>%
  group_by(Age_gr, week) %>%
  summarise(total_deaths = length(date)) %>% ungroup() %>%
  complete(., Age_gr, week, fill = list(total_deaths = 0)) %>%
  arrange(Age_gr, week)

# p1 <- ggplot(data = weekly_deaths_all_ages, aes(x = week, y =total_deaths)) +
#   geom_point() +
#   facet_wrap(~Age_gr, ncol = 2)

# x <- 1

mcmc_death_ests <- melt(str2str::lm2a(lapply(1:105, function(x){ # for each week
  df_tmp <- cbind(mcmc$output[mcmc$output$phase=="sampling",paste0("U_5_Rate_Week",x)], mcmc$output[mcmc$output$phase=="sampling",paste0("U_5_Rate_Week",x)] * mcmc$output[mcmc$output$phase=="sampling",paste0("RR",2:17)]) # multiply the week estimates by the RR for each sample
  colnames(df_tmp) <- 1:17
  return(apply(df_tmp, 2, function(y){return(c(min(y), max(y)))})) # get the min and max estiamted values.
})), varnames = c("MinMax","Age_gr","week"), value.name = "Deaths") %>%
  pivot_wider(names_from = MinMax, values_from = Deaths)

p2 <- ggplot() +
  geom_point(data = weekly_deaths_all_ages, aes(x = as.numeric(week), y =total_deaths), size = 0.5) +
  geom_errorbar(data = mcmc_death_ests, aes(x = as.numeric(week), ymin = `1`, ymax = `2`)) +
  xlab("Week") + #ylab("AG1 Deaths")
  facet_wrap(~as.numeric(Age_gr), ncol = 2)

pdf(file = "analysis/figures/34_03_mcmc_results_2018_2019.pdf", height = 30, width = 15)
# p1
p2
dev.off()

p3 <- ggplot() +
  geom_point(data = weekly_deaths_all_ages, aes(x = as.numeric(Age_gr), y =total_deaths), size = 0.5) +
  geom_errorbar(data = mcmc_death_ests, aes(x = as.numeric(Age_gr), ymin = `1`, ymax = `2`)) +
  xlab("Age group") + #ylab("AG1 Deaths")
  facet_wrap(~as.numeric(week), ncol = 8)

pdf(file = "analysis/figures/34_04_mcmc_results_2018_2019_week.pdf", height = 30, width = 15)
# p1
p3 + ggtitle("MCMC fit to the backgound mortality by week")
dev.off()


mcmc$output[1001:1100,paste0("RR",2:17)] * as.numeric(1:100)

t(t(mcmc$output[1001:1100,paste0("RR",2:17)]) * as.numeric(1:100))
mcmc$output[1001:1100,paste0("U_5_Rate_Week",33)] * mcmc$output[1001:1100,paste0("RR",2:17)]
t(mcmc$output[1001:1100,paste0("U_5_Rate_Week",33)] * t(mcmc$output[1001:1100,paste0("RR",2:17)]))


# If I take the data and scale by the relative risks...? But there are lots of estimates for that..


plot_par(mcmc, phase = "burnin")
plot_par(mcmc, show = "U_5_Rate_Week1", phase = "burnin")
plot_par(mcmc, show = "U_5_Rate_Week33", phase = "burnin")
plot_par(mcmc, show = "U_5_Rate_Week33", phase = "sampling")

plot_par(mcmc, show = "RR2", phase = "burnin")
plot_par(mcmc, show = "RR3", phase = "burnin")
plot_par(mcmc, show = "RR4", phase = "burnin")
plot_par(mcmc, show = "RR5", phase = "burnin")
plot_par(mcmc, show = "RR6", phase = "burnin")
plot_par(mcmc, show = "RR7", phase = "burnin")
plot_par(mcmc, show = "RR8", phase = "burnin")
plot_par(mcmc, show = "RR9", phase = "burnin")
plot_par(mcmc, show = "RR10", phase = "burnin")
plot_par(mcmc, show = "RR11", phase = "burnin")
plot_par(mcmc, show = "RR12", phase = "burnin")
plot_par(mcmc, show = "RR13", phase = "burnin")
plot_par(mcmc, show = "RR14", phase = "burnin")
plot_par(mcmc, show = "RR16", phase = "burnin")
plot_par(mcmc, show = "RR17", phase = "burnin")

cbind(mcmc$diagnostics$rhat<1.1)
mcmc$diagnostics$rhat
cbind(mcmc$diagnostics$ess)
# 42: 1.1499
plot_par(mcmc, show = "U_5_Rate_Week42", phase = "burnin")

plot_par(mcmc, show = "U_5_Rate_Week1", phase = "sampling")
plot_par(mcmc, show = "U_5_Rate_Week10", phase = "sampling")

plot_par(mcmc, show = "RR2", phase = "sampling")
plot_par(mcmc, show = "RR3", phase = "sampling")
plot_par(mcmc, show = "RR4", phase = "sampling")
plot_par(mcmc, show = "RR5", phase = "sampling")
plot_par(mcmc, show = "RR6", phase = "sampling")
plot_par(mcmc, show = "RR7", phase = "sampling")
plot_par(mcmc, show = "RR8", phase = "sampling")
plot_par(mcmc, show = "RR9", phase = "sampling")
plot_par(mcmc, show = "RR10", phase = "sampling")
plot_par(mcmc, show = "RR11", phase = "sampling")
plot_par(mcmc, show = "RR12", phase = "sampling")
plot_par(mcmc, show = "RR13", phase = "sampling")
plot_par(mcmc, show = "RR14", phase = "sampling")
plot_par(mcmc, show = "RR16", phase = "sampling")
plot_par(mcmc, show = "RR17", phase = "sampling")

mcmc$output %>% filter(phase =="sampling") %>% select(-chain, -phase, -iteration) %>%
  colMeans()



cbind(mcmc$diagnostics$rhat<1.1)
























































### Example

# set random seed
set.seed(1)

# define true parameter values
mu_true <- 3
sigma_true <- 2

# draw example data
data_list <- list(x = rnorm(10, mean = mu_true, sd = sigma_true))

# define parameters dataframe
df_params <- define_params(name = "mu", min = -10, max = 10,
                           name = "sigma", min = 0, max = Inf)

print(df_params)
#>    name min max
#> 1    mu -10  10
#> 2 sigma   0 Inf

# define log-likelihood function
r_loglike <- function(params, data, misc) {
  print("call_ll")
  # extract parameter values
  mu <- params["mu"]
  sigma <- params["sigma"]

  # calculate log-probability of data
  ret <- sum(dnorm(data$x, mean = mu, sd = sigma, log = TRUE))

  # return
  return(ret)
}

# define log-prior function
r_logprior <- function(params, misc) {
  print("call_lp")
  # extract parameter values
  mu <- params["mu"]
  sigma <- params["sigma"]

  # calculate log-prior
  ret <- dunif(mu, min = -10, max = 10, log = TRUE) +
    dlnorm(sigma, meanlog = 0, sdlog = 1.0, log = TRUE)

  # return
  return(ret)
}

mcmc <- run_mcmc(data = data_list,
                 df_params = df_params,
                 loglike = r_loglike,
                 logprior = r_logprior,
                 burnin = 5,
                 samples = 5,
                 pb_markdown = TRUE,
                 chains = 1)


library(profvis)
profvis({

  mcmc <- run_mcmc(data = data_list,
                   df_params = df_params,
                   loglike = r_loglike,
                   logprior = r_logprior,
                   burnin = 5,
                   samples = 5,
                   pb_markdown = TRUE,
                   chains = 1)

})
























## Get my data
UTH_Mortality_Total <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv")
monthly_deaths_list <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2020-01-01", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F),
         year = format(date,"%Y"),
         month = format(date,"%m")) %>%
  group_by(Age_gr, year, month) %>%
  summarise(total_deaths = length(date)) %>%
  spread(key = Age_gr, value = total_deaths) %>%
  ungroup() %>% select(-year, -month) %>%
  setNames(paste0("age_cat",1:17)) %>%
  as.list()
#
num_age_cat=17
num_months=24
#
# # define parameters for each of the age_rates
df_params <- define_params(name = paste0("age_rate_est",1:num_age_cat),
                           min =rep(0,num_age_cat), max = rep(100000,num_age_cat))
#
#
#
#
# # define log-likelihood function
r_loglike <- function(params, data, misc) {

  print("call")

  # find relavent parameters and age category variables
  rates <- as.numeric(params)
  age_cats<-grep("age_cat",names(data))
  ## sum over age cats and months
  ret<-0
  for(i in 1:length(age_cats)){
    ret<-ret+sum(dpois(unlist(data[age_cats[i]]),lambda=rates[i],log=TRUE))
  }
  # return
  return(ret)
}
#
# ## define params (yet to do currently effectively uniform between 0 and 100000 as we've set those limits for the params)
r_logprior <- function(params, misc) {

  # extract parameter values
  # mu <- params["mu"]
  #sigma <- params["sigma"]

  # calculate log-prior
  ret <- 0

  # return
  return(ret)
}
#
mcmc <- run_mcmc(data = monthly_deaths_list,
                 df_params = df_params,
                 loglike = r_loglike,
                 logprior = r_logprior,
                 burnin = 5,
                 samples = 5,
                 pb_markdown = TRUE)
#
# # plot_par(mcmc)
#
# # For each of those, I need to sample 100
# Samples_age_ests <- apply(mcmc$output[mcmc$output$phase=="sampling",paste0("age_rate_est",1:17)],
#       MARGIN = 2,
#       function(x){
#         sample(x = x, size = 100)
#       })
#
# # saveRDS(Samples_age_ests, "analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")
# Samples_age_ests <- readRDS("analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")



############################################################
############################################################
############################################################


## 1. Fit a poisson fit to the  under 5 data for teh three years
# Under_5s_deaths <- UTH_Mortality_Total %>%
#   mutate(date = as.Date(dod, "%m/%d/%y")) %>%
#   filter(dod != ".") %>%
#   filter(date >= "2018-01-01",date < "2021-01-01", age_years !=".") %>%
#   mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F),
#          year = format(date,"%Y"),
#          month = format(date,"%m")) %>%
#   group_by(Age_gr, year, month) %>%
#   summarise(total_deaths = length(date)) %>%
#   filter(Age_gr ==1) %>%
#   # spread(key = Age_gr, value = total_deaths) %>%
#   ungroup() %>% pull(total_deaths)
#   # setNames(paste0("age_cat",1:17)) %>%
#   # as.list()
#
# monthly_deaths_list$under_5s_deaths <- Under_5s_deaths
# monthly_deaths_list$age_cat1 <- NULL
# # num_months=36
#
#
#
# num_age_cat=17
# # define parameters for each of the age_rates
# df_params <- define_params(name = c("U_5_Rate", paste0("RR",2:num_age_cat)),
#                            min =c(0, rep(0,num_age_cat-1)), max = c(300,rep(10,num_age_cat-1)))
#
#
# # define log-likelihood function
# r_loglike <- function(params, data, misc) {
#
#   # Split parameters: under 5 rate and age category variables
#   rate_under_5 <- as.numeric(params["U_5_Rate"])
#   rel_rates <- as.numeric(params[paste0("RR",2:num_age_cat)])
#   age_cats <- grep("age_cat",names(data))
#
#   # Split data
#   under_5s_deaths <- as.numeric(unlist(data[grep("under_5s_deaths", names(data))]))
#   m_d_list <- data[grep("age_cat",names(data))]
#
#   ret<-0
#   ret <- ret + sum(sapply(X = under_5s_deaths, FUN = dpois, lambda = rate_under_5, log = T))
#
#   ## sum over age cats and months
#   for(i in 1:length(age_cats)){
#     ret <- ret + sum(sapply(X = m_d_list[[age_cats[i]]], FUN = dpois, lambda = rel_rates[i]*rate_under_5, log=TRUE))
#   }
#   # return
#   return(ret)
# }
#
# # r_loglike(params = df_params, data = monthly_deaths_list)
#
# ##
# r_logprior <- function(params, misc) {
#
#   # extract parameter values
#   rel_rates <- as.numeric(params[paste0("RR",2:num_age_cat)])
#
#   # calculate log-prior
#   ret <- 0
#
#   # Add a prior for each of the age group relative risks
#   for(i in 1:length(rel_rates)){
#     ret <- ret + dlnorm(x = rel_rates[i], meanlog = 1, sdlog = 10, log = T)
#   }
#
#   # return
#   return(ret)
# }
#
# # par(mfrow=c(1,1))
# # curve(expr = dlnorm(x = x, meanlog = log(1), sdlog = 2), to = 1)
# # curve(expr = dlnorm(x = x, meanlog = 4, sdlog = 2), to = 2)
# # curve(expr = dlnorm(x = x, meanlog = 16, sdlog = 4), to = 2)
# # abline(v = 1)
# #
# # dlnorm(x = c(0.9,1,1.1), meanlog = exp(1), sdlog = 2)
#
# mcmc <- run_mcmc(data = monthly_deaths_list,
#                  df_params = df_params,
#                  loglike = r_loglike,
#                  logprior = r_logprior,
#                  burnin = 1e3,
#                  samples = 1e3,
#                  pb_markdown = TRUE)
#
# # plot_par(mcmc)
#
# plot_par(mcmc, phase = "burnin")
# plot_par(mcmc, phase = "sampling")
#
#
# mean(mcmc$output$phase)
# mean(mcmc$output$RR12)
#
# colMeans(mcmc$output[mcmc$output$phase=="sampling",df_params$name])
#
# # For each of those, I need to sample 100
# Samples_age_ests <- apply(mcmc$output[mcmc$output$phase=="sampling",df_params$name],
#                           MARGIN = 2,
#                           function(x){
#                             sample(x = x, size = 100)
#                           })
#
# plot(colMeans(Samples_age_ests))
# length(colMeans(Samples_age_ests))




############################################################
############################################################
############################################################

# weekly_deaths_list_5_plus <- UTH_Mortality_Total %>%
#   mutate(date = as.Date(dod, "%m/%d/%y")) %>%
#   filter(dod != ".") %>%
#   filter(date >= "2018-01-01",date < "2020-01-01", age_years !=".") %>%
#   mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = paste0("AG",1:17)),
#          year = format(date,"%Y"),
#          month = format(date,"%m")) %>%
#   filter(Age_gr != paste0("AG",1)) %>%
#   group_by(year) %>%
#   mutate(week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = paste0("W",1:53))) %>%
#   group_by(Age_gr, year, week) %>%
#   summarise(total_deaths = as.numeric(length(date))) %>%
#   filter(week %in% paste0("W",1:53)) %>%
#   ungroup() %>%
#   # complete(Age_gr, year, week) %>%
#   # mutate(total_deaths = ifelse(is.na(total_deaths),0,total_deaths)) %>%
#   pivot_wider(names_from = c(Age_gr,week), values_from = total_deaths, values_fill = 0) %>%
#   select(-year) %>%
#   # spread(key = Age_gr, value = total_deaths) %>%
#   ungroup() %>%
#   # setNames(paste0("age_cat",1:17)) %>%
#   as.list()
#
#
# ## 1. Fit a poisson fit to the  under 5 data for teh three years
# Under_5s_deaths <- UTH_Mortality_Total %>%
#   mutate(date = as.Date(dod, "%m/%d/%y")) %>%
#   filter(dod != ".") %>%
#   filter(date >= "2018-01-01",date < "2021-01-01", age_years !=".") %>%
#   mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F),
#          year = format(date,"%Y"),
#          month = format(date,"%m")) %>%
#   group_by(year) %>%
#   mutate(week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = F)) %>%
#   ungroup() %>% group_by(Age_gr, week, year) %>%
#   summarise(total_deaths = length(date)) %>%
#   # mutate(total_deaths = ifelse(is.na(total_deaths),0, total_deaths)) %>%
#   filter(Age_gr ==1, week %in% 1:53) %>%
#   ungroup() %>%
#   spread(key = week, value = total_deaths, fill = 0) %>%
#   select(-Age_gr, -year) %>%
#   # spread(key = Age_gr, value = total_deaths) %>%
#   # ungroup() %>% %>% group_by(week) %>% #pull(total_deaths)
# setNames(paste0("U5_W",1:53)) %>%
# as.list()
#
#
#
# data_list <- c(Under_5s_deaths,weekly_deaths_list_5_plus)
# # data_list$weekly_deaths_list_5_plus <- weekly_deaths_list_5_plus
#
# # monthly_deaths_list$under_5s_deaths <- Under_5s_deaths
# # monthly_deaths_list$age_cat1 <- NULL
# # num_months=36
#
#
#
# num_age_cat=17
# # define parameters for each of the age_rates
# df_params <- define_params(name = c(paste0("U_5_Rate_Week",1:length(Under_5s_deaths)), paste0("RR",2:num_age_cat)),
#                            min =c(rep(0,length(Under_5s_deaths)), rep(0,num_age_cat-1)), max = c(rep(150,length(Under_5s_deaths)),rep(10,num_age_cat-1)))
#
#
# # define log-likelihood function
# r_loglike <- function(params, data, misc) {
#
#   # Split parameters: under 5 rate and age category variables
#   u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:53)])
#   rel_rates <- as.numeric(params[paste0("RR",2:num_age_cat)])
#   age_cats <- 17#grep("age_cat",names(data))
#
#   # Split data
#   u_5s_d_list <- data[grep("U5_W", names(data))]
#   m_d_list <- data[grep("AG",names(data))]
#   #
#   ret<-0
#
#
#   ret <- sum(sapply(1:length(u_5s_d_list), function(i){
#     ret_tmp <- sum(dpois(x = as.numeric(u_5s_d_list[[i]]), lambda = as.numeric(u5_w_rates[i]), log=TRUE))
#     ret_tmp <- ret_tmp +
#       sum(sapply(2:age_cats, function(j){
#         ret_tmp2 <- sum(dpois(x = as.numeric(unlist(data[grepl(paste0("AG",j,"_W",i,"$"),names(data))])),
#                                lambda = as.numeric(rel_rates[j-1]*u5_w_rates[i], log=TRUE)))
#       }))})
#   )
#
#   # ret <- sum(sapply(1:length(u_5s_d_list), function(i){
#   #   ret_tmp <- sum(sapply(X = as.numeric(u_5s_d_list[[i]]), FUN = dpois,
#   #                           lambda = as.numeric(u5_w_rates[i]), log=TRUE))
#   #   ret_tmp <- ret_tmp +
#   #     sum(sapply(2:age_cats, function(j){
#   #       ret_tmp2 <- sum(sapply(X = as.numeric(unlist(data[grepl(paste0("AG",j,"_W",i,"$"),names(data))])),
#   #                               FUN = dpois,
#   #                               lambda = as.numeric(rel_rates[j-1]*u5_w_rates[i], log=TRUE)))
#   #     }))})
#   #   )
#
#
#   # ## sum over under 5 for each week
#   # for(i in 1:length(u_5s_d_list)){
#   #   ret <- ret + sum(sapply(X = as.numeric(u_5s_d_list[[i]]), FUN = dpois,
#   #                           lambda = as.numeric(u5_w_rates[i]), log=TRUE))
#   #   for(j in 2:age_cats){
#   #     ret <- ret + sum(sapply(X = as.numeric(unlist(data[grepl(paste0("AG",j,"_W",i,"$"),names(data))])),
#   #                             FUN = dpois,
#   #                             lambda = as.numeric(rel_rates[j-1]*u5_w_rates[i], log=TRUE)))
#   #   }
#   # }
#
#   return(ret)
# }
#
# # r_loglike(params = df_params, data = monthly_deaths_list)
#
# ##
# r_logprior <- function(params, misc) {
#
#   # extract parameter values
#   u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:53)])
#   rel_rates <- as.numeric(params[paste0("RR",2:num_age_cat)])
#
#   # calculate log-prior
#   ret <- 0
#
#   # Add a prior for each of the age group relative risks
#   for(i in 1:length(rel_rates)){
#     ret <- ret + dlnorm(x = rel_rates[i], meanlog = 1, sdlog = 10, log = T)
#   }
#
#   # return
#   return(ret)
# }
#
# # par(mfrow=c(1,1))
# # curve(expr = dlnorm(x = x, meanlog = log(1), sdlog = 2), to = 1)
# # curve(expr = dlnorm(x = x, meanlog = 4, sdlog = 2), to = 2)
# # curve(expr = dlnorm(x = x, meanlog = 16, sdlog = 4), to = 2)
# # abline(v = 1)
# #
# # dlnorm(x = c(0.9,1,1.1), meanlog = exp(1), sdlog = 2)
#
# mcmc <- run_mcmc(data = data_list,
#                  df_params = df_params,
#                  loglike = r_loglike,
#                  logprior = r_logprior,
#                  burnin = 500,#1e3,
#                  samples = 500,#1e3,
#                  pb_markdown = TRUE)
#
# # plot_par(mcmc)
#
# # saveRDS(mcmc, file = "../Bonus Files/2022-03-13_mcmc_U5Age_RR.rds")
# mcmc <- readRDS(file = "../Bonus Files/2022-03-13_mcmc_U5Age_RR.rds")
#
# mcmc$output
#
# U5Data <- do.call(rbind.data.frame, Under_5s_deaths)
# colnames(U5Data) <- 1:3
#
# plot(colMeans(mcmc$output[,grepl("U_5", colnames(mcmc$output))]), type= "l")
# points(x = rownames(U5Data), y = unlist(lapply(Under_5s_deaths, mean)), pch =20, size = 0.3)
#
# library(ggplot2)
# plot_data <- data.frame(x = 1:53, y = colMeans(mcmc$output[,grepl("U_5", colnames(mcmc$output))]))
# ggplot(data = plot_data, aes(x = x, y = y)) +        # ggplot2 plot with confidence intervals
#   geom_line() +
#   geom_point(aes(y = unlist(lapply(Under_5s_deaths, mean)))) +
#   geom_errorbar(aes(ymin = unlist(lapply(Under_5s_deaths, min)), ymax = unlist(lapply(Under_5s_deaths, max)))) +
#   xlab("Week") + ylab("Deaths") +
#   ggtitle("Under 5 deaths: 2018-2020")
#
#
# plot_data2 <- data.frame(x = 2:17, y = colMeans(mcmc$output[,grepl("RR", colnames(mcmc$output))]))
# ggplot(data = plot_data2, aes(x = x, y = y)) +        # ggplot2 plot with confidence intervals
#   geom_line() +
#   geom_point(data = Ages_Deaths, aes(x = Age, y = deaths)) +
#   xlab("Age groups (2-17)") + ylab("Relative risk") +
#   ggtitle("MCMC fit to total age deaths")
#
# Ages_Deaths <- UTH_Mortality_Total %>%
#   mutate(date = as.Date(dod, "%m/%d/%y")) %>%
#   filter(dod != ".") %>%
#   filter(date >= "2018-01-01",date < "2020-01-01", age_years !=".") %>%
#   mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = paste0("AG",1:17)),
#          year = format(date,"%Y"),
#          month = format(date,"%m")) %>%
#   # filter(Age_gr != paste0("AG",1)) %>%
#   group_by(year) %>%
#   # mutate(week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = paste0("W",1:53))) %>%
#   group_by(Age_gr) %>%
#   summarise(total_deaths = as.numeric(length(date))) %>%
#   mutate(deaths = total_deaths/total_deaths[1]) %>%
#   filter(Age_gr != "AG1") %>%
#   mutate(Age = 2:17)
#
#
# plot(colMeans(mcmc$output[,grepl("RR", colnames(mcmc$output))]))







################################################################################
################################################################################
################################################################################

weekly_deaths_list_5_plus <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2020-01-01", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = paste0("AG",1:17)),
         year = format(date,"%Y"),
         month = format(date,"%m")) %>%
  filter(Age_gr != paste0("AG",1)) %>%
  group_by(year) %>%
  mutate(week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = paste0("W",1:53))) %>%
  group_by(Age_gr, year, week) %>%
  summarise(total_deaths = as.numeric(length(date))) %>%
  filter(week %in% paste0("W",1:53)) %>%
  ungroup() %>%
  # complete(Age_gr, year, week) %>%
  # mutate(total_deaths = ifelse(is.na(total_deaths),0,total_deaths)) %>%
  pivot_wider(names_from = c(Age_gr,week), values_from = total_deaths, values_fill = 0) %>%
  select(-year) %>%
  # spread(key = Age_gr, value = total_deaths) %>%
  ungroup() %>%
  # setNames(paste0("age_cat",1:17)) %>%
  as.list()

## 1. Fit a poisson fit to the  under 5 data for teh three years
Under_5s_deaths <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2021-01-01", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F),
         year = as.numeric(format(date,"%Y")),
         month = format(date,"%m")) %>%
  # group_by(year) %>%
  mutate(week = as.numeric(cut.Date(date, breaks = "1 week", start.on.monday = T, labels = F))) %>%
  ungroup() %>% group_by(Age_gr, week, year) %>%
  summarise(total_deaths = as.numeric(length(date))) %>%
  # mutate(total_deaths = ifelse(is.na(total_deaths),0, total_deaths)) %>%
  filter(Age_gr ==1) %>%
  ungroup() %>%
  # spread(key = week, value = total_deaths, fill = 0) %>%
  select(-Age_gr)
  # spread(key = Age_gr, value = total_deaths) %>%
  # ungroup() %>% %>% group_by(week) %>% #pull(total_deaths)
# setNames(paste0("U5_W",1:nrow(.))) %>%
# as.list()



data_list <- c(list(Under_5s_deaths = Under_5s_deaths),weekly_deaths_list_5_plus)
# data_list$weekly_deaths_list_5_plus <- weekly_deaths_list_5_plus

# monthly_deaths_list$under_5s_deaths <- Under_5s_deaths
# monthly_deaths_list$age_cat1 <- NULL
# num_months=36



num_age_cat=17
# define parameters for each of the age_rates
df_params <- define_params(name = c(paste0("U_5_Rate_Week",1:nrow(Under_5s_deaths)), paste0("RR",2:num_age_cat)),
                           min =c(rep(0,nrow(Under_5s_deaths)), rep(0,num_age_cat-1)), max = c(rep(150,nrow(Under_5s_deaths)),rep(10,num_age_cat-1)))


# define log-likelihood function
r_loglike <- function(params, data, misc) {

  # browser()

  # Split parameters: under 5 rate and age category variables
  u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:157)])
  rel_rates <- as.numeric(params[paste0("RR",2:num_age_cat)])
  age_cats <- 17#grep("age_cat",names(data))

  # Split data
  u_5s_d_list <- data[[grep("Under_5", names(data))]]
  m_d_list <- data[grep("AG",names(data))]

  ret<-0

  # browser()
  ret <- sum(sapply(1:nrow(u_5s_d_list), function(i){
    ret_tmp <- sum(dpois(x = as.numeric(u_5s_d_list[i,"total_deaths"]), lambda = as.numeric(u5_w_rates[i]), log=TRUE))
    if(u_5s_d_list$year[i]==2020){
      ret_tmp <- ret_tmp +
      # print(i)
        sum(sapply(2:age_cats, function(j){
          ret_tmp2 <- sum(dpois(x = as.numeric(unlist(data[grepl(paste0("AG",j,"_W",i-104,"$"),names(data))])),
                                lambda = as.numeric(rel_rates[j-1]*u5_w_rates[i]), log=TRUE))
        }))
      # browser()
    }
    ret_tmp
  }))
  # browser()

  return(ret)
}

# r_loglike(params = df_params, data = monthly_deaths_list)

##
r_logprior <- function(params, misc) {

  # extract parameter values
  u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:157)])
  rel_rates <- as.numeric(params[paste0("RR",2:num_age_cat)])

  # calculate log-prior
  ret <- 0

  # Add a prior for each of the age group relative risks
  for(i in 1:length(rel_rates)){
  ret <- ret + dlnorm(x = rel_rates[i], meanlog = 1, sdlog = 10, log = T)
  }

  # return
  return(ret)
}

# par(mfrow=c(1,1))
# curve(expr = dlnorm(x = x, meanlog = log(1), sdlog = 2), to = 1)
# curve(expr = dlnorm(x = x, meanlog = 4, sdlog = 2), to = 2)
# curve(expr = dlnorm(x = x, meanlog = 16, sdlog = 4), to = 2)
# abline(v = 1)
#
# dlnorm(x = c(0.9,1,1.1), meanlog = exp(1), sdlog = 2)

mcmc <- run_mcmc(data = data_list,
                 df_params = df_params,
                 loglike = r_loglike,
                 logprior = r_logprior,
                 burnin = 200,#1e3,
                 samples = 200,#1e3,
                 pb_markdown = TRUE)

# plot_par(mcmc)

# saveRDS(mcmc, file = "../Bonus Files/2022-03-13_mcmc_U5Age_RR.rds")
# mcmc <- readRDS(file = "../Bonus Files/2022-03-13_mcmc_U5Age_RR.rds")

mcmc$output

U5Data <- do.call(rbind.data.frame, Under_5s_deaths)
colnames(U5Data) <- 1:3

plot(colMeans(mcmc$output[,grepl("U_5", colnames(mcmc$output))]), type= "l")
points(x = rownames(U5Data), y = unlist(lapply(Under_5s_deaths, mean)), pch =20, size = 0.3)

library(ggplot2)
plot_data <- data.frame(x = 1:53, y = colMeans(mcmc$output[,grepl("U_5", colnames(mcmc$output))]))
ggplot(data = plot_data, aes(x = x, y = y)) +        # ggplot2 plot with confidence intervals
  geom_line() +
  geom_point(aes(y = unlist(lapply(Under_5s_deaths, mean)))) +
  geom_errorbar(aes(ymin = unlist(lapply(Under_5s_deaths, min)), ymax = unlist(lapply(Under_5s_deaths, max)))) +
  xlab("Week") + ylab("Deaths") +
  ggtitle("Under 5 deaths: 2018-2020")


plot_data2 <- data.frame(x = 2:17, y = colMeans(mcmc$output[,grepl("RR", colnames(mcmc$output))]))
ggplot(data = plot_data2, aes(x = x, y = y)) +        # ggplot2 plot with confidence intervals
  geom_line() +
  geom_point(data = Ages_Deaths, aes(x = Age, y = deaths)) +
  xlab("Age groups (2-17)") + ylab("Relative risk") +
  ggtitle("MCMC fit to total age deaths")

Ages_Deaths <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2020-01-01", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = paste0("AG",1:17)),
         year = format(date,"%Y"),
         month = format(date,"%m")) %>%
  # filter(Age_gr != paste0("AG",1)) %>%
  group_by(year) %>%
  # mutate(week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = paste0("W",1:53))) %>%
  group_by(Age_gr) %>%
  summarise(total_deaths = as.numeric(length(date))) %>%
  mutate(deaths = total_deaths/total_deaths[1]) %>%
  filter(Age_gr != "AG1") %>%
  mutate(Age = 2:17)


plot(colMeans(mcmc$output[,grepl(x = colnames(mcmc$output), pattern = "RR")]))











































Under_5s_deaths




plot_par(mcmc, phase = "burnin")
plot_par(mcmc, phase = "sampling")


mean(mcmc$output$phase)
mean(mcmc$output$RR12)

colMeans(mcmc$output[mcmc$output$phase=="sampling",df_params$name])
plot(colMeans(mcmc$output[mcmc$output$phase=="sampling",df_params$name])[1:53], type = "l")
points(x = rownames(U5Data), y = U5Data$`1`, pch =20, size = 0.3)
points(x = rownames(U5Data), y = U5Data$`2`, pch =20, size = 0.3)
points(x = rownames(U5Data), y = U5Data$`3`, pch =20, size = 0.3)

U5Data <- do.call(rbind.data.frame, Under_5s_deaths)
colnames(U5Data) <- 1:3

# For each of those, I need to sample 100
Samples_age_ests <- apply(mcmc$output[mcmc$output$phase=="sampling",df_params$name],
                          MARGIN = 2,
                          function(x){
                            sample(x = x, size = 100)
                          })

colMeans(Samples_age_ests)

U5Data



























































# Tests_shorter_duration[[1]]$output

Tests_shorter_duration <- readRDS("../Bonus Files/2022-03-03_Testing_pois_bin_bin_x3.rds")

Tests_shorter_duration$X1$pmcmc_results$chains$chain1$results$log_posterior
Tests_shorter_duration$X33$pmcmc_results$chains$chain1$results$log_posterior
Tests_shorter_duration$X37$pmcmc_results$chains$chain1$results$log_posterior

Tests_shorter_duration$X33$pmcmc_results$inputs

index <- squire:::odin_index(Tests_shorter_duration[[1]]$model)
Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                 to = as.Date("2020-10-02"),
                                 by = "week"),as.Date("2020-10-02"))

readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data.rds")
Mort_Deaths_Age_Week <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age.rds") %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  group_by(week, Age_gr) %>%
  summarise(total_deaths = sum(Mort_deaths),
            date = head(date,1))


# Graphs <- lapply(list(Test), function(fit){
Graphs <- lapply(Testx3, function(fit){
# Graphs <- lapply(Tests_shorter_duration, function(fit){
  # browser()
  Mod_Age_Deaths_Lus <- apply(fit$output,3,function(x){

    AgeRes <- lapply(1:ncol(x[,index$S]),function(Age_gr){
      Ds_Lus <- diff(x[as.Date(rownames(x)) %in% Days_for_comparison, index$D][,Age_gr],na.rm = T)
      Ds_Mor <- Ds_Lus*0.8
      ll <- dpois(x = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_gr==Age_gr,]$total_deaths, lambda = Ds_Mor + mean(Samples_age_ests[,Age_gr]*12/52), log = T)

      return(data.frame(Week_gr = 1:16,
                        Age_gr = Age_gr,
                        Ds_Lus = Ds_Lus,
                        Ds_Mor = Ds_Mor,
                        Ds_Tot_Mor = Ds_Mor + mean(Samples_age_ests[,Age_gr]*12/52),
                        total_deaths = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_gr==Age_gr,]$total_deaths,
                        ll = ll))
    })

    AgeRes <- do.call(rbind.data.frame, AgeRes)
    AgeRes
  })

  Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus %>%
    str2str::ld2a() %>%
    cbind(.[,c("Age_gr","Week_gr","total_deaths"),1],
          data.frame(Mean = apply(.[,c("Ds_Lus","Ds_Mor","Ds_Tot_Mor","ll"),], MARGIN = 2, FUN = function(x){rowMeans(x)}),
                     Max = apply(.[,c("Ds_Lus","Ds_Mor","Ds_Tot_Mor","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = max)}),
                     Min = apply(.[,c("Ds_Lus","Ds_Mor","Ds_Tot_Mor","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = min)}))) %>%
    select(Age_gr,Week_gr,total_deaths,
           Mean.Ds_Lus, Max.Ds_Lus, Min.Ds_Lus,
           Mean.Ds_Mor, Max.Ds_Mor, Min.Ds_Mor,
           Mean.Ds_Tot_Mor, Max.Ds_Tot_Mor, Min.Ds_Tot_Mor,
           Mean.ll, Max.ll, Min.ll) %>%
    arrange(Age_gr, Week_gr) %>%
    filter(Week_gr != 4)
  rownames(Mod_Age_Deaths_Lus_Av) <- NULL

  ## Plot by week
  Mod_Age_Deaths_Lus_Av_Week <- merge(Mod_Age_Deaths_Lus_Av, Mort_Deaths_Age_Week[,c("week","Age_gr","date")] %>% rename(Week_gr = week)) %>% group_by(Week_gr) %>%
    summarise(date = head(date,1)+3,#Week_gr = head(Week_gr,1),
              total_deaths = sum(total_deaths),
              Mean.Ds_Tot_Mor = sum(Mean.Ds_Tot_Mor),
              Max.Ds_Tot_Mor = sum(Max.Ds_Tot_Mor),
              Min.Ds_Tot_Mor = sum(Min.Ds_Tot_Mor)
              # Could add pcr, but this would need to be weighted by population size in each age group?
    )
# browser()
  p1 <- ggplot(Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Ds_Tot_Mor),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Tot_Mor, ymax=Max.Ds_Tot_Mor), alpha=0.3) +
    # Modelled True deaths
    # geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    # geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = total_deaths)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Date") + ylab("Deaths") +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle(paste("COVID-19 weekly deaths\n(Mortuary) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
    theme(plot.title = element_text(size = 10))

  Age_grs.labs <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_gr) %>%
    summarise(Mean.ll = sum(Mean.ll)) %>%
    mutate(lab = paste0("pois ll AG ",Age_gr,": ", round(Mean.ll,1))) %>%
    select(lab) %>%
    unlist()
  #
  names(Age_grs.labs) <- 1:17


  p2 <- ggplot(merge(Mod_Age_Deaths_Lus_Av, Mort_Deaths_Age_Week[,c("week","Age_gr","date")] %>% rename(Week_gr = week)), aes(x = date)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Ds_Tot_Mor),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Tot_Mor, ymax=Max.Ds_Tot_Mor), alpha=0.3) +
    facet_wrap(vars(Age_gr), labeller = labeller(Age_gr = Age_grs.labs)) +
    # Modelled True deaths
    # geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    # geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = total_deaths)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Week") + ylab("Deaths") +
    # xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle(paste("COVID-19 weekly deaths\n(Lusaka) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
    theme(plot.title = element_text(size = 10))

  return(list(p1,p2))
  })




cowplot::plot_grid(Graphs$X1[[1]],Graphs$X33[[1]],Graphs$X37[[1]])

pdf(file = "analysis/figures/34_02_Age_Week_Deaths_Test.pdf", width = 30, height = 10)
cowplot::plot_grid(Graphs$X1[[2]] + xlab("0.2x0.2"),
                   Graphs$X33[[2]] + xlab("0.2x1"),
                   Graphs$X37[[2]] + xlab("1x1"), nrow = 1)
dev.off()


Mod_Age_Deaths_Lus <- apply(Tests_shorter_duration[[3]]$output,3,function(x){

  AgeRes <- lapply(1:ncol(x[,index$S]),function(Age_gr){
    Ds_Lus <- diff(x[as.Date(rownames(x)) %in% Days_for_comparison, index$D][,Age_gr],na.rm = T)
    Ds_Mor <- Ds_Lus*0.8
    ll <- dpois(x = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_gr==Age_gr,]$total_deaths, lambda = Ds_Mor + mean(Samples_age_ests[,Age_gr]*12/52), log = T)

    return(data.frame(Week_gr = 1:16,
                      Age_gr = Age_gr,
                      Ds_Lus = Ds_Lus,
                      Ds_Mor = Ds_Mor,
                      Ds_Tot_Mor = Ds_Mor + mean(Samples_age_ests[,Age_gr]*12/52),
                      total_deaths = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_gr==Age_gr,]$total_deaths,
                      ll = ll))
  })

  AgeRes <- do.call(rbind.data.frame, AgeRes)
  AgeRes
})


