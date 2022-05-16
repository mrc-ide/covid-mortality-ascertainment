rm(list = ls())
# devtools::install_github("mrc-ide/drjacoby")
library(drjacoby)
library(dplyr)
library(tidyr)
library(tidyverse)

# mcmc <- readRDS("../Bonus Files/2022-04-05_mcmc_baseline.rds")
# mcmc2 <- readRDS("../Bonus Files/2022-04-06_mcmc_baseline.rds")
# mcmc3 <- readRDS("../Bonus Files/2022-04-07_mcmc_baseline.rds")
# mcmc4 <- readRDS("../Bonus Files/2022-04-12_mcmc_baseline.rds")
# mcmc5 <- readRDS("../Bonus Files/2022-03-13_mcmc_U5Age_RR.rds")
# mcmc6 <- readRDS("../Bonus Files/2022-03-31_mcmc_RR_U5RW.rds")
# mcmc5$parameters[3]
# mcmc5$diagnostics$run_time
# mcmc6$parameters[[3]]
# mcmc6$diagnostics$run_time
# mcmc$parameters[3]
# mcmc2$parameters[3]
# mcmc3$parameters[3]
# mcmc4$parameters[3]
# mcmc$diagnostics$run_time
# mcmc2$diagnostics$run_time
# mcmc3$diagnostics$run_time
# mcmc4$diagnostics$run_time

# function(params, data, misc) {
#
#   age_cats <- 17
#
#   # Split data
#   data_u5 <- data$Under_5s_deaths
#   data_5p <- data$weekly_deaths_list_5_plus
#
#   # Split parameters: under 5 rate and age category variables
#   u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:157)])
#   rel_rates <- as.numeric(params[paste0("RR",2:age_cats)])
#
#   ret<-0
#
#   for(i in 1:nrow(data_u5)){
#     ret <- ret + dpois(x = data_u5$total_deaths[i], lambda = u5_w_rates[i], log=TRUE)
#     if(data_u5$year[i] != 2020){
#       for(j in 1:(age_cats-1)){
#         ret <- ret + dpois(x = data_5p[j,i], lambda = rel_rates[j]*u5_w_rates[i], log=TRUE)
#       }
#     }
#   }
#   return(ret)
# }

# mcmc$parameters



####################################
####################################
### Leave one out cross validation:
####################################
####################################

## Format inputs
# Data
UTH_Mortality_Total <- read.csv(file = "~/Documents/Imperial/PostDoc/Zambia/covid-mortality-ascertainment/analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv")

weekly_deaths_list_5_plus <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2019-12-30", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
         week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = c(1:104))) %>%
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
  filter(date >= "2018-01-01",date < "2019-12-30", age_years !=".") %>%
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

df_params <- define_params(name = c(paste0("U_5_Rate_Week",1:nrow(Under_5s_deaths)), paste0("RR",2:age_cats)),
                           min =c(rep(0,nrow(Under_5s_deaths)), rep(0,age_cats-1)), max = c(rep(150,nrow(Under_5s_deaths)),rep(10,age_cats-1)))

r_loglike <- function(params, data, misc) {
  # browser()
  # Split data
  data_u5 <- data$Under_5s_deaths
  data_5p <- data$weekly_deaths_list_5_plus

  age_cats <- misc$age_cats
  Test_weeks <- misc$Test_weeks

  # Split parameters: under 5 rate and age category variables
  u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:nrow(data_u5))])
  rel_rates <- as.numeric(params[paste0("RR",2:age_cats)])

  ret<-0

  for(i in 1:nrow(data_u5)){
    ret <- ret + dpois(x = data_u5$total_deaths[i], lambda = u5_w_rates[i], log=TRUE)
    if(i != Test_weeks){
      for(j in 1:(age_cats-1)){
        ret <- ret + dpois(x = data_5p[j,i], lambda = rel_rates[j]*u5_w_rates[i], log=TRUE)
      }
    }
  }
  # print(ret)
  return(ret)
}

r_logprior <- function(params, misc) {

  age_cats <- misc$age_cats
  # Test_weeks <- misc$Test_weeks

  # extract parameter values
  # u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:105)])
  rel_rates <- as.numeric(params[105:120])

  # calculate log-prior
  ret <- 0

  # Add a prior for each of the age group relative risks
  # for(i in 1:length(rel_rates)){
  #   ret <- ret + dlnorm(x = rel_rates[i], meanlog = 1, sdlog = 10, log = T)
  # }

  browser()
  benchmark(#for(i in 1:length(rel_rates)){ret <- ret + dlnorm(x = rel_rates[i], meanlog = 1, sdlog = 10, log = T)},
            sum(sapply(1:length(rel_rates), function(x){dlnorm(x = rel_rates[x], meanlog = 1, sdlog = 10, log = T)})),
            replications=500)


  # return
  return(ret)
}




# library(rbenchmark)
# benchmark(as.numeric(df_params[paste0("RR",2:age_cats)]),
#           replications=50)


# Test_weeks <- 1:5

# system.time(
# mcmc <- drjacoby::run_mcmc(data = data_list,
#                            df_params = df_params,
#                            loglike = r_loglike,
#                            logprior = r_logprior,
#                            # burnin = 5e2,
#                            burnin = 1e1,
#                            samples = 1e1,
#                            pb_markdown = TRUE,
#                            chains = 5,
#                            misc = list(age_cats = age_cats,
#                                        Test_weeks = Test_weeks)
#                            )
# )

LOOCV <- readRDS("../Bonus Files/2022-04-22_mcmc_LOOCV_1.rds")

# LOOCV


# plot_par(LOOCV, show = "U_5_Rate_Week1", phase = "burnin")
# plot_par(LOOCV, show = "U_5_Rate_Week10", phase = "burnin")
# plot_par(LOOCV, show = "U_5_Rate_Week104", phase = "burnin")
# plot_par(LOOCV, show = "RR2", phase = "burnin")
# plot_par(LOOCV, show = "RR10", phase = "burnin")
# plot_par(LOOCV, show = "RR17", phase = "burnin")
#
#
# plot_par(LOOCV, show = "U_5_Rate_Week1", phase = "sampling")
# plot_par(LOOCV, show = "U_5_Rate_Week10", phase = "sampling")
# plot_par(LOOCV, show = "U_5_Rate_Week104", phase = "sampling")
#
# plot_par(LOOCV, show = "RR2", phase = "sampling")
# plot_par(LOOCV, show = "RR3", phase = "sampling")
# plot_par(LOOCV, show = "RR4", phase = "sampling")
# plot_par(LOOCV, show = "RR5", phase = "sampling")
# plot_par(LOOCV, show = "RR6", phase = "sampling")
# plot_par(LOOCV, show = "RR7", phase = "sampling")
# plot_par(LOOCV, show = "RR8", phase = "sampling")
# plot_par(LOOCV, show = "RR9", phase = "sampling")
# plot_par(LOOCV, show = "RR10", phase = "sampling")
# plot_par(LOOCV, show = "RR11", phase = "sampling")
# plot_par(LOOCV, show = "RR12", phase = "sampling")
# plot_par(LOOCV, show = "RR13", phase = "sampling")
# plot_par(LOOCV, show = "RR14", phase = "sampling")
# plot_par(LOOCV, show = "RR16", phase = "sampling")
# plot_par(LOOCV[[1]], show = "RR17", phase = "sampling")

# lapply(1:length(LOOCV), function(x){
#   table(LOOCV[[x]]$diagnostics$rhat<1.1)
# })

# min(unlist(lapply(1:length(LOOCV), function(x){
#   min(LOOCV[[x]]$diagnostics$ess)
# })))

# LOOCV[[1]]$diagnostics$ess

#### So how do I now show that this is better than the NULL model?
### So I take the predictions, and see if the mean is a better predictor.
## Start with SS

Test_weeks <- lapply(1:25, function(x){(x*4-3):(x*4)})

# LOOCV <- readRDS("../Bonus Files/2022-04-22_mcmc_LOOCV_1.rds")
## For each of the missing weeks, get the range of estimates

# as.matrix(unlist(LOOCV[[1]]$output[LOOCV[[1]]$output$phase=="sampling", paste0("RR",2:17)][1,]))

# rpois(n = 1, lambda = as.matrix(unlist(LOOCV[[1]]$output[LOOCV[[1]]$output$phase=="sampling", paste0("U_5_Rate_Week",1:100)][1,])[1]))

# UTH_Mortality_Total %>% filter(Age_gr ==1, Week_gr ==1)


LOOCV_Ests <- lapply(1:nrow(LOOCV[[1]]$output[LOOCV[[1]]$output$phase=="sampling",]), function(y){
  List_rep <- lapply(1:length(Test_weeks), function(x){
    # browser()
    Lamdas <- rbind(t(as.matrix(unlist(LOOCV[[x]]$output[LOOCV[[x]]$output$phase=="sampling", paste0("U_5_Rate_Week",Test_weeks[[x]])][y,]))),
                    as.matrix(unlist(LOOCV[[x]]$output[LOOCV[[x]]$output$phase=="sampling", paste0("RR",2:17)][y,])) %*%
      t(as.matrix(unlist(LOOCV[[x]]$output[LOOCV[[x]]$output$phase=="sampling", paste0("U_5_Rate_Week",Test_weeks[[x]])][y,]))))
  rownames(Lamdas) <- 1:17
  samples_from_lambdas <- apply(Lamdas, MARGIN = c(1,2), FUN = function(x){rpois(n = 1, lambda = x)})
  })
  do.call(cbind.data.frame, List_rep)
})

LOOCV_Ests_Av <- apply(str2str::ld2a(LOOCV_Ests), 2, rowMeans)

LOOCV_Ests_median <- apply(str2str::ld2a(LOOCV_Ests), 2, function(x){
  apply(x, 1, median)
})

LOOCV_Ests_CI_low <- apply(str2str::ld2a(LOOCV_Ests), 2, function(x){
  apply(x, 1, function(y){bayestestR::ci(y)$CI_low})
})

LOOCV_Ests_CI_high <- apply(str2str::ld2a(LOOCV_Ests), 2, function(x){
  apply(x, 1, function(y){bayestestR::ci(y)$CI_high})
})

UTH_Mortality_Total <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv") %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(date >= "2018-01-01" & date < "2019-12-30", age_years !=".", dod != ".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F),
         Week_gr = cut.Date(date, breaks = "1 week", labels = FALSE, start.on.monday = T),
         Week_gr_date_start = lubridate::floor_date(date, "week", week_start = 1)) %>%
  tidyr::complete(Age_gr, Week_gr, fill = list(Mort_deaths = 0)) %>%
  group_by(Week_gr,Age_gr,Week_gr_date_start) %>%
  summarise(Mort_deaths = length(date)) %>%
  ungroup %>%
  tidyr::complete(Week_gr, Age_gr, fill = list(Mort_deaths = 0))


Plot_Res <- melt(LOOCV_Ests_median, varnames = c("Age_gr", "Week_gr"), value.name = "Deaths_median") %>%
  mutate(Age_gr = as.numeric(gsub(Age_gr, pattern = "RR", replacement = "")),
         Week_gr = as.numeric(gsub(x = Week_gr, "U_5_Rate_Week", ""))) %>%
  merge(melt(LOOCV_Ests_CI_low, varnames = c("Age_gr", "Week_gr"), value.name = "Deaths_low_CI") %>%
          mutate(Age_gr = as.numeric(Age_gr),
                 Week_gr = as.numeric(gsub(x = Week_gr, "U_5_Rate_Week", "")))) %>%
  merge(melt(LOOCV_Ests_CI_high, varnames = c("Age_gr", "Week_gr"), value.name = "Deaths_high_CI") %>%
          mutate(Age_gr = as.numeric(Age_gr),
                 Week_gr = as.numeric(gsub(x = Week_gr, "U_5_Rate_Week", "")))) %>%
  merge(UTH_Mortality_Total %>% select(Week_gr, Week_gr_date_start) %>% unique())




Age_groups.labs <- c(paste0("Age: ", c("0-4","5-9","10-14","15-29","20-24","25-29",
                                       "30-34","35-39","40-44","45-49","50-54","55-59",
                                       "60-64","65-69","70-74","75-79","80+")))
names(Age_groups.labs) <- 1:17

merge(Plot_Res, UTH_Mortality_Total) %>% filter(Age_gr ==1) %>%



UTH_Mortality_Total %>% filter(Age_gr ==1)

p2 <- ggplot(data = UTH_Mortality_Total, aes(x = Week_gr_date_start, y = Mort_deaths)) +
  geom_point(aes(alpha = "Mortuary deaths")) +
  geom_line(data = Plot_Res, aes(x = Week_gr_date_start, y = Deaths_median, alpha = "MCMC ests. mean"), col = "black", linetype = 2, inherit.aes = F) +
  geom_ribbon(data = Plot_Res, aes(x = Week_gr_date_start, ymin = Deaths_low_CI, ymax = Deaths_high_CI, alpha = "Post pred dist median +/- 95% CI"), fill ="darkred", inherit.aes = F) +
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
                   values = c("Mortuary deaths" = 1, "Post pred dist median +/- 95% CI" = 0.5),
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
  ggpubr::theme_pubr(legend = c(5/6,0.07)) +
  theme(legend.text = element_text(size=12))

pdf("analysis/figures/41_01_2018-2019_All_cause_mortality_mcmc_ests.pdf", width = 12, height = 15)
p2
dev.off()


Age_groups.labs <- c(paste0(c("0-4","5-9","10-14","15-29","20-24","25-29",
                              "30-34","35-39","40-44","45-49","50-54","55-59",
                              "60-64","65-69","70-74","75-79","80+")))
names(Age_groups.labs) <- 1:17


p3 <- ggplot(data = merge(UTH_Mortality_Total,Plot_Res %>% filter(Age_gr !=1)), #%>%
               # mutate(Age_gr_lab = ifelse(Age_gr %in% 1:16, paste0(as.numeric(Age_gr)*5-5, "-", as.numeric(Age_gr)*5-1),
                                          # "80+")),
             aes(x = Mort_deaths, y = Deaths_median, col = as.factor(Age_gr))) +
  geom_point() +
  # geom_point(aes(alpha = "Mortuary deaths")) +
  geom_errorbar(aes(ymin = Deaths_low_CI, ymax = Deaths_high_CI)) +
  theme_minimal()+
  geom_abline(slope=1, intercept=0, linetype = 2)+
  viridis::scale_color_viridis(discrete = T, name = "Age group",
                               breaks = 1:17,
                               labels= Age_groups.labs) +
  xlab("Mortuary deaths") + ylab("Predictions")

  # geom_line(data = Plot_Res, aes(x = Week_gr_date_start, y = Deaths_median, alpha = "MCMC ests. mean"), col = "black", linetype = 2, inherit.aes = F) +
  # geom_ribbon(data = Plot_Res, aes(x = Week_gr_date_start, ymin = Deaths_low_CI, ymax = Deaths_high_CI, alpha = "Post pred dist median +/- 95% CI"), fill ="darkred", inherit.aes = F) +
  # facet_wrap(~Age_gr, ncol = 3, scales = "free_y", labeller = labeller(Age_gr = Age_groups.labs)) +
  # xlab("Weeks (2018-2019)") + ylab("All cause deaths") +
  # scale_alpha_manual(name = NULL,
                   # breaks = c(),
                   # values = c("Mortuary deaths" = 1, "Post pred dist median +/- 95% CI" = 0.5),
                   # guide = guide_legend(override.aes = list(
                   #   linetype =c("blank","solid"),
                   #   color=c("black","black"),
                   #   shape = c(16, NA),
                   #   fill = c(NA, "darkred"),
                   #   alpha = c(1,0.5)))) +
  # )
  # scale_alpha_manual(name = NULL,
  #                   # breaks = c("MCMC ests. median +/- 95% CI"),
  #                   values = c("MCMC ests. median +/- 95% CI" = 0.5)) +
  # ggpubr::theme_pubr(legend = c(5/6,0.07)) +
  # theme(legend.text = element_text(size=12))

pdf("analysis/figures/41_02_2018-2019_All_cause_mortality_against_predictions.pdf")
p3
dev.off()


tiff("analysis/figures/41_02_2018-2019_All_cause_mortality_against_predictions.tiff", res = 300, units = "in", height = 7, width = 7)
p3
dev.off()














LOOCV_error <-
  unlist(lapply(1:5000, FUN = function(x){
    sum(sapply(1:length(LOOCV), function(y){
      # browser()
      sum((as.matrix(unlist(LOOCV[[y]]$output[LOOCV[[1]]$output$phase=="sampling",paste0("RR",2:17)][x,])) %*% t(as.matrix(unlist(LOOCV[[y]]$output[LOOCV[[1]]$output$phase=="sampling",paste0("U_5_Rate_Week",Test_weeks[[y]])][x,])))-
           weekly_deaths_list_5_plus[,Test_weeks[[y]]])^2)/(16*4)
  }))/25
}))

## Compare with the model error when using the means.
## I think - in order to do this comparatiely, I need to run the same process but instead of the mcmc, just take a mean of the remaining values.

LOOCV_error_null <- sum((t(apply(t(sapply(1:length(Test_weeks), function(x){
  # Take the data, remove the test weeks, take the means, then use these as the values
  rowMeans(weekly_deaths_list_5_plus[,1:100][,-Test_weeks[[x]]])
})),2, rep, each = 4)) - weekly_deaths_list_5_plus[,1:100])^2)/(16*100)

hist(LOOCV_error, xlim = c(25,45))
abline(v = LOOCV_error_null, col = "red", lty = 2)

## What are the mean values for each age group?
Mean_Deaths_Age <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2019-12-30", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
         week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = c(1:104))) %>%
  group_by(Age_gr, week) %>%
  summarise(total_deaths = length(date)) %>% ungroup() %>%
  complete(., Age_gr, week, fill = list(total_deaths = 0)) %>%
  arrange(Age_gr, week) %>%
  group_by(Age_gr) %>%
  summarise(mean_deaths = mean(total_deaths))






# Samples_age_ests <- readRDS("../Bonus Files/2022-04-12_mcmc_baseline.rds")

rowMeans((Samples_age_ests$output %>% filter(phase=="sampling"))[,paste0("U_5_Rate_Week",1:5)])
rowMeans((Samples_age_ests$output %>% filter(phase=="sampling"))[,paste0("U_5_Rate_Week",6:10)])
rowMeans((Samples_age_ests$output %>% filter(phase=="sampling"))[,paste0("U_5_Rate_Week",6:10)])

Testing <-UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2020-01-06", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
         week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = c(1:105))) %>%
  group_by(Age_gr, week) %>%
  summarise(total_deaths = length(date)) %>% ungroup() %>%
  complete(., Age_gr, week, fill = list(total_deaths = 0)) %>%
  arrange(Age_gr, week) %>%
  # filter(Age_gr !=1) %>%
  ungroup() %>%
  pivot_wider(names_from = c(week), values_from = total_deaths, values_fill = 0) %>%
  tibble::column_to_rownames(var="Age_gr")

Av_df <- sapply(c(1:21), function(x){
  # browser()
  rowMeans(Testing[,(x*5-4):(x*5)])
}) %>% reshape2::melt(varnames = c("Age_gr","Week_gr"), value.name = "av_deaths")


library(ggplot2)
library(viridis)
p1 <- ggplot(Av_df, aes(x = Week_gr, y = av_deaths, group = Age_gr, color = as.factor(Age_gr))) + geom_line() +
  ylab("Average deaths") + xlab("4-Week group") +
  # scale_color_discrete() +
  viridis::scale_color_viridis(discrete=T) + labs(color='Age group') +
  ggpubr::theme_pubr(legend = "right")
p2 <- ggplot(Av_df, aes(x = Age_gr, y = av_deaths, group = Week_gr)) + geom_line()
cowplot::plot_grid(p1,p2)


#####################################################
#####################################################
#####################################################
Samples_age_ests <- readRDS("../Bonus Files/2022-04-12_mcmc_baseline.rds")

Av_samples <- sapply(c(1:25), function(x){
  # browser()
  rowMeans((Samples_age_ests$output %>% filter(phase=="sampling"))[,paste0("U_5_Rate_Week",(x*4-3):(x*4))])
}) %>% reshape2::melt(varnames = c("Rep","Week_gr"), value.name = "av_deaths")

RRs <- (Samples_age_ests$output %>% filter(phase=="sampling"))[,paste0("RR",2:17)] %>%
  as.matrix() %>%
  reshape2::melt(varnames = c("Rep","Age_gr"), value.name = "RR") %>%
  mutate(Age_gr = as.numeric(gsub(pattern = "RR", replacement = "", x = Age_gr)))

Testing <-UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2020-01-06", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = c(1:17)),
         week = cut.Date(date, breaks = "1 week", start.on.monday = T, labels = c(1:105))) %>%
  group_by(Age_gr, week) %>%
  summarise(total_deaths = length(date)) %>% ungroup() %>%
  complete(., Age_gr, week, fill = list(total_deaths = 0)) %>%
  arrange(Age_gr, week) %>%
  # filter(Age_gr !=1) %>%
  ungroup() %>%
  pivot_wider(names_from = c(week), values_from = total_deaths, values_fill = 0) %>%
  tibble::column_to_rownames(var="Age_gr")

Av_df <- sapply(c(1:25), function(x){
  # browser()
  rowMeans(Testing[,(x*4-3):(x*4)])
}) %>% reshape2::melt(varnames = c("Age_gr","Week_gr"), value.name = "av_deaths")


Av_df <- Av_df %>% filter(Age_gr !=1, Week_gr !=1) %>% group_by(Age_gr) %>% summarise(av_deaths_age = mean(av_deaths)) %>%
  merge(Av_df) %>% merge(FullEsts)

FullEsts <- merge(Av_samples, RRs) %>%
  mutate(Age_Week_gr_deaths = RR*av_deaths) %>%
  group_by(Week_gr, Age_gr) %>%
  summarise(median = median(Age_Week_gr_deaths),
            CI_high = bayestestR::ci(Age_Week_gr_deaths)$CI_high,
            CI_low = bayestestR::ci(Age_Week_gr_deaths)$CI_low) %>%
  merge(Av_df)

ggplot(FullEsts, aes(x = Week_gr, y = median)) +
  geom_line() +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.3) +
  facet_wrap(~Age_gr, scales = "free_y") +
  geom_point(data = Av_df %>% filter(Age_gr !=1), aes(x = Week_gr, y = av_deaths), size = 0.5) +
  ggpubr::theme_pubr(legend = "right") +
  geom_hline(data = Av_df %>% filter(Age_gr !=1, Week_gr !=1) %>% group_by(Age_gr) %>% summarise(av_deaths = mean(av_deaths)), aes(yintercept = av_deaths))


p3 <- ggplot(Av_df, aes(x = Week_gr, y = av_deaths-av_deaths_age, group = Age_gr, color = as.factor(Age_gr))) + geom_line() +
  ylab("Average deaths") + xlab("4-Week group") +
  # scale_color_discrete() +
  viridis::scale_color_viridis(discrete=T) + labs(color='Age group') +
  ggpubr::theme_pubr(legend = "right")

p4 <- ggplot(Av_df, aes(x = Week_gr, y = av_deaths-median, group = Age_gr, color = as.factor(Age_gr))) + geom_line() +
  ylab("Average deaths") + xlab("4-Week group") +
  # scale_color_discrete() +
  viridis::scale_color_viridis(discrete=T) + labs(color='Age group') +
  ggpubr::theme_pubr(legend = "right")


# Av_samples %>% group_by(Age_gr, Week_gr) %>%
  # summarise(median = median())

# For each of the sets, take hte remaining weeks and agerage across





## 50 seconds to run burn = 10, samples = 10
## 273 seconds to run burn = 100, samples = 10
## xxx seconds to run burn = 500, samples = 1000

## What I can do is run this on the cluster, taking samples from each one

# Option 1: All the weeks
# list(1:105)
# Option 2: Sets of 5 weeks
# list(matrix(1:105, nrow = 5))
# lapply(1:25, function(x){(x*4-3):(x*4)})
# Option 3: Overlapping sets
# lapply(1:101, function(x){x:x+4)})


## Plot the missing samples against the predictions
## Plot the data

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

####################################
pdf(file = "analysis/figures/41_03_mcmc_results_2018_2019.pdf", height = 30, width = 15)
# p1
p2
dev.off()
####################################

p3 <- ggplot() +
  geom_point(data = weekly_deaths_all_ages, aes(x = as.numeric(Age_gr), y =total_deaths), size = 0.5) +
  geom_errorbar(data = mcmc_death_ests, aes(x = as.numeric(Age_gr), ymin = `1`, ymax = `2`)) +
  xlab("Age group") + #ylab("AG1 Deaths")
  facet_wrap(~as.numeric(week), ncol = 8)

####################################
pdf(file = "analysis/figures/41_04_mcmc_results_2018_2019_week.pdf", height = 30, width = 15)
# p1
p3 + ggtitle("MCMC fit to the backgound mortality by week")
dev.off()
####################################



####################################
####################################
####################################
####################################





























#### Parameters

age_cats <- nrow(weekly_deaths_list_5_plus)


# define parameters for each of the age_rates
df_params <- define_params(name = c(paste0("U5_base_week_rate_",1:nrow(Under_5s_deaths)),
                                    paste0("U5_week_RE_",1:nrow(Under_5s_deaths)),
                                    "U5_week_RE_rate_sigma",
                                    paste0("U5_Fixed_Effect_",1:16),
                                    paste0("Rel_rates_age", 2:age_cats),
                           min =c(rep(0,nrow(Under_5s_deaths)), # Base
                                  rep(-100,nrow(Under_5s_deaths)), # RE
                                  0, # RE sd
                                  rep(0, 16),
                                  rep(0, age_cat-1)),
                           max =c(rep(150,nrow(Under_5s_deaths)), # Base
                                  rep(100,nrow(Under_5s_deaths)), # RE
                                  20, # RE sd
                                  rep(50, 16),
                                  rep(10, age_cat-1))))

# define log-likelihood function
r_loglike <- function(params, data, misc) { # Split data

  browser()


  data_u5 <- data$Under_5s_deaths
  data_5p <- data$weekly_deaths_list_5_plus # Split parameters: under 5 rate and age category variables
  ## somehow get an indicator for the switch to the 2020 model
  start_2020_week<-misc$start_2020_week
  # u5_w_rates <- as.numeric(params[paste0("U_5_Rate_Week",1:157)])
  ### now have 'normal mortality patterns' characterised in terms of a mean base_u5_rate and a variance of sigma_u5
  base_u5_rate<-as.numeric(params["base_u5_rate"])
  sigma_u5<-as.numeric(params["sigma_u5"])
  ### have parameters for a random effect
  u5_random_effects<-as.numeric(params[paste0("U_5_random_effect",1:'full_data')])
  ## get u5_rates if typical
  u5_w_rates <-base_u5_rate+u5_random_effects
  ## get distribution of pandemic change in under 5
  FE_change_in_rates_pandemic=as.numeric(params[paste0("pandemic_change",1:'weeks in 2020')])
  excess_deaths_rate<- 'matrix of rates in above 5 by pandemic week'
  rel_rates <- as.numeric(params[paste0("RR",2:age_cats)])
  ret<-0

  ## first we have pre 2020 model as before but now with a RE on under 5
  for(i in 1:(start_2020_week-1)){
    ## get likelihood of each random effect
    ret <- ret + dnorm(x = u5_random_effects[i], mean = 0, sd = sigma_u5)
    # ret <- ret + dpois(x = data_u5$total_deaths[i], lambda = u5_w_rates[i], log=TRUE)
    ## same as before but U5_w_rate now encompasses RE
    ret <- ret + dpois(x = data_u5$total_deaths[i], lambda = u5_w_rates[i], log=TRUE)
    for(j in 1:(age_cats-1)){
      ret <- ret + dpois(x = data_5p[j,i], lambda = rel_rates[j]*u5_w_rates[i], log=TRUE)
    }
  }

  ### Now have a seperate model for our under 5 model with a change we assume is not due to covid
  ### model the over 5s with the same non-covid change and an additional covid-specific change
  for(i in start_2020_week:end){
    ## get likelihood of each random effect
    ret <- ret + dnorm(x = u5_random_effects[i], mean = 0, sd = sigma_u5)
    # ret <- ret + dpois(x = data_u5$total_deaths[i], lambda = u5_w_rates[i], log=TRUE)
    ###likelihood of under 5s now changes accordign to a fixed effect each week
    ret <- ret + dpois(x = data_u5$total_deaths[i], lambda = u5_w_rates[i]*FE_change_in_rates_pandemic[i-start_2020_week+1], log=TRUE)
    for(j in 1:(age_cats-1)){
      ### change in the over 5s also changes according to this fixed effect and another excess-mortality FE
      ret <- ret + dpois(x = data_5p[j,i], lambda = rel_rates[j]*u5_w_rates[i]*FE_change_in_rates_pandemic[i-start_2020_week+1]*excess_deaths_rate[i-start_2020_week+1,j], log=TRUE)
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

  # Priors for Base U5
  # Priors for U5 RE
  # Prior for U5 RE sigma
  # Prior for FE change in rates during pandemic

  # Priors for age group relative risks
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
                           chains = 5,
                           misc = list(start_2020_week = 140))

# saveRDS(mcmc, "../Bonus Files/2022-03-31_mcmc_RR_U5RW.rds")
mcmc <- readRDS("../Bonus Files/2022-04-07_mcmc_baseline.rds")
