## Testing the new likelihood function:
# devtools::install_github("mrc-ide/cma")
devtools::install()
devtools::load_all()
library(squire)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(cma)
# devtools::load_all(".")
# bring my data in:
PMP_data_age <- readRDS("analysis/data/Code-generated-data/00_06_Mortuary_post-mortem_age.rds") %>% select(date,Age_group,sampled_deaths,CT_45_Either,CT_40_Either)
Mort_data_age <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age.rds")
Combined <- merge(Mort_data_age, PMP_data_age, by.x = c("date","Age_group"), all = T) %>% replace_na(list(total_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0)) %>%
  complete(Age_group, date, fill = list(total_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0))

# Group by weeks
Combined_Weeks <- Combined[-nrow(Combined),] %>% mutate(Week_gr = cut.Date(x = date, breaks = "weeks", labels = F)) %>%
  group_by(Week_gr,Age_group) %>% summarise(date = head(date,1),
                                  total_deaths = sum(total_deaths),
                                  sampled_deaths = sum(sampled_deaths),
                                  CT_45_Either = sum(CT_45_Either),
                                  CT_40_Either = sum(CT_40_Either)) %>%
  ungroup %>% complete(Age_group, nesting(Week_gr,date), fill = list(total_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0))

Combined_Weeks[Combined_Weeks$Week_gr==4, "total_deaths"] <-
  Combined_Weeks[Combined_Weeks$Week_gr %in% c(3,5), c("Age_group","Week_gr","total_deaths")] %>% group_by(Age_group) %>%
  summarise(total_deaths = round(mean(total_deaths))) %>% select(total_deaths)


# Missing_rows <- which(table(Combined_Weeks[,c("Week_gr","Age_group")]) ==0, arr.ind = T)
PMP_data <- readRDS("analysis/data/Code-generated-data/00_06_Mortuary_post-mortem.rds") %>% select(date,sampled_deaths,CT_45_Either,CT_40_Either)
data <- PMP_data %>% select(date, CT_45_Either) %>% rename(deaths = CT_45_Either)

Lus_Pop_Age <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
Weighted_Durs_Hosp <- readRDS("analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
Lancet_Data <- readRDS("analysis/data/Code-generated-data/00_10_Lancet_Data.rds")
deaths_age <- readRDS("analysis/data/Code-generated-data/00_03_IFR_values_Brazeau.rds")


IFR_mat <- readRDS("analysis/data/Code-generated-data/IFR_mat_Ints2.rds")

## Now using the Slopes and Intercepts, I need to calculate specific IFR by age groups
Age_groups <- seq(2.5,82.5, by = 5)
IFR_Age_var_slope_int <- t(apply(IFR_mat, 1, function(x){
  exp(Age_groups * x["Slope_abs"] + x["Int_abs"])
}))


# t(IFR_Age_var_slope_int)/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp)
# max(IFR_Age_var_slope_int[81,]/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp))
IFR_vals_1 <- apply(IFR_Age_var_slope_int, MARGIN = 1, FUN = function(x){x/(100*squire::parameters_explicit_SEEIR("Zambia")$prob_hosp)
  return(max(x/(100*squire::parameters_explicit_SEEIR("Zambia")$prob_hosp)))})>1
IFR_Age_var_slope_int_fil <- IFR_Age_var_slope_int[!IFR_vals_1,]
IFR_mat_fil <- IFR_mat[!IFR_vals_1,]


fit_l_IFR_slope <- lapply(1:nrow(IFR_Age_var_slope_int_fil), function(x){

  prob_death_tot <- IFR_Age_var_slope_int_fil[x,]/(100*squire::parameters_explicit_SEEIR("Zambia")$prob_hosp)
  # browser()

  print(x)
  # Sys.setenv(SQUIRE_PARALLEL_DEBUG = "TRUE")
  Sys.setenv(SQUIRE_PARALLEL_DEBUG = "")
  fit_BMJ_mort <- fit_spline_rt(data = data,
                                combined_data = Combined_Weeks,
                                country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                                population = Lus_Pop_Age,
                                reporting_fraction = 1,
                                # reporting_fraction_bounds = c(0.25,0.1,1),
                                n_mcmc = 1000,
                                replicates = 100,
                                rw_duration = 14,
                                hosp_beds = 1e10,
                                icu_beds = 1e10,
                                prob_severe = rep(0,17),
                                prob_non_severe_death_treatment = deaths_age$Prob_death_when_hospitalised,
                                # prob_non_severe_death_treatment = prob_death_tot,
                                dur_get_ox_survive = Weighted_Durs_Hosp$Surv_Dur_Weighted,
                                dur_get_ox_die = Weighted_Durs_Hosp$Death_Dur_Weighted,
                                dur_R = Inf
                                # sero_df_start = as.Date(c("2020-07-04")),#,"2020-07-18")),
                                # sero_df_end = as.Date(c("2020-07-27")),#,"2020-08-10")),
                                # sero_df_pos = as.numeric(as.integer(c(Lancet_Data$Sero_prev["val"]/100*Lancet_Data$Sero_prev["n"]))),#, 0.106*1952))), # See Table 2
                                # sero_df_samples = Lancet_Data$Sero_prev["n"],#,1952),
                                # pcr_df_start = as.Date(c("2020-07-04")),
                                # pcr_df_end = as.Date(c("2020-07-27")),
                                # pcr_df_pos = as.integer(c(Lancet_Data$PCR_prev["val"]/100*Lancet_Data$PCR_prev["n"])), # See Table 2
                                # pcr_df_samples = Lancet_Data$PCR_prev["n"]
                                # IFR_slope_bounds = c(1,0.5,1.5),
                                # IFR_multiplier_bounds = c(1,0.4,1.5)
  )
  })

# SavedBit <-fit_BMJ_mort


p1 <- plot(fit_BMJ_mort, particle_fit = T) + theme(legend.position = "none")

sero_pcr <- seroprev_df(fit_BMJ_mort)
ser_pcr_2 <- Summ_sero_pcr_data(sero_pcr)

p2 <- ggplot(ser_pcr_2, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=7.6)) +
  geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0)) +
  ylab(paste0("PCR %")) + xlab("Date")

p3 <- ggplot(ser_pcr_2, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=2.1)) +
  geom_errorbar(aes(ymin=1.1,ymax=3.1,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0)) +

  # geom_point(aes(x= as.Date("2020-07-29"),y=10.6), color="darkblue") +
  # geom_errorbar(aes(ymin=7.3,ymax=13.9,x=as.Date("2020-07-29"), width=10), color="darkblue") +
  # geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=10.6, height=0), color="darkblue") +
  ylab(paste0("Sero+ve %"))  + xlab("Date")

cowplot::plot_grid(p1,p3,p2,nrow = 1)

## From these graphs, I can see that the seroprev reahed about 13%, while the pcr reached about 6%

# I want to compare the model results with the true results by week

Deaths <- squire::format_output(fit_BMJ_mort, c("D"), date_0 = max(fit_BMJ_mort$pmcmc_results$inputs$data$date), reduce_age = F) %>%
  filter(date %in% c(unique(Combined_Weeks$date)-1, max(Combined_Weeks$date))) %>%
  group_by(replicate,age_group) %>%
  mutate(D = y,
         D_end = c(0,head(y,-1))) %>%
  mutate(D_weekly = D-D_end) %>%
  filter(!row_number() == 1) %>%
  group_by(age_group, replicate) %>%
  mutate(WeekNo = 1:16)

Deaths_by_age <- Deaths %>% group_by(replicate, age_group) %>%
  summarise(Deaths = sum(D_weekly)) %>% ungroup() %>%
  group_by(age_group) %>%
  summarise(mean_Deaths = mean(Deaths),
            max_Deaths = max(Deaths),
            min_Deaths = min(Deaths))

Deaths_by_week <- Deaths %>% group_by(replicate, date) %>%
  summarise(Deaths = sum(D_weekly)) %>% ungroup() %>%
  group_by(date) %>%
  summarise(mean_Deaths = mean(Deaths),
            max_Deaths = max(Deaths),
            min_Deaths = min(Deaths))

## I want to get average infections broken down by week and age group
date_0 <- max(fit_BMJ_mort$pmcmc_results$inputs$data$date)
# Deaths_data <- squire::format_output(fit_BMJ_mort, c("D"), date_0 = max(fit_BMJ_mort$pmcmc_results$inputs$data$date), reduce_age = T)
inf <- squire::format_output(fit_BMJ_mort, c("S"), date_0 = max(fit_BMJ_mort$pmcmc_results$inputs$data$date), reduce_age = F) %>%
  na.omit() %>%
  mutate(S = as.integer(.data$y)) %>%
  group_by(replicate, age_group) %>%
  mutate(infections = c(0, diff(max(.data$S)-.data$S))) %>%
  select(replicate, t, date, .data$S, .data$infections)
# select(replicate, age_group, t, date, .data$S, .data$infections, D)

inf <- inf %>%
  group_by(replicate,age_group) %>%
  na.omit() %>%
  mutate(pcr_positive = roll_func(.data$infections, pcr_det),
         pcr_perc = .data$pcr_positive/max(.data$S,na.rm = TRUE)) %>%
  ungroup %>%
  filter(date %in% c(unique(Combined_Weeks$date)+3)) %>%
  group_by(age_group, replicate) %>%
  mutate(WeekNo = 1:16)

pcr_dates <- inf %>% group_by(date,age_group) %>%
  summarise(mean_pcr = mean(pcr_perc),
            max_pcr = max(pcr_perc),
            min_pcr = min(pcr_perc))



Deaths_inf <- merge(x = inf, y = Deaths, by = c("age_group","replicate","WeekNo")) %>%
  select(age_group, replicate, WeekNo, date.y, pcr_perc, D_weekly)

Non_cov_deaths_est <- Combined_Weeks %>% rename(age_group = "Age_group", WeekNo = "Week_gr") %>%
  merge(x = ., y = Deaths_inf, by = c("age_group","WeekNo")) %>%
  select(-date.y) %>%
  mutate(Mod_cd = D_weekly) %>%
  mutate(Mod_cd_Lus = Mod_cd*0.8) %>%
  mutate(tot_mort_deaths = total_deaths) %>%
  mutate(Mod_ncd = (total_deaths - Mod_cd_Lus)) %>% # non-covid deaths
  mutate(Mod_pos_ncd = Mod_ncd*pcr_perc) %>%
  mutate(Mod_tot_pos_mort = Mod_cd_Lus+Mod_pos_ncd)

Model_Data_Comp <- Non_cov_deaths_est %>% group_by(age_group, replicate) %>%
  summarise(Mod_cd = sum(Mod_cd),
            Mod_cd_Lus = sum(Mod_cd_Lus),
            tot_mort_deaths = sum(tot_mort_deaths),
            Mod_ncd = sum(Mod_ncd),
            Mod_pos_ncd = sum(Mod_pos_ncd),
            Mod_tot_pos_mort = sum(Mod_tot_pos_mort),
            sampled_deaths = sum(sampled_deaths),
            CT_45_Either = sum(CT_45_Either),
            # Mod_cov_deaths = sum(Mod_cov_deaths),
            ) %>%
  ungroup() %>% group_by(age_group) %>%
  summarise(Mod_cd = mean(Mod_cd),
            Mod_cd_Lus = mean(Mod_cd_Lus),
            tot_mort_deaths = mean(tot_mort_deaths),
            Mod_ncd = mean(Mod_ncd),
            Mod_pos_ncd = mean(Mod_pos_ncd),
            Mod_tot_pos_mort = mean(Mod_tot_pos_mort),
            sampled_deaths = mean(sampled_deaths),
            CT_45_Either = mean(CT_45_Either)
  ) %>%
  mutate(Mod_prop_pos = Mod_tot_pos_mort/tot_mort_deaths,
         Mort_prop_pos = CT_45_Either/sampled_deaths,
         Mod_Pos_Test = Mod_prop_pos*sampled_deaths)

colSums(Model_Data_Comp)

## But what about the break-down by age and week? Are there weeks that should be impossible?
Model_Data_Comp_Week <- Non_cov_deaths_est %>% group_by(age_group, WeekNo, replicate) %>%
  summarise(Mod_cd = sum(Mod_cd),
            Mod_cd_Lus = sum(Mod_cd_Lus),
            tot_mort_deaths = sum(tot_mort_deaths),
            Mod_ncd = sum(Mod_ncd),
            Mod_pos_ncd = sum(Mod_pos_ncd),
            Mod_tot_pos_mort = sum(Mod_tot_pos_mort),
            sampled_deaths = sum(sampled_deaths),
            CT_45_Either = sum(CT_45_Either),
            # Mod_cov_deaths = sum(Mod_cov_deaths),
  ) %>%
  ungroup() %>% group_by(age_group, WeekNo) %>%
  summarise(Mod_cd = mean(Mod_cd),
            Mod_cd_Lus = mean(Mod_cd_Lus),
            tot_mort_deaths = mean(tot_mort_deaths),
            Mod_ncd = mean(Mod_ncd),
            Mod_pos_ncd = mean(Mod_pos_ncd),
            Mod_tot_pos_mort = mean(Mod_tot_pos_mort),
            sampled_deaths = mean(sampled_deaths),
            CT_45_Either = mean(CT_45_Either)
  ) %>%
  mutate(Mod_prop_pos = Mod_tot_pos_mort/tot_mort_deaths,
         Mort_prop_pos = CT_45_Either/sampled_deaths,
         Mod_Pos_Test = Mod_prop_pos*sampled_deaths)


inf %>% group_by(age_group, WeekNo) %>%
  summarise(pcr_perc = mean(pcr_perc))


Combined_Weeks %>% group_by(Age_group) %>%
  summarise(total_deaths = sum(total_deaths),
            sampled_deaths = sum(sampled_deaths),
            CT_45_Either = sum(CT_45_Either))

Non_cov_deaths_est %>% filter(age_group ==1) group_by()


  mutate(Combined_Weeks)
nrow(inf)
nrow(Deaths)




# pcr_perc <- inf %>% filter(date %in% c(unique(Combined_Weeks$date)+3)) %>%
#   # select(replicate, age_group, date, pcr_perc) %>%
#   select(replicate, date, pcr_perc) %>%
#   mutate(Deaths = Deaths_by_Week$Deaths_weeks) %>% ungroup() %>% group_by(replicate) %>%
#   mutate(Total_deaths = Ttd$total_deaths) %>%
#   mutate(Bonus_deaths = (0.8Total_deaths - Deaths)*pcr_perc)





# index <- squire:::odin_index(fit_BMJ_mort$model)
# plot(rowMeans(fit_BMJ_mort$output[nrow(fit_BMJ_mort$output),index$D,1]), type = "l")
# Data_Est <- Combined_Weeks %>% group_by(Age_group) %>%
#   summarise(Deaths = sum(CT_45_Either),
#             total_deaths = sum(total_deaths),
#             sampled_deaths = sum(sampled_deaths)) %>%
#   mutate(Model_Est = rowMeans(fit_BMJ_mort$output[nrow(fit_BMJ_mort$output),index$D,])/ total_deaths * sampled_deaths)

p4 <- ggplot(data = Model_Data_Comp, aes(x = age_group, y = CT_45_Either)) + geom_point() +
  geom_line(aes(y = Mod_Pos_Test), linetype = 2) +
  geom_line(aes(y = sampled_deaths * Mod_cd_Lus/tot_mort_deaths)) +
  xlab("Age Group") + ylab("Tot +ve Sampled")

# Dates_Needed <- as.Date(rownames(fit_BMJ_mort$output)) %in% c(unique(Combined_Weeks$date)-1,tail(rownames(fit_BMJ_mort$output),1))
# Data_Est_Weeks <- Combined_Weeks %>% group_by(Week_gr) %>%
#   summarise(Deaths = sum(CT_45_Either),
#             total_deaths = sum(total_deaths),
#             sampled_deaths = sum(sampled_deaths)) %>%
#   mutate(Model_Est = rowMeans(apply(fit_BMJ_mort$output[Dates_Needed,index$D,], 3, function(x){diff(rowSums(x))}))/ total_deaths * sampled_deaths)
#
# p5 <- ggplot(data = Data_Est_Weeks, aes(x = Week_gr, y = Deaths)) + geom_point() +
#   geom_line(aes(y = Model_Est)) +xlab("Week")
#
# cowplot::plot_grid(p4,p5)
#
# ### So how many deaths could be attributed to pcr prevalence? How many non-covid deaths could be attributed to covid?
# # Based on the results, can we estimate numbers in each age group that were thing?
#
# ## I do it the same way I did before So I need total deaths for each replicate
# Dates_Listed<- as.Date(rownames(fit_BMJ_mort$output)) %in% c(unique(Combined_Weeks$date)-1,tail(rownames(fit_BMJ_mort$output),1))
# Dates_Listed_inf<- as.Date(rownames(fit_BMJ_mort$output)) %in% c(unique(Combined_Weeks$date)+3)
#
# # Mod_Deaths_Age <- sapply(1:(dim(fit_BMJ_mort$output)[3]), function(x){
# #   # browser()
# #   apply(fit_BMJ_mort$output[Dates_Listed,index$D,x], 2, diff)
# # })
#
# Mod_Deaths_Age <- apply(fit_BMJ_mort$output[Dates_Listed,index$D,1], 2, diff)
# colnames(Mod_Deaths_Age) <- 1:17
# Mod_Deaths_Age <- Mod_Deaths_Age %>%
#   melt(value.name = "Ds", varnames= c("Week_num","Age_gr")) # 16 time periods, 17 age groups.
#
#
#
#
# ## Infections:
# date_0 <- max(fit_BMJ_mort$pmcmc_results$inputs$data$date)
# Deaths_data <- squire::format_output(fit_BMJ_mort, c("D"), date_0 = max(fit_BMJ_mort$pmcmc_results$inputs$data$date), reduce_age = T)
#
# inf <- squire::format_output(fit_BMJ_mort, c("S"), date_0 = max(fit_BMJ_mort$pmcmc_results$inputs$data$date), reduce_age = T) %>%
#   mutate(D = Deaths_data$y) %>%
#   na.omit() %>%
#   mutate(S = as.integer(.data$y)) %>%
#   # group_by(replicate,age_group) %>%
#   group_by(replicate) %>%
#   mutate(infections = c(0, diff(max(.data$S)-.data$S))) %>%
#   select(replicate, t, date, .data$S, .data$infections, D)
# # select(replicate, age_group, t, date, .data$S, .data$infections, D)
#
#
# inf <- inf %>%
#   group_by(replicate) %>%
#   # group_by(replicate,age_group) %>%
#   na.omit() %>%
#   mutate(pcr_positive = roll_func(.data$infections, pcr_det),
#          pcr_perc = .data$pcr_positive/max(.data$S,na.rm = TRUE)) %>%
#   ungroup
#
# Deaths_by_Week <- inf %>% #group_by(replicate, age_group) %>%
#   group_by(replicate) %>%
#   filter(date >="2020-06-15" & date<="2020-10-05") %>%
#   mutate(WeekNo = cut.Date(x = date, breaks = "weeks", labels = F)) %>%
#   # group_by(replicate, age_group, WeekNo) %>%
#   group_by(replicate, WeekNo) %>%
#   summarise(Deaths_weeks = sum(D))
#
# Ttd <- Combined_Weeks %>% group_by(Week_gr) %>% summarise(total_deaths = sum(total_deaths)) %>% select(total_deaths)
# pcr_perc <- inf %>% filter(date %in% c(unique(Combined_Weeks$date)+3)) %>%
#   # select(replicate, age_group, date, pcr_perc) %>%
#   select(replicate, date, pcr_perc) %>%
#   mutate(Deaths = Deaths_by_Week$Deaths_weeks) %>% ungroup() %>% group_by(replicate) %>%
#   mutate(Total_deaths = Ttd$total_deaths) %>%
#   mutate(Bonus_deaths = (0.8Total_deaths - Deaths)*pcr_perc)
#
#
#
# sero_pcr %>% group_by(replicate) %>% filter(date %in% c(unique(Combined_Weeks$date)+3)) %>%
#   select(replicate,date, pcr_perc)







## Each of those pcr_percentages needs to be multiplied by the Total deaths - MOdelled deaths

# frac_mort <- 0.8 # Mortuary captures 80% of deaths in Lusaka
# Deaths_cov_mortuary <- frac_mort*Mod_Deaths_Age$Ds # Estimated covid deaths in the mortuary by age and week
# Deaths_ncov_mort <- Combined_Weeks$total_deaths- Deaths_cov_mortuary # Estimated mortuary deaths not from covid
# covid_pos_mort <- Deaths_cov_mortuary + pcr_perc$pcr_pos*Deaths_ncov_mort #
#
#
# Combined_Weeks %>% data.frame(Pos_Samples = CT_45_Either,
#                               Samples = sampled_deaths,
#                               Mod_Pos_deaths = round(covid_pos_mort),
#                               TotD_min_ModD = round(Comb_data$total_deaths - covid_pos_mort)
# ) %>%
#   mutate(TotD_min_ModD = ifelse(TotD_min_ModD<0, Samples-Pos_Samples, TotD_min_ModD)) %>%
#   mutate(Mod_Pos_deaths = ifelse(Pos_Samples>Mod_Pos_deaths, Pos_Samples,Mod_Pos_deaths)) %>%
#   mutate(TotD_min_ModD = ifelse((Samples - Pos_Samples)>TotD_min_ModD, (Samples - Pos_Samples),TotD_min_ModD)) %>%
#   mutate(ll = sapply(1:length(Pos_Samples), function(x){
#     dhyper(x = Pos_Samples[x], m =  Mod_Pos_deaths[x], n = TotD_min_ModD[x], k = Samples[x], log = T)
#   }))
#
#
#
# ## Then I need
#
#
#
#
#
# Mort_data_age
# Combined_Weeks %>% group_by(Week_gr) %>% summarise(deaths = sum(total_deaths))
# ## I'm going to bump up the numbers in week f
# Combined_Weeks[Combined_Weeks$Week_gr==1,]
# Combined_Weeks[Combined_Weeks$Week_gr==2,]
# Combined_Weeks[Combined_Weeks$Week_gr==3,]
# Combined_Weeks[Combined_Weeks$Week_gr==4,] <-
#   Combined_Weeks[Combined_Weeks$Week_gr %in% c(3,5),] %>% group_by(Age_group) %>%
#   summarise(Week_gr = 4,
#             date = as.Date("2020-07-06"),
#             total_deaths = mean(total_deaths),
#             sampled_deaths = mean(sampled_deaths),
#             CT_45_Either = mean(CT_45_Either),
#             CT_40_Either = mean(CT_40_Either)) ## This is a problem
# Combined_Weeks[Combined_Weeks$Week_gr==5,]
# Combined_Weeks[Combined_Weeks$Week_gr==6,]
# Combined_Weeks[Combined_Weeks$Week_gr==7,]
# Combined_Weeks[Combined_Weeks$Week_gr==8,]
# Combined_Weeks[Combined_Weeks$Week_gr==9,]
# Combined_Weeks[Combined_Weeks$Week_gr==10,]
# Combined_Weeks[Combined_Weeks$Week_gr==11,]
# Combined_Weeks[Combined_Weeks$Week_gr==12,]
# Combined_Weeks[Combined_Weeks$Week_gr==13,]
# Combined_Weeks[Combined_Weeks$Week_gr==14,]
# Combined_Weeks[Combined_Weeks$Week_gr==15,]
# Combined_Weeks[Combined_Weeks$Week_gr==16,]
