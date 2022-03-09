###########################
##### Data Management #####
###########################
rm(list = ls())
library(squire)
library(tidyverse)
devtools::load_all()

###################################################
### 1. Official Lusaka Province/District Death Data
###################################################
df_Off_Lu <- read.csv(file = "analysis/data/raw/00_official_reports_covid_zambia.csv") %>%
  mutate(date = as.Date(Date), deaths = Mort_deaths)

## Filter deaths for Lusaka PROVINCE until November 2020
df_Off_Lu_Prov <- df_Off_Lu %>%
  filter(Province=="Lusaka" & date < "2020-11-01") %>%
  group_by(date) %>% summarise(deaths = sum(deaths)) %>%
  select(date, deaths) %>% na.omit()

# Add missing dates with 0 deaths
date_list_P <- seq(min(df_Off_Lu_Prov$date), max(df_Off_Lu_Prov$date), by = 1)
missing_dates_P <- date_list_P[!date_list_P %in% df_Off_Lu_Prov$date]
df_Off_Lu_Prov <- add_row(df_Off_Lu_Prov, date = missing_dates_P, deaths = 0) %>% arrange(date)
# Save
saveRDS(object = df_Off_Lu_Prov, file = "analysis/data/Code-generated-data/00_01_Lusaka_Prov_Deaths_Official.rds")

## Filter deaths for Lusaka DISTRICT until November 2020
# Include the 6 deaths prior to May 21st 2020 where no district was given.
df_Off_Lu_Dist <- df_Off_Lu %>%
  filter(District=="Lusaka" & date < "2020-11-01" | Province =="Lusaka" & date < "2020-05-21") %>%
  group_by(date) %>% summarise(deaths = sum(deaths)) %>%
  select(date, deaths) %>% na.omit()

# Add missing dates with 0 deaths
date_list_D <- seq(min(df_Off_Lu_Dist$date), max(df_Off_Lu_Dist$date), by = 1)
missing_dates_D <- date_list_D[!date_list_D %in% df_Off_Lu_Dist$date]
df_Off_Lu_Dist <- add_row(df_Off_Lu_Dist, date = missing_dates_D, deaths = 0) %>% arrange(date)
# Save
saveRDS(object = df_Off_Lu_Dist, file = "analysis/data/Code-generated-data/00_01_Lusaka_Dist_Deaths_Official.rds")
###################################################
###################################################




##############################################
### 2. Lusaka Population structure
##############################################
# 2020 Age distribution for Lusaka Province from opendataforafrica (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
pop_st_lu_prov <- c(549475,448008,379841,339600,317148,305521,271318,232188,172365,125531,75157,51883,35587,22219,14960,8570,10812)
saveRDS(pop_st_lu_prov, "analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020_opendataforafrica.rds")

# 2020 Age distribution for Lusaka District  opendataforafrica (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
pop_st_lu_dist <- c(439632,356146,302389,273911,265588,260473,230783,195211,142218,100552,58537,26654,40513,15878,10201,5695,7314)
saveRDS(pop_st_lu_dist, "analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
# sum(pop_st_lu)

## Population size for Lusaka Province
# 2010: 2191225, 2222812 or 2238569 (https://en.wikipedia.org/wiki/Lusaka_Province)
# 2014: 2669249 (https://zambia.opendataforafrica.org/apps/atlas/Lusaka)
# 2018: 3186336 (https://en.wikipedia.org/wiki/Provinces_of_Zambia)
# 2019: 3308438 (https://www.citypopulation.de/en/zambia/admin/05__lusaka/)
# 2019: 3308400 (https://en.wikipedia.org/wiki/Lusaka, https://www.citypopulation.de/en/zambia/cities/)
# 2020: 3.36e6 (https://knoema.com/atlas/Zambia/ranks/Population)
# 2020: 3360183 (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
# 2021: 3484394 (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
## Other Pop sizes
# Lusaka District sizes
# 2000: 1084703
# 2010: 1747152 (https://en.wikipedia.org/wiki/Lusaka_Province)
# 2018: 2.5e6  (https://en.wikipedia.org/wiki/Lusaka)
# 2019: 2627716 (https://en.wikipedia.org/wiki/Lusaka_Province)
# 2019: 3.3e6 (https://en.wikipedia.org/wiki/Lusaka)
# 2020: 2731696 (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
# 2021: 2905993 (https://worldpopulationreview.com/world-cities/lusaka-population)

# 2020 estimate from opendataforafrica
# pop_tot <- 3360183

# Adjust for population structure
# pop_gpza <- as.integer(get_population("Zambia")$n * pop_tot/sum(get_population("Zambia")$n)) # Province of Lusaka

# 2010 Age distribution Lusaka Province (https://www.citypopulation.de/en/zambia/admin/05__lusaka/)
# 2010 Age distribution Lusaka District (https://www.citypopulation.de/en/zambia/wards/admin/0504__lusaka/)

##############################################
##############################################



###################################################
### 3. Zambia Total IFR by age/probability of death
###################################################

# Get parameters (the parameters used are generic)
Zmb_p <-parameters_explicit_SEEIR("Zambia")
# IFR_Age
# Calculate the probability of death by weighting probabilities of death given treatment by probability of severe
prob_death_tot <- Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment +
  (1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_treatment
# Calculate IFR for each age group: multiply probability of death of a case by probability hospitalised
IFR_Age <- 100*(Zmb_p$prob_hosp * prob_death_tot)
IFR_Age_gr <- seq(2.5, 82.5, by = 5)
IFR_Coefs <- lm(log(IFR_Age) ~ seq(2.5, 82.5, by = 5))$coefficients
# Save
# saveRDS(prob_death_tot, "analysis/data/Code-generated-data/00_03_Tot_Prob_Death_By_Age_Zam.rds")
saveRDS(list(Prob_death_when_hospitalised = prob_death_tot, IFR_Age_gr = IFR_Age_gr, IFR_Age = IFR_Age, IFR_Coefs = IFR_Coefs), "analysis/data/Code-generated-data/00_03_IFR_values_Brazeau.rds")

## Draw IFR grid
# pop_st_lu <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020_opendataforafrica.rds")
pop_st_lu <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")

# 9x9 matrix of IFR values:
IFR_vec <- seq(0.2,1, by = 0.2) # IFR vector
IFR_vec_lsc <- sort(unique(c(IFR_vec,1/IFR_vec)))
# IFR_vec <- seq(0.2,1.8, by = 0.2) # IFR vector
Slope_vec <- seq(0.2,1, by = 0.2) # IFR vector
Slope_vec_lsc <- sort(unique(c(Slope_vec,1/Slope_vec)))
# Slope_vec <- seq(0.2, 1.8,by = 0.2) # Slope vector

# Expand IFR/slope
IFR_mat <- expand.grid("IFR_x" = IFR_vec_lsc, "Slope_x" = Slope_vec_lsc)
IFR_mat <- IFR_mat %>% mutate(IFR_abs = IFR_x * sum(IFR_Age * pop_st_lu/sum(pop_st_lu)),
                              Slope_abs = Slope_x * IFR_Coefs[2])

# Apply the function to calculate intercept values for each IFR gradient combination
IFR_mat$Int_abs <- apply(IFR_mat, 1, function(x){
  Int_calc(IFR = x["IFR_abs"], Slope = x["Slope_abs"], Age_grs = IFR_Age_gr, Pop_str = pop_st_lu)
})
saveRDS(IFR_mat, "analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")
IFR_mat <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")
##

# Translate this matrix into probability of death: using the Slopes and Intercepts, I need to calculate specific IFR by age groups
IFR_Age_var_slope_int <- t(apply(IFR_mat, 1, function(x){
  exp(IFR_Age_gr * x["Slope_abs"] + x["Int_abs"])
}))

# Make sure that none of the probabilities are higher than 1
IFR_vals_1 <- apply(IFR_Age_var_slope_int, MARGIN = 1, FUN = function(x){x/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp)
  return(max(x/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp)))})>1
IFR_Age_var_slope_int_fil <- IFR_Age_var_slope_int[!IFR_vals_1,]
IFR_mat_fil_coefficients <- IFR_mat[!IFR_vals_1,]
rownames(IFR_mat_fil) <- 1:nrow(IFR_mat_fil)
Prob_Death_Matrix <- as.list(data.frame(apply(IFR_Age_var_slope_int_fil,1, function(x){x/(100*squire::parameters_explicit_SEEIR("Zambia")$prob_hosp)})))
Prob_Index_Vector <- (1:length(IFR_vals_1))[!IFR_vals_1]

cbind(readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")[readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds"),],1:72)
# saveRDS(Prob_Death_Matrix, "analysis/data/Code-generated-data/00_03_Prob_Death_Matrix_log_sc.rds")
# saveRDS(Prob_Index_Vector, "analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds")

length(readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds"))
readRDS("analysis/data/Code-generated-data/00_03_Prob_Death_Matrix.rds")
readRDS("analysis/data/Code-generated-data/IFR_mat_Ints2.rds")
IFR_mat[1,]
IFR_Age_var_slope_int[1,]
Prob_Death_Matrix[1]

# Calculate total IFR:
# IFR_Sum_Zam <- sum(IFR_Age * get_population("Zambia")$n/sum(get_population("Zambia")$n))
# IFR_Sum_Lus <- sum(IFR_Age * pop_st_lu_dist/sum(pop_st_lu_dist))
# saveRDS(list(IFR_Sum_Zam = IFR_Sum_Zam, IFR_Sum_Lus = IFR_Sum_Lus), "analysis/data/Code-generated-data/00_03c_Total_IFR_Zambia_Lusaka.rds")
##############################################
# sum(100*(Zmb_p$prob_hosp * prob_death_tot)* get_population("United Kingdom")$n/sum(get_population("United Kingdom")$n))
# sum(100*(Zmb_p$prob_hosp * prob_death_tot)* get_population("Zambia")$n/sum(get_population("Zambia")$n))
# sum(IFR_Age * get_population("Madagascar")$n/sum(get_population("Madagascar")$n))
# sum(IFR_Age* get_population("Nicaragua")$n/sum(get_population("Nicaragua")$n))
# sum(IFR_Age* get_population("Grenada")$n/sum(get_population("Grenada")$n))
# sum(IFR_Age* get_population("Malta")$n/sum(get_population("Malta")$n))

###############################################
###############################################



##############################################
### 4. Zambia Weighted durations for death/survive given grouped infections
##############################################
Zmb_p <-parameters_explicit_SEEIR("Zambia")

# Weight duration times for each age group.
Death_Dur_WeightedByAge <-
  Zmb_p$dur_get_mv_die *
  Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment/
  (Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment + (1-Zmb_p$prob_severe)*Zmb_p$prob_non_severe_death_treatment) +

  Zmb_p$dur_get_ox_die *
  (1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_treatment/
  (Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment + (1-Zmb_p$prob_severe)*Zmb_p$prob_non_severe_death_treatment)


Surv_Dur_WeightedByAge <-
  Zmb_p$dur_get_mv_survive *
  Zmb_p$prob_severe*(1-Zmb_p$prob_severe_death_treatment)/
  (Zmb_p$prob_severe*(1-Zmb_p$prob_severe_death_treatment) + (1-Zmb_p$prob_severe)*(1-Zmb_p$prob_non_severe_death_treatment)) +

  Zmb_p$dur_get_ox_survive *
  (1-Zmb_p$prob_severe) * (1-Zmb_p$prob_non_severe_death_treatment)/
  (Zmb_p$prob_severe*(1-Zmb_p$prob_severe_death_treatment) + (1-Zmb_p$prob_severe)*(1-Zmb_p$prob_non_severe_death_treatment))

## Weight durations per age group according to age prevalence in Lusaka and probability they need treatment by age
# pop_st_lu_dist <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_dist_Pop_Struc_2020.rds")
Death_Dur_Weighted <- sum(Death_Dur_WeightedByAge * Zmb_p$prob_hosp/sum(Zmb_p$prob_hosp))
Surv_Dur_Weighted <- sum(Surv_Dur_WeightedByAge * Zmb_p$prob_hosp/sum(Zmb_p$prob_hosp))
saveRDS(list(Death_Dur_Weighted = Death_Dur_Weighted, Surv_Dur_Weighted = Surv_Dur_Weighted), "analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
##############################################
##############################################





##############################################
### 5. BMJ report data
##############################################

# Get the proportion of deaths from BMJ
# BMJ_Deaths_by_Age_Covid_Neg <- c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2)
# BMJ_Deaths_by_Age_Covid_Pos <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# BMJ_Deaths_by_Age_Covid_Pos2 <- c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# BMJ_Deaths_by_Age_Tot <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2)


## Time series data
BMJ_data_report <- data.frame(
  "Time_Period" = c("W24-W25","W26-W27","W28-W29","W30-W31","W32-W33","W34-W35","W36-W37","W38-W39"),
  "TotSamDeaths" = c(51,25,22,26,39,40,53,37),
  "CovSamDeaths_Strict" = c(2,4,14,13,11,8,2,4),
  "CovSamDeaths" = c(2,4,14,13,11,8,2,4) + c(0,1,2,1,3,0,4,1))
saveRDS(BMJ_data_report, "analysis/data/Code-generated-data/00_05_BMJ_report_data_time.rds")

## Estimating total deaths in Lusaka PROVINCE from BMJ report
pop_st_lu_prov <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020_opendataforafrica.rds")

BMJ_data_report_lus_prov_ests <- BMJ_data_report %>%
  mutate(TotEstCovDeathsLus_10x = CovSamDeaths*10/0.8,
         Sampling = c(5,5,5,5,3,3,3*1/10+2*9/10,2),  # Sampling was 1 in 5 for June, July, 1 in 3 for Aug, 1 in 2 for Sep.
         Cap = c(1,1,1,1,1,1,1/10,0)) %>% # Daily tests were capped at 5 or 6 in June, July and Aug. No cap in Sep.
  mutate(TotEstCovDeathsLus_Low_Est = CovSamDeaths*Sampling/0.8,
         TotEstCovDeathsLus_High_Est = (CovSamDeaths*Sampling + CovSamDeaths/TotSamDeaths * Cap*(3600-sum(TotSamDeaths*Sampling))/(sum(Cap)))/0.8,
         TotEstCovDeathsLus_High_Est_Strict = (CovSamDeaths_Strict*Sampling + CovSamDeaths_Strict/TotSamDeaths * Cap*(3600-sum(TotSamDeaths*Sampling))/(sum(Cap)))/0.8,
         q1_10x = TotEstCovDeathsLus_10x/sum(pop_st_lu_prov),
         q1_High_Est = TotEstCovDeathsLus_High_Est/sum(pop_st_lu_prov),
         q1_High_Est_Strict = TotEstCovDeathsLus_High_Est_Strict/sum(pop_st_lu_prov),
         Mort_deaths_High_Est = TotSamDeaths * Sampling + Cap*(3600-sum(TotSamDeaths*Sampling))/(sum(Cap)))
saveRDS(BMJ_data_report_lus_prov_ests, "analysis/data/Code-generated-data/00_05_BMJ_report_data_time_lus_prov_ests.rds")

# Plot Data estimates for
# p1 <- ggplot(BMJ_data_report_lus_prov_ests, aes(x = Time_Period, y = CovSamDeaths/TotSamDeaths)) + geom_point(aes(colour = "CT<45")) +
#   xlab("Date") + ylab("COVID-19 +ve deaths in mortuary sample (%)") +
#   geom_point(aes(y = CovSamDeaths_Strict/TotSamDeaths, colour = "CT<40")) +
#   labs(color = "Test Result") +
#   scale_color_manual(values = c("CT<40" = "black", "CT<45" = "blue"))

# p2 <- ggplot(BMJ_data_report_lus_prov_ests, aes(x = Time_Period, y = Mort_deaths_High_Est)) + geom_point(color = "black") +
#   xlab("Date") + ylab("Total estimated deaths in mortuary") +
#   ylim(c(0,700))

# p3 <- ggplot(BMJ_data_report_lus_prov_ests, aes(x = Time_Period, y = Mort_deaths_High_Est * CovSamDeaths/TotSamDeaths /0.8)) + geom_point(aes(colour = "CT<45")) +
#   xlab("Date") + ylab("COVID-19 +ve deaths in mortuary sample") +
#   geom_point(aes(y = Mort_deaths_High_Est * CovSamDeaths_Strict/TotSamDeaths /0.8, colour = "CT<40")) +
#   labs(color = "Test Result") +
#   scale_color_manual(values = c("CT<40" = "black", "CT<45" = "blue"))

# Explanation of High Estimation:
# MinDeaths <- sum(BMJdata$TotSamDeaths*BMJdataTotEsts$Sampling) # Min number of deaths, estimated from sampling:
# UnaccountedDeaths <- 3600-MinDeaths # Number of deaths unaccounted for
# Unacc_Deaths_Attrib <- BMJdataTotEsts$Cap*UnaccountedDeaths/(sum(BMJdataTotEsts$Cap)) # Attribute unaccounted deaths into time periods according to days that had caps
# cvd_death_prev <- BMJdata$CovSamDeaths/BMJdata$TotSamDeaths # Covid death prevalence:
# Min_cvd_deaths <- BMJdata$CovSamDeaths*BMJdataTotEsts$Sampling # Min cvd deaths from sampling
# (Min_cvd_deaths + cvd_death_prev * Unacc_Deaths_Attrib)/0.8 # Add min cvd deaths with estimated

# Break down BMJ data by day:
df_off_fil <- df_Off_Lu_Prov %>% filter(date >= "2020-06-08" & date <= "2020-09-27")
df_off_fil <- df_off_fil %>%
  mutate(TimeFrame = cut.Date(date, breaks = as.Date(c("2020-06-08","2020-06-22","2020-07-06","2020-07-20","2020-08-03","2020-08-17","2020-08-31","2020-09-14","2020-09-28")), labels = 1:8, start.on.monday = T))
#
BMJ_data_report_lus_prov_ests <- BMJ_data_report_lus_prov_ests %>% mutate(TimePeriod = 1:8) # Assign week labels to BMJ data
df_off_fil_gr <- df_off_fil %>% group_by(TimeFrame) %>%
  summarise(FrameDeaths = sum(deaths)) # Get the official sum of deaths in each timeframe

# Take the total number of estimated deaths in Lusaka
# Deduct the ones that were officially reported, and distribute the other deaths evenly
# 10x, lower limit and higher limit.
df_off_fil <- df_off_fil %>% mutate(Est_Deaths_BMJ_10 = rep((BMJ_data_report_lus_prov_ests$TotEstCovDeathsLus_10x - df_off_fil_gr$FrameDeaths)/14, each = 14) + deaths,
                                    Est_Deaths_BMJ_Sam_LE = rep((BMJ_data_report_lus_prov_ests$TotEstCovDeathsLus_Low_Est - df_off_fil_gr$FrameDeaths)/14, each = 14) + deaths,
                                    Est_Deaths_BMJ_Sam_HE = rep((BMJ_data_report_lus_prov_ests$TotEstCovDeathsLus_High_Est - df_off_fil_gr$FrameDeaths)/14, each = 14) + deaths,
                                    Est_Deaths_BMJ_Sam_HE_Strict = rep((BMJ_data_report_lus_prov_ests$TotEstCovDeathsLus_High_Est_Strict - df_off_fil_gr$FrameDeaths)/14, each = 14) + deaths) %>%
  mutate(q1_10x = Est_Deaths_BMJ_10/sum(pop_st_lu_prov),
         q1_HighEst = Est_Deaths_BMJ_Sam_HE/sum(pop_st_lu_prov),
         q1_HighEst_Strict = Est_Deaths_BMJ_Sam_HE_Strict/sum(pop_st_lu_prov))
saveRDS(object = df_off_fil, file = "analysis/data/Code-generated-data/00_05_BMJ_report_data_DailyEsts.rds")

# ggplot(df_off_fil, aes(x = date, y = Est_Deaths_BMJ_Sam_HE/0.8)) + geom_point(aes(colour = "CT<45")) +
#   xlab("Date") + ylab("Estimated COVID-19 +ve deaths in Lusaka (dummy)") +
#   geom_point(aes(y = Est_Deaths_BMJ_Sam_HE_Strict/0.8, colour = "CT<40")) +
#   labs(color = "Test Result") +
#   scale_color_manual(values = c("CT<40" = "black", "CT<45" = "blue")) +
#   ylim(c(0,76.5))

##############################################
##############################################




##############################################
### 6. Mortuary Post-mortem data
##############################################

### Full Data set 1
UTH_Mortuary_COVID_Sampling <- read.csv(file = "analysis/data/raw/BMJ_Mwananyanda_PostMortem_UTH/covid_results.csv")
# Filter death with no date (1)
df_MPM <- UTH_Mortuary_COVID_Sampling %>% filter(deceased_date != ".") %>% rename(date = deceased_date) %>% mutate(date = as.Date(date))

# date_list_BMJ_1 <- seq(min(df_BMJ$date), max(df_BMJ$date), by = 1)
# missing_dates_BMJ_1 <- date_list_BMJ_1[!date_list_BMJ_1 %in% df_BMJ$date] # Add missing dates with 0 deaths
# df_BMJ <- add_row(df_BMJ, date = missing_dates_BMJ_1, Mort_deaths = 0) %>% arrange(date)

df_MPM <- df_MPM %>% filter(date >= "2020-06-15" & date <= "2020-11-01") %>%
  mutate(Test1_45 = ifelse(N1_CT_1 != ".", T, F),
         Test2_45 = ifelse(N2_CT_1 != ".", T, F),
         Test1_40 = ifelse(N1_CT_1 != "." & N1_CT_1<40, T, F),
         Test2_40 = ifelse(N2_CT_1 != "." & N2_CT_1<40, T, F)) %>%
  mutate(Either_Test_Pos = ifelse(Test1_45==T | Test2_45==T, T, F),
         Both_Tests_Pos = ifelse(Test1_45==T & Test2_45==T, T, F),
         Either_Test_Pos_40 = ifelse(Test1_40==T | Test2_40==T, T, F),
         Both_Tests_Pos_40 = ifelse(Test1_40==T & Test2_40==T, T, F))

df_MPM_Date <- df_MPM %>%
  group_by(date) %>% summarise(Samples = length(N1_CT_1),
                               CT_45_Either = sum(Either_Test_Pos, na.rm=T),
                               CT_45_Both = sum(Both_Tests_Pos, na.rm=T),
                               CT_40_Either = sum(Either_Test_Pos_40, na.rm=T),
                               CT_40_Both = sum(Both_Tests_Pos_40, na.rm=T)) %>%
  mutate(PCR_Prev_inc = CT_45_Either/Samples,
         PCR_Prev_str = CT_40_Either/Samples) %>%
  tidyr::complete(date, fill = list(Samples = 0, CT_45_Either = 0, CT_45_Both = 0, CT_40_Either = 0, CT_40_Both = 0))


df_MPM_Date_age <- df_MPM %>% mutate(Age_gr = cut(as.numeric(age_death), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  group_by(date,Age_gr) %>% summarise(Samples = length(N1_CT_1),
                               CT_45_Either = sum(Either_Test_Pos, na.rm=T),
                               CT_45_Both = sum(Both_Tests_Pos, na.rm=T),
                               CT_40_Either = sum(Either_Test_Pos_40, na.rm=T),
                               CT_40_Both = sum(Both_Tests_Pos_40, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(date, Age_gr, fill = list(Samples = 0, CT_45_Either = 0, CT_45_Both = 0, CT_40_Either = 0, CT_40_Both = 0))

# date_list_Mort_sam <- seq(min(df_MPM$date), max(df_MPM$date), by = 1)
# missing_dates_Mort_sam <- date_list_Mort_sam[!date_list_Mort_sam %in% df_MPM$date] # Add missing dates with 0 deaths

# df_MPM <- add_row(df_MPM, date = missing_dates_Mort_sam,
#                   sampled_deaths = 0,
#                   CT_45_Either = 0,
#                   CT_45_Both = 0,
#                   CT_40_Either = 0,
#                   CT_40_Both = 0,
#                   PCR_Prev_inc = 0,
#                   PCR_Prev_str = 0
#                   ) %>% arrange(date)


df_MPM_Week <- df_MPM_Date %>% select(date, Samples, CT_45_Either, CT_40_Either) %>%
  mutate(Week_gr = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  group_by(Week_gr) %>%
  summarise(Samples = sum(Samples),
            CT_45_Either = sum(CT_45_Either),
            CT_40_Either = sum(CT_40_Either)
  ) %>%
  mutate(PCR_Prev_inc = CT_45_Either/Samples,
         PCR_Prev_str = CT_40_Either/Samples
  )

saveRDS(object = df_MPM, file = "analysis/data/Code-generated-data/00_06_Mortuary_post-mortem.rds")
saveRDS(object = df_MPM_Date_age, file = "analysis/data/Code-generated-data/00_06_Mortuary_post-mortem_age.rds")


# 0.6*(0.14+0.5+0.1-0.14*0.25 - 0.25*0.1)/(0.14*0.25 + 0.1*0.25 +0.25*0.1)
# (0.14*0.6 + 0.6*0.1 - 0.14*0.6*0.25 + 2*0.6*0.25*0.25 - 0.6*0.25*0.1)/(2*0.14*0.25 + 2*0.25*0.1)
# (0.14*0.6 + 0.5*0.6 + 0.6*0.1 - 0.14*0.6*0.25 + 2*0.6*0.25*0.25 - 0.6*0.25*0.1)/(2*0.14*0.25 + 2*0.5*0.25 + 2*0.25*0.1)


p1 <- ggplot(df_MPM, aes(x = date, y = Samples)) + geom_point(colour = "black") +
  xlab("Date") + ylab("Sampled deaths in mortuary") + ylim(0,8)
  # geom_point(aes(y = PCR_Prev_str, colour = "CT<40")) +
  # labs(color = "Test Result") +
  # scale_color_manual(values = c("CT<40" = "black", "CT<45" = "blue"))

# p2 <- ggplot(df_MPM, aes(x = date, y = CT_45_Either)) + geom_point(aes(colour = "CT<45")) +
#   xlab("Date") + ylab("+ve tests in sample") +
#   geom_point(aes(y = CT_40_Either, colour = "CT<40")) +
#   labs(color = "Test Result") +
#   scale_color_manual(values = c("CT<40" = "black", "CT<45" = "blue"))  + ylim(0,8)


# ggplot(df_MPM, aes(x = date, y = PCR_Prev_inc)) + geom_point(aes(colour = "CT<45")) +
#   xlab("Week") + ylab("COVID-19 +ve deaths in mortuary sample (%)") +
#   geom_point(aes(y = PCR_Prev_str, colour = "CT<40")) +
#   labs(color = "Test Result") +
#   scale_color_manual(values = c("CT<40" = "black", "CT<45" = "blue"))


# ggplot(df_MPM_Week, aes(x = week, y = PCR_Prev_inc)) + geom_point(aes(colour = "CT<45")) +
#   xlab("Week") + ylab("COVID-19 +ve deaths in mortuary sample (%)") +
#   geom_point(aes(y = PCR_Prev_str, colour = "CT<40")) +
#   labs(color = "Test Result") +
#   scale_color_manual(values = c("CT<40" = "black", "CT<45" = "blue"))

##############################################
##############################################




##############################################
### 7. Mortuary data
##############################################

UTH_Mortality_Total <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv")
UTH_Mortality_Total <- UTH_Mortality_Total %>% mutate(date = as.Date(dod, "%m/%d/%y"))

UTH_deaths_by_age <- UTH_Mortality_Total %>% filter(dod != ".") %>%
  filter(date >= "2020-06-15" & date < "2020-10-05", age_years !=".") %>%
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  group_by(date,Age_gr) %>% summarise(Mort_deaths = length(date)) %>%
  ungroup %>%
  tidyr::complete(date, Age_gr, fill = list(Mort_deaths = 0))
  # filter(date >= "2020-06-15" & date <= "2020-11-01")

UTH_deaths_by_date <- UTH_Mortality_Total %>% filter(dod != ".") %>%
  group_by(date) %>% summarise(Mort_deaths = length(date)) %>%
  # filter(date >= "2020-06-15" & date <= "2020-11-01")
  filter(date >= "2020-06-15" & date <= "2020-10-05")


date_list_Mort <- seq(min(UTH_deaths_by_date$date), max(UTH_deaths_by_date$date), by = 1)
missing_dates_Mort <- date_list_Mort[!date_list_Mort %in% UTH_deaths_by_date$date] # Add missing dates with 0 deaths

UTH_deaths_by_date <- add_row(UTH_deaths_by_date, date = missing_dates_Mort, Mort_deaths = 0) %>% arrange(date)

UTH_deaths_by_week <- UTH_deaths_by_date %>%
  mutate(Week_gr = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  group_by(Week_gr) %>%
  summarise(Mort_deaths = sum(Mort_deaths))

p2 <- ggplot(UTH_deaths_by_date, aes(x = date, y = Mort_deaths)) + geom_point(colour = "black") +
  xlab("Date") + ylab("Total deaths registered in UTH mortuary") +
  ylim(0,100)

# ggplot(UTH_deaths_by_week, aes(x = week, y = Mort_deaths)) + geom_point(colour = "black") +
#   xlab("Week") + ylab("Total deaths registered in UTH mortuary") +
#   ylim(0,500)

saveRDS(object = UTH_deaths_by_date, file = "analysis/data/Code-generated-data/00_07_Mortuary_data.rds")
saveRDS(object = UTH_deaths_by_age, file = "analysis/data/Code-generated-data/00_07_Mortuary_data_age.rds")

UTH_Mortality_Weeks <- UTH_Mortality_Total %>% filter(date>="2020-06-15" & date<"2020-10-05") %>%
  mutate(Week_gr = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  group_by(Week_gr) %>%
  summarise(deaths = length(date))

saveRDS(object = UTH_Mortality_Weeks, file = "analysis/data/Code-generated-data/00_07_Mortuary_data_weeks.rds")


##############################################
##############################################




##############################################
### 8. Combined Mortuary, post-mortem data
##############################################

Combined_Dates <- merge(df_MPM_Date_age, UTH_deaths_by_age, all = T) %>% replace_na(list(Mort_deaths = 0, Samples = 0, CT_45_Either = 0, CT_40_Either = 0)) %>%
  complete(Age_gr, date, fill = list(Mort_deaths = 0, Samples = 0, CT_45_Either = 0, CT_40_Either = 0))

Combined_Weeks <- Combined_Dates %>% mutate(Week_gr = cut.Date(x = date, breaks = "weeks", labels = F)) %>%
  group_by(Week_gr, Age_gr) %>% summarise(date = head(date,1),
                                            Mort_deaths = sum(Mort_deaths),
                                            Samples = sum(Samples),
                                            CT_45_Either = sum(CT_45_Either),
                                            CT_40_Either = sum(CT_40_Either)) %>%
  ungroup %>% tidyr::complete(Age_gr, nesting(Week_gr,date), fill = list(Mort_deaths = 0, Samples = 0, CT_45_Either = 0, CT_40_Either = 0))

Combined_Weeks[Combined_Weeks$Week_gr==4, "Mort_deaths"] <-
  Combined_Weeks[Combined_Weeks$Week_gr %in% c(3,5), c("Age_gr","Week_gr","Mort_deaths")] %>% group_by(Age_gr) %>%
  summarise(Mort_deaths = round(mean(Mort_deaths))) %>% select(Mort_deaths)

saveRDS(object = Combined_Weeks, file = "analysis/data/Code-generated-data/00_08_Combined_mortuary_postmortem_data.rds")



# MPM_mortuary_combined <- merge(df_MPM, UTH_deaths_by_date, by.x = "date", all = T) %>%
#   mutate(#Est_Deaths_inc = Mort_deaths * PCR_Prev_inc,
#          Est_Deaths_str = Mort_deaths * PCR_Prev_str,
#          Sampling_effort = sampled_deaths/Mort_deaths) %>%
#   filter(date >= "2020-06-15" & date <= "2020-10-02") %>%
#   select(date, Est_Deaths_inc, Est_Deaths_str, Sampling_effort, Mort_deaths, sampled_deaths) %>%
#   replace_na(list(Est_Deaths_inc = 0, Est_Deaths_str = 0, Sampling_effort = 0, sampled_deaths = 0)) #%>%
#   # mutate(Sampling_effort = ifelse(is.infinite(Sampling_effort), 10, Sampling_effort))
#
# MPM_mortuary_combined_week <- MPM_mortuary_combined %>%
#   mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
#   group_by(week) %>%
#   summarise(Est_Deaths_inc = sum(Est_Deaths_inc),
#             Est_Deaths_str = sum(Est_Deaths_str))
#
# p3 <- ggplot(MPM_mortuary_combined, aes(x = date, y = Sampling_effort)) + geom_point(colour = "black") +
#   xlab("Date") + ylab("Deaths sampled (%)")
#
# # ggplot(MPM_mortuary_combined_week, aes(x = week, y = Est_Deaths_inc/0.8)) + geom_point(aes(colour = "CT<45")) +
# #   xlab("Week") + ylab("Estimated COVID-19 +ve deaths in Lusaka") +
# #   geom_point(aes(y = Est_Deaths_str/0.8, colour = "CT<40")) +
# #   labs(color = "Test Result") +
# #   scale_color_manual(values = c("CT<40" = "black", "CT<45" = "blue"))
#
# saveRDS(object = MPM_mortuary_combined, file = "analysis/data/Code-generated-data/00_08_Mortuary_Post-mortem_combined.rds")
#
# Combined <- merge(Mort_data_age, PMP_data_age, by.x = c("date","Age_gr"), all = T) %>% replace_na(list(Mort_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0)) %>%
#   complete(Age_gr, date, fill = list(Mort_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0))
#
# # Group by weeks
# Combined_Weeks <- Combined[-nrow(Combined),] %>% mutate(Week_gr = cut.Date(x = date, breaks = "weeks", labels = F)) %>%
#   group_by(Week_gr,Age_gr) %>% summarise(date = head(date,1),
#                                             Mort_deaths = sum(Mort_deaths),
#                                             sampled_deaths = sum(sampled_deaths),
#                                             CT_45_Either = sum(CT_45_Either),
#                                             CT_40_Either = sum(CT_40_Either)) %>%
#   ungroup %>% tidyr::complete(Age_gr, nesting(Week_gr,date), fill = list(Mort_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0))
#
# Combined_Weeks[Combined_Weeks$Week_gr==4, "Mort_deaths"] <-
#   Combined_Weeks[Combined_Weeks$Week_gr %in% c(3,5), c("Age_gr","Week_gr","Mort_deaths")] %>% group_by(Age_gr) %>%
#   summarise(Mort_deaths = round(mean(Mort_deaths))) %>% select(Mort_deaths)


##############################################

##############################################
##############################################

### Grouping Official data by BMJ fortnights ###

## Group
# BMJ_Fortnight_dates <- c("2020-06-15","2020-06-29","2020-07-13","2020-07-27","2020-08-10","2020-08-24","2020-09-07","2020-09-21","2020-10-05")

# df_Off_Lu_Prov_BMJ_Dates <- df_Off_Lu_Prov %>% filter(date >= "2020-06-15" & date < "2020-10-05") %>%
#   mutate(TimeFrame = cut.Date(date, breaks = as.Date(BMJ_Fortnight_dates), labels = 1:8, start.on.monday = T)) %>%
#   group_by(TimeFrame) %>% summarise(deaths = sum(deaths))

# saveRDS(object = df_Off_Lu_Prov_BMJ_Dates, file = "analysis/data/Code-generated-data/00_01b_Lusaka_Prov_Deaths_Official_grouped_BMJ_Dates.rds")

# Lusaka District
# df_Off_Lu_Dist_BMJ_Dates <- df_Off_Lu_Dist %>% filter(date >= "2020-06-15" & date <= "2020-10-04") %>%
#   mutate(TimeFrame = cut.Date(date, breaks = as.Date(BMJ_Fortnight_dates), labels = 1:8, start.on.monday = T)) %>%
#   group_by(TimeFrame) %>% summarise(deaths = sum(deaths))

# saveRDS(object = df_Off_Lu_Dist_BMJ_Dates, file = "analysis/data/Code-generated-data/00_01d_Lusaka_Dist_Deaths_Official_grouped_BMJ_Dates.rds")
##############################################




##############################################
### 9. Crude Mortality in Zambia
##############################################
Cr_mrt <- 6.321/1000
Prob_death_per_day <- Cr_mrt/365 # and per day
saveRDS(Prob_death_per_day, "analysis/data/Code-generated-data/00_09_Crude_Mortality.rds")
# p_bg <- 14*Cr_mrt/365 # and per fortnight
##############################################



##############################################
### 10. PCR, Sero data from Mulenga et al.
##############################################
Sero_prev <- c(val = 2.1, n = 2614, ci95l = 1.1, ci95h = 3.1)
# Sero_prev <- c(val = 3.2, n = 2614, ci95l = 1.7, ci95h = 4.8)
PCR_prev <- c(val = 7.6, n = 2848, ci95l = 4.7, ci95h = 10.6)
Tot_cov_prev <- c(val = 10.6, n = 1886, ci95l = 7.3, ci95h = 13.9)
Lancet_Data <- list(PCR_prev=PCR_prev, Sero_prev=Sero_prev, Tot_cov_prev=Tot_cov_prev)
saveRDS(Lancet_Data, "analysis/data/Code-generated-data/00_10_Lancet_Data.rds")
##############################################


##############################################
### 11. Mixing Matrix
##############################################
MixMatrix <- read.csv(file = "analysis/data/raw/ContMat-Sheppard-Covid19-ZW/Sheppard-Covid19-ZW/zw_ms_s_noFilters-Urban.csv", header = F, sep = " ")
squire::get_mixing_matrix("Zambia")
saveRDS(MixMatrix, file = "analysis/data/Code-generated-data/00_11_Nyanga_Mixing_Matrix.rds")


##############################################
### 12. BMJData Full Data 2
##############################################
df <- readxl::read_xlsx(path = "analysis/data/raw/covid results first paper.xlsx")
df <- df %>% select(sampleid, deceased_date, COVID19, covid19_combined, N1_CT_1,N2_CT_1,NG_CT_1,OR_CT_1, age_death) %>%
  rename(date = deceased_date)

df_1 <- df %>% mutate(N1_CT_1 = ifelse(N1_CT_1 != ".", T, F),
                      N2_CT_1 = ifelse(N2_CT_1 != ".", T, F),
                      NG_CT_1 = ifelse(NG_CT_1 != ".", T, F),
                      OR_CT_1 = ifelse(OR_CT_1 != ".", T, F),
                      date = as.Date(date)) %>%
  mutate(PosTests = N1_CT_1 + N2_CT_1 + NG_CT_1 + OR_CT_1) %>%
  select(sampleid, date, COVID19, covid19_combined, PosTests, age_death)

df_2 <- df %>% mutate(N1_CT_1 = ifelse(N1_CT_1 != "." & N1_CT_1<40, T, F),
                      N2_CT_1 = ifelse(N2_CT_1 != "." & N1_CT_1<40, T, F),
                      NG_CT_1 = ifelse(NG_CT_1 != "." & N1_CT_1<40, T, F),
                      OR_CT_1 = ifelse(OR_CT_1 != "." & N1_CT_1<40, T, F)) %>%
  mutate(PosTests_Strict = N1_CT_1 + N2_CT_1 + NG_CT_1 + OR_CT_1) %>%
  select(sampleid,date, COVID19, covid19_combined, PosTests_Strict, age_death)

df <- merge(df_1, df_2, all = T) %>%
  mutate(Age_gr = cut(as.numeric(age_death), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>% select(-age_death)

df_Date <- df %>% group_by(date, Age_gr) %>%
  summarise(Samples = length(PosTests),
            PosTests = sum(ifelse(PosTests !=0,T,F)),
            PosTests_Strict = sum(ifelse(PosTests_Strict >=2,T,F))) %>%
  arrange(date) %>% ungroup() %>%
  tidyr::complete(Age_gr, date = seq.Date(min(date), max(date), by="day"),
                  fill = list(Samples = 0, PosTests = 0, PosTests_Strict = 0))

saveRDS(df_Date,"analysis/data/Code-generated-data/00_12_Post_Mortem_Complete_Age.rds")

df_Date_no_age <- df_Date %>% group_by(date) %>%
  summarise(date = head(date,1),
            Samples = sum(Samples),
            PosTests = sum(PosTests),
            PosTests_Strict = sum(PosTests_Strict)) %>% filter(date >="2020-06-15")

saveRDS(df_Date_no_age,"analysis/data/Code-generated-data/00_12_Post_Mortem_Complete_Date.rds")

df_weeks <- df_Date_no_age %>% mutate(Week_gr = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  group_by(Week_gr) %>%
  summarise(Samples = sum(Samples),
            PosTests = sum(PosTests),
            PosTests_Strict = sum(PosTests_Strict))

saveRDS(df_weeks,"analysis/data/Code-generated-data/00_12_Post_Mortem_Complete_weeks.rds")



##############################################
### 13. Combined Mortuary, post-mortem data 2
##############################################

Combined_Dates2 <- merge(df_Date, UTH_deaths_by_age, all = T) %>% tidyr::replace_na(list(Mort_deaths = 0, Samples = 0, PosTests = 0, PosTests_Strict = 0)) %>%
  tidyr::complete(Age_gr, date, fill = list(Mort_deaths = 0, Samples = 0, PosTests = 0, PosTests_Strict = 0))

Combined_Weeks2 <- Combined_Dates2 %>% filter(date>="2020-06-15") %>% mutate(Week_gr = cut.Date(x = date, breaks = "weeks", labels = F)) %>%
  group_by(Week_gr, Age_gr) %>% summarise(date = head(date,1),
                                             Mort_deaths = sum(Mort_deaths),
                                             Samples = sum(Samples),
                                             PosTests = sum(PosTests),
                                             PosTests_Strict = sum(PosTests_Strict)) %>%
  ungroup %>% tidyr::complete(Age_gr, tidyr::nesting(Week_gr,date), fill = list(Mort_deaths = 0, sampled_deaths = 0, CT_45_Either = 0, CT_40_Either = 0))

# table(Combined_Weeks[,c("Age_gr","Week_gr")])

Combined_Weeks2[Combined_Weeks2$Week_gr==4, "Mort_deaths"] <-
  Combined_Weeks2[Combined_Weeks2$Week_gr %in% c(3,5), c("Age_gr","Week_gr","Mort_deaths")] %>% group_by(Age_gr) %>%
  summarise(Mort_deaths = round(mean(Mort_deaths))) %>% select(Mort_deaths)
# sum(Combined_Weeks2_no_age$Samples)
# sum(Combined_Weeks2_no_age$PosTests)
# sum(Combined_Weeks2_no_age$PosTests_Strict)
# sum(Combined_Weeks2_no_age$Mort_deaths)

saveRDS(object = Combined_Weeks2, file = "analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")

Combined_Weeks2_no_age <- Combined_Weeks2 %>% ungroup() %>% group_by(Week_gr) %>%
  summarise(Mort_deaths = sum(Mort_deaths),
            Samples = sum(Samples),
            PosTests = sum(PosTests),
            PosTests_Strict = sum(PosTests_Strict))
saveRDS(object = Combined_Weeks2_no_age, file = "analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete_weeks_only.rds")


####
### 14: Mulenga Combined prevalence by age group.

Mulenga_Combined_Prev_Age <- data.frame(Combined_Prev = c(4, 8.1, 10.3, 14.4, 5.4, 16.093, 11.523, 13.311),
           LowerInt = c(0.7, 3.8, 6.6, 4.9, 1.4, 0.085, 1.681, 2.384),
           HigherInt = c(7.4, 12.3, 14.0, 23.9, 9.4, 32.182, 21.451, 24.261),
           row.names = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70+"))
saveRDS(Mulenga_Combined_Prev_Age, "analysis/data/Code-generated-data/14_Mulenga_Combined_Prevalence_by_Age.rds")




##############################################
### 11. Euroimmun ELISA detection:
##############################################
## Current Probability of Sero Conversion: A gamma distribution
# sero_sens = 0.9
# prob_conversion <-  cumsum(dgamma(0:300,shape = 5, rate = 1/2))/max(cumsum(dgamma(0:300,shape = 5, rate = 1/2)))
# sero_reversion <- cumsum(dweibull(0:300, 3.669807, scale = 143.7046))
# sero_det_a <- prob_conversion-sero_reversion
# sero_det_a[sero_det_a < 0] <- 0
# sero_det <- sero_det_a/max(sero_det_a)*sero_sens  # assumed maximum test sensitivities
#
# plot(x = 0:300, prob_conversion, type = "l", xlim = c(0,50), xlab = "Day", ylab = "Sero Detection")
# points(x = 0:300, sero_det, type = "l", lty = 2)
#
# sero_det_func <- function(max_sens, shape, rate, times){
#   prob_conversion <-  cumsum(dgamma(x = times, shape = shape, rate = rate))/max(cumsum(dgamma(times,shape = shape, rate = rate)))
#   sero_reversion <- cumsum(dweibull(times, 3.669807, scale = 143.7046))
#   sero_det_a <- prob_conversion-sero_reversion
#   sero_det_a[sero_det_a < 0] <- 0
#   sero_det <- sero_det_a/max(sero_det_a)*max_sens  # assumed maximum test sensitivities
#   return(sero_det)
# }
#
#
# # Bring in the data from Van Elslande
# Data_VEsl <- data.frame(DaysPostSympt = c(5.5, 7.5, 9.5, 11.5, 13.5, 15.5, 17.5),
#                         PosPerc = c(23.333333333333343,27.564102564102555,37.3076923076923,50.38461538461539,67.43589743589745,78.5897435897436,87.56410256410257)/100,
#                         PosPercUI = c(35.52825261158594, 37.457264957264954, 46.06362773029439, 59.1957502374169, 74.85042735042735, 84.27291073124407, 91.9889601139601)/100,
#                         PosPercLI = c(14.309116809116802, 19.35422602089269, 29.296058879392206, 42.72495251661918, 59.86348528015194, 71.88271604938271, 81.89874169040836)/100)
# points(x = Data_VEsl$DaysPostSympt, y = Data_VEsl$PosPerc, col = 3, pch = 20,cex = 0.5, type = "l")
#
# Data_Elslande_June <- data.frame(DaysPostSympt =c(5.5,7.5,9.5,11.5,13.5,15.5,17.5),
#                                  PosPerc = c(19.300911854103347,27.355623100303962,37.841945288753806,53.039513677811556,
#                                              70.66869300911854,80.69908814589667,89.96960486322189)/100)
# points(x = Data_Elslande_June$DaysPostSympt, y = Data_Elslande_June$PosPerc, col = 7, type = "l")
#
# ### I am going to vary the max sensitivity, the shape and rate of the dgamma seroconversion curve. Here are the values I'm going to vary over:
# max_sens_vec <- seq(0.95,0.98, by = 0.01)
# shape_vec <- seq(2,9, by = 0.01)
# rate_vec <- seq(0.2,0.9, by = 0.01)
# parm_space <- expand.grid(max_sens = max_sens_vec,
#                           shape = shape_vec,
#                           rate = rate_vec)
#
#
# # sero_det_func(0.9, 5, 0.5, Data_VEsl$DaysPostSympt)
#
# LSS_parms <- apply(X = parm_space, MARGIN = 1, FUN = function(x){
#   # browser()
#   # Create the model
#   sero_det <- sero_det_func(x["max_sens"], x["shape"], x["rate"], Data_VEsl$DaysPostSympt[-1:-2])
#   # Match with the data from Elslande
#   sum((sero_det - Data_VEsl$PosPerc[-1:-2])^2)
# })
#
# cbind(parm_space, LSS_parms)
# min(LSS_parms)
# which.min(LSS_parms)
# cbind(parm_space, LSS_parms)[1,]
#
# points(x = 0:300, sero_det_func(0.95, 2, 0.2, 0:300),col = 2, type = "l")
#
# sum((sero_det_func(0.9, 5, 0.5, Data_VEsl$DaysPostSympt[-1:-2]) - Data_VEsl$PosPerc[-1:-2])^2)
# sum((sero_det_func(0.95, 4, 0.4, Data_VEsl$DaysPostSympt[-1:-2]) - Data_VEsl$PosPerc[-1:-2])^2)
# points(sero_det_func(0.9, 5, 0.4, Data_VEsl$DaysPostSympt), col = 3)
# plot(sero_det_func(0.9, 5, 0.4, 0:300),xlim = c(0,50))
# points(sero_det_func(0.9, 5, 0.4, seq(0.5,50.5,by = 0.5)),col = 4)
# points(Data_VEsl$PosPerc[-1], col = 2)
#
# sum((sero_det_func(0.95, 5, 0.4, Data_VEsl$DaysPostSympt[-1:-2]) - Data_VEsl$PosPerc[-1:-2])^2)
# sum((sero_det_func(0.95, 2, 0.2, Data_VEsl$DaysPostSympt[-1:-2]) - Data_VEsl$PosPerc[-1:-2])^2)
#
# ((sero_det_func(0.95, 5, 0.4, Data_VEsl$DaysPostSympt[-1:-2]) - Data_VEsl$PosPerc[-1:-2])^2)
# ((sero_det_func(0.95, 2, 0.2, Data_VEsl$DaysPostSympt[-1:-2]) - Data_VEsl$PosPerc[-1:-2])^2)
#
# ((sero_det_func(0.95, 5, 0.4, Data_VEsl$DaysPostSympt[-1:-2]) - Data_VEsl$PosPerc[-1:-2]))
# ((sero_det_func(0.95, 2, 0.2, Data_VEsl$DaysPostSympt[-1:-2]) - Data_VEsl$PosPerc[-1:-2]))
#
# plot(x = 0:300, y = sero_det_func(0.95, 2, 0.2, 0:300), type = "l", xlim = c(0,50), xlab = "Day", ylab = "Sero Detection")
# points(x = 0:300, y = sero_det_func(0.95, 5, 0.4, 0:300), col = 2,type = "l")
# points(x = seq(0, 300, by = 10), y = sero_det_func(0.95, 5, 0.4, seq(0, 300, by = 10)), col = 2,type = "l")
# points(x = Data_VEsl$DaysPostSympt[-1:-2], y = sero_det_func(0.95, 5, 0.4, Data_VEsl$DaysPostSympt[-1:-2]), col = 4, pch = 20,cex = 0.5, type = "l")
# points(x = Data_VEsl$DaysPostSympt, y = Data_VEsl$PosPerc, col = 3, pch = 20,cex = 0.5, type = "l")
# points(x = Data_VEsl$DaysPostSympt[-1:-2], y = sero_det_func(0.95, 5, 0.4, Data_VEsl$DaysPostSympt[-1:-2]), col = 4, pch = 20,cex = 0.5, type = "l")
#
#
# plot(x = 0:300, y = sero_det_func(0.9, 5, 0.4, 0:300), type = "l", xlim = c(0,50), xlab = "Day", ylab = "Sero Detection")
# plot(x = 0:300, y = sero_det_func(parm_space[82,"max_sens"], parm_space[82,"shape"], parm_space[82,"rate"], 0:300), type = "l", xlim = c(0,50), xlab = "Day", ylab = "Sero Detection")
# plot(x = 0:300, y = sero_det_func(0.9, 5, 0.4, 0:300), type = "l", xlim = c(0,50), xlab = "Day", ylab = "Sero Detection")
# plot(x = 0:300, y = sero_det_func(0.9, 5, 0.4, 0:300), type = "l", xlim = c(0,50), xlab = "Day", ylab = "Sero Detection")
# points(x = 0:300, y = sero_det_func(0.95, 5, 0.4, 0:300), type = "l", lty = 2)
#
#
#
#
#
#
#
#
# ## I vary on the max sensitivity. Let's fit it to the data:
# seq(0.9,1,by=0.001)
# # For each of those values, match the fit
# sero_det <- sero_det_a/max(sero_det_a)*sero_sens  # assumed maximum test sensitivities
# # For the fit, take the right values, those are days, so I need the right values.
# (sero_det[Data_Elslande_June$DaysPostSympt]+sero_det[Data_Elslande_June$DaysPostSympt+1])/2
# # And then I can compare that with the actual values
# Data_VEsl$PosPerc
#
# function(x){
#   sero_det <- sero_det_a/max(sero_det_a)*x
#   sero_vals <- (sero_det[Data_Elslande_June$DaysPostSympt]+sero_det[Data_Elslande_June$DaysPostSympt+1])/2
#   Data_VEsl$PosPerc - sero_vals
# }
#
# nls(formula = Data_VEsl$PosPerc ~ Data_VEsl$DaysPostSympt)
#
# p_con <- cumsum(dgamma(Data_VEsl$DaysPostSympt, shape = 5, rate = 1/2))/max(cumsum(dgamma(Data_VEsl$DaysPostSympt,shape = 5, rate = 1/2)))
# p_rev <- cumsum(dweibull(Data_VEsl$DaysPostSympt, 3.669807, scale = 143.7046))
#
# sero_sens * cumsum(dgamma(Data_VEsl$DaysPostSympt, shape = 5, rate = 1/2))/max(cumsum(dgamma(Data_VEsl$DaysPostSympt,shape = 5, rate = 1/2))) - cumsum(dweibull(Data_VEsl$DaysPostSympt, 3.669807, scale = 143.7046))/
#   max(cumsum(dgamma(Data_VEsl$DaysPostSympt, shape = 5, rate = 1/2))/max(cumsum(dgamma(Data_VEsl$DaysPostSympt,shape = 5, rate = 1/2))) - cumsum(dweibull(Data_VEsl$DaysPostSympt, 3.669807, scale = 143.7046)))
#
# nls(Data_VEsl$PosPerc ~ x * cumsum(dgamma(Data_VEsl$DaysPostSympt, shape = 5, rate = 1/2))/max(cumsum(dgamma(Data_VEsl$DaysPostSympt,shape = 5, rate = 1/2))) - cumsum(dweibull(Data_VEsl$DaysPostSympt, 3.669807, scale = 143.7046))/
#      max(cumsum(dgamma(Data_VEsl$DaysPostSympt, shape = 5, rate = 1/2))/max(cumsum(dgamma(Data_VEsl$DaysPostSympt,shape = 5, rate = 1/2))) - cumsum(dweibull(Data_VEsl$DaysPostSympt, 3.669807, scale = 143.7046)))
# )
# Data_VEsl$DaysPostSympt
# Data_Elslande_June$DaysPostSympt
#
#
# x <- Data_VEsl$DaysPostSympt[-1]
# y <- Data_Elslande_June$PosPerc[-1]
#
# m <- nls(y ~ a * cumsum(dgamma(x, shape = 5, rate = 1/2))/max(cumsum(dgamma(x,shape = 5, rate = 1/2))) - cumsum(dweibull(x, 3.669807, scale = 143.7046))/
#       max(cumsum(dgamma(x, shape = 5, rate = 1/2))/max(cumsum(dgamma(x,shape = 5, rate = 1/2))) - cumsum(dweibull(x, 3.669807, scale = 143.7046))),
#     start = list(a = 0.9))
#
# summary(m)$coefficients[1]
#
# # plot(x = x, y = y)
# lines(x, predict(m), col = 2)
#
# x <- 0:300
# plot(x = x, y = cumsum(dgamma(x, shape = 5, rate = 1/2))/max(cumsum(dgamma(x,shape = 5, rate = 1/2))) - cumsum(dweibull(x, 3.669807, scale = 143.7046))/
#        max(cumsum(dgamma(x, shape = 5, rate = 1/2))/max(cumsum(dgamma(x,shape = 5, rate = 1/2))) - cumsum(dweibull(x, 3.669807, scale = 143.7046))), type = "l")
# lines(x,  summary(m)$coefficients[1] * cumsum(dgamma(x, shape = 5, rate = 1/2))/max(cumsum(dgamma(x,shape = 5, rate = 1/2))) - cumsum(dweibull(x, 3.669807, scale = 143.7046))/
#         max(cumsum(dgamma(x, shape = 5, rate = 1/2))/max(cumsum(dgamma(x,shape = 5, rate = 1/2))) - cumsum(dweibull(x, 3.669807, scale = 143.7046))), col = 2, lwd = 3)
#
# m <- nls(y~a * x ^ 3 + b * x + c,
#          start = list(a = 1, b = 2, c = 1))
#
# ##############################################
#
#
#
