###########################
##### Data Management #####
###########################
rm(list = ls())
library(squire)
library(tidyverse)

##############################################
### 1. Official Lusaka Province Death Data
df_Off_Lu <- read.csv(file = "analysis/data/raw/00_official_reports_covid_zambia.csv")

# Filter deaths for Lusaka Province until November 2020
df_Off_Lu_Prov <- df_Off_Lu %>%
  mutate(date = as.Date(Date), deaths = Total_Deaths) %>%
  filter(Province=="Lusaka" & date < "2020-11-01") %>%
  select(date, deaths) %>% na.omit() %>%
  group_by(date) %>% summarise(deaths = sum(deaths))

date_list <- seq(min(df_Off_Lu_Prov$date), max(df_Off_Lu_Prov$date), by = 1)
missing_dates <- date_list[!date_list %in% df_Off_Lu_Prov$date] # Add missing dates with 0 deaths

df_Off_Lu_Prov <- add_row(df_Off_Lu_Prov, date = missing_dates, deaths = 0) %>% arrange(date)

saveRDS(object = df_Off_Lu_Prov, file = "analysis/data/Code-generated-data/00_01_Lusaka_Prov_Deaths.rds")

df_Off_Lu_Prov_BMJ_Dates <- df_Off_Lu_Prov %>% filter(date >= "2020-06-08" & date <= "2020-09-27") %>%
  mutate(TimeFrame = cut.Date(date, breaks = as.Date(c("2020-06-08","2020-06-22","2020-07-06","2020-07-20","2020-08-03","2020-08-17","2020-08-31","2020-09-14","2020-09-28")), labels = 1:8, start.on.monday = T)) %>%
  group_by(TimeFrame) %>% summarise(deaths = sum(deaths))
saveRDS(object = df_Off_Lu_Prov_BMJ_Dates, file = "analysis/data/Code-generated-data/00_01b_Lusaka_Prov_Deaths_BMJ_Dates.rds")
##############################################


##############################################
### 2. Lusaka Population structure
# 2020 Age distribution for Lusaka Province from opendataforafrica (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
pop_st_lu_prov <- c(549475,448008,379841,339600,317148,305521,271318,232188,172365,125531,75157,51883,35587,22219,14960,8570,10812)
saveRDS(pop_st_lu_prov, "analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds")
# sum(pop_st_lu)

## Population size for Lusaka Province
# 2010: 2191225, 2222812 or 2238569 (https://en.wikipedia.org/wiki/Lusaka_Province)
# 2014: 2669249 (https://zambia.opendataforafrica.org/apps/atlas/Lusaka)
# 2018: 3186336 (https://en.wikipedia.org/wiki/Provinces_of_Zambia)
# 2019: 3308438 (https://www.citypopulation.de/en/zambia/admin/05__lusaka/)
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
# 2019: 3308400 (https://en.wikipedia.org/wiki/Lusaka, https://www.citypopulation.de/en/zambia/cities/)
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
### 3. Zambia Total IFR by age/probability of death
Zmb_p <-parameters_explicit_SEEIR("Zambia")
prob_death_tot <- Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment +
  (1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_treatment
saveRDS(prob_death_tot, "analysis/data/Code-generated-data/00_03_Tot_Prob_Death_By_Age_Zam.rds")
##############################################



##############################################
### 4. Zambia Weighted durations for
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
# pop_st_lu_prov <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds")
Death_Dur_Weighted <- sum(Death_Dur_WeightedByAge * Zmb_p$prob_hosp/sum(Zmb_p$prob_hosp))
Surv_Dur_Weighted <- sum(Surv_Dur_WeightedByAge * Zmb_p$prob_hosp/sum(Zmb_p$prob_hosp))
saveRDS(list(Death_Dur_Weighted = Death_Dur_Weighted, Surv_Dur_Weighted = Surv_Dur_Weighted), "analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
##############################################


##############################################
### 5. BMJ Data
BMJdata <- read.csv("../Bonus Files/Data/RE-Deaths-in-Lusaka.csv")[1:8,c("Weeks","Sampled.Deaths.Total","Sampled.Deaths.from.Covid")] %>%
  rename(TotSamDeaths = Sampled.Deaths.Total, CovSamDeaths = Sampled.Deaths.from.Covid) %>%
  mutate(TotEstCovDeathsLus_10x = CovSamDeaths*10/0.8,
         Sampling = c(5,5,5,5,3,3,3*1/10+2*9/10,2),  # Sampling was 1 in 5 for June, July, 1 in 3 for Aug, 1 in 2 for Sep.
         Cap = c(1,1,1,1,1,1,1/10,0)) %>% # Daily tests were capped at 5 or 6 in June, July and Aug. No cap in Sep.
  mutate(TotEstCovDeathsLus_Low_Est = CovSamDeaths*Sampling/0.8,
         TotEstCovDeathsLus_High_Est = (CovSamDeaths*Sampling + CovSamDeaths/TotSamDeaths * Cap*(3600-sum(TotSamDeaths*Sampling))/(sum(Cap)))/0.8,
         q1_10x = TotEstCovDeathsLus_10x/sum(pop_st_lu_prov),
         q1_High_Est = TotEstCovDeathsLus_High_Est/sum(pop_st_lu_prov))
saveRDS(BMJdata, "analysis/data/Code-generated-data/00_05_BMJ_Data.rds")

# Explanation of High Estimation:
# MinDeaths <- sum(BMJdata$TotSamDeaths*BMJdata$Sampling) # Min number of deaths, estimated from sampling:
# UnaccountedDeaths <- 3600-MinDeaths # Number of deaths unaccounted for
# Unacc_Deaths_Attrib <- BMJdata$Cap*UnaccountedDeaths/(sum(BMJdata$Cap)) # Attribute unaccounted deaths into time periods according to days that had caps
# cvd_death_prev <- BMJdata$CovSamDeaths/BMJdata$TotSamDeaths # Covid death prevalence:
# Min_cvd_deaths <- BMJdata$CovSamDeaths*BMJdata$Sampling # Min cvd deaths from sampling
# (Min_cvd_deaths + cvd_prev * Unacc_Deaths_Attrib)/0.8 # Add min cvd deaths with estimated

# Break down BMJ data by day:
df_off_fil <- df_Off_Lu_Prov %>% filter(date >= "2020-06-08" & date <= "2020-09-27")
# DateSequence <- seq(as.Date("2020-06-08"), max(as.Date(df_off_fil$date))+4, by =1) # I added 4 more days... unclear why

df_off_fil <- df_off_fil %>% #add_row(date = as.Date(c(as.character(DateSequence[!(DateSequence %in% df_off_fil$date)]))),
                                     # deaths = 0) %>%
  mutate(TimeFrame = cut.Date(date, breaks = as.Date(c("2020-06-08","2020-06-22","2020-07-06","2020-07-20","2020-08-03","2020-08-17","2020-08-31","2020-09-14","2020-09-28")), labels = 1:8, start.on.monday = T))


## First for the simple estimate (1 in 10 deaths tested, assuming constant sampling)
BMJdata <- BMJdata %>% mutate(TimePeriod = 1:8)
df_off_fil_gr <- df_off_fil %>% group_by(TimeFrame) %>%
  summarise(FrameDeaths = sum(deaths))

## Complex estimates
# Take the total number of estimated deaths in Lusaka, deduct the ones that were officially reported, and distribute the other deaths evenly
# 10x, lower limit and higher limit.
df_off_fil <- df_off_fil %>% mutate(Est_Deaths_BMJ_10 = rep((BMJdata$TotEstCovDeathsLus_10x - df_off_fil_gr$FrameDeaths)/14, each = 14) + deaths,
                                              Est_Deaths_BMJ_Sam_LE = rep((BMJdata$TotEstCovDeathsLus_Low_Est - df_off_fil_gr$FrameDeaths)/14, each = 14) + deaths,
                                              Est_Deaths_BMJ_Sam_HE = rep((BMJdata$TotEstCovDeathsLus_High_Est - df_off_fil_gr$FrameDeaths)/14, each = 14) + deaths) %>%
  mutate(q1_10x = Est_Deaths_BMJ_10/sum(pop_st_lu_prov),
         q1_HighEst = Est_Deaths_BMJ_Sam_HE/sum(pop_st_lu_prov))

saveRDS(object = df_off_fil, file = "analysis/data/Code-generated-data/00_05b_BMJ_Data_DailyEsts.rds")
##############################################



##############################################
### 6. Crude Mortality in Zambia
Cr_mrt <- 6.321/1000
Prob_death_per_day <- Cr_mrt/365 # and per day
saveRDS(Prob_death_per_day, "analysis/data/Code-generated-data/00_06_Crude_Mortality.rds")
# p_bg <- 14*Cr_mrt/365 # and per fortnight


##############################################
### 7. Euroimmun ELISA detection:
## Current Probability of Sero Conversion: A gamma distribution
plot(cumsum(dgamma(0:300, shape = 5, rate = 1/2))/max(cumsum(dgamma(0:300,shape = 5, rate = 1/2))), type = "l", xlim = c(0,50), xlab = "Day", ylab = "Sero Detection")
sero_sens = 0.9
prob_conversion <-  cumsum(dgamma(0:300,shape = 5, rate = 1/2))/max(cumsum(dgamma(0:300,shape = 5, rate = 1/2)))
sero_det <- cumsum(dweibull(0:300, 3.669807, scale = 143.7046))
sero_det <- prob_conversion-sero_det
sero_det[sero_det < 0] <- 0
sero_det <- sero_det/max(sero_det)*sero_sens  # assumed maximum test sensitivitys
points(sero_det, type = "l", lty = 2)

## Things I can vary: shape, rate...


# Bring in the data from Van Elslande
Data_VEsl <- data.frame(DaysPostSympt = c(5.5, 7.5, 9.5, 11.5, 13.5, 15.5, 17.5),
                        PosPerc = c(23.333333333333343,27.564102564102555,37.3076923076923,50.38461538461539,67.43589743589745,78.5897435897436,87.56410256410257)/100,
                        PosPercUI = c(35.52825261158594, 37.457264957264954, 46.06362773029439, 59.1957502374169, 74.85042735042735, 84.27291073124407, 91.9889601139601)/100,
                        PosPercLI = c(14.309116809116802, 19.35422602089269, 29.296058879392206, 42.72495251661918, 59.86348528015194, 71.88271604938271, 81.89874169040836)/100)
points(x = Data_VEsl$DaysPostSympt, y = Data_VEsl$PosPerc, col = 3, pch = 20,cex = 0.5, type = "l")

Data_Elslande_June <- data.frame(DaysPostSympt =c(5.5,7.5,9.5,11.5,13.5,15.5,17.5),
                                 PosPerc = c(19.300911854103347,27.355623100303962,37.841945288753806,53.039513677811556,
                                             70.66869300911854,80.69908814589667,89.96960486322189)/100)
points(x = Data_Elslande_June$DaysPostSympt, y = Data_Elslande_June$PosPerc, col = 7, type = "l")



##############################################
# Add Lancet Data



