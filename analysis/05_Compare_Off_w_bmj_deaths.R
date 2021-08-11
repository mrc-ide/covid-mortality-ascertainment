### BMJ reported 70 covid deaths from 362 tests conducted, from a pool of ~3600 total deaths over the period ###
### How do these numbers compare with the official statistics? ###
library(tidyverse)
library(squire)
devtools::load_all(".")


### Official statistics from the period:
df_off <- read.csv(file = "analysis/data/zambia_covid.csv")
# Filter by district and Date.
# Note: The data for the Province of Lusaka is essentially the same as the district (there was an additional 1 death in Chirundu)
df_off_fil <- df_off %>% filter(District=="Lusaka" & !is.na(Total_Deaths) & as.Date(Date) >= "2020-06-08" & as.Date(Date) <= "2020-09-27") %>%
  select("Date","Total_Deaths","Facility_Deaths","Community_Deaths","Notes") %>%
  mutate(TimeFrame = cut.Date(as.Date(Date), breaks = as.Date(c("2020-06-08","2020-06-22","2020-07-06","2020-07-20","2020-08-03","2020-08-17","2020-08-31","2020-09-14","2020-09-28")), labels = 1:8, start.on.monday = T)) %>%
  group_by(TimeFrame) %>% summarise(Deaths = sum(Total_Deaths))

DeathTotalOff <- sum(df_off_fil$Deaths)


## Estimate underreporting fraction using model
data <- df_off %>%
  filter(Province=="Lusaka" & as.Date(Date) < "2020-11-01") %>%
  select(Date, Total_Deaths) %>%
  na.omit() %>%
  group_by(Date) %>% summarise(deaths = sum(Total_Deaths)) %>%
  rename(date = Date) %>%
  mutate(date = as.Date(date))

# data$date <- as.Date(data$date)
date_list <- seq(min(data$date), max(data$date), by = 1)
missing_dates <- date_list[!date_list %in% data$date]
data <- add_row(data, date = missing_dates, deaths = 0)
pop_st_lu <- readRDS("analysis/data/Lusaka_pop_2020_Open_Africa.rds") # See 02_zambia_lusaka_initial
prob_death_tot_IFR_frac <- readRDS("analysis/data/Tot_Prob_Age_Death_Zam.rds") # See 02_zambia_lusaka_initial
fit <- fit_spline_rt(data = data,
                     country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                     population = pop_st_lu,
                     reporting_fraction = 0.1,
                     reporting_fraction_bounds = c(0.25,0.1,1),
                     n_mcmc = 100000,
                     replicates = 100,
                     rw_duration = 14,
                     hosp_beds = 1e10,
                     icu_beds = 1e10,
                     prob_severe = rep(0,17),
                     prob_non_severe_death_treatment = prob_death_tot_IFR_frac,
                     dur_get_ox_survive =12,
                     dur_get_ox_die =10,
                     dur_R = Inf,
                     sero_df_start = c("2020-07-04","2020-07-18"),
                     sero_df_end = c("2020-07-27","2020-08-10"),
                     sero_df_pos = c(0.021*4258, 0.106*4258),
                     sero_df_samples = c(4258,4258)

)


plot(fit, particle_fit = T)

mean(fit$replicate_parameters$rf)

fit_to_send <- fit
fit_to_send$output <- NULL
saveRDS(fit_to_send, file = "../fit_weird_rf.rds")
r <- readRDS("../fit_weird_rf.rds")

r$parameters

df_off_fil

### BMJ data
df_bmj <- read.csv("../Bonus Files/Data/RE-Deaths-in-Lusaka.csv")[1:8,c("Weeks","Sampled.Deaths.Total","Sampled.Deaths.from.Covid")] %>%
  rename(TotSamDeaths = Sampled.Deaths.Total,CovSamDeaths = Sampled.Deaths.from.Covid)

# Approx est. based on 1 in 10 reporting, UTH has 80% coverage of deaths in Lusaka:
df_bmj_approx <- df_bmj %>% mutate(TotEstCovDeathsLus_10x = CovSamDeaths*(10)/0.8)
sum(df_bmj_approx$TotEstCovDeathsLus_10x)

DeathTotalOff/sum(df_bmj_approx$TotEstCovDeathsLus_10x)

# How many "COVID" deaths were attributed to COVID?
# Remove potential those that died with covid, not from covid
PrevInNCD <- seq(0.01,0.08,0.01)
par(mfrow=c(1,2))
plot(PrevInNCD, (sum(df_bmj_approx$TotEstCovDeathsLus_10x) - PrevInNCD*(3600/0.8))/(1-PrevInNCD),
     xlab="COVID Prev. in Non-COVID Deaths", ylab = "True COVID Deaths (est.)", type = "l")
plot(PrevInNCD, DeathTotalOff/(sum(df_bmj_approx$TotEstCovDeathsLus_10x) - PrevInNCD*(3600/0.8))/(1-PrevInNCD),
     xlab="COVID Prev. in Non-COVID Deaths", ylab = "Reporting Fraction (Gvt/Total exp.)", type = "l")



# Instead of multiplying by 10 for each row, we can use the sampling effort to estimate how many died each time period.
# We can then use the time sampling proportions to scale to the deaths in each time period.
Sampling <- c(5,5,5,5,3,3,3*1/10+2*9/10,2) # Sampling was 1 in 5 for June, July, 1 in 3 for Aug, 1 in 2 for Sep.
Cap <- c(1,1,1,1,1,1,1/10,0) # Daily tests were capped at 5 or 6 in June, July and Aug. No cap in Sep.
# That means that for September we can calculated the original deaths, but for the other months we can scale to 3600

# I actually have 363 deaths at the moment in the absence of clearer data.
# These were the deaths that I am confident occurred.
# df_bmj$TotSamDeaths*Sampling
# Deduct this number from Total Deaths expected
# 3600-sum(df_bmj$TotSamDeaths*Sampling)
# These deaths need to be attributed to the weeks. Attribute evenly, except for the September weeks
# (3600-sum(df_bmj$TotSamDeaths*Sampling))/(sum(Cap))
# these can be added to the actual numbers
# df_bmj$TotSamDeaths*Sampling + Cap*(3600-sum(df_bmj$TotSamDeaths*Sampling))/(sum(Cap))
# Deaths scaled up + proportion expected to be covid deaths * Extra Deaths attributed to each week
# df_bmj$CovSamDeaths*Sampling + df_bmj$CovSamDeaths/df_bmj$TotSamDeaths * Cap*(3600-sum(df_bmj$TotSamDeaths*Sampling))/(sum(Cap))

# Create new column
# The low est doesn't take into account the capping of deaths.
df_bmj_approx <- df_bmj_approx %>% mutate(TotEstCovDeathsLus_Low_Est = CovSamDeaths*Sampling/0.8,
                                        TotEstCovDeathsLus_High_Est = (CovSamDeaths*Sampling + CovSamDeaths/TotSamDeaths * Cap*(3600-sum(TotSamDeaths*Sampling))/(sum(Cap)))/0.8)

### Now plot the data with the additional expected deaths.
par(mfrow=c(1,1))
plot(x = 1:8, y = df_bmj_approx$CovSamDeaths, type = "l", ylim = c(1,300), ylab = "Deaths", xlab = "Reporting Period", xaxt="n")
axis(side=1, labels = paste0("W",c("24-25","26-27","28-29","30-31","32-33","34-35","36-37","38-39")), at = 1:8)
points(x = 1:8, y = df_bmj_approx$TotEstCovDeathsLus_10x, type = "l", lty=2, col =2)
points(x = 1:8, y = df_bmj_approx$TotEstCovDeathsLus_Low_Est, type = "l", lty=3, col = 3)
points(x = 1:8, y = df_bmj_approx$TotEstCovDeathsLus_High_Est, type = "l", lty=4, col = 4)
legend("topright", legend = c("Official deaths","10x Off. deaths", "Est. deaths (sampling)", "Est. deaths (sampling, scaled)"),lty = 1:4, col = c(1,2,3,4))

# Estimated underreporting per time period
plot(100*df_off_fil$Deaths/df_bmj_approx$TotEstCovDeathsLus_High_Est, type = "l", ylab = "% Reported")

### Now estimate data per day.
### Find Lusaka data and group by time periods.
df_off_fil_full <- df_off %>% filter(District=="Lusaka" & !is.na(Total_Deaths) & as.Date(Date) >= "2020-06-01" & as.Date(Date) <= "2020-09-30")
DateSequence <- seq(as.Date("2020-06-08"), max(as.Date(df_off_fil_full$Date))+4, by =1)

df_off_fil_full <- df_off_fil_full %>% add_row(Date = c(as.character(DateSequence[!(DateSequence %in% as.Date(df_off_fil_full$Date))])),
                        Total_Deaths = 0, Facility_Deaths = 0, Community_Deaths = 0) %>%
  select("Date","Total_Deaths","Facility_Deaths","Community_Deaths","Notes") %>%
  arrange(Date) %>%
  mutate(TimeFrame = cut.Date(as.Date(Date), breaks = as.Date(c("2020-06-08","2020-06-22","2020-07-06","2020-07-20","2020-08-03","2020-08-17","2020-08-31","2020-09-14","2020-09-28")), labels = 1:8, start.on.monday = T))


## First for the simple estimate (1 in 10 deaths tested, assuming constant sampling)
df_bmj_approx <- df_bmj_approx %>% mutate(TimePeriod = 1:8)
df_off_fil_full_gr <- df_off_fil_full %>% group_by(TimeFrame) %>%
  summarise(FrameDeaths = sum(Total_Deaths))

## Complex estimates
# Take the total number of estimated deaths in Lusaka, deduct the ones that were officially reported, and distribute the other deaths evenly
# 10x, lower limit and higher limit.
df_off_fil_full <- df_off_fil_full %>% mutate(Est_Deaths_BMJ_10 = rep((df_bmj_approx$TotEstCovDeathsLus_10x - df_off_fil_full_gr$FrameDeaths)/14, each = 14) + Total_Deaths,
                                              Est_Deaths_BMJ_Sam_LE = rep((df_bmj_approx$TotEstCovDeathsLus_Low_Est - df_off_fil_full_gr$FrameDeaths)/14, each = 14) + Total_Deaths,
                                              Est_Deaths_BMJ_Sam_HE = rep((df_bmj_approx$TotEstCovDeathsLus_High_Est - df_off_fil_full_gr$FrameDeaths)/14, each = 14) + Total_Deaths
                                              )

saveRDS(object = df_off_fil_full, file = "analysis/data/BMJ_estimated_mortality_sampling.rds")

# Melt and plot
library(reshape2)
dd <- melt(df_off_fil_full[,c("Date","Est_Deaths_BMJ_Sam_HE","Est_Deaths_BMJ_10","Est_Deaths_BMJ_Sam_LE","Total_Deaths")],id="Date")
# cols <- c("LINE1"="black","LINE2"="red","LINE3"="blue", "LINE4"="purple")
p <- ggplot(data = dd, aes(x=as.Date(Date), y = value)) + geom_area(aes(fill = variable), position="identity") +
  xlab("Date") + ylab("Deaths") +
  scale_fill_manual(name = "Dataset",
                    values = alpha(c("red","darkmagenta","blue","black"),c(0.5,0.6,0.7,1)),
                    labels = c("BMJ: Low Est. w/ Sampling","BMJ: 10x Est.","BMJ: Low Est. w/ Sampling","Official Data"))
p



## Fit the spline model to the Higher estimated death data
df_fitting <- df_off_fil_full %>% select(Date, Est_Deaths_BMJ_Sam_HE) %>% rename(deaths = Est_Deaths_BMJ_Sam_HE, date = Date) %>%
  mutate(deaths = round(deaths))

fit <- fit_spline_rt(data = df_fitting,
                     country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                     population = pop_st_lu,
                     reporting_fraction = 1,
                     n_mcmc = 10000,
                     replicates = 100,
                     rw_duration = 14,
                     hosp_beds = 1e10,
                     icu_beds = 1e10)


# plot(fit, particle_fit = T)
# Estiamte seroprev and PCR+
sero_pcr <- seroprev_df(fit)
sero_pcr_tidy <- Summ_sero_pcr_data(sero_pcr)

# Plot pcr and sero using estimated death from BMJ
pdf(file = "analysis/figures/05_PCR_Sero_BMJ_RevEng_Deaths.pdf", height = 5)
plot(fit, particle_fit = T) +
  annotate(geom = "point", x= as.Date("2020-05-03"), y = 48) +
  annotate(geom = "text", x=as.Date("2020-05-07"), y = 48, hjust=0, label = "BMJ data estimated deaths") +
  annotate(geom = "segment", x=as.Date("2020-05-01"), xend = as.Date("2020-05-05"), y = 45, yend = 45, col = "red", alpha = 0.7) +
  annotate(geom = "rect", xmin=as.Date("2020-05-01"), xmax = as.Date("2020-05-05"), ymin = 44.8, ymax = 45.2, fill = NA, color = "black", linetype = 2) +
  annotate(geom = "text", x=as.Date("2020-05-07"), y = 45, hjust = 0, label = "Model fit")


p <- ggplot(sero_pcr_tidy, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.076)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=0.076+0.005, label="Mulenga et al.", hjust =0) +
  annotate("text", x=as.Date("2020-09-01"), y=0.02, label="Model fit", alpha=0.8) +
  geom_errorbar(aes(ymin=0.047,ymax=0.106,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.076, height=0)) +
  ylab(paste0("PCR % with ",100,"% death reporting"))
p
p <- ggplot(sero_pcr_tidy, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.021)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=0.021+0.005, label="Mulenga et al.", hjust =0) +
  annotate("text", x=as.Date("2020-09-01"), y=0.16, label="Model fit", alpha=0.8, hjust=0) +
  geom_errorbar(aes(ymin=0.011,ymax=0.031,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.021, height=0)) +
  geom_point(aes(x= as.Date("2020-07-29"),y=0.106), color="darkblue") +
  annotate("text", x=as.Date("2020-07-29")+15, y=0.106, label="Mulenga et al. \n (extrapolated \n from PCR)", color = "darkblue", hjust =0) +
  # geom_text(aes(label = "Mulenga et al. \n (extrapolated \n from PCR)", x= as.Date("2020-07-29")+15,y=0.106), color = "darkblue", hjust= 0) +
  geom_errorbar(aes(ymin=0.073,ymax=0.139,x=as.Date("2020-07-29"), width=10), color="darkblue") +
  geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=0.106, height=0), color="darkblue") +
  ylab(paste0("Sero+% with ",100,"% death reporting"))
p
dev.off()


# Fit the model to the 10x death data

df_fitting10 <- df_off_fil3 %>% select(Date, Est_Deaths_BMJ_10) %>% rename(deaths = Est_Deaths_BMJ_10, date = Date) %>%
  mutate(deaths = round(deaths))
fit10 <- fit_spline_rt(data = df_fitting10,
                       country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                       population = pop_st_lu,
                       reporting_fraction = 1,
                       n_mcmc = 10000,
                       replicates = 100,
                       rw_duration = 14,
                       hosp_beds = 1e10,
                       icu_beds = 1e10)
sero_pcr10 <- seroprev_df(fit10)
sero_pcr_tidy10 <- sero_pcr10  %>% group_by(date) %>%
  summarise(mean_pcr = mean(pcr_perc), min_pcr = min(pcr_perc), max_pcr = max(pcr_perc),
            mean_sero = mean(sero_perc), min_sero = min(sero_perc),max_sero = max(sero_perc))

plot(fit10, particle_fit = T) +
  annotate(geom = "point", x= as.Date("2020-05-03"), y = 48) +
  annotate(geom = "text", x=as.Date("2020-05-07"), y = 48, hjust=0, label = "BMJ data estimated deaths") +
  annotate(geom = "segment", x=as.Date("2020-05-01"), xend = as.Date("2020-05-05"), y = 45, yend = 45, col = "red", alpha = 0.7) +
  annotate(geom = "rect", xmin=as.Date("2020-05-01"), xmax = as.Date("2020-05-05"), ymin = 44.8, ymax = 45.2, fill = NA, color = "black", linetype = 2) +
  annotate(geom = "text", x=as.Date("2020-05-07"), y = 45, hjust = 0, label = "Model fit")


p <- ggplot(sero_pcr_tidy10, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.076)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=0.076+0.005, label="Mulenga et al.", hjust =0) +
  annotate("text", x=as.Date("2020-09-01"), y=0.02, label="Model fit", alpha=0.8) +
  geom_errorbar(aes(ymin=0.047,ymax=0.106,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.076, height=0)) +
  ylab(paste0("PCR % with ",100,"% death reporting"))
p
p <- ggplot(sero_pcr_tidy10, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.021)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=0.021+0.005, label="Mulenga et al.", hjust =0) +
  annotate("text", x=as.Date("2020-09-01"), y=0.16, label="Model fit", alpha=0.8, hjust=0) +
  geom_errorbar(aes(ymin=0.011,ymax=0.031,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.021, height=0)) +
  geom_point(aes(x= as.Date("2020-07-29"),y=0.106), color="darkblue") +
  annotate("text", x=as.Date("2020-07-29")+15, y=0.106, label="Mulenga et al. \n (extrapolated \n from PCR)", color = "darkblue", hjust =0) +
  # geom_text(aes(label = "Mulenga et al. \n (extrapolated \n from PCR)", x= as.Date("2020-07-29")+15,y=0.106), color = "darkblue", hjust= 0) +
  geom_errorbar(aes(ymin=0.073,ymax=0.139,x=as.Date("2020-07-29"), width=10), color="darkblue") +
  geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=0.106, height=0), color="darkblue") +
  ylab(paste0("Sero+% with ",100,"% death reporting"))
p


## So the underrerporting changes through time... Compare directly the reporting fractions:
df_off_fil4 <- df_off %>% filter(District=="Lusaka" & !is.na(Total_Deaths) & as.Date(Date) >= "2020-06-08" & as.Date(Date) <= "2020-09-27") %>%
  select("Date","Total_Deaths")

df_fitting

## Vary the fit with different reporting fractions.
##
## Q1: How is the IFR calculated?
# Multiply pop structure by IFRs given in report 34, then sum?
# I think they don't take into account higher deaths at the not included higher age categories
IFRZam <- sum(squire::get_population(country = "Zambia")[,"n"]/sum(squire::get_population(country = "Zambia")[,"n"]) * 100* fit$parameters$prob_hosp *(fit$parameters$prob_severe * fit$parameters$prob_severe_death_treatment +
                                                                                                                                                   (1-fit$parameters$prob_severe) * fit$parameters$prob_non_severe_death_treatment))
sum(squire::get_population(country = "Madagascar")[,"n"]/sum(squire::get_population(country = "Madagascar")[,"n"]) *
      100* fit$parameters$prob_hosp *(fit$parameters$prob_severe * fit$parameters$prob_severe_death_treatment + (1-fit$parameters$prob_severe) * fit$parameters$prob_non_severe_death_treatment))


fit <- fit_spline_rt(data = df_fitting,
                     country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                     population = pop_st_lu,
                     reporting_fraction = 1,
                     n_mcmc = 10000,
                     replicates = 100,
                     rw_duration = 14,
                     hosp_beds = 1e10,
                     icu_beds = 1e10,
                     prob_non_severe_death_treatment,
                     prob_severe_death_treatment)

parameters_explicit_SEEIR("Zambia")$prob_non_severe_death_treatment
parameters_explicit_SEEIR("Zambia")$prob_severe_death_treatment

## Create Scale
IFR_vec <- seq(0.05,0.25, length.out = 11)
IFR_sc_vec <- IFRZam/IFR_vec
Psdt_m <- ifelse(sapply(IFR_sc_vec, function(x){fit$parameters$prob_severe_death_treatment/x})>1,1,sapply(IFR_sc_vec, function(x){fit$parameters$prob_severe_death_treatment/x}))
Pnsdt_m <- sapply(IFR_sc_vec, function(x){fit$parameters$prob_non_severe_death_treatment/x})

sum(squire::get_population(country = "Zambia")[,"n"]/sum(squire::get_population(country = "Zambia")[,"n"]) * 100* fit$parameters$prob_hosp *(fit$parameters$prob_severe * fit$parameters$prob_severe_death_treatment+
                                                                                                                                               (1-fit$parameters$prob_severe) * fit$parameters$prob_non_severe_death_treatment))
sum(squire::get_population(country = "Zambia")[,"n"]/sum(squire::get_population(country = "Zambia")[,"n"]) * 100* fit$parameters$prob_hosp *(fit$parameters$prob_severe * Psdt_m[,3]+
                                                                                                                                               (1-fit$parameters$prob_severe) * Pnsdt_m[,3] ))
fit_l_IFR <- lapply(1:11, function(x){
  fit_spline_rt(data = df_fitting,
                       country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                       population = pop_st_lu,
                       reporting_fraction = 1,
                       n_mcmc = 10000,
                       replicates = 100,
                       rw_duration = 14,
                       hosp_beds = 1e10,
                       icu_beds = 1e10,
                       prob_non_severe_death_treatment = Pnsdt_m[,x],
                       prob_severe_death_treatment = Psdt_m[,x]
                       )
})

# saveRDS(object = fit_l_IFR, file = "../fit_var_IFR.rds")

Plot_List_IFR <- lapply(X = 1:11, FUN = function(x){p <- plot(fit_l_IFR[[x]], particle_fit = TRUE) + theme(legend.position = "none") +
  annotate(geom = "point", x= as.Date("2020-05-01"), y = 48) +
  annotate(geom = "text", x=as.Date("2020-05-07"), y = 48, hjust=0, label = "BMJ data estimated deaths") +
  annotate(geom = "segment", x=as.Date("2020-04-25"), xend = as.Date("2020-05-05"), y = 45, yend = 45, col = "red", alpha = 0.5, lwd = 2) +
  annotate(geom = "rect", xmin=as.Date("2020-04-25"), xmax = as.Date("2020-05-05"), ymin = 44.5, ymax = 45.5, fill = NA, color = "black", linetype = 2) +
  annotate(geom = "text", x=as.Date("2020-05-07"), y = 45, hjust = 0, label = "Model fit")})
pdf(file = "analysis/figures/05_02_Plot_Fits_var_IFR.pdf", width = 20, height = 7)
cowplot::plot_grid(Plot_List_IFR[[1]],Plot_List_IFR[[2]],Plot_List_IFR[[3]],Plot_List_IFR[[4]],Plot_List_IFR[[5]],Plot_List_IFR[[6]],Plot_List_IFR[[7]],Plot_List_IFR[[8]],Plot_List_IFR[[9]],Plot_List_IFR[[10]],Plot_List_IFR[[11]], nrow=2)
dev.off()

sero_pcr_df_l_IFR <- lapply(X = fit_l_IFR, FUN = function(x){
  seroprev_df(x)})

Simple_Plot_Data_l_IFR <- lapply(sero_pcr_df_l_IFR, function(x){x %>% group_by(date) %>%
    summarise(mean_pcr = mean(pcr_perc), min_pcr = min(pcr_perc), max_pcr = max(pcr_perc),
              mean_sero = mean(sero_perc), min_sero = min(sero_perc),max_sero = max(sero_perc))})

PCR_Plot_List_IFR <- lapply(X = 1:11, FUN = function(x){p <- ggplot(Simple_Plot_Data_l_IFR[[x]], aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.076)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=0.076+0.01, label="Mulenga et al.", hjust =0, size = 3) +
  annotate("text", x=as.Date("2020-09-15"), y=0.02, label="Model fit", alpha=0.8, size = 3) +
  geom_errorbar(aes(ymin=0.047,ymax=0.106,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.076, height=0)) +
  ylab(paste0("PCR % with ",IFR_vec[x]," IFR")) +
  theme_bw()})
Sero_Plot_List_IFR <- lapply(X = 1:11, FUN = function(x){p <- ggplot(Simple_Plot_Data_l_IFR[[x]], aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.021)) +
  geom_errorbar(aes(ymin=0.011,ymax=0.031,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.021, height=0)) +

  geom_point(aes(x= as.Date("2020-07-29"),y=0.106), color="darkblue") +
  geom_errorbar(aes(ymin=0.073,ymax=0.139,x=as.Date("2020-07-29"), width=10), color="darkblue") +
  geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=0.106, height=0), color="darkblue") +
  ylab(paste0("Sero+% with ",IFR_vec[x]," IFR")) +
  annotate("text", x=as.Date("2020-07-15")+5, y=0.021+0.02, label="Mulenga et al.", hjust =0, size = 3) +
  annotate("text", x=as.Date("2020-09-01"), y=max(Simple_Plot_Data_l_IFR[[x]]$max_sero)-0.01, label="Model fit", alpha=0.8, hjust=0, size = 3) +
  annotate("text", x=as.Date("2020-07-29")+15, y=0.106, label="Mulenga et al. \n (extrapolated \n from PCR)", color = "darkblue", hjust =0, size = 3) +
  theme_bw()})


# pdf(file = "analysis/figures/05_03_PCR_Sero_var_IFR.pdf", width = 8, height = 30)
# cowplot::plot_grid(PCR_Plot_List_IFR[[1]],Sero_Plot_List_IFR[[1]],
#                    PCR_Plot_List_IFR[[2]],Sero_Plot_List_IFR[[2]],
#                    PCR_Plot_List_IFR[[3]],Sero_Plot_List_IFR[[3]],
#                    PCR_Plot_List_IFR[[4]],Sero_Plot_List_IFR[[4]],
#                    PCR_Plot_List_IFR[[5]],Sero_Plot_List_IFR[[5]],
#                    PCR_Plot_List_IFR[[6]],Sero_Plot_List_IFR[[6]],
#                    PCR_Plot_List_IFR[[7]],Sero_Plot_List_IFR[[7]],
#                    PCR_Plot_List_IFR[[8]],Sero_Plot_List_IFR[[8]],
#                    PCR_Plot_List_IFR[[9]],Sero_Plot_List_IFR[[9]],
#                    PCR_Plot_List_IFR[[10]],Sero_Plot_List_IFR[[10]],
#                    PCR_Plot_List_IFR[[11]],Sero_Plot_List_IFR[[11]],nrow = 11)
# dev.off()

pdf(file = "analysis/figures/05_04_Fit_PCR_Sero_var_IFR.pdf", width = 12, height = 30)
cowplot::plot_grid(Plot_List_IFR[[1]],PCR_Plot_List_IFR[[1]],Sero_Plot_List_IFR[[1]],
                   Plot_List_IFR[[2]],PCR_Plot_List_IFR[[2]],Sero_Plot_List_IFR[[2]],
                   Plot_List_IFR[[3]],PCR_Plot_List_IFR[[3]],Sero_Plot_List_IFR[[3]],
                   Plot_List_IFR[[4]],PCR_Plot_List_IFR[[4]],Sero_Plot_List_IFR[[4]],
                   Plot_List_IFR[[5]],PCR_Plot_List_IFR[[5]],Sero_Plot_List_IFR[[5]],
                   Plot_List_IFR[[6]],PCR_Plot_List_IFR[[6]],Sero_Plot_List_IFR[[6]],
                   Plot_List_IFR[[7]],PCR_Plot_List_IFR[[7]],Sero_Plot_List_IFR[[7]],
                   Plot_List_IFR[[8]],PCR_Plot_List_IFR[[8]],Sero_Plot_List_IFR[[8]],
                   Plot_List_IFR[[9]],PCR_Plot_List_IFR[[9]],Sero_Plot_List_IFR[[9]],
                   Plot_List_IFR[[10]],PCR_Plot_List_IFR[[10]],Sero_Plot_List_IFR[[10]],
                   Plot_List_IFR[[11]],PCR_Plot_List_IFR[[11]],Sero_Plot_List_IFR[[11]],nrow = 11)
dev.off()


## So now, how do I fit the binomial likelihood to this?

Sero_Ests_1 <- unlist(lapply(1:11, function(x){mean(sero_pcr_df_l_IFR[[x]]$sero_perc[sero_pcr_df_l_IFR[[x]]$date ==as.Date("2020-07-15")])}))
Sero_Ests_2 <- unlist(lapply(1:11, function(x){mean(sero_pcr_df_l_IFR[[x]]$sero_perc[sero_pcr_df_l_IFR[[x]]$date ==as.Date("2020-07-29")])}))

par(mfrow=c(1,2))

plot(x = IFR_vec, log(dbinom(x = as.integer(0.021*4258), size = 4258, prob = Sero_Ests_1)), type = "l", xlab = "IFR", ylab = "log likelihood (15th July, 2.1% sero+)")
plot(x = IFR_vec, log(dbinom(x = as.integer(0.106*4258), size = 4258, prob = Sero_Ests_2)), type = "l", xlab = "IFR", ylab = "log likelihood  (29th July, 10.6% sero+)")
