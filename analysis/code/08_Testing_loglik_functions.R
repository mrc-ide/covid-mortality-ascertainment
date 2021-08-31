devtools::load_all("../../Squire/squire/squire.Rproj")
# library(squire)
library(tidyverse)
devtools::load_all(".")


### Official statistics from the period:
df_off <- read.csv(file = "analysis/data/zambia_covid.csv")
# Filter by district and Date.
# Note: The data for the Province of Lusaka is essentially the same as the district (there was an additional 1 death in Chirundu)
df_off_fil <- df_off %>% filter(Province=="Lusaka" & !is.na(Total_Deaths) & as.Date(Date) >= "2020-06-08" & as.Date(Date) <= "2020-09-27") %>%
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
                     n_mcmc = 10000,
                     replicates = 100,
                     rw_duration = 14,
                     hosp_beds = 1e10,
                     icu_beds = 1e10,
                     prob_severe = rep(0,17),
                     prob_non_severe_death_treatment = prob_death_tot_IFR_frac,
                     dur_get_ox_survive =12,
                     dur_get_ox_die =10,
                     dur_R = Inf,
                     sero_df_start = as.Date(c("2020-07-04","2020-07-18")),
                     sero_df_end = as.Date(c("2020-07-27","2020-08-10")),
                     sero_df_pos = as.numeric(as.integer(c(0.021*4258, 0.106*4258))),
                     sero_df_samples = c(4258,4258),
                     pcr_df_start = as.Date(c("2020-07-04")),
                     pcr_df_end = as.Date(c("2020-07-27")),
                     pcr_df_pos = as.integer(c(0.076*4258)),
                     pcr_df_samples = c(4258)


)

fit

mean(fit$replicate_parameters$rf)
p1 <- plot(fit, particle_fit = TRUE) +
  annotate(geom = "point", x= as.Date("2020-03-03"), y = max(data$deaths/0.3)) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(data$deaths/0.3), hjust=0, label = "Official data estimated deaths") +
  annotate(geom = "segment", x=as.Date("2020-03-01"), xend = as.Date("2020-03-05"), y = max(data$deaths/0.3)-10, yend = max(data$deaths/0.3)-10, col = "red", alpha = 0.7) +
  annotate(geom = "rect", xmin=as.Date("2020-03-01"), xmax = as.Date("2020-03-05"), ymin = max(data$deaths/0.3)-10.2, ymax = max(data$deaths/0.3)-9.8, fill = NA, color = "black", linetype = 2) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(data$deaths/0.3)-10, hjust = 0, label = "Model fit") +
  theme_bw() + theme(legend.position = "none")

PlotData <- Summ_sero_pcr_data(seroprev_df(fit))

p2 <- ggplot(PlotData, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.021)) +
  geom_errorbar(aes(ymin=0.011,ymax=0.031,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.021, height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=0.021+0.005, label="Mulenga et al.", hjust =0) +
  # annotate("text", x=as.Date("2020-09-01"), y=0.18, label="Model fit", alpha=0.8, hjust=0) +

  geom_point(aes(x= as.Date("2020-07-29"),y=0.106), color="darkblue") +
  geom_errorbar(aes(ymin=0.073,ymax=0.139,x=as.Date("2020-07-29"), width=10), color="darkblue") +
  geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=0.106, height=0), color="darkblue") +
  annotate("text", x=as.Date("2020-07-29")+15, y=0.106, label="Mulenga et al. \n (combined measure)", color = "darkblue", hjust =0) +
  ylab(paste0("Sero+% with ",round(mean(fit$replicate_parameters$rf),3),"% death reporting")) +
  annotate("text", x=as.Date("2020-05-01"), y=0.18, label=paste(""), color = "darkblue", hjust =0) +
  theme_bw()


p3 <- ggplot(PlotData, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.076)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=0.076+0.005, label="Mulenga et al.", hjust =0) +
  # annotate("text", x=as.Date("2020-09-01"), y=0.02, label="Model fit", alpha=0.8) +
  geom_errorbar(aes(ymin=0.047,ymax=0.106,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.076, height=0)) +
  ylab(paste0("PCR % with ",round(mean(fit$replicate_parameters$rf),3),"% death reporting")) +
  theme_bw()

cowplot::plot_grid(p1,p3,p2,nrow = 1)

#######
test <- run_mcmc_func(n_mcmc = rep(n_mcmc, n_chains),
              curr_pars = pars_init[[1]],
              inputs = inputs,
              calc_lprior = calc_lprior,
              calc_ll = calc_ll,
              first_data_date = data$date[1],
              output_proposals = output_proposals,
              required_acceptance_ratio = required_acceptance_ratio,
              start_adaptation = start_adaptation,
              proposal_kernel = proposal_kernel,
              scaling_factor = scaling_factor,
              pars_discrete = pars_discrete,
              pars_min = pars_min,
              pars_max = pars_max)
