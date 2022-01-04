### Attack rate.
Off_Fit_Data <- readRDS(file = "../Bonus Files/p_01_Official_Data_Lancet_est_rf_resonable.rds")

# Cases/Persons at risk
# Off_Fit_Data$output[grep("cum_infs", colnames(Off_Fit_Data$output))]
# Off_Fit_Data$output[dim(Off_Fit_Data$output)[1],grep("cum_infs", colnames(Off_Fit_Data$output)),]
Cum_Death_Age <- rowMeans(Off_Fit_Data$output[dim(Off_Fit_Data$output)[1],grep("cum_infs", colnames(Off_Fit_Data$output)),])
PopStr <- Off_Fit_Data$parameters$population
plot(x = seq(2.5, 82.5, by = 5),
     y = Cum_Death_Age/PopStr,
     pch = 20,
     xlab = "Age", ylab = "Attack rate",
     main = "Squire fit to official data, Lancet PCR+ and sero+")

devtools::load_all(".")
library(squire)
library(reshape2)
library(tidyverse)
#####################################################################
### 1. Fit model to official data and lancet values: Estimate rf. ###
#####################################################################
data <- readRDS(file = "analysis/data/Code-generated-data/00_01_Lusaka_Prov_Deaths.rds") # See 00 data code
pop_st_lu <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds") # See 00 data code
prob_death_tot_IFR_frac <- readRDS("analysis/data/Code-generated-data/00_03_Tot_Prob_Death_By_Age_Zam.rds") # See 00 data code
Weighted_Durs_Hosp <- readRDS("analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds") # See 00 data code
Lancet_Data <- readRDS("analysis/data/Code-generated-data/00_07_Lancet_Data.rds") # See 00 data code

# squire::get_mixing_matrix(country = "Netherlands")

fit_Contact_Matrix <- fit_spline_rt(data = data,
                                    country = "Netherlands", # here you still need to say what country the data is from so the right contact matrix is loaded
                                    # country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                                    population = pop_st_lu,
                                    reporting_fraction = 1,
                                    reporting_fraction_bounds = c(0.25,0.1,1),
                                    n_mcmc = 10000,
                                    replicates = 100,
                                    rw_duration = 14,
                                    hosp_beds = 1e10,
                                    icu_beds = 1e10,
                                    prob_severe = rep(0,17),
                                    prob_non_severe_death_treatment = prob_death_tot_IFR_frac,
                                    dur_get_ox_survive =Weighted_Durs_Hosp$Surv_Dur_Weighted,
                                    dur_get_ox_die =Weighted_Durs_Hosp$Death_Dur_Weighted,
                                    dur_R = Inf,
                                    sero_df_start = as.Date(c("2020-07-04")),#,"2020-07-18")),
                                    sero_df_end = as.Date(c("2020-07-27")),#,"2020-08-10")),
                                    sero_df_pos = as.numeric(as.integer(c(Lancet_Data$Sero_prev["val"]/100*Lancet_Data$Sero_prev["n"]))),#, 0.106*1952))), # See Table 2
                                    sero_df_samples = Lancet_Data$Sero_prev["n"],#,1952),
                                    pcr_df_start = as.Date(c("2020-07-04")),
                                    pcr_df_end = as.Date(c("2020-07-27")),
                                    pcr_df_pos = as.integer(c(Lancet_Data$PCR_prev["val"]/100*Lancet_Data$PCR_prev["n"])), # See Table 2
                                    pcr_df_samples = Lancet_Data$PCR_prev["n"]
)

Sero_Pcr_df_1 <- seroprev_df(fit_Contact_Matrix)
PlotData_1 <- Summ_sero_pcr_data(Sero_Pcr_df_1)

p1 <-
  plot(fit_Contact_Matrix, particle_fit = TRUE) +
  geom_bar(data = data, stat = "identity", aes(x=date, y=deaths), alpha = 0.6) +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)/mean(fit_Contact_Matrix$replicate_parameters$rf), yend = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)/mean(fit_Contact_Matrix$replicate_parameters$rf), alpha = 0.5, size = 1.5) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)/mean(fit_Contact_Matrix$replicate_parameters$rf), hjust = 0, label = "Gov. reported deaths") +
  annotate(geom = "point", x= as.Date("2020-03-02"), y = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)*0.95/mean(fit_Contact_Matrix$replicate_parameters$rf)) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)*0.95/mean(fit_Contact_Matrix$replicate_parameters$rf), hjust=0, label = "Gov. reported deaths (scaled)") +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_Contact_Matrix$replicate_parameters$rf), yend = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_Contact_Matrix$replicate_parameters$rf), col = "red", alpha = 0.5, size = 1.5) +
  annotate(geom = "rect", xmin=as.Date("2020-02-28"), xmax = as.Date("2020-03-05"), ymin = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)*0.89/mean(fit_Contact_Matrix$replicate_parameters$rf), ymax = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)*0.91/mean(fit_Contact_Matrix$replicate_parameters$rf), fill = NA, color = "black", linetype = 2) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit_Contact_Matrix$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_Contact_Matrix$replicate_parameters$rf), hjust = 0, label = "Model fit") +
  theme_bw() + theme(legend.position = "none")

p2 <- ggplot(PlotData_1, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  annotate("text", x=as.Date("2020-10-01"), y=max(tail(PlotData_1$max_pcr))*2, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$PCR_prev["val"])) +
  geom_errorbar(aes(ymin=Lancet_Data$PCR_prev["ci95l"],ymax=Lancet_Data$PCR_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$PCR_prev["val"], height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$PCR_prev["val"]+0.5, label="Mulenga et al.", hjust=0) +

  ylab(paste0("PCR+ % with ",100*round(mean(fit_Contact_Matrix$replicate_parameters$rf),3),"% death reporting")) +
  xlab("Date") +
  theme_bw()

p3 <- ggplot(PlotData_1, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  annotate("text", x=as.Date("2020-10-01"), y=max(tail(PlotData_1$max_sero))*1.12, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$Sero_prev["val"])) +
  geom_errorbar(aes(ymin=Lancet_Data$Sero_prev["ci95l"],ymax=Lancet_Data$Sero_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$Sero_prev["val"], height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$Sero_prev["val"]+0.5, label="Mulenga et al.", hjust =0) +

  geom_point(aes(x= as.Date("2020-07-29"),y=Lancet_Data$Tot_cov_prev["val"]), color="darkblue") +
  geom_errorbar(aes(ymin=Lancet_Data$Tot_cov_prev["ci95l"],ymax=Lancet_Data$Tot_cov_prev["ci95h"],x=as.Date("2020-07-29"), width=10), color="darkblue") +
  geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=Lancet_Data$Tot_cov_prev["val"], height=0), color="darkblue") +
  annotate("text", x=as.Date("2020-07-29")+15, y=Lancet_Data$Tot_cov_prev["val"], label="Mulenga et al. \n (combined\n  measure)", color = "darkblue", hjust =0) +

  ylab(paste0("Sero+% with ",100*round(mean(fit_Contact_Matrix$replicate_parameters$rf),3),"% death reporting")) +
  xlab("Date") +
  theme_bw()





fit_Contact_Matrix$parameters
Par_Sims<- lapply(1:length(Off_Fit_Data$parameters), FUN = function(x){
  # browser()
  any(unlist(Off_Fit_Data$parameters[[x]]) != unlist(fit_Contact_Matrix$parameters[[x]]))
})
Off_Fit_Data$parameters[which(unlist(Par_Sims)==TRUE)]

Ranges_Neth <- apply(fit_Contact_Matrix$output[dim(fit_Contact_Matrix$output)[1],grep("cum_infs", colnames(fit_Contact_Matrix$output)),],
      1,
      quantile, probs = c(0.025, 0.975))

Ranges_Zamb <- apply(Off_Fit_Data$output[dim(Off_Fit_Data$output)[1],grep("cum_infs", colnames(Off_Fit_Data$output)),],
                     1,
                     quantile, probs = c(0.025, 0.975))



Cum_Death_Age_Neth <- rowMeans(fit_Contact_Matrix$output[dim(fit_Contact_Matrix$output)[1],grep("cum_infs", colnames(fit_Contact_Matrix$output)),])
plot(x = seq(2.5, 82.5, by = 5),
     y = Cum_Death_Age/PopStr,
     pch = 20, col = 1,
     xlab = "Age", ylab = "Attack rate", ylim = c(0,0.4),
     main = "Squire fit to official data, Lancet PCR+ and sero+")
points(x = seq(2.5, 82.5, by = 5),
     y = Cum_Death_Age_Neth/PopStr,
     pch = 20, col = 2,
     xlab = "Age", ylab = "Attack rate",
     main = "Squire fit to official data, Lancet PCR+ and sero+")
legend("topleft", legend = c("Zambia Contact Matrix", "Netherlands Contact Matrix"), col = 1:2, pch = 20)

Neth_Con_Mat <- squire::get_mixing_matrix(country = "Netherlands")
Zamb_Con_Mat <- squire::get_mixing_matrix(country = "Zambia")
rownames(Neth_Con_Mat) <- 1:16
colnames(Neth_Con_Mat) <- 1:16
rownames(Zamb_Con_Mat) <- 1:16
colnames(Zamb_Con_Mat) <- 1:16

par(mfrow=c(1,1))


LancetData <- rbind(c(4.1,0.7,7.4),
                    c(8.1,3.8,12.3),
                    c(10.3,6.6,14),
                    c(14.4,4.9,23.9),
                    c(5.4,1.4,9.4),
                    c(16.220899470899475,0.14579659024103542,32.123769106408005),
                    c(11.570601851851857, 1.7533068783068817, 21.445307907113467),
                    c(13.43646200764257, 2.499650940623166,24.344567533803648))


plot(x = seq(2.5, 82.5, by = 5),
     y = Cum_Death_Age/PopStr,
     pch = 20, col = 1,
     xlab = "Age", ylab = "Attack rate/Prevalence", ylim = c(0,0.4),
     main = "Squire fit to official data, Lancet PCR+ and sero+")
arrows(seq(2.5, 82.5, by = 5), Ranges_Zamb[2,]/PopStr, seq(2.5, 82.5, by = 5), Ranges_Zamb[1,]/PopStr, angle=90, code=3, length = 0.1, lwd=0.5)
points(x = seq(2.5, 82.5, by = 5),
       y = Cum_Death_Age_Neth/PopStr,
       pch = 20, col = "red",
       xlab = "Age", ylab = "Attack rate",
       main = "Squire fit to official data, Lancet PCR+ and sero+")
arrows(seq(2.5, 82.5, by = 5), Ranges_Neth[2,]/PopStr, seq(2.5, 82.5, by = 5), Ranges_Neth[1,]/PopStr, angle=90, code=3, col = "red", length = 0.1, lwd = 0.5)
points(x = seq(5,75, by = 10), y = LancetData[,1]/100, col = "darkblue", pch = 20)
arrows(seq(5,75, by = 10), LancetData[,3]/100, seq(5,75, by = 10), LancetData[,2]/100, angle=90, code=3, col = "darkblue", length = 0.1, lwd = 0.5)
legend("topleft", legend = c("AR: Zambia Contact Matrix", "AR: Netherlands Contact Matrix", "Lancet PCR prevalence"), col = c(1,"red","darkblue"), pch = 20)
# heatmap(Neth_Con_Mat, Rowv = NA, Colv = NA, main = "Netherlands")
# heatmap(Zamb_Con_Mat, Rowv = NA, Colv = NA, main = "Zambia")



fit_Contact_Matrix_2 <- fit_spline_rt(data = data,
                                    country = "Netherlands", # here you still need to say what country the data is from so the right contact matrix is loaded
                                    # country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                                    population = get_population("Netherlands")$n,
                                    reporting_fraction = 1,
                                    reporting_fraction_bounds = c(0.25,0.1,1),
                                    n_mcmc = 10000,
                                    replicates = 100,
                                    rw_duration = 14,
                                    hosp_beds = 1e10,
                                    icu_beds = 1e10,
                                    prob_severe = rep(0,17),
                                    prob_non_severe_death_treatment = prob_death_tot_IFR_frac,
                                    dur_get_ox_survive =Weighted_Durs_Hosp$Surv_Dur_Weighted,
                                    dur_get_ox_die =Weighted_Durs_Hosp$Death_Dur_Weighted,
                                    dur_R = Inf,
                                    sero_df_start = as.Date(c("2020-07-04")),#,"2020-07-18")),
                                    sero_df_end = as.Date(c("2020-07-27")),#,"2020-08-10")),
                                    sero_df_pos = as.numeric(as.integer(c(Lancet_Data$Sero_prev["val"]/100*Lancet_Data$Sero_prev["n"]))),#, 0.106*1952))), # See Table 2
                                    sero_df_samples = Lancet_Data$Sero_prev["n"],#,1952),
                                    pcr_df_start = as.Date(c("2020-07-04")),
                                    pcr_df_end = as.Date(c("2020-07-27")),
                                    pcr_df_pos = as.integer(c(Lancet_Data$PCR_prev["val"]/100*Lancet_Data$PCR_prev["n"])), # See Table 2
                                    pcr_df_samples = Lancet_Data$PCR_prev["n"]
)


Sero_Pcr_df_2 <- seroprev_df(fit_Contact_Matrix_2)
PlotData_2 <- Summ_sero_pcr_data(Sero_Pcr_df_2)

p1 <-
  plot(fit_Contact_Matrix_2, particle_fit = TRUE) +
  geom_bar(data = data, stat = "identity", aes(x=date, y=deaths), alpha = 0.6) +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)/mean(fit_Contact_Matrix_2$replicate_parameters$rf), yend = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)/mean(fit_Contact_Matrix_2$replicate_parameters$rf), alpha = 0.5, size = 1.5) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)/mean(fit_Contact_Matrix_2$replicate_parameters$rf), hjust = 0, label = "Gov. reported deaths") +
  annotate(geom = "point", x= as.Date("2020-03-02"), y = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)*0.95/mean(fit_Contact_Matrix_2$replicate_parameters$rf)) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)*0.95/mean(fit_Contact_Matrix_2$replicate_parameters$rf), hjust=0, label = "Gov. reported deaths (scaled)") +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_Contact_Matrix_2$replicate_parameters$rf), yend = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_Contact_Matrix_2$replicate_parameters$rf), col = "red", alpha = 0.5, size = 1.5) +
  annotate(geom = "rect", xmin=as.Date("2020-02-28"), xmax = as.Date("2020-03-05"), ymin = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)*0.89/mean(fit_Contact_Matrix_2$replicate_parameters$rf), ymax = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)*0.91/mean(fit_Contact_Matrix_2$replicate_parameters$rf), fill = NA, color = "black", linetype = 2) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit_Contact_Matrix_2$pmcmc_results$inputs$data$deaths)*0.9/mean(fit_Contact_Matrix_2$replicate_parameters$rf), hjust = 0, label = "Model fit") +
  theme_bw() + theme(legend.position = "none")

p2 <- ggplot(PlotData_2, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  annotate("text", x=as.Date("2020-10-01"), y=max(tail(PlotData_2$max_pcr))*2, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$PCR_prev["val"])) +
  geom_errorbar(aes(ymin=Lancet_Data$PCR_prev["ci95l"],ymax=Lancet_Data$PCR_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$PCR_prev["val"], height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$PCR_prev["val"]+0.5, label="Mulenga et al.", hjust=0) +

  ylab(paste0("PCR+ % with ",100*round(mean(fit_Contact_Matrix_2$replicate_parameters$rf),3),"% death reporting")) +
  xlab("Date") +
  theme_bw()

p3 <- ggplot(PlotData_2, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  annotate("text", x=as.Date("2020-10-01"), y=max(tail(PlotData_2$max_sero))*1.12, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$Sero_prev["val"])) +
  geom_errorbar(aes(ymin=Lancet_Data$Sero_prev["ci95l"],ymax=Lancet_Data$Sero_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$Sero_prev["val"], height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$Sero_prev["val"]+0.5, label="Mulenga et al.", hjust =0) +

  geom_point(aes(x= as.Date("2020-07-29"),y=Lancet_Data$Tot_cov_prev["val"]), color="darkblue") +
  geom_errorbar(aes(ymin=Lancet_Data$Tot_cov_prev["ci95l"],ymax=Lancet_Data$Tot_cov_prev["ci95h"],x=as.Date("2020-07-29"), width=10), color="darkblue") +
  geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=Lancet_Data$Tot_cov_prev["val"], height=0), color="darkblue") +
  annotate("text", x=as.Date("2020-07-29")+15, y=Lancet_Data$Tot_cov_prev["val"], label="Mulenga et al. \n (combined\n  measure)", color = "darkblue", hjust =0) +

  ylab(paste0("Sero+% with ",100*round(mean(fit_Contact_Matrix_2$replicate_parameters$rf),3),"% death reporting")) +
  xlab("Date") +
  theme_bw()

Cum_Death_Age_Neth_2 <- rowMeans(fit_Contact_Matrix_2$output[dim(fit_Contact_Matrix_2$output)[1],grep("cum_infs", colnames(fit_Contact_Matrix_2$output)),])
Ranges_Neth2 <- apply(fit_Contact_Matrix_2$output[dim(fit_Contact_Matrix_2$output)[1],grep("cum_infs", colnames(fit_Contact_Matrix_2$output)),],
                     1,
                     quantile, probs = c(0.025, 0.975))
plot(x = seq(2.5, 82.5, by = 5),
     y = Cum_Death_Age/PopStr,
     pch = 20, col = 1,
     xlab = "Age", ylab = "Attack rate/Prevalence", ylim = c(0,0.4),
     main = "Squire fit to official data, Lancet PCR+ and sero+")
arrows(seq(2.5, 82.5, by = 5), Ranges_Zamb[2,]/PopStr, seq(2.5, 82.5, by = 5), Ranges_Zamb[1,]/PopStr, angle=90, code=3, length = 0.1, lwd=0.5)
points(x = seq(2.5, 82.5, by = 5),
       y = Cum_Death_Age_Neth_2/fit_Contact_Matrix_2$parameters$population,
       pch = 20, col = "red",
       xlab = "Age", ylab = "Attack rate",
       main = "Squire fit to official data, Lancet PCR+ and sero+")
arrows(seq(2.5, 82.5, by = 5), Ranges_Neth2[2,]/fit_Contact_Matrix_2$parameters$population, seq(2.5, 82.5, by = 5), Ranges_Neth2[1,]/fit_Contact_Matrix_2$parameters$population, angle=90, code=3, col = "red", length = 0.1, lwd = 0.5)
points(x = seq(5,75, by = 10), y = LancetData[,1]/100, col = "darkblue", pch = 20)
arrows(seq(5,75, by = 10), LancetData[,3]/100, seq(5,75, by = 10), LancetData[,2]/100, angle=90, code=3, col = "darkblue", length = 0.1, lwd = 0.5)
# legend("topleft", legend = c("AR: Zambia Contact Matrix", "AR: Netherlands Contact Matrix", "Lancet PCR prevalence"), col = c(1,"red","darkblue"), pch = 20)
legend("topleft", legend = c("AR: Zambia Contact Matrix", "AR: Netherlands Contact Matrix"), col = c(1,"red"), pch = 20)
