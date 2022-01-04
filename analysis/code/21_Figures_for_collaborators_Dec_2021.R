####
# Figures for Collaborator presentation December 2021

library(dplyr)
devtools::load_all(".")


## Plot Mortality data without fit
## plot PCR point
## Plot sero point
Full_Data <- readRDS("../Bonus Files/fit_l_IFR_slope_Total_Covid.rds")

plot(Full_Data[[1]], particle_fit = T)

df_1 <- Full_Data[[1]]$pmcmc_results$inputs$data %>%
  mutate(Date = as.Date(date), Deaths = deaths)

p1 <- ggplot(data = df_1, aes(x = date, y = deaths)) + geom_point() + xlim(as.Date("2020-04-10"),as.Date("2020-10-01")) +
  theme_bw() +
  xlab("Date") + ylab("Deaths")

Lancet_Data <- readRDS("analysis/data/Code-generated-data/00_07_Lancet_Data.rds")

p2 <- ggplot() + geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$PCR_prev["val"])) +
  geom_errorbar(aes(ymin=Lancet_Data$PCR_prev["ci95l"],ymax=Lancet_Data$PCR_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$PCR_prev["val"], height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$PCR_prev["val"]+0.5, label="Mulenga et al.", hjust=0) +

  ylab(paste0("PCR +ve %")) +
  xlab("Date") + xlim(as.Date("2020-04-10"),as.Date("2020-10-01")) + ylim(0,31) +
  theme_bw() + xlim(as.Date("2020-04-10"),as.Date("2020-10-01"))



p3 <- ggplot() +
  geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$Sero_prev["val"])) +
    geom_errorbar(aes(ymin=Lancet_Data$Sero_prev["ci95l"],ymax=Lancet_Data$Sero_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$Sero_prev["val"], height=0)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$Sero_prev["val"]+0.5, label="Mulenga et al.", hjust =0) +

    # geom_point(aes(x= as.Date("2020-07-29"),y=Lancet_Data$Tot_cov_prev["val"]), color="darkblue") +
    # geom_errorbar(aes(ymin=Lancet_Data$Tot_cov_prev["ci95l"],ymax=Lancet_Data$Tot_cov_prev["ci95h"],x=as.Date("2020-07-29"), width=10), color="darkblue") +
    # geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=Lancet_Data$Tot_cov_prev["val"], height=0), color="darkblue") +
    # annotate("text", x=as.Date("2020-07-29")+15, y=Lancet_Data$Tot_cov_prev["val"], label="Mulenga et al. \n (combined\n  measure)", color = "darkblue", hjust =0) +
    #
    ylab(paste0("Sero +ve %")) +
    xlab("Date") + xlim(as.Date("2020-04-10"),as.Date("2020-10-01")) + ylim(0,40) +
    theme_bw()

cowplot::plot_grid(p1,p3,p2, nrow = 1)

## Now standard fits:
fit <- Full_Data[[36]]
fit <- Full_Data[[40]]
fit <- Full_Data[[34]]
p1 <- plot(fit, particle_fit = TRUE) +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit$pmcmc_results$inputs$data$deaths)/mean(fit$replicate_parameters$rf), yend = max(fit$pmcmc_results$inputs$data$deaths)/mean(fit$replicate_parameters$rf), alpha = 0.5, size = 1.5) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit$pmcmc_results$inputs$data$deaths)/mean(fit$replicate_parameters$rf), hjust = 0, label = "Gov. reported deaths") +
  annotate(geom = "point", x= as.Date("2020-03-02"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.95/mean(fit$replicate_parameters$rf)) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.95/mean(fit$replicate_parameters$rf), hjust=0, label = "Gov. reported deaths (scaled)") +
  annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.9/mean(fit$replicate_parameters$rf), yend = max(fit$pmcmc_results$inputs$data$deaths)*0.9/mean(fit$replicate_parameters$rf), col = "red", alpha = 0.5, size = 1.5) +
  annotate(geom = "rect", xmin=as.Date("2020-02-28"), xmax = as.Date("2020-03-05"), ymin = max(fit$pmcmc_results$inputs$data$deaths)*0.89/mean(fit$replicate_parameters$rf), ymax = max(fit$pmcmc_results$inputs$data$deaths)*0.91/mean(fit$replicate_parameters$rf), fill = NA, color = "black", linetype = 2) +
  annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.9/mean(fit$replicate_parameters$rf), hjust = 0, label = "Model fit") +
  theme_bw() + theme(legend.position = "none") + xlim(as.Date("2020-04-10"),as.Date("2020-10-01"))

PCR_Sero_PlotData <- Summ_sero_pcr_data(seroprev_df(fit))

p2 <- ggplot(PCR_Sero_PlotData, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  annotate("text", x=as.Date("2020-10-01"), y=max(tail(PCR_Sero_PlotData$max_pcr))*2, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$PCR_prev["val"])) +
  geom_errorbar(aes(ymin=Lancet_Data$PCR_prev["ci95l"],ymax=Lancet_Data$PCR_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$PCR_prev["val"], height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$PCR_prev["val"]+0.5, label="Mulenga et al.", hjust=0) +

  ylab(paste0("PCR +ve %")) +
  xlab("Date") +
  theme_bw() + ylim(0,31) + xlim(as.Date("2020-04-10"),as.Date("2020-10-01"))

p3 <- ggplot(PCR_Sero_PlotData, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  annotate("text", x=as.Date("2020-09-25"), y=max(tail(PCR_Sero_PlotData$max_sero))*1.05, label="Model fit", alpha=0.8) +

  geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$Sero_prev["val"])) +
  geom_errorbar(aes(ymin=Lancet_Data$Sero_prev["ci95l"],ymax=Lancet_Data$Sero_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$Sero_prev["val"], height=0)) +
  annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$Sero_prev["val"]+0.5, label="Mulenga et al.", hjust =0) +

  # geom_point(aes(x= as.Date("2020-07-29"),y=Lancet_Data$Tot_cov_prev["val"]), color="darkblue") +
  # geom_errorbar(aes(ymin=Lancet_Data$Tot_cov_prev["ci95l"],ymax=Lancet_Data$Tot_cov_prev["ci95h"],x=as.Date("2020-07-29"), width=10), color="darkblue") +
  # geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=Lancet_Data$Tot_cov_prev["val"], height=0), color="darkblue") +
  # annotate("text", x=as.Date("2020-07-29")+15, y=Lancet_Data$Tot_cov_prev["val"], label="Mulenga et al. \n (combined\n  measure)", color = "darkblue", hjust =0) +

  ylab(paste0("Sero +ve %")) +
  xlab("Date") +
  theme_bw() + ylim(0,40)+ xlim(as.Date("2020-04-10"),as.Date("2020-10-01"))


cowplot::plot_grid(p1,p3,p2, nrow = 1)


# df1 <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv")
df1 <- readRDS(file = "analysis/data/Code-generated-data/00_05c_BMJ_Data_full.rds")

df1 %>% mutate(PosPerc)
