devtools::load_all(".")
library(tidyverse)
# library(gsheet)
library(gridExtra)
# library(reshape2)


## Bring in Zambia data and filter Lusaka
# df <- as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1qKPsUZivONw8n_ZDh9mle46n3DHcdk-pa89Ag4ihJeM/edit#gid=0'))
df <- read.csv(file = "analysis/data/zambia_covid.csv")


# Filter deaths for Lusaka Province until November 2020
data <- df %>%
  filter(Province=="Lusaka" & as.Date(Date) < "2020-11-01") %>%
  select(Date, Total_Deaths) %>%
  na.omit() %>%
  group_by(Date) %>% summarise(Total_Deaths = sum(Total_Deaths)) %>%
  rename(deaths = Total_Deaths, date = Date)

data$date <- as.Date(data$date)
date_list <- seq(min(data$date), max(data$date), by = 1)
missing_dates <- date_list[!date_list %in% data$date]

data <-add_row(data, date = missing_dates, deaths = 0)

# 2020 Age distribution for Lusaka Province from opendataforafrica (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
pop_st_lu <- c(549475,448008,379841,339600,317148,305521,271318,232188,172365,125531,75157,51883,35587,22219,14960,8570,10812)


# Bulawayo contact matrix:
# Bula_Con_Mat<- read.csv("analysis/data/Bulawayo-Contact-Matrix.csv", row.names = 1, header = T, nrows = 16)
# Choose different contact matrix:

## Initial run
fit <- fit_spline_rt(data = data,
                     country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                     population = pop_st_lu,
                     reporting_fraction = 0.1,
                     n_mcmc = 10000,
                     replicates = 100,
                     rw_duration = 14,
                     hosp_beds = 1e10,
                     icu_beds = 1e10)

## Testing fit consistency
# fit_l <- lapply(X = 1:6, FUN = function(x){
#   fit_spline_rt(data = data,
#                 country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
#                 population = pop_st_lu,
#                 reporting_fraction = 1,
#                 n_mcmc = 10000,
#                 replicates = 100,
#                 rw_duration = 14,
#                 hosp_beds = 1e10,
#                 icu_beds = 1e10)
# })

# Across range of reporting fractions DON'T RUN THIS AGAIN IF POSSIBLE IT TOOK FOREVER
fit_l <- lapply(X = seq(0.1,1,0.1), FUN = function(x){
  fit_spline_rt(data = data,
                country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                population = pop_st_lu,
                reporting_fraction = x,
                n_mcmc = 10000,
                replicates = 100,
                rw_duration = 14,
                hosp_beds = 1e10,
                icu_beds = 1e10)
})
## Still need to add new contact matrix - Bulawayo

# Save/Load Data
# lapply(1:10, FUN = function(x){saveRDS(fit_l[[x]], file = paste0("analysis/fit_RepFrac_",x,".rds"))})
# save(fit_l,file = "../fit_RepFrac.Rdata")
# saveRDS(object = fit_l, file = "../fit_RepFrac.rds")
load(file = "../fit_RepFrac.Rdata")

## Plot particle fits at each reporting fraction
Plot_List <- lapply(X = 1:10, FUN = function(x){p <- plot(fit_l[[x]], particle_fit = TRUE)})
pdf(file = "analysis/figures/02_01_Plot_Fits_RepFrac.pdf", width = 15, height = 7.5)
cowplot::plot_grid(Plot_List[[1]],Plot_List[[2]],Plot_List[[3]],Plot_List[[4]],Plot_List[[5]],Plot_List[[6]],Plot_List[[7]],Plot_List[[8]],Plot_List[[9]],Plot_List[[10]], nrow=2)
dev.off()

# Get PCR/Seroprev data
# sero_pcr_df <- seroprev_df(fit)
sero_pcr_df_l <- lapply(X = fit_l, FUN = function(x){
  seroprev_df(x)})

# ggplot(sero_pcr_df_l[[1]], aes(date, pcr_perc)) + geom_line() + ylab("PCR Prevalence") + scale_y_continuous(labels = scales::percent)
# ggplot(sero_pcr_df, aes(date, sero_perc)) + geom_line() + ylab("Seroprevalence") + scale_y_continuous(labels = scales::percent)

# Get range and means for each time point.
Simple_Plot_Data_l <- lapply(sero_pcr_df_l, function(x){x %>% group_by(date) %>%
    summarise(mean_pcr = mean(pcr_perc), min_pcr = min(pcr_perc), max_pcr = max(pcr_perc),
              mean_sero = mean(sero_perc), min_sero = min(sero_perc),max_sero = max(sero_perc))})

# ggplot(Simple_Plot_Data_l[[1]], aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr)) +
#   geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
#   geom_point(aes(x= as.Date("2020-07-15"),y=0.076)) +
#   geom_errorbar(aes(ymin=0.047,ymax=0.106,x=as.Date("2020-07-15"), width=3)) +
#   geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.076, height=0.01)) +
#   ylab(paste0("PCR % with ",seq(0.1,1,0.1)[1]*100,"% death reporting"))

# Create summary plots for PCR and Sero+ over reporting fractions
PCR_Plot_List <- lapply(X = 1:10, FUN = function(x){p <- ggplot(Simple_Plot_Data_l[[x]], aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.076)) +
  geom_errorbar(aes(ymin=0.047,ymax=0.106,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.076, height=0)) +
  ylab(paste0("PCR % with ",seq(0.1,1,0.1)[x]*100,"% death reporting"))})
Sero_Plot_List <- lapply(X = 1:10, FUN = function(x){p <- ggplot(Simple_Plot_Data_l[[x]], aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
  geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
  geom_point(aes(x= as.Date("2020-07-15"),y=0.021)) +
  geom_errorbar(aes(ymin=0.011,ymax=0.031,x=as.Date("2020-07-15"), width=10)) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.021, height=0)) +

  geom_point(aes(x= as.Date("2020-07-29"),y=0.106), color="darkblue") +
  geom_errorbar(aes(ymin=0.073,ymax=0.139,x=as.Date("2020-07-29"), width=10), color="darkblue") +
  geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=0.106, height=0), color="darkblue") +
  ylab(paste0("Sero+% with ",seq(0.1,1,0.1)[x]*100,"% death reporting"))})


# Plot everything
pdf(file = "analysis/figures/02_03_PCR_Sero_RepFrac3.pdf", width = 8, height = 28)
cowplot::plot_grid(PCR_Plot_List[[1]],Sero_Plot_List[[1]],
                   PCR_Plot_List[[2]],Sero_Plot_List[[2]],
                   PCR_Plot_List[[3]],Sero_Plot_List[[3]],
                   PCR_Plot_List[[4]],Sero_Plot_List[[4]],
                   PCR_Plot_List[[5]],Sero_Plot_List[[5]],
                   PCR_Plot_List[[6]],Sero_Plot_List[[6]],
                   PCR_Plot_List[[7]],Sero_Plot_List[[7]],
                   PCR_Plot_List[[8]],Sero_Plot_List[[8]],
                   PCR_Plot_List[[9]],Sero_Plot_List[[9]],
                   PCR_Plot_List[[10]],Sero_Plot_List[[10]], nrow = 10)
dev.off()

# Plot to send to collaborators
pdf(file = "analysis/figures/02_03_PCR_Sero_RepFrac4.pdf", width = 8, height = 11)
cowplot::plot_grid(PCR_Plot_List[[10]],Sero_Plot_List[[10]],
                   PCR_Plot_List[[5]],Sero_Plot_List[[5]],
                   PCR_Plot_List[[3]],Sero_Plot_List[[3]],
                   PCR_Plot_List[[2]],Sero_Plot_List[[2]], ncol = 2)
dev.off()




# ggplot(sero_pcr_df_l[[1]], aes(x = date, y = pcr_perc)) + #geom_line() +
#   geom_ribbon(aes(ymin=min(pcr_perc), ymax=max(pcr_perc)), orientation = "y")+
#   geom_point(aes(x= as.Date("2020-07-15"),y=0.076)) +
#   geom_errorbar(aes(ymin=0.047,ymax=0.106,x=as.Date("2020-07-15"), width=3)) +
#   geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=0.076, height=0.01)) +
#     # geom_vline(xintercept = as.Date(c("2020-07-04", "2020-07-27"))) +
#   # geom_hline(yintercept = 0.076) +
#   # geom_text(x=as.Date("2020-10-01"), y=0.076, label = "\nPCR Prev. (7.6%)", size = 2) +
#   ylab(paste0("PCR % with ",seq(0.1,1,0.1)[1]*100,"% death reporting"))

# PCR_Plot_List <- lapply(X = 1:10, FUN = function(x){p <- ggplot(sero_pcr_df_l[[x]], aes(date, pcr_perc, group = replicate)) + geom_line() + geom_vline(xintercept = as.Date(c("2020-07-04", "2020-07-27"))) + geom_hline(yintercept = 0.076) + geom_text(x=as.Date("2020-10-01"), y=0.076, label = "\nPCR Prev. (7.6%)", size = 2) + ylab(paste0("PCR % with ",seq(0.1,1,0.1)[x]*100,"% death reporting"))})
# Sero_Plot_List <- lapply(X = 1:10, FUN = function(x){p <- ggplot(sero_pcr_df_l[[x]], aes(date, sero_perc, group = replicate)) + geom_line() + geom_vline(xintercept = as.Date(c("2020-07-04", "2020-07-27"))) + geom_hline(yintercept = 0.021)+ geom_text(x=as.Date("2020-10-01"), y=0.021, label = "\nSero+ Prev. (2.1%)", size = 2) + ylab(paste0("Sero+ % with ",seq(0.1,1,0.1)[x]*100,"% death reporting"))})
# pdf(file = "analysis/figures/02_02_PCR_Sero_RepFrac.pdf", width = 15, height = 6)
# cowplot::plot_grid(PCR_Plot_List[[1]],PCR_Plot_List[[2]],PCR_Plot_List[[3]],PCR_Plot_List[[4]],PCR_Plot_List[[5]],PCR_Plot_List[[6]],PCR_Plot_List[[7]],PCR_Plot_List[[8]],PCR_Plot_List[[9]],PCR_Plot_List[[10]], nrow = 2)
# cowplot::plot_grid(Sero_Plot_List[[1]],Sero_Plot_List[[2]],Sero_Plot_List[[3]],Sero_Plot_List[[4]],Sero_Plot_List[[5]],Sero_Plot_List[[6]],Sero_Plot_List[[7]],Sero_Plot_List[[8]],Sero_Plot_List[[9]],Sero_Plot_List[[10]], nrow = 2)
# dev.off()

# grid.arrange(PCR_Plot_List[[1]],PCR_Plot_List[[2]])#,PCR_Plot_List[[3]],PCR_Plot_List[[4]],PCR_Plot_List[[5]],PCR_Plot_List[[6]],PCR_Plot_List[[7]],PCR_Plot_List[[8]],PCR_Plot_List[[9]],PCR_Plot_List[[10]], nrow = 2)

# png(file = "analysis/figures/02_03_PCR_Sero_RepFrac2.png", width = 8, height = 28, units= "in", res = 300)
# cowplot::plot_grid(PCR_Plot_List[[1]],Sero_Plot_List[[1]],
#                    PCR_Plot_List[[2]],Sero_Plot_List[[2]],
#                    PCR_Plot_List[[3]],Sero_Plot_List[[3]],
#                    PCR_Plot_List[[4]],Sero_Plot_List[[4]],
#                    PCR_Plot_List[[5]],Sero_Plot_List[[5]],
#                    PCR_Plot_List[[6]],Sero_Plot_List[[6]],
#                    PCR_Plot_List[[7]],Sero_Plot_List[[7]],
#                    PCR_Plot_List[[8]],Sero_Plot_List[[8]],
#                    PCR_Plot_List[[9]],Sero_Plot_List[[9]],
#                    PCR_Plot_List[[10]],Sero_Plot_List[[10]], nrow = 10)
# dev.off()

# g1 <- ggplot(seroprev_df(out02), aes(date, pcr_perc, group = replicate)) + geom_line() + geom_vline(xintercept = as.Date(c("2020-07-01", "2020-07-21"))) + geom_hline(yintercept = 0.076) + ylab("PCR % with 20% death reporting")
# g2 <- ggplot(seroprev_df(out05), aes(date, sero_perc, group = replicate)) + geom_line() + geom_vline(xintercept = as.Date(c("2020-07-01", "2020-07-21"))) + geom_hline(yintercept = 0.021) + ylab("Sero+ % with 50% death reporting")
# cowplot::plot_grid(g1, g2)


# Sero_Plot_List <- lapply(sero_pcr_df_l, function(x){ggplot(x, aes(date, sero_perc)) + geom_line() + ylab("Seroprevalence") + scale_y_continuous(labels = scales::percent)})
# grid.arrange(Sero_Plot_List[[1]],Sero_Plot_List[[2]],Sero_Plot_List[[3]],Sero_Plot_List[[4]],Sero_Plot_List[[5]],Sero_Plot_List[[6]], nrow=2)

# ggplot(sero_pcr_df, aes(date, sero_perc)) + geom_line() + ylab("Seroprevalence") + scale_y_continuous(labels = scales::percent)

# Seroprev is supposed to be 2% in July.
# PCR is supposed to be 8%.
