##############################################################################
### 02. Fit the model to the Lusaka Data and vary over reporting fractions ###
##############################################################################

## See plots:
## 02_01_Plot_Fits_RepFrac.pdf (model fits: 0.1-1 reporting fraction)
## 02_02_PCR_Sero_RepFrac.pdf (PCR and Sero plots: 0.1-1 reporting fraction)
## 02_03_PCR_Sero_RepFrac_2.pdf (PCR and Sero plots: 0.2,0.3,0.5 and 1 reporting fractions)

rm(list=ls())
devtools::load_all(".")
library(squire)
library(tidyverse)
library(gridExtra)


## Bring in Lusaka Death data
data <- readRDS("analysis/data/Code-generated-data/00_01_Lusaka_Prov_Deaths_Official.rds")
# sum(data$deaths)

## Bring in population structure
pop_st_lu <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020_opendataforafrica.rds")
# sum(pop_st_lu)

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

mean(fit$replicate_parameters$rf)

# Plot the results
plot(fit, particle_fit = TRUE)

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
sero_pcr_df_l <- lapply(X = fit_l, FUN = function(x){seroprev_df(x)})

# Get range and means for each time point.
Simple_Plot_Data_l <- lapply(sero_pcr_df_l, Summ_sero_pcr_data)

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
pdf(file = "analysis/figures/02_02_PCR_Sero_RepFrac.pdf", width = 8, height = 28)
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
pdf(file = "analysis/figures/02_03_PCR_Sero_RepFrac.pdf", width = 8, height = 11)
cowplot::plot_grid(PCR_Plot_List[[10]],Sero_Plot_List[[10]],
                   PCR_Plot_List[[5]],Sero_Plot_List[[5]],
                   PCR_Plot_List[[3]],Sero_Plot_List[[3]],
                   PCR_Plot_List[[2]],Sero_Plot_List[[2]], ncol = 2)
dev.off()
