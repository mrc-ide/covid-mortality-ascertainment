#### Analysis with the data:
rm(list = ls())
devtools::load_all(".")
library(squire)
library(reshape2)
library(tidyverse)

# Things that still need discussing:
# 1. We would still like to get the contact matrix for the peri-urban area of Manicaland.
# 4. How are we fitting the reporting fraction of the official deaths? Allow it to vary or set it at a specific value?
## What are the pros and cons?

################################################################################
# Part 1. Estimate SARS-CoV-2 prevalence during the first wave #################
################################################################################

### 1a. Vary age-stratified infection fatality ratios for use in analysis
# Load Brazeau IFR data (report 34)
Braz_IFR <- readRDS("analysis/data/Code-generated-data/00_03_IFR_values_Brazeau.rds")

# Estimate total IFR for Lusaka population (not age stratified)
# This requires Lusaka population data
Lus_Pop_Age <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds")
Overall_IFR <- sum(Braz_IFR$IFR_Age * Lus_Pop_Age/sum(Lus_Pop_Age))

# Generate slope multiplier and IFR multiplier matrix
IFR_vec <- seq(0.2,1.8, by = 0.2) # IFR
Slope_vec <- seq(0.2, 1.8,by = 0.2) # slope
IFR_mat <- expand.grid("IFR_x" = IFR_vec, "Slope_x" = Slope_vec) #
# Estimate absolute slope and overall IFR relative to multipliers.
IFR_mat <- IFR_mat %>% mutate(IFR_abs = IFR_x * sum(Braz_IFR$IFR_Age * Lus_Pop_Age/sum(Lus_Pop_Age)),
                              Slope_abs = Slope_x * Braz_IFR$IFR_Coefs[2])
# Apply the function to calculate absolute intercept values for each overall IFR and gradient combination
IFR_mat$Int_abs <- apply(IFR_mat, 1, function(x){
  Int_calc(x["IFR_abs"],x["Slope_abs"],Braz_IFR$IFR_Age_gr,Lus_Pop_Age)
})
# Using the absolute slope and intercept, estimate IFR by age group for each slope intercept combination
IFR_Age_var_slope_int <- t(apply(IFR_mat, 1, function(x){
  exp(Braz_IFR$IFR_Age_gr * x["Slope_abs"] + x["Int_abs"])
}))
# Use these IFRs to estimate probability of hospitalisation for use in the model
Prob_Death_mat <- apply(IFR_Age_var_slope_int, MARGIN = 1, FUN = function(x){x/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp)})

# saveRDS(list(IFR_mat = IFR_mat, Prob_Death_mat = Prob_Death_mat), "analysis/data/Code-generated-data/20_IFR_mat_Ints.rds")
IFR_mat <- readRDS("analysis/data/Code-generated-data/20_IFR_mat_Ints.rds")


# 1b. Use these probability of death in the model to estimate SARS-CoV-2 prevalence
off_data <- readRDS(file = "analysis/data/Code-generated-data/00_01c_Lusaka_Dist_Deaths_Official.rds") # See 00 data code
prob_death_tot_CFR_mat <- readRDS("analysis/data/Code-generated-data/20_IFR_mat_Ints.rds")$Prob_Death_mat # See 20 data code
Weighted_Durs_Hosp <- readRDS("analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds") # See 00 data code
Lancet_Data <- readRDS("analysis/data/Code-generated-data/00_07_Lancet_Data.rds") # See 00 data code

fit_l_IFR_slope_Official <- lapply(1:ncol(prob_death_tot_CFR_mat), function(x){

  print(x)
  if(any(prob_death_tot_CFR_mat[,x]>1)){return(NULL)}

  fit_Off_mort <- fit_spline_rt(data = off_data,
                                country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                                population = Lus_Pop_Age,
                                reporting_fraction = 1,
                                reporting_fraction_bounds = c(0.25,0.1,1),
                                n_mcmc = 20000,
                                replicates = 100,
                                rw_duration = 14,
                                hosp_beds = 1e10,
                                icu_beds = 1e10,
                                prob_severe = rep(0,17),
                                prob_non_severe_death_treatment = prob_death_tot_CFR_mat[,x],
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


})

# Save the results:
# saveRDS(object = fit_l_IFR_slope_Official, file = "../Bonus Files/2021-12-02-OfficialFit_Vary_IFR.rds")



# 1c. Check resulting fits
# Calculate pcrs and seros
sero_pcr_df_l_IFR_slope_Official <- lapply(X = fit_l_IFR_slope_Official, FUN = function(x){
  seroprev_df(x)})

# Simplify data
Simple_Plot_Data_l_IFR_slope <- lapply(sero_pcr_df_l_IFR_slope_Official, Summ_sero_pcr_data)

PlotsList_1 <- lapply(1:nrow(IFR_mat), function(x){
  if(is.null(fit_l_IFR_slope_Official[[x]])#IFR_vals_1[x]
     ){return(NA)} else {
    # y <- sum(!IFR_vals_1[1:x])
    p1 <- mortality_plot(off_data, fit_l_IFR_slope_Official[[x]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat$IFR_x[x],"\nSlope x ",IFR_mat$Slope_x[x]), vjust = 1.5) #+ annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nDeath data:",round(loglik_data(fit_l_IFR[[x]]),2)), hjust = 0, vjust = 4)
    return(p1)
  }
})

PlotsList_2 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(is.null(Simple_Plot_Data_l_IFR_slope[[x]])){
    # browser;
    return(NULL)} else {
      # y <- sum(!IFR_vals_1[1:x])
      p2 <- pcr_plot(Simple_Plot_Data_l_IFR_slope[[x]], fit_l_IFR_slope_Official[[x]], Lancet_Data) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat$IFR_x[x],"\nSlope x ",IFR_mat$Slope_x[x]), vjust = 1.5)# + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nPCR 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$pcr_15,2)), hjust = 0, vjust = 1.5)
      return(p2)
    }
})

PlotsList_3 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(is.null(Simple_Plot_Data_l_IFR_slope[[x]])){return(NA)} else {
    # y <- sum(!IFR_vals_1[1:x])
    p3 <- sero_plot(Simple_Plot_Data_l_IFR_slope[[x]], fit_l_IFR_slope_Official[[x]], Lancet_Data) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat$IFR_x[x],"\nSlope x ",IFR_mat$Slope_x[x]), vjust = 1.5)# + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nSero 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$sero_15,2)), hjust = 0, vjust = 1.5)
    return(p3)
  }
})

Dataset_Name <- "INITIAL"

pdf(file = paste0("analysis/figures/20_1_Official_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_1, nrow = 9,byrow = F)
dev.off()

pdf(file = paste0("analysis/figures/20_2_Official_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_2, nrow = 9,byrow = F)
dev.off()

pdf(file = paste0("analysis/figures/20_3_Official_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_3, nrow = 9,byrow = F)
dev.off()




####################################################################
# Part 2. Run model fitting over BMJ+Official Data #################
####################################################################

# 2a. Use PCR prevalence to estimate True covid deaths in BMJ data
# For each day, need to estimate covid prevalence.

# Background mortality
p_bg <- readRDS(file = "analysis/data/Code-generated-data/00_06_Crude_Mortality.rds") # per day
BMJ_mortuary <- readRDS(file = "analysis/data/Code-generated-data/00_05c_BMJ_Data_full.rds") # per day
pop_lu_dist <- sum(readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds"))

# Scale up
BMJ_TC_est <- lapply(seq(Simple_Plot_Data_l_IFR_slope), function(x){
  if(is.null(Simple_Plot_Data_l_IFR_slope[[x]])){return(NULL)}
  p_pos <- Simple_Plot_Data_l_IFR_slope[[x]] %>% filter(date>="2020-06-15" & date<="2020-10-02") %>%
    select(mean_pcr) %>% mutate(mean_pcr = mean_pcr/100)

  r_maf <- ((BMJ_mortuary$Est_Deaths_inc/pop_lu_dist)/p_pos - p_bg)/(1-p_bg)
  r_maf_Strict <- ((BMJ_mortuary$Est_Deaths_str/pop_lu_dist)/p_pos - p_bg)/(1-p_bg)

  # TC_est_inc <- pop_lu_dist*r_maf*p_pos
  # TC_est_str <- pop_lu_dist*r_maf_Strict*p_pos
# browser()
BMJ_mortuary <- BMJ_mortuary %>% mutate(TC_est_inc = unlist(pop_lu_dist*r_maf*p_pos),
                          TC_est_str = unlist(pop_lu_dist*r_maf_Strict*p_pos)) %>%
    mutate(TC_est_inc = replace(round(TC_est_inc), TC_est_inc<0, 0),
           TC_est_str = replace(round(TC_est_str), TC_est_str<0, 0)) %>%
    mutate(Est_Deaths_inc = round(Est_Deaths_inc),
           Est_Deaths_str = round(Est_Deaths_str))
  return(BMJ_mortuary)
})


# Strict deaths
# Inclusive deaths
# Strict deaths - True Covid
# Inclusive deaths - True Covid

# 2b. Combine the official mortality with each mortuary estimate set.
Off_Data_outside_BMJ <- readRDS(file = "analysis/data/Code-generated-data/00_01c_Lusaka_Dist_Deaths_Official.rds") %>%
  filter(date < "2020-05-15"  | date > "2020-10-02")
##### Still need to work out how to use the reporting fraction for the official data when it's combined with the mortuary data #####

BMJ_mortuary_off_comb <- lapply(seq(BMJ_TC_est), function(x){
if(is.null(BMJ_TC_est[[x]])){return(NULL)}
  BMJ_TC_est[[x]] %>%
    add_row(date = Off_Data_outside_BMJ$date,
            Est_Deaths_inc = Off_Data_outside_BMJ$deaths,
            Est_Deaths_str = Off_Data_outside_BMJ$deaths,
            TC_est_inc = Off_Data_outside_BMJ$deaths,
            TC_est_str = Off_Data_outside_BMJ$deaths) %>%
    arrange(date)})


## Clear large data objects
rm(fit_1, fit_l_IFR_slope_Official, sero_pcr_df_l_IFR_slope_Official, Simple_Plot_Data_l_IFR_slope,
            PlotsList_1, PlotsList_2, PlotsList_3)

# 2c. For each set, fit the model to the data again, using the same IFR as previously determined.

## Starting with inclusive, fit the model.






# For each IFR variant, perform the model fitting to the official data

off_data <- readRDS(file = "analysis/data/Code-generated-data/00_01c_Lusaka_Dist_Deaths_Official.rds") # See 00 data code
pop_st_lu <- readRDS("analysis/data/Code-generated-data/00_02b_Lusaka_Dist_Pop_Struc_2020_opendataforafrica.rds") # See 00 data code
prob_death_tot_CFR <- readRDS("analysis/data/Code-generated-data/00_03_Tot_Prob_Death_By_Age_Zam.rds") # See 00 data code
Weighted_Durs_Hosp <- readRDS("analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds") # See 00 data code
Lancet_Data <- readRDS("analysis/data/Code-generated-data/00_07_Lancet_Data.rds") # See 00 data code


fit_Off_mort <- fit_spline_rt(data = off_data,
                       country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                       population = pop_st_lu,
                       reporting_fraction = 1,
                       reporting_fraction_bounds = c(0.25,0.1,1),
                       n_mcmc = 20000,
                       replicates = 100,
                       rw_duration = 14,
                       hosp_beds = 1e10,
                       icu_beds = 1e10,
                       prob_severe = rep(0,17),
                       prob_non_severe_death_treatment = prob_death_tot_CFR,
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

# saveRDS(fit_Off_mort, file = "../Bonus Files/2021-11-24_fit_Off_mortality.rds")
fit_Off_mort <- readRDS(file = "../Bonus Files/2021-11-24_fit_Off_mortality.rds")

Sero_Pcr_df <- seroprev_df(fit_Off_mort)
PlotData <- Summ_sero_pcr_data(Sero_Pcr_df)

p1 <- mortality_plot(data = off_data, fit = fit_Off_mort)
p2 <- pcr_plot(PCR_Sero_PlotData = PlotData, fit = fit_Off_mort, Lancet_Data = Lancet_Data)
p3 <- sero_plot(PCR_Sero_PlotData = PlotData, fit = fit_Off_mort, Lancet_Data = Lancet_Data)

pdf(file = "analysis/figures/Analysis_Official_Reported_Deaths_Lusaka_dist_fit_est_rf_Mulenga_Data.pdf", height = 12, width = 4)
cowplot::plot_grid(p1,p2,p3,nrow = 3)
dev.off()





# 2. Estimate total mortuary deaths from sample compared with total deaths.
BMJ_data <- readRDS(file = "analysis/data/Code-generated-data/00_05c_BMJ_Data_full.rds") %>%
  select(date, Est_Deaths_inc) %>%
  rename(deaths = Est_Deaths_inc)




# 3. Combine official deaths with BMJ deaths.
Combined_Data <- off_data %>% filter(date < min(BMJ_data$date) | date > max(BMJ_data$date)) %>%
  rbind(., BMJ_data) %>% arrange(date)




# 4. Fit model to off/mortuary deaths, applying some sort of reporting fraction to the results.
fit_Off_mort <- fit_spline_rt(data = off_data,
                              country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                              population = pop_st_lu,
                              reporting_fraction = 1,
                              reporting_fraction_bounds = c(0.25,0.1,1),
                              n_mcmc = 20000,
                              replicates = 100,
                              rw_duration = 14,
                              hosp_beds = 1e10,
                              icu_beds = 1e10,
                              prob_severe = rep(0,17),
                              prob_non_severe_death_treatment = prob_death_tot_CFR,
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



# 5. Fit over a range of overall Infection Fatality ratios.
## Option 1: fit to a constant value
## Option 2: Allow to vary: it will likely assume that the reporting fraction is 100%


# 6. Fit over a range of slope and overall Infection Fatality ratios.

