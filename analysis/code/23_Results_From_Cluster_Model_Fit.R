rm(list = ls())
devtools::load_all()
library(squire)
library(ggplot2)
library(dplyr)
library(reshape2)

### Load data
Dataset_Name <- "Total_Covid_Hyper_Geo_New_Matrix_Fixed_Data"
fit_Model <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-01-26_Fixed_Inputs.rds")
# fit_Model_old <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-01-18_Cluster_fit_30000.rds")
# fit_Model_rerun <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-01-19_Cluster_fit_earlier_start_date_subset.rds")
# fit_Model_rerun <- readRDS("~/Documents/Imperial/PostDoc/Zambia/Bonus Files/2022-01-18_Cluster_fit_rerun.rds")

# fit_Model[names(fit_Model_rerun)] <- lapply(names(fit_Model_rerun), function(x){
#   # browser()
#   fit_Model[x] <- fit_Model_rerun[x]
#   fit_Model[[x]]
# })

# rm(fit_Model_rerun)

tail(fit_Model$X33$pmcmc_results$chains$chain1$results$log_likelihood)
tail(fit_Model$X34$pmcmc_results$chains$chain1$results$log_likelihood)
tail(fit_Model$X35$pmcmc_results$chains$chain1$results$log_likelihood)
tail(fit_Model$X36$pmcmc_results$chains$chain1$results$log_likelihood)
tail(fit_Model$X37$pmcmc_results$chains$chain1$results$log_likelihood)
tail(fit_Model$X38$pmcmc_results$chains$chain1$results$log_likelihood)
tail(fit_Model$X39$pmcmc_results$chains$chain1$results$log_likelihood)
tail(fit_Model$X40$pmcmc_results$chains$chain1$results$log_likelihood)
tail(fit_Model$X41$pmcmc_results$chains$chain1$results$log_likelihood)

fit_Model_old$X1$pmcmc_results$inputs$pars$pars_init[[1]]$start_date
fit_Model_old$X1$pmcmc_results$inputs$pars$pars_min$start_date
fit_Model_old$X1$pmcmc_results$inputs$pars$pars_max$start_date

fit_Model$X1$pmcmc_results$inputs$pars$pars_init[[1]]$start_date
fit_Model$X1$pmcmc_results$inputs$model_params$prob_non_severe_death_treatment
fit_Model$X1$pmcmc_results$inputs$pars$pars_max$start_date

fit_Model_old$X1$pmcmc_results$inputs$model_params$contact_matrix_set
fit_Model_old$X1$pmcmc_results$inputs$model_params$mix_mat_set

plot(fit_Model_old[[36]], particle_fit = T)

colMeans(cbind(fit_Model$X37$replicate_parameters$R0,fit_Model_old$X36$replicate_parameters$R0))

hist(fit_Model$X37$replicate_parameters$Rt_rw_2)
hist(fit_Model_old$X36$replicate_parameters$Rt_rw_2)

plot(fit_Model$X37$pmcmc_results$chains$chain1$results$R0)
plot(fit_Model_old$X36$pmcmc_results$chains$chain1$results$R0)

mean(fit_Model$X37$replicate_parameters$R0)
mean(fit_Model_old$X36$replicate_parameters$R0)

mean(fit_Model$X37$replicate_parameters$Rt_rw_1)
mean(fit_Model_old$X36$replicate_parameters$Rt_rw_1)

plot(table(fit_Model$X37$replicate_parameters$start_date))
plot(table(fit_Model_old$X36$replicate_parameters$start_date))

heatmap(x = as.matrix(fit_Model$X37$pmcmc_results$inputs$model_params$contact_matrix_set[[1]])*pop_st_lu, Rowv = NA, Colv = NA)
heatmap(x = as.matrix(fit_Model_old$X36$pmcmc_results$inputs$model_params$contact_matrix_set[[1]])*pop_st_lu, Rowv = NA, Colv = NA)
fit_Model_old$X36$pmcmc_results$inputs$model_params$contact_matrix_set



plot(fit_Model$X37$pmcmc_results$chains$chain1$results$Rt_rw_1)
plot(fit_Model_old$X36$pmcmc_results$chains$chain1$results$Rt_rw_1)

# fit_Model$X1$pmcmc_results$chains$chain1$acceptance_rate
# fit_Model$X2$pmcmc_results$chains$chain1$acceptance_rate
# fit_Model$X21$pmcmc_results$chains$chain1$acceptance_rate
# fit_Model$X65$pmcmc_results$chains$chain1$acceptance_rate
# fit_Model$X2$pmcmc_results$chains$chain1$ess
# fit_Model$X21$pmcmc_results$chains$chain1$ess
#
# colMeans(fit_Model$X1$replicate_parameters[,-1])
# colMeans(fit_Model$X2$replicate_parameters[,-1])
# colMeans(fit_Model$X21$replicate_parameters[,-1])
# colMeans(fit_Model$X65$replicate_parameters[,-1])
#
#
# fit_Model$X1$replicate_parameters$start_date
# fit_Model$X2$replicate_parameters$start_date
# fit_Model$X21$replicate_parameters$start_date
# fit_Model$X65$replicate_parameters$start_date
#
# mean(fit_Model$X1$replicate_parameters$R0)
# mean(fit_Model$X2$replicate_parameters$R0)
# mean(fit_Model$X21$replicate_parameters$R0)
# mean(fit_Model$X65$replicate_parameters$R0)

IFR_mat <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients.rds")
IFR_vec <- 1:nrow(IFR_mat) %in% readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector.rds")
IFR_mat_fil <- IFR_mat[IFR_vec,]
rownames(IFR_mat_fil) <- 1:nrow(IFR_mat_fil)

# IFR_mat <- readRDS("analysis/data/Code-generated-data/IFR_mat_Ints2.rds")
# Age_groups <- seq(2.5,82.5, by = 5)
# IFR_Age_var_slope_int <- t(apply(IFR_mat, 1, function(x){
#   exp(Age_groups * x["Slope_abs"] + x["Int_abs"])
# }))

# cbind(IFR_mat[!IFR_vals_1,],1:65)

# t(IFR_Age_var_slope_int)/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp)
# max(IFR_Age_var_slope_int[81,]/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp))
# IFR_vals_1 <- apply(IFR_Age_var_slope_int, MARGIN = 1, FUN = function(x){x/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp)
#   return(max(x/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp)))})>1
# IFR_Age_var_slope_int_fil <- IFR_Age_var_slope_int[!IFR_vals_1,]
# IFR_mat_fil <- IFR_mat[!IFR_vals_1,]



# Dataset_Name <- "Total_Covid_Strict"
# Dataset_Name <- "True_Covid"
# Dataset_Name <- "True_Covid_Strict"

# saveRDS(fit_l_IFR_slope, "../Bonus Files/fit_l_IFR_slope2.rds")
# saveRDS(fit_l_IFR_slope, paste0("../Bonus Files/fit_l_IFR_slope_",Dataset_Name,".rds"))
# fit_l_IFR_slope <- readRDS(paste0("../Bonus Files/fit_l_IFR_slope_",Dataset_Name,".rds"))
# fit_l_IFR_slope <- readRDS("../Bonus Files/fit_l_IFR_slope_True_Covid_Strict.rds")
# fit_l_IFR_slope <- readRDS("../Bonus Files/fit_l_IFR_slope_Total_Covid.rds")

# So now I want to plot against those dimensions to see how that worked
# estimate pcr/serology across data:
sero_pcr_df_l_IFR_slope <- lapply(X = fit_Model, FUN = function(x){
  seroprev_df(x)})

# Simplify data
Simple_Plot_Data_l_IFR_slope <- lapply(sero_pcr_df_l_IFR_slope, Summ_sero_pcr_data)

### Calculate Log-likelihood function
log_lik_sero_fun <- function(sero_fit){
  sero_pred_1<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-15"), "sero_perc"])
  ll_sero_15 <- log(dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_pred_1))

  # sero_pred_2<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-29"), "sero_perc"])
  # ll_sero_29 <- log(dbinom(x = as.integer(0.106*1952), size = 1952, prob = sero_pred_2))

  pcr_pred_1<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-15"), "pcr_perc"])
  ll_pcr_15 <- log(dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_pred_1))

  # Combined measure
  comb_pred_1<- colMeans(sero_fit[sero_fit$date==as.Date("2020-07-15"), "combined_perc"])
  ll_comb_15 <- log(dbinom(x = as.integer(0.091*2990), size = 0.631*1952, prob = comb_pred_1))


  return(list("sero_15"=ll_sero_15, "pcr_15"=ll_pcr_15, "comb_15"=ll_comb_15))#, "sero_29"=ll_sero_29))
}

loglik_data <- function(fit_res){
  # browser()
  Ds <- diff(rowMeans(apply(fit_res$output[as.Date(rownames(fit_res$output)) %in% fit_res$pmcmc_results$inputs$data$date, odin_index(fit_res$model)$D,], MARGIN = 3, FUN = rowSums)))
  ll_deaths <- sum(ll_nbinom(data = fit_res$pmcmc_results$inputs$data$deaths[-1],
                             model = Ds,
                             phi = fit_res$pmcmc_results$inputs$pars_obs$phi_death,
                             k = fit_res$pmcmc_results$inputs$pars_obs$k_death,
                             exp_noise = fit_res$pmcmc_results$inputs$pars_obs$exp_noise))
  return(ll_deaths)
}

# apply(fit_l_IFR[[1]]$output[as.Date(rownames(fit_l_IFR[[1]]$output)) %in% fit_l_IFR[[1]]$pmcmc_results$inputs$data$date, odin_index(fit_l_IFR[[1]]$model)$D,], MARGIN = 3, FUN = rowSums)

# Plot graphs
part_fit_plot <- function(fit){plot(fit, particle_fit = T) +
    annotate(geom = "point", x= as.Date("2020-04-18"), y = 50) +
    annotate(geom = "text", x=as.Date("2020-04-22"), y = 50, hjust=0, label = "BMJ data estimated deaths") +
    annotate(geom = "segment", x=as.Date("2020-04-15"), xend = as.Date("2020-04-20"), y = 50*0.92, yend = 50*0.92, col = "red", alpha = 0.7) +
    annotate(geom = "rect", xmin=as.Date("2020-04-15"), xmax = as.Date("2020-04-20"), ymin = 50*0.915, ymax = 50*0.925, fill = NA, color = "black", linetype = 2) +
    annotate(geom = "text", x=as.Date("2020-04-22"), y = 50*0.92, hjust = 0, label = "Model fit") +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 50)) +
    theme(legend.position = "none")
}
pcr_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-15"),y=7.6)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=7.6+0.5, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=2, label="Model fit", alpha=0.8) +
    geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0)) +
    ylab(paste0("PCR % with ",100,"% death reporting")) +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 15)) +
    theme_bw()}
sero_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-15"),y=2.1)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=2.1+0.5, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=max(Summ_sero_fit$max_sero), label="Model fit", alpha=0.8, hjust=0, vjust = 2) +
    geom_errorbar(aes(ymin=1.1,ymax=3.1,x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0)) +
    geom_point(aes(x= as.Date("2020-07-29"),y=10.6), color="darkblue") +
    annotate("text", x=as.Date("2020-07-29")+15, y=10.6, label="Mulenga et al. \n (extrapolated \n from PCR)", color = "darkblue", hjust =0) +
    # geom_text(aes(label = "Mulenga et al. \n (extrapolated \n from PCR)", x= as.Date("2020-07-29")+15,y=0.106), color = "darkblue", hjust= 0) +
    geom_errorbar(aes(ymin=7.3,ymax=13.9,x=as.Date("2020-07-29"), width=10), color="darkblue") +
    geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=10.6, height=0), color="darkblue") +
    ylab(paste0("Sero+% with ",100,"% death reporting")) +
    annotate("text", x=as.Date("2020-05-01"), y=1.8, label=paste(""), color = "darkblue", hjust =0) +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 20)) +
    theme_bw()}

combined_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_combined)) + geom_line(aes(x=date, y=mean_combined),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_combined, ymax=max_combined), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-15"),y=9.1)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=9.1+0.5, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=max(Summ_sero_fit$max_combined), label="Model fit", alpha=0.8, hjust=0, vjust = 2) +
    geom_errorbar(aes(ymin=2.6,ymax=15.7,x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0)) +
    # geom_point(aes(x= as.Date("2020-07-29"),y=10.6), color="darkblue") +
    # annotate("text", x=as.Date("2020-07-29")+15, y=10.6, label="Mulenga et al. \n (extrapolated \n from PCR)", color = "darkblue", hjust =0) +
    # geom_text(aes(label = "Mulenga et al. \n (extrapolated \n from PCR)", x= as.Date("2020-07-29")+15,y=0.106), color = "darkblue", hjust= 0) +
    # geom_errorbar(aes(ymin=7.3,ymax=13.9,x=as.Date("2020-07-29"), width=10), color="darkblue") +
    # geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=10.6, height=0), color="darkblue") +
    ylab(paste0("Sero+% with ",100,"% death reporting")) +
    # annotate("text", x=as.Date("2020-05-01"), y=1.8, label=paste(""), color = "darkblue", hjust =0) +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 20)) +
    theme_bw()}


PlotsList_1 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){return(NA)} else {
    # browser()
    y <- sum(IFR_vec[1:x])
    p1 <- part_fit_plot(fit_Model[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5) #+ annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nDeath data:",round(loglik_data(fit_l_IFR[[x]]),2)), hjust = 0, vjust = 4)
    return(p1)
  }
})

PlotsList_2 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){
    # browser;
    return(NULL)} else {
      # browser()
      y <- sum(IFR_vec[1:x])
      p2 <- pcr_fit_plot(Simple_Plot_Data_l_IFR_slope[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5)# + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nPCR 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$pcr_15,2)), hjust = 0, vjust = 1.5)
      return(p2)
    }
})

PlotsList_3 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){return(NA)} else {
    y <- sum(IFR_vec[1:x])
    p3 <- sero_fit_plot(Simple_Plot_Data_l_IFR_slope[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5)# + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nSero 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$sero_15,2)), hjust = 0, vjust = 1.5)
    return(p3)
  }
})

PlotsList_4 <- lapply(seq(nrow(IFR_mat)), function(x){
  if(!IFR_vec[x]){return(NA)} else {
    y <- sum(IFR_vec[1:x])
    p3 <- combined_fit_plot(Simple_Plot_Data_l_IFR_slope[[y]]) + annotate("text",x = as.Date("2020-09-15"),y = Inf, label=paste0("IFR x ",IFR_mat_fil$IFR_x[y],"\nSlope x ",IFR_mat_fil$Slope_x[y]), vjust = 1.5)# + annotate("text",x = as.Date("2020-04-15"),y = Inf, label=paste("LL\nSero 15th:",round(log_lik_sero_fun(sero_pcr_df_l_IFR[[x]])$sero_15,2)), hjust = 0, vjust = 1.5)
    return(p3)
  }
})


pdf(file = paste0("analysis/figures/23_1_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_1, nrow = 9,byrow = F)
dev.off()

pdf(file = paste0("analysis/figures/23_2_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_2, nrow = 9,byrow = F)
dev.off()

pdf(file = paste0("analysis/figures/23_3_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_3, nrow = 9,byrow = F)
dev.off()

pdf(file = paste0("analysis/figures/23_6_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 30, width = 30)
cowplot::plot_grid(plotlist = PlotsList_4, nrow = 9,byrow = F)
dev.off()




sero_pcr_df_l_IFR_slope <- lapply(seq(length(sero_pcr_df_l_IFR_slope)), function(x){sero_pcr_df_l_IFR_slope[[x]] %>% mutate(IFR_val = IFR_mat_fil$IFR_x[[x]],
                                                                                                                            Slope_val = IFR_mat_fil$Slope_x[[x]])})
sero_pcr_df_l_IFR_slope_com <- data.table::rbindlist(sero_pcr_df_l_IFR_slope)

## Calculate the multinomial likelihood for the age distribution
# Get the proportion of deaths from BMJ
BMJ_Deaths_by_Age_Covid_Neg <- c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2)
BMJ_Deaths_by_Age_Covid_Pos <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Covid_Pos2 <- c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Tot <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2)


Likelihoods_IFR_Age_structure<- sapply(1:length(fit_Model), function(y){
  Tot_Deaths_by_Age_gr <- sapply(1:100, function(x){

    Deaths_reps_by_age <- tail(fit_Model[[y]]$output[,paste0("D[",1:17,"]"),x],1) - fit_Model[[y]]$output["2020-06-14",paste0("D[",1:17,"]"),x]
    # print(Deaths_reps_by_age)
    if(is.na(Deaths_reps_by_age)){return(0)}
    # Deaths_reps_by_age/pop_st_lu
    if(Dataset_Name %in% c("Total_Covid_Strict","True_Covid_Strict")){
      dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos2[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos2[17:21])), # Strict ct cut off
                size = sum(BMJ_Deaths_by_Age_Covid_Pos2),
                prob = Deaths_reps_by_age)
    } else {
      dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])), # Not strict ct cut off
                size = sum(BMJ_Deaths_by_Age_Covid_Pos),
                prob = Deaths_reps_by_age)
    }

  })
  # rowMeans(Tot_Deaths_by_Age_gr)
})


IFR_Age_LL <- IFR_mat_fil %>% cbind(t(log(Likelihoods_IFR_Age_structure))) %>%
  melt(data = ., id = c("IFR_x","Slope_x","IFR_abs","Slope_abs","Int_abs"), variable = "replicate",value.name = "Age_ll") %>%
  rename(IFR_val = "IFR_x", Slope_val = "Slope_x") %>%
  select(IFR_val, Slope_val, replicate, Age_ll)


dd_l_IFR_slope <- sero_pcr_df_l_IFR_slope_com %>% filter(date %in% as.Date(c("2020-07-15","2020-07-29"))) %>%
  select(replicate, date, sero_perc, pcr_perc, combined_perc, IFR_val, Slope_val) %>%
  group_by(replicate, IFR_val, Slope_val) %>%
  # mutate(sero_perc_2 = sero_perc[date==as.Date("2020-07-29")]) %>% ungroup() %>%
  filter(date==as.Date("2020-07-15")) %>% select(-date) %>%
  mutate(ll_sero_1 = log(dbinom(x = as.integer(0.021*2704), size = 2704, prob = sero_perc)),
         # ll_sero_2 = log(dbinom(x = as.integer(0.106*1952), size = 1952, prob = sero_perc_2)),
         ll_pcr = log(dbinom(x = as.integer(0.076*2990), size = 2990, prob = pcr_perc)),
         ll_combined = dbinom(x = as.integer(0.091*0.631*1952), size = as.integer(0.631*1952), prob = combined_perc, log = T),
         ) %>%
  # ll_deaths = Death_LLs) %>%
  merge(IFR_Age_LL,by = c("replicate","IFR_val","Slope_val")) %>%
  mutate(ll_total = ll_sero_1 + ll_pcr) %>% #+ Age_ll) %>%
  select(-sero_perc,-pcr_perc) %>%
  melt(data = ., id=c("IFR_val","Slope_val", "replicate"),value.name = "ll")

# pdf(file = "analysis/figures/p_3_BMJ_Lancet_var_IFR_slope_4.pdf", height = 20, width = 15)
pdf(file = paste0("analysis/figures/12_4_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 20, width = 15)
ggplot(filter(dd_l_IFR_slope, IFR_val >0), aes(x=ll, y=as.factor(IFR_val), fill=as.factor(Slope_val))) +
  geom_boxplot() +
  facet_wrap(. ~ variable, scales = "free_x", nrow=1, labeller = as_labeller(c(`ll_sero_1` = "Sero+ % (15-07-2020)", `ll_pcr` = "PCR+ % (15-07-2020)", `ll_total` = "Total"))) +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank(), panel.grid.minor = element_blank()) +
  xlab("log likelihood") + ylab("x IFR")
dev.off()




# For each run, get the deaths by age for the time period
# tail(fit_l_IFR_slope[[1]]$output)
# str(fit_l_IFR_slope[[1]]$output)
# colnames(fit_l_IFR_slope[[1]]$output)
# plot(t(tail(fit_l_IFR_slope[[3]]$output[,paste0("D[",1:17,"]"),1],1)), pch = 20)
# cbind(IFR_mat,log(colMeans(Likelihoods_IFR_Age_structure)))
# Likelihoods_IFR_Age_structure[,1]/pop_st_lu/sum(pop_st_lu)
# which.max(colMeans(log(Likelihoods_IFR_Age_structure)))
# length(colMeans(log(Likelihoods_IFR_Age_structure)))

# IFR_mat[1,]
# IFR_mat[10,]
# IFR_mat[19,]

# Deaths_reps_by_age <- sapply(1:100, function(x){tail(fit_l_IFR_slope[[9]]$output[,paste0("D[",1:17,"]"),x],1) - fit_l_IFR_slope[[9]]$output["2020-06-14",paste0("D[",1:17,"]"),x]})
# Deaths_reps_by_age <- sapply(1:100, function(x){tail(fit_l_IFR_slope[[1]]$output[,paste0("D[",1:17,"]"),x],1) - fit_l_IFR_slope[[1]]$output["2020-06-14",paste0("D[",1:17,"]"),x]})

# rmultinom(n = 1, size = 70, prob = Deaths_reps_by_age[,2]/sum(Deaths_reps_by_age[,2]))

# rmultinom(n = 1, size = 888, prob = Deaths_reps_by_age[,2]/sum(Deaths_reps_by_age[,2]))
# rmultinom(n = 1, size = 888, prob = Deaths_reps_by_age[,2]/pop_st_lu)

# log(dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])),
#           size = 70,
#           prob = Deaths_reps_by_age[,2]/pop_st_lu))


# apply(IFR_mat, MARGIN = 1, function(x){
#   # browser()
#   exp(x["Int_abs"] + x["Slope_abs"]*Age_groups) * pop_st_lu/sum(pop_st_lu)
# })[,1]

# Deaths_reps_by_age <- sapply(1:100, function(x){tail(fit_l_IFR_slope[[1]]$output[,paste0("D[",1:17,"]"),x],1) - fit_l_IFR_slope[[1]]$output["2020-06-14",paste0("D[",1:17,"]"),x]})
# rowMeans(Deaths_reps_by_age)/sum(rowMeans(Deaths_reps_by_age))

# plot(rowMeans(Deaths_reps_by_age)/sum(rowMeans(Deaths_reps_by_age)), apply(IFR_mat, MARGIN = 1, function(x){
#   # browser()
#   exp(x["Int_abs"] + x["Slope_abs"]*Age_groups) * pop_st_lu/sum(pop_st_lu)
# })[,1]
# )


# Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr)

# c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])))

# dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])),
#           size = sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))),
#           prob = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr))


# fit_l_IFR_slope[[1]]$output
# Off_data <- readRDS("../Bonus Files/p_01_Official_Data_Lancet_est_rf_resonable.rds")

# Tot_Deaths_by_Age_gr <- sapply(1:17, function(x){
#   mean(Off_data$output["2020-10-01",paste0("D[",x,"]"),]) - mean(Off_data$output["2020-06-14",paste0("D[",x,"]"),])
# })

# Tot_Deaths_by_Age_gr <- sapply(1:17, function(x){
#   mean(fit_l_IFR_slope[[1]]$output["2020-10-01",paste0("D[",x,"]"),]) - mean(fit_l_IFR_slope[[1]]["2020-06-14",paste0("D[",x,"]"),])
# })


# saveRDS(object = dd_l_IFR_slope, file = paste0("analysis/data/Code-generated-data/12_HeatmapData",Dataset_Name,".rds"))
# Dataset_Name <- "Total_Covid_Strict"
# Dataset_Name <- "Total_Covid"
# Dataset_Name <- "True_Covid_Strict"
# dd_l_IFR_slope <- readRDS(file = paste0("analysis/data/Code-generated-data/12_HeatmapData",Dataset_Name,".rds"))


HeatMapData1 <- dd_l_IFR_slope %>% filter(variable == "ll_sero_1") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup() %>%
  # mutate(mean_gr = cut(mean_ll, breaks= c(-Inf,-100,-50,-20,-10,-5,0), labels = c("1","2","3","4","5","6")))
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))


HeatMapData2 <- dd_l_IFR_slope %>% filter(variable == "ll_pcr") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup %>%
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))

HeatMapData3 <- dd_l_IFR_slope %>% filter(variable == "Age_ll") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup %>%
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))

HeatMapData4 <- dd_l_IFR_slope %>% filter(variable == "ll_total") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup() %>%
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))

HeatMapData5 <- dd_l_IFR_slope %>% filter(variable == "ll_combined") %>% select(IFR_val, Slope_val, ll) %>%
  group_by(IFR_val,Slope_val) %>% summarise(mean_ll = mean(ll)) %>% ungroup() %>%
  mutate(mean_gr = as.numeric(cut(round(mean_ll), breaks= c(-Inf,round(max(mean_ll,na.rm = T))-500,
                                                            round(max(mean_ll,na.rm = T))-200,
                                                            round(max(mean_ll,na.rm = T))-100,
                                                            round(max(mean_ll,na.rm = T))-50,
                                                            round(max(mean_ll,na.rm = T))-20,
                                                            round(max(mean_ll,na.rm = T))-12,
                                                            round(max(mean_ll,na.rm = T))-8,
                                                            round(max(mean_ll,na.rm = T))-4,
                                                            round(max(mean_ll,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))


# pdf(file = "analysis/figures/p_3_BMJ_Lancet_var_IFR_slope_5.pdf", height = 6, width = 20)
# pdf(file = "analysis/figures/p_3_BMJ_Lancet_var_IFR_slope_5_All_Age_Deaths.pdf", height = 6, width = 20)
pdf(file = paste0("analysis/figures/23_5_BMJ_Lancet_var_IFR_slope_",Dataset_Name,".pdf"), height = 6, width = 20)
cowplot::plot_grid(
  ggplot(HeatMapData2, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
    geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
    scale_colour_manual(values = c("white", "black")) +
    ggtitle("Log likelihood: PCR prevalence") + xlab("xIFR") + ylab("xSlope") +
    labs(fill = "Mean ll") + theme(legend.position = "none") +
    scale_fill_viridis_c(),
  ggplot(HeatMapData1, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
    geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
    scale_colour_manual(values = c("white", "black")) +
    ggtitle("Log likelihood: seroprevalence") + xlab("xIFR") + ylab("xSlope") +
    labs(fill = "Mean ll") + theme(legend.position = "none") +
    scale_fill_viridis_c(),
  # ggplot(HeatMapData3, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
  #   geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
  #   scale_colour_manual(values = c("white", "black")) +
  #   ggtitle("Log likelihood: deaths by age") + xlab("xIFR") + ylab("xSlope") +
  #   labs(fill = "Mean ll") + theme(legend.position = "none") +
  #   scale_fill_viridis_c(),
  ggplot(HeatMapData4, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
    # geom_text(aes(label = round(Tot_LL), colour = Tot_LL>max(Tot_LL)-2), size = 4) +
    geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
    scale_colour_manual(values = c("white", "black")) +
    ggtitle("Log likelihood: total") + xlab("xIFR") + ylab("xSlope") +
    labs(fill = "Mean ll") + theme(legend.position = "none") +
    scale_fill_viridis_c(),
  ggplot(HeatMapData5, aes(x = IFR_val, y = Slope_val, fill = mean_gr)) + geom_tile() +
    # geom_text(aes(label = round(Tot_LL), colour = Tot_LL>max(Tot_LL)-2), size = 4) +
    geom_text(aes(label = round(mean_ll), colour = (mean_gr >= max(mean_gr, na.rm=T))), size = 4) +
    scale_colour_manual(values = c("white", "black")) +
    ggtitle("Log likelihood: combined") + xlab("xIFR") + ylab("xSlope") +
    labs(fill = "Mean ll") + theme(legend.position = "none") +
    scale_fill_viridis_c(),
  nrow = 1)
dev.off()





fit_BMJ_mort <- fit_Model[[33]]
# Combined_Weeks <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$combined_data
# pcr_det <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$pcr_det
## Check that the likelihood calculations are the same as expected.

get_deaths_age_week <- function(fit_BMJ_mort){
  Combined_Weeks <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$combined_data
  # browser()
  Deaths <- squire::format_output(fit_BMJ_mort, c("D"), date_0 = max(fit_BMJ_mort$pmcmc_results$inputs$data$date), reduce_age = F) %>%
    group_by(replicate,age_group) %>%
    filter(date %in% c(min(date, na.rm = T),unique(Combined_Weeks$date)-1, max(date, na.rm = T))) %>%
    mutate(D = y,
           D_end = c(0,head(y,-1))) %>%
    mutate(D_weekly = D-D_end) %>%
    filter(!row_number() == 1) %>%
    group_by(age_group, replicate) %>%
    mutate(WeekNo = 1:16)

  return(Deaths)

}


# Deaths_by_age <- Deaths %>% group_by(replicate, age_group) %>%
#   summarise(Deaths = sum(D_weekly)) %>% ungroup() %>%
#   group_by(age_group) %>%
#   summarise(mean_Deaths = mean(Deaths),
#             max_Deaths = max(Deaths),
#             min_Deaths = min(Deaths))

# Deaths_by_week <- Deaths %>% group_by(replicate, date) %>%
#   summarise(Deaths = sum(D_weekly)) %>% ungroup() %>%
#   group_by(date) %>%
#   summarise(mean_Deaths = mean(Deaths),
#             max_Deaths = max(Deaths),
#             min_Deaths = min(Deaths))


get_expected_pcr_only <- function(fit_BMJ_mort){
  pcr_det <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$pcr_det
  Combined_Weeks <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$combined_data
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

  # pcr_dates <- inf %>% group_by(date,age_group) %>%
  #   summarise(mean_pcr = mean(pcr_perc),
  #             max_pcr = max(pcr_perc),
  #             min_pcr = min(pcr_perc))

  return(inf)
}

# get_expected_pcr_only(fit_Model[[1]])

recalculate_likelihood <- function(fit_BMJ_mort){

    Deaths <- get_deaths_age_week(fit_BMJ_mort)
    pcr_vals <- get_expected_pcr_only(fit_BMJ_mort)
    Combined_Weeks <- fit_BMJ_mort$pmcmc_results$inputs$pars_obs$combined_data

    Deaths_inf <- merge(x = pcr_vals, y = Deaths, by = c("age_group","replicate","WeekNo")) %>%
      mutate(D_weekly = replace_na(D_weekly, 0)) %>%
      select(age_group, replicate, WeekNo, date.y, pcr_perc, D_weekly)
# browser()
    Non_cov_deaths_est <- Combined_Weeks %>% rename(age_group = "Age_group", WeekNo = "Week_gr") %>%
      merge(x = ., y = Deaths_inf, by = c("age_group","WeekNo")) %>%
      select(-date.y) %>%
      mutate(Mod_cd = D_weekly) %>%
      mutate(Mod_cd_mort = Mod_cd*0.8) %>%
      mutate(tot_mort_deaths = total_deaths) %>%
      mutate(Mod_ncd = (total_deaths - Mod_cd_mort)) %>% # non-covid deaths
      mutate(Mod_pos_ncd = Mod_ncd*pcr_perc) %>%
      mutate(Mod_tot_pos_mort = Mod_cd_mort+Mod_pos_ncd)

    Model_Data_Comp <- Non_cov_deaths_est %>% group_by(age_group, replicate) %>%
      summarise(Mod_cd = sum(Mod_cd),
                Mod_cd_mort = sum(Mod_cd_mort),
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
                Mod_cd_mort = mean(Mod_cd_mort),
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

    return(Model_Data_Comp)


    # Model_Data_Comp_Week <- Non_cov_deaths_est %>% group_by(age_group, WeekNo, replicate) %>%
    #   summarise(Mod_cd = sum(Mod_cd),
    #             Mod_cd_mort = sum(Mod_cd_mort),
    #             tot_mort_deaths = sum(tot_mort_deaths),
    #             Mod_ncd = sum(Mod_ncd),
    #             Mod_pos_ncd = sum(Mod_pos_ncd),
    #             Mod_tot_pos_mort = sum(Mod_tot_pos_mort),
    #             sampled_deaths = sum(sampled_deaths),
    #             CT_45_Either = sum(CT_45_Either),
    #             # Mod_cov_deaths = sum(Mod_cov_deaths),
    #   ) %>%
    #   ungroup() %>% group_by(age_group, WeekNo) %>%
    #   summarise(Mod_cd = mean(Mod_cd),
    #             Mod_cd_mort = mean(Mod_cd_mort),
    #             tot_mort_deaths = mean(tot_mort_deaths),
    #             Mod_ncd = mean(Mod_ncd),
    #             Mod_pos_ncd = mean(Mod_pos_ncd),
    #             Mod_tot_pos_mort = mean(Mod_tot_pos_mort),
    #             sampled_deaths = mean(sampled_deaths),
    #             CT_45_Either = mean(CT_45_Either)
    #   ) %>%
    #   mutate(Mod_prop_pos = Mod_tot_pos_mort/tot_mort_deaths,
    #          Mort_prop_pos = CT_45_Either/sampled_deaths,
    #          Mod_Pos_Test = Mod_prop_pos*sampled_deaths) %>%
    #   mutate(Too_Many_Deaths = ifelse(round(Mod_ncd + Mod_cd_mort) > tot_mort_deaths, TRUE, FALSE))
    #
    # return(Model_Data_Comp_Week)
}


fit_Model[[5]]$pmcmc_results$inputs$pars_obs$combined_data$total_deaths
Recreate_results <- lapply(fit_Model, recalculate_likelihood)

Res_fit_33 <- recalculate_likelihood(fit_Model[[33]])
Res_fit_32 <- recalculate_likelihood(fit_Model[[32]])
Res_fit_1 <- recalculate_likelihood(fit_Model[[1]])
Res_fit_5 <- recalculate_likelihood(fit_Model[[5]])



Recreate_results

fit_Model$X2$pmcmc_results$inputs$model_params$prob_non_severe_death_treatment

rownames(IFR_mat_fil) <- 1:65
IFR_mat_fil

fit_Model$X1$pmcmc_results$chains$chain1$results
plot(fit_Model$X4$replicate_parameterspmcmc_results$chains$chain1$results$log_posterior)

lapply(Recreate_results, function(x){any(x$Too_Many_Deaths)})

# "tot_mort_deaths"

cbind(fit_Model[[1]]$pmcmc_results$inputs$pars_obs$combined_data$total_deaths,
      rowSums(Recreate_results[[64]][,c("Mod_cd_mort", "Mod_ncd")]))


fit_BMJ_mort <- fit_Model[[1]]
Res_first_one <- recalculate_likelihood(fit_Model[[1]])
Res_first_one

Res_first_one$Too_Many_Deaths

head(Res_first_one)


Deaths_inf <- merge(x = inf, y = Deaths, by = c("age_group","replicate","WeekNo")) %>%
  select(age_group, replicate, WeekNo, date.y, pcr_perc, D_weekly)

Non_cov_deaths_est <- Combined_Weeks %>% rename(age_group = "Age_group", WeekNo = "Week_gr") %>%
  merge(x = ., y = Deaths_inf, by = c("age_group","WeekNo")) %>%
  select(-date.y) %>%
  mutate(Mod_cd = D_weekly) %>%
  mutate(Mod_cd_mort = Mod_cd*0.8) %>%
  mutate(tot_mort_deaths = total_deaths) %>%
  mutate(Mod_ncd = (total_deaths - Mod_cd_mort)) %>% # non-covid deaths
  mutate(Mod_pos_ncd = Mod_ncd*pcr_perc) %>%
  mutate(Mod_tot_pos_mort = Mod_cd_mort+Mod_pos_ncd)

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


































































## plot the standard IFR against the best IFR we estimated
# lm(log(IFR_l$IFR_Age) ~ Age_groups)

MaxLL <- HeatMapData4[which.max(HeatMapData4$Tot_LL),]

plot(y = Age_groups*IFR_Coefs[2] + IFR_Coefs[1], x = Age_groups, type = "l",
     xlab = "Age", ylab = "log IFR")
lines(x = Age_groups,
      y = Age_groups*IFR_mat[IFR_mat$IFR_x == MaxLL$IFR_val & IFR_mat$Slope_x == MaxLL$Slope_val,"Slope_abs"] + IFR_mat[IFR_mat$IFR_x == MaxLL$IFR_val & IFR_mat$Slope_x == MaxLL$Slope_val,"Int_abs"],
      col = "darkblue", lty = 2)

plot(y = exp(Age_groups*IFR_Coefs[2] + IFR_Coefs[1]), x = Age_groups, type = "l",
     xlab = "Age", ylab = "log IFR")
lines(x = Age_groups,
      y = exp(Age_groups*IFR_mat[IFR_mat$IFR_x == MaxLL$IFR_val & IFR_mat$Slope_x == MaxLL$Slope_val,"Slope_abs"] + IFR_mat[IFR_mat$IFR_x == MaxLL$IFR_val & IFR_mat$Slope_x == MaxLL$Slope_val,"Int_abs"]),
      col = "darkblue", lty = 2)

#######################
## Now I want to plot a similar plot to the example plot I had:
#######################

IFR_Age <- readRDS(file = "analysis/data/Code-generated-data/00_03b_IFR_values.rds")
Age_vals <- seq(2.5, 82.5, by = 5)

#Plot 1: IFR curves
# What is the intercept when the IFR is 1.1?
library(dplyr)
library(tidyverse)
library(reshape2)

plot1_df <- data.frame(Age = seq(0,100,by = 0.1)) %>%
  mutate(IFR_1_1 = exp(Age * IFR_Coefs[2] + IFR_Coefs[1]),
         IFR_Best_1 = exp(Age * IFR_Coefs[2] + IFR_mat[round(IFR_mat$IFR_x,1)==1.4 & IFR_mat$Slope_x == 0.8,"Int_abs"]),
         IFR_Best_2 = exp(Age * IFR_Coefs[2] + IFR_mat[round(IFR_mat$IFR_x,1)==1.2 & IFR_mat$Slope_x == 0.8,"Int_abs"]),
  )

plot1_df_melt <- melt(plot1_df, id.vars = "Age") %>%
  mutate(IFR = recode(variable, "IFR_1_1" = "Standard IFR", "IFR_Best_1" = "1.4xIFR, 0.8xSlope", "IFR_Best_2" = "1.2xIFR, 0.8xSlope"))

p1 <- ggplot(data = plot1_df_melt, aes(x = Age, y = value, group = variable, col = IFR, linetype = IFR)) + geom_line() +
  scale_colour_manual(values = c("black", "darkred","darkblue")) +
  scale_linetype_manual(values = c(1,2,2)) +
  ylab("IFR") + xlim(0,85) + ylim(0,25) + labs(color='', linetype = '') #+
# geom_ribbon(data = subset(plot1_df_melt, variable == "IFR_0.6_1"),
#             aes(ymin = value, ymax = subset(plot1_df_melt, variable == "IFR_1.4_1")[,"value"]),
#             fill = "darkorange", alpha = 0.1) +
# geom_ribbon(data = subset(plot1_df_melt, variable == "IFR_1_0.9"),
#             aes(ymin = value, ymax = subset(plot1_df_melt, variable == "IFR_1_1.1")[,"value"]),
#             fill = "darkred", alpha = 0.1)

# Plot 2: Death curves
# fit_l_IFR_slope <- readRDS("../Bonus Files/fit_l_IFR_slope.RDS")

# Select the correct elements of the list
# IFR_vec <- c(0.4,0.6,0.8,0.9,1,1.1,1.2,1.4,1.6)
# Slope_vec <- seq(0.6, 1.4,by = 0.1)# * IFR_Coefs[2]
# IFR_mat <- expand.grid("IFR_x" = IFR_vec, "Slope_x" = Slope_vec)
IFR_mat_fil[IFR_mat_fil$IFR_x == 1 & IFR_mat_fil$Slope_x == 1 | round(IFR_mat_fil$IFR_x,1) == 1.4 & IFR_mat_fil$Slope_x == 0.8 | round(IFR_mat_fil$IFR_x,1) == 1.2 & IFR_mat_fil$Slope_x == 0.8, ]

names(fit_l_IFR_slope) <- apply(X = IFR_mat_fil, MARGIN = 1, function(x){paste0("IFR_",x[1],"_Slope",x[2])})

fit_subs <- fit_l_IFR_slope[IFR_mat_fil$IFR_x == 1 & IFR_mat_fil$Slope_x == 1 | round(IFR_mat_fil$IFR_x,1) == 1.4 & IFR_mat_fil$Slope_x == 0.8 | round(IFR_mat_fil$IFR_x,1) == 1.2 & IFR_mat_fil$Slope_x == 0.8]
fit_subs <- fit_subs[c(3,2,1)]

## For each element in the list plot the mean mortality fitted data
# rowMeans(apply(fit_subs[[1]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-1]-rowMeans(apply(fit_subs[[1]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-nrow(fit_subs[[1]]$output)]

colnames <- names(fit_subs)
Deaths_list <- lapply(1:length(fit_subs), function(x){
  Deaths_tmp <- rownames_to_column(data.frame(Deaths = rowMeans(apply(fit_subs[[x]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-1]-rowMeans(apply(fit_subs[[x]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-nrow(fit_subs[[x]]$output)])) %>%
    rename(Date = "rowname")
  names(Deaths_tmp)[2] <- colnames[x]
  Deaths_tmp
})

Deaths_df <- Deaths_list %>% reduce(full_join, by = "Date") %>%
  melt(id = "Date", variable = "IFR", value.name = "Deaths") %>%
  mutate(IFR = recode(IFR, "IFR_1_1" = "Standard IFR", "IFR_Best" = "Fitted IFR"))


fit_subs[[1]]$pmcmc_results$inputs$data

p2 <- ggplot(data = Deaths_df) +
  geom_line(aes(x = as.Date(Date), y = Deaths, group = IFR, col = IFR, linetype = IFR)) +
  scale_colour_manual(values = c("black","darkred","darkblue")) +
  scale_linetype_manual(values = c(1,2,2)) + #+ labs(color='', linetype = '') +
  xlab("Date") +
  geom_point(data = fit_subs[[1]]$pmcmc_results$inputs$data, aes(x = as.Date(date), y = deaths), alpha = 0.5, size = 0.5)
p2
# geom_ribbon(data = subset(Deaths_df, IFR == "IFR_1_Slope0.8"),
#             aes(ymin = Deaths, ymax = subset(Deaths_df, IFR == "IFR_1_Slope1.2")[,"Deaths"]),
#             fill = "darkred", alpha = 0.2) +
# geom_ribbon(data = subset(Deaths_df, IFR == "IFR_0.6_Slope1"),
#             aes(ymin = Deaths, ymax = subset(Deaths_df, IFR == "IFR_1.4_Slope1")[,"Deaths"]),
#             fill = "darkorange", alpha = 0.2)



## Plot 3: Plot the PCR/Sero curves
sero_pcr_df_l_IFR_slope <- lapply(X = fit_subs, FUN = function(x){
  seroprev_df(x)})

# Simplify data
Simple_Plot_Data_l_IFR_slope <- lapply(sero_pcr_df_l_IFR_slope, Summ_sero_pcr_data)

colnames <- names(Simple_Plot_Data_l_IFR_slope)
pcr_sero_list <- lapply(1:length(Simple_Plot_Data_l_IFR_slope), function(x){
  sero_pcr_tmp <- Simple_Plot_Data_l_IFR_slope[[x]] %>% select(date, mean_pcr, mean_sero)
  names(sero_pcr_tmp)[2] <- paste0("PCR_",colnames[x])
  names(sero_pcr_tmp)[3] <- paste0("sero_",colnames[x])
  sero_pcr_tmp
})


sero_df <- pcr_sero_list %>% reduce(full_join, by = "date") %>%
  select(date, paste0("sero_IFR_",c(1,1.4,1.2),"_Slope",c(1,0.8,0.8))) %>% arrange(date) %>%
  rename(IFR_1_Slope1 = "sero_IFR_1_Slope1",
         IFR_1.4_Slope0.8 = "sero_IFR_1.4_Slope0.8",
         IFR_1.2_Slope0.8 = "sero_IFR_1.2_Slope0.8") %>%
  melt(id = c("date"), variable = "IFR", value.name = "sero")

pcr_df <- pcr_sero_list %>% reduce(full_join, by = "date") %>%
  select(date, paste0("PCR_IFR_",c(1,1.4,1.2),"_Slope",c(1,0.8,0.8))) %>% arrange(date) %>%
  rename(IFR_1_Slope1 = "PCR_IFR_1_Slope1",
         IFR_1.4_Slope0.8 = "PCR_IFR_1.4_Slope0.8",
         IFR_1.2_Slope0.8 = "PCR_IFR_1.2_Slope0.8") %>%
  melt(id = c("date"), variable = "IFR", value.name = "pcr")

pcr_sero_df <- full_join(x = sero_df, y = pcr_df, by = c("date","IFR"))

p3 <- ggplot(pcr_sero_df, aes(x = date, y = pcr, group = IFR, col = IFR, linetype = IFR)) +
  geom_line() +
  scale_colour_manual(values = c("black","darkred","darkblue")) +
  scale_linetype_manual(values = c(1,2,2)) +# labs(color='', linetype = '') +
  geom_line(aes(y = sero/3)) +
  scale_y_continuous(
    name = "PCR+ prevalence", sec.axis = sec_axis(~.*3, name = "Seroprevalence"),
  ) + xlab("Date") +
  geom_point(aes(x= as.Date("2020-07-15"),y=7.6), col = "black", size = 0.9) +
  geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10), col = "black", width = 3, size = 0.3) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0), col = "black", size = 0.3, height = 0.2) +
  # annotate("text", x=as.Date("2020-07-15")+5, y=7.6+0.5, label="Mulenga et al.", hjust=0) +
  geom_point(aes(x= as.Date("2020-07-15"),y=2.1/3), col = "black", size = 0.9) +
  geom_errorbar(aes(ymin=1.1/3,ymax=3.1/3,x=as.Date("2020-07-15"), width=10), col = "black", width = 3, size = 0.3) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1/3, height=0), col = "black", size = 0.3, height = 0.2)+
  # annotate(geom = "text", x = "2020-06-01", y = 7.6,  label = "PCR") +
  annotate(geom = "text", x=as.Date("2020-07-19"), y = 8, hjust=0, label = "PCR+", fontface = "bold", size = 2.5) +
  annotate(geom = "text", x=as.Date("2020-07-19"), y = 1.1, hjust=0, label = "Sero+", fontface = "bold", size = 2.5)


# geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_1_Slope0.8"),
#             aes(ymin = sero/3, ymax = subset(pcr_sero_df, IFR == "IFR_1_Slope1.2")[,"sero"]/3),
#             fill = "darkred", alpha = 0.1) +
# geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_0.6_Slope1"),
#             aes(ymin = sero/3, ymax = subset(pcr_sero_df, IFR == "IFR_1.4_Slope1")[,"sero"]/3),
#             fill = "darkorange", alpha = 0.1) +
# geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_1_Slope0.8"),
#             aes(ymin = pcr, ymax = subset(pcr_sero_df, IFR == "IFR_1_Slope1.2")[,"pcr"]),
#             fill = "darkred", alpha = 0.1) +
# geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_0.6_Slope1"),
#             aes(ymin = pcr, ymax = subset(pcr_sero_df, IFR == "IFR_1.4_Slope1")[,"pcr"]),
#             fill = "darkorange", alpha = 0.1)
p3



# Plot 4: Plot the Death distribution by age of each:
# Can I also plot the expected points from the BMJ paper?
# Get the proportion of deaths from BMJ
BMJ_Deaths_by_Age_Covid_Neg <- c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2)
BMJ_Deaths_by_Age_Covid_Pos <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Covid_Pos2 <- c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Tot <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2)

Deaths_Age_List <- lapply(1:length(fit_subs), function(x){
  # browser()
  return(rowMeans(fit_subs[[x]]$output[nrow(fit_subs[[x]]$output),paste0("D[",1:17,"]"),]))
})
names(Deaths_Age_List) <- names(fit_subs)
Deaths_Age_df<-do.call(cbind.data.frame, Deaths_Age_List) %>%
  mutate(Age = Age_vals) %>%
  melt(id = "Age", variable = "IFR", value.name = "Deaths")

Est_Deaths <- data.frame(Age = Age_vals,
                         Deaths = c((BMJ_Deaths_by_Age_Covid_Pos2*10/0.8)[1:16],sum((BMJ_Deaths_by_Age_Covid_Pos2*10/0.8)[17:20])))

p4 <- ggplot(Deaths_Age_df, aes(x = Age)) +
  geom_line(aes(y = Deaths, group = IFR, col = IFR, linetype = IFR)) +
  scale_colour_manual(values = c("black","darkred","darkblue")) +
  scale_linetype_manual(values = c(1,2,2)) + labs(color='', linetype = '') +
  geom_point(data = Est_Deaths, aes(x = Age, y = Deaths), size = 0.5, alpha = 1)






# pdf(file = "analysis/figures/12_06_Best_Fit_Comp.pdf", height = 6, width = 8)
pdf(file = paste0("analysis/figures/12_06_Best_Fit_Comp_",Dataset_Name,".pdf"), height = 6, width = 8)
cowplot::plot_grid(p1 + theme_light() + theme(legend.position = "none"),
                   p2 + theme_light() +theme(legend.position = "none"),
                   cowplot::get_legend(p1 + theme_light() + labs(color='') + theme(legend.box.margin = margin(130,0,0,10))),
                   p3 + theme_light() +theme(legend.position = "none"),
                   p4 + theme_light() +theme(legend.position = "none"),
                   nrow = 2, rel_widths = c(4, 4, 2))
dev.off()

summary(cowplot::get_legend(p2))






lines(x = Age_groups,
      y = Age_groups*IFR_mat[2,"Slope_abs"] + IFR_mat[2,"Int_abs"],
      col = "darkred", lty = 2)
lines(x = Age_groups,
      y = Age_groups*IFR_mat[8,"Slope_abs"] + IFR_mat[8,"Int_abs"],
      col = "darkgreen", lty = 2)
lines(x = Age_groups,
      y = Age_groups*IFR_mat[10,"Slope_abs"] + IFR_mat[10,"Int_abs"],
      col = "darkgreen", lty = 2)

legend("topleft", legend = c("Brazeau IFR","1.2xIFR, 0.9xSlope","1.1xIFR, 1.2xSlope","1.4xIFR, 1.6xSlope"), col = c(1,"darkblue","darkred","darkgreen"), lty = c(1,2,2,2), bty = "n")





## Plot the proportion of total deaths in BMJ data that occur in each age group
BMJ_Deaths_by_Age_Covid_Neg <- c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2)
BMJ_Deaths_by_Age_Covid_Pos <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Tot <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2)

barplot(BMJ_Deaths_by_Age_Covid_Pos/BMJ_Deaths_by_Age_Tot)
barplot(BMJ_Deaths_by_Age_Covid_Pos/sum(BMJ_Deaths_by_Age_Covid_Pos),
        names = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-90","90-94","95-99",">99"), col="darkgrey", las = 2,
        ylab = "Proportion of Deaths")

## I also need the prevalence of death in each age group from the model we ran:

tail(Off_data$output)
str(Off_data$output)
colnames(Off_data$output)
plot(tail(Off_data$output[,"D[2]",],1))

tail(Off_data$output[,paste0("D[",1,"]"),],1)

names(Off_data$output[,"D[1]",1]) < "2020-10-1" & names(Off_data$output[,"D[1]",1]) > "2020-06-14"

Tot_Deaths_by_Age_gr <- sapply(1:17, function(x){
  mean(Off_data$output["2020-10-01",paste0("D[",x,"]"),]) - mean(Off_data$output["2020-06-14",paste0("D[",x,"]"),])
})

Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr)

c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])))

dmultinom(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21])),
          size = sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))),
          prob = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr))



dmultinom(x = round(100*c(0.002,0.004,0.01,0.012,0.014,0.019,0.03,0.04,0.05,0.05,0.05,0.05,0.07,0.09,0.1,0.13,0.2)),
          size = sum(round(100*c(0.002,0.004,0.01,0.012,0.014,0.019,0.03,0.04,0.05,0.05,0.05,0.05,0.07,0.09,0.1,0.13,0.2))),
          prob = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr))


plot(x = 1:17, y = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))), pch = 20, ylim = c(0,0.2), ylab = "Proportion Of Deaths")
points(x = 1:17, y = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr), col = 2, pch = 20)
legend("topleft", legend = c("BMJ","Model"), pch = 20, col = c(1,2))

plot(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))),
     y = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr), xlab = "BMJ Death Proportion", ylab = "Model Death Proportion",
     pch = 1:17, type = "n")
text(x = c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))/sum(c(BMJ_Deaths_by_Age_Covid_Pos[1:16],sum(BMJ_Deaths_by_Age_Covid_Pos[17:21]))),
     y = Tot_Deaths_by_Age_gr/sum(Tot_Deaths_by_Age_gr))
abline(a = c(0,1))
