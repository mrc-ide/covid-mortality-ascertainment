## Diagnostic plot:
devtools::load_all()
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
Res1 <- readRDS("../Bonus Files/2022-03-09_08_pois_bin_bin_decline_full_Just_Pois.rds")
Res2 <- readRDS("../Bonus Files/2022-03-09_08_pois_bin_bin_decline_full")

# readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")

Test1 <- Diagnostic_Plot_08(fit_num = 1, fit_Model_list = list(Res2[[1]]), IFRvals = readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[1,])

readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds")

Plots1 <- lapply(1:length(Res1), FUN = Diagnostic_Plot_08, fit_Model_list = Res1, IFRvals = readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),])
Plots2 <- lapply(1:length(Res2), FUN = Diagnostic_Plot_08, fit_Model_list = Res2, IFRvals = readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")[readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds"),])


pdf("analysis/figures/38_Model_Fit_Res_08_pois_bin_bin_JustPois_full.pdf", height = 8, width = 8)
Plots1
dev.off()

pdf("analysis/figures/38_Model_Fit_Res_08_pois_bin_bin_full.pdf", height = 9, width = 9)
Plots2
dev.off()


IFR_mat <- readRDS("analysis/data/Code-generated-data/00_03_IFR_matrix_coefficients_log_scale.rds")
IFR_vec <- 1:nrow(IFR_mat) %in% readRDS("analysis/data/Code-generated-data/00_03_Prob_Index_Vector_log_sc.rds")
IFR_mat_fil <- IFR_mat[IFR_vec,]
rownames(IFR_mat_fil) <- 1:nrow(IFR_mat_fil)

get_Posterior <- function(model_fit){
  # Select all sampled posteriors
  PosC1 <- model_fit$pmcmc_results$chains$chain1$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(model_fit$replicate_parameters), value = T))))]
  PosC2 <- model_fit$pmcmc_results$chains$chain2$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(model_fit$replicate_parameters), value = T))))]
  PosC3 <- model_fit$pmcmc_results$chains$chain3$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(model_fit$replicate_parameters), value = T))))]
  # Average out posteriors
  mean(unlist(list(PosC1,PosC2,PosC3)))
}

IFR_mat$AvPost <- NA
IFR_mat$AvPost[IFR_vec] <- unlist(lapply(Res1, get_Posterior))

Res1$X1$pmcmc_results$chains$chain1$results$log_likelihood
Res1$X1$pmcmc_results$chains$chain1$results$log_prior

## Plot heatmap of Posteriors
IFR_mat <- IFR_mat %>% mutate(Post_col_group = as.numeric(cut(round(AvPost),
                                                              breaks= c(-Inf,round(max(AvPost,na.rm = T))-500,
                                                                        round(max(AvPost,na.rm = T))-200,
                                                                        round(max(AvPost,na.rm = T))-100,
                                                                        round(max(AvPost,na.rm = T))-50,
                                                                        round(max(AvPost,na.rm = T))-20,
                                                                        round(max(AvPost,na.rm = T))-12,
                                                                        round(max(AvPost,na.rm = T))-8,
                                                                        round(max(AvPost,na.rm = T))-4,
                                                                        round(max(AvPost,na.rm = T))), labels = c("1","2","3","4","5","6","7","8","9"))))


ggplot(IFR_mat, aes(x = IFR_x, y = Slope_x, fill = Post_col_group)) + geom_tile() +
  geom_text(aes(label = round(AvPost), colour = (Post_col_group >= max(Post_col_group, na.rm=T))), size = 4) +
  scale_colour_manual(values = c("white", "black")) +
  ggtitle("Log Posterior") + xlab("xIFR") + ylab("xSlope") +
  labs(fill = "Mean Post") + theme(legend.position = "none") +
  xlim(0,2) + ylim(0,2) +
  scale_fill_viridis_c()




tail(Res1$X1$pmcmc_results$chains$chain1$results$log_posterior)
tail(Res1$X30$pmcmc_results$chains$chain1$results$log_posterior)
tail(Res1$X34$pmcmc_results$chains$chain1$results$log_posterior)

tail(Res2$X1$pmcmc_results$chains$chain1$results$log_posterior)
tail(Res2$X30$pmcmc_results$chains$chain1$results$log_posterior)
tail(Res2$X34$pmcmc_results$chains$chain1$results$log_posterior)


Test1$Diagnostic
Test1$Age_week





##### Confidence intervals

x_bar <- 10.6
LCI <- 7.3
UCI <- 13.9

Diff <- mean(c(x_bar - LCI,abs(x_bar - UCI)))

Dist_pop <- c(237299,190419,2731696,212070,585974,332649)
Tot_pop <- 4290107

100*(900/(Dist_pop/Tot_pop))/sum(500/(Dist_pop/Tot_pop))

Dist_Prevs <- c(5.6,4.8,63.1,4,14.9,7.5)/100

Samples <- Dist_Prevs/Dist_Weights
# So where did the confidence intervals come from?


sum(c(rep(1,230),rep(0,2848-230)))/2848



