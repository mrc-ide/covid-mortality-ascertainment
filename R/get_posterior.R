####

get_Posterior <- function(model_fit){
  # browser()
  # Select all sampled posteriors
  PosC1 <- model_fit$pmcmc_results$chains$chain1$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(model_fit$replicate_parameters), value = T))))]
  PosC2 <- model_fit$pmcmc_results$chains$chain2$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(model_fit$replicate_parameters), value = T))))]
  PosC3 <- model_fit$pmcmc_results$chains$chain3$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(model_fit$replicate_parameters), value = T))))]
  # Average out posteriors
  log(mean(exp(as.numeric(unlist(list(PosC1,PosC2,PosC3))))))
}

get_Prior <- function(model_fit){
  # browser()
  # Select all sampled posteriors
  PriC1 <- model_fit$pmcmc_results$chains$chain1$results$log_prior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(model_fit$replicate_parameters), value = T))))]
  PriC2 <- model_fit$pmcmc_results$chains$chain2$results$log_prior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(model_fit$replicate_parameters), value = T))))]
  PriC3 <- model_fit$pmcmc_results$chains$chain3$results$log_prior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(model_fit$replicate_parameters), value = T))))]
  # Average out posteriors
  log(mean(exp(as.numeric(unlist(list(PriC1,PriC2,PriC3))))))
}



# exp(-90)
# exp(-45) * exp(-2)
#
# ## log(2*5) = log(2) + log(5)
# log(0.5) + log(0.2)
# log(0.1)# -90
# exp(-90/2)
#
# exp(-9)
# -9
#
# exp(-4.5) * exp(-2)
#
# PosC1 <- model_fit$pmcmc_results$chains$chain1$results$log_prior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain1", rownames(model_fit$replicate_parameters), value = T))))]
# PosC2 <- model_fit$pmcmc_results$chains$chain2$results$log_likelihood[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain2", rownames(model_fit$replicate_parameters), value = T))))]
# PosC3 <- model_fit$pmcmc_results$chains$chain3$results$log_posterior[floor(as.numeric(gsub("^\\D*\\d.",replacement = "", grep("chain3", rownames(model_fit$replicate_parameters), value = T))))]
