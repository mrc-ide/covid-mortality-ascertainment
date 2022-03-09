# Run odin model to calculate log-likelihood
# return: Set to 'll' to return the log-likelihood (for MCMC) or to
#
calc_loglikelihood_Hyper_Geo_Lik <- function(pars, data, squire_model, model_params,
                                       pars_obs, n_particles,
                                       forecast_days = 0, return = "ll",
                                       Rt_args,
                                       interventions,...) {
# browser()
  #----------------..
  # specify particle setup
  #----------------..
  switch(return,
         "full" = {
           save_particles <- TRUE
           full_output <- TRUE
           pf_return <- "sample"
         },
         "ll" = {
           save_particles <- FALSE
           forecast_days <- 0
           full_output <- FALSE
           pf_return <- "single"
         },
         {
           stop("Unknown return type to calc_loglikelihood")
         }
  )

  #----------------..
  # (potentially redundant) assertion
  #----------------..
  squire:::assert_in(c("R0", "start_date"), names(pars),
                     message = "Must specify R0, start date to infer")

  #----------------..
  # unpack current params
  #----------------..

  R0 <- pars[["R0"]]
  start_date <- pars[["start_date"]]

  # reporting fraction par if in pars list
  if("rf" %in% names(pars)) {
    assert_numeric(pars[["rf"]])
    pars_obs$phi_death <- pars[["rf"]]
  }


  #----------------..
  # more assertions
  #----------------..
  squire:::assert_pos(R0)
  squire:::assert_date(start_date)

  #----------------..
  # setup model based on inputs and interventions
  #----------------..
  R0_change <- interventions$R0_change
  date_R0_change <- interventions$date_R0_change
  date_contact_matrix_set_change <- interventions$date_contact_matrix_set_change
  date_ICU_bed_capacity_change <- interventions$date_ICU_bed_capacity_change
  date_hosp_bed_capacity_change <- interventions$date_hosp_bed_capacity_change
  date_vaccine_change <- interventions$date_vaccine_change
  date_vaccine_efficacy_infection_change <- interventions$date_vaccine_efficacy_infection_change
  date_vaccine_efficacy_disease_change <- interventions$date_vaccine_efficacy_disease_change

  # change betas
  if (is.null(date_R0_change)) {
    tt_beta <- 0
  } else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_R0_change,
                                                    change = R0_change,
                                                    start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt),
                                                    starting_change = 1)
    model_params$tt_beta <- tt_list$tt
    R0_change <- tt_list$change
    date_R0_change <- tt_list$dates
  }

  # and contact matrixes
  if (is.null(date_contact_matrix_set_change)) {
    tt_contact_matrix <- 0
  } else {

    # here just provide positions for change and then use these to index mix_mat_set
    tt_list <- squire:::intervention_dates_for_odin(dates = date_contact_matrix_set_change,
                                                    change = seq_along(interventions$contact_matrix_set)[-1],
                                                    start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt),
                                                    starting_change = 1)


    model_params$tt_matrix <- tt_list$tt
    model_params$mix_mat_set <- model_params$mix_mat_set[tt_list$change,,]
  }

  # and icu beds
  if (is.null(date_ICU_bed_capacity_change)) {
    tt_ICU_beds <- 0
  } else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_ICU_bed_capacity_change,
                                                    change = interventions$ICU_bed_capacity[-1],
                                                    start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt),
                                                    starting_change = interventions$ICU_bed_capacity[1])
    model_params$tt_ICU_beds <- tt_list$tt
    model_params$ICU_beds <- tt_list$change
  }

  # and hosp beds
  if (is.null(date_hosp_bed_capacity_change)) {
    tt_hosp_beds <- 0
  } else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_hosp_bed_capacity_change,
                                                    change = interventions$hosp_bed_capacity[-1],
                                                    start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt),
                                                    starting_change = interventions$hosp_bed_capacity[1])
    model_params$tt_hosp_beds <- tt_list$tt
    model_params$hosp_beds <- tt_list$change
  }

  # and vaccine coverage
  if (is.null(date_vaccine_change)) {
    tt_vaccine <- 0
  } else {
    tt_list <- squire:::intervention_dates_for_odin(dates = date_vaccine_change,
                                                    change = interventions$max_vaccine[-1],
                                                    start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt),
                                                    starting_change = interventions$max_vaccine[1])
    model_params$tt_vaccine <- tt_list$tt
    model_params$max_vaccine <- tt_list$change
  }

  # and vaccine efficacy infection
  if (is.null(date_vaccine_efficacy_infection_change)) {
    tt_vaccine_efficacy_infection <- 0
  } else {

    # here we just pass the change as a position vector as we need to then
    # index the array of vaccine efficacies
    tt_list <- squire:::intervention_dates_for_odin(dates = date_vaccine_efficacy_infection_change,
                                                    change = seq_along(interventions$vaccine_efficacy_infection)[-1],
                                                    start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt),
                                                    starting_change = 1)

    model_params$tt_vaccine_efficacy_infection <- tt_list$tt

    # here we have to not index the array by the postion vectors that are reutrned by intervention_dates_for_odin
    model_params$vaccine_efficacy_infection <- model_params$vaccine_efficacy_infection[tt_list$change,,]
  }

  # and vaccine efficacy disease
  if (is.null(date_vaccine_efficacy_disease_change)) {
    tt_vaccine_efficacy_disease <- 0
  } else {

    # here we just pass the change as a position vector as we need to then
    # index the array of vaccine efficacies
    tt_list <- squire:::intervention_dates_for_odin(dates = date_vaccine_efficacy_disease_change,
                                                    change = seq_along(interventions$vaccine_efficacy_disease)[-1],
                                                    start_date = start_date,
                                                    steps_per_day = round(1/model_params$dt),
                                                    starting_change = 1)

    model_params$tt_vaccine_efficacy_disease <- tt_list$tt

    # here we have to not index the array by the position vectors that are returned by intervention_dates_for_odin
    model_params$prob_hosp <- model_params$prob_hosp[tt_list$change,,]
  }

  #--------------------..
  # update new R0s based on R0_change and R0_date_change, and Meff_date_change
  #--------------------..
  # and now get new R0s for the R0
  R0 <- squire:::evaluate_Rt_pmcmc(R0_change = R0_change,
                                   R0 = R0,
                                   date_R0_change = date_R0_change,
                                   pars = pars,
                                   Rt_args = Rt_args)

  # which allow us to work out our beta
  beta_set <- squire:::beta_est(squire_model = squire_model,
                                model_params = model_params,
                                R0 = R0)

  #----------------..
  # update the model params accordingly from new inputs
  #----------------..
  model_params$beta_set <- beta_set


  #####################################################
  #####################################################

  #----------------..
  # run the particle filter
  #----------------..
  if (inherits(squire_model, "stochastic")) {

    pf_result <- squire:::run_particle_filter(data = data,
                                              squire_model = squire_model,
                                              model_params = model_params,
                                              model_start_date = start_date,
                                              obs_params = pars_obs,
                                              n_particles = n_particles,
                                              forecast_days = forecast_days,
                                              save_particles = save_particles,
                                              full_output = full_output,
                                              return = pf_return)

  } else if (inherits(squire_model, "deterministic")) {

    pf_result <- run_deterministic_comparison_HYPGEO(data = data,
                                                      squire_model = squire_model,
                                                      model_params = model_params,
                                                      model_start_date = start_date,
                                                      obs_params = pars_obs,
                                                      forecast_days = forecast_days,
                                                      save_history = save_particles,
                                                      return = pf_return)

  }

  # out
  pf_result
}


#############################################################################
#############################################################################
#############################################################################
run_deterministic_comparison_HYPGEO <- function(data,
                                                squire_model,
                                                 model_params,
                                                 model_start_date = "2020-02-02",
                                                 obs_params = list(phi_cases = 0.1,
                                                                   k_cases = 2,
                                                                   phi_death = 1,
                                                                   k_death = 2,
                                                                   exp_noise = 1e6),
                                                 forecast_days = 0,
                                                 save_history = FALSE,
                                                 return = "ll") {


  # parameter checks
  if (!(return %in% c("full", "ll", "sample", "single"))) {
    stop("return argument must be full, ll, sample", "single")
  }
  if (as.Date(data$date[data$deaths > 0][1], "%Y-%m-%d") < as.Date(model_start_date, "%Y-%m-%d")) {
    stop("Model start date is later than data start date")
  }

  # convert data into particle-filter form
  data <- squire:::particle_filter_data(data = data,
                                        start_date = model_start_date,
                                        steps_per_day = round(1 / model_params$dt))

  model_params$tt_beta <- round(model_params$tt_beta*model_params$dt)
  model_params$tt_contact_matrix <- round(model_params$tt_contact_matrix*model_params$dt)
  model_params$tt_hosp_beds <- round(model_params$tt_hosp_beds*model_params$dt)
  model_params$tt_ICU_beds <- round(model_params$tt_ICU_beds*model_params$dt)

  #set up model
  model_func <- squire_model$odin_model(user = model_params,
                                        unused_user_action = "ignore")

  # steps for the deterministic
  steps <- c(0, data$day_end)
  fore_steps <- seq(data$day_end[nrow(data)], length.out = forecast_days + 1L)
  steps <- unique(c(steps,fore_steps))

  # model run
  if("atol" %in% names(obs_params) && "rtol" %in% names(obs_params)) {
    squire:::assert_numeric(obs_params$atol)
    atol <- obs_params$atol
    squire:::assert_numeric(obs_params$rtol)
    rtol <- obs_params$rtol
  } else {
    atol <- 1e-6
    rtol <- 1e-6
  }



  out <- model_func$run(t = seq(0, tail(steps,1), 1), atol = atol, rtol = rtol)
  index <- squire:::odin_index(model_func)

  # browser()

  ### Actual data for comparison
  Comb_data <- obs_params$combined_data
  pcr_det <- obs_params$pcr_det

  ### Get model data for comparison
  Dates_needed <- c(unique(Comb_data$date)-1,tail(unique(Comb_data$date)-1,1)+7)  ## These are the day ends I want.
  Date_sequence <- seq(from = min(as.Date(data$date))+1, to = as.Date(max(as.Date(Comb_data$date)))+7, by = 1) ## Create a date sequence to encompass everything, starting with day 1
  Days_for_comparison <- seq_along(Date_sequence)[Date_sequence %in% Dates_needed] # Day 1 is 2020-05-28: Day start = 1, Day end = 0/29? I need 2020-05-14 day end
  Days_for_comparison_b <- ifelse(Days_for_comparison>max(data$day_end),max(data$day_end),Days_for_comparison) # If any days are past the available days, lower the highest to the max day


  ### Model Deaths
  Mod_Deaths_Age <- apply(out[Days_for_comparison_b,index$D], 2, diff)
  colnames(Mod_Deaths_Age) <- 1:17
  Mod_Deaths_Age <- Mod_Deaths_Age %>%
    reshape2::melt(value.name = "Ds", varnames= c("Week_num","Age_gr")) # 16 time periods, 17 age groups.

  roll_func <- function(x, det) {
    l <- length(det)
    ret <- rep(0, length(x))
    for(i in seq_along(ret)) {
      to_sum <- tail(x[seq_len(i)], length(det))
      ret[i] <- sum(rev(to_sum)*head(det, length(to_sum)))
    }
    return(ret)
  }


  ### Model PCR: Number of Infections
  pcr_pos <- apply(out[,index$S],2,function(x){
    # browser()
    x <- as.integer(x)
    infs <- c(0,diff(max(x)-x))
    pcr_positive <- roll_func(infs, pcr_det)
    pcr_perc <- pcr_positive/max(x)
  })

  pcr_perc <- pcr_pos[Days_for_comparison[-1]-3,] # This could be -3 or -4 to get Weds or Thurs.
  colnames(pcr_perc) <- 1:17 # Reshape
  pcr_perc <- pcr_perc %>% reshape2::melt(value.name = "pcr_perc", varnames= c("Week_num","Age_gr")) # 16 time periods, 17 age groups.


  ################################################
  ################################################

  # calculate ll for deaths
  frac_mort <- 0.8 # Mortuary captures 80% of deaths in Lusaka
  Mod_Deaths_Age <- Mod_Deaths_Age %>% dplyr::rename(age_group = "Age_gr", week_no = "Week_num")
  Comb_data <- Comb_data %>% dplyr::rename(age_group = "Age_group", week_no = "Week_gr")
  pcr_perc <- pcr_perc %>% dplyr::rename(age_group = "Age_gr", week_no = "Week_num")

  Mod_Deaths_Age <- Mod_Deaths_Age %>% merge(x = ., y = Comb_data) %>%
    merge(x = ., y = pcr_perc) %>%
    mutate(Mod_cd = Ds) %>%
    mutate(Mod_cd_Lus = Mod_cd*frac_mort) %>%
    mutate(tot_mort_deaths = total_deaths) %>%
    mutate(Mod_ncd = (total_deaths - Mod_cd_Lus)) %>% # non-covid deaths
    mutate(Mod_pos_ncd = Mod_ncdx*pcr_perc) %>%
    mutate(Mod_tot_pos_mort = Mod_cd_Lus+Mod_pos_ncd)

  # browser()

  # Likelihood_data <- data.frame(Pos_Samples = Comb_data$CT_45_Either,
  #            Samples = Comb_data$sampled_deaths,
  #            Mod_Pos_deaths = round(covid_pos_mort),
  #            TotD_min_ModD = round(Comb_data$total_deaths - covid_pos_mort)
  #            )
# browser()
  Likelihood_data <- Mod_Deaths_Age %>%
    mutate(Mod_tot_neg_mort = tot_mort_deaths-Mod_tot_pos_mort) %>%
    mutate(Mod_tot_neg_mort = ifelse(Mod_tot_neg_mort<0, sampled_deaths-CT_45_Either, Mod_tot_neg_mort)) %>% # If the number of modelled covid deaths exceeds the total number of deaths (ie the number of non-covid deaths is negative), increase the modelled number of non-covid deaths to the minimum it needs to be: min number of covid deaths that we know occurred.
    mutate(Mod_tot_pos_mort = ifelse(CT_45_Either>Mod_tot_pos_mort, CT_45_Either,Mod_tot_pos_mort)) %>% # If the number of modelled positive tests is below the number of positive tests we know occurred, increase this to the minimum value it needs to be
    mutate(Mod_tot_neg_mort = ifelse((sampled_deaths - CT_45_Either)>Mod_tot_neg_mort, (sampled_deaths - CT_45_Either),Mod_tot_neg_mort)) %>% # If the number of modelled negative tests is below the number of negative tests we know occurred, increase this to the minimun value it needs to be
    mutate(ll = dhyper(x = CT_45_Either, m =  as.integer(Mod_tot_pos_mort), n = as.integer(Mod_tot_neg_mort), k = sampled_deaths, log = T))

  # Likelihood_data$LL
  # Likelihood_data[70:80,]

  # Pos_Samples <- Comb_data$CT_45_Either
  # Samples <- Comb_data$sampled_deaths
  # Mod_Pos_deaths <- round(covid_pos_mort)
  # TotD_min_ModD <- round(Comb_data$total_deaths - covid_pos_mort) %>% ifelse(.<0, Samples-Pos_Samples, .)

# browser()
  # cbind(Comb_data$Week_gr, Comb_data$Age_group, Pos_Samples,Mod_Pos_deaths, Comb_data$total_deaths, TotD_min_ModD,Samples, sapply(1:length(Pos_Samples), function(x){
  #   # browser()
  #   dhyper(x = Pos_Samples[x], m =  Mod_Pos_deaths[x], n = TotD_min_ModD[x], k = Samples[x])
  # }))[40:50,]
  # cbind(Pos_Samples,Mod_Pos_deaths,Comb_data$total_deaths, TotD_min_ModD,Samples,sapply(1:length(Pos_Samples), function(x){
  #   # browser()
  #   dhyper(x = Pos_Samples[x], m =  Mod_Pos_deaths[x], n = TotD_min_ModD[x], k = Samples[x])
  # }))[240:250,]

  ll <- Likelihood_data$ll

# browser()

    # ll <- ll_nbinom_HYPGEO(data = Comb_data, model_deaths =  Mod_Deaths_Age, model_pcr_pos = pcr_perc,  obs_params$phi_death, obs_params$k_death, obs_params$exp_noise, obs_params$phi_change)


  # # calculate ll for the seroprevalence
  # lls <- 0
  # if("sero_df" %in% names(obs_params) && "sero_det" %in% names(obs_params)) {
  #
  #   sero_df <- obs_params$sero_df
  #   sero_det <- obs_params$sero_det
  #
  #   # put some checks here that sero_df is correctly formatted
  #   squire:::check_sero_df(sero_df)
  #
  #   # were there actually seroprevalence data points to compare against
  #   if(nrow(sero_df) > 0) {
  #
  #     sero_at_date <- function(date, symptoms, det, dates, N) {
  #
  #       di <- which(dates == date)
  #       if(length(di) > 0) {
  #         to_sum <- tail(symptoms[seq_len(di)], length(det))
  #         min(sum(rev(to_sum)*head(det, length(to_sum)), na.rm=TRUE)/N, 0.99)
  #       } else {
  #         0
  #       }
  #
  #     }
  #
  #     # get symptom incidence
  #     symptoms <- rowSums(out[,index$E2]) * model_params$gamma_E
  #
  #     # dates of incidence, pop size and dates of sero surveys
  #     dates <- data$date[[1]] + seq_len(nrow(out)) - 1L
  #     N <- sum(model_params$population)
  #     sero_dates <- list(sero_df$date_end, sero_df$date_start, sero_df$date_start + as.integer((sero_df$date_end - sero_df$date_start)/2))
  #     unq_sero_dates <- unique(c(sero_df$date_end, sero_df$date_start, sero_df$date_start + as.integer((sero_df$date_end - sero_df$date_start)/2)))
  #     det <- obs_params$sero_det
  #
  #     # estimate model seroprev
  #     sero_model <- vapply(unq_sero_dates, sero_at_date, numeric(1), symptoms, det, dates, N)
  #     sero_model_mat <- do.call(cbind,lapply(sero_dates, function(x) {sero_model[match(x, unq_sero_dates)]}))
  #
  #     # likelihood of model obvs
  #     lls <- rowMeans(dbinom(sero_df$sero_pos, sero_df$samples, sero_model_mat, log = TRUE))
  #
  #   }
  #
  # }
  #
  # # calculate ll for the PCR prevalence
  # llp <- 0
  # if("pcr_df" %in% names(obs_params) && "pcr_det" %in% names(obs_params)) {
  #
    # pcr_df <- obs_params$pcr_df
    # pcr_det <- obs_params$pcr_det
  #
  #   # put some checks here that pcr_df is correctly formatted
  #   squire:::check_pcr_df(pcr_df)
  #
  #   # were there actually pcr prevalence data points to compare against
  #   if(nrow(pcr_df) > 0) {
  #
      # pcr_at_date <- function(date, infections, det, dates, N) {
      #
      #   di <- which(dates == date)
      #   if(length(di) > 0) {
      #     to_sum <- tail(infections[seq_len(di)], length(det))
      #     min(sum(rev(to_sum)*head(det, length(to_sum)), na.rm=TRUE)/N, 0.99)
      #   } else {
      #     0
      #   }
      #
      # }
  #
  #     # get infection incidence
  #     infections <- c(0,rowSums(out[-nrow(out),index$S]-out[-1,index$S]))
  # #
  # #     # dates of incidence, pop size and dates of pcr surveys
  #     dates <- data$date[[1]] + seq_len(nrow(out)) - 1L
  #     N <- sum(model_params$population)
  #     pcr_dates <- list(pcr_df$date_end, pcr_df$date_start, pcr_df$date_start + as.integer((pcr_df$date_end - pcr_df$date_start)/2))
  #     # pcr_dates <- list(data$date[Days_for_comparison_b])
  #     unq_pcr_dates <- unique(c(pcr_df$date_end, pcr_df$date_start, pcr_df$date_start + as.integer((pcr_df$date_end - pcr_df$date_start)/2)))
  #     det <- obs_params$pcr_det
  #
  # #     # estimate model pcrprev
  #     pcr_model <- vapply(unq_pcr_dates, pcr_at_date, numeric(1), infections, det, dates, N)
  #     pcr_model_mat <- do.call(cbind,lapply(pcr_dates, function(x) {pcr_model[match(x, unq_pcr_dates)]}))
  #
  #     # likelihood of model obvs
  #     llp <- rowMeans(dbinom(pcr_df$pcr_pos, pcr_df$samples, pcr_model_mat, log = TRUE))
  #
  #   }

  # }
  # browser()
  # out

  # format the out object
  date <- data$date[[1]] + seq_len(nrow(out)) - 1L
  rownames(out) <- as.character(date)
  attr(out, "date") <- date

  # format similar to particle_filter nomenclature
  pf_results <- list()
  pf_results$log_likelihood <- sum(ll)# + sum(lls) + sum(llp)

  # single returns final state
  if (save_history) {
    pf_results$states <- out
  } else if (return == "single") {
    pf_results$sample_state <- out[nrow(out), ]
  }

  # create returned object
  if (return == "ll") {
    ret <- pf_results$log_likelihood
  } else if (return == "sample") {
    ret <- pf_results$states
  } else if (return == "single" || return == "full") {
    ret <- pf_results
  }

  ret
}


#############################################################################
#############################################################################
#############################################################################
# ll_nbinom_HYPGEO <- function(data, model_deaths, model_pcr_pos, phi, k, exp_noise, phi_change) {
#   # browser()
#   # if(length(phi)==2 & !is.null(phi_change)){
#   #   mu <- c(phi[1] * model[1:(phi_change-1)] + rexp(length(model), rate = exp_noise), phi[2] * model[phi_change:length(model)]) + rexp(length(model), rate = exp_noise)
#   # } else {
#   #   mu <- phi * model + rexp(length(model), rate = exp_noise)
#   # }
#
#   # mu <- model
#
#   # Sampled_Pos_Deahts (Data)
#   # Total sampled (Data)
#   # Model: (Total deaths + incidental deaths) * 0.8
#   # Model: Total in +ve deaths hospital
#
#   Comb_data
#   Mod_Deaths_Age$Ds
#   cbind(Comb_data$Week_gr, Comb_data$CT_45_Either,Comb_data$total_deaths, Mod_Deaths_Age$Ds, round(Comb_data$total_deaths - Mod_Deaths_Age$Ds), Comb_data$sampled_deaths)
#
#   dhyper(x = Comb_data$CT_45_Either, m =  Mod_Deaths_Age$Ds, n = Comb_data$total_deaths - Mod_Deaths_Age$Ds, k = Comb_data$sampled_deaths)
#
#
# }
#

# Roll function
roll_func <- function(x, det) {
  l <- length(det)
  ret <- rep(0, length(x))
  for(i in seq_along(ret)) {
    to_sum <- tail(x[seq_len(i)], length(det))
    ret[i] <- sum(rev(to_sum)*head(det, length(to_sum)))
  }
  return(ret)
}

