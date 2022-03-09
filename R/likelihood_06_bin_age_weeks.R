# Run odin model to calculate log-likelihood
# return: Set to 'll' to return the log-likelihood (for MCMC) or to
#
calc_loglikelihood_bin_age_weeks_06 <- function(pars, data, squire_model, model_params,
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

    pf_result <- run_deterministic_bin_age_weeks_06(data = data,
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
run_deterministic_bin_age_weeks_06 <- function(data,
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

  ##############################################################################
  ##############################################################################
  ######################## Likelihood calculations #############################
  ##############################################################################
  ##############################################################################

  ### Get model output
  out <- model_func$run(t = seq(0, tail(steps,1), 1), atol = atol, rtol = rtol)
  index <- squire:::odin_index(model_func)

  ### Assign dates to the output
  date <- data$date[[1]] + seq_len(nrow(out)) - 1L
  rownames(out) <- as.character(date)
  attr(out, "date") <- date

  ### Data for comparison: age and weeks
  pcr_det <- obs_params$pcr_det
  frac_mort <- 0.8 # Mortuary captures 80% of deaths in Lusaka

  ### Get model data for comparison
  Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                   to = as.Date("2020-10-02"),
                                   by = "week"),as.Date("2020-10-02"))

  Mod_Deaths_Age <- apply(out[as.Date(rownames(out)) %in% Days_for_comparison,index$D], 2, diff)
  colnames(Mod_Deaths_Age) <- 1:17
  rownames(Mod_Deaths_Age) <- 1:16
  Mod_Deaths_Age <- Mod_Deaths_Age %>%
    reshape2::melt(value.name = "Ds", varnames= c("Week_num","Age_gr")) # 16 time periods, 17 age groups.

  pcr_pos_age <- apply(out[,index$S],2,function(x){
    x <- as.integer(x)
    infs <- c(0,diff(max(x)-x))
    pcr_positive <- roll_func(infs, pcr_det)
    pcr_perc <- pcr_positive/max(x)
  })

  pcr_perc_age <- pcr_pos_age[as.Date(rownames(out)) %in% (Days_for_comparison[-1]-3),] # This could be -3 or -4 to get Weds or Thurs.
  colnames(pcr_perc_age) <- 1:17 # Reshape
  pcr_perc_age <- pcr_perc_age %>% reshape2::melt(value.name = "pcr_perc", varnames= c("Week_num","Age_gr")) # 16 time periods, 17 age groups.

  Mod_Deaths_Age <- Mod_Deaths_Age %>% dplyr::rename(age_group = "Age_gr", week_no = "Week_num")
  Comb_data <- obs_params$combined_data %>% dplyr::rename(age_group = "Age_group", week_no = "Week_gr")
  pcr_perc_age <- pcr_perc_age %>% dplyr::rename(age_group = "Age_gr", week_no = "Week_num")

  Mod_Deaths_Age_ll <- Mod_Deaths_Age %>% merge(x = ., y = Comb_data) %>%
    merge(x = ., y = pcr_perc_age) %>%
    dplyr::rename(Mod_cd = Ds) %>%
    mutate(Mod_cd_UTH = Mod_cd*frac_mort) %>%
    dplyr::rename(tot_mort_deaths = total_deaths) %>%
    mutate(Mod_ncd = ifelse((tot_mort_deaths - Mod_cd_UTH)<0,0,(tot_mort_deaths - Mod_cd_UTH))) %>% # non-covid deaths
    mutate(Mod_pos_ncd = Mod_ncd*pcr_perc) %>%
    mutate(Mod_tot_pos_mort = Mod_cd_UTH+Mod_pos_ncd) %>%
    mutate(Mod_Pos_Tests = Mod_tot_pos_mort * Samples/tot_mort_deaths) %>%
    mutate(Pos_prev = ifelse(Mod_tot_pos_mort<=tot_mort_deaths,Mod_tot_pos_mort/tot_mort_deaths, 0.999999)) %>%
    mutate(ll = dbinom(x = PosTestNum, size = Samples, prob = Pos_prev, log = T))

  llaw <- sum(Mod_Deaths_Age_ll$ll)


  ################################################
  ################################################
  ## And the ll for the combined prevalence.
  llc <- 0

  if(all(unlist(lapply(list(obs_params$comb_df, obs_params$comb_det),function(x){!is.null(x)})))){
    comb_df <- obs_params$comb_df
    comb_det <- obs_params$comb_det

    ### Model PCR: Number of Infections/combined prevalence
    Sus <- rowSums(out[,index$S])
    infs <- c(0,as.integer(diff(max(Sus)-Sus)))
    comb_positive <- roll_func(infs, comb_det)
    comb_perc <- comb_positive/max(Sus)

    comb_dates <- c(comb_df$date_start, comb_df$date_end, comb_df$date_start + as.integer((comb_df$date_end - comb_df$date_start)/2))#list(comb_df$date_end, comb_df$date_start, comb_df$date_start + as.integer((comb_df$date_end - comb_df$date_start)/2))
    comb_perc <- comb_perc[as.Date(rownames(out)) %in% comb_dates] # This could be -3 or -4 to get Weds or Thurs.

    # likelihood of model obvs
    llc <- mean(dbinom(comb_df$comb_pos, comb_df$samples, comb_perc, log = TRUE))
  }

  ################################################
  ################################################
  ll <- llaw+llc

  # format similar to particle_filter nomenclature
  pf_results <- list()
  pf_results$log_likelihood <- ll

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

