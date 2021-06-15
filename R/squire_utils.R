
#' Fit squire model using rw splines
#'
#' @inheritParams squire::pmcmc
#' @inheritParams squire::parameters_explicit_SEEIR
#' @param rw_duration Random Walk/Spline Duration. Default = 14 days
#'
#' @return Model fit from [squire:::pmcmc]
#' @export
#'
fit_spline_rt <- function(data,
                          country,
                          population,
                          reporting_fraction,
                          n_mcmc = 10000,
                          replicates = 100,
                          rw_duration = 14,
                          hosp_beds = 10000000000,
                          icu_beds = 10000000000) {

  ## -----------------------------------------------------------------------------
  ## Step 1 DATA CLEANING AND ORDERING
  ## -----------------------------------------------------------------------------

  # order data
  data <- data[order(data$date),]
  data$date <- as.Date(data$date)

  # and remove the rows with no data up to the first date that a death was reported
  first_report <- which(data$deaths>0)[1]
  missing <- which(data$deaths == 0 | is.na(data$deaths))
  to_remove <- missing[missing<first_report]
  if(length(to_remove) > 0) {
    if(length(to_remove) == (nrow(data)-1)) {
      data <- data[-head(to_remove,-1),]
    } else {
      data <- data[-to_remove,]
    }
  }

  ## -----------------------------------------------------------------------------
  ## Step 2a: PMCMC SETUP
  ## -----------------------------------------------------------------------------

  # dat_0 is just the current date now
  date_0 <- max(data$date)

  # what is the date of first death
  null_na <- function(x) {if(is.null(x)) {NA} else {x}}
  min_death_date <- data$date[which(data$deaths>0)][1]

  # We set the R0_change here to be 1 everywhere to effectively turn off mobility
  R0_change <- rep(1, nrow(data))
  date_R0_change <- data$date
  R0_change <- R0_change[as.Date(date_R0_change) <= date_0]
  date_R0_change <- date_R0_change[as.Date(date_R0_change) <= date_0]

  # pmcmc args
  n_particles <- 2 # we use the deterministic model now so this does nothing (makes your life quicker and easier too)
  n_chains <- 3 # number of chains
  start_adaptation <- 1000 # how long before adapting

  # parallel call
  suppressWarnings(future::plan(future::multiprocess()))

  # Defualt parameter edges for pmcmc
  R0_min <- 1.6
  R0_max <- 5.6
  last_start_date <- as.Date(null_na(min_death_date))-10
  first_start_date <- as.Date(null_na(min_death_date))-55
  start_date <- as.Date(null_na(min_death_date))-30

  # These 4 parameters do nothign as setting R0_change to 1
  Meff_min <- -2
  Meff_max <- 2
  Meff_pl_min <- 0
  Meff_pl_max <- 1
  Rt_shift_min <- 0
  Rt_shift_max <- 0.001
  Rt_shift_scale_min <- 0.1
  Rt_shift_scale_max <- 10


  ## -----------------------------------------------------------------------------
  ## Step 2b: Sourcing suitable starting conditions
  ## -----------------------------------------------------------------------------

  date_start <- data$date[which(cumsum(data$deaths)>10)[1]] - 30
  R0_start <- 3

  # These are the the initial conditions now loaded from our previous run.
  R0_start <- min(max(R0_start, R0_min), R0_max)
  date_start <- min(max(as.Date(start_date), as.Date(first_start_date)), as.Date(last_start_date))

  # again these all do nothing
  Meff_start <- min(max(0, Meff_min), Meff_max)
  Meff_pl_start <- min(max(0.5, Meff_pl_min), Meff_pl_max)
  Rt_shift_start <- min(max(0.0005, Rt_shift_min), Rt_shift_max)
  Rt_shift_scale_start <- min(max(5, Rt_shift_scale_min), Rt_shift_scale_max)

  # Our random walk parameters start after the Meff change
  # Basically just set this suitably far back in the past
  date_Meff_change <- date_start - 1

  ## -----------------------------------------------------------------------------
  ## Step 2c: Spline set up
  ## -----------------------------------------------------------------------------

  last_shift_date <- as.Date(date_Meff_change) + 1
  remaining_days <- as.Date(date_0) - last_shift_date - 14 # reporting delay in place

  # how many spline pars do we need
  Rt_rw_duration <- rw_duration # i.e. we fit with a 2 week duration for our random walks.
  rw_needed <- as.numeric(ceiling(remaining_days/Rt_rw_duration))

  # set up rw pars
  pars_init_rw <- as.list(rep(0, rw_needed))
  pars_min_rw <- as.list(rep(-5, rw_needed))
  pars_max_rw <- as.list(rep(5, rw_needed))
  pars_discrete_rw <- as.list(rep(FALSE, rw_needed))
  names(pars_init_rw) <- names(pars_min_rw) <- names(pars_max_rw) <- names(pars_discrete_rw) <- paste0("Rt_rw_", seq_len(rw_needed))

  ## -----------------------------------------------------------------------------
  ## Step 2d: PMCMC parameter set up
  ## -----------------------------------------------------------------------------

  # PMCMC Parameters
  pars_init = list('start_date' = date_start,
                   'R0' = R0_start,
                   'Meff' = Meff_start,
                   'Meff_pl' = Meff_pl_start,
                   "Rt_shift" = 0,
                   "Rt_shift_scale" = Rt_shift_scale_start)
  pars_min = list('start_date' = first_start_date,
                  'R0' = R0_min,
                  'Meff' = Meff_min,
                  'Meff_pl' = Meff_pl_min,
                  "Rt_shift" = Rt_shift_min,
                  "Rt_shift_scale" = Rt_shift_scale_min)
  pars_max = list('start_date' = last_start_date,
                  'R0' = R0_max,
                  'Meff' = Meff_max,
                  'Meff_pl' = Meff_pl_max,
                  "Rt_shift" = Rt_shift_max,
                  "Rt_shift_scale" = Rt_shift_scale_max)
  pars_discrete = list('start_date' = TRUE, 'R0' = FALSE, 'Meff' = FALSE,
                       'Meff_pl' = FALSE, "Rt_shift" = FALSE, "Rt_shift_scale" = FALSE)
  pars_obs = list(phi_cases = 1, k_cases = 2, phi_death = 1, k_death = 2, exp_noise = 1e6)

  # add in the spline list
  pars_init <- append(pars_init, pars_init_rw)
  pars_min <- append(pars_min, pars_min_rw)
  pars_max <- append(pars_max, pars_max_rw)
  pars_discrete <- append(pars_discrete, pars_discrete_rw)

  # Covriance Matrix
  proposal_kernel <- diag(length(names(pars_init))) * 0.3
  rownames(proposal_kernel) <- colnames(proposal_kernel) <- names(pars_init)
  proposal_kernel["start_date", "start_date"] <- 1.5

  # MCMC Functions - Prior and Likelihood Calculation
  logprior <- function(pars){
    ret <- dunif(x = pars[["start_date"]], min = -55, max = -10, log = TRUE) +
      dnorm(x = pars[["R0"]], mean = 3, sd = 1, log = TRUE) +
      dnorm(x = pars[["Meff"]], mean = 0, sd = 1, log = TRUE) +
      dunif(x = pars[["Meff_pl"]], min = 0, max = 1, log = TRUE) +
      dnorm(x = pars[["Rt_shift"]], mean = 0, sd = 1, log = TRUE) +
      dunif(x = pars[["Rt_shift_scale"]], min = 0.1, max = 10, log = TRUE)

    # get rw spline parameters
    if(any(grepl("Rt_rw", names(pars)))) {
      Rt_rws <- pars[grepl("Rt_rw", names(pars))]
      for (i in seq_along(Rt_rws)) {
        ret <- ret + dnorm(x = Rt_rws[[i]], mean = 0, sd = 0.2, log = TRUE)
      }
    }
    return(ret)
  }

  ## -----------------------------------------------------------------------------
  ## Step 3: Run PMCMC
  ## -----------------------------------------------------------------------------

  # mixing matrix - assume is same as country as whole
  mix_mat <- squire::get_mixing_matrix(country)

  # run the pmcmc
  res <- squire::pmcmc(data = data,
                       n_mcmc = n_mcmc,
                       log_prior = logprior,
                       n_particles = 1,
                       steps_per_day = 1,
                       log_likelihood = NULL,
                       reporting_fraction = reporting_fraction,
                       squire_model = squire:::deterministic_model(),
                       output_proposals = FALSE,
                       n_chains = n_chains,
                       pars_init = pars_init,
                       pars_min = pars_min,
                       pars_max = pars_max,
                       pars_discrete = pars_discrete,
                       proposal_kernel = proposal_kernel,
                       population = population,
                       baseline_contact_matrix = mix_mat,
                       R0_change = R0_change,
                       date_R0_change = date_R0_change,
                       Rt_args = squire:::Rt_args_list(
                         date_Meff_change = date_Meff_change,
                         scale_Meff_pl = TRUE,
                         Rt_shift_duration = 1,
                         Rt_rw_duration = Rt_rw_duration),
                       burnin = ceiling(n_mcmc/10),
                       seeding_cases = 5,
                       replicates = replicates,
                       required_acceptance_ratio = 0.20,
                       start_adaptation = start_adaptation,
                       baseline_hosp_bed_capacity = hosp_beds,
                       baseline_ICU_bed_capacity = icu_beds)


  ## remove things so they don't atke up so much memory when you save them :)

  # Add the prior
  res$pmcmc_results$inputs$prior <- as.function(c(formals(logprior),
                                                  body(logprior)),
                                                envir = new.env(parent = environment(stats::acf)))

  # remove states to keep object memory save down
  for(i in seq_along(res$pmcmc_results$chains)) {
    res$pmcmc_results$chains[[i]]$states <- NULL
    res$pmcmc_results$chains[[i]]$covariance_matrix <- tail(res$pmcmc_results$chains$chain1$covariance_matrix,1)
  }

  return(res)

}

#' Extract PCR prevalence and seroprevalence from squire model fit
#'
#' @param res Output of \code{\link{squire::pmcmc}}
seroprev_df <- function(res) {

# seroconversion data from brazeay report 34
prob_conversion <-  cumsum(dgamma(0:300,shape = 5, rate = 1/2))/max(cumsum(dgamma(0:300,shape = 5, rate = 1/2)))*0.95
sero_det <- cumsum(dweibull(0:300, 3.669807, scale = 143.7046))
sero_det <- cumsum(prob_conversion-sero_det)
sero_det[sero_det < 0] <- 0
sero_det <- sero_det/max(sero_det)

# from Kay et al 2021 Science (actually from preprint)
pcr_det <- c(9.206156e-13, 9.206156e-13, 3.678794e-01, 9.645600e-01,
             9.575796e-01, 9.492607e-01, 9.393628e-01, 9.276090e-01,
             9.136834e-01, 8.972309e-01, 8.778578e-01, 8.551374e-01,
             8.286197e-01, 7.978491e-01, 7.623916e-01, 7.218741e-01,
             6.760375e-01, 6.248060e-01, 5.683688e-01, 5.072699e-01,
             4.525317e-01, 4.036538e-01, 3.600134e-01, 3.210533e-01,
             2.862752e-01, 2.552337e-01, 2.275302e-01, 2.028085e-01,
             1.807502e-01, 1.610705e-01, 1.435151e-01, 1.278563e-01,
             1.138910e-01, 1.014375e-01, 9.033344e-02)

# additional_functions for rolling
roll_func <- function(x, det) {
  l <- length(det)
  c(NA,
    zoo::rollapply(x,
                   list(seq(-l, -1)),
                   function(i) {
                     sum(i*tail(det, length(i)), na.rm = TRUE)
                   },
                   partial = 1
    ))
}

# get symptom onset data
date_0 <- max(res$pmcmc_results$inputs$data$date)
inf <- squire::format_output(res, c("S"), date_0 = max(res$pmcmc_results$inputs$data$date)) %>%
  mutate(S = as.integer(y)) %>%
  group_by(replicate) %>%
  mutate(infections = lag(S, 1)-S) %>%
  select(replicate, t, date, S, infections)

# correctly format
inf <- left_join(inf,
                 squire::format_output(res, c("infections"), date_0 = max(res$pmcmc_results$inputs$data$date)) %>%
                   mutate(symptoms = as.integer(y)) %>%
                   select(replicate, t, date, symptoms),
                 by = c("replicate", "t", "date"))

inf <- inf %>%
  group_by(replicate) %>%
  mutate(pcr_positive = roll_func(infections, pcr_det),
         sero_positive = roll_func(symptoms, sero_det),
         ps_ratio = pcr_positive/sero_positive,
         sero_perc = sero_positive/max(S,na.rm = TRUE),
         pcr_perc = pcr_positive/max(S,na.rm = TRUE)) %>%
  ungroup

inf$reporting_fraction <- res$pmcmc_results$inputs$pars_obs$phi_death
return(inf)

}
