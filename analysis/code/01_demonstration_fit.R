#############################
### 01. Demonstration Fit ###
#############################

# OJ's example code for how to use fit_spline_rt, seroprev_df and plotting results
# install.packages("here")
# install.packages("devtools")
library(devtools)
library(tidyverse)
devtools::load_all(".")
devtools::install_dev_deps()


# Example of how to run say one model fit for a made up country:
owid <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# This is the death time series for Zambia
data <- owid %>% filter(iso_code == "ZMB") %>%
  select(date, new_deaths) %>%
  na.omit() %>%
  rename(deaths = new_deaths)


# Let's use this to make a dummy data set for say a 10th of the country
# this would in practice be the data that has been collected for your setting
data$deaths <- vapply(data$deaths, function(x) {rbinom(1, size = x, prob = 1/10)}, numeric(1))
data <- filter(data, date < as.Date("2020-11-01"))

# we also need to know the population size
# here we will just make a dummy pop for 1/10 of the pop
pop <- as.integer(squire::get_population("Zambia")$n/10)

sum(pop)

# conduct epidemic fit for this country using squire assuming 100% reporting
# N.B. would be good to run for at least 20K iterations. Ideally. 100K to ensure good fit.
# Request 3 cores if running on a cluster to speed up life by having a core for each chain
fit <- fit_spline_rt(data = data,
                     country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                     population = pop,
                     reporting_fraction = 1,
                     n_mcmc = 10000,
                     replicates = 100,
                     rw_duration = 14)
plot(fit$pmcmc_results$chains$chain1$results$R0)

# to check the fit
plot(fit, particle_fit = TRUE)
fit$replicate_parameters
# this is a squire object so we can get data as normal:
deaths <- squire::format_output(fit, "deaths")


# Notes:

# This is a fit assuming that 100% deaths have been reported.
# However, we don't know if this is true so we need some other data
# source to check against, e.g. seroprevalence and PCR prevalence
sero_pcr_df <- seroprev_df(fit)
ggplot(sero_pcr_df, aes(date, pcr_perc)) + geom_line() + ylab("PCR Prevalence") + scale_y_continuous(labels = scales::percent)
ggplot(sero_pcr_df, aes(date, sero_perc)) + geom_line() + ylab("Seroprevalence") + scale_y_continuous(labels = scales::percent)

# So we could use these to compare against the observed sero/pcr data to see
# which reporting fraction most closely captures pcr and sero data.
