# model non-covid deaths by age as a random process using data from 2018,2019
# add in the covid deaths implied by the model
# total_deaths~NB(age_spec_rate+calendar_month_random_effect+covid deaths,dispersion)
# that's also the framework we'll need for the Moz data..


# total_deaths~NB(age_spec_rate+calendar_month_random_effect+covid deaths,dispersion)

# Model total deaths as a negative binomially distributed:
# Age_spec_rate + random_effect_of_month + covid_deaths, dispersion

dnbinom(x = , size = 2, mu = )


data_list <- list(x = UTH_deaths_by_week_age %>% filter(Age_group == 1, month == "01", year != "2020") %>%
  ungroup() %>% select(total_deaths))

# Estimating age-specific death rate and dispersion.

library(drjacoby)
# data
# parameters
# likelihood
# prior

# set.seed(1)
# mu_true <- 3
# sigma_true <- 2


# data_list <- list(x = rnorm(10, mean = mu_true, sd = sigma_true))
data_list <- list(x1 = UTH_deaths_by_week_age %>% filter(month == "01", year != "2020") %>%
                    mutate(total_deaths = as.numeric(total_deaths)) %>% ungroup() %>% group_by(Age_group) %>% select(total_deaths) %>% pull())


# df_params <- define_params(name = "mu", min = -10, max = 10,
#                            name = "sigma", min = 0, max = Inf)
df_params <- define_params(name = "disp", min = 0, max = Inf,
                           name = "mu1", min = 0, max = 1000,
                           name = "mu2", min = 0, max = 1000,
                           name = "mu3", min = 0, max = 1000,
                           name = "mu4", min = 0, max = 1000,
                           name = "mu5", min = 0, max = 1000,
                           name = "mu6", min = 0, max = 1000,
                           name = "mu7", min = 0, max = 1000,
                           name = "mu8", min = 0, max = 1000,
                           name = "mu9", min = 0, max = 1000,
                           name = "mu10", min = 0, max = 1000,
                           name = "mu11", min = 0, max = 1000,
                           name = "mu12", min = 0, max = 1000,
                           name = "mu13", min = 0, max = 1000,
                           name = "mu14", min = 0, max = 1000,
                           name = "mu15", min = 0, max = 1000,
                           name = "mu16", min = 0, max = 1000,
                           name = "mu17", min = 0, max = 1000)

# define log-likelihood function
r_loglike <- function(params, data, misc) {

  # extract parameter values
  mu1 <- params["mu1"]
  mu2 <- params["mu2"]
  mu3 <- params["mu3"]
  mu4 <- params["mu4"]
  mu5 <- params["mu5"]
  mu6 <- params["mu6"]
  mu7 <- params["mu7"]
  mu8 <- params["mu8"]
  mu9 <- params["mu9"]
  mu10 <- params["mu10"]
  mu11 <- params["mu11"]
  mu12 <- params["mu12"]
  mu13 <- params["mu13"]
  mu14 <- params["mu14"]
  mu15 <- params["mu15"]
  mu16 <- params["mu16"]
  mu17 <- params["mu17"]
  disp <- params["disp"]

  # calculate log-probability of data
  ret <- sum(dnbinom(data$x, size = disp, mu = mu, log = TRUE))

  # return
  return(ret)
}

r_logprior <- function(params, misc) {

  # extract parameter values
  mu <- params["mu"]
  disp <- params["disp"]

  # calculate log-prior
  ret <- dunif(mu, min = 0, max = 1000, log = TRUE) +
    dlnorm(disp, meanlog = 0, sdlog = 1.0, log = TRUE)

  # return
  return(ret)
}

mcmc <- run_mcmc(data = data_list,
                 df_params = df_params,
                 loglike = r_loglike,
                 logprior = r_logprior,
                 burnin = 1e3,
                 samples = 1e3,
                 pb_markdown = TRUE)

head(mcmc$output)



plot_par(mcmc, show = "mu", phase = "burnin")
plot_par(mcmc, show = "disp", phase = "burnin")

mcmc$diagnostics$rhat

plot_par(mcmc, show = "mu", phase = "sampling")
plot_par(mcmc, show = "disp", phase = "sampling")




## Fit the normal death model to the 2018-2019 data
UTH_Mortality_Total <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv")
UTH_Mortality_Total <- UTH_Mortality_Total %>% mutate(date = as.Date(dod, "%m/%d/%y"))

UTH_deaths_by_age <- UTH_Mortality_Total %>% filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2021-01-01", age_years !=".") %>%
  mutate(Age_group = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  group_by(date,Age_group) %>% summarise(total_deaths = length(date))# %>%
# ungroup %>%
# tidyr::complete(date, Age_group, fill = list(total_deaths = 0))
# filter(date >= "2020-06-15" & date <= "2020-11-01")

# UTH_deaths_by_date <- UTH_Mortality_Total %>% filter(dod != ".") %>%
#   group_by(date) %>% summarise(total_deaths = length(date)) #%>%
# filter(date >= "2020-06-15" & date <= "2020-11-01")
# filter(date >= "2020-06-15" & date <= "2020-10-05")


date_list_Mort <- seq(min(UTH_deaths_by_age$date), max(UTH_deaths_by_age$date), by = 1)
missing_dates_Mort <- date_list_Mort[!date_list_Mort %in% UTH_deaths_by_age$date] # Add missing dates with 0 deaths
#
UTH_deaths_by_age <- add_row(UTH_deaths_by_age, date = missing_dates_Mort, total_deaths = 0) %>% arrange(date)

UTH_deaths_by_week_age <- UTH_deaths_by_age %>%#  filter(Age_group ==1) %>%
  mutate(date = as.Date(cut.Date(date, breaks = "1 week"))) %>%
  mutate(year = format(date,"%Y"),
         month = format(date,"%m")) %>%
  group_by(Age_group, month, year) %>%
  summarise(total_deaths = sum(total_deaths))

p1 <- ggplot(UTH_deaths_by_week_age, aes(x = month, group = year, col = year)) +
  ggtitle("Total monthly deaths by year (log10 axis)") +
  geom_line(aes(y = total_deaths)) +
  facet_wrap(vars(Age_group), ncol = 1) +#, scales = "free") +
  geom_vline(xintercept = "06", linetype = 2, colour = "darkblue") +
  geom_vline(xintercept = "10", linetype = 2, colour = "darkblue") +
  scale_y_continuous(trans='log10') +
  ylab("Deaths") + xlab("Month") +
  theme(legend.position="none")




UTH_deaths_by_week_age_Weeks <- UTH_deaths_by_age %>%
  mutate(year = format(date,"%Y"),
         month = format(date,"%m"),
         day = format(date,"%d"),
         date_comp = as.Date(paste0(2020,"-",month,"-",day))) %>%
  mutate(week_comp = as.Date(cut.Date(date_comp, breaks = "1 week"))) %>%
  group_by(Age_group, year, week_comp) %>%
  summarise(total_deaths = sum(total_deaths))
