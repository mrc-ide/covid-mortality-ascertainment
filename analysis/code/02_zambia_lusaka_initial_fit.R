devtools::load_all(".")
library(tidyverse)
library(gsheet)



## Bring in Zambia data and filter Lusaka
df <- as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1qKPsUZivONw8n_ZDh9mle46n3DHcdk-pa89Ag4ihJeM/edit#gid=0'))

# Filter deaths for Lusaka Province until November 2020
data <- df %>%
  filter(Province=="Lusaka" & Date < "2020-11-01") %>%
  select(Date, Total_Deaths) %>%
  na.omit() %>%
  group_by(Date) %>% summarise(Total_Deaths = sum(Total_Deaths)) %>%
  rename(deaths = Total_Deaths, date = Date)

## Population size for Lusaka Province
# 2010: 2191225, 2222812 or 2238569 (https://en.wikipedia.org/wiki/Lusaka_Province)
# 2014: 2669249 (https://zambia.opendataforafrica.org/apps/atlas/Lusaka)
# 2018: 3186336 (https://en.wikipedia.org/wiki/Provinces_of_Zambia)
# 2019: 3308438 (https://www.citypopulation.de/en/zambia/admin/05__lusaka/)
# 2020: 3.36e6 (https://knoema.com/atlas/Zambia/ranks/Population)
# 2020: 3360183 (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
# 2021: 3484394 (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
## Other Pop sizes
# Lusaka District sizes
# 2000: 1084703
# 2010: 1747152 (https://en.wikipedia.org/wiki/Lusaka_Province)
# 2018: 2.5e6  (https://en.wikipedia.org/wiki/Lusaka)
# 2019: 2627716 (https://en.wikipedia.org/wiki/Lusaka_Province)
# 2019: 3.3e6 (https://en.wikipedia.org/wiki/Lusaka)
# 2019: 3308400 (https://en.wikipedia.org/wiki/Lusaka, https://www.citypopulation.de/en/zambia/cities/)
# 2020: 2731696 (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
# 2021: 2905993 (https://worldpopulationreview.com/world-cities/lusaka-population)

# Select 2020 estimate from opendataforafrica
pop_tot <- 3360183

# Adjust for population structure
# pop_gpza <- as.integer(get_population("Zambia")$n * pop_tot/sum(get_population("Zambia")$n)) # Province of Lusaka

# 2010 Age distribution Lusaka Province (https://www.citypopulation.de/en/zambia/admin/05__lusaka/)
# 2010 Age distribution Lusaka District (https://www.citypopulation.de/en/zambia/wards/admin/0504__lusaka/)

# 2020 Age distribution for Lusaka Province from opendataforafrica (https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09)
pop_st_lu <- c(549475,448008,379841,339600,317148,305521,271318,232188,172365,125531,75157,51883,35587,22219,14960,8570,10812)

# Lusaka District Pop struc 2020 https://zambia.opendataforafrica.org/thrqjfb/population-and-demographic-projections-2011-2035?regionId=ZM-09
# c(439632,356146,302389,273911,265588,260473,230783,195211,142218,100552,58537,40513,26654,15878,10201,5695,7314)

# Compare opendataforafrica lusaka province with default
# plot(y = pop_gpza-pop_zs,
#      x = get_population("Zambia")$age_group,
#      ylab = "GetPop(Zam: adj for PopSizEst) - ZamStats")
# abline(h = 0)

# Bulawayo contact matrix:
Bula_Con_Mat<- read.csv("analysis/data/Bulawayo-Contact-Matrix.csv", row.names = 1, header = T, nrows = 16)
# max(signif(Bula_Con_Mat - get_mixing_matrix(country = "Zambia"), 1)) # Compare the Bulawayo contact matrix with default
## There isn't really much difference, as far as I can tell.
# Use the bulawayo anyway

fit <- fit_spline_rt(data = data,
                     country = "Zambia", # here you still need to say what country the data is from so the right contact matrix is loaded
                     population = pop_st_lu,
                     reporting_fraction = 1,
                     n_mcmc = 100,
                     replicates = 100,
                     rw_duration = 14,
                     hosp_beds = 1e10,
                     icu_beds = 1e10)

## Still need to add new contact matrix - Bulawayo

fit$replicate_parameters

plot(fit, date_0 = max(data$date), x_var = "date")
plot(fit, particle_fit = TRUE)

# Variation in replicates doesn't look like it's working yet.

sero_pcr_df <- seroprev_df(fit)
# PCR doesn't look like it's working.
ggplot(sero_pcr_df, aes(date, pcr_perc)) + geom_line() + ylab("PCR Prevalence") + scale_y_continuous(labels = scales::percent)
ggplot(sero_pcr_df, aes(date, sero_perc)) + geom_line() + ylab("Seroprevalence") + scale_y_continuous(labels = scales::percent)

# Seroprev is supposed to be 2% in July.
# PCR is supposed to be 8%.
