library(dplyr)
library(ggplot2)

df <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv")

## 1. Plot all the mortality data by year

df <- df %>% mutate(date = as.Date(dod, "%m/%d/%y"))

deaths_by_date <- df %>% filter(dod != ".") %>%
  group_by(date) %>% summarise(deaths = length(date)) %>%
  mutate(year = format(date, "%Y"))


deaths_by_year <- deaths_by_date %>% group_by(year) %>%
  summarise(deaths = sum(deaths))

facet.labs <- paste(deaths_by_year$year,"\nn =",deaths_by_year$deaths)
names(facet.labs) <- deaths_by_year$year

ggplot(data = deaths_by_date, aes(x = date, y = deaths)) + geom_point() +
  facet_grid(. ~year, scale = "free",
             labeller = labeller(year = facet.labs)) +
  # scale_y_continuous(trans='log10') +
  scale_x_date(date_labels = "%b") +
  xlab("Date") + ylab("Deaths")

## - You can't see an increase in deaths in 2020
## group by month?
# deaths_by_month <- deaths_by_date %>% mutate(month_year = format(date, "%Y-%m")) %>% group_by(month_year) %>%
#   summarise(Deaths = sum(deaths)) %>%
#   mutate(month_label = as.Date(paste0(month_year,"-01")),
#          year = format(month_label, "%Y"))
#
# ggplot(data = deaths_by_month, aes(x = month_label, y = Deaths)) + geom_point() +
#   facet_grid(. ~year, scale = "free",
#              labeller = labeller(year = facet.labs)) +
#   # scale_y_continuous(limits = c(1,1000),trans='log10') +
#   # ylim(c(1,400)) +
#   scale_x_date(date_labels = "%b") +
#   ggtitle("Monthly deaths") +
#   xlab("Date") + ylab("Deaths")



## 2. Take a closer look at the 2020 and 2021 data
deaths_by_date_2020_2021 <- deaths_by_date %>% filter(date >= "2020-01-01")
ggplot(data = deaths_by_date_2020_2021, aes(x = date, y = deaths)) + geom_point() +
  scale_x_date(date_labels = "%b %Y")

## - You can see that there are some peaks that could correspond to peaks in the data.

## 3. Plot this data with the UTH COVID sampling and Official data

BMJ_PostMort_Deaths <- read.csv(file = "analysis/data/raw/BMJ_Mwananyanda_PostMortem_UTH/covid_results.csv")
BMJ_PostMort_Deaths_by_Date <- BMJ_PostMort_Deaths %>% filter(deceased_date != ".") %>% group_by(deceased_date) %>% summarise(deaths = length(deceased_date))
Official_Data <- read.csv(file = "analysis/data/raw/00_official_reports_covid_zambia.csv") %>% filter(Province == "Lusaka")
ggplot(data = deaths_by_date_2020_2021, aes(x = date, y = deaths)) + geom_point() +
  # scale_y_continuous(trans='log10') +
  scale_x_date(date_labels = "%b %Y") +
  geom_point(data = df_tot_sampled_sp_gr, aes(x = as.Date(date), y = deaths), col = "blue") +
  geom_point(data = Official_Data, aes(x = as.Date(Date), y = Total_Deaths), col = "purple") +
  geom_point(data = df_tot_sampled_sp_gr, aes(x = as.Date(date), y = CT_45_Either), col = "red")






############# Let's work out the Covid proportions to scale
deaths_by_date_2020 <- deaths_by_date_2020_2021 %>% filter(date >= min(df_tot_sampled_sp_gr$date) & date <= max(df_tot_sampled_sp_gr$date))
SampledDeaths <- df_tot_sampled_sp_gr %>% rename(total_sampled = deaths)

Full_Data <- merge(deaths_by_date_2020, SampledDeaths, by = "date", all.y = T)

Full_Data <- Full_Data %>% mutate(Est_COVID_deaths = deaths * CT_45_Either/total_sampled)

plot(x = Full_Data$date, y = Full_Data$Est_COVID_deaths, pch = 20)

## Deaths by month in 2020:
deaths_by_month_2020 <- deaths_by_date %>% filter(date >= "2020-01-01", date < "2021-01-01") %>% mutate(month = format(date, "%m")) %>%
  group_by(month) %>% summarise(deaths = sum(deaths)) %>%
  mutate(Hamulake_BID = c(590,588,556,564,583,696,810,794,648,636,610,681))

# Compared with the Hamulake paper:
ggplot(data = deaths_by_month_2020, aes(x = month, y = deaths)) + geom_point() +
  geom_point(aes(y = Hamulake_BID), col = 2) +
  ylim(c(0,1500))
