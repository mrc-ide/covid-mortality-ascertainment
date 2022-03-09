UTH_Mortality_Total <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv")
UTH_Mortality_Total <- UTH_Mortality_Total %>% mutate(date = as.Date(dod, "%m/%d/%y"))

UTH_Mortality_Total %>% #filter(age_years !=".") %>%
  filter(date >= "2020-06-15" & date < "2020-10-05") %>%
  mutate(Age_group = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  group_by(Age_group) %>% summarise(total_deaths = length(date))


All_years <- UTH_Mortality_Total %>% filter(age_years !=".") %>%
  mutate(Age_group = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  group_by(Age_group) %>% summarise(total_deaths = length(date)) %>%
  mutate(total_deaths/sum(total_deaths))

Last_year <- UTH_Mortality_Total %>% #filter(age_years !=".") %>%
  filter(date >= "2020-03-01") %>%
  mutate(Age_group = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  group_by(Age_group) %>% summarise(total_deaths = length(date)) %>%
  mutate(total_deaths/sum(total_deaths))

Study_period <- UTH_Mortality_Total %>%
  filter(date >= "2020-06-15" & date < "2020-10-05", age_years !=".") %>%
  mutate(Age_group = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  group_by(Age_group) %>% summarise(total_deaths = length(date)) %>%
  mutate(total_deaths/sum(total_deaths))



Imputed <- UTH_Mortality_Total %>% filter(date >= "2020-06-15" & date < "2020-10-05") %>%
  # mutate(Age_group = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  mutate(Age_group = ifelse(age_years == ".",sample(x = 1:17, size = sum(age_years=="."), replace = T, prob = Study_period$`total_deaths/sum(total_deaths)`),cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)))

table(Imputed$Age_group, useNA = "always")

