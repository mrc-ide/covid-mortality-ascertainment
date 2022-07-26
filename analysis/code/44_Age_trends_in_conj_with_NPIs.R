### Did the underlying age death patterns change when the non-pharmaceutical interventions were put in place?
library(dplyr)
library(ggplot2)

UTH_Mortality_Total <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records_v2.csv")
UTH_Mortality_Total <- UTH_Mortality_Total %>%
  filter(age_years !=".",
         dod !=".") %>%
  mutate(date = as.Date(dod, "%m/%d/%y"),
         Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  select(-sex, -dod, -age_years)

UTH_Mortality_Total_Months_Age <- UTH_Mortality_Total %>%
  filter(date >=as.Date("2017-06-01")) %>%
  mutate(months =  lubridate::floor_date(x = date, unit = "month")+14) %>%
  group_by(months, Age_gr) %>%
  summarise(monthly_deaths = length(date)) %>%
  group_by(months) %>%
  mutate(Tot_monthly_deaths = sum(monthly_deaths),
         Age_Prop_monthly_deaths = monthly_deaths/Tot_monthly_deaths)

ggplot(data = UTH_Mortality_Total_Months_Age, aes(x = months, y = monthly_deaths, group = Age_gr, col = as.factor(Age_gr))) +
  scale_color_discrete() +
  geom_line() +
  geom_vline(xintercept = as.Date(c("2020-03-17",
                                    "2020-03-20",
                                    "2020-03-26"))) +
  geom_vline(xintercept = as.Date(c("2020-04-24",
                                    "2020-05-08",
                                    "2020-06-01",
                                    "2020-06-08",
                                    "2020-06-25",
                                    "2020-09-11",
                                    "2020-10-05")), col = "darkred")


p1 <- ggplot(data = UTH_Mortality_Total_Months_Age, aes(x = months, y = Age_Prop_monthly_deaths, group = Age_gr, col = as.factor(Age_gr))) +
  # scale_color_discrete(name = "Age group") +
  annotate(geom = "rect", xmin = as.Date("2020-03-17"), xmax = as.Date("2020-03-20"), ymin = -Inf, ymax = Inf, fill = "darkblue", alpha = 0.2) +
  annotate(geom = "rect", xmin = as.Date("2020-03-20"), xmax = as.Date("2020-03-26"), ymin = -Inf, ymax = Inf, fill = "darkblue", alpha = 0.5) +
  annotate(geom = "rect", xmin = as.Date("2020-03-26"), xmax = as.Date("2020-04-24"), ymin = -Inf, ymax = Inf, fill = "darkblue", alpha = 1) +
  annotate(geom = "rect", xmin = as.Date("2020-04-24"), xmax = as.Date("2020-05-08"), ymin = -Inf, ymax = Inf, fill = "darkblue", alpha = 0.7) +
  annotate(geom = "rect", xmin = as.Date("2020-05-08"), xmax = as.Date("2020-06-01"), ymin = -Inf, ymax = Inf, fill = "darkblue", alpha = 0.6) +
  annotate(geom = "rect", xmin = as.Date("2020-06-01"), xmax = as.Date("2020-06-08"), ymin = -Inf, ymax = Inf, fill = "darkblue", alpha = 0.5) +
  annotate(geom = "rect", xmin = as.Date("2020-06-08"), xmax = as.Date("2020-06-25"), ymin = -Inf, ymax = Inf, fill = "darkblue", alpha = 0.4) +
  annotate(geom = "rect", xmin = as.Date("2020-06-25"), xmax = as.Date("2020-09-11"), ymin = -Inf, ymax = Inf, fill = "darkblue", alpha = 0.3) +
  annotate(geom = "rect", xmin = as.Date("2020-09-11"), xmax = as.Date("2020-10-05"), ymin = -Inf, ymax = Inf, fill = "darkblue", alpha = 0.2) +
  geom_smooth() +
  geom_line() +
  # geom_vline(xintercept = as.Date(c("2020-03-17",
                                    # "2020-03-20",
                                    # "2020-03-26")), col = "darkblue", size = 0.2) +

  # geom_vline(xintercept = as.Date(c("2020-04-24",
                                    # "2020-05-08",
                                    # "2020-06-01",
                                    # "2020-06-08",
                                    # "2020-06-25",
                                    # "2020-09-11",
                                    # "2020-10-05")), col = "darkred") +
  # geom_vline(xintercept = as.Date(c("2020-06-15")), size = 2) +
  coord_cartesian(ylim = c(0,0.25)) +
  xlab("Date") + ylab("Monthly proportion of deaths") +
  viridis::scale_color_viridis(name = "Age Group", discrete = T)

ggplot(data = UTH_Mortality_Total_Months_Age %>% filter(months<as.Date("2020-06-15")), aes(x = months, y = Age_Prop_monthly_deaths, group = Age_gr, col = as.factor(Age_gr))) +
  scale_color_discrete() +
  geom_smooth() +
  geom_line() +
  # geom_vline(xintercept = as.Date(c("2020-03-17",
                                    # "2020-03-20",
                                    # "2020-03-26"))) +
  geom_vline(xintercept = as.Date(c("2020-03-15")), size = 1) +
  coord_cartesian(ylim = c(0,0.25))

## Prior to the panedemic, what is the monthly proportion of deaths?
Comp_df <- UTH_Mortality_Total_Months_Age %>%
  filter(months < as.Date("2020-06-01")) %>%
  mutate(Pre_NPIs = ifelse(months <as.Date("2020-03-01"), T, F)) %>%
  group_by(Age_gr, Pre_NPIs) %>%
    summarise(median_prop = median(Age_Prop_monthly_deaths),
              mean_prop = mean(Age_Prop_monthly_deaths),
              Low_95CI_mean = confintr::ci_mean(Age_Prop_monthly_deaths)$interval[1],
              High_95CI_mean = confintr::ci_mean(Age_Prop_monthly_deaths)$interval[2],
              min_prop = min(Age_Prop_monthly_deaths),
              max_prop = max(Age_Prop_monthly_deaths))
              # Low_95CI_med = confintr::ci_median(Age_Prop_monthly_deaths)$interval[1],
              # High_95CI_med = confintr::ci_median(Age_Prop_monthly_deaths)$interval[2])

ggplot(Comp_df, aes(x = Pre_NPIs, group = Age_gr, col = Age_gr)) +
  geom_line(aes(y = mean_prop))

ggplot(UTH_Mortality_Total_Months_Age %>% filter(months < as.Date("2020-06-01")),
       aes(group = Age_gr, y = Age_Prop_monthly_deaths)) +
  geom_boxplot()


ggplot(Comp_df %>% filter(Pre_NPIs==T), aes(x = Age_gr)) +
  # geom_point(aes(y = mean_prop)) +
  geom_boxplot(aes(y = mean_prop, group = Age_gr))+
  geom_errorbar(aes(ymin = Low_95CI_mean, ymax = High_95CI_mean)) +
  geom_point(data = Comp_df %>% filter(Pre_NPIs==F), aes(y = mean_prop), col = 2)

  # geom_errorbar(aes(ymin = min_prop, ymax = max_prop))


### Proportion of age deaths within each age bracket
UTH_Mortality_Total <- UTH_Mortality_Total %>% mutate(Time_period = cut.Date(x = as.Date(date),
                                                      breaks = as.Date(c("2017-05-03",
                                                                         "2020-03-17",
                                                                         "2020-03-20",
                                                                         "2020-03-26",
                                                                         "2020-04-24",
                                                                         "2020-05-08",
                                                                         "2020-06-01",
                                                                         "2020-06-08",
                                                                         "2020-06-25",
                                                                         "2020-09-11",
                                                                         "2020-10-05",
                                                                         "2022-10-05")), labels = F))

UTH_Mortality_Total_grouped <- UTH_Mortality_Total %>% group_by(Time_period) %>%
  summarise(Total_deaths_all_ages = length(Time_period)) %>%
  mutate(Time_group = case_when(Time_period == 1 ~ "1. Pre-pandemic",
                                Time_period %in% c(2:4) ~ "2. Restrictions",
                                Time_period %in% c(5:10) ~ "3. Lifting restrictions",
                                Time_period == 11 ~ "4. No restrictions"))

NPI_dates <- cbind(Time_period = 1:11,
                   NPI_start_date = c("2017-05-03",
                                          "2020-03-17",
                                          "2020-03-20",
                                          "2020-03-26",
                                          "2020-04-24",
                                          "2020-05-08",
                                          "2020-06-01",
                                          "2020-06-08",
                                          "2020-06-25",
                                          "2020-09-11",
                                          "2020-10-05"))

p2 <- UTH_Mortality_Total %>% group_by(Time_period, Age_gr) %>%
  summarise(Total_deaths = length(Time_period)) %>%
  merge(UTH_Mortality_Total_grouped) %>%
  merge(NPI_dates) %>%
  mutate(Prop_deaths = Total_deaths/Total_deaths_all_ages) %>%
  ggplot(aes(x = NPI_start_date, y = Prop_deaths, group = as.factor(Age_gr), fill = as.factor(Age_gr))) +
  geom_bar(stat = "identity", position = "stack") +
  viridis::scale_fill_viridis(discrete = T) +
  guides(fill=guide_legend(title="")) +
  facet_grid(~Time_group, scales = "free_x", space = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -3)) +
  ylab("Proportion") +
  xlab("Key NPI dates")

pdf("analysis/figures/44_Effect_of_NPIs_on_age_structured_deaths", height = 7, width = 18)
cowplot::plot_grid(p1+ theme_minimal() +theme(legend.position = "none"),p2)
dev.off()


BurRegs <- readRDS("analysis/data/Code-generated-data/00_07_Burial_registrations_by_week_2017_to_2021.rds") %>%
  rename(Week = Week_st,
         BurRegs = Total_deaths) %>%
  filter(Week >= as.Date("2019-01-01"),
         Week < as.Date("2021-06-15"))

Off_data <- readRDS("analysis/data/Code-generated-data/00_01_Lusaka_Dist_Deaths_Official.rds") %>%
  mutate(Week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(Week) %>% summarise(Off_deaths = sum(deaths))

coeff <- 500
p3 <- UTH_Mortality_Total %>%
  mutate(Week = lubridate::floor_date(date, unit = "week", week_start = 1),
         Age_gr = cut(Age_gr, c(seq(0,18,by = 2)), right = T, labels = F)) %>%
  group_by(Week, Age_gr) %>%
  summarise(Weekly_Age_deaths = n()) %>%
  group_by(Week) %>%
  mutate(Weekly_Total_deaths = sum(Weekly_Age_deaths),
         Weekly_Prop_deaths = Weekly_Age_deaths/Weekly_Total_deaths) %>%
  # merge(BurRegs, all = T) %>% merge(Off_data, all = T) %>%
  filter(Week >= as.Date("2019-01-01"),
         Week < as.Date("2021-06-15")) %>%
  ggplot(aes(x = Week, y = Weekly_Prop_deaths, group = as.factor(Age_gr), fill = as.factor(Age_gr))) +
  geom_bar(stat = "identity", position = "stack", width = 10) +
  viridis::scale_fill_viridis(discrete = T, option = "H") +
  guides(fill=guide_legend(title="")) +
  # facet_grid(~stringr::str_wrap(Time_group, width = 10), space = "free_x", scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -3)) +
  ylab("Proportion") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0,0)) +
  annotate(geom = "text", x = as.Date(c("2020-03-17","2020-05-17",
                                        "2020-07-15","2020-10-05",
                                        "2021-01-01"))+2,
           y = 0.99, label = stringr::str_wrap(c("NPIs implemented","Initial restrictions lifted",
                              "First wave","Restrictions fully lifted","Second wave"),
                              width = 20),
           color = "white", angle = 90, size = 3, hjust = 1, vjust = 1) +
  geom_vline(xintercept = as.Date(c("2020-03-17","2020-05-17",
                            "2020-07-15","2020-10-05",
                            "2021-01-01")), color = "white", linetype = "dashed") +
    geom_line(data = BurRegs, aes(y = BurRegs/coeff, x = Week), size = 1.2, inherit.aes = F) +
  geom_line(data = Off_data, aes(y = Off_deaths/coeff, x = Week), size = 1.2, inherit.aes = F) +
  scale_y_continuous(name = "Proportion",

                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~.*coeff, name="Burial Registraions"))

  # geom_segment(aes(x = as.Date(c("2020-03-17","2020-05-17",
                                 # "2020-08-01","2020-10-05",
                                 # "2021-02-01")), y = 0, yend = 1))


### Get data for weekly burial registrations:


# mutate(Time_period = cut.Date(x = as.Date(Week),
#          breaks = as.Date(c("2017-05-03",
#                             "2020-03-17",
#                             # "2020-03-20",
#                             # "2020-03-26",
#                             "2020-04-24",
#                             # "2020-05-08",
#                             "2020-06-01",
#                             # "2020-06-08",
#                             # "2020-06-25",
#                             "2020-07-14",
#                             "2020-08-08",
#                             # "2020-09-11",
#                             "2020-10-05",
#                             "2021-01-01",
#                             "2021-02-15",
#                             "2022-10-05")), labels = F)) %>%
# mutate(Time_group = case_when(Time_period == 1 ~ "1. Pre-pandemic",
#                               Time_period %in% c(2) ~ "2. Restrictions",
#                               Time_period %in% c(3:4) ~ "3. Lifting restrictions",
#                               # Time_period %in% c(4) ~ "Lifting more restrictions",
#                               Time_period %in% c(5) ~ "4. First wave",
#                               Time_period %in% c(6) ~ "5. Post first wave",
#                               Time_period %in% c(7) ~ "6. No restrictions",
#                               Time_period %in% c(8) ~ "7. Second wave",
#                               Time_period %in% c(9) ~ "8. Post second wave")) %>%


# geom_vline(xintercept = as.Date(c("2020-03-17",
  # "2020-03-20",
  # "2020-03-26")), col = "darkblue", size = 0.2) +
  #
  # geom_vline(xintercept = as.Date(c("2020-04-24",
  # "2020-05-08",
  # "2020-06-01",
  # "2020-06-08",
  # "2020-06-25",
  # "2020-09-11",
  # "2020-10-05")), col = "darkred")



## Time period:
# 1 Prior to the pandemic

# 2 Restrictions on travel, mass gatherings
# 3 Educational institutions shut down
# 4 3 airports closed, restaurant only do take-aways

# 5 Resumed congregational worship, non-contact sports, barbershops with some regulations
# 6 restaurants, cinemas, gyms, casinos
# 7 primary and secondary schools reopen for examination students
# 8 phased opening of colleges/universities for final year students
# 9 International airports open
# 10 Fully reopen schools/universities, partial reopening of pubs, taverns, nightclubs
# 11 Fully reopen bars, clubs, casinos, churches.

UTH_Mortality_Total_prop_deaths <- UTH_Mortality_Total %>% group_by(Time_period, Age_gr) %>%
  summarise(Total_deaths = length(Time_period)) %>%
  merge(UTH_Mortality_Total_grouped) %>%
  merge(NPI_dates) %>%
  mutate(Prop_deaths = Total_deaths/Total_deaths_all_ages)

UTH_Mortality_Total_prop_deaths_Pre_pandemic <- UTH_Mortality_Total_prop_deaths %>% filter(Time_period ==1) %>%
  select(Age_gr, Prop_deaths) %>%
  rename(PP_prop_deaths = Prop_deaths)

unique(UTH_Mortality_Total_prop_deaths$NPI_start_date)


labels_time <- paste0(c(rep("Inc. NPIs: ",3),rep("Dec. NPIs: ",6),"No NPIs: "),
                      c(unique(UTH_Mortality_Total_prop_deaths$NPI_start_date)[-1]),
                      " - ",
                      c(as.character(as.Date(unique(UTH_Mortality_Total_prop_deaths$NPI_start_date)[-1:-2])-1),"end of data"))
names(labels_time)<-2:11

p1 <- UTH_Mortality_Total_prop_deaths %>%
  filter(Time_period != 1) %>%
  merge(UTH_Mortality_Total_prop_deaths_Pre_pandemic) %>%
  group_by(Age_gr, Time_period) %>%
  mutate(Rel_prop_deaths = Prop_deaths - PP_prop_deaths) %>%
  ggplot(aes(x = Age_gr*5-2.5, y = Rel_prop_deaths, group = Age_gr, fill = as.factor(Age_gr))) +
  geom_bar(stat="identity", position = "dodge") +
  facet_wrap(~Time_period, scales = "free_x", labeller=labeller(Time_period = labels_time), nrow = 2) +
  viridis::scale_fill_viridis(discrete = T) +
  theme_minimal() +
  theme(legend.position = "none") + xlab("Age") + ylab("Change in proportion")

pdf()
