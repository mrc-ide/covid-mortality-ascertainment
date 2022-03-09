## Plots for negative and total deaths by age group
rm(list = ls())
library(dplyr)
library(ggplot2)
devtools::load_all()

Combined_Mort_PMT <- readRDS("analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete.rds")

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

  # filter(month> 05, month <12) %>%

p2 <- ggplot(UTH_deaths_by_week_age_Weeks, aes(x = week_comp, group = year, col = year)) +
  ggtitle("Total weekly deaths by year") +
  geom_line(aes(y = total_deaths)) +
  facet_wrap(vars(Age_group), ncol = 1, scales = "free") +
  geom_vline(xintercept = as.Date("2020-06-15"), linetype = 2, colour = "darkblue") +
  geom_vline(xintercept = as.Date("2020-10-02"), linetype = 2, colour = "darkblue") +
  ylab("Deaths") + xlab("Date") +
  theme(legend.position="none")

  # scale_y_continuous(trans='log10')


p3 <- ggplot(UTH_deaths_by_week_age_Weeks %>% filter(week_comp>="2020-06-01",week_comp<"2020-11-01"), aes(x = week_comp, group = year, col = year)) +
  ggtitle("Total weekly deaths by year - study period") +
  geom_line(aes(y = total_deaths)) +
  facet_wrap(vars(Age_group), ncol = 1, scales = "free") +
  geom_vline(xintercept = as.Date("2020-06-15"), linetype = 2, colour = "darkblue") +
  geom_vline(xintercept = as.Date("2020-10-02"), linetype = 2, colour = "darkblue") +
  ylab("Deaths") + xlab("Date")



p4 <- ggplot(Combined_Mort_PMT, aes(x = date)) +
  ggtitle("Total deaths (2020), est -ve prev") +
  geom_line(aes(y = total_deaths)) +
  # geom_line(aes(y = total_deaths - (total_deaths*(Samples-PosTestNum)/Samples)), col = 2) +
  geom_line(aes(y = total_deaths - total_deaths*(PosTestNum/Samples)), col = 2) +
  geom_point(aes(y = total_deaths - total_deaths*(PosTestNum/Samples)), col = 2, size = 0.7) +
  facet_wrap(vars(Age_group), ncol = 1, scales = "free") +
  ylab("Deaths") + xlab("Date")

p5 <- ggplot(Combined_Mort_PMT, aes(x = date)) +
  ggtitle("Total samples and -ve results") +
  # geom_line(aes(y = total_deaths)) +
  # geom_line(aes(y = total_deaths - (total_deaths*(Samples-PosTestNum)/Samples)), col = 2) +
  geom_line(aes(y = Samples), col = 1) +
  # geom_point(aes(y = (Samples)), col = 1, size = 0.7) +
  geom_line(aes(y = Samples - PosTestNum), col = 2) +
  # geom_point(aes(y = (PosTestNum)), col = 2, size = 0.7) +
  facet_wrap(vars(Age_group), ncol = 1, scales = "free") +
  ylab("Deaths") + xlab("Date")


p6 <- ggplot(Combined_Mort_PMT, aes(x = date)) +
  ggtitle("-ve prev in samples") +
  # geom_line(aes(y = total_deaths)) +
  # geom_line(aes(y = total_deaths - (total_deaths*(Samples-PosTestNum)/Samples)), col = 2) +
  geom_line(aes(y = 1-(PosTestNum/Samples)), col = 2) +
  geom_point(aes(y = 1-(PosTestNum/Samples)), col = 2, size = 0.7) +
  facet_wrap(vars(Age_group), ncol = 1, scales = "free") +
  ylab("Prevalence") + xlab("Date")

Test_02x02



Test_02x1
Test_1x1



pdf("analysis/figures/34_Mortuary_deaths_samples_tests.pdf", height = 60, width = 30)
cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow = 1)
dev.off()

Age_groups.labs <- UTH_deaths_by_week_age_Weeks %>% group_by(Age_group) %>%
  summarise(Age_group = head(Age_group,1)) %>%
  # summarise(Mean.ll = sum(Mean.ll)) %>%
  mutate(lab = case_when(
    Age_group==1 ~ "Age: 0-4",
    Age_group==2 ~ "Age: 5-9",
    Age_group==3 ~ "Age: 10-14",
    Age_group==4 ~ "Age: 15-19",
    Age_group==5 ~ "Age: 20-24",
    Age_group==6 ~ "Age: 25-29",
    Age_group==7 ~ "Age: 30-34",
    Age_group==8 ~ "Age: 35-39",
    Age_group==9 ~ "Age: 40-44",
    Age_group==10 ~ "Age: 45-49",
    Age_group==11 ~ "Age: 50-54",
    Age_group==12 ~ "Age: 55-59",
    Age_group==13 ~ "Age: 60-64",
    Age_group==14 ~ "Age: 65-69",
    Age_group==15 ~ "Age: 70-74",
    Age_group==16 ~ "Age: 75-79",
    Age_group==17 ~ "Age: 80+")) %>%
  select(lab) %>%
  unlist()
#
names(Age_groups.labs) <- 1:17

# UTH_deaths_by_week_age_Weeks <- UTH_deaths_by_week_age_Weeks# %>%
  # mutate(linetype = case_when(year == 2020 ~ "2020",

p3_alone <- ggplot(UTH_deaths_by_week_age_Weeks %>% filter(week_comp>="2020-06-01",week_comp<"2020-11-01"), aes(x = week_comp, group = year, col = year, linetype = year)) +
  ggtitle("Weekly excess mortuary deaths by age") +
  geom_line(aes(y = total_deaths)) +
  facet_wrap(vars(Age_group),nrow = 4, scales = "free", labeller = labeller(Age_group = Age_groups.labs),dir = "h") +
  geom_vline(xintercept = as.Date("2020-06-15"), linetype = 2, colour = "darkblue") +
  geom_vline(xintercept = as.Date("2020-10-02"), linetype = 2, colour = "darkblue") +
  ylab("Deaths") + xlab("Date") +
  labs(col = "Year") +
  scale_color_manual(name = "Year", values=c("blue", "red", "black")) +
  scale_linetype_manual(name = "Year", values=c("dashed","dashed","solid"))
# year %in% c(2018,2019) ~ "Pre-2020"))

pdf(file = "analysis/figures/36_01_Total_Deaths_Excess_Deaths.pdf", width = 12, height = 6)
p3_alone
dev.off()


Age_groups.labs <- Combined_Mort_PMT %>% group_by(Age_group) %>%
  summarise(Age_group = head(Age_group,1)) %>%
  # summarise(Mean.ll = sum(Mean.ll)) %>%
  mutate(lab = case_when(
    Age_group==1 ~ "Age: 0-4",
    Age_group==2 ~ "Age: 5-9",
    Age_group==3 ~ "Age: 10-14",
    Age_group==4 ~ "Age: 15-19",
    Age_group==5 ~ "Age: 20-24",
    Age_group==6 ~ "Age: 25-29",
    Age_group==7 ~ "Age: 30-34",
    Age_group==8 ~ "Age: 35-39",
    Age_group==9 ~ "Age: 40-44",
    Age_group==10 ~ "Age: 45-49",
    Age_group==11 ~ "Age: 50-54",
    Age_group==12 ~ "Age: 55-59",
    Age_group==13 ~ "Age: 60-64",
    Age_group==14 ~ "Age: 65-69",
    Age_group==15 ~ "Age: 70-74",
    Age_group==16 ~ "Age: 75-79",
    Age_group==17 ~ "Age: 80+")) %>%
  select(lab) %>%
  unlist()
#
names(Age_groups.labs) <- 1:17

p4_alone <- ggplot(Combined_Mort_PMT %>% filter(Age_group %in% c(1:4)), aes(x = date)) +
  ggtitle("Total deaths (2020), est +ve prev") +
  geom_line(aes(y = total_deaths)) +
  geom_line(aes(y = total_deaths*(PosTestNum/Samples)), col = 2) +
  geom_point(aes(y = total_deaths*(PosTestNum/Samples)), col = 2, size = 0.7) +
  facet_wrap(vars(Age_group), nrow = 1, scales = "free", labeller = labeller(Age_group = Age_groups.labs)) +
  ylab("Deaths") + xlab("Date")

p5_alone <- ggplot(Combined_Mort_PMT %>% filter(Age_group %in% c(1:4)), aes(x = date)) +
  ggtitle("Total samples and +ve results") +
  geom_line(aes(y = Samples), col = 1) +
  geom_line(aes(y = PosTestNum), col = 2) +
  facet_wrap(vars(Age_group), nrow = 1, scales = "free", labeller = labeller(Age_group = Age_groups.labs)) +
  ylab("Deaths") + xlab("Date")

cowplot::plot_grid(p5_alone,p4_alone, ncol = 1)

