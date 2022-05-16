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
  mutate(Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  group_by(date,Age_gr) %>% summarise(Mort_deaths = length(date))# %>%
  # ungroup %>%
  # tidyr::complete(date, Age_gr, fill = list(Mort_deaths = 0))
# filter(date >= "2020-06-15" & date <= "2020-11-01")

# UTH_deaths_by_date <- UTH_Mortality_Total %>% filter(dod != ".") %>%
#   group_by(date) %>% summarise(Mort_deaths = length(date)) #%>%
  # filter(date >= "2020-06-15" & date <= "2020-11-01")
  # filter(date >= "2020-06-15" & date <= "2020-10-05")


date_list_Mort <- seq(min(UTH_deaths_by_age$date), max(UTH_deaths_by_age$date), by = 1)
missing_dates_Mort <- date_list_Mort[!date_list_Mort %in% UTH_deaths_by_age$date] # Add missing dates with 0 deaths
#
UTH_deaths_by_age <- add_row(UTH_deaths_by_age, date = missing_dates_Mort, Mort_deaths = 0) %>% arrange(date)

UTH_deaths_by_week_age <- UTH_deaths_by_age %>%#  filter(Age_gr ==1) %>%
  mutate(date = as.Date(cut.Date(date, breaks = "1 week"))) %>%
  mutate(year = format(date,"%Y"),
         month = format(date,"%m")) %>%
  group_by(Age_gr, month, year) %>%
  summarise(Mort_deaths = sum(Mort_deaths))

p1 <- ggplot(UTH_deaths_by_week_age, aes(x = month, group = year, col = year)) +
  ggtitle("Total monthly deaths by year (log10 axis)") +
  geom_line(aes(y = Mort_deaths)) +
  facet_wrap(vars(Age_gr), ncol = 1) +#, scales = "free") +
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
  group_by(Age_gr, year, week_comp) %>%
  summarise(Mort_deaths = sum(Mort_deaths))

  # filter(month> 05, month <12) %>%

p2 <- ggplot(UTH_deaths_by_week_age_Weeks, aes(x = week_comp, group = year, col = year)) +
  ggtitle("Total weekly deaths by year") +
  geom_line(aes(y = Mort_deaths)) +
  facet_wrap(vars(Age_gr), ncol = 1, scales = "free") +
  geom_vline(xintercept = as.Date("2020-06-15"), linetype = 2, colour = "darkblue") +
  geom_vline(xintercept = as.Date("2020-10-02"), linetype = 2, colour = "darkblue") +
  ylab("Deaths") + xlab("Date") +
  theme(legend.position="none")

  # scale_y_continuous(trans='log10')


p3 <- ggplot(UTH_deaths_by_week_age_Weeks %>% filter(week_comp>="2020-06-01",week_comp<"2020-11-01"), aes(x = week_comp, group = year, col = year)) +
  ggtitle("Total weekly deaths by year - study period") +
  geom_line(aes(y = Mort_deaths)) +
  facet_wrap(vars(Age_gr), ncol = 1, scales = "free") +
  geom_vline(xintercept = as.Date("2020-06-15"), linetype = 2, colour = "darkblue") +
  geom_vline(xintercept = as.Date("2020-10-02"), linetype = 2, colour = "darkblue") +
  ylab("Deaths") + xlab("Date")



p4 <- ggplot(Combined_Mort_PMT, aes(x = date)) +
  ggtitle("Total deaths (2020), est -ve prev") +
  geom_line(aes(y = Mort_deaths)) +
  # geom_line(aes(y = Mort_deaths - (Mort_deaths*(Samples-PosTests)/Samples)), col = 2) +
  geom_line(aes(y = Mort_deaths - Mort_deaths*(PosTests/Samples)), col = 2) +
  geom_point(aes(y = Mort_deaths - Mort_deaths*(PosTests/Samples)), col = 2, size = 0.7) +
  facet_wrap(vars(Age_gr), ncol = 1, scales = "free") +
  ylab("Deaths") + xlab("Date")

p5 <- ggplot(Combined_Mort_PMT, aes(x = date)) +
  ggtitle("Total samples and -ve results") +
  # geom_line(aes(y = Mort_deaths)) +
  # geom_line(aes(y = Mort_deaths - (Mort_deaths*(Samples-PosTests)/Samples)), col = 2) +
  geom_line(aes(y = Samples), col = 1) +
  # geom_point(aes(y = (Samples)), col = 1, size = 0.7) +
  geom_line(aes(y = Samples - PosTests), col = 2) +
  # geom_point(aes(y = (PosTests)), col = 2, size = 0.7) +
  facet_wrap(vars(Age_gr), ncol = 1, scales = "free") +
  ylab("Deaths") + xlab("Date")


p6 <- ggplot(Combined_Mort_PMT, aes(x = date)) +
  ggtitle("-ve prev in samples") +
  # geom_line(aes(y = Mort_deaths)) +
  # geom_line(aes(y = Mort_deaths - (Mort_deaths*(Samples-PosTests)/Samples)), col = 2) +
  geom_line(aes(y = 1-(PosTests/Samples)), col = 2) +
  geom_point(aes(y = 1-(PosTests/Samples)), col = 2, size = 0.7) +
  facet_wrap(vars(Age_gr), ncol = 1, scales = "free") +
  ylab("Prevalence") + xlab("Date")

# Test_02x02



# Test_02x1
# Test_1x1



pdf("analysis/figures/36_Mortuary_deaths_samples_tests_2.pdf", height = 60, width = 30)
cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow = 1)
dev.off()

Age_grs.labs <- UTH_deaths_by_week_age_Weeks %>% group_by(Age_gr) %>%
  summarise(Age_gr = head(Age_gr,1)) %>%
  # summarise(Mean.ll = sum(Mean.ll)) %>%
  mutate(lab = case_when(
    Age_gr==1 ~ "Age: 0-4",
    Age_gr==2 ~ "Age: 5-9",
    Age_gr==3 ~ "Age: 10-14",
    Age_gr==4 ~ "Age: 15-19",
    Age_gr==5 ~ "Age: 20-24",
    Age_gr==6 ~ "Age: 25-29",
    Age_gr==7 ~ "Age: 30-34",
    Age_gr==8 ~ "Age: 35-39",
    Age_gr==9 ~ "Age: 40-44",
    Age_gr==10 ~ "Age: 45-49",
    Age_gr==11 ~ "Age: 50-54",
    Age_gr==12 ~ "Age: 55-59",
    Age_gr==13 ~ "Age: 60-64",
    Age_gr==14 ~ "Age: 65-69",
    Age_gr==15 ~ "Age: 70-74",
    Age_gr==16 ~ "Age: 75-79",
    Age_gr==17 ~ "Age: 80+")) %>%
  select(lab) %>%
  unlist()
#
names(Age_grs.labs) <- 1:17

# UTH_deaths_by_week_age_Weeks <- UTH_deaths_by_week_age_Weeks# %>%
  # mutate(linetype = case_when(year == 2020 ~ "2020",

p3_alone <- ggplot(UTH_deaths_by_week_age_Weeks %>% filter(week_comp>="2020-06-01",week_comp<"2020-11-01"), aes(x = week_comp, group = year, col = year, linetype = year)) +
  ggtitle("Weekly excess mortuary deaths by age") +
  geom_line(aes(y = Mort_deaths)) +
  facet_wrap(vars(Age_gr),nrow = 4, scales = "free", labeller = labeller(Age_gr = Age_grs.labs),dir = "h") +
  geom_vline(xintercept = as.Date("2020-06-15"), linetype = 2, colour = "darkblue") +
  geom_vline(xintercept = as.Date("2020-10-02"), linetype = 2, colour = "darkblue") +
  ylab("Deaths") + xlab("Date") +
  labs(col = "Year") +
  scale_color_manual(name = "Year", values=c("blue", "red", "black")) +
  scale_linetype_manual(name = "Year", values=c("dashed","dashed","solid"))
# year %in% c(2018,2019) ~ "Pre-2020"))

pdf(file = "analysis/figures/36_01_Mort_deaths_Excess_Deaths.pdf", width = 12, height = 6)
p3_alone
dev.off()


Age_grs.labs <- Combined_Mort_PMT %>% group_by(Age_gr) %>%
  summarise(Age_gr = head(Age_gr,1)) %>%
  # summarise(Mean.ll = sum(Mean.ll)) %>%
  mutate(lab = case_when(
    Age_gr==1 ~ "Age: 0-4",
    Age_gr==2 ~ "Age: 5-9",
    Age_gr==3 ~ "Age: 10-14",
    Age_gr==4 ~ "Age: 15-19",
    Age_gr==5 ~ "Age: 20-24",
    Age_gr==6 ~ "Age: 25-29",
    Age_gr==7 ~ "Age: 30-34",
    Age_gr==8 ~ "Age: 35-39",
    Age_gr==9 ~ "Age: 40-44",
    Age_gr==10 ~ "Age: 45-49",
    Age_gr==11 ~ "Age: 50-54",
    Age_gr==12 ~ "Age: 55-59",
    Age_gr==13 ~ "Age: 60-64",
    Age_gr==14 ~ "Age: 65-69",
    Age_gr==15 ~ "Age: 70-74",
    Age_gr==16 ~ "Age: 75-79",
    Age_gr==17 ~ "Age: 80+")) %>%
  select(lab) %>%
  unlist()
#
names(Age_grs.labs) <- 1:17

p4_alone <- ggplot(Combined_Mort_PMT #%>% filter(Age_gr %in% c(1:4))
                   , aes(x = date)) +
  ggtitle("Total deaths (2020), est +ve prev") +
  geom_line(aes(y = Mort_deaths)) +
  geom_line(aes(y = Mort_deaths*(PosTests/Samples)), col = 2) +
  geom_point(aes(y = Mort_deaths*(PosTests/Samples)), col = 2, size = 0.7) +
  facet_wrap(vars(Age_gr), scales = "free", labeller = labeller(Age_gr = Age_grs.labs)) +
  ylab("Deaths") + xlab("Date")

coeff <- 0.5

p5_alone <- ggplot(Combined_Mort_PMT #%>% filter(Age_gr %in% c(1:4))
                   , aes(x = date)) +
  ggtitle("Total samples and +ve results") +
  geom_line(aes(y = Samples), col = 1) +
  geom_line(aes(y = PosTests), col = 2) +
  geom_line(aes(y = (PosTests/Samples)/coeff), col = "grey", linetype = 2) +
  geom_point(aes(y = (PosTests/Samples)/coeff), col = "grey", size = 0.8) +
  facet_wrap(vars(Age_gr), scales = "free", labeller = labeller(Age_gr = Age_grs.labs)) +
  ylab("Deaths") + xlab("Date") +
  scale_y_continuous(
    name = "Tests",
    sec.axis = sec_axis(~.*coeff, name="Prevalence")
  )

# cowplot::plot_grid(p5_alone,p4_alone, ncol = 1)

pdf(file = "analysis/figures/36_02_Mort_samples_pos_tests.pdf", width = 12, height = 6)
p5_alone
dev.off()

