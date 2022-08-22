### Figure 1 plot:
library(shadowtext)

UTH_Mortality_Total <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records_v2.csv") %>%
  filter(age_years !=".",
         dod !=".") %>%
  mutate(date = as.Date(dod, "%m/%d/%y"),
         Age_gr = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F)) %>%
  select(-sex, -dod, -age_years)

BurRegs <- readRDS("analysis/data/Code-generated-data/00_07_Burial_registrations_by_week_2017_to_2021.rds") %>%
  rename(Week = Week_st,
         BurRegs = Total_deaths) %>%
  filter(Week >= as.Date("2017-12-25"),
         Week < as.Date("2021-06-15")) %>%
  complete(Week)

Off_data <- readRDS("analysis/data/Code-generated-data/00_01_Lusaka_Dist_Deaths_Official.rds") %>%
  mutate(Week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(Week) %>% summarise(Off_deaths = sum(deaths))

# Off_data %>% filter(Week>=as.Date("2020-06-16"),
                    # Week<as.Date("2020-10-04")) %>% tail()  pull(Off_deaths) %>% sum()

PlotData <- UTH_Mortality_Total %>%
  mutate(Week = lubridate::floor_date(date, unit = "week", week_start = 1),
         Age_gr = cut(Age_gr, c(seq(0,18,by = 2)), right = T, labels = F)) %>%
  group_by(Week, Age_gr) %>%
  summarise(Weekly_Age_deaths = n()) %>%
  ungroup() %>% complete(Week, Age_gr, fill = list(Weekly_Age_deaths = 0)) %>%
  group_by(Week) %>%
  mutate(Weekly_Total_deaths = sum(Weekly_Age_deaths),
         Weekly_Prop_deaths = Weekly_Age_deaths/Weekly_Total_deaths) %>%
  # merge(BurRegs, all = T) %>% merge(Off_data, all = T) %>%
  filter(Week >= as.Date("2017-12-25"),
         Week < as.Date("2021-06-15"))

Av_Age <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records_v2.csv") %>%
  filter(age_years !=".",
         dod !=".") %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  mutate(Week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(Week) %>% summarise(av_age = mean(as.numeric(age_years))) %>%
  filter(Week >= as.Date("2017-12-25"),
         Week < as.Date("2021-06-15"))


coeff <- 600
p1 <- ggplot(data = PlotData, aes(x = Week, y = Weekly_Prop_deaths, group = as.factor(Age_gr), fill = as.factor(Age_gr))) +
  geom_area() +
  # geom_bar(stat = "identity", position = "stack", width = 10) +
  viridis::scale_fill_viridis(name = "Age group", discrete = T, option = "H", labels = c("0-5",
                                                                     paste0(c(1:7)*10-5,"-",c(1:7)*10+5),
                                                                     "75+")) +
  # guides(fill=guide_legend(title="Age group")) +
  # facet_grid(~stringr::str_wrap(Time_group, width = 10), space = "free_x", scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = -3)) +
  ylab("Proportion") +
  xlab("Date") +
  geom_vline(xintercept = as.Date(c("2020-03-17",
                                    # "2020-03-26",
                                    "2020-04-24",
                                    # "2020-05-08",
                                    "2020-06-01",
                                    "2020-10-05")), color = "white", linetype = "dashed", size = 1) +
  # annotate(geom = "shadowtext", x = as.Date(c("2020-03-17",
  #                                       # "2020-03-26",
  #                                       "2020-04-24",
  #                                       # "2020-05-08",
  #                                       "2020-06-01",
  #                                       "2020-10-05")),
  #          y = 0.99, label = stringr::str_wrap(c("NPIs implemented",
  #                                                # "Full NPIs implemented",
  #                                                "Some restrictions lifted",
  #                                                # "Further restrictions lifted",
  #                                                "Further restrictions lifted",
  #                                                "Restrictions fully lifted"), width = 40),
  #          color = "white", angle = 90, size = 3.5, hjust = 1, vjust = 0.5, fontface =2, bg.color='black',bg.r = 0.3) +
  geom_line(data = BurRegs, aes(y = BurRegs/coeff, x = Week, color = "Burial registrations"), size = 1.2, inherit.aes = F) +
  # geom_line(data = Off_data, aes(y = Off_deaths/coeff, x = Week, color = "Official covid deaths"), size = 1.2, inherit.aes = F) +
  ggborderline::geom_borderline(data = Off_data, aes(y = Off_deaths/coeff, x = Week, color = "Official covid deaths"), size = 1.2, inherit.aes = F) +
  # geom_ribbon(data = Off_data, aes(ymin = Off_deaths/coeff,ymax = Off_deaths/coeff, x = Week, fill = "Official covid deaths"), inherit.aes = F) +
  scale_y_continuous(name = "Proportion",
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~.*coeff, name=""), expand = c(0.01,0.01)) +
  guides(color = guide_legend(title = "",override.aes = list(color = c("black","white"),
                                                  bordercolor = c("white", viridis::turbo(n = 9, begin = 0, end =1)[9]),
                                                  bordersize = c(NULL, 2.5)),
                              order = 1)) +
    # theme(legend.key = element_rect(fill = NA),
        # legend.background = element_rect(fill = NA),
        # legend.text = element_text(size = 12),
        # legend.title = element_text(size = 12,face='bold'))
  scale_color_manual(name = "", values = c("black","white")) +
  # scale_fill_discrete(name = "Test") +
  # scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0,0)) +
  scale_x_date(position = "bottom", date_breaks = "1 month", date_labels = "%b %Y",
               sec.axis = dup_axis(breaks = as.Date(c("2020-03-17",
                                                      # "2020-03-26",
                                                      "2020-04-24",
                                                      # "2020-05-08",
                                                      "2020-06-01",
                                                      "2020-10-05")),
                                   labels = c("NPIs implemented",
                                              # "Full NPIs implemented",
                                              "Some NPIs lifted",
                                              # "Further restrictions lifted",
                                              "Further NPIs lifted",
                                              "NPIs fully lifted")), expand = c(0,0)) +
  # date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(#axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = -0.05),
        axis.title.x.top = element_blank(),
        # plot.title = element_text(vjust = -25),
        legend.spacing.y = unit(0.3, 'cm'))

  # theme(legend.key = element_rect(fill = viridis::turbo(n = 9, begin = 0, end =1)[6], color = "black"))

pdf("analysis/figures/47_Figure_1_Prop_Burial_Regs.pdf", width = 15)
p1
dev.off()

tiff("analysis/figures/47_Figure_1_Prop_Burial_Regs.tiff", width = 15, height = 7, units = "in", res = 200)
p1
dev.off()
