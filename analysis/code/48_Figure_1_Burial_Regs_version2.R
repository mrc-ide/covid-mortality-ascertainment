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
         Week < as.Date("2021-06-12"))

Off_data_prov <- readRDS("analysis/data/Code-generated-data/00_01_Lusaka_Prov_Deaths_Official.rds") %>%
  mutate(Week = lubridate::floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(Week) %>% summarise(Off_deaths = sum(deaths))

Off_data_dist <- readRDS("analysis/data/Code-generated-data/00_01_Lusaka_Dist_Deaths_Official.rds") %>%
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


# coeff <- 600
p1 <- ggplot(data = PlotData, aes(x = Week, y = Weekly_Prop_deaths, group = as.factor(Age_gr), fill = as.factor(Age_gr))) +
  geom_area() +
  # geom_bar(stat = "identity", position = "stack", width = 10) +
  viridis::scale_fill_viridis(discrete = T, option = "H", labels = c("0-5",
                                                                     paste0(c(1:7)*10-5,"-",c(1:7)*10+5),
                                                                     "75+")) +
  guides(fill=guide_legend(title="")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Proportion") +
  xlab("Date") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0,0)) +
  geom_vline(xintercept = as.Date(c("2020-03-17",
                                    # "2020-03-26",
                                    "2020-04-24",
                                    # "2020-05-08",
                                    "2020-06-01",
                                    "2020-10-05")), color = "white", linetype = "dashed", size = 1) +
  # annotate(geom = "shadowtext", x = as.Date(c("2020-03-17",
  #                                             "2020-03-26",
  #                                             "2020-04-24",
  #                                             "2020-05-08",
  #                                             "2020-06-01",
  #                                             "2020-10-05")),
  #          y = 0.99, label = stringr::str_wrap(c("Initial NPIs implemented",
  #                                                "Full NPIs implemented",
  #                                                "Some restrictions lifted",
  #                                                "Further restrictions lifted",
  #                                                "Further restrictions lifted",
#                                                "Restrictions fully lifted"), width = 40),
# color = "white", angle = 90, size = 3.5, hjust = 1, vjust = 0.5, fontface =2, bg.color='black',bg.r = 0.3) +
scale_y_continuous(name = "Proportion", expand = c(0,0)) +
  guides(color = guide_legend(title = "Age group",override.aes = list(color = c("black","white"),
                                                                      bordercolor = c("white", viridis::turbo(n = 9, begin = 0, end =1)[9]),
                                                                      bordersize = c(NULL, 2.5)))) +
  coord_cartesian(as.Date(c("2017-12-25","2021-06-15"))) +
  ggtitle(label = "D")


p2 <- ggplot() +
  geom_line(data = BurRegs, aes(y = BurRegs, x = Week), size = 1.2, inherit.aes = F, color = "black") +
  # geom_line(data = Off_data_dist, aes(y = Off_deaths, x = Week, linetype = stringr::str_wrap("Official covid deaths (Lusaka district)", width = 25)), size = 1.2, inherit.aes = F, color = "black") +
  # geom_line(data = Off_data_prov, aes(y = Off_deaths, x = Week, linetype = stringr::str_wrap("Official covid deaths (Lusaka province)", width = 25)), size = 1, inherit.aes = F, color = "black") +
  theme_minimal() +
  ylab("Burial registrations") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0,0)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  guides(linetype = guide_legend(byrow = TRUE))+
  # theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  coord_cartesian(xlim = as.Date(c("2017-12-25","2021-06-15")), ylim = c(0,500)) +
  # geom_segment(aes(x = as.Date("2020-03-17"), xend = as.Date("2020-03-17"), y = -100, yend = 450))+
  geom_vline(xintercept = as.Date(c("2020-03-17",
                                    # "2020-03-26",
                                    "2020-04-24",
                                    # "2020-05-08",
                                    "2020-06-01",
                                    "2020-10-05")), color = "black", linetype = "dashed", size = 1) +
  ggtitle(label = "B")
  # scale_linetype_manual("", values = c(1,2,3))
# annotate(geom = "text", x = as.Date(c("2020-03-17",
#                                             "2020-03-26",
#                                             "2020-04-24",
#                                             "2020-05-08",
#                                             "2020-06-01",
#                                             "2020-10-05")),
#          y = 450, label = stringr::str_wrap(c("Initial NPIs implemented",
#                                                "Full NPIs implemented",
#                                                "Some restrictions lifted",
#                                                "Further restrictions lifted",
#                                                "Further restrictions lifted",
#                                                "Restrictions fully lifted"), width = 40),
#          color = "black", angle = 45, size = 3.5, hjust = 0, vjust = 0.5, fontface =1)

p3 <- ggplot() +
  geom_line(data = Av_Age, mapping = aes(x = Week, y = av_age), size = 1.2) +
  theme_minimal() +
  ylab("Average age at death") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0,0)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  coord_cartesian(xlim = as.Date(c("2017-12-25","2021-06-15"))) +
  geom_vline(xintercept = as.Date(c("2020-03-17",
                                    # "2020-03-26",
                                    "2020-04-24",
                                    # "2020-05-08",
                                    "2020-06-01",
                                    "2020-10-05")), color = "black", linetype = "dashed", size = 1) +
  ggtitle("C")

p4 <- ggplot() +
  geom_line(data = Off_data_dist, aes(y = Off_deaths, x = Week, linetype = stringr::str_wrap("Lusaka district", width = 25)), size = 1.2, inherit.aes = F, color = "black") +
  geom_line(data = Off_data_prov, aes(y = Off_deaths, x = Week, linetype = stringr::str_wrap("Lusaka province", width = 25)), size = 1, inherit.aes = F, color = "black") +
  theme_minimal() +
  ylab("Confirmed covid deaths") +
  scale_x_date(position = "bottom", date_breaks = "1 month", date_labels = "%b %Y",
               sec.axis = dup_axis(breaks = as.Date(c("2020-03-17",
                                                      # "2020-03-26",
                                                      "2020-04-24",
                                                      # "2020-05-08",
                                                      "2020-06-01",
                                                      "2020-10-05")),
                                   labels = c("NPIs implemented",
                                              # "Full NPIs implemented",
                                              "Some restrictions lifted",
                                              # "Further restrictions lifted",
                                              "Further restrictions lifted",
                                              "Restrictions fully lifted")), expand = c(0,0)) +
  # date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.text.x.top = element_text(angle = 45, hjust = 0),
        plot.title = element_text(vjust = -25),
        legend.spacing.y = unit(0.3, 'cm')) +
  coord_cartesian(xlim = as.Date(c("2017-12-25","2021-06-15"))) +
  geom_vline(xintercept = as.Date(c("2020-03-17",
                                    # "2020-03-26",
                                    "2020-04-24",
                                    # "2020-05-08",
                                    "2020-06-01",
                                    "2020-10-05")), color = "black", linetype = "dashed", size = 1) +
  ggtitle("A") +
  scale_linetype_manual("", values = c(1,3))




g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)#
g3 <- ggplotGrob(p3)#
g4 <- ggplotGrob(p4)#

# set the same widths for both blots
library(grid)
g1$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths)
g2$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths)
g3$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths)
g4$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths)

# stack them afterwards
g <- rbind(g4, g2, g3, g1, size="first") # stack the two plots

# g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
g$heights[7] <- unit(1,"null")
g$heights[19] <- unit(2,"null")
g$heights[31] <- unit(1,"null")
g$heights[43] <- unit(4,"null")
grid.newpage()
grid.draw(g)


pdf("analysis/figures/47_Figure_1_Prop_Burial_Regs_version2.pdf", width = 15, height = 10)
grid.draw(g)
dev.off()

tiff("analysis/figures/47_Figure_1_Prop_Burial_Regs_version2.tiff", width = 15, height = 10, units = "in", res = 150)
grid.draw(g)
dev.off()



# grid.arrange(p2, p1 + theme(legend.position = "none"), ncol = 1, heights = c(2, 1))


# p3 <- ggplot() +
#   # geom_line(data = BurRegs, aes(y = BurRegs, x = Week, color = "Burial registrations"), size = 1.2, inherit.aes = F, color = "black") +
#   geom_line(data = Off_data_dist, aes(y = Off_deaths, x = Week, color = "Official covid deaths (Lusaka district)"), size = 1.2, inherit.aes = F, color = "black") +
#   geom_line(data = Off_data_prov, aes(y = Off_deaths, x = Week, color = "Official covid deaths (Lusaka province)"), size = 1, inherit.aes = F, linetype = "dashed", color = "black") +
#   theme_minimal() +
#   ylab("Mortality") +
#   scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0,0)) +
#   theme(axis.text.x = element_text(angle = 90))

# p1

# library(grid)
# library(gridExtra)

gb1 <- ggplot_build(p1)
gb2 <- ggplot_build(p2)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
# g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
# g$heights[panels] <- list(unit(5, "null"), unit(1,"null")) # change 5 to other int

g1 <- ggplotGrob(p1)
# g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
g2 <- ggplotGrob(p2)

g1$widths <- unit.pmax(g1$widths, g2$widths)
g2$widths <- unit.pmax(g1$widths, g2$widths)
g <- rbind(g1, g2, size="first") # stack the two plots
grid.newpage()
g$heights[g$layout$t[grep("panel", g$layout$name)]] <- list(unit(1*5, "null"), unit(1,"null"))

length(g1$panel$ranges[[1]]$y.labels)

grid.draw(g)

grid.newpage()
grid.draw(rbind(ggplotGrob(g1), ggplotGrob(g2), size = "last"))
grid.draw(rbind(ggplotGrob(p3), ggplotGrob(p2), size = "last"))
grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p1), size = "last"))
grid.arrange(p1,p2, ncol = 1)


pdf("analysis/figures/47_Figure_1_Prop_Burial_Regs.pdf", width = 15)
p1
dev.off()

tiff("analysis/figures/47_Figure_1_Prop_Burial_Regs.tiff", width = 15, height = 7, units = "in", res = 200)
p1
dev.off()

# scale_color_manual(name = "", values = c("white")) +
# geom_line(data = Av_Age, aes(x = Week, y = 5*av_age/coeff), inherit.aes = F, color = "white", size = 2) +
# annotate(geom = "text", hjust = 0, vjust = 1, size = 4, x = as.Date(c("2019-01-05")), y = 5*50/coeff,label = "Average age", color = "white")

