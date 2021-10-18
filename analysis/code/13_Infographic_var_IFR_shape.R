### Infographic:
rm(list = ls())
devtools::load_all(".")


IFR_vals <- readRDS(file = "analysis/data/Code-generated-data/00_03b_IFR_values.rds")
Age_vals <- seq(2.5, 82.5, by = 5)

#Plot 1: IFR curves
# What is the intercept when the IFR is 1.1?
IFR_Ints <- readRDS("analysis/data/Code-generated-data/IFR_mat_Ints.rds")

plot1_df <- data.frame(Age = seq(0,100,by = 0.1)) %>%
  mutate(IFR_1_1 = exp(Age * IFR_vals$IFR_Coefs[2] + IFR_vals$IFR_Coefs[1]),
         IFR_1.4_1 = exp(Age * IFR_vals$IFR_Coefs[2] + IFR_Ints[IFR_Ints$IFR_x==0.4 & IFR_Ints$Slope_x == 1,"Int_abs"]),
         IFR_0.6_1 = exp(Age * IFR_vals$IFR_Coefs[2] + IFR_Ints[IFR_Ints$IFR_x==1.6 & IFR_Ints$Slope_x == 1,"Int_abs"]),
         IFR_1_0.9 = exp(Age * 0.8 *IFR_vals$IFR_Coefs[2] + IFR_Ints[IFR_Ints$IFR_x==1.4 & IFR_Ints$Slope_x == 1,"Int_abs"]),
         IFR_1_1.1 = exp(Age * 1.2* IFR_vals$IFR_Coefs[2] + IFR_Ints[IFR_Ints$IFR_x==1.4 & IFR_Ints$Slope_x == 1,"Int_abs"]),
  )

library(dplyr)
library(tidyverse)
library(reshape2)
plot1_df_melt <- melt(plot1_df, id.vars = "Age") %>%
  # mutate(colour = variable)
  # browser()
  mutate(IFR = recode(variable, "IFR_1_1" = "Standard IFR", "IFR_1.4_1" = "1.4xIFR", "IFR_0.6_1" = "0.6xIFR", "IFR_1_0.9" = "0.8xSlope", "IFR_1_1.1" = "1.2xSlope"))
  # mutate(slope = recode(variable, "IFR_1_1" = "Standard IFR", "IFR_1.4_1" = "xIFR", "IFR_0.6_1" = "xIFR", "IFR_1_0.9" = "xSlope", "IFR_1_1.1" = "xSlope")) %>%

p1 <- ggplot(data = plot1_df_melt, aes(x = Age, y = value, group = variable, col = IFR, linetype = IFR)) + geom_line() +
  scale_colour_manual(values = c("black", "darkblue","darkblue", "darkred","darkred")) +
  scale_linetype_manual(values = c(1,3,2,3,2)) +
  ylab("IFR") + xlim(0,85) + ylim(0,25) + labs(color='', linetype = '') #+
  # geom_ribbon(data = subset(plot1_df_melt, variable == "IFR_0.6_1"),
  #             aes(ymin = value, ymax = subset(plot1_df_melt, variable == "IFR_1.4_1")[,"value"]),
  #             fill = "darkorange", alpha = 0.1) +
  # geom_ribbon(data = subset(plot1_df_melt, variable == "IFR_1_0.9"),
  #             aes(ymin = value, ymax = subset(plot1_df_melt, variable == "IFR_1_1.1")[,"value"]),
  #             fill = "darkred", alpha = 0.1)

# Plot 2: Death curves
fit_l_IFR_slope <- readRDS("../Bonus Files/fit_l_IFR_slope.RDS")

# Select the correct elements of the list
IFR_vec <- c(0.4,0.6,0.8,0.9,1,1.1,1.2,1.4,1.6)
Slope_vec <- seq(0.6, 1.4,by = 0.1)# * IFR_Coefs[2]
IFR_mat <- expand.grid("IFR_x" = IFR_vec, "Slope_x" = Slope_vec)
IFR_mat[IFR_mat$IFR_x %in% c(1.4,1,0.6) & IFR_mat$Slope_x == 1 | IFR_mat$IFR_x %in% c(1) & round(IFR_mat$Slope_x,1) %in% c(0.8,1,1.2),]

names(fit_l_IFR_slope) <- apply(X = IFR_mat, MARGIN = 1, function(x){paste0("IFR_",x[1],"_Slope",x[2])})

fit_subs <- fit_l_IFR_slope[IFR_mat$IFR_x %in% c(1.4,1,0.6) & IFR_mat$Slope_x == 1 | IFR_mat$IFR_x %in% c(1) & round(IFR_mat$Slope_x,1) %in% c(0.8,1,1.2)]
fit_subs <- fit_subs[c(3,2,4,1,5)]

## For each element in the list plot the mean mortality fitted data
# rowMeans(apply(fit_subs[[1]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-1]-rowMeans(apply(fit_subs[[1]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-nrow(fit_subs[[1]]$output)]

colnames <- names(fit_subs)
Deaths_list <- lapply(1:length(fit_subs), function(x){
  Deaths_tmp <- rownames_to_column(data.frame(Deaths = rowMeans(apply(fit_subs[[x]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-1]-rowMeans(apply(fit_subs[[x]]$output[,paste0("D[",1:17,"]"),], c(1,3), sum, na.rm = T))[-nrow(fit_subs[[x]]$output)])) %>%
    rename(Date = "rowname")
  names(Deaths_tmp)[2] <- colnames[x]
  Deaths_tmp
})

Deaths_df <- Deaths_list %>% reduce(full_join, by = "Date") %>%
  melt(id = "Date", variable = "IFR", value.name = "Deaths") %>%
  mutate(IFR = recode(IFR, "IFR_1_1" = "Standard IFR", "IFR_1.4_1" = "1.4xIFR", "IFR_0.6_1" = "0.6xIFR", "IFR_1_0.9" = "0.9xSlope", "IFR_1_1.1" = "1.1xSlope"))


fit_subs[[1]]$pmcmc_results$inputs$data

p2 <- ggplot(data = Deaths_df) +
  geom_line(aes(x = as.Date(Date), y = Deaths, group = IFR, col = IFR, linetype = IFR)) +
  scale_colour_manual(values = c("black", "darkblue","darkblue", "darkred","darkred")) +
  scale_linetype_manual(values = c(1,3,2,3,2)) + labs(color='', linetype = '') +
  xlab("Date") +
  geom_point(data = fit_subs[[1]]$pmcmc_results$inputs$data, aes(x = as.Date(date), y = deaths), alpha = 0.5, size = 0.5)
p2
  # geom_ribbon(data = subset(Deaths_df, IFR == "IFR_1_Slope0.8"),
  #             aes(ymin = Deaths, ymax = subset(Deaths_df, IFR == "IFR_1_Slope1.2")[,"Deaths"]),
  #             fill = "darkred", alpha = 0.2) +
  # geom_ribbon(data = subset(Deaths_df, IFR == "IFR_0.6_Slope1"),
  #             aes(ymin = Deaths, ymax = subset(Deaths_df, IFR == "IFR_1.4_Slope1")[,"Deaths"]),
  #             fill = "darkorange", alpha = 0.2)



## Plot 3: Plot the PCR/Sero curves
sero_pcr_df_l_IFR_slope <- lapply(X = fit_subs, FUN = function(x){
  seroprev_df(x)})

# Simplify data
Simple_Plot_Data_l_IFR_slope <- lapply(sero_pcr_df_l_IFR_slope, Summ_sero_pcr_data)

colnames <- names(Simple_Plot_Data_l_IFR_slope)
pcr_sero_list <- lapply(1:length(Simple_Plot_Data_l_IFR_slope), function(x){
  sero_pcr_tmp <- Simple_Plot_Data_l_IFR_slope[[x]] %>% select(date, mean_pcr, mean_sero)
  names(sero_pcr_tmp)[2] <- paste0("PCR_",colnames[x])
  names(sero_pcr_tmp)[3] <- paste0("sero_",colnames[x])
  sero_pcr_tmp
})


sero_df <- pcr_sero_list %>% reduce(full_join, by = "date") %>%
  select(date, paste0("sero_IFR_",c(1,0.6,1.4,1,1),"_Slope",c(1,1,1,0.8,1.2))) %>% arrange(date) %>%
  rename(IFR_1_Slope1 = "sero_IFR_1_Slope1",
         IFR_0.6_Slope1 = "sero_IFR_0.6_Slope1",
         IFR_1.4_Slope1 = "sero_IFR_1.4_Slope1",
         IFR_1_Slope0.8 = "sero_IFR_1_Slope0.8",
         IFR_1_Slope1.2 = "sero_IFR_1_Slope1.2") %>%
  melt(id = c("date"), variable = "IFR", value.name = "sero")

pcr_df <- pcr_sero_list %>% reduce(full_join, by = "date") %>%
  select(date, paste0("PCR_IFR_",c(1,0.6,1.4,1,1),"_Slope",c(1,1,1,0.8,1.2))) %>% arrange(date) %>%
  rename(IFR_1_Slope1 = "PCR_IFR_1_Slope1",
         IFR_0.6_Slope1 = "PCR_IFR_0.6_Slope1",
         IFR_1.4_Slope1 = "PCR_IFR_1.4_Slope1",
         IFR_1_Slope0.8 = "PCR_IFR_1_Slope0.8",
         IFR_1_Slope1.2 = "PCR_IFR_1_Slope1.2") %>%
  melt(id = c("date"), variable = "IFR", value.name = "pcr")

pcr_sero_df <- full_join(x = sero_df, y = pcr_df, by = c("date","IFR"))

p3 <- ggplot(pcr_sero_df, aes(x = date, y = pcr, group = IFR, col = IFR, linetype = IFR)) +
  geom_line() +
  scale_colour_manual(values = c("black", "darkblue","darkblue", "darkred","darkred")) +
  scale_linetype_manual(values = c(1,3,2,3,2)) + labs(color='', linetype = '') +
  geom_line(aes(y = sero/3)) +
  scale_y_continuous(
    name = "PCR+ prevalence", sec.axis = sec_axis(~.*3, name = "Seroprevalence"),
  ) + xlab("Date") +
  geom_point(aes(x= as.Date("2020-07-15"),y=7.6), col = "black", size = 0.9) +
  geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10), col = "black", width = 3, size = 0.3) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0), col = "black", size = 0.3, height = 0.2) +
  # annotate("text", x=as.Date("2020-07-15")+5, y=7.6+0.5, label="Mulenga et al.", hjust=0) +
  geom_point(aes(x= as.Date("2020-07-15"),y=2.1/3), col = "black", size = 0.9) +
  geom_errorbar(aes(ymin=1.1/3,ymax=3.1/3,x=as.Date("2020-07-15"), width=10), col = "black", width = 3, size = 0.3) +
  geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1/3, height=0), col = "black", size = 0.3, height = 0.2)+
  # annotate(geom = "text", x = "2020-06-01", y = 7.6,  label = "PCR") +
  annotate(geom = "text", x=as.Date("2020-07-19"), y = 8, hjust=0, label = "PCR+", fontface = "bold", size = 2.5) +
  annotate(geom = "text", x=as.Date("2020-07-19"), y = 1.1, hjust=0, label = "Sero+", fontface = "bold", size = 2.5)
  # geom_label(aes(x = as.Date("2020-06-17"), y = 8,  label = "PCR", col = "black"))+
  # geom_label(aes(x = as.Date("2020-08-03"), y = 1.2,  label = "Sero"))



  # geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_1_Slope0.8"),
  #             aes(ymin = sero/3, ymax = subset(pcr_sero_df, IFR == "IFR_1_Slope1.2")[,"sero"]/3),
  #             fill = "darkred", alpha = 0.1) +
  # geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_0.6_Slope1"),
  #             aes(ymin = sero/3, ymax = subset(pcr_sero_df, IFR == "IFR_1.4_Slope1")[,"sero"]/3),
  #             fill = "darkorange", alpha = 0.1) +
  # geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_1_Slope0.8"),
  #             aes(ymin = pcr, ymax = subset(pcr_sero_df, IFR == "IFR_1_Slope1.2")[,"pcr"]),
  #             fill = "darkred", alpha = 0.1) +
  # geom_ribbon(data = subset(pcr_sero_df, IFR == "IFR_0.6_Slope1"),
  #             aes(ymin = pcr, ymax = subset(pcr_sero_df, IFR == "IFR_1.4_Slope1")[,"pcr"]),
  #             fill = "darkorange", alpha = 0.1)
p3



# Plot 4: Plot the Death distribution by age of each:
# Can I also plot the expected points from the BMJ paper?
# Get the proportion of deaths from BMJ
BMJ_Deaths_by_Age_Covid_Neg <- c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
# c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2)
BMJ_Deaths_by_Age_Covid_Pos <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Covid_Pos2 <- c(40,3,4,7,15,20,24,34,33,26,18,26,23,16,17,16,12,9,5,2,2) - c(37,3,3,6,14,17,21,28,27,21,16,22,19,14,14,11,11,6,2,2,0)
BMJ_Deaths_by_Age_Tot <- c(42,3,4,7,15,22,24,34,35,29,18,26,23,17,18,16,12,10,5,2,2)

Deaths_Age_List <- lapply(1:length(fit_subs), function(x){
  # browser()
  return(rowMeans(fit_subs[[x]]$output[nrow(fit_subs[[x]]$output),paste0("D[",1:17,"]"),]))
})
names(Deaths_Age_List) <- names(fit_subs)
Deaths_Age_df<-do.call(cbind.data.frame, Deaths_Age_List) %>%
  mutate(Age = Age_vals) %>%
  melt(id = "Age", variable = "IFR", value.name = "Deaths")

Est_Deaths <- data.frame(Age = Age_vals,
                         Deaths = c((BMJ_Deaths_by_Age_Covid_Pos2*10/0.8)[1:16],sum((BMJ_Deaths_by_Age_Covid_Pos2*10/0.8)[17:20])))

p4 <- ggplot(Deaths_Age_df, aes(x = Age)) +
  geom_line(aes(y = Deaths, group = IFR, col = IFR, linetype = IFR)) +
  scale_colour_manual(values = c("black", "darkblue","darkblue", "darkred","darkred")) +
  scale_linetype_manual(values = c(1,3,2,3,2)) + labs(color='', linetype = '') +
  geom_point(data = Est_Deaths, aes(x = Age, y = Deaths), size = 0.5, alpha = 1)




pdf("analysis/figures/13_Infographic.pdf", width = 8, height = 6)
cowplot::plot_grid(p1 + theme_light() + theme(legend.position = "none"),
                   p2 + theme_light() +theme(legend.position = "none"),
                   cowplot::get_legend(p1 + theme_light() + labs(color='') + theme(legend.box.margin = margin(120,0,0,10))),
                   p3 + theme_light() +theme(legend.position = "none"),
                   p4 + theme_light() +theme(legend.position = "none"),
                   nrow = 2, rel_widths = c(4, 4, 1.5))
dev.off()

summary(cowplot::get_legend(p2))


