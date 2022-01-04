library(tidyverse)
# 18: Looking at the BMJ data:

df_BMJ <- read.csv(file = "analysis/data/raw/BMJ_Mwananyanda_PostMortem_UTH/covid_results.csv")
df_BMJ <- df_BMJ %>% filter(deceased_date != ".") %>% mutate(date = as.Date(deceased_date)) # Filter death with no date (1)
df_BMJ <- df_BMJ %>% mutate(Test1_45 = ifelse(N1_CT_1 != ".", T, F),
                  Test2_45 = ifelse(N2_CT_1 != ".", T, F),
                  Test1_40 = ifelse(N1_CT_1 != "." & N1_CT_1<=40, T, F),
                  Test2_40 = ifelse(N2_CT_1 != "." & N2_CT_1<=40, T, F)) %>%
                  # Test1_40 = ifelse(!is.na(round(as.numeric(N1_CT_1))) & N1_CT_1<=40, T, F),
                  # Test2_40 = ifelse(!is.na(round(as.numeric(N2_CT_1))) & N2_CT_1<=40, T, F)) %>%
  mutate(Either_Test_Pos = ifelse(Test1_45==T | Test2_45==T, T, F),
         Both_Tests_Pos = ifelse(Test1_45==T & Test2_45==T, T, F),
         Either_Test_Pos_40 = ifelse(Test1_40==T | Test2_40==T, T, F),
         Both_Tests_Pos_40 = ifelse(Test1_40==T & Test2_40==T, T, F)) %>%
  arrange(date)

# Plot all the data
df_tot_sampled <- df_BMJ %>% group_by(date) %>% summarise(deaths = length(N1_CT_1))
plot(x = df_tot_sampled$date, y = df_tot_sampled$deaths, pch = 20, ylim = c(0,15), xlab = "Date", ylab = "Deaths", xaxt = "n", main = "All Deaths")
axis(1, at=as.Date(c("2020-07-01","2020-10-01","2021-01-01","2021-04-01","2021-07-01")),labels=as.character(c("Jul (2020)","Oct","Jan (2021)","Apr","Jul")), las=1)

# Plot the data from the first study period
df_tot_sampled_sp <- df_BMJ %>% filter(date >= "2020-06-01" & date <= "2020-11-01")
df_tot_sampled_sp_gr <- df_tot_sampled_sp %>%
  group_by(date) %>% summarise(deaths = length(N1_CT_1),
                                        CT_45_Either = sum(Either_Test_Pos, na.rm=T),
                                        CT_45_Both = sum(Both_Tests_Pos, na.rm=T),
                                        CT_40_Either = sum(Either_Test_Pos_40, na.rm=T),
                                        CT_40_Both = sum(Both_Tests_Pos_40, na.rm=T)) %>%
  mutate(Date_Limits = case_when(date < "2020-06-15" | date > "2020-10-01" ~ "red",
                                 TRUE ~ "black"
                                 ))

plot(x = df_tot_sampled_sp_gr$date, y = df_tot_sampled_sp_gr$deaths, pch = 20, ylim = c(0,15), xlab = "Date", ylab = "Deaths", col = df_tot_sampled_sp_gr$Date_Limits, xaxt = "n", main = "Sample Period 2020")
text(x = as.Date(c("2020-09-20","2020-09-20")), y = c(15,14), c(paste("n =",sum(df_tot_sampled_sp_gr$deaths)), paste("n =",sum(df_tot_sampled_sp_gr$deaths)-3)), col = c("darkred",1))
abline(v = as.Date("2020-06-15"), lty = 2)
abline(v = as.Date("2020-10-01"), lty = 2)
axis(1, at=as.Date(c("2020-06-15","2020-07-01","2020-08-01","2020-09-01","2020-10-01")),labels=as.character(c("Jun 15","Jul 1","Aug 1","Sep 1","Oct 1")), las=1)

# Plot all deaths with positive CT
plot(x = df_tot_sampled_sp_gr$date, y = df_tot_sampled_sp_gr$CT_45_Either, pch = 20, ylim = c(0,6), xlab = "Date", ylab = "Deaths", col = "blue", xaxt = "n", main = "CT+ve (<45)")
points(x = df_tot_sampled_sp_gr$date, y = df_tot_sampled_sp_gr$CT_45_Both, pch = 20, col = df_tot_sampled_sp_gr$Date_Limits, xaxt = "n")
text(x = as.Date(c("2020-09-20")), y = c(6), c(paste("n =",sum(df_tot_sampled_sp_gr$CT_45_Either))), col = c("darkblue"))
text(x = as.Date(c("2020-09-20")), y = c(5.5), c(paste("n =",sum(df_tot_sampled_sp_gr$CT_45_Both))), col = c(1))
abline(v = as.Date("2020-06-15"), lty = 2)
abline(v = as.Date("2020-10-01"), lty = 2)
axis(1, at=as.Date(c("2020-06-15","2020-07-01","2020-08-01","2020-09-01","2020-10-01")),labels=as.character(c("Jun 15","Jul 1","Aug 1","Sep 1","Oct 1")), las=2)

# Plot deaths with CT < 40
plot(x = df_tot_sampled_sp_gr$date, y = df_tot_sampled_sp_gr$CT_40_Either, pch = 20, ylim = c(0,6), xlab = "Date", ylab = "Deaths", col = "blue", xaxt = "n", main = "CT < 40")
points(x = df_tot_sampled_sp_gr$date, y = df_tot_sampled_sp_gr$CT_40_Both, pch = 20, col = df_tot_sampled_sp_gr$Date_Limits, xaxt = "n")
text(x = as.Date(c("2020-09-20")), y = c(6), c(paste("n =",sum(df_tot_sampled_sp_gr$CT_40_Either))), col = c("darkblue"))
text(x = as.Date(c("2020-09-20")), y = c(5.5), c(paste("n =",sum(df_tot_sampled_sp_gr$CT_40_Both))), col = c(1))
abline(v = as.Date("2020-06-15"), lty = 2)
abline(v = as.Date("2020-10-01"), lty = 2)
axis(1, at=as.Date(c("2020-06-15","2020-07-01","2020-08-01","2020-09-01","2020-10-01")),labels=as.character(c("Jun 15","Jul 1","Aug 1","Sep 1","Oct 1")), las=2)


df_tot_sampled_sp_tf <- df_tot_sampled_sp_gr %>% mutate(TimeFrame = cut.Date(date, breaks = as.Date(c("2020-06-01","2020-06-15","2020-06-29","2020-07-13","2020-07-27","2020-08-10","2020-08-24","2020-09-07","2020-09-21","2020-10-05")), labels = 0:8)) %>%
  group_by(TimeFrame) %>% summarise(deaths = sum(deaths),
                                    CT_Either = sum(CT_45_Either),
                                    CT_Both = sum(CT_45_Both),
                                    CT_Either_40 = sum(CT_40_Either),
                                    CT_Both_40 = sum(CT_40_Both))

BMJdata <- readRDS("analysis/data/Code-generated-data/00_05_BMJ_Data.rds")

sum(df_tot_sampled_sp_tf$deaths)
sum(BMJdata$TotSamDeaths)

sum(df_tot_sampled_sp_tf$CT_Either)
sum(BMJdata$CovSamDeaths)

sum(df_tot_sampled_sp_tf$CT_Either_40)
sum(BMJdata$CovSamDeaths_Strict)


library(reshape2)

df_tot_sampled_sp_tf_reshaped <- df_tot_sampled_sp_tf %>%
  mutate(d_CT_40 = CT_Either_40, d_CT_40_45 = CT_Either - d_CT_40, d_CT_45 = deaths - d_CT_40_45 - d_CT_40,
         Total = deaths) %>%
  select(TimeFrame, d_CT_40_45, d_CT_40, d_CT_45, Total) %>%
  melt(id.vars = c("TimeFrame","Total"), value.name = "Deaths", variable.name = c("CT"))

NegDeaths_BMJ <- data.frame(TimeFrame = as.character(c(0:8)),
                            r_CT_40_45 = c(0,BMJdata$CovSamDeaths - BMJdata$CovSamDeaths_Strict),
                            r_CT_40 = c(0,BMJdata$CovSamDeaths_Strict),
                            r_CT_45 = c(0,BMJdata$TotSamDeaths - BMJdata$CovSamDeaths),
                            Total = c(0,BMJdata$TotSamDeaths)) %>%
  melt(id.vars = c("TimeFrame","Total"), value.name = "Deaths", variable.name = c("CT"))

ggplot(df_tot_sampled_sp_tf_reshaped, aes(fill=CT, y=Deaths, x=as.character(TimeFrame))) +
  geom_bar(position="stack", stat="identity",width = 0.5) +
  scale_fill_manual(values=c("orange","purple","deeppink3","orange3","purple4","deeppink4"), labels = c("CT<40","40<=CT<45", "CT>45","D: CT<40", "D: 40<=CT<45", "D: CT>45")) +
  labs(fill = "CT results (inc.)") +
  xlab("Timeframe") +

  geom_bar(data = NegDeaths_BMJ, aes(x = as.numeric(TimeFrame)+1, y = Deaths, fill = CT), position = "stack", stat = "identity")


## start by plotting just their graph:
ggplot(NegDeaths_BMJ, aes(fill=CT, y=Deaths, x=as.character(TimeFrame))) +
  geom_bar(position="stack", stat="identity",width = 0.8) +
  scale_fill_manual(values=c("orange3","purple4","deeppink4","orange","purple","deeppink3"), labels = c("D: CT<40","D: 40<=CT<45", "D: CT>45","CT<40", "40<=CT<45", "CT>45")) +
  labs(fill = "CT results") +
  xlab("Timeframe") +
  geom_bar(data = df_tot_sampled_sp_tf_reshaped, aes(x = as.numeric(TimeFrame), y = Deaths, fill = CT, width = 0.5), position = "stack", stat = "identity")

ggplot(NegDeaths_BMJ, aes(fill=CT, y=Deaths/Total, x=as.character(TimeFrame))) +
  geom_bar(position="stack", stat="identity",width = 0.8) +
  scale_fill_manual(values=c("orange3","purple4","deeppink4","orange","purple","deeppink3"), labels = c("D: CT<40","D: 40<=CT<45", "D: CT>45","CT<40", "40<=CT<45", "CT>45")) +
  labs(fill = "CT results") +
  xlab("Timeframe") +
  geom_bar(data = df_tot_sampled_sp_tf_reshaped, aes(x = as.numeric(TimeFrame), y = Deaths/Total, fill = CT, width = 0.5), position = "stack", stat = "identity")

ggplot(NegDeaths_BMJ, aes(fill=CT, y=Deaths, x=as.character(TimeFrame))) +
  geom_bar(position="stack", stat="identity",width = 0.8) +
  scale_fill_manual(values=c("orange3","purple4","deeppink4","orange","purple","deeppink3"), labels = c("D: CT<40","D: 40<=CT<45", "D: CT>45","CT<40", "40<=CT<45", "CT>45")) +
  labs(fill = "CT results") +
  xlab("Timeframe") +
  geom_bar(data = df_tot_sampled_sp_tf_reshaped, aes(x = as.numeric(TimeFrame), y = Deaths, fill = CT, width = 0.5), position = "stack", stat = "identity")


ggplot(NegDeaths_BMJ, aes(fill=CT, y=Deaths, x=TimeFrame, fill = c("CT<45","40<=CT<45","CT>45"))) +
  geom_bar(position="stack", stat="identity") +
  labs(color = "Test Result") +
  scale_fill_manual("CT results", values = c("CT<40" = "orange", "40<=CT<45" = "purple","CT>45" = "deeppink3", "D: CT<40" = "orange3", "D: 40<=CT<45" = "purple4", "D: CT>45" = "deeppink4")) +
  # scale_fill_manual(values=c("orange","purple","deeppink3","orange3","purple4","deeppink4"), labels = c("CT<40","40<=CT<45", "CT>45","D: CT<40", "D: 40<=CT<45", "D: CT>45")) +
  # labs(fill = "CT results") +
  xlab("Timeframe") +

  geom_bar(data = df_tot_sampled_sp_tf_reshaped, aes(x = as.numeric(TimeFrame), y = Deaths, fill = c("D: CT<45","D: 40<=CT<45","D: CT>45")), position = "stack", stat = "identity", width = 0.5)


BMJdata$TotSamDeaths


table(df_tot_sampled_sp[,c("Test1_45","Test2_45")],useNA = "always") # This makes 62, + 2 deaths = 64/364, 6 short of 70
table(df_tot_sampled_sp[,c("Test1_40","Test2_40")],useNA = "always") # This makes 54, + 2 deaths = 56/364, 2 short of 58



### Let's imagine that this dataset is correct.
# Let's combine the two datasets:
Off_data <- readRDS(file = "analysis/data/Code-generated-data/00_01_Lusaka_Prov_Deaths.rds")
plot(Off_data$date, Off_data$deaths, pch = 20, xlab = "Date", ylab = "Deaths")
points(df_tot_sampled_sp_gr$deceased_date, df_tot_sampled_sp_gr$CT_45_Either,col = 2)
legend("topleft", legend = c("Official Data", "BMJ Data"), pch = c(20,1), col = c(1,2))

Off_data <- Off_data %>% filter(date < "2020-06-15")
Combined_Data <- rbind(Off_data,data.frame(date = df_tot_sampled_sp_gr$deceased_date, deaths = df_tot_sampled_sp_gr$CT_45_Either))
plot(Combined_Data$date, Combined_Data$deaths, pch = 20, xlab = "Date", ylab = "Deaths")
abline(v = as.Date("2020-06-15"), lty = 2)

