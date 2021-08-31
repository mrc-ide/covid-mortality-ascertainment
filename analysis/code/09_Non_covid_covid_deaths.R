rm(list = ls())
### Non covid covid deaths
devtools::load_all(".")
library(tidyverse)

# Pop Size
pop_lu_prov <- sum(readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds"))

# q_1: proportion that are covid +ve and dead
# Get this from the BMJ data
q1 <- readRDS("analysis/data/Code-generated-data/00_05_BMJ_Data.rds")$q1_10x
q1_b <- readRDS("analysis/data/Code-generated-data/00_05_BMJ_Data.rds")$q1_High_Est

# P_pos: expected prevalence of covid.
# Get this from estimated covid deaths:
fit <- readRDS(file = "analysis/results/p_01_Official_Data_Lancet_est_rf_resonable.rds")
sero_pcr <- seroprev_df(fit)
p_pos <- sero_pcr %>% filter(date>"2020-06-08" & date<"2020-09-28") %>%
  group_by(date) %>% summarise(pcr_perc_av = mean(pcr_perc)) %>%
  mutate(TimeFrame = cut.Date(as.Date(date), breaks = as.Date(c("2020-06-08","2020-06-22","2020-07-06","2020-07-20","2020-08-03","2020-08-17","2020-08-31","2020-09-14","2020-09-28")), labels = 1:8, start.on.monday = T)) %>%
  group_by(TimeFrame) %>% summarise(PCR = mean(pcr_perc_av)) %>%
  select(PCR)


# P_bg: expected prevalence of background death
# Crude mortality 2019, per year
p_bg <-14*readRDS(file = "analysis/data/Code-generated-data/00_06_Crude_Mortality.rds") # per fortnight

r_maf <- (q1/p_pos - p_bg)/(1-p_bg)
pop_lu_prov*r_maf*p_pos

r_maf_b <- (q1_b/p_pos - p_bg)/(1-p_bg)
pop_lu_prov*r_maf_b*p_pos
