Comb_data <- readRDS("analysis/data/Code-generated-data/00_13_Combined_mortuary_postmortem_data_complete_weeks_only.rds")


Comb_data
library(tidyr)
library(dplyr)
library(reshape2)

Test <- lapply(1:nrow(Comb_data), function(x){
  browser()
  Test_res <- data.frame(Pos_Deaths_Sample = 10,
             Pos_Deaths_Mort = 0:Comb_data$total_deaths[x],
             Neg_Deaths_Mort = Comb_data$total_deaths[x]:0,
             Samples = Comb_data$Samples[x]) %>%

    mutate(Pos_Deaths_Lus = Pos_Deaths_Mort / 0.8,
           hg_l = dhyper(x = Comb_data$PosTestNum[x], m = 0:Comb_data$total_deaths[x], n = Comb_data$total_deaths[x]:0, k = Comb_data$Samples[x]),
           weights = hg_l/sum(hg_l),
           Mod_Pos_Deaths_Lus = 50,
           nb_l = dnbinom(x = round(Pos_Deaths_Lus), size = 7, mu = Mod_Pos_Deaths_Lus),
           ## Loop through
           wtd_nb_ll = weights*nb_l)
  # browser()
  log(sum(Test_res$wtd_nb_ll))
})

sum(unlist(Test))

head(Test[[2]], 30)
tail(Test[[2]], 30)

## Precalculate weights to give as inputs
#

sum(Test[[1]]$weights)

plot(Test[[2]]$hg_l)

sum(Test[[1]]$wtd_nb_ll)
sum(unlist(lapply(Test, function(x){sum(x[,"wtd_nb_ll"])})))

dnbinom(x = 0.5, size = 2, prob = 0.6)


# hg_l = dhyper(x = Comb_data$PosTestNum[x], m = 0:Comb_data$total_deaths[x], n = Comb_data$total_deaths[x]:0, k = Comb_data$Samples[x]),
