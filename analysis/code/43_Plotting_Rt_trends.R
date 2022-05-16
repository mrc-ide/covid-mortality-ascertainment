# Plotting Rt trends.# Plot Rt
library(tidyverse)
ModRes <- readRDS("../Bonus Files/2022-04-27_Model_Fit_10_pois_bin_bin_int_over.rds")$X41
ModRes$output
ModRes$parameters
ModRes$replicate_parameters$R0

ModRes$replicate_parameters[,c("R0", paste0("Rt_rw_",1:10))]

squire:::evaluate_Rt_pmcmc()


## These need to be converted into actual R0 values.


# plot(c(mean(Mod_Res5$X28$replicate_parameters$R0),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_1),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_2),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_3),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_4),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_5),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_6),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_7),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_8),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_9),
#        mean(Mod_Res5$X28$replicate_parameters$R0+Mod_Res5$X28$replicate_parameters$Rt_rw_10)))

squire:::evaluate_Rt_pmcmc()

Rt_vals_Reps <- lapply(seq_along(ModRes$replicate_parameters$R0), function(y) {

  tt <- squire:::intervention_dates_for_odin(dates = ModRes$interventions$date_R0_change,
                                             change = ModRes$interventions$R0_change,
                                             start_date = ModRes$replicate_parameters$start_date[y],
                                             steps_per_day = 1/ModRes$parameters$dt)

  Rt <- squire:::evaluate_Rt_pmcmc(
    R0_change = tt$change,
    date_R0_change = tt$dates,
    R0 = ModRes$replicate_parameters$R0[y],
    pars = as.list(ModRes$replicate_parameters[y,]),
    Rt_args = ModRes$pmcmc_results$inputs$Rt_args)

  data.frame(Date = as.Date(tt$dates), Rt) %>% filter(Date>="2020-06-15") %>%
    pull(Rt)

})
names(Rt_vals_Reps) <- paste0("Rep_",1:100)
Rt_vals_Reps_df <- cbind(date = seq.Date(from = as.Date("2020-06-15"), by = 1, length.out = length(Rt_vals_Reps[[1]])), do.call(cbind.data.frame, Rt_vals_Reps)) %>% pivot_longer(cols = starts_with("Rep_"), names_to = "Replicate", values_to = "Rt")

Rt_vals_Reps_df_summ <- Rt_vals_Reps_df %>% group_by(date) %>%
  summarise(Rt_mean = mean(Rt),
            Rt_median = median(Rt),
            Confint_lower = confintr::ci_mean(Rt)$interval[1],
            Confint_higher = confintr::ci_mean(Rt)$interval[2])

confintr::ci_mean(Rt_vals_Reps_df %>% filter(date=="2020-06-15") %>% pull(Rt))

mean_test <- Rt_vals_Reps_df %>% filter(date=="2020-06-15") %>% pull(Rt) %>% mean()
mean_test <- Rt_vals_Reps_df %>% filter(date=="2020-07-25") %>% pull(Rt) %>% mean()
mean_test <- Rt_vals_Reps_df %>% filter(date=="2020-10-01") %>% pull(Rt) %>% mean()

sd_test <- Rt_vals_Reps_df %>% filter(date=="2020-06-15") %>% pull(Rt) %>% sd()
sd_test <- Rt_vals_Reps_df %>% filter(date=="2020-07-25") %>% pull(Rt) %>% sd()
sd_test <- Rt_vals_Reps_df %>% filter(date=="2020-10-01") %>% pull(Rt) %>% sd()

margin_test <- qt(0.975,df=99)*sd_test/sqrt(100)
mean_test + margin_test
mean_test - margin_test

p1 <- ggplot(Rt_vals_Reps_df, aes(x = date, y = Rt, group = Replicate)) + geom_line() +
  ylim(0.5,2.5) +
  theme_minimal()

p2 <- ggplot(Rt_vals_Reps_df_summ, aes(x = date, y = Rt_mean)) + geom_line() +
  geom_ribbon(aes(ymin = Confint_lower, ymax = Confint_higher), alpha = 0.4) +
  ylim(0.5,2.5) +
  theme_minimal() +
  xlab("Date") + ylab("Rt")

pdf("analysis/figures/43_Rt_through_time_1x1.pdf")
p2
dev.off()

tiff("analysis/figures/43_Rt_through_time_1x1.tiff", res = 300, units = "in", width = 7, height = 7)
p2
dev.off()

cowplot::plot_grid(p1,p2)

hist(Rt_vals_Reps_df %>% filter(date=="2020-06-15") %>% pull(Rt))
abline(v = c(confintr::ci_mean(Rt_vals_Reps_df %>% filter(date=="2020-06-15") %>% pull(Rt))$interval))

Test_df <- rt(n = 10, df = 99)
hist(Test_df)
abline(v = confintr::ci_mean(Test_df)$interval)

plot(x = seq(-3,3,0.01), y =dt(x = seq(-3,3,0.01), df = 99))
abline(v = qt(p = c(0.025, 0.975), df = 99))

confintr::ci_mean(Rt_vals_Reps_df %>% filter(date=="2020-06-15") %>% pull(Rt), probs = c(0.1,0.9))

plot(Rt_vals_Reps[,1], type = "l")
plot(Rt_vals_Reps[,2])

Mod_Res5$X28$replicate_parameters[2,]

## I want to plot the first replicate along date
Res_Rt <- as.data.frame(do.call(cbind, Rt_vals_Reps)) %>%
  rowwise() %>%
  mutate(mean = mean(c_across(where(is.numeric))),
         min = min(c_across(where(is.numeric))),
         max = max(c_across(where(is.numeric)))) %>%
  select(mean,max,min) %>%
  cbind(date = seq.Date(from = as.Date("2020-06-15"), to = as.Date("2020-10-02"), by = 1),.)

ggplot(data = Res_Rt, aes(x = date)) +
  geom_line(aes(y = mean)) +
  geom_ribbon(aes(ymin=min, ymax=max), alpha=0.3) +
  xlab("Date") + ylab("Rt")


plot(data.frame(Date = seq.Date(from = as.Date("2020-06-15"), to = as.Date("2020-10-02"), by = 1),rowMeans(do.call(cbind, Rt_vals_Reps))), type = "l")


Rt_vals_Reps[[1]]

plot(Rt_vals_Reps[[2]], type = "l")
