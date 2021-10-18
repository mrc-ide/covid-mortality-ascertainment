## Forward projections from the end of the modelled epidemic with different Rt values
rm(list = ls())
devtools::load_all(".")
devtools::load_all("../../Squire/squire/squire.Rproj")
library(tidyverse)
library(reshape2)
# library(squire)

## Run model fitting process using new IFR, BMJ data
BMJ_data_HighEst <- readRDS("analysis/data/Code-generated-data/00_05c_BMJ_Data_DailyEsts_HighEsts.rds") %>%
  select(date, TrueCovDeathsLus_HighEst) %>%
  rename(deaths = TrueCovDeathsLus_HighEst)
pop_st_lu <- readRDS("analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds")
Weighted_Durs_Hosp <- readRDS("analysis/data/Code-generated-data/00_04_Weighted_durations_death_survive.rds")
IFR_vals <- readRDS("analysis/data/Code-generated-data/IFR_mat_Ints2.rds") %>%
  filter(IFR_x == 1.2, Slope_x == 0.8)
Age_groups <- seq(2.5,82.5, by = 5)
IFR_Age_var_slope_int <- exp(Age_groups * IFR_vals$Slope_abs + IFR_vals$Int_abs)
prob_death_tot <- IFR_Age_var_slope_int/(100*parameters_explicit_SEEIR("Zambia")$prob_hosp)

# Sys.getenv("SQUIRE_PARALLEL_DEBUG") == "TRUE"
# Sys.setenv(SQUIRE_PARALLEL_DEBUG = "")
# Sys.setenv()
High_IFR_Low_Slope_for_Projection <- fit_spline_rt(data = BMJ_data_HighEst,
                                                   country = "Zambia",
                                                   population = pop_st_lu,
                                                   reporting_fraction = 1,
                                                   n_mcmc = 10000,
                                                   replicates = 100,
                                                   rw_duration = 14,
                                                   hosp_beds = 1e10,
                                                   icu_beds = 1e10,
                                                   prob_severe = rep(0,17),
                                                   prob_non_severe_death_treatment = prob_death_tot,
                                                   dur_get_ox_survive =Weighted_Durs_Hosp$Surv_Dur_Weighted,
                                                   dur_get_ox_die =Weighted_Durs_Hosp$Death_Dur_Weighted,
                                                   dur_R = Inf
)

# saveRDS(High_IFR_Low_Slope_for_Projection, "../Bonus Files/2021-09-28-BMJ_1.2IFR_0.8Slope.rds")

out <- High_IFR_Low_Slope_for_Projection
tt <- squire:::intervention_dates_for_odin(dates = out$interventions$date_R0_change,
                                           change = out$interventions$R0_change,
                                           start_date = out$replicate_parameters$start_date[1],
                                           steps_per_day = 1/out$parameters$dt)

Rt_vec <-sapply(1:nrow(out$replicate_parameters), function(y){
  Rt <- squire:::evaluate_Rt_pmcmc(
    R0_change = tt$change,
    date_R0_change = tt$dates,
    R0 = out$replicate_parameters$R0[y],
    pars = as.list(out$replicate_parameters[y,]),
    Rt_args = out$pmcmc_results$inputs$Rt_args)
})

## So Meff = 0
## R0_change = c(1,1,...,1)
## Meff_pl = 0
## mob_up = c(0,0,...,0)
## Rt_pl_change = c(0,0,0...)
## Rt_rw_change = c(0,0,0...)

# Rt <- R0 * 2*(plogis( -Meff * -(R0_change-1) - Meff_pl*(mob_up) - Rt_pl_change - Rt_rw_change))
# Rt <- R0 * 2*(plogis( -Meff * -(R0_change-1) - Meff_pl*(mob_up) - Rt_pl_change - Rt_rw_change))


## Using these conditions to start, project forwards
## Take the output out of the pmcmc function and pass that to projections
## with either proportional changes to Rt or changes to specific Rt values


# High_IFR_Low_Slope_for_Projection$output
# projections(r = High_IFR_Low_Slope_for_Projection)
# p <- projections(r = High_IFR_Low_Slope_for_Projection, time_period = 100)
# plot(p, var_select = "ICase")

## Initial R0
# High_IFR_Low_Slope_for_Projection$replicate_parameters$R0

## Final Rt - this is negative... so...
# High_IFR_Low_Slope_for_Projection$replicate_parameters$Rt_rw_10

## In the middle:
# Middle_Rt <- mean(c(High_IFR_Low_Slope_for_Projection$replicate_parameters$R0, High_IFR_Low_Slope_for_Projection$replicate_parameters$Rt_rw_10))
Middle_Rt <- mean(c(mean(Rt_vec[nrow(Rt_vec),]), mean(High_IFR_Low_Slope_for_Projection$replicate_parameters$R0)))

## Initial R0:
Start_Rt <- mean(High_IFR_Low_Slope_for_Projection$replicate_parameters$R0)

p1 <- projections(r = High_IFR_Low_Slope_for_Projection, time_period = 100, R0 = round(Middle_Rt,2))
# plot(p, var_select = "ICase")

p2 <- projections(r = High_IFR_Low_Slope_for_Projection, time_period = 100, R0 = round(Start_Rt,2))
# plot(p, var_select = "ICase")

p3 <- projections(r = High_IFR_Low_Slope_for_Projection, time_period = 100, R0 = 7)
# plot(p, var_select = "ICase")


ggproj <- projection_plotting(r_list = list(p1,p2,p3,p),
                              scenarios = c("Rt = 2.23 (Mid-range)","Rt = 4.04 (Rt at model start)","Rt = 7.00 (Delta est.)","Rt = 0.42 (Rt at model end)"),
                              var_select = c("deaths"),
                              add_parms_to_scenarios = FALSE,ci = FALSE,summarise = TRUE,
                              x_var = "date",date_0 = "2020-10-01") +
  scale_color_viridis_d(begin = 0.2, end = 0.8, guide = guide_legend(reverse = TRUE), name ="Rt from 28th Sept 2020") +
  geom_point(data = out$pmcmc_results$inputs$data, aes(x = as.Date(date), y = deaths), alpha = 0.5, size = 0.5) +

  # theme(legend.title = "Rt from 28th October 2020") +
  ylab(label = "Deaths") + xlab(label = "Date") +
  guides(linetype = "none") + theme(legend.position = "none")

Rt_Data <-data.frame(Dates = as.Date(rownames(High_IFR_Low_Slope_for_Projection$output)[-1]), Rt = rowMeans(Rt_vec)) %>%
  mutate(Rt_1 = Rt, Rt_2 = Rt, Rt_3 = Rt) %>%
  add_row(Dates = as.Date(tail(.$Dates,1))+1:100,
          Rt = rep(tail(.$Rt,1),100),
          Rt_1 = rep(Middle_Rt,100),
          Rt_2 = rep(Start_Rt,100),
          Rt_3 = rep(7,100)) %>%
  melt(id = "Dates", value.name = "Rt")


pdf(file = "analysis/figures/14_01_Forward_Projections.pdf", width = 10, height = 3.5)
cowplot::plot_grid(
  ggproj,

  ggplot(data = Rt_Data, aes(x = Dates, y = Rt, group = variable)) + geom_line(aes(col = variable)) +
    scale_color_viridis_d(begin = 0.2, end = 0.8) +
    ggplot2::theme_bw() +
    theme(legend.position = "none") +
    xlab(label = "Date"),

  cowplot::get_legend(ggproj + theme_light() + labs(color='') + theme(legend.box.margin = margin(0,0,30,0))),
  nrow = 1, rel_widths = c(4, 4, 2.2))
dev.off()
