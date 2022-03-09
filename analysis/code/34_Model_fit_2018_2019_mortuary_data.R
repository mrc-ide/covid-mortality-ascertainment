rm(list = ls())
devtools::install_github("mrc-ide/drjacoby")
library(drjacoby)

## Get my data
UTH_Mortality_Total <- read.csv(file = "analysis/data/raw/BMJ_UTH_excess_mortality/mortuary_records.csv")
monthly_deaths_list <- UTH_Mortality_Total %>%
  mutate(date = as.Date(dod, "%m/%d/%y")) %>%
  filter(dod != ".") %>%
  filter(date >= "2018-01-01",date < "2020-01-01", age_years !=".") %>%
  mutate(Age_group = cut(as.numeric(age_years), c(seq(0,80,by = 5),Inf), right = F, labels = F),
         year = format(date,"%Y"),
         month = format(date,"%m")) %>%
  group_by(Age_group, year, month) %>%
  summarise(total_deaths = length(date)) %>%
  spread(key = Age_group, value = total_deaths) %>%
  ungroup() %>% select(-year, -month) %>%
  setNames(paste0("age_cat",1:17)) %>%
  as.list()

num_age_cat=17
num_months=24

# define parameters for each of the age_rates
df_params <- define_params(name = paste0("age_rate_est",1:num_age_cat),
                           min =rep(0,num_age_cat), max = rep(100000,num_age_cat))




# define log-likelihood function
r_loglike <- function(params, data, misc) {

  # find relavent parameters and age category variables
  rates <- as.numeric(params)
  age_cats<-grep("age_cat",names(data))
  ## sum over age cats and months
  ret<-0
  for(i in 1:length(age_cats)){
    ret<-ret+sum(dpois(unlist(data[age_cats[i]]),lambda=rates[i],log=TRUE))
  }
  # return
  return(ret)
}

## define params (yet to do currently effectively uniform between 0 and 100000 as we've set those limits for the params)
r_logprior <- function(params, misc) {

  # extract parameter values
  # mu <- params["mu"]
  #sigma <- params["sigma"]

  # calculate log-prior
  ret <- 0

  # return
  return(ret)
}

mcmc <- run_mcmc(data = monthly_deaths_list,
                 df_params = df_params,
                 loglike = r_loglike,
                 logprior = r_logprior,
                 burnin = 1e3,
                 samples = 1e3,
                 pb_markdown = TRUE)

# plot_par(mcmc)

# For each of those, I need to sample 100
Samples_age_ests <- apply(mcmc$output[mcmc$output$phase=="sampling",paste0("age_rate_est",1:17)],
      MARGIN = 2,
      function(x){
        sample(x = x, size = 100)
      })

# saveRDS(Samples_age_ests, "analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")
Samples_age_ests <- readRDS("analysis/data/Code-generated-data/34_01_Samples_age_ests.rds")


# Tests_shorter_duration[[1]]$output

Tests_shorter_duration <- readRDS("../Bonus Files/2022-03-03_Testing_pois_bin_bin_x3.rds")

Tests_shorter_duration$X1$pmcmc_results$chains$chain1$results$log_posterior
Tests_shorter_duration$X33$pmcmc_results$chains$chain1$results$log_posterior
Tests_shorter_duration$X37$pmcmc_results$chains$chain1$results$log_posterior

Tests_shorter_duration$X33$pmcmc_results$inputs

index <- squire:::odin_index(Tests_shorter_duration[[1]]$model)
Days_for_comparison <-c(seq.Date(from = as.Date("2020-06-14"),
                                 to = as.Date("2020-10-02"),
                                 by = "week"),as.Date("2020-10-02"))

readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data.rds")
Mort_Deaths_Age_Week <- readRDS("analysis/data/Code-generated-data/00_07_Mortuary_data_age.rds") %>%
  mutate(week = cut.Date(date, breaks = "1 week", labels = FALSE)) %>%
  group_by(week, Age_group) %>%
  summarise(total_deaths = sum(total_deaths),
            date = head(date,1))


# Graphs <- lapply(list(Test), function(fit){
Graphs <- lapply(Testx3, function(fit){
# Graphs <- lapply(Tests_shorter_duration, function(fit){
  # browser()
  Mod_Age_Deaths_Lus <- apply(fit$output,3,function(x){

    AgeRes <- lapply(1:ncol(x[,index$S]),function(Age_Group){
      Ds_Lus <- diff(x[as.Date(rownames(x)) %in% Days_for_comparison, index$D][,Age_Group],na.rm = T)
      Ds_Mor <- Ds_Lus*0.8
      ll <- dpois(x = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_group==Age_Group,]$total_deaths, lambda = Ds_Mor + mean(Samples_age_ests[,Age_Group]*12/52), log = T)

      return(data.frame(Week_gr = 1:16,
                        Age_group = Age_Group,
                        Ds_Lus = Ds_Lus,
                        Ds_Mor = Ds_Mor,
                        Ds_Tot_Mor = Ds_Mor + mean(Samples_age_ests[,Age_Group]*12/52),
                        total_deaths = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_group==Age_Group,]$total_deaths,
                        ll = ll))
    })

    AgeRes <- do.call(rbind.data.frame, AgeRes)
    AgeRes
  })

  Mod_Age_Deaths_Lus_Av <- Mod_Age_Deaths_Lus %>%
    str2str::ld2a() %>%
    cbind(.[,c("Age_group","Week_gr","total_deaths"),1],
          data.frame(Mean = apply(.[,c("Ds_Lus","Ds_Mor","Ds_Tot_Mor","ll"),], MARGIN = 2, FUN = function(x){rowMeans(x)}),
                     Max = apply(.[,c("Ds_Lus","Ds_Mor","Ds_Tot_Mor","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = max)}),
                     Min = apply(.[,c("Ds_Lus","Ds_Mor","Ds_Tot_Mor","ll"),], MARGIN = 2, FUN = function(x){apply(x, MARGIN = 1, FUN = min)}))) %>%
    select(Age_group,Week_gr,total_deaths,
           Mean.Ds_Lus, Max.Ds_Lus, Min.Ds_Lus,
           Mean.Ds_Mor, Max.Ds_Mor, Min.Ds_Mor,
           Mean.Ds_Tot_Mor, Max.Ds_Tot_Mor, Min.Ds_Tot_Mor,
           Mean.ll, Max.ll, Min.ll) %>%
    arrange(Age_group, Week_gr) %>%
    filter(Week_gr != 4)
  rownames(Mod_Age_Deaths_Lus_Av) <- NULL

  ## Plot by week
  Mod_Age_Deaths_Lus_Av_Week <- merge(Mod_Age_Deaths_Lus_Av, Mort_Deaths_Age_Week[,c("week","Age_group","date")] %>% rename(Week_gr = week)) %>% group_by(Week_gr) %>%
    summarise(date = head(date,1)+3,#Week_gr = head(Week_gr,1),
              total_deaths = sum(total_deaths),
              Mean.Ds_Tot_Mor = sum(Mean.Ds_Tot_Mor),
              Max.Ds_Tot_Mor = sum(Max.Ds_Tot_Mor),
              Min.Ds_Tot_Mor = sum(Min.Ds_Tot_Mor)
              # Could add pcr, but this would need to be weighted by population size in each age group?
    )
# browser()
  p1 <- ggplot(Mod_Age_Deaths_Lus_Av_Week, aes(x = date)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Ds_Tot_Mor),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Tot_Mor, ymax=Max.Ds_Tot_Mor), alpha=0.3) +
    # Modelled True deaths
    # geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    # geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = total_deaths)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Date") + ylab("Deaths") +
    xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle(paste("COVID-19 weekly deaths\n(Mortuary) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
    theme(plot.title = element_text(size = 10))

  Age_groups.labs <- Mod_Age_Deaths_Lus_Av %>% group_by(Age_group) %>%
    summarise(Mean.ll = sum(Mean.ll)) %>%
    mutate(lab = paste0("pois ll AG ",Age_group,": ", round(Mean.ll,1))) %>%
    select(lab) %>%
    unlist()
  #
  names(Age_groups.labs) <- 1:17


  p2 <- ggplot(merge(Mod_Age_Deaths_Lus_Av, Mort_Deaths_Age_Week[,c("week","Age_group","date")] %>% rename(Week_gr = week)), aes(x = date)) +
    # Modelled Positive deaths
    geom_line(aes(y=Mean.Ds_Tot_Mor),linetype="dashed") +
    geom_ribbon(aes(ymin=Min.Ds_Tot_Mor, ymax=Max.Ds_Tot_Mor), alpha=0.3) +
    facet_wrap(vars(Age_group), labeller = labeller(Age_group = Age_groups.labs)) +
    # Modelled True deaths
    # geom_line(aes(y=Mean.Ds_Lus),linetype="dashed") +
    # geom_ribbon(aes(ymin=Min.Ds_Lus, ymax=Max.Ds_Lus), alpha=0.3, fill = "red") +
    # Scaled positive deaths
    geom_point(aes(y = total_deaths)) +
    # Scaled True deaths
    # geom_point(data = WeekData, aes(y = TrueDeathsLus), col = "brown", shape = 1) +
    xlab("Week") + ylab("Deaths") +
    # xlim(as.Date("2020-04-15"),as.Date("2020-10-01")) +
    ggtitle(paste("COVID-19 weekly deaths\n(Lusaka) \npois ll = ", round(sum(Mod_Age_Deaths_Lus_Av$Mean.ll),1))) +
    theme(plot.title = element_text(size = 10))

  return(list(p1,p2))
  })




cowplot::plot_grid(Graphs$X1[[1]],Graphs$X33[[1]],Graphs$X37[[1]])

pdf(file = "analysis/figures/34_02_Age_Week_Deaths_Test.pdf", width = 30, height = 10)
cowplot::plot_grid(Graphs$X1[[2]] + xlab("0.2x0.2"),
                   Graphs$X33[[2]] + xlab("0.2x1"),
                   Graphs$X37[[2]] + xlab("1x1"), nrow = 1)
dev.off()


Mod_Age_Deaths_Lus <- apply(Tests_shorter_duration[[3]]$output,3,function(x){

  AgeRes <- lapply(1:ncol(x[,index$S]),function(Age_Group){
    Ds_Lus <- diff(x[as.Date(rownames(x)) %in% Days_for_comparison, index$D][,Age_Group],na.rm = T)
    Ds_Mor <- Ds_Lus*0.8
    ll <- dpois(x = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_group==Age_Group,]$total_deaths, lambda = Ds_Mor + mean(Samples_age_ests[,Age_Group]*12/52), log = T)

    return(data.frame(Week_gr = 1:16,
                      Age_group = Age_Group,
                      Ds_Lus = Ds_Lus,
                      Ds_Mor = Ds_Mor,
                      Ds_Tot_Mor = Ds_Mor + mean(Samples_age_ests[,Age_Group]*12/52),
                      total_deaths = Mort_Deaths_Age_Week[Mort_Deaths_Age_Week$Age_group==Age_Group,]$total_deaths,
                      ll = ll))
  })

  AgeRes <- do.call(rbind.data.frame, AgeRes)
  AgeRes
})


