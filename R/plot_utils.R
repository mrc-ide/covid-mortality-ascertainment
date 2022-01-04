#' @noRd
# save figure
save_figs <- function(name,
                      fig,
                      width = 6,
                      height = 6,
                      root = file.path(here::here(), "analysis/plots")) {

  dir.create(root, showWarnings = FALSE)
  fig_path <- function(name) {paste0(root, "/", name)}

  cowplot::save_plot(filename = fig_path(paste0(name,".png")),
                     plot = fig,
                     base_height = height,
                     base_width = width)

  pdf(file = fig_path(paste0(name,".pdf")), width = width, height = height)
  print(fig)
  dev.off()

}

mortality_plot <- function(data, fit){
  plot(fit, particle_fit = TRUE) +
    geom_bar(data = data, stat = "identity", aes(x=date, y=deaths), alpha = 0.6) +
    annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit$pmcmc_results$inputs$data$deaths)/mean(fit$replicate_parameters$rf), yend = max(fit$pmcmc_results$inputs$data$deaths)/mean(fit$replicate_parameters$rf), alpha = 0.5, size = 1.5) +
    annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit$pmcmc_results$inputs$data$deaths)/mean(fit$replicate_parameters$rf), hjust = 0, label = "Gov. reported deaths") +
    annotate(geom = "point", x= as.Date("2020-03-02"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.95/mean(fit$replicate_parameters$rf)) +
    annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.95/mean(fit$replicate_parameters$rf), hjust=0, label = "Gov. reported deaths (scaled)") +
    annotate(geom = "segment", x=as.Date("2020-02-28"), xend = as.Date("2020-03-05"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.9/mean(fit$replicate_parameters$rf), yend = max(fit$pmcmc_results$inputs$data$deaths)*0.9/mean(fit$replicate_parameters$rf), col = "red", alpha = 0.5, size = 1.5) +
    annotate(geom = "rect", xmin=as.Date("2020-02-28"), xmax = as.Date("2020-03-05"), ymin = max(fit$pmcmc_results$inputs$data$deaths)*0.89/mean(fit$replicate_parameters$rf), ymax = max(fit$pmcmc_results$inputs$data$deaths)*0.91/mean(fit$replicate_parameters$rf), fill = NA, color = "black", linetype = 2) +
    annotate(geom = "text", x=as.Date("2020-03-07"), y = max(fit$pmcmc_results$inputs$data$deaths)*0.9/mean(fit$replicate_parameters$rf), hjust = 0, label = "Model fit") +
    theme_bw() + theme(legend.position = "none")
}

pcr_plot <- function(PCR_Sero_PlotData, fit, Lancet_Data){
  ggplot(PCR_Sero_PlotData, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
    annotate("text", x=as.Date("2020-10-01"), y=max(tail(PCR_Sero_PlotData$max_pcr))*2, label="Model fit", alpha=0.8) +

    geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$PCR_prev["val"])) +
    geom_errorbar(aes(ymin=Lancet_Data$PCR_prev["ci95l"],ymax=Lancet_Data$PCR_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$PCR_prev["val"], height=0)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$PCR_prev["val"]+0.5, label="Mulenga et al.", hjust=0) +

    ylab(paste0("PCR+ % with ",100*round(mean(fit$replicate_parameters$rf),3),"% death reporting")) +
    xlab("Date") +
    theme_bw()
}

sero_plot <- function(PCR_Sero_PlotData, fit, Lancet_Data){
  ggplot(PCR_Sero_PlotData, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
    annotate("text", x=as.Date("2020-10-01"), y=max(tail(PCR_Sero_PlotData$max_sero))*1.12, label="Model fit", alpha=0.8) +

    geom_point(aes(x= as.Date("2020-07-15"),y=Lancet_Data$Sero_prev["val"])) +
    geom_errorbar(aes(ymin=Lancet_Data$Sero_prev["ci95l"],ymax=Lancet_Data$Sero_prev["ci95h"],x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=Lancet_Data$Sero_prev["val"], height=0)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=Lancet_Data$Sero_prev["val"]+0.5, label="Mulenga et al.", hjust =0) +

    geom_point(aes(x= as.Date("2020-07-29"),y=Lancet_Data$Tot_cov_prev["val"]), color="darkblue") +
    geom_errorbar(aes(ymin=Lancet_Data$Tot_cov_prev["ci95l"],ymax=Lancet_Data$Tot_cov_prev["ci95h"],x=as.Date("2020-07-29"), width=10), color="darkblue") +
    geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=Lancet_Data$Tot_cov_prev["val"], height=0), color="darkblue") +
    annotate("text", x=as.Date("2020-07-29")+15, y=Lancet_Data$Tot_cov_prev["val"], label="Mulenga et al. \n (combined\n  measure)", color = "darkblue", hjust =0) +

    ylab(paste0("Sero+% with ",100*round(mean(fit$replicate_parameters$rf),3),"% death reporting")) +
    xlab("Date") +
    theme_bw()
}
