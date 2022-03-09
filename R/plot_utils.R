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





######
part_fit_plot <- function(fit){plot(fit, particle_fit = T) +
    annotate(geom = "point", x= as.Date("2020-04-18"), y = 50) +
    annotate(geom = "text", x=as.Date("2020-04-22"), y = 50, hjust=0, label = "BMJ data estimated deaths") +
    annotate(geom = "segment", x=as.Date("2020-04-15"), xend = as.Date("2020-04-20"), y = 50*0.92, yend = 50*0.92, col = "red", alpha = 0.7) +
    annotate(geom = "rect", xmin=as.Date("2020-04-15"), xmax = as.Date("2020-04-20"), ymin = 50*0.915, ymax = 50*0.925, fill = NA, color = "black", linetype = 2) +
    annotate(geom = "text", x=as.Date("2020-04-22"), y = 50*0.92, hjust = 0, label = "Model fit") +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 50)) +
    theme(legend.position = "none")
}
pcr_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_pcr)) + geom_line(aes(x=date, y=mean_pcr),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_pcr, ymax=max_pcr), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-15"),y=7.6)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=7.6+0.5, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=2, label="Model fit", alpha=0.8) +
    geom_errorbar(aes(ymin=4.7,ymax=10.6,x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=7.6, height=0)) +
    ylab(paste0("PCR +ve %")) +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 15)) +
    theme_bw()} #+
    # ggtitle(paste("bin ll", round(mean(dbinom(x = round(0.076*2990), size = 2990, prob = filter(pcr_sero_data, date %in% as.Date(c("2020-07-15")))$mean_pcr/100,log = T)),1)))}
sero_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_sero)) + geom_line(aes(x=date, y=mean_sero),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_sero, ymax=max_sero), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-15"),y=2.1)) +
    annotate("text", x=as.Date("2020-07-15")+5, y=2.1+0.5, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=max(Summ_sero_fit$max_sero), label="Model fit", alpha=0.8, hjust=0, vjust = 2) +
    geom_errorbar(aes(ymin=1.1,ymax=3.1,x=as.Date("2020-07-15"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-27"),y=2.1, height=0)) +
    # geom_point(aes(x= as.Date("2020-07-29"),y=10.6), color="darkblue") +
    # annotate("text", x=as.Date("2020-07-29")+15, y=10.6, label="Mulenga et al. \n (extrapolated \n from PCR)", color = "darkblue", hjust =0) +
    # geom_text(aes(label = "Mulenga et al. \n (extrapolated \n from PCR)", x= as.Date("2020-07-29")+15,y=0.106), color = "darkblue", hjust= 0) +
    # geom_errorbar(aes(ymin=7.3,ymax=13.9,x=as.Date("2020-07-29"), width=10), color="darkblue") +
    # geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=10.6, height=0), color="darkblue") +
    ylab(paste0("Sero +ve %")) +
    annotate("text", x=as.Date("2020-05-01"), y=1.8, label=paste(""), color = "darkblue", hjust =0) +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 20)) +
    theme_bw()}

combined_fit_plot <- function(Summ_sero_fit){ggplot(Summ_sero_fit, aes(x = date, y = mean_combined)) + geom_line(aes(x=date, y=mean_combined),linetype="dashed") +
    geom_ribbon(aes(x=date,ymin=min_combined, ymax=max_combined), alpha=0.3)+
    geom_point(aes(x= as.Date("2020-07-11"),y=9.1)) +
    annotate("text", x=as.Date("2020-07-11")+5, y=9.1+0.5, label="Mulenga et al.", hjust =0) +
    annotate("text", x=as.Date("2020-09-01"), y=max(Summ_sero_fit$max_combined), label="Model fit", alpha=0.8, hjust=0, vjust = 2) +
    geom_errorbar(aes(ymin=2.6,ymax=15.7,x=as.Date("2020-07-11"), width=10)) +
    geom_errorbarh(aes(xmin=as.Date("2020-07-04"),xmax=as.Date("2020-07-19"),y=9.1, height=0)) +
    # geom_point(aes(x= as.Date("2020-07-29"),y=10.6), color="darkblue") +
    # annotate("text", x=as.Date("2020-07-29")+15, y=10.6, label="Mulenga et al. \n (extrapolated \n from PCR)", color = "darkblue", hjust =0) +
    # geom_text(aes(label = "Mulenga et al. \n (extrapolated \n from PCR)", x= as.Date("2020-07-29")+15,y=0.106), color = "darkblue", hjust= 0) +
    # geom_errorbar(aes(ymin=7.3,ymax=13.9,x=as.Date("2020-07-29"), width=10), color="darkblue") +
    # geom_errorbarh(aes(xmin=as.Date("2020-07-18"),xmax=as.Date("2020-08-10"),y=10.6, height=0), color="darkblue") +
    ylab(paste0("Combined +ve %")) +
    # annotate("text", x=as.Date("2020-05-01"), y=1.8, label=paste(""), color = "darkblue", hjust =0) +
    xlim(as.Date("2020-04-15"), as.Date("2020-10-01")) + coord_cartesian(ylim=c(0, 20)) +
    theme_bw()}
