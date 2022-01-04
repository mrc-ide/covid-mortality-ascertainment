###
library(squire)
PopStr <- readRDS(file = "analysis/data/Code-generated-data/00_02_Lusaka_Prov_Pop_Struc_2020.rds")

LancetData <- rbind(c(4.1,0.7,7.4),
                    c(8.1,3.8,12.3),
                    c(10.3,6.6,14),
                    c(14.4,4.9,23.9),
                    c(5.4,1.4,9.4),
                    c(14.7, 4.2, 25.1))

LancetAgeNum <- c(29.3,22.3,19.4,14.2,8.3,6.5)
LancetAgePrev <- c(4.1,8.1,10.3,14.4,5.4,14.7)

OddsTab <- cbind(4258*LancetAgeNum/100 *LancetAgePrev/100,
      4258*LancetAgeNum/100 *(1-LancetAgePrev/100))

AR <- OddsTab[,1]/(4258*LancetAgeNum/100)
RR <- AR/tail(AR,1)
UCI_RR <- c(head(exp(log(RR) + 1.96*sqrt((OddsTab[,2]/OddsTab[,1])/(4258*LancetAgeNum/100) + tail((OddsTab[,2]/OddsTab[,1])/(4258*LancetAgeNum/100), 1))),-1),1)
LCI_RR <- c(head(exp(log(RR) - 1.96*sqrt((OddsTab[,2]/OddsTab[,1])/(4258*LancetAgeNum/100) + tail((OddsTab[,2]/OddsTab[,1])/(4258*LancetAgeNum/100), 1))),-1),1)



                    # c(16.220899470899475,0.14579659024103542,32.123769106408005),
                    # c(11.570601851851857, 1.7533068783068817, 21.445307907113467),
                    # c(13.43646200764257, 2.499650940623166,24.344567533803648))

# c(sum(PopStr[1:2]),
#   sum(PopStr[3:4]),
#   sum(PopStr[5:6]),
#   sum(PopStr[7:8]),
#   sum(PopStr[9:10]),
#   sum(PopStr[11:17]))/sum(PopStr)


# c(sum(PopStr[1:2]),
#   sum(PopStr[3:4]),
#   sum(PopStr[5:6]),
#   sum(PopStr[7:8]),
#   sum(PopStr[9:10]),
#   sum(PopStr[11:12]),
#   sum(PopStr[13:17]))/sum(PopStr)

# Mean_PCR_60plus <- sum(LancetData[7:8,1] *   c(sum(PopStr[11:12]), sum(PopStr[13:17]))/sum(PopStr[11:17]))

# SEs <- (LancetData[7:8,3]-LancetData[7:8,2])/3.92
# SE_Tot <- sqrt(sum((SEs*c(sum(PopStr[11:12]), sum(PopStr[13:17]))/sum(PopStr[11:17]))^2))
# LancetData_comb <- rbind(LancetData[1:6,],
      # c(Mean_PCR_60plus, Mean_PCR_60plus - SE_Tot*1.96,Mean_PCR_60plus + SE_Tot*1.96))

# Run the model over a range of R0 values:
Zam_det <- lapply(X = c(2,4,6,8), FUN = function(x){run_deterministic_SEIR_model(country = "Zambia",R0 = x,contact_matrix_set = get_mixing_matrix("Zambia"), population = PopStr)})
Chi_det <- lapply(X = c(2,4,6,8), FUN = function(x){run_deterministic_SEIR_model(country = "China",R0 = x)})
Net_det <- lapply(X = c(2,4,6,8), FUN = function(x){run_deterministic_SEIR_model(country = "Netherlands",R0 = x)})

# For each run, extract the point at which 10.6 are infected.
AttackRate <- function(Output, cum_inf){
  lapply(cum_inf, function(cum_inf_i){
    Res_output_cum_inf <- lapply(Output, function(Output_i){
      TimePoint <- min(which(rowSums(Output_i$output[,c(grep("cum_infs", colnames(Output_i$output))),1])/sum(Output_i$parameters$population)>=cum_inf_i/100))
      Res_output_cum_inf <- Output_i$output[TimePoint,c(grep("cum_infs", colnames(Output_i$output))),1]
      c(Res_output_cum_inf[1:10],sum(Res_output_cum_inf[11:17]))/c(Output_i$parameters$population[1:10],sum(Output_i$parameters$population[11:17]))
    })
    data.frame(matrix(unlist(Res_output_cum_inf), nrow = length(Output), byrow = T))
  })
}

RelativeAttackRate <- function(Output, cum_inf){
  AR <- AttackRate(Output, cum_inf)
  # Calculate relative risk for each row
  lapply(AR, function(AR_i){
    # browser()
    RR_tmp <- apply(AR_i, 1, function(x){x/tail(x,1)})
    rbind(rowMeans(RR_tmp), apply(RR_tmp, 1, function(x){
      return(c(min(x),max(x)))
    }))
  })
}


cum_inf_v <- c(10.6,30,50)
Col_lines_v <- c("black","red","darkblue")
Col_back_v <- c("grey","pink","lightblue")

Plot_func <- function(Output){ # Where output is a list of the three attack rate dfs
  par(mfrow=c(1,length(Output[[1]]))) # Set up the plotting area
  # plot the first graph
  lapply(1:length(Output[[1]]), function(x){
    plot("n", ylim = c(0,6.5), xlab = "Age Group", xlim = c(1,11), ylab = "Relative Risk", xaxt = "n", main = paste0("Cumulative Infection = ",cum_inf_v[x],"%"))
    if(x ==1){legend("topleft", legend = c("Zambia", "China", "Netherlands", "Zambia SARS-CoV-2  prev. (rel.)"), col = c(1,2,"darkblue",1), lty = c(1,1,1,NA), pch = c(NA,NA,NA, 20))}
    axis(1, at=seq(1:11),labels=as.character(c(seq(5,50,by = 5),"50+")), las=2)

    lapply(1:length(Output), function(y){
      polygon(x = c(1:11,11:1), y = c(Output[[y]][[x]][2,],rev(Output[[y]][[x]][3,])), border=F, col = Col_back_v[y])
      points(Output[[y]][[x]][1,], pch = 20, ylim = c(0,8), type = "l", col = Col_lines_v[y])
    })
    if(x ==1){
      points(x = seq(1,11,2), RR, pch = 20)
      # points(x = seq(1,15, by = 2), y = LancetData[,1]/100, col = "darkblue", pch = 20)
      arrows(seq(1,11,2), UCI_RR, seq(1,11,2), LCI_RR, angle=90, code=3, length = 0.05, lwd = 0.5)}
  })
}

AttackRate_Zam <- RelativeAttackRate(Output = Zam_det, cum_inf = c(10.6,30,50))
AttackRate_Chi <- RelativeAttackRate(Output = Chi_det, cum_inf = c(10.6,30,50))
AttackRate_Net <- RelativeAttackRate(Output = Net_det, cum_inf = c(10.6,30,50))

pdf(file = "analysis/figures/17_1_Relative_Attack_Rate_Neth_Chin_Zam.pdf", width = 12, height = 5)
Plot_func(list(AttackRate_Zam, AttackRate_Chi, AttackRate_Net))
dev.off()


Neth_Con_Mat <- squire::get_mixing_matrix(country = "Netherlands")
Zamb_Con_Mat <- squire::get_mixing_matrix(country = "Zambia")
Chin_Con_Mat <- squire::get_mixing_matrix(country = "China")
rownames(Neth_Con_Mat) <- 1:16
colnames(Neth_Con_Mat) <- 1:16
rownames(Chin_Con_Mat) <- 1:16
colnames(Chin_Con_Mat) <- 1:16
rownames(Zamb_Con_Mat) <- 1:16
colnames(Zamb_Con_Mat) <- 1:16
heatmap(Neth_Con_Mat, Rowv = NA, Colv = NA, main = "Netherlands")
heatmap(Chin_Con_Mat, Rowv = NA, Colv = NA, main = "China")
heatmap(Zamb_Con_Mat, Rowv = NA, Colv = NA, main = "Zambia")
