

## So as you chnage the proportion that is receiving hospital treatment, how does that affect the IFR?

Proportion_Treated <- 1

Zmb_p <-parameters_explicit_SEEIR("Zambia")
# IFR_Age
# Calculate the probability of death by weighting probabilities of death given treatment by probability of severe
prob_death_tot <- Proportion_Treated * (Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment +
  (1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_treatment) +
  (1-Proportion_Treated) * (Zmb_p$prob_severe * Zmb_p$prob_severe_death_no_treatment +
                          (1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_no_treatment)

Change_Prop_Treated <- sapply(X = seq(from = 1, to = 0, by = -0.1), FUN = function(Proportion_Treated){
  # browser()
  100*Zmb_p$prob_hosp*(Proportion_Treated * (Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment +
                          (1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_treatment) +
    (1-Proportion_Treated) * (Zmb_p$prob_severe * Zmb_p$prob_severe_death_no_treatment +
                                (1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_no_treatment))
})

# Calculate IFR for each age group: multiply probability of death of a case by probability hospitalised
IFR_Age <- 100*(Zmb_p$prob_hosp * prob_death_tot)
IFR_Age_gr <- seq(2.5, 82.5, by = 5)



plot(Zmb_p$prob_hosp*(Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment))
plot(Zmb_p$prob_hosp*(Zmb_p$prob_severe * Zmb_p$prob_non_severe_death_treatment))
plot(Zmb_p$prob_hosp*((1-Zmb_p$prob_severe) * Zmb_p$prob_severe_death_no_treatment))
plot(Zmb_p$prob_hosp*((1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_no_treatment))

p1 <- cbind("p_hosp" = Zmb_p$prob_hosp,
            "p_severe" = Zmb_p$prob_severe,
            "p_nsev_death_tre" = Zmb_p$prob_non_severe_death_treatment,
            "p_nsev_death_no_tre" = Zmb_p$prob_non_severe_death_no_treatment,
            "p_sev_death_tre" = Zmb_p$prob_severe_death_treatment,
            "p_sev_death_no_tre" = Zmb_p$prob_severe_death_no_treatment) %>% melt() %>%
  ggplot(aes(x = Var1*5-2.5, y = value, group = as.factor(Var2), col = as.factor(Var2))) + geom_line() +
  viridis::scale_color_viridis(discrete = T,
                               name = "",
                               labels = c("P(Hosp | Inf)",
                                          "P(Sev | Hosp, Inf)",
                                          "P(D | Tr, nSev, Hosp, Inf)",
                                          "P(D | nTr, nSev, Hosp, Inf)",
                                          "P(D | Tr, Sev, Hosp, Inf)",
                                          "P(D | nTr, Sev, Hosp, Inf)")) + theme_minimal() +
  xlab("Age") + ylab("Probability") + theme(legend.position = "bottom") +
  guides(color=guide_legend(nrow=3, byrow=TRUE))


p4 <- cbind(severe_death_treatment = 100*Zmb_p$prob_hosp*(Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment),
            severe_death_no_treatment = 100*Zmb_p$prob_hosp*(Zmb_p$prob_severe * Zmb_p$prob_severe_death_no_treatment),
            non_severe_death_treatment = 100*Zmb_p$prob_hosp*((1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_treatment),
            non_severe_death_no_treatment = 100*Zmb_p$prob_hosp*((1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_no_treatment)) %>% melt() %>%
  ggplot(aes(x = Var1*5-2.5, y = value, group = as.factor(Var2), col = as.factor(Var2))) + geom_line() +
  viridis::scale_color_viridis(discrete = T, begin = 2/5, end = 1,
                               name = "",
                               labels = c("P(D, Tr, nSev | Inf)",
                                          "P(D, nTr, nSev | Inf)",
                                          "P(D, Tr, Sev | Inf)",
                                          "P(D, nTr, Sev | Inf)"
                                          )) + theme_minimal() + theme(legend.position = "bottom") +
  xlab("Age") + ylab("IFR (%)") +
  guides(color=guide_legend(nrow=2, byrow=TRUE))

p4b <- cbind(death_treatment = 100*Zmb_p$prob_hosp*(Zmb_p$prob_severe * Zmb_p$prob_severe_death_treatment + (1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_treatment),
            death_no_treatment = 100*Zmb_p$prob_hosp*(Zmb_p$prob_severe * Zmb_p$prob_severe_death_no_treatment + (1-Zmb_p$prob_severe) * Zmb_p$prob_non_severe_death_no_treatment)) %>% melt() %>%
  ggplot(aes(x = Var1*5-2.5, y = value, group = as.factor(Var2), col = as.factor(Var2))) + geom_line() +
  viridis::scale_color_viridis(discrete = T, begin = 2/5, end = 1,
                               name = "",
                               labels = c("P(D, Tr | Inf)",
                                          "P(D, nTr | Inf)"
                               )) + theme_minimal() + theme(legend.position = "bottom") +
  xlab("Age") + ylab("IFR (%)") +
  guides(color=guide_legend(nrow=2, byrow=TRUE))

p5 <- melt(Change_Prop_Treated) %>% ggplot(aes(x = Var1*5-2.5, y = value, group = as.factor(Var2), color = as.factor(Var2))) + geom_line() +
  viridis::scale_color_viridis(discrete = T,
                               name = "Prop. Treated",
                               labels = paste0(100*seq(1,0,by = -0.1),"%")) +
  theme_minimal() + xlab("Age") + ylab("IFR (%)")

p6 <- melt(Change_Prop_Treated) %>% ggplot(aes(x = Var1*5-2.5, y = value, group = as.factor(Var2), color = as.factor(Var2))) + geom_line() +
  viridis::scale_color_viridis(discrete = T,
                               name = "Prop. Treated",
                               labels = paste0(100*seq(1,0,by = -0.1),"%")) +
  theme_minimal() + xlab("Age") + ylab("IFR (%)") +
  scale_y_continuous(trans='log10')
  # ylim=c(0.01,20)


cowplot::plot_grid(p1,p4,p5,p6, rel_heights = c(1,0.8))
