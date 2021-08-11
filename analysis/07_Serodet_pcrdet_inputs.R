### Other options for Seroconversion
library(dplyr)
library(ggplot2)

## Current Probability of Sero Conversion: A gamma distribution
plot(cumsum(dgamma(0:300, shape = 5, rate = 1/2))/max(cumsum(dgamma(0:300,shape = 5, rate = 1/2))), type = "l", xlim = c(0,50), xlab = "Day", ylab = "Sero Detection")
sero_sens = 0.9
prob_conversion <-  cumsum(dgamma(0:300,shape = 5, rate = 1/2))/max(cumsum(dgamma(0:300,shape = 5, rate = 1/2)))
sero_det <- cumsum(dweibull(0:300, 3.669807, scale = 143.7046))
sero_det <- prob_conversion-sero_det
sero_det[sero_det < 0] <- 0
sero_det <- sero_det/max(sero_det)*sero_sens  # assumed maximum test sensitivitys
points(sero_det, type = "l", lty = 2)

## Data from Euroimmun https://www.fda.gov/media/137609/download
Data <- data.frame(DaysPostSympt = c(0:21,23,26,32,36),
                   n = c(3,1,5,2,4,5,2,6,2,2,4,3,3,8,5,1,3,5,2,4,2,1,1,1,2,1),
                   Neg = c(2,1,5,2,4,5,1,4,2,1,3,1,0,5,2,1,1,2,1,0,0,0,0,0,0,0),
                   Bor = c(rep(0,7),1,rep(0,11),1,rep(0,6)),
                   Pos = c(1,rep(0,5),1,1,0,1,1,2,3,3,3,0,2,3,1,3,2,1,1,1,2,1))
Data <- Data %>% mutate(PosPerc = Pos/n)

points(y = Data$PosPerc, x = Data$DaysPostSympt, pch = 20, cex = 0.5, col = 2)

## Beavis Data - Post positive PCR test
Data_Beavis <- data.frame(DaysPostPCR = c(1:12),
                          Neg = c(6,14,6,1,rep(0,8)),
                          Pos = c(1,2,9,1,1,3,1,2,3,1,1,30))
Data_Beavis <- Data_Beavis %>% mutate(PosPerc = Pos/(Pos+Neg))

# points(y = Data_Beavis$PosPerc, x = Data_Beavis$DaysPostPCR, col = 2, pch = 20, cex = 0.5)

# Van Eslande - Post Symptomatic, this has error bars.
Data_VEsl <- data.frame(DaysPostSympt = c(5.5, 7.5, 9.5, 11.5, 13.5, 15.5, 17.5),
           PosPerc = c(23.333333333333343,27.564102564102555,37.3076923076923,50.38461538461539,67.43589743589745,78.5897435897436,87.56410256410257)/100,
           PosPercUI = c(35.52825261158594, 37.457264957264954, 46.06362773029439, 59.1957502374169, 74.85042735042735, 84.27291073124407, 91.9889601139601)/100,
           PosPercLI = c(14.309116809116802, 19.35422602089269, 29.296058879392206, 42.72495251661918, 59.86348528015194, 71.88271604938271, 81.89874169040836)/100)


points(x = Data_VEsl$DaysPostSympt, y = Data_VEsl$PosPerc, col = 3, pch = 20,cex = 0.5, type = "l")
arrows(x0=Data_VEsl$DaysPostSympt, y0=Data_VEsl$PosPercLI, x1=Data_VEsl$DaysPostSympt, y1=Data_VEsl$PosPercUI, code=3, angle=90, length=0.01, col=3)

Data_Theal <- data.frame(DaysPostSympt = c(9:18),
                         n = 11,
                         Pos= c(1,1,3,3,5,8,rep(11,4)))
Data_Theal <- Data_Theal %>% mutate(PosPerc = Pos/n)
points(x = Data_Theal$DaysPostSympt, y = Data_Theal$PosPerc, col = 4, pch = 20, cex = 0.5, type = "l")

Data_Tuaillon <- data.frame(DaysPostSympt = c(1:18),
                            n = 4,
                            PosPerc = c(1,1,1,1,2,3,3,3,3,3,4,4,4,4,4,4,4,4)/4)
# points(x = Data_Tuaillon$DaysPostSympt, y = Data_Tuaillon$PosPerc, col = 5, pch = 20, cex = 0.5, type = "l")

## Buchholtz: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0251587

Data_Buchholtz <- data.frame(DaysPostSympt =c(4.971992579714683,5.900954595580595,7.775281663565373,8.886612840256028,9.888064050494457,10.853652721844446,
                                  11.96416884083309,14.966637650612478,15.933856437366487,16.890428532887988,25.87720381414403,26.989400989643066,
                                  28.02926179408872,28.881557444859315,29.921418249304963,30.959343291708343),
                             Sensitivity = c(8.683889650967828,7.636031092821213,44.31108062795305,49.45511354976376,
                                             51.74135040390185,51.64609053497942,58.31428135954885,68.69760707209267,65.55403139765279, 82.31976832799879, 82.60554793476604, 86.1301630848956, 86.03490321597317, 89.46425849718031, 89.36899862825788,92.89361377838743)/100)

points(x = Data_Buchholtz$DaysPostSympt, y = Data_Buchholtz$Sensitivity, col = 6, type = "l")


## Elslande June 2020
Data_Elslande_June <- data.frame(DaysPostSympt =c(5.5,7.5,9.5,11.5,13.5,15.5,17.5),
                             PosPerc = c(19.300911854103347,27.355623100303962,37.841945288753806,53.039513677811556,
                                             70.66869300911854,80.69908814589667,89.96960486322189)/100)

points(x = Data_Elslande_June$DaysPostSympt, y = Data_Elslande_June$PosPerc, col = 7, type = "l")



legend("bottomright", legend = c("prob_conversion",
                                 "sero_det",
                                 "US fda (n: 1-8)",# "Beavis (Post PCR)",
                                 "Van Elslande May (n = 86)", "Theal (n = 11)",
                                 "Bucholtz (n = 29)",
                                 "Van Elslande June (n = 106)"), col = c(1,1,2,3,4,6,7),pch = c(NA,NA,20,20,20,20,20,20), lty = c(1,2,NA,NA,NA,NA,NA,NA))


sero_sens = 0.90
sero_det <- sero_det/max(sero_det)*sero_sens  # assumed maximum test sensitivitys

plot(cumsum(dgamma(0:300, shape = 5, rate = 1/2))/max(cumsum(dgamma(0:300,shape = 5, rate = 1/2))), type = "l", xlim = c(0,50), xlab = "Day", ylab = "Sero Detection")
points(sero_det, type = "l", lty = 2)
points(x = Data_VEsl$DaysPostSympt, y = Data_VEsl$PosPerc, col = 3, pch = 20,cex = 0.5, type = "l")
arrows(x0=Data_VEsl$DaysPostSympt, y0=Data_VEsl$PosPercLI, x1=Data_VEsl$DaysPostSympt, y1=Data_VEsl$PosPercUI, code=3, angle=90, length=0.01, col=3)
points(x = Data_Elslande_June$DaysPostSympt, y = Data_Elslande_June$PosPerc, col = 7, type = "l")

sero_sens = 0.95
sero_det <- sero_det/max(sero_det)*sero_sens  # assumed maximum test sensitivitys

points(sero_det, type = "l", lty = 2, col = 3)

legend("bottomright", legend = c("prob_conversion",
                                 "sero_det (sens = 0.9)",
                                 # "US fda (n: 1-8)",# "Beavis (Post PCR)",
                                 "Van Elslande May (n = 86)",# "Theal (n = 11)",
                                 # "Bucholtz (n = 29)",
                                 "Van Elslande June (n = 106)",
                                 "sero_det (sens = 0.95)"), col = c(1,1,3,7,3),pch = c(NA,NA,20,20,NA), lty = c(1,2,NA,NA,2))
