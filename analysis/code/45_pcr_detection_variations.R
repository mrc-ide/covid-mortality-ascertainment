sero_sens <- 0.9
pcr_sens <- 0.95

prob_conversion <-  cumsum(dgamma(0:300,shape = 5, rate = 1/2))/max(cumsum(dgamma(0:300,shape = 5, rate = 1/2)))
sero_det <- cumsum(dweibull(0:300, 3.669807, scale = 143.7046))
sero_det <- prob_conversion-sero_det
sero_det[sero_det < 0] <- 0
sero_det <- sero_det/max(sero_det)*sero_sens  # assumed maximum test sensitivitys

# from Hay et al 2021 Science (actually from preprint)
pcr_sens = 0.8
pcr_det <- c(9.206156e-13, 9.206156e-13, 3.678794e-01, 9.645600e-01,
             9.575796e-01, 9.492607e-01, 9.393628e-01, 9.276090e-01,
             9.136834e-01, 8.972309e-01, 8.778578e-01, 8.551374e-01,
             8.286197e-01, 7.978491e-01, 7.623916e-01, 7.218741e-01,
             6.760375e-01, 6.248060e-01, 5.683688e-01, 5.072699e-01,
             4.525317e-01, 4.036538e-01, 3.600134e-01, 3.210533e-01,
             2.862752e-01, 2.552337e-01, 2.275302e-01, 2.028085e-01,
             1.807502e-01, 1.610705e-01, 1.435151e-01, 1.278563e-01,
             1.138910e-01, 1.014375e-01, 9.033344e-02)
pcr_det <- (pcr_det/max(pcr_det))*pcr_sens


pcr_det_sero_len <- c(pcr_det, rep(0, length(sero_det)-length(pcr_det)))
comb_det <-   1-((1-c(0, 0, 0, 0, head(sero_det, -4))) * (1-pcr_det_sero_len))

points(pcr_det, pch = 20)
# points(sero_det)
# plot(comb_det)


##########################################
##########################################
## Test results with CT value of 37

library(evd)
points(dgumbel(seq(1,10,by = 0.1), loc = 1.4, scale = 0.4), type = "l")


dlogis()

beta_1 + beta_2*x + beta_2*beta_3*x*I_func(x)
Time_from_infection <- seq(0,30,0.001)
C <- 3.18
x <- Time_from_infection - C
# C <- time_of_breakpoint
# x <- time_between_infction_and_testing_minus_breakpoint

# beta_1 <- 1.51
# beta_2 <- 2.19
# beta_3 <- -1.1

beta_1 <- 1.51
beta_2 <- 2.19
beta_3 <- -1.1

plot(x = Time_from_infection, LaplacesDemon::invlogit(beta_1 + beta_2*x + ifelse(x>0, beta_2*beta_3*x, 0)), type = "l", ylim = c(0,1))

points(x = Time_from_infection, LaplacesDemon::invlogit(beta_1 + beta_2*x + ifelse(x>0, beta_2*beta_3*x, 0)), type = "l", ylim = c(0,1))

## Can change the beta_3 to only change the descent.



##########################################
##########################################

## Gumbel distribution:
library(evd)
a_vec <- 1:100

t_e <- 0
C_z <- 40

t_p <- 5
C_p <- 20.6 # 19.7

# t_s <- 9.38 # Prior
t_s <- 16 # Post
C_s <- 33 # 38

t_LOD <- Inf
C_LOD <- 40

sigma_obs <- 5.15 # 5
s_mod <- 4 # 7.89
t_mod <- 14

plot(dgumbel(x = a_vec, loc = 0, scale = 5))
plot(dgumbel(x = a_vec, loc = 10, scale = 1))

plot(dgumbel(x = a_vec,
             loc = ifelse(a_vec<=t_e, C_z,
                          ifelse(a_vec<=t_e+t_p, C_z + (C_p-C_z)/t_p * (a_vec-t_e),
                                 ifelse(a_vec<=t_e+t_p + t_s, C_p + (C_s-C_p)/t_s * (a_vec-t_e-t_p),
                                        C_s + (C_LOD-C_s)/(t_LOD-t_s-t_p-t_e) * (a_vec-t_e-t_p-t_s)))),
             scale = ifelse(a_vec<t_e+t_p+t_s, sigma_obs,
                            ifelse(a_vec<t_e+t_p+t_s+t_mod, sigma_obs*(1-((1-s_mod)/t_mod * (a_vec-t_e-t_p-t_s))),
                                   sigma_obs * s_mod))))

dgumbel(x = a_vec,
        loc = ifelse(a_vec<=t_e, C_z,
                     ifelse(a_vec<=t_e+t_p, C_z + (C_p-C_z)/t_p * (a_vec-t_e),
                            ifelse(a_vec<=t_e+t_p + t_s, C_p + (C_s-C_p)/t_s * (a_vec-t_e-t_p),
                                   C_s + (C_LOD-C_s)/(t_LOD-t_s-t_p-t_e) * (a_vec-t_e-t_p-t_s)))),
        scale = ifelse(a_vec<t_e+t_p+t_s, sigma_obs,
                       ifelse(a_vec<t_e+t_p+t_s+t_mod, sigma_obs*(1-((1-s_mod)/t_mod * (a_vec-t_e-t_p-t_s))),
                              sigma_obs * s_mod)))

p_addl

P_detectable = P[C(a) <= C_LOD](1-p_addl)^(a-t_e-t_p-t_s)

P[C(a)<=C_LOD]
