########################################################################################################################
### 03. Contact Matrices: Create contact matrix for Nyanga from the Manicaland data to be used as a proxy for Lusaka ###
########################################################################################################################
## Follows methods in supp 2 of Melegaro et al. 2017
rm(list=ls())
library(tidyverse)
library(reshape2)

#####################
### 1. Prep files ###
#####################

# Load and format the data
Con_df <- read.csv("analysis/data/raw/Manicaland_Contact_Matrix_Data/2017_Melegaro_Zimbabwe_contact_common.csv")[,c(1:5)]
ConEx_df <- read.csv("analysis/data/raw/Manicaland_Contact_Matrix_Data/2017_Melegaro_Zimbabwe_contact_extra.csv")
Par_df <- read.csv("analysis/data/raw/Manicaland_Contact_Matrix_Data/2017_Melegaro_Zimbabwe_participant_common.csv")[,c(1,3)]
Par2_df <- read.csv("analysis/data/raw/Manicaland_Contact_Matrix_Data/2017_Melegaro_Zimbabwe_participant_extra.csv")[,c(1,3,4)]

# Merge par files
Par_full <- merge(Par_df, Par2_df, by = "part_id")

# For the time being, let's remove the rows where participant age was not given (there is only one of these),
# and specify location
Par_full <- Par_full[!is.na(Par_full$part_age),] %>%
  mutate(site = ifelse(study_site==2,"Nyanga",ifelse(study_site==1,"Watsomba",NA))) %>%
  mutate(par_age_gr = cut(x = part_age, breaks = c(seq(0,75,5),120),include.lowest = T, right = F))

# Merge Contact files
Con_full <- merge(Con_df,ConEx_df, by = "cont_id")

# Merge Con and Par files, 27 rows are lost because we are removing one participant (481)
df_full <- merge(Con_full, Par_full, by = "part_id")
# saveRDS(object = df_full, file = "analysis/data/Code-generated-data/Con_Mat_df_full.rds")
# df_full <- readRDS("analysis/data/Code-generated-data/Con_Mat_df_full.rds")

# Also, 4 participants are not found in the contact data: part_id: 152, 385, 1012, 1103
# table(Par_full$part_id %in% Con_full$part_id)
# Par_full[!Par_full$part_id %in% unique(df_full$part_id),]

## Calculate age distributions by groups

# - Get the sample age groups n_i:
ni <- table(cut(x = unique(df_full[,c("part_id","part_age")])$part_age, breaks = c(seq(0,75,5),120),include.lowest = T, right = F))
sigma_ni <- sum(ni)

# - Get the sample age groups per site
ni_s <- cbind(ni, sapply(X = 1:2, function(x){
  table(cut(x = unique(df_full[df_full$study_site==x,c("part_id","part_age")])$part_age, breaks = c(seq(0,75,5),120),include.lowest = T, right = F))
}))
# sum(c(sigma_ni_s1,sigma_ni_s2))

# - Get the sample age groups per site per day (These are the same as per site)
ni_s_d <- cbind(ni_s, apply(expand.grid(c(1,2),c(1,2)), 1, function(x){
  table(cut(x = unique(df_full[df_full$study_site==x[1] & df_full$studyDay==x[2],c("part_id","part_age")])$part_age, breaks = c(seq(0,75,5),120),include.lowest = T, right = F))
}))
colnames(ni_s_d)[-1] <- c("ni_s1","ni_s2","ni_s1_d1","ni_s2_d1","ni_s1_d2","ni_s2_d2") # Label nicely
ni_s_d <- ni_s_d[,c("ni","ni_s1","ni_s2","ni_s1_d1","ni_s1_d2","ni_s2_d1","ni_s2_d2")] # Reorder

# Bring in pop age distribution:
PopAgeDistr <- read.csv(file = "analysis/data/raw/Manicaland_Contact_Matrix_Data/age_dist_ny_wa_total.csv",sep = ";")
# saveRDS(object = PopAgeDistr, file = "../PopAgeDistr.rds")


##############################
### 1. Impute missing ages ###
##############################

# Break down PopAgeDistr by into individual ages
PopAgeBreakDown <- rbind(PopAgeDistr[1:15,],
      cbind(age = 15:89,apply(PopAgeDistr[16:30,2:3]/5, 2, FUN = rep, each = 5)),
      cbind(age = 90:105,sapply(PopAgeDistr[31,2:3]/length(90:105), FUN = rep, each = length(90:105)))
      ) %>%
  gather(key = Site, value = val, -age) %>%
  mutate(Site = str_to_title(Site))

# 20 imputation steps, returns grouped ages of cont and part
Imp <- 1:20
set.seed(seed = 1)
df_ImpList <- lapply(X = Imp, function(y){
  GroupedAgesFromMin <- apply(X = df_full[is.na(df_full$cnt_age_exact),c("cnt_age_est_min","cnt_age_est_max","site")], MARGIN = 1, FUN = function(x){
    if(is.na(x["cnt_age_est_min"])){
      sample(x = unique(as.numeric(PopAgeBreakDown$age)), replace = T, size = 1, prob = PopAgeBreakDown[PopAgeBreakDown$Site == x["site"],"val"])} else {
        sample(x = x["cnt_age_est_min"]:x["cnt_age_est_max"], replace = T, size = 1, prob = PopAgeBreakDown[as.numeric(PopAgeBreakDown$age) %in% x["cnt_age_est_min"]:x["cnt_age_est_max"] & PopAgeBreakDown$Site == x["site"],"val"])
      }})
  df_full[is.na(df_full$cnt_age_exact),"cnt_age_exact"] <- GroupedAgesFromMin
  df_full$con_age_gr <- cut(x = df_full$cnt_age_exact, breaks = c(seq(0,75,5),120),include.lowest = T, right = F)
  return(df_full)
})

# Check values are full
table(is.na(df_ImpList[[1]]$con_age_gr))
table(is.na(df_ImpList[[1]]$par_age_gr))


###################################
### 2. Construct Contact Matrix ###
###################################

# Take the data and...
df_ImpList_mat <-  lapply(Imp, function(x){
  df_ImpList[[x]] %>% group_by(site, studyDay) %>% # group by site and day
    select(par_age_gr, con_age_gr, site, studyDay) %>% count(par_age_gr, con_age_gr) %>% # count numbers in each group
    spread(key = con_age_gr, value = n) %>% ungroup %>% # create matrix
    split(list(.$studyDay,.$site))}) # Split the matrix into 4 for sites and days

# Format into something that looks a bit more like a contact matrix
MatList <- lapply(Imp, function(x){
  lapply(1:length(df_ImpList_mat[[x]]), function(y){
    df_ImpList_mat[[x]][[y]] %>% select(-site,-studyDay) %>% # Remove site and studyDay columns
      column_to_rownames(var = "par_age_gr") %>% # Turn age gr column into row names
      replace(is.na(.),0) # replace NA with 0
  })
  })


# Average number of observed contacts per day, per site (eq 7)
m_ij_sd <- lapply(Imp, function(x){
  lapply(1:length(df_ImpList_mat[[x]]), function(y){
    MatList[[x]][[y]]/ni_s_d[,c("ni_s1_d1","ni_s1_d2","ni_s2_d1","ni_s2_d2")][,y]})
})

# Average number of observed contacts per site (eq 8)
m_ij_s <- lapply(Imp, function(x){
  lapply(c(1,3),function(y){
    (m_ij_sd[[x]][[y]]*ni_s_d[,c("ni_s1_d1","ni_s1_d2","ni_s2_d1","ni_s2_d2")][,y] + m_ij_sd[[x]][[y+1]]*ni_s_d[,c("ni_s1_d1","ni_s1_d2","ni_s2_d1","ni_s2_d2")][,y+1])/
      (rowSums(ni_s_d[,c("ni_s1_d1","ni_s1_d2","ni_s2_d1","ni_s2_d2")][,c(y,y+1)]))})
})


# Average number of observed contacts (eq 9).
# Note that the n_i per day are the same, but it doesn't matter if you use for one day or the total, because they are found on both sides of the fraction
m_ij <- lapply(Imp, function(x){
  (m_ij_s[[x]][[1]]*rowSums(ni_s_d[,c("ni_s1_d1","ni_s1_d2")]) + m_ij_sd[[x]][[2]]*rowSums(ni_s_d[,c("ni_s2_d1","ni_s2_d2")]))/
      (rowSums(ni_s_d[,c("ni_s1_d1","ni_s1_d2","ni_s2_d1","ni_s2_d2")]))
  })

# Get the population grouped age structure
PopAgeTot <- PopAgeBreakDown %>% group_by(AgeGroup = cut(x = as.numeric(age), breaks = c(seq(0,75,5),120),include.lowest = T, right = F)) %>%
  summarise(AgeTot = sum(val))
pop_i <- as.numeric(PopAgeTot$AgeTot)

# Expected number of contacts at the population level (eq 10):
C_ij <- lapply(Imp, function(x){
  pop_i * m_ij[[x]]})

# Correction for reciprocity of contacts (eq 11):
n_i <- table(unique(df_ImpList[[1]][,c("part_id","par_age_gr")])$par_age_gr)
n_i_plus_n_j <- matrix(rep(n_i, length(n_i)), nrow = length(n_i)) + t(matrix(rep(n_i, length(n_i)), nrow = length(n_i)))

Cs_ij <- lapply(Imp, function(x){
  (C_ij[[x]]*c(n_i) + t(C_ij[[x]]*c(n_i)))/ n_i_plus_n_j
})

# Average number of contacts of individual i with contacts j
ms_ij <- lapply(Imp, function(x){
  Cs_ij[[x]]/pop_i})

# Contact rates at which individual aged i contacts per day an individual aged j from the matrix
cs_ij <- lapply(Imp, function(x){
  t(t(ms_ij[[x]])/pop_i)})

# Average out replicates:
cs_ij_av <- Reduce('+',cs_ij)/20


squire::get_mixing_matrix("Zimbabwe")
heatmap(squire::get_mixing_matrix("Zimbabwe"),Rowv = NA, Colv = NA)
heatmap(cs_ij_av,Rowv = NA, Colv = NA)



##############################
### 3. Bivariate Smoothing ###
##############################


# Turn matrix back into data frame
cs_ij_av <- data.frame(cs_ij_av)
colnames(cs_ij_av) <- 1:16
Gathered_Matrix <- cs_ij_av %>%
  remove_rownames() %>% mutate(n_i = n_i) %>% mutate(pop_i = pop_i) %>%
  rownames_to_column(var = "Par_Age_gr") %>%
  gather(key = Con_Age_gr, value = val, -Par_Age_gr, -n_i, -pop_i)

## Prep Data:
Gathered_Matrix$Par_Age_gr <- as.numeric(Gathered_Matrix$Par_Age_gr)
Gathered_Matrix$Con_Age_gr <- as.numeric(Gathered_Matrix$Con_Age_gr)
Gathered_Matrix$n_i <- as.numeric(Gathered_Matrix$n_i)
Gathered_Matrix$ln_n_i <- log(Gathered_Matrix$n_i)

# Perform GAM smoothing
library(mgcv)
ModFit <- gam(val ~  te(Par_Age_gr, Con_Age_gr, bs="tp", k = 10) + offset(ln_n_i),
              data = Gathered_Matrix,
              family = nb)

# summary(ModFit)
# plot(ModFit)
# plot(ModFit, scheme = 2)

# Create predictions based on model
Predictions <- predict(object = ModFit, type='response')

# Look at fit
cbind(Gathered_Matrix,Predictions)

# Shape as matrix
BsmMat <- Gathered_Matrix %>% mutate(Predictions = Predictions) %>%
  select(Par_Age_gr,Con_Age_gr,Predictions) %>%
  spread(key = Con_Age_gr, value = Predictions) %>%
  column_to_rownames("Par_Age_gr")

## Scale up again
t(t(BsmMat)*pop_i)
heatmap(t(t(BsmMat)*pop_i),Rowv = NA, Colv = NA)

# Compare with Reported Matrix
squire::get_mixing_matrix("Zimbabwe")
