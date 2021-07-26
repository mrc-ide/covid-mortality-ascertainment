Con_df <- read.csv("../Bonus Files/Data/Contact_Matrix_Meta_Data/Zim/Zimbabwe_Melegaro_2017/2017_Melegaro_Zimbabwe_contact_common.csv")[,c(1:5)]
ConEx_df <- read.csv("../Bonus Files/Data/Contact_Matrix_Meta_Data/Zim/Zimbabwe_Melegaro_2017/2017_Melegaro_Zimbabwe_contact_extra.csv")
Par_df <- read.csv("../Bonus Files/Data/Contact_Matrix_Meta_Data/Zim/Zimbabwe_Melegaro_2017/2017_Melegaro_Zimbabwe_participant_common.csv")[,c(1,3)]
Par2_df <- read.csv("../Bonus Files/Data/Contact_Matrix_Meta_Data/Zim/Zimbabwe_Melegaro_2017/2017_Melegaro_Zimbabwe_participant_extra.csv")[,c(1,3,4)]
# table(Par2_df[,c(3,9)])

# Add ages to participant ID
Con_df_age <- merge(Con_df, Par_df, by="part_id")

# How many participants were missing an age?
# Par_df[is.na(Par_df$part_age),]
## Only participant 481 was missing
# Par2_df[Par2_df$part_id==481,]
## 481 was from study site 2 (Nyanga) and is in age group 2: that means they are 1-5 years old, so likely to be in the first category, although there's a chance that could be 5 and in the next group.

# For the time being, let's remove these rows
Con_df_age <- Con_df_age[!is.na(Con_df_age$part_age),]
# table(is.na(Con_df_age$part_age))

# Select rural/town data
ID_Town <- Par2_df$part_id[Par2_df$study_site==2]
# ID_Rur <- Par2_df$part_id[Par2_df$study_site==1]
Con_df_age <- Con_df_age[Con_df_age$part_id %in% ID_Town,]
# Rur_df <- Con_df_age[Con_df_age$part_id %in% ID_Rur,]

# For the time being, use all the data.

# Check how many contact ages are missing:
# Con_df_age$cnt_age_Group <- NaN
# If data is exact, bring in 5 year age group
# Where ages are exact
ExactAges <- !is.na(Con_df_age$cnt_age_exact)
Con_df_age$cnt_age_Group <- cut(x = Con_df_age$cnt_age_exact, breaks = c(seq(0,75,5),120),include.lowest = T, right = F)

## Look at remaining min and max ages:
MinMax_rows <- is.na(Con_df_age$cnt_age_Group)# & !is.na(Con_df_age$cnt_age_est_min)
## Age categories are 1: < 1 yr. 2: 1-5 yrs. 3: 6-12 yrs. 4: 13-18 yrs. 5: 19-34 yrs. 6: 35-59 yrs. 7: 60+ yrs.

# Bring in pop age distribution:
AgeDistr <- read.csv(file = "../Bonus Files/Data/Contact_Matrix_Meta_Data/Zim/Zimbabwe_Melegaro_2017/age_dist_ny_wa_total.csv",sep = ";")

# So for the category groupings listed above,
ReformedAgeStr <- lapply(X = list("[0,5)"=0:4,
                                  "[5,10)"=5:9,
                                  "[10,15)"=10:14,
                                  "[15,20)"="15-19",
                                  "[20,25)"="20-24",
                                  "[25,30)"="25-29",
                                  "[30,35)"="30-34",
                                  "[35,40)"="35-39",
                                  "[40,45)"="40-44",
                                  "[45,50)"="45-49",
                                  "[50,55)"="50-54",
                                  "[55,60)"="55-59",
                                  "[60,65)"="60-64",
                                  "[65,70)"="65-69",
                                  "[70,75)"="70-74",
                                  "[75,120]"=c("75-79","80-84","85-89","90+")), FUN = function(x){#browser()
                                    colSums(AgeDistr[AgeDistr$age %in% x,2:3])})


ReformedAgeStr2 <- as.data.frame(cbind(names(ReformedAgeStr),do.call(rbind.data.frame, ReformedAgeStr)))
colnames(ReformedAgeStr2) <- c("AgeGroup","Nyanga","Watsomba")
AgeDistr <- ReformedAgeStr2

# Make a probability matrix for Nyanga
AgeProbDist <- AgeDistr$Nyanga/sum(AgeDistr$Nyanga)
# Make matrix to correlate divergence in age groups
AgeGConMat <- cbind(
  c(1,rep(0,15)), # if min = 0: 0-5
  c(4,1,rep(0,14)), # if min = 1, 4 in 0-4, 1 in 5-9
  c(0,4,3,rep(0,13)), # if min = 6 (6-12), 4 in 5-9, 3 in the 10-14 group
  c(rep(0,2),2,4,rep(0,12)), # if min = 13(-18), 2 in 10-14, 4 in 15-18
  c(rep(0,3),1,1,1,1,rep(0,9)), # if min = 19(-35),
  c(rep(0,7),1,1,1,1,1,rep(0,4)),
  c(rep(0,12),rep(1,4)))

MinAges <-sort(unique(Con_df_age$cnt_age_est_min),na.last = T)
# Con_df_age[MinMax_rows,]$cnt_age_Group

set.seed(seed = 1)

GroupedAgesFromMin <- sapply(X = Con_df_age[MinMax_rows,]$cnt_age_est_min, FUN = function(x){
  # browser()
  if(is.na(x)){sample(x = AgeDistr$AgeGroup, replace = T, size = 1, prob = AgeProbDist)} else {
    sample(x = AgeDistr$AgeGroup, replace = T, size = 1, prob = AgeGConMat[,which(x==MinAges)]*AgeProbDist)
  }})

Con_df_age$cnt_age_Group[MinMax_rows] <- GroupedAgesFromMin


### Participant age grouping:
Con_df_age$prt_age_Group <- cut(x = Con_df_age$part_age, breaks = c(seq(0,75,5),120),include.lowest = T, right = F)

# Split into the two days:


Day1 <- ConEx_df$cont_id[ConEx_df$studyDay==1]
Day2 <- ConEx_df$cont_id[ConEx_df$studyDay==2]


Con_df_age[Con_df_age$cont_id %in% Day1,]


Con_Mat_ini <- table(Con_df_age[,c("cnt_age_Group","prt_age_Group")])


# library(squire)
# get_mixing_matrix(country = "Spain")




## Practise with the missing data points
## They fill the missing points by taking a random value from the population structure distribution.
## Q; What population structure distribution are they using?

# table(is.na(Town_df$part_age))

# Town_df$par_age_grp <- cut(Town_df$part_age, breaks = breaks, include.lowest = T, right = F)#, labels = 1:16)
# Town_df$con_age_grp <- cut(Town_df$cnt_age_exact, breaks = breaks, include.lowest = F, right = F)#, labels = 1:16)

# Town_df_fil <- na.omit(Town_df[,c("part_id","par_age_grp","con_age_grp")])

# Ini_Mat <- table(Town_df_fil[,-1])


Av_con <- t(apply(X = Con_Mat_ini, MARGIN = 1, FUN = function(x){x/colSums(Con_Mat_ini)}))

Av_con_Sym <- (Av_con+t(Av_con))/2

squire::get_mixing_matrix("France")
squire::get_mixing_matrix("Zambia")

# colSums(Ini_Mat)


##


