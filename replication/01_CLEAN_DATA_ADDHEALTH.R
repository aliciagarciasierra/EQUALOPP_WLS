# BUILDING MASTER DATA #

rm(list = ls())
source("00_MASTER_ADDHEALTH.R")

current_year = 2023
datasets_dir <- "Work/GenesEquality/datasets/"



# READ ----



# - WAVE I HOME

# -- parent info, parenting, observed ability, grades

#waveI <- read_xpt("Data/Core Files - Wave I/Wave I In Home Interview Data/allwave1.xpt") %>%
#   select(AID, IYEAR, p_age = PA2, 
#          cognitive = AH_PVT, 
#          any_of(NON_COG_home)) 
#save(waveI, file = paste0(datasets_dir,"waveI.rda"))
load(paste0(datasets_dir,"waveI.rda"))


# recode NAs
waveI <- waveI %>% 
  mutate(p_age = ifelse(p_age==996,        NA, p_age),
         neu1 = ifelse(neu1 %in% c(6,8),   NA, neu1),
         neu2 = ifelse(neu2 %in% c(6,8),   NA, neu2),
         neu3 = ifelse(neu3 %in% c(6,8),   NA, neu3),
         neu4 = ifelse(neu4 %in% c(6,8),   NA, neu4),
         neu5 = ifelse(neu5 %in% c(6,8,9), NA, neu5),
         neu6 = ifelse(neu6 %in% c(6,8,9), NA, neu6),
         con1 = ifelse(con1 %in% c(6,8,9), NA, con1),  # invert
         con2 = ifelse(con2 %in% c(6,8,9), NA, con2),  # invert
         con3 = ifelse(con3 %in% c(6,8,9), NA, con3),  # invert
         con4 = ifelse(con4 %in% c(6,8,9), NA, con4),  # invert
         ) 

waveI <- waveI %>% 
  mutate(across(contains("con"),invert))

  
summary(waveI)






# - WAVE I SCHOOL

# -- parent info, parenting, observed ability, grades

#waveIsch <- read_xpt("Data/Core Files - Wave I/Wave I In-School Questionnaire Data/Inschool.xpt") %>%
#  select(AID, any_of(NON_COG_sch)) 
#save(waveIsch, file = paste0(datasets_dir,"waveIsch.rda"))
#load(paste0(datasets_dir,"waveIsch.rda"))
#
#
## recode NAs
#waveIsch <- waveIsch %>% 
#  mutate(ext1 = ifelse(ext1==9, NA, ext1),
#         ext2 = ifelse(ext2==9, NA, ext2),
#         ext3 = ifelse(ext3==9, NA, ext3)) 
#
#summary(waveIsch)







# - WAVE IV

#waveIV <- read_xpt("Data/Core Files - Wave IV/Wave IV In Home Interview Data/wave4/wave4.xpt") %>%
#   select(AID, 
#          education  = H4ED2,
#          income     = H4EC2,
#          health     = H4GH1,
#          birth_year = H4OD1Y, 
#          gender     = BIO_SEX4) 
#save(waveIV, file = paste0(datasets_dir,"waveIV.rda"))
load(paste0(datasets_dir,"waveIV.rda"))


 
waveIV <- waveIV %>% 
 mutate(
   education = cross_edu_4$years_edu[match(education,cross_edu_4$highest_edu)],
   income    = ifelse(income %in% c(9999996,9999998), NA, income),
   health    = invert(health),
   male = case_when(gender == 1 ~ 1, gender == 2 ~ 0, TRUE ~ NA), 
   age  = current_year-birth_year
   ) %>%  
 select( -gender)


summary(waveIV)


 
# - WAVE V
 
#waveV <- read_xpt("Data/Core Files - Wave V/Wave V Mixed-Mode Survey Data/wave5.xpt") %>%
#   select(AID, 
#          education = H5OD11) 
#save(waveV, file = paste0(datasets_dir,"waveV.rda"))
#load(paste0(datasets_dir,"waveV.rda"))
#
#waveV <- waveV %>% 
#  mutate(
#    education = cross_edu_5$years_edu[match(education,cross_edu_5$highest_edu)]
#  ) 
#
#summary(waveV)
 


# -- siblings pairs

#pairs <- read_xpt("Data/Sibling Files/Adolescent Pairs Data/pairs.xpt") %>%
#  select(AID_1, AID_2, FAMID, SIBCL1) %>% 
#  filter(SIBCL1 %in% c("DZ","FS","MZ","UD")) %>%
#  mutate(AID_1= as.numeric(AID_1), AID_2 = as.numeric(AID_2))
#
#siblings <- reshape2::melt(pairs, id.vars = c("FAMID", "SIBCL1"), variable.name = "which_sib", value.name = "AID") %>%
#  mutate(AID = as.character(AID)) %>% select(-which_sib, -SIBCL1) %>% distinct()     
#save(siblings, file = paste0(datasets_dir,"siblings.rda"))
load(paste0(datasets_dir,"siblings.rda"))





# - PGIS

# Wave IV Polygenic Scores - Release 2
pgi <- read_xpt("Data/Genetic Files/Wave IV Polygenic Scores - Release 2/pgs2.xpt") %>%
  select(AID, PSANCEST, any_of(PGIs)) 

# check n European ancestry
nrow(filter(pgi, PSANCEST == "European ancestry"))/nrow(pgi)

# keep only European ancestry
pgi <- filter(pgi, PSANCEST == "European ancestry")






# check age at each wave

min_yob = current_year-max(waveIV$age, na.rm=T)
max_yob = current_year-min(waveIV$age, na.rm=T)
# from 1974 to 1983

# wave I:   1994-1995
# wave II:  1996
# wave III: 2001-2002
# wave IV:  2008
# wave V:   2016-2019


print(paste0("Age range wave I: ",   1994-max_yob,"-",1995-min_yob))
print(paste0("Age range wave II: ",  1996-max_yob,"-",1996-min_yob))
print(paste0("Age range wave III: ", 2001-max_yob,"-",2002-min_yob))
print(paste0("Age range wave IV: ",  2008-max_yob,"-",2008-min_yob))
print(paste0("Age range wave V: ",   2016-max_yob,"-",2019-min_yob))






# ----- MERGE ----

data <- merge(siblings, waveIV, all.x=T, by = "AID") %>% 
  merge(waveI, all.x=T, by = "AID") %>%  
  #merge(waveIsch, all.x=T, by = "AID") %>%
  merge(pgi, all.x=T, by = "AID") 








# ---- BUILD AGGREGATE VARIABLES ----


# parent age at birth
data <- data %>% 
  mutate(IYEAR = as.numeric(paste0("19",IYEAR))) %>%
  mutate(parent_age_birth =  p_age - (IYEAR-birth_year))



# non-cognitive

## earliest available
#data <- data %>% mutate(
#  risk      = coalesce(!!!select(.,contains("risk"))),
#  future    = coalesce(!!!select(.,contains("future"))),
#  conscient = coalesce(!!!select(.,contains("conscient")))) %>%
#  mutate(non_cognitive = rowMeans(across(any_of(NON_COG_VARS)), na.rm=T))

# average across waves
data <- data %>% mutate(
  neuroticism       = rowMeans(across(contains("neu")), na.rm=T),
  conscientiousness = rowMeans(across(contains("con")), na.rm=T),
  extraversion      = rowMeans(across(contains("ext")), na.rm=T)) %>%
  select(-any_of(names(NON_COG_home)),-any_of(names(NON_COG_sch)))



# birth order and spacing

data <- data %>% group_by(FAMID) %>% arrange(age) %>% 
  mutate(birth_order   = dense_rank(age),
         birth_spacing = age - lag(age, default=first(age))) %>% ungroup()





# ---- ALPHA -----

# -- non cognitive
# within
#noncog <- select(data, contains("risk"))
#cronbach.alpha(noncog, CI=T, na.rm=T)
# risk aversion: 0.79

#noncog <- select(data, contains("future"))
#cronbach.alpha(noncog, CI=T, na.rm=T)
# risk aversion: 0.741

#noncog <- select(data, contains("conscient"))
#cronbach.alpha(noncog, CI=T, na.rm=T)
# risk aversion: 0.742

# between
#noncog <- select(data, risk, future, conscient)
#cronbach.alpha(noncog, CI=T, na.rm=T)
# earliest : 0.559
# average across waves: 0.597







# ---- SAVE ----

# select vars
data_sub <- data %>% select(AID,
                        FAMID, 
                        any_of(OUTCOMES), 
                        any_of(ASCRIBED), 
                        contains(PGIs),
                        any_of(PCs),  
                        any_of(ABILITY)) 

# check NA
sapply(data_sub, function(x) sum(is.na(x)))

# remove rows with NA
data_sub <- na.omit(data_sub)


# remove singletons
data_sub <- data_sub %>% group_by(FAMID) %>% filter(n()>1) %>% ungroup()
nrow(data_sub)



# use right types
data_sub <- data_sub %>% 
  mutate(FAMID = as.character(FAMID),
         male = as.factor(as.character(male)))









saveRDS(data_sub, file = paste0(datasets_dir,"clean/data.rds"))

#rm(list = ls())

n_distinct(data_sub$FAMID)
n_distinct(data_sub$AID)


