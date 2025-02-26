

#####################################################################
###################### DATA CLEANING AND RESHAPING #################
#####################################################################

source("00_MASTER_WLS.R")


# FAST READ FROM RDS

data       <- readRDS("data/data.rds")
pgi_cog    <- readRDS("data/pgi_cog.rds")
pgi_noncog <- readRDS("data/pgi_noncog.rds")


############################ IDENTIFIERS ########################

#--- first, an unique individual ID
data$ID<- paste(data$familypub, data$personid, sep = "_")
n_distinct(data$ID) #check how many unique obs

#--- second, a family ID (rename only)
data$familyID<-data$familypub
n_distinct(data$familypub) #check how many unique obs

#--- third, a within family identifier (rename only)
data$withinID<-data$personid

#---- fourth, an ID for PGIs merging
data$pgiID<-paste(data$idpub,data$rtype, sep = "_")


########################## DEMOGRAPHICS ##########################

#----- Basic ones
data <- data %>%
  mutate(
    # race = z_ie020re, # 1 white, 2 other (we don't need it, but leave it here just in case)
    sex = z_sexrsp, # 1 male (0), 2 female (1)
    birth_year =  z_brdxdy # note that birth month is protected characteristic in WLS, but we don't need it
  )


#--- Parental ages at the time of birth
data <- data %>%
  mutate(
    birth_year_mother = ifelse(z_ge051ma  < 0, NA, z_ge051ma), 
    birth_year_father = ifelse(z_he063fa  < 0, NA, z_he063fa),
    birth_year        = ifelse(birth_year < 0, NA, birth_year),
    sex               = case_when(sex == 1 ~ 0, sex == 2 ~ 1, TRUE ~ NA_real_ )) %>% 
  mutate(
    mother_age_birth = birth_year- birth_year_mother,
    father_age_birth = birth_year- birth_year_father)


#----- Birth order
data <- data %>%
  group_by(familyID) %>% 
  mutate(birth_order = ifelse(rank(birth_year) == 1, 1, 2)) %>% 
  ungroup()  #
# Note that birth space is not relevant here because since they are only two siblings it doesn't vary



########################## OUTCOMES ######################################

# Check missing cases in the educational variables
missing_summary <- data %>%
  summarise(
    valid_yoe_1 = sum(!is.na(z_edeqyr)), # R03 Equivalent years of regular education.
    valid_yoe_2 = sum(!is.na(z_rb004red)), # R04 Summary of equivalent yrs of regular education based on most recent degree.
    valid_yoe_3 = sum(!is.na(z_gb103red)), # R05 How many years of education does R have based on his or her highest degree?
    valid_yoe_4 = sum(!is.na(z_mx001rer)) #R06 Summary of equivalent years of regular education based on highest degree.
  )


EDU         <- c(education_1       = "z_edeqyr",   education_2       = "z_rb004red", education_3      = "z_gb103red", education_4   = "z_hb103red") # years of education
OCCU        <- c(occu_3            = "z_ocsxcru2", occu_4           = "sfu57ref") # occupation (measured as 1970 Duncan SEI, note the 1970 because there are more)
INC_IND     <- c(income_ind_5      = "z_gp250rec", income_ind_6      = "z_hpu50rec") # individual level income (total personal income)
INC         <- c(income_5       = "z_gp260hec", income_6       = "z_hpu60hec") # household level income (total household income)
WEALTH      <- c(wealth_4          = "z_rr043rec", wealth_5          = "z_gr100rpc", wealth_6         = "z_hr100rpc")  # wealth (net worth at the family level)
HEALTH_S    <- c(health_self_4     = "z_mx001rer", health_self_5     = "z_ix001rer", health_self_6    = "z_jx001rer", health_self_7 = "z_q1x001rer") # self-reported health (from 1 very poor to 5 excellent)
HEALTH_ILL  <- c(health_illness_4  = "z_mx117rec", health_illness_5  = "z_ix117rec", health_illness_6 = "z_jx117rec") # total number of illnesses
HEALTH_HOSP <- c(health_hospital_4 = "z_mx008rer", health_hospital_5 = "z_ixhi08re") # number of times in the hospital in previous year (overnight stay)



# Rename
data <- data %>% rename(!!!EDU, !!!OCCU, !!!INC_IND, !!!INC, !!!WEALTH, !!!HEALTH_S, !!!HEALTH_ILL, !!!HEALTH_HOSP)


# Clean (sending negative values to NA)

# -- wealth (only negative with label)
data <- data %>%
  mutate_at(vars(names(WEALTH)), ~ ifelse(. %in% -31:-1, NA, .))

# -- other (all negative)
data <- data %>%
  mutate_at(vars(names(EDU), names(OCCU), names(INC_IND), names(INC), names(HEALTH_S), names(HEALTH_ILL), names(HEALTH_HOSP)),
            ~ ifelse(. < 0, NA, .))


# Combine averaging to have more stable measures
data <- data %>%
  mutate(
    education       = rowMeans(select(., all_of(names(EDU))),         na.rm=TRUE),
    occupation      = rowMeans(select(., all_of(names(OCCU))),        na.rm=TRUE),
    income_ind      = rowMeans(select(., all_of(names(INC_IND))),     na.rm=TRUE),
    income          = rowMeans(select(., all_of(names(INC))),         na.rm=TRUE),
    wealth          = rowMeans(select(., all_of(names(WEALTH))),      na.rm=TRUE),
    health_self     = rowMeans(select(., all_of(names(HEALTH_S))),    na.rm=TRUE),
    health_illness  = rowMeans(select(., all_of(names(HEALTH_ILL))),  na.rm=TRUE),
    health_hospital = rowMeans(select(., all_of(names(HEALTH_HOSP))), na.rm=TRUE)
    )


# Principal components for the combined health variable
pcdata <- data %>% # extract variables 
  select(ID, health_self_4, health_illness_4, health_hospital_4)

nb_comp <- estim_ncpPCA(pcdata %>% select(-ID)) 
pca_result <- imputePCA(pcdata %>% select(-ID), ncp = nb_comp$ncp)  # PCA with missing data imputation

pcdata <- pcdata %>%
  mutate(
    health_pc = pca_result$completeObs[, 1]) # %>% # extract the first principal component (PC1) and center it
  #mutate(health_pc = health_pc - mean(health_pc)))

pcdata <- pcdata %>%
  select(ID, health_pc)

# Merge with the original data
data <- merge(data, pcdata, by = "ID", all.x = TRUE)





########################## PGIs cognitive ######################################

# Relabel and select the variables of interest
pgi_cog<- pgi_cog %>%
  mutate(
    pgiID = paste(idpub,rtype, sep = "_")
  )%>%
  select(
    pgiID,
    pgi_education = pgs_ea3_gwas,
    pgi_cognitive = pgs_cp_gwas,
    pgi_math_exam = pgs_hm_mtag,
    pgi_math_ability = pgs_ma_mtag,
    pc1cog = pc1_shuffled,
    pc2cog = pc2_shuffled,
    pc3cog = pc3_shuffled,
    pc4cog = pc4_shuffled,
    pc5cog = pc5_shuffled,
    pc6cog = pc6_shuffled,
    pc7cog = pc7_shuffled,
    pc8cog = pc8_shuffled,
    pc9cog = pc9_shuffled,
    pc10cog = pc10_shuffled
  )

# Merge with main data

data <-merge(data, pgi_cog, by="pgiID", all.y=TRUE)


########################## PGIs non-cognitive ######################################

# Relabel and select the variables of interest
pgi_noncog<- pgi_noncog %>%
  mutate(
    pgiID = paste(idpub,rtype, sep = "_")
  )%>%
  select(
    pgiID,
    pgi_depression = pgs_dep_gwas,
    pgi_neuroticism = pgs_neur_gwas,
    pgi_well_being = pgs_swb_gwas
  )

# Merge with main data

data <-merge(data, pgi_noncog, by="pgiID", all.y=TRUE)


########################## OBSERVED ABILITY cognitive ##########################

# check valids
valid_summary <- data %>%
  summarise(
    total_cognition_grad_4 = sum(!is.na(ri001re)), # 
    total_cognition_sib_4  = sum(!is.na(si001re)), # 
    cog_test               = sum(!is.na(z_gwiiq_bm)), # Adolescent cognitive test score
    centile_rank_cog_test  = sum(!is.na(z_ghncr_bm)), # Centile rank based on national test takers for Henmon-Nelson test score from junior year
    
  )

valid_summary 
# use collected by phone

# Rename
data <- data %>% rename(IQ = z_gwiiq_bm, centile_rank_IQ = z_ghncr_bm)

# Clean (sending negative values to NA)
data <- data %>%
  mutate_at(vars(IQ, centile_rank_IQ),
            ~ ifelse(. < 0, NA, .))  # Replace negative values with NA

# check distributions
summary(select(data, IQ, centile_rank_IQ))


########################## OBSERVED ABILITY non-cognitive ##########################

# check valids
valid_summary <- data %>%
  summarise(
    valid_extra_1 = sum(!is.na(z_rh001rec)), # collected by phone
    valid_openn_1 = sum(!is.na(z_rh003rec)), # collected by phone
    valid_neuro_1 = sum(!is.na(z_rh005rec)), # collected by phone
    valid_consc_1 = sum(!is.na(z_rh007rec)), # collected by phone
    valid_agree_1 = sum(!is.na(z_rh009rec)), # collected by phone
    
    valid_extra_2 = sum(!is.na(z_mh001rec)), # collected by mail
    valid_openn_2 = sum(!is.na(z_mh032rec)), # collected by mail
    valid_neuro_2 = sum(!is.na(z_mh025rec)), # collected by mail
    valid_consc_2 = sum(!is.na(z_mh017rec)), # collected by mail
    valid_agree_2 = sum(!is.na(z_mh009rec))  # collected by mail
  )

valid_summary 
# use collected by phone in wave 4

# Rename
data <- data %>% rename(extraversion      = z_rh001rec, openness      = z_rh003rec, neuroticism = z_rh005rec, 
                        conscientiousness = z_rh007rec, agreeableness = z_rh009rec)

# Clean (sending negative values to NA)
data <- data %>%
  mutate_at(vars(any_of(OBSERVED_NON_COG)),
            ~ ifelse(. < 0, NA, .))  # Replace negative values with NA

# check distributions
summary(select(data, any_of(OBSERVED_NON_COG)))





########################## SELECT FINAL SAMPLE ##########################

# Select all the relevant variables to extract them from the sample
siblings <- data %>%
  select(ID, familyID, withinID, pgiID,                 # IDs
         any_of(ASCRIBED),                              # demographics 
         any_of(OUTCOMES),                              # outcomes
         any_of(PGI_COG),                               # PGIs cog
         any_of(PGI_NON_COG),                           # PGIs noncog
         all_of(PC_COG),                                # principal components cog
         any_of(OBSERVED_COG), any_of(OBSERVED_NON_COG) # observed abilities
  )

# Label NAs rightly
siblings <- siblings %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))  # Applies to all columns

# There are four cases that are not labelled properly and show 3 or 4 siblings, delete them
siblings<- siblings[!(siblings$withinID %in% c(3, 4)), ]


# If we want to select only complete observations it would be...
siblings <- siblings[complete.cases(siblings),] 
n_distinct(siblings$ID)       # 5107
n_distinct(siblings$familyID) # 4319

# keep only families with at least two kids
siblings <- siblings %>%
  group_by(familyID) %>%
  filter(n() >= 2) %>%
  ungroup()
n_distinct(siblings$ID) # 1576
n_distinct(siblings$familyID) # 788


# center health PC
siblings <- siblings %>% mutate(health_pc = health_pc - mean(health_pc))


# Check the number of siblings in each family
n_siblings <- siblings %>% 
  group_by(familyID) %>% 
  summarise(count = n_distinct(ID)) %>%
  ungroup()

summary(n_siblings$count)
# only 2

saveRDS(siblings, file = "data/siblings.rds")



# Check age at ability measurement
1977-min(siblings$birth_year)
1977-max(siblings$birth_year)







####################--------  JUST IN CASE PARENTING -------- #################################

#Note that despite it is prepared to be included in the analyses, I haven't do so because we lose plenty of cases. Something to discuss.

# There are two types of variables, one that captures higher values positive parenting, and one type
# in which higher values are negative parenting. I am first going to rescale the second type
# so that we have all the variables in the first direction

if(FALSE){
  
#------------------ Negative parenting

# Identify variables
neg_parenting <- data %>%
  select( ID,
          nw028rer, nw029rer, nw030rer, nw031rer,
          nw032rer, nw033rer, nw034rer, nw035rer,
          nw036rer, nw037rer, nw038rer, nw039rer
  )

# Function to clean the missing values and reverse the scale
transform_scale <- function(x) {
  x[x == -3] <- NA  # Remove -3 by replacing with NA
  return(3 - x)     # Reverse the scale
}

# Clean the neg_parenting variables
for (i in 28:39) {
  column_name <- paste0("nw", sprintf("%03d", i), "rer")
  neg_parenting[[column_name]] <- transform_scale(neg_parenting[[column_name]])
}

# Create neg_parenting variable
neg_parenting <-neg_parenting %>%
  mutate(
    neg_parenting_avg = rowMeans(select(., nw028rer:nw039rer), na.rm = TRUE)
  )

# Select and extract 
neg_parenting <- neg_parenting %>%
  select(ID, neg_parenting_avg)

#------------------ Positive parenting

# Identify variables
pos_parenting <- data %>%
  select(
    ID,
    nw004rer, nw005rer, nw006rer, nw007rer,
    nw008rer, nw009rer, nw010rer, nw011rer,
    nw012rer, nw013rer, nw014rer, nw015rer,
    nw016rer, nw017rer, nw018rer, nw019rer,
    nw020rer, nw021rer, nw022rer, nw023rer,
    nw024rer, nw025rer, nw026rer, nw027rer
  )

# Calculate the row-wise mean of the pos_parenting variables, excluding NA values
pos_parenting <- pos_parenting %>%
  mutate(
    pos_parenting_avg = rowMeans(select(., nw004rer:nw027rer), na.rm = TRUE)
  )

# Select and extract 
pos_parenting <- pos_parenting %>%
  select( ID, pos_parenting_avg)

#------------------ Merge both parenting variables

parenting<-merge(pos_parenting, neg_parenting, by="ID")

# Combine both since after transformation they go in the same direction
parenting <-parenting %>%
  mutate(parenting = rowMeans(cbind(pos_parenting_avg, neg_parenting_avg), na.rm = TRUE)) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>% # Label NAs correctly
  select(ID, parenting)

#------------------ Merge with general dataset

merge <- merge (siblings, parenting, by="ID", all.x=TRUE)

#--- How many cases do we lose if we include parenting?
complete_siblings_without_parenting<- siblings[complete.cases(siblings),] #2697 cases
complete_siblings_with_parenting<- merge[complete.cases(merge),] #1081 cases

} # closing of the if(FALSE) to prevent this section fron running

















