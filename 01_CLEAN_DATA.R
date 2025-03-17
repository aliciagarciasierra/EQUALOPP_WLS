

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

# Rename
data <- data %>% rename(extraversion      = z_rh001rec, openness      = z_rh003rec, neuroticism = z_rh005rec, 
                        conscientiousness = z_rh007rec, agreeableness = z_rh009rec)

# Clean (sending negative values to NA)
data <- data %>%
  mutate_at(vars(any_of(OBSERVED_NON_COG)),
            ~ ifelse(. < 0, NA, .))  # Replace negative values with NA

# check distributions
summary(select(data, any_of(OBSERVED_NON_COG)))


########################## SELECT FINAL VARIABLES  ##########################

# Select all the relevant variables to extract them from the sample
siblings <- data %>%
  select(ID, familyID, withinID, pgiID,                 # IDs
         any_of(ASCRIBED),                              # demographics 
         any_of(OUTCOMES),                              # outcomes
         any_of(PGI_COG),                               # PGIs cog
         any_of(PGI_NON_COG),                           # PGIs noncog
         all_of(PC_COG),                                # principal components
         any_of(OBSERVED_COG), any_of(OBSERVED_NON_COG), # observed abilities
  )

# Label NAs rightly
siblings <- siblings %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))  # Applies to all columns

# There are a few cases that are not labelled properly and show 3 or 4 siblings, delete them
siblings <- siblings[!(siblings$withinID %in% c(3, 4)), ]

# check variables' types
str(siblings)

# convert familyID and sex to avoid scaling them
siblings <- siblings %>% 
  mutate(familyID = as.character(familyID),
         sex      = as.factor(as.character(sex)))


########################## MULTIPLE IMPUTATION  ##########################

# Perform multiple imputation
imputed_data <- mice(siblings, m = m, maxit = 20, 
                     method = 'cart', seed = 123) # When code is ready, use m=25, maxit=20
# We use cart because it's better for handling a mix of categorical and continuous variables than pmm

# View imputed data summary
summary(imputed_data)

#------ DELETE OUTCOME VALUES IMPUTED (FOLLOWING VON HIPPEL, 2007)
# Count NAs for each outcome variable
na_counts <- sapply(OUTCOMES, function(var) sum(is.na(siblings[[var]])))
na_counts

# Function to replace imputed outcome values with NA
replace_imputed_with_na <- function(siblings, imputed_data, OUTCOMES) {
  # Create a copy of the imputed dataset to modify
  clean_data <- imputed_data
  # Loop through each outcome variable
  for (var in OUTCOMES) {
    if (var %in% names(siblings)) {
      # Find indices where the original data was NA but the imputed data has values
      imputed_indices <- which(!is.na(clean_data[[var]]) & is.na(siblings[[var]]))
      # Replace these imputed values with NA in the imputed dataset
      clean_data[[var]][imputed_indices] <- NA
    }
  }
  
  return(clean_data)
}

# Extract imputed datasets from the imputed_data object (list of m datasets)
imputed_datasets <- complete(imputed_data, action = "all")

# Apply the function to each dataset in the imputed_datasets list
imputed_datasets_without_y <- mclapply(imputed_datasets, function(imputed_dataset) {
  replace_imputed_with_na(siblings, imputed_dataset, OUTCOMES)
}, mc.cores = 4)

# Check that in the imputed_datasets_without_y there are outcomes with NAs
first_imputed_dataset <- imputed_datasets_without_y[[1]]


# If we want to select only complete observations without implementing multiple imputation:
#siblings <- siblings[complete.cases(siblings),] 
#n_distinct(siblings$ID)       # 5107
#n_distinct(siblings$familyID) # 4319

########################## REMOVE THOSE WITH UNREALISTIC PARENTAL AGES  ##########################
# Remove those with unrealistic parental ages
filtered_datasets <- lapply(imputed_datasets_without_y, function(dataset) {
  dataset %>%
    filter(mother_age_birth >= 14 & father_age_birth >= 14)
})

#Check
first_imputed_dataset <- filtered_datasets[[1]]
summary(first_imputed_dataset$father_age_birth) # works

########################## PRINCIPAL COMPONENTS FOR HEALTH  ##########################

# Apply PCA to each filtered dataset
final_datasets <- lapply(filtered_datasets, function(dataset) {
  
  # Extract relevant health variables (you can adjust this list of variables if needed)
  pcdata <- dataset %>%
    select(ID, health_self, health_illness, health_hospital)
  
  # Estimate the number of components to retain
  nb_comp <- estim_ncpPCA(pcdata %>% select(-ID)) 
  
  # Perform PCA with missing data imputation
  pca_result <- imputePCA(pcdata %>% select(-ID), ncp = nb_comp$ncp)
  
  # Create a new variable for the first principal component and center it
  pcdata <- pcdata %>%
    mutate(
      health_pc = pca_result$completeObs[, 1]  # First PC (PC1)
    ) %>%
    mutate(health_pc = health_pc - mean(health_pc))  # Center the first PC
  
  # Select only the ID and health_pc columns
  pcdata <- pcdata %>%
    select(ID, health_pc)
  
  # Merge the PCA results back to the original dataset
  dataset <- merge(dataset, pcdata, by = "ID", all.x = TRUE)
  
  return(dataset)
})


########################## KEEP ONLY TWO-SIBLINGS FAMILIES  ##########################

# Apply the filtering process to each dataset in the datasets_without_imputed_y list
final_datasets <- lapply(final_datasets, function(dataset) {
  dataset %>%
    group_by(familyID) %>%
    filter(n() >= 2) %>%
    ungroup()
})

# Count the number of siblings in each family for all datasets in final_datasets
n_siblings_list <- lapply(final_datasets, function(dataset) {
  n_siblings <- dataset %>% 
    group_by(familyID) %>% 
    summarise(count = n_distinct(ID)) %>%
    ungroup()
  
  return(n_siblings)
})

#Check
n_siblings_first_dataset <- n_siblings_list[[1]]
summary(n_siblings_first_dataset$count) # only 2

########################## SAVE ALL THE IMPUTED DATSETS  ##########################

saveRDS(final_datasets, file = "data/final_datasets.rds")














