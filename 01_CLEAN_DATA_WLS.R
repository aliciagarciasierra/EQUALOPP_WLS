

#####################################################################
###################### DATA CLEANING AND RESHAPING #################
#####################################################################


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
    sex = z_sexrsp, # 1 male, 2 female
    birth_year =  z_brdxdy # note that birth month is protected characteristic in WLS, but we don't need it
  )


#--- Parental ages at the time of birth
data <- data %>%
  mutate(
    birth_year_mother = ifelse(z_ge051ma < 0, NA, z_ge051ma), 
    birth_year_father = ifelse(z_he063fa < 0, NA, z_he063fa),
    birth_year = ifelse(birth_year < 0, NA, birth_year)) %>% 
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

# Rename
data <- data %>%
  mutate(
    education_1 = z_edeqyr, education_2= z_rb004red, education_3= z_gb103red, education_4= z_hb103red  ,# years of education (variables with the highest number of valid cases across cohorts)
    occu_3 = z_ocsx1u2 , occu_4 = z_rcu22sp, # occupation (measured as 1970 Duncan SEI, note the 1970 because there are more)
    income_ind_5 = z_gp250rec, income_ind_6 = z_hpu50rec,  # individual level income (total personal income)
    income_hh_5 = z_gp260hec, income_hh_6 = z_hpu60hec, # household level income (total household income)
    wealth_4 = z_rr043rec, wealth_5 = z_gr100rpc, # wealth (net worth at the family level)
    health_self_4 = z_mx001rer, health_self_5 = z_ix001rer,    health_self_6    = z_jx001rer, health_self_7 = z_q1x001rer, # self-reported health (from 1 very poor to 5 excellent)
    health_illness_4  = z_mx117rec, health_illness_5 = z_ix117rec, health_illness_6 = z_jx117rec, # total number of illnesses 
    health_hospital_4 = z_mx008rer, health_hospital_5 = z_ixhi08re # number of times in the hospital in previous year (overnight stay)
  )

# Clean (sending negative values to NA)
data <- data %>%
  mutate_at(vars(education_1, education_2, education_3, education_4, 
                 occu_3, 
                 income_ind_5, income_ind_6, income_hh_5, income_hh_6, 
                 wealth_4, wealth_5, 
                 health_self_4, health_self_5, health_self_6, health_self_7,
                 health_illness_4, health_illness_5, health_illness_6,
                 health_hospital_4, health_hospital_5),
            ~ ifelse(. < 0, NA, .))  # Replace negative values with NA

# Combine averaging to have more stable measures
data <- data %>%
  mutate(
    education=rowMeans(cbind(education_1, education_2, education_3, education_4), na.rm=TRUE),
    occupation = rowMeans(cbind(occu_3, occu_4), na.rm=TRUE),
    income_ind = rowMeans(cbind(income_ind_5, income_ind_6), na.rm=TRUE),
    income_hh = rowMeans(cbind(income_hh_5, income_hh_6), na.rm=TRUE),
    wealth = rowMeans(cbind(wealth_4, wealth_5), na.rm=TRUE),
    health_self = rowMeans(cbind(health_self_4, health_self_5, health_self_6, health_self_7), na.rm=TRUE),
    health_illness = rowMeans(cbind(health_illness_4, health_illness_5, health_illness_6), na.rm=TRUE),
    health_hospital = rowMeans(cbind(health_hospital_4, health_hospital_5), na.rm=TRUE)
    )

# Principal components for the combined health variable

pcdata <- data %>% # extract variables 
  select(ID, health_self_4, health_illness_4, health_hospital_4)

nb_comp <- estim_ncpPCA(pcdata %>% select(-ID)) 
pca_result <- imputePCA(pcdata %>% select(-ID), ncp = nb_comp$ncp)  # PCA with missing data imputation

pcdata <- pcdata %>%
  mutate(health_pc = pca_result$completeObs[, 1]) %>% # extract the first principal component (PC1) and center it
  mutate(health_pc = health_pc - mean(health_pc))

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
    pgi_well_being = pgs_swb_gwas,
    pc1noncog = pc1_shuffled,
    pc2noncog = pc2_shuffled,
    pc3noncog = pc3_shuffled,
    pc4noncog = pc4_shuffled,
    pc5noncog = pc5_shuffled,
    pc6noncog = pc6_shuffled,
    pc7noncog = pc7_shuffled,
    pc8noncog = pc8_shuffled,
    pc9noncog = pc9_shuffled,
    pc10noncog = pc10_shuffled
  )

# Merge with main data

data <-merge(data, pgi_noncog, by="pgiID", all.y=TRUE)


########################## SELECT FINAL SAMPLE ##########################

# Select all the relevant variables to extract them from the sample
siblings <- data%>%
  select(ID, familyID, withinID, # IDs
         sex, birth_year, mother_age_birth, father_age_birth, birth_order,  # demographics 
         education, # education
         occupation, # occupation
         income_ind, income_hh, #income
         wealth, #wealth
         health_self, health_illness, health_pc, #health
         pgiID, pgi_education, pgi_cognitive, pgi_math_exam, pgi_math_ability, # PGIs cog
         pgi_depression, pgi_well_being, pgi_neuroticism, # PGIs noncog
         pc1cog, pc2cog, pc3cog, pc4cog, pc5cog, pc6cog, pc7cog, pc8cog, pc9cog, pc10cog, # principal components cog
         pc1noncog, pc2noncog, pc3noncog, pc4noncog, pc5noncog, pc6noncog, pc7noncog, pc8noncog, pc9noncog, pc10noncog, # principal components non-cog
         )

# Label NAs rightly
siblings <- siblings %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))  # Applies to all columns

# There are four cases that are not labelled properly and show 3 or 4 siblings, delete them
siblings<- siblings[!(siblings$withinID %in% c(3, 4)), ]

# Find those families who have siblings
filter<- siblings%>% 
  group_by(familyID) %>% 
  summarise(count = n_distinct(ID)) %>%
  filter(count > 1)%>% 
  ungroup()

# Filter only those with siblings from the original sample
siblings <- siblings%>% 
  filter(familyID %in% unique(filter$familyID))

# Count unique observations
n_distinct(siblings$ID) # 4140
n_distinct(siblings$familyID) # 2070
 
# If we want to select only complete observations it would be...
siblings<-siblings[complete.cases(siblings),] #3313 cases, nested in 1970 families

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

















