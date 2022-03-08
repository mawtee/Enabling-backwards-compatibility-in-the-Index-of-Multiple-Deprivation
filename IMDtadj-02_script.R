################################################################################
########################## Time-adjusted IMD ( 02 ) ############################
################################################################################

# Author: Matthew Tibbles 
#-----------------------------

# Description
################################################################################

# This script loads/generates dependent/control variables used in estimation of
# REWB models for IMD/time-adjusted IMD.

# It generates the following variable outputs:
# 1 - SAMHI mental health index (DV)
# 2 - Residential area classification indicator (Control)
# 3 - Median age (Control)
# 4 - Proportion of male residents (Control)
# 5 - Population density (Control)
# 6 - Standardized mortality rate (Control)

# All variables are combined with IMD data, to create balanced panel dataset
# between 2011 and 2019 at the (aggregated) LSOA level.


# Initialize working environment
################################################################################

rm(list=ls())

library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(stringr)
library(reshape2)


# Load LSOA boundary data frame
LSOA_bound.df <- readRDS("Time Adjusted IMD/Data/Derived/LSOA_bound.rds")


# Mental Health Index (SAMHI)
################################################################################

# Load data
samhi.df <- read.csv("Time Adjusted IMD/Data/Source/samhi_v3.01_LSOA_2011-19.csv") 

# Drop redundant variables
samhi.df <- samhi.df[c("lsoa11", "year", "samhi_index")]

# Rename
names(samhi.df) <- c("LSOA_code", "year", "samhi")

# Add boundary changes
samhi.df <- dplyr::full_join(samhi.df, LSOA_bound.df,
                             by = "LSOA_code", all.x=TRUE)

# Drop irretrievable LSOAs + non-England LSOAs
samhi.df <-subset(samhi.df, change != "X")
samhi.df <- samhi.df[grepl("^E",samhi.df$LSOA_code),]

# Aggregate 2011 split LSOAs 
attach(samhi.df)
samhi_agg.df <- samhi.df %>%
  dplyr::group_by(LSOA_code_old, year) %>%
  dplyr::mutate(across(c(samhi), ~
                         case_when(change == "S" ~ mean(.),
                                   TRUE ~ .))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(LSOA_code_agg =case_when(change == "S" ~ LSOA_code_old,
                                         TRUE ~ LSOA_code)) %>%
  dplyr::distinct(LSOA_code_agg, year, .keep_all = TRUE)
detach(samhi.df)

# Drop redundant variables
samhi_agg.df <- samhi_agg.df[, c("LSOA_code_agg", "year", "samhi")]


#  Residential Area Classification
################################################################################

# Load data
resarea.df <- readxl::read_excel("Time Adjusted IMD/Data/Source/res_area_LSOA_2011.xls",
                                  sheet = "Clusters by SOA")

# Drop redundant variables
resarea.df <- resarea.df[, c(1,6,7)]

# Rename
names(resarea.df) <- c("LSOA_code", "resarea_code", "resarea_name")

# Add boundary changes
resarea.df <- dplyr::full_join(resarea.df, LSOA_bound.df,
                                      by = "LSOA_code", all.x=TRUE)
# Drop irretrievable LSOAs + non-England LSOAs
resarea.df <-subset(resarea.df, change != "X")
resarea.df <- resarea.df[grepl("^E",resarea.df$LSOA_code),]

# Aggregate 2011 split LSOAs 
attach(resarea.df)
resarea_agg.df <- resarea.df %>%
  dplyr::group_by(LSOA_code_old) %>%
  dplyr::mutate(resarea_code = replace(resarea_code, n_distinct(resarea_code)!=1, 0)) %>%
  dplyr::mutate(across(c(resarea_code), ~
                         case_when(change == "S" ~ mean(.),
                                   TRUE ~ .))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(LSOA_code_agg =case_when(change == "S" ~ LSOA_code_old,
                                         TRUE ~ LSOA_code)) %>%
  dplyr::filter(resarea_code != 0) %>%
  dplyr::distinct(LSOA_code_agg, .keep_all = TRUE)
detach(resarea.df)

# Drop redundant variables
resarea_agg.df <- resarea_agg.df[, c("LSOA_code_agg", "resarea_code", "resarea_name")]


# Median Age
################################################################################

### Define function for calculation of median age by year
medage.fun <- function(Fyear) {

  # Load data
  ageFyear.df <- as.data.frame(readxl::read_excel(paste0("Time Adjusted IMD/Data/Source/pop_by_age_LSOA_",Fyear,".xls"),
                                                   sheet = paste0("Mid-",Fyear," Persons")))
  # For 2011 only:
  if (Fyear == 2011) {
     # Extract LSOA code (and trim)
     ageFyear.df[, 1] <- stringr::str_extract(string = ageFyear.df[, 1], pattern = "[^:]+")
     ageFyear.df[, 1]  <- stringr::str_trim(ageFyear.df[, 1])
     # Remove age prefix from columns
     names(ageFyear.df) <- substring(names(ageFyear.df),5)
  }
  
  # For all years except 2011:
  else {
     # Drop LA estimates
     ageFyear.df <- subset(ageFyear.df, !is.na(ageFyear.df[, 3]) )
  }
  
  # Reshape from wide to long 
  ageFyear_long.df <- gather(ageFyear.df,
                             key = "age_group",
                             value = "frequency",
                             c("0":"90+"),
                             -c(1))
  rm(ageFyear.df)
  gc(full = TRUE) 
  
  # Drop redundant variables
  freq <- ncol(ageFyear_long.df)
  ageg <- freq-1
  ageFyear_long.df <- ageFyear_long.df[, c(1, ageg, freq)]
  
  # Variable names
  names(ageFyear_long.df) <- c("LSOA_code", "age_group", "frequency")
  
  # Age group var as numeric
  ageFyear_long.df$age_group <- as.character(ageFyear_long.df$age_group)
  ageFyear_long.df$age_group <- stringr::str_replace(ageFyear_long.df$age_group, "[+]", "")
  ageFyear_long.df$age_group <- as.numeric(ageFyear_long.df$age_group)
  
  # Expand age group frequencies as rows
  ageFyear_long2.df <- ageFyear_long.df[rep(1:nrow(ageFyear_long.df), ageFyear_long.df$frequency),]
  
  rm(ageFyear_long.df)
  gc(full = TRUE) 
  
  # Calculate median age by LSOA
  ageFyear_med.df <- ageFyear_long2.df %>%
     dplyr::group_by(LSOA_code) %>%
     dplyr::summarise(medage = median(age_group)) %>%
     dplyr::ungroup()
  
  rm(ageFyear_long2.df)
  gc(full = TRUE) 
  
  # Add boundary changes
  ageFyear_med.df <- dplyr::full_join(ageFyear_med.df, LSOA_bound.df,
                                      by = "LSOA_code", all.x=TRUE)
  
  # Drop irretrievable LSOAs + non-England LSOAs
  ageFyear_med.df <-subset(ageFyear_med.df, change != "X")
  ageFyear_med.df <- ageFyear_med.df[grepl("^E",ageFyear_med.df$LSOA_code),]
  
  # Aggregate 2011 split LSOAs 
  attach(ageFyear_med.df)
  ageFyear_med_agg.df <- ageFyear_med.df %>%
     dplyr::group_by(LSOA_code_old) %>%
     dplyr::mutate(across(c(medage), ~
                             case_when(change == "S" ~ mean(.),
                                       TRUE ~ .))) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(LSOA_code_agg =case_when(change == "S" ~ LSOA_code_old,
                                            TRUE ~ LSOA_code)) %>%
     dplyr::distinct(LSOA_code_agg, .keep_all = TRUE) %>%
     dplyr::mutate(year = Fyear)
  detach(ageFyear_med.df)
  
  rm(ageFyear_med.df)
  gc(full = TRUE) 
  
 
  return(ageFyear_med_agg.df)
  
  rm(ageFyear_med_agg.df)
  gc(full = TRUE)
  
}
# Apply function over years 
Fyear.vec <- c("2011":"2019")
medage.list <- lapply(Fyear.vec, FUN = medage.fun)

# Append years
medage_agg.df <- dplyr::bind_rows(medage.list[[1]], medage.list[[2]], medage.list[[3]],
                                  medage.list[[4]], medage.list[[5]], medage.list[[6]],
                                  medage.list[[7]], medage.list[[8]], medage.list[[9]])
rm(medage.list)

# Drop redundant variables
medage_agg.df <- medage_agg.df[, c("LSOA_code_agg", "year", "medage")]


# Gender
################################################################################
popgen.df <- read.csv("Time Adjusted IMD/Data/Source/pop_by_gender_LSOA_2011-19.csv", sep = ',')

# Extract LSOA_code (and trim)
popgen.df$LSOA_code <- stringr::str_extract(string = popgen.df$LSOA_code, pattern = "[^:]+")
popgen.df$LSOA_code <- stringr::str_trim(popgen.df$LSOA_code)

# Proportion of male residents
popgen.df$male_share <- (popgen.df$male/popgen.df$total)*100

# Generate year variable
popgen.df <- popgen.df %>%                             
  dplyr::group_by(LSOA_code) %>%
  dplyr::mutate(obno = row_number()) %>%
  ungroup()
popgen.df$year <- plyr::mapvalues(popgen.df$obno, from = c(1:9),
                                   to = c(2011:2019))

# Add boundary changes
popgen.df <- dplyr::full_join(popgen.df, LSOA_bound.df,
                                by = "LSOA_code", all.x=TRUE)

# Drop irretrievable LSOAs + non-England LSOAs
popgen.df <-subset(popgen.df, change != "X")
popgen.df <- popgen.df[grepl("^E",popgen.df$LSOA_code),]

# Aggregate 2011 split LSOAs 
attach(popgen.df)
popgen_agg.df <- popgen.df %>%
  dplyr::group_by(LSOA_code_old, year) %>%
  dplyr::mutate(across(c(male_share), ~
                         case_when(change == "S" ~ mean(.),
                                   TRUE ~ .))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(LSOA_code_agg =case_when(change == "S" ~ LSOA_code_old,
                                         TRUE ~ LSOA_code)) %>%
  dplyr::distinct(LSOA_code_agg, year, .keep_all = TRUE)
detach(popgen.df)

# Drop redundant variables
popgen_agg.df <- popgen_agg.df[, c("LSOA_code_agg", "year", "male_share")]


# Population density
################################################################################
popdens.fun <- function(Fyear) {
  
  # Load data
  popdensFyear.df <- as.data.frame(readxl::read_excel(paste0("Time Adjusted IMD/Data/Source/popdens_LSOA_",Fyear,".xls"),
                                                      sheet = paste0("Mid-",Fyear," Population Density")))
  # Trim LSOA string
  popdensFyear.df[, 1]  <- stringr::str_trim(popdensFyear.df[, 1])

  # Add boundary changes
  popdensFyear.df <- dplyr::full_join(popdensFyear.df, LSOA_bound.df,
                                      by = "LSOA_code", all.x=TRUE)
  
  # Drop irretrievable LSOAs + non-England LSOAs
  popdensFyear.df <-subset(popdensFyear.df, change != "X")
  popdensFyear.df <- popdensFyear.df[grepl("^E",popdensFyear.df$LSOA_code),]
  
  # Aggregate 2011 split LSOAs 
  attach(popdensFyear.df)
  popdensFyear_agg.df <- popdensFyear.df %>%
    dplyr::group_by(LSOA_code_old) %>%
    dplyr::mutate(across(c(popdens), ~
                           case_when(change == "S" ~ mean(.),
                                     TRUE ~ .))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LSOA_code_agg =case_when(change == "S" ~ LSOA_code_old,
                                           TRUE ~ LSOA_code)) %>%
    dplyr::distinct(LSOA_code_agg, .keep_all = TRUE) %>%
    dplyr::mutate(year = Fyear)
  detach(popdensFyear.df)
  
  return(popdensFyear_agg.df)
  
}

# Apply function over years 
Fyear.vec <- c("2011":"2019")
popdens_agg.list <- lapply(Fyear.vec, FUN = popdens.fun)

# Append years
popdens_agg.df <- dplyr::bind_rows(popdens_agg.list[[1]], popdens_agg.list[[2]], popdens_agg.list[[3]],
                                   popdens_agg.list[[4]], popdens_agg.list[[5]], popdens_agg.list[[6]],
                                   popdens_agg.list[[7]], popdens_agg.list[[8]], popdens_agg.list[[9]])
rm(popdens_agg.list)

# Drop redundant variables          
popdens_agg.df <- popdens_agg.df[, c("LSOA_code_agg", "year", "popdens")]
popdens_agg.df$popdens <- popdens_agg.df$popdens/100

# Mortality rate     
##########################

# Observed deaths 
################################################################################

# ONS Deaths by LSOA
#-------------------------

# Load data
deaths_LSOA.df <- readxl::read_excel("Time Adjusted IMD/Data/Source/deaths_by_age_LSOA_2010-19.xlsx",
                                     sheet = "Deaths - Persons")

# Drop redundant variables 
deaths_LSOA.df <- deaths_LSOA.df[ -c(1:3,22:24) ]

# Standardize variable names with age prefix
deaths_LSOA.df <- deaths_LSOA.df %>%
  dplyr::rename_with(~stringr::str_replace_all(.x, "[:punct:]", "_")) %>%
  dplyr::rename_with(~stringr::str_replace_all(.x, "[:space:]", "_")) 
colnames(deaths_LSOA.df)[3:18] <- paste("A__", colnames(deaths_LSOA.df[,c(3:18)]), sep = "")

# Drop non-England LSOAs
deaths_LSOA.df <- deaths_LSOA.df[grepl("^E",deaths_LSOA.df$LSOA_code),]

# Map year values
deaths_LSOA.df <- deaths_LSOA.df %>%                             
  group_by(LSOA_code) %>%
  dplyr::mutate(obno = row_number()) %>%
  ungroup()
deaths_LSOA.df$year <- plyr::mapvalues(deaths_LSOA.df$obno, from = c(1:9),
                                       to = c(2011:2019))

# Recode 0-4 age group
deaths_LSOA.df$A__00_04 <- rowSums(deaths_LSOA.df[, c(3,4)])
deaths_LSOA.df <- deaths_LSOA.df[ -c(3,4) ]

# Merge with LSOA boundary data
deaths_LSOA.df <- dplyr::left_join(deaths_LSOA.df, LSOA_bound.df[, c("LSOA_code", "LSOA_code_old",
                                                                     "change")], by = "LSOA_code") 

# Drop irretrievable LSOAs
deaths_LSOA.df <- subset(deaths_LSOA.df, change != "X")

# Drop 2011 split LSOAs 
attach(deaths_LSOA.df)
deaths_LSOA_agg.df <- deaths_LSOA.df %>%
  dplyr::group_by(LSOA_code_old, year) %>%
  dplyr::mutate(across(c("A__05_09": "A__70_74", "A__00_04"), ~
                         case_when(change == "S" ~ mean(.),
                                   TRUE ~ .))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(LSOA_code_agg = case_when(change == "S" ~ LSOA_code_old,
                                         TRUE ~ LSOA_code)) %>%
  dplyr::distinct(LSOA_code_agg, year, .keep_all = TRUE)
detach(deaths_LSOA.df)

# Drop redundant variables
deaths_LSOA_agg.df <- deaths_LSOA_agg.df[, c(22, 18, 19, 3:16)]

# Observed total LSOA deaths by age group
obdeaths_LSOA_agg.df <- deaths_LSOA_agg.df %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(obdeaths = sum(c_across(c(A__05_09:A__70_74, A__00_04)))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(LSOA_code_agg)

# Drop redundant variables
deaths_ENG.df <- obdeaths_LSOA_agg.df
obdeaths_agg.df <- obdeaths_LSOA_agg.df[, c("LSOA_code_agg", "year", "obdeaths" )]


# Expected deaths
################################################################################

# Observed deaths (country-wide)
#------------------- ------------------

# Observed total country-wide deaths by age group 
deaths_ENG.df <- deaths_ENG.df %>%
  group_by(year) %>%
  dplyr::summarise(across(starts_with("A__"), sum)) %>%
  ungroup()

# Reshape from wide to long 
deaths_ENG_long.df <- reshape2::melt(deaths_ENG.df, id.vars = c("year"))

# Reshape from long to wide (but wide by year instead of age)
deaths_ENG_wide.df <- reshape2::dcast(deaths_ENG_long.df, 
                                      variable ~ year, value.var = "value")

# Add variable name prefix
colnames(deaths_ENG_wide.df)[2:10] <- paste("deaths_", 
                                                colnames(deaths_ENG_wide.df[2:10]),
                                                sep = "")
colnames(deaths_ENG_wide.df)[colnames(deaths_ENG_wide.df) == "variable"] <- "age_group"


#  Population (country wide)
#---------------------------------------------
# nomis: Population estimates - local authority based by five year age band (using country totals)

pop_ENG.df <- readxl::read_excel("Time Adjusted IMD/Data/Source/pop_by_age_ENG_2011-19.xlsx")

# Recode 0-4 age group
pop_ENG_recode.df <- pop_ENG.df %>%
  dplyr::mutate(obno = row_number()) %>%
  dplyr::mutate(tag = case_when(obno == 1 | obno == 2 ~ 1,
                                TRUE ~ 0)) %>%
  group_by(tag) %>%
  dplyr::summarise(across(starts_with("2"), sum)) %>%
  ungroup()

age_recode.vec <- as.numeric(pop_ENG_recode.df[2,])
for (i in 2:10) {
  pop_ENG.df[1,i] <- age_recode.vec[i]
}
pop_ENG.df <- pop_ENG.df[-2,] 

# Standardized age varible names
names(pop_ENG.df)[1] <- tolower(names(pop_ENG.df)[1])
age_names.vec <- as.character(deaths_ENG_wide.df[,1])
pop_ENG.df[,1] <- age_names.vec
colnames(pop_ENG.df)[2:10] <- paste("pop_", colnames(pop_ENG.df[,c(2:10)]), sep = "")


# Population (LSOA)
#-----------------------------
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates

#### Define function for looping over years
pop_LSOA.fun <- function(Fyear) {
   
  # Load
  pop_LSOA_Fyear.df <- read.csv(paste0("Time Adjusted IMD/Data/Source/pop_by_age_LSOA_",Fyear,".csv"))
  
  # Numeric columns
  pop_LSOA_Fyear.df <- pop_LSOA_Fyear.df %>% 
    mutate_at(c(2:15), as.double)

  # Map year
  pop_LSOA_Fyear.df$year <- as.numeric(Fyear)
   
  # Standardize variable names
  colnames(pop_LSOA_Fyear.df)[1] <- "LSOA_code"
  pop_LSOA_Fyear.df <- pop_LSOA_Fyear.df  %>% 
    dplyr::select(LSOA_code, year, everything()) 
  colnames(pop_LSOA_Fyear.df)[3:17] <- age_names.vec

  # Extract LSOA_code from combined code/name
  pop_LSOA_Fyear.df$LSOA_code <- str_replace(pop_LSOA_Fyear.df$LSOA_code,":.*", "")
  pop_LSOA_Fyear.df$LSOA_code <- stringr::str_trim(pop_LSOA_Fyear.df$LSOA_code)

  # Merge with LSOA boundary data
   pop_LSOA_Fyear.df <- dplyr::left_join(pop_LSOA_Fyear.df, 
                                         LSOA_bound.df[, c("LSOA_code", "LSOA_code_old", "change")],
                                                            by = "LSOA_code") 
   
   # Drop non-England LSOAs
   pop_LSOA_Fyear.df <- pop_LSOA_Fyear.df[grepl("^E", pop_LSOA_Fyear.df$LSOA_code),]
      
   # Drop irretrievable LSOAs
   pop_LSOA_Fyear.df <- subset(pop_LSOA_Fyear.df, change != "X")
   
   # Aggregate 2011 split LSOAs 
   attach(pop_LSOA_Fyear.df)
   pop_LSOA_Fyear_agg.df <- pop_LSOA_Fyear.df %>%
     dplyr::group_by(LSOA_code_old) %>%
     dplyr::mutate(across(c(A__00_04:A__70_74), ~
                            case_when(change == "S" ~ mean(.),
                                      TRUE ~ as.numeric(.)))) %>%
     dplyr::ungroup() %>%
     dplyr::mutate(LSOA_code_agg = case_when(change == "S" ~ LSOA_code_old,
                                             TRUE ~ LSOA_code)) %>%
     dplyr::distinct(LSOA_code_agg, .keep_all = TRUE)
   detach(pop_LSOA_Fyear.df)
   
   # Drop redundant variables
   pop_LSOA_Fyear_agg.df <- subset(pop_LSOA_Fyear_agg.df, select = c(LSOA_code_agg, year, 
                                                                     A__00_04:A__70_74))

   # Return dataframe
   return(pop_LSOA_Fyear_agg.df)
}

# Apply function 
Fyear.vec <- c(2011:2019)
pop_LSOA_agg.list <- lapply(as.numeric(unlist(Fyear.vec)), FUN = pop_LSOA.fun)

# Append years
pop_LSOA_agg.df <- dplyr::bind_rows(pop_LSOA_agg.list[[1]], pop_LSOA_agg.list[[2]], pop_LSOA_agg.list[[3]],
                                    pop_LSOA_agg.list[[4]], pop_LSOA_agg.list[[5]], pop_LSOA_agg.list[[6]], 
                                    pop_LSOA_agg.list[[7]], pop_LSOA_agg.list[[8]], pop_LSOA_agg.list[[9]])


# Expected deaths calculation
#-----------------------------

# Join ENG deaths with ENG population
moR_ENG.df <- inner_join(deaths_ENG_wide.df, pop_ENG.df, by = "age_group")

# Calculate ENG mortality rate by age
moR_ENG.df[paste0("moR_", 2011:2019)] <- moR_ENG.df[paste0("deaths_", 2011:2019)] /
                                         moR_ENG.df[paste0("pop_", 2011:2019)]

# Drop redundant variables
moR_ENG.df <- subset(moR_ENG.df, select = c(age_group, moR_2011:moR_2019))

# Variable names as year
moR_ENG.df <- moR_ENG.df %>%
   rename_with(~str_replace_all(.x, "moR_", "")) # year cols

# Reshape
moR_ENG_long.df <- reshape2::melt(moR_ENG.df, 
                                         id.vars = c("age_group"),
                                         variable.name = "year",
                                         value.name = "moR")
moR_ENG_long.df$year = as.numeric(as.character(moR_ENG_long.df$year))

pop_LSOA_agg_long.df <- reshape2::melt(pop_LSOA_agg.df, 
                                       id.vars = c("LSOA_code_agg", "year"),
                                       variable.name = "age_group", 
                                       value.name = "pop")
pop_LSOA_agg_long.df$year = as.numeric(as.character(pop_LSOA_agg_long.df$year))

# Join LSOA population estimates with ENG mortality rates by year by age group
expdeaths_agg.df <- dplyr::left_join(pop_LSOA_agg_long.df, moR_ENG_long.df, by = c("age_group", "year"))

# Expected deaths for LSOA by year by age group (ENG mortality rate * LSOA population)
expdeaths_agg.df$expdeaths_age <- expdeaths_agg.df$moR*expdeaths_agg.df$pop

# Total expected deaths for LSOA by year (sum across LSOA age groups)
expdeaths_agg.df <- expdeaths_agg.df %>%
   group_by(LSOA_code_agg, year) %>%
   dplyr::summarise(expdeaths = sum(expdeaths_age)) %>%
   ungroup()

# Standardized mortality rate
#------------------------------

# Join observed and expected deaths
moRz_agg.df <- dplyr::left_join(obdeaths_agg.df, expdeaths_agg.df, 
                                by = c("LSOA_code_agg", "year"))

# Standardized mortality rate
moRz_agg.df$moRz <- moRz_agg.df$obdeaths/moRz_agg.df$expdeaths

# Drop redundant variables
moRz_agg.df <- moRz_agg.df[, c("LSOA_code_agg", "year", "moRz")]


# Combine IMD, DV and controls
################################################################################

# Load time-adjusted IMD dataframe
IMD_TS_agg.df <- readRDS("Time Adjusted IMD/Data/Derived/IMD_TS_agg.rds")

# Add SAMHI 
IMD_samhi_TS_agg.df <- dplyr::left_join(IMD_TS_agg.df, samhi_agg.df,
                                        by = c("LSOA_code_agg", "year"))

# Add residential areas
IMD_samhi_TS_agg.df <- dplyr::left_join(IMD_samhi_TS_agg.df, resarea_agg.df,
                                    by = "LSOA_code_agg")

# Add median age
IMD_samhi_TS_agg.df <- dplyr::left_join(IMD_samhi_TS_agg.df, medage_agg.df,
                                    by = c("LSOA_code_agg", "year"))

# Add male share
IMD_samhi_TS_agg.df <- dplyr::left_join(IMD_samhi_TS_agg.df, popgen_agg.df,
                                    by = c("LSOA_code_agg", "year"))

# Add population density
IMD_samhi_TS_agg.df <- dplyr::left_join(IMD_samhi_TS_agg.df, popdens_agg.df,
                                    by = c("LSOA_code_agg", "year"))

# Add mortality rate
IMD_samhi_TS_agg.df <- dplyr::left_join(IMD_samhi_TS_agg.df, moRz_agg.df,
                                    by = c("LSOA_code_agg", "year"))

IMD_samhi_TS_agg.df <- subset(IMD_samhi_TS_agg.df, year != 2010)

# Save
saveRDS(IMD_samhi_TS_agg.df,file = "Time Adjusted IMD/Data/Derived/IMD_samhi_TS_agg.rds")





###############################################################################
################################################################################


