rm(list=ls())

library(tidyverse)
library(plyr)
library(dplyr)
library(readxl)
library(stringr)
library(reshape2)


# Load time-adjusted IMD dataframe
IMD_TS.df <- readRDS("Time Adjusted IMD/Data/Derived/IMD_TS.rds")

# Load LSOA boundary data frame
LSOA_bound.df <- readRDS("Time Adjusted IMD/Data/Derived/LSOA_bound.rds")


# Observed deaths 
################################################################################
# LSOA deaths-by-age dataframe (2008-2017) 
# ... I use mortality rates for t-2
# ... bc. IMD scores for year t is based on data from around t-2 
#ONS Deaths by LSOA
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12626deathsbylowerlayersuperoutputarealsoaenglandandwalesmidyear2001to2019
deaths_LSOA.df <- readxl::read_excel("Time Adjusted IMD/Data/Source/deaths_by_age_LSOA_2008-17.xlsx",
                                     sheet = "Deaths - Persons")
# Drop redundant variables 
deaths_LSOA.df <- deaths_LSOA.df[ -c(1:3,22:24) ]

# Standardize variable names with age prefix
deaths_LSOA.df <- deaths_LSOA.df %>%
  rename_with(~str_replace_all(.x, "[:punct:]", "_")) %>%
  rename_with(~str_replace_all(.x, "[:space:]", "_")) 
colnames(deaths_LSOA.df)[3:18] <- paste("A__", colnames(deaths_LSOA.df[,c(3:18)]), sep = "")

# Drop non-England LSOAs
deaths_LSOA.df <- deaths_LSOA.df[!grepl("W0", deaths_LSOA.df$LSOA_code),]

# Map year values
deaths_LSOA.df <- deaths_LSOA.df %>%                             
  group_by(LSOA_code) %>%
  dplyr::mutate(obno = row_number()) %>%
  ungroup()
deaths_LSOA.df$year <- plyr::mapvalues(deaths_LSOA.df$obno, from = c(1:10),
                                       to = c(2008:2017))

# Recode 0-4 age group
deaths_LSOA.df$A__00_04 <- rowSums(deaths_LSOA.df[, c(3,4)])
deaths_LSOA.df <- deaths_LSOA.df[ -c(3,4) ]

# Merge with LSOA boundary data
deaths_LSOA.df <- dplyr::left_join(deaths_LSOA.df, LSOA_bound.df[, c("LSOA_code", "change")],
                                by = "LSOA_code") 

# Drop split/fragmented LSOAs
deaths_LSOA.df <- subset(deaths_LSOA.df, change == "U" | change == "M")

# Drop duplicates (ONS has already aggregated estimates for pre-2011 merged LSOAs  )
deaths_LSOA.df <- deaths_LSOA.df %>%
  group_by(year)  %>%
  dplyr::distinct(LSOA_code, .keep_all = TRUE)  %>%
  ungroup()

# Drop redundant variables
deaths_LSOA.df <- deaths_LSOA.df[, -c(17, 20)]

# Observed total LSOA deaths by age group
deaths_LSOA.df <- deaths_LSOA.df %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(ob_deaths = sum(c_across(c(A__00_04, A__05_09:A__70_74)))) %>%
  dplyr::select(LSOA_code, LSOA_name, year, A__00_04, everything()) %>%
  ungroup()

# Clean dataframe
ob_deaths.df <- subset(deaths_LSOA.df, select = c(LSOA_code, LSOA_name, year, ob_deaths))
attach(ob_deaths.df)
ob_deaths.df$LSOA_code <- stringr::str_trim(LSOA_code)
detach(ob_deaths.df)


# Expected deaths
################################################################################

# Observed deaths (country-wide)
#------------------- ------------------

# Observed total country-wide deaths by age group 
#(excluding split/fragmented LSOAs )
deaths_ENG.df <- deaths_LSOA.df %>%
  group_by(year) %>%
  dplyr::summarise(across(starts_with("A__"), sum)) %>%
  ungroup()

# reshape to long
deaths_ENG_long.df <- reshape2::melt(deaths_ENG.df, id.vars = c("year"))

# back to wide (but wide by year instead of age)
deaths_ENG_wide.df <- reshape2::dcast(deaths_ENG_long.df, 
                                      variable ~ year, value.var = "value")

# Add year prefix
colnames(deaths_ENG_wide.df)[2:11] <- paste("deaths_", 
                                      colnames(deaths_ENG_wide.df[,c(2:11)]), sep = "")
colnames(deaths_ENG_wide.df)[colnames(
                             deaths_ENG_wide.df) == "variable"] <- "age"


#  Population (country wide)
#---------------------------------------------
# nomis: Population estimates - local authority based by five year age band (using country totals)

pop_ENG.df <- readxl::read_excel("Time Adjusted IMD/Data/Source/pop_by_age_ENG_2008-17.xlsx")

# Recode 0-4 age group
pop_ENG_recode.df <- pop_ENG.df %>%
  dplyr::mutate(obno = row_number()) %>%
  dplyr::mutate(tag = case_when(obno == 1 | obno == 2 ~ 1,
                                TRUE ~ 0)) %>%
  group_by(tag) %>%
  dplyr::summarise(across(starts_with("2"), sum)) %>%
  ungroup()
age_recode.vec <- as.numeric(pop_ENG_recode.df[2,])
for (i in 2:11) {
  pop_ENG.df[1,i] <- age_recode.vec[i]
}
#remove duplicate row
pop_ENG.df <- pop_ENG.df[-2,] 

# Standardized age varible names
names(pop_ENG.df)[1] <- tolower(names(pop_ENG.df)[1])
age_names.vec <- as.character(deaths_ENG_wide.df[,1])
pop_ENG.df[,1] <- age_names.vec
colnames(pop_ENG.df)[2:11] <- paste("pop_", colnames(pop_ENG.df[,c(2:11)]), sep = "")


# this minus pop estimates for split and x



# Population (LSOA)
#-----------------------------
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates

#### Define function for looping over pre-2011 estimates
pop_LSOA_pre2011.fun <- function(Fyear) {
   
   # Load data
   pop_LSOA_Fyear.df <- as.data.frame(readxl::read_excel("Time Adjusted IMD/Data/Source/pop_by_age_LSOA_2008-10.xls",
                                                         sheet = Fyear))
   # Clean variable names
   pop_LSOA_Fyear.df <- pop_LSOA_Fyear.df %>%
      rename_with(~str_replace_all(.x, "p", "A__")) %>%
      rename_with(~str_replace_all(.x, "[:space:]", "")) %>%
      
      # Drop redundant variables
      select(!c(LAD11CD:all_ages,A__75:A__90A__lus))

   # Reshape data from wide to long
   pop_LSOA_Fyear_long.df <- reshape2::melt(pop_LSOA_Fyear.df, 
                                            id.vars = c("LSOA11CD"),
                                            variable.name = "age")
 
   # Group individual years into 5-year age groups
   pop_LSOA_Fyear_long.df <- pop_LSOA_Fyear_long.df %>%
      dplyr::arrange(LSOA11CD, age) %>%
      dplyr::group_by(LSOA11CD) %>%
      dplyr::mutate(group = as.integer(gl(n(), 5, n()))) %>%
      ungroup()
   
   # LSOA population by age-group
   pop_LSOA_Fyear_long.df <- pop_LSOA_Fyear_long.df %>%
      group_by(LSOA11CD, group )%>%
      dplyr::mutate(group_sum = as.numeric(sum(value))) %>%
      ungroup(LSOA11CD) %>%
      dplyr::distinct(LSOA11CD, .keep_all = TRUE) %>%
      ungroup()

   # Reshape from long to wide (with age groups as column names)
   pop_LSOA_Fyear_wide.df <- reshape2::dcast(pop_LSOA_Fyear_long.df, 
                                             LSOA11CD ~ group, value.var = "group_sum")
   
   # Standardize age group names
   for (i in 2:16) {
      im1 = i -1
      colnames(pop_LSOA_Fyear_wide.df)[i] <- age_names.vec[im1]
   }
   
   # Merge with LSOA boundary data 
   colnames(pop_LSOA_Fyear_wide.df)[1] <- "LSOA_code"
   pop_LSOA_Fyear_wide.df$LSOA_code <- stringr::str_trim(pop_LSOA_Fyear_wide.df$LSOA_code)
   pop_LSOA_Fyear_wide.df <- dplyr::left_join(pop_LSOA_Fyear_wide.df, 
                                              LSOA_bound.df[, c("LSOA_code", "change")],
                                              by = "LSOA_code") 
   
   # Drop non-England LSOAs
   pop_LSOA_Fyear_wide.df <- pop_LSOA_Fyear_wide.df[!grepl("W0", pop_LSOA_Fyear_wide.df$LSOA_code),]
   
   # Drop LSOA merger duplicates (merged LSOA estimates already aggregated)
   pop_LSOA_Fyear_wide.df <- pop_LSOA_Fyear_wide.df %>%
      dplyr::distinct(LSOA_code, .keep_all = TRUE)  
    
   
   # Dataframe for excluded LSOAs 
   pop_LSOA_Fyear_na.df <- subset(pop_LSOA_Fyear_wide.df, change == "S" | change == "X")
   
   # Calculate (and store) population total of all excluded LSOAs by age group
   pop_LSOA_Fyear_na.df <- pop_LSOA_Fyear_na.df %>% 
      dplyr::summarise(across(A__00_04:A__70_74, sum))
   poptodrop_Fyear.vec <- as.vector(pop_LSOA_Fyear_na.df)
   
   # Dataframe for remaining LSOAs
   pop_LSOA_Fyear_wide.df <- subset(pop_LSOA_Fyear_wide.df, change == "U" | change == "M")
   pop_LSOA_Fyear_wide.df <- subset(pop_LSOA_Fyear_wide.df, select = -c(change)) 
   pop_LSOA_Fyear_wide.df <- pop_LSOA_Fyear_wide.df %>%
      dplyr::mutate(year = as.numeric(Fyear)) %>%
      dplyr::select(LSOA_code, year, everything()) 
   
   # Return results
   return.list <- list(poptodrop_Fyear.vec, pop_LSOA_Fyear_wide.df)
   return(return.list)
   
}

# Apply function over pre-2011 years 
Fyear.vec <- c("2008", "2009", "2010")
pop_LSOA_pre2011.list <- lapply(Fyear.vec, FUN = pop_LSOA_pre2011.fun)




# Extract dataframe from list
# and store NA LSOA vectors within matrix
exclupop.mat <- matrix(nrow = 15, ncol = 10)
y <-2007
for (i in 1:3) {
   y = y + 1
   eval(parse(text=paste0("pop_LSOA_", y,".df <- pop_LSOA_pre2011.list[[",i,"]][[2]]")))
   for (a in 1:15) {
      exclupop.mat[a,i] <- pop_LSOA_pre2011.list[[i]][[1]][[a]]
   }
}


# Append
pop_LSOA_pre2011.df <- dplyr::bind_rows(pop_LSOA_2008.df, pop_LSOA_2009.df, pop_LSOA_2010.df)
age_names_yr.vec <- names(pop_LSOA_pre2011.df)


#### Define function for looping over post-2011 estimates
pop_LSOA_post2011.fun <- function(Fyear) {
   
   # Load
   pop_LSOA_Fyear.df <- read.csv(paste0("Time Adjusted IMD/Data/Source/pop_by_age_LSOA_",Fyear,".csv"))
   
   # Map year
   pop_LSOA_Fyear.df$year <- as.numeric(Fyear)
   
   # Standardize variable names
   colnames(pop_LSOA_Fyear.df)[1] <- "LSOA_code"
   pop_LSOA_Fyear.df <- pop_LSOA_Fyear.df  %>% 
      dplyr::select(LSOA_code, year, everything()) 
   colnames(pop_LSOA_Fyear.df) <- age_names_yr.vec

   # Extract LSOA_code from combined code/name
   pop_LSOA_Fyear.df$LSOA_code <- str_replace(pop_LSOA_Fyear.df$LSOA_code,":.*", "")
   pop_LSOA_Fyear.df$LSOA_code <- stringr::str_trim(pop_LSOA_Fyear.df$LSOA_code)
   
    # Merge with LSOA boundary data
   pop_LSOA_Fyear.df <- dplyr::left_join(pop_LSOA_Fyear.df, LSOA_bound.df[, c("LSOA_code", "change")],
                                                            by = "LSOA_code") 
   # Drop non-England LSOAs
   pop_LSOA_Fyear.df <- pop_LSOA_Fyear.df[!grepl("W0", pop_LSOA_Fyear.df$LSOA_code),]
      
   # Drop LSOA merger duplicates (merged pre-2011LSOA estimates already aggregated)
   pop_LSOA_Fyear.df <- pop_LSOA_Fyear.df %>%
         dplyr::distinct(LSOA_code, .keep_all = TRUE) 

   # Dataframe for excluded LSOAs 
   pop_LSOA_Fyear_na.df <- subset(pop_LSOA_Fyear.df, change == "S" | change == "X")

   # Calculate (and store) population total of all excluded LSOAs
   pop_LSOA_Fyear_na.df <- pop_LSOA_Fyear_na.df %>% 
      dplyr::summarise(across(A__00_04:A__70_74, sum))
   poptodrop_Fyear.vec <- as.vector(pop_LSOA_Fyear_na.df)

   # Dataframe for remaining LSOAs
   pop_LSOA_Fyear.df <- subset(pop_LSOA_Fyear.df, change == "U" | change == "M")
   pop_LSOA_Fyear.df <- subset(pop_LSOA_Fyear.df, select = -c(change)) 

   # Return results
   return2.list <- list(poptodrop_Fyear.vec, pop_LSOA_Fyear.df)
   return(return2.list)
}
      
# Apply function over post-2011 years 
Fyear2.vec <- c(2011:2017)
pop_LSOA_post2011.list <- lapply(Fyear2.vec, FUN = pop_LSOA_post2011.fun)


# Extract dataframe from list
# and store NA LSOA vectors within matrix
y <-2010
for (i in 1:7) {
   y = y + 1
   eval(parse(text=paste0("pop_LSOA_", y,".df <- pop_LSOA_post2011.list[[",i,"]][[2]]")))
   imat = i + 3
   for (a in 1:15) {
      exclupop.mat[a, imat] <- pop_LSOA_post2011.list[[i]][[1]][[a]]
   }
}

# Append years
pop_LSOA_post2011.df <- dplyr::bind_rows(pop_LSOA_2011.df, pop_LSOA_2012.df, pop_LSOA_2013.df,
                                         pop_LSOA_2014.df, pop_LSOA_2015.df, pop_LSOA_2016.df, 
                                         pop_LSOA_2017.df)

# Append pre-2011 and post-2011 years 
pop_LSOA.df <- dplyr::bind_rows(pop_LSOA_pre2011.df, pop_LSOA_post2011.df)


# Expected deaths calculation
#-----------------------------

# Subtract excluded LSOAs from England population counts (by age group)
pop_ENG_exclu.df <- pop_ENG.df
for (y in 1:10) {
   for (a in 1:15) {
      ydf = y + 1
      pop_ENG_exclu.df[a,ydf] <-  pop_ENG.df[a,ydf] -  exclupop.mat[a,y]
   }
}

# Join ENG deaths with ENG population
moR_ENG.df <- inner_join(deaths_ENG_wide.df, pop_ENG_exclu.df, by = "age")


# Calculate ENG mortality rate by age
moR_ENG.df[paste0("moR_", 2008:2017)] <- moR_ENG.df[paste0("deaths_", 2008:2017)] /
                                         moR_ENG.df[paste0("pop_", 2008:2017)]
moR_ENG.df <- subset(moR_ENG.df, select = c(age, moR_2008:moR_2017))

moR_ENG.df <- moR_ENG.df %>%
   rename_with(~str_replace_all(.x, "moR_", "")) # year cols

# Reshape
moR_ENG2.df <- reshape2::melt(moR_ENG.df, 
                                         id.vars = c("age"),
                                         variable.name = "year",
                                         value.name = "moR")
moR_ENG2.df$year = as.numeric(as.character(moR_ENG2.df$year))

pop_LSOA2.df <- reshape2::melt(pop_LSOA.df, 
                              id.vars = c("LSOA_code", "year"),
                              variable.name = "age", 
                              value.name = "pop")
pop_LSOA2.df$year = as.numeric(as.character(pop_LSOA2.df$year))

exp_deaths.df <- dplyr::left_join(pop_LSOA2.df, moR_ENG2.df, by = c("age", "year"))

# Expected deaths for LSOA by year for at each group
exp_deaths.df$exp_deaths_byage <- exp_deaths.df$moR*exp_deaths.df$pop

# total expected deaths for LSOA by year (across age groups)
exp_deaths.df <- exp_deaths.df %>%
   group_by(LSOA_code, year) %>%
   dplyr::summarise(exp_deaths = sum(exp_deaths_byage)) %>%
   ungroup()

# 
moRz.df <- dplyr::left_join(ob_deaths.df, exp_deaths.df, 
                                 by = c("LSOA_code", "year"))


# Standardized mortality rate
moRz.df$moRz <- moRz.df$ob_deaths/moRz.df$exp_deaths

# Re-adjust year var to align with IMD data
moRz.df$year <- moRz.df$year + 2 

# merge with imd, ADD SCORE.......(IMD_ipol, IMD_tadj_ipol
IMD_TS_moRz.df <- left_join(moRz.df, IMD_TS.df, 
                            by = c("LSOA_code", "year"))

# Save
saveRDS(IMD_TS_moRz.df,file = "Time Adjusted IMD/Data/Derived/IMD_TS_moRz.rds")





###############################################################################
################################################################################


