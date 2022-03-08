################################################################################
########################## Time-adjusted IMD ( 01 ) ############################
################################################################################


# Author: Matthew Tibbles 
#-----------------------------

# Description
################################################################################

# This script generates time-adjusted IMD scores for 2010 and 2019 IMD using
# 2015 IMD as the year of reference. Time-adjusted IMD is derived from 
# MFP regression scaling, as documented in the main article. 

# IMD/time-adjusted scores for specific LSOAs are subject to an aggregation process 
# which enables compatibility between geographic units that were either merged 
# or split as part the 2011 update to LSOA boundaries.

# In the process, it generates the following output:
# 1 - Coefficient table for IMD domain score (Table 2)
# 2 - Residual plot of IMD domain scores (Figure 1)
# 3 - Mean difference plot of IMD versus time-adjusted IMD (Figure 2)
# 4 - Bar chart of 5-year difference in IMD versus time-adjusted IMD (Figure 3)


# Initialize working environment
################################################################################

# Clear memory
rm(list=ls())

# Load required packages
library(rio)
library(plyr)
library(dplyr)
library(readxl)
library(lm.beta)
library(purrr)
library(lm.beta)
library(mfp)
# MFP package was removed from Cran in Jan 2022 (for unspecified reasons)
# To install, download latest version (1.5.2) from https://cran.r-project.org/src/contrib/Archive/mfp/
# And then install using - install.packages("path", repos = NULL, type="source")
# Note, installation requires install of - Rtools
library(ggplot2)
library(ggsci)
library(ggtext)
library(cowplot)
library(kableExtra)
library(zoo)
library(stringr)



# Load IMD/domains data by year (and )
################################################################################

# 2010
#-------------------

# Load 2010 scores
IMD_2010.df <- read.csv("Time Adjusted IMD/Data/Source/IMD2010_SCORE_DOMS.csv")

# Drop redundant variables
IMD_2010.df <- IMD_2010.df[c(1,8,seq(14,22, by = 2))]

# Rename
new_names <- c("LSOA_code_old", "IMD", "health", "educ", "barr", "crime", "livenv")
names(IMD_2010.df) <- new_names

# Load LSOA boundary changes look-up table
LSOA_bound.df <- read.csv("Time Adjusted IMD/Data/Source/LSOA_boundaries.csv")

# Drop redundant variables
LSOA_bound.df <- LSOA_bound.df[c(-2, -4, -8, -9)]

# Rename 
new_names2 <- c("LSOA_code_old", "LSOA_code", "change", "LA_code", "LA_name")
names(LSOA_bound.df) <- new_names2

# Trim LSOA strings 
lapply(LSOA_bound.df, function(x) stringr::str_trim(x))
IMD_2010.df$LSOA_code_old <- stringr::str_trim(IMD_2010.df$LSOA_code_old)

# Save for later use
saveRDS(LSOA_bound.df,file = "Time Adjusted IMD/Data/Derived/LSOA_bound.rds")

# Add boundary changes to IMD data
IMD_2010_bound.df <- dplyr::full_join(IMD_2010.df, LSOA_bound.df,
                                      by = "LSOA_code_old", all.x=TRUE)

# Drop irretrievable LSOAs + Wales LSOAs
IMD_2010_bound.df<-subset(IMD_2010_bound.df, change != "X")
IMD_2010_bound.df <- IMD_2010_bound.df[!grepl('W', IMD_2010_bound.df$LSOA_code),]

# Aggregate 2011 merged LSOAs
attach(IMD_2010_bound.df)
IMD_2010_agg.df <- IMD_2010_bound.df %>%
  dplyr::group_by(LSOA_code) %>%
  dplyr::mutate(across(c(IMD, health, educ, barr, crime, livenv), ~
                         case_when(change == "M" ~ mean(.),
                                   TRUE ~ .))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(LSOA_code_agg =case_when(change == "S" ~ LSOA_code_old,
                                         TRUE ~ LSOA_code)) %>%
  dplyr::distinct(LSOA_code_agg, .keep_all = TRUE)
detach(IMD_2010_bound.df)

# Drop redundant variables
IMD_2010_agg.df <- subset(IMD_2010_agg.df, select = -c(LSOA_code_old, LSOA_code, 
                                                       change))

# Year identifier
IMD_2010_agg.df$year <- as.numeric(as.character(2010))

# Re-order
IMD_2010_agg.df <- IMD_2010_agg.df[, c(9, 7, 8, 10, 1, 3, 2, 5, 4, 6)]


# 2015
#-------------------  

# Load 2015 scores
IMD_2015.df <- readxl::read_excel("Time Adjusted IMD/Data/Source/IMD2015_SCORE_DOMS.xlsx",
                                  sheet="ID2015 Scores")

# Drop redundant variables
IMD_2015.df <- IMD_2015.df[c(1,2,5,8:12)]

# Rename
new_names3 <- c("LSOA_code", "LSOA_name", "IMD", "educ", "health", "crime", "barr", "livenv")
names(IMD_2015.df) <- new_names3

# Trim LSOA string
IMD_2015.df$LSOA_code <- stringr::str_trim(IMD_2015.df$LSOA_code)

# Add boundary changes to IMD data
IMD_2015_bound.df <- dplyr::full_join(IMD_2015.df, LSOA_bound.df[, c("LSOA_code_old", "LSOA_code", "LA_code", "LA_name", "change")],
                                by = "LSOA_code", all.x=TRUE)
# Drop irretrievable + Wales LSOAs
IMD_2015_bound.df<-subset(IMD_2015_bound.df, change != "X")
IMD_2015_bound.df <- IMD_2015_bound.df[!grepl('W', IMD_2015_bound.df$LSOA_code),]

# Aggregate 2011 split LSOAs
attach(IMD_2015_bound.df)
IMD_2015_agg.df <- IMD_2015_bound.df %>%
  dplyr::group_by(LSOA_code_old) %>%
  dplyr::mutate(across(c(IMD, health, educ, barr, crime, livenv), ~
                         case_when(change == "S" ~ mean(.),
                                   TRUE ~ .))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(LSOA_code_agg = case_when(change == "S" ~ LSOA_code_old,
                                          TRUE ~ LSOA_code)) %>%
  dplyr::distinct(LSOA_code_agg, .keep_all = TRUE)
detach(IMD_2015_bound.df)

# Drop redundant variables
IMD_2015_agg.df <- subset(IMD_2015_agg.df, select = -c(LSOA_code_old, LSOA_code,
                                                       LSOA_name, change))
# Year identifier
IMD_2015_agg.df$year <- as.numeric(as.character(2015))

# Re-order
IMD_2015_agg.df <- IMD_2015_agg.df[, c(9, 7, 8, 10, 1, 2, 3, 4, 5, 6)]


# 2019
#---------------------

# Load 2019 scores
IMD_2019.df <- readxl::read_excel("Time Adjusted IMD/Data/Source/IMD2019_SCORE_DOMS.xlsx",
                                  sheet="IoD2019 Scores")

# Drop redundant variables
IMD_2019.df <- IMD_2019.df[c(1,2,5,8:12)]

# Rename
names(IMD_2019.df) <- new_names3

# Trim LSOA string
IMD_2019.df$LSOA_code <- stringr::str_trim(IMD_2019.df$LSOA_code)

# Add boundary changes to IMD data
IMD_2019_bound.df <- dplyr::full_join(IMD_2019.df, LSOA_bound.df[, c("LSOA_code_old", "LSOA_code", "LA_code", "LA_name", "change")],
                                      by = "LSOA_code", all.x=TRUE)
# Drop irretrievable + Wales LSOAs
IMD_2019_bound.df<-subset(IMD_2019_bound.df, change != "X")
IMD_2019_bound.df <- IMD_2019_bound.df[!grepl('W', IMD_2019_bound.df$LSOA_code),]

# Aggregate 2011 split LSOAs
attach(IMD_2019_bound.df)
IMD_2019_agg.df <- IMD_2019_bound.df %>%
  dplyr::group_by(LSOA_code_old) %>%
  dplyr::mutate(across(c(IMD, health, educ, barr, crime, livenv), ~
                         case_when(change == "S" ~ mean(.),
                                   TRUE ~ .))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(LSOA_code_agg = case_when(change == "S" ~ LSOA_code_old,
                                          TRUE ~ LSOA_code)) %>%
  dplyr::distinct(LSOA_code_agg, .keep_all = TRUE)
detach(IMD_2019_bound.df)

# Drop redundant variables
IMD_2019_agg.df <- subset(IMD_2019_agg.df, select = -c(LSOA_code_old, LSOA_code,
                                                       LSOA_name, change))
# Year identifier
IMD_2019_agg.df$year <- as.numeric(as.character(2019))

# Re-order
IMD_2019_agg.df <- IMD_2019_agg.df[, c(9, 7, 8, 10, 1, 2, 3, 4, 5, 6)]


# Fit OLS regression of IMD on domain scores by year
###############################################################################

# 2010
#-----------------------

# OLS regression
lm.2010 <- lm(IMD ~ educ + health + crime + barr + livenv, data = IMD_2010_agg.df)

# Beta coefficients for Table 1
lm.2010.bcoef <- lm.beta(lm.2010)


# 2015 
#----------------------

# OLS regression
lm.2015 <- lm(IMD ~ educ + health + crime + barr + livenv, data = IMD_2015_agg.df)

# Beta coefficients for Table 1
lm.2015.bcoef <- lm.beta(lm.2015)


# 2019
#-----------------------

# OLS regression
lm.2019 <- lm(IMD ~ educ + health + crime + barr + livenv, data = IMD_2019_agg.df)

# Beta coefficients for Table 1
lm.2019.bcoef <- lm.beta(lm.2019)


# Residual plot for IMD 2015
################################################################################

# Residuals to data frame
IMD_2015_agg.df$residual <- residuals(lm.2015)

# Domains input vector
doms = names(IMD_2015_agg.df)[6:10]
doms = set_names(doms)
domnames.vec <-c("Education", "Health", "Crime", "Barriers", "Living environment")


### Function to plot residuals by domain
residplot.fun = function(xF, yF) {
  
  # LOWESS estimates
  f <- paste("residual ~", xF)
  domcount <<- domcount + 1 
  loess <- loess(f, data = IMD_2015_agg.df, span = 0.25) 
  loess_fit <- as.data.frame(predict(loess))
  names(loess_fit) <- c("fit")
  IMD_2015_agg.df$fit <- loess_fit$fit
  IMD_2015_agg.df <- IMD_2015_agg.df %>%
    dplyr::arrange(xF)

# Plot  
resdom.gg <- ggplot(IMD_2015_agg.df, aes(x = .data[[xF]], y = .data[[yF]]) ) +
  # Zero line
  geom_hline(yintercept=0, size= 0.25, color="black", linetype = "dashed") +
  # Scatter points
  geom_point(shape = 21, size = 1, fill = "#a0a0a0", color = "black", alpha = .1, stroke = .05,
             position = position_jitter(w = 0.2)) + 
  # Rug plot
  geom_rug(alpha = 0.1, color = "#a0a0a0",
           outside = TRUE, sides = "tr",
           length = unit(0.2,"cm")) +
  # Allow rug to float outside plot
  coord_cartesian(clip = "off") +
  # LOWESS line
  geom_line(IMD_2015_agg.df, 
              mapping = aes(x = .data[[xF]], y = fit), col = "#EFC000", size = 1) +
  # X axis title
  labs(x = domnames.vec[domcount], y = "Residual") +
  theme_bw() +
  theme(
        # Plot margin
        plot.margin = unit(c(.2, .5, .2, .2), "cm"))
        if (xF == "educ" || xF == "barr") {
          # Y axis title
          resdom.gg <- resdom.gg + theme(axis.title.y=element_text(vjust = -0.05))
          return(resdom.gg)
        }
        else {
          # Y axis title
          resdom.gg <- resdom.gg + theme(axis.title.y=element_blank(),
                                   axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank())
          return(resdom.gg)
        }
  
}

# Apply function over domains
domcount <- 0   
domresplots <- lapply(doms, FUN = residplot.fun, y= "residual")

# Combine plots
resdomC.gg <- cowplot::plot_grid(domresplots[[1]], domresplots[[2]],
                           domresplots[[3]], domresplots[[4]],
                           domresplots[[5]],nrow=2, 
                           rel_widths = c(1.06, .94, .94, 1.06, .94)) 

# Adjust margin
resdomC.gg <- resdomC.gg +
  theme(plot.margin = margin(t=2, r=2, b=7, l=1, "mm"))



#P1_line_1 <- expression(paste(italic("Note"),". Plot of residuals by domain from OLS regression of 2019 IMD score on 2019 IMD domain scores. Solid yellow lines represent locally smoothed fit from")) 
#P1_line_2 <- ("LOESS regression using a bandwidth of 0.25.")

#resdomC.gg <- resdomC.gg + # allows plotting anywhere on the canvas
 # draw_label(P1_line_1, x = .485, y = -0.0275, size = 7.5) +
  #draw_label(P1_line_2, x = .1445, y = -0.0575, size = 7.5)

resdomC.gg



# Fit MFP regression of IMD on domains scores by year
################################################################################

# 2015
#---------------

# MFP procedure
mfp2015 <- mfp(IMD ~ fp(educ) + fp(health) + fp(crime) + fp(barr) + fp(livenv)
               , family = gaussian, data = IMD_2015_agg.df, verbose = TRUE)

# MFP transformations
attach(IMD_2015_agg.df)
IMD_2015_agg.df$educ_FP1 <- educ/10
IMD_2015_agg.df$educ_FP2 <- (educ/10) * log((educ/10))
IMD_2015_agg.df$health_FP1 <- (health + 3.4)/10
IMD_2015_agg.df$health_FP2 <- ((health + 3.4)/10)^2
IMD_2015_agg.df$crime_FP1 <- crime + 2.8
IMD_2015_agg.df$crime_FP2 <- ((crime +2.8)/10)^2
IMD_2015_agg.df$barr_FP1 <- (barr / 10)^3
IMD_2015_agg.df$barr_FP2 <- ((barr / 10)^3)*log(barr/10)
IMD_2015_agg.df$livenv_FP1 <- (livenv / 10)^2
IMD_2015_agg.df$livenv_FP2 <- ((livenv / 10)^2) * log(livenv/10)
detach(IMD_2015_agg.df)

# MFP regression
lm.mfp.2015 <- lm(IMD ~ educ_FP1 + educ_FP2 + health_FP1 + health_FP2 +
                    crime_FP1 + crime_FP2 + barr_FP1 + barr_FP2 + 
                    livenv_FP1 + livenv_FP2, data = IMD_2015_agg.df)

# Beta coefficients for Table 1
lm.mfp.2015.bcoef <- lm.beta(lm.mfp.2015)

# Store standard deviation of residuals
MFPresSD2015 <- sigma(lm.mfp.2015)

# Matrix of coefficients
coef_2015.mat <- lm.mfp.2015$coefficients


# 2010
#--------------------

# MFP transformations
attach(IMD_2010_agg.df)
IMD_2010_agg.df$educ_FP1 <- educ/10
IMD_2010_agg.df$educ_FP2 <- (educ/10) * log((educ/10))
IMD_2010_agg.df$health_FP1 <- (health + 3.4)/10
IMD_2010_agg.df$health_FP2 <- ((health + 3.4)/10)^2
IMD_2010_agg.df$crime_FP1 <- crime + 2.8
IMD_2010_agg.df$crime_FP2 <- ((crime +2.8)/10)^2
IMD_2010_agg.df$barr_FP1 <- (barr / 10)^3
IMD_2010_agg.df$barr_FP2 <- ((barr / 10)^3)*log(barr/10)
IMD_2010_agg.df$livenv_FP1 <- (livenv / 10)^2
IMD_2010_agg.df$livenv_FP2 <- ((livenv / 10)^2) * log(livenv/10)
detach(IMD_2010_agg.df)

# MFP regression
lm.mfp.2010 <- lm(IMD ~ educ_FP1 + educ_FP2 + health_FP1 + health_FP2 +
                    crime_FP1 + crime_FP2 + barr_FP1 + barr_FP2 + 
                    livenv_FP1 + livenv_FP2, data = IMD_2010_agg.df)

# Beta coefficients for Table 1
lm.mfp.2010.bcoef <- lm.beta(lm.mfp.2010)

# Residuals to data frame
IMD_2010_agg.df$MFPres <- residuals(lm.mfp.2010)

# Store standard deviation of residuals
MFPresSD2010 <- sigma(lm.mfp.2010)


# 2019
#--------------------

# MFP transformations
attach(IMD_2019_agg.df)
IMD_2019_agg.df$educ_FP1 <- educ/10
IMD_2019_agg.df$educ_FP2 <- (educ/10) * log((educ/10))
IMD_2019_agg.df$health_FP1 <- (health + 3.4)/10
IMD_2019_agg.df$health_FP2 <- ((health + 3.4)/10)^2
IMD_2019_agg.df$crime_FP1 <- crime + 2.8
IMD_2019_agg.df$crime_FP2 <- ((crime +2.8)/10)^2
IMD_2019_agg.df$barr_FP1 <- (barr / 10)^3
IMD_2019_agg.df$barr_FP2 <- ((barr / 10)^3)*log(barr/10)
IMD_2019_agg.df$livenv_FP1 <- (livenv / 10)^2
IMD_2019_agg.df$livenv_FP2 <- ((livenv / 10)^2) * log(livenv/10)
detach(IMD_2019_agg.df)

# MFP regression
lm.mfp.2019 <- lm(IMD ~ educ_FP1 + educ_FP2 + health_FP1 + health_FP2 +
                    crime_FP1 + crime_FP2 + barr_FP1 + barr_FP2 + 
                    livenv_FP1 + livenv_FP2, data = IMD_2019_agg.df)

# Beta coefficients for Table 1
lm.mfp.2019.bcoef <- lm.beta(lm.mfp.2019)

# Residuals to data frame
IMD_2019_agg.df$MFPres <- residuals(lm.mfp.2019)

# Store standard deviation of residuals
MFPresSD2019 <- sigma(lm.mfp.2019)



# Create time-adjusted IMD (with 2015 IMD as reference)
#---------------------------------------------------------------

# 2010
#---------------

# Scale by 2015 coefficients/residual SD
IMD_2010_agg.df$IMD_tadj <-
  #| 2019 Constant
  (coef_2015.mat[1] +
     #| plus 2010 values scaled by 2015 FP coefficient 
     (coef_2015.mat[2]*IMD_2010_agg.df$educ_FP1)  + (coef_2015.mat[3]*IMD_2010_agg.df$educ_FP2) +
     (coef_2015.mat[4]*IMD_2010_agg.df$health_FP1) + (coef_2015.mat[5]*IMD_2010_agg.df$health_FP2) +
     (coef_2015.mat[6]*IMD_2010_agg.df$crime_FP1) + (coef_2015.mat[7]*IMD_2010_agg.df$crime_FP2) +
     (coef_2015.mat[8]*IMD_2010_agg.df$barr_FP1) + (coef_2015.mat[9]*IMD_2010_agg.df$barr_FP2) +
     (coef_2015.mat[10]*IMD_2010_agg.df$livenv_FP1) + (coef_2015.mat[11]*IMD_2010_agg.df$livenv_FP2) +
     #| plus 2010 standardized residual scaled by by 2015 residual SD
     + ((IMD_2010_agg.df$MFPres*MFPresSD2015)/MFPresSD2010))


# 2019
#---------------

# Scale by 2015 coefficients/residual SD
IMD_2019_agg.df$IMD_tadj <-
  #| 2015 Constant
  (coef_2015.mat[1] +
     #| plus 2019 values scaled by 2015 FP coefficient 
     (coef_2015.mat[2]*IMD_2019_agg.df$educ_FP1)  + (coef_2015.mat[3]*IMD_2019_agg.df$educ_FP2) +
     (coef_2015.mat[4]*IMD_2019_agg.df$health_FP1) + (coef_2015.mat[5]*IMD_2019_agg.df$health_FP2) +
     (coef_2015.mat[6]*IMD_2019_agg.df$crime_FP1) + (coef_2015.mat[7]*IMD_2019_agg.df$crime_FP2) +
     (coef_2015.mat[8]*IMD_2019_agg.df$barr_FP1) + (coef_2015.mat[9]*IMD_2019_agg.df$barr_FP2) +
     (coef_2015.mat[10]*IMD_2019_agg.df$livenv_FP1) + (coef_2015.mat[11]*IMD_2019_agg.df$livenv_FP2) +
     #| plus 2019 standardized residual scaled by by 2015 residual SD
     + ((IMD_2019_agg.df$MFPres*MFPresSD2015)/MFPresSD2019))


# Table 2
################################################################################

# Matrix to store coefficients
domreg.mat <- matrix(nrow = 16, ncol = 6,
                     dimnames = list(c("Linear", "FP1", "FP2",
                                       "Linear", "FP1", "FP2",
                                       "Linear", "FP1", "FP2",
                                       "Linear", "FP1", "FP2",
                                       "Linear", "FP1", "FP2", "R<sup>2</sup>"),
                                     c("(OLS)", "(MFP)", "(OLS)",
                                       "(MFP)", "(OLS)", "(MFP)")))

# Get coefficients
mcount <- 0
for (m in c("","mfp")) {
  mcount <- mcount + 1
  if (mcount == 1) {
    ycount <- 1
  }
  else {
    ycount <- 2
  }
  for (y in c(2010, 2015, 2019)) {
    print(y)
    pcount <- 1
    if (mcount == 1) {
      for (i in seq(from = 1, to = 13, by =3)) {
        print(i)
        pcount <- pcount + 1
        domreg.mat[i,ycount] <-  eval(parse(text=paste0("lm.",y,".bcoef[[1]][[pcount]]")))
      }
      domreg.mat[16,ycount] <- eval(parse(text=paste0("summary(lm.",y,")$r.squared")))
      ycount <- ycount + 2
    }
    else {
      
      for (i in c(2,3,5,6,8,9,11,12,14,15)) {
        
        pcount <- pcount + 1
        
        domreg.mat[i,ycount] <-  eval(parse(text=paste0("lm.mfp.",y,".bcoef[[1]][[pcount]]")))
      }
      
      domreg.mat[16,ycount] <- eval(parse(text=paste0("summary(lm.mfp.",y,")$r.squared")))
      ycount <- ycount + 2
    }
  }
}

domreg.mat <- round(domreg.mat, 3)       
domreg.mat[is.na(domreg.mat)] <- ""

# Table                                  
domreg.tab <- kbl(domreg.mat, bookmarks = T,linesep = "\\addlinespace",
                  align = c(rep("c", 5)), escape = F) %>%
  add_header_above(c(" " = 1, "2010" = 2, "2015" = 2, "2019" = 2),
                   line = T, line_sep = 5, escape = FALSE ) %>%
  kable_classic(full_width = F,html_font = "Cambria", font_size = 10) %>%
  kable_styling(full_width = T) %>%
  
  pack_rows("Education", 1, 3, bold= F, italic = T) %>%
  pack_rows("Health", 4, 6, bold = F, italic = T) %>%
  pack_rows("Crime", 7, 9,  bold = F, italic = T) %>%
  pack_rows("Barriers", 10, 12,  bold = F, italic = T) %>%
  pack_rows("Environment",13, 15, bold = F, italic = T) %>%
  
  row_spec(1:15, extra_css = "border-top: .8px solid; border-color: white") %>%
 
  
  footnote(general = "Standarized beta coefficients. <br> <i>p</i> < 0.01 for all model coefficients.",
           general_title = "Note.",
           footnote_as_chunk = T, title_format = c("italic"), escape = F) 

domreg.tab


# Mean difference plot
################################################################################


# 2010
#------------

# Mean and mean difference vars
IMD_2010_agg.df$IMD_dif <- IMD_2010_agg.df$IMD_tadj - IMD_2010_agg.df$IMD
IMD_2010_agg.df$IMD_mean <- (IMD_2010_agg.df$IMD + IMD_2010_agg.df$IMD_tadj)/2

# Mean + SD values 
meanline <- round(mean(IMD_2010_agg.df$IMD_dif), digits=2)
difSD <- sd(IMD_2010_agg.df$IMD_dif)
lbciline <- round(meanline - (1.96*difSD), digits=2)
ubciline <- round(meanline + (1.96*difSD), digits=2)

### Plot
meandif2010.gg <- ggplot(IMD_2010_agg.df,aes(x=IMD_mean, y=IMD_dif)) + 
  #geom_point(color="#868686FF", size=.6, shape=1, alpha=0.4, 
            # position = position_jitter(w = 0.5, h = 0.75)) + 
  geom_point(shape = 21, size = 1, fill = "#a0a0a0", color = "black", alpha = .3, stroke = .03,
             position = position_jitter(w = 0.5, h = 0.75)) + 
  geom_hline(yintercept=meanline, size=0.8, color="#efc000") +
  xlim(0, 100)  + 
  geom_hline(yintercept = c(lbciline,ubciline), 
             linetype="solid", color="#3b3b3b",
             size=0.6) +
  annotate("text", x = 96.5, y = 0.4, 
           label = paste0("Mean = ", meanline),
           size=2.75, fontface="bold") +
  annotate("text", x = 93.9, y = -2.2, 
           label = paste0("-1.96 S.D. = ", lbciline), size=2.75, fontface="bold") +
  annotate("text", x = 94.25, y = 2.25, 
           label = paste0("+1.96 S.D. = ", ubciline), 
           size=2.75, fontface="bold") +
  coord_cartesian(xlim = c(0, 100), ylim = c(-5, 5)) +
  theme_bw()+             
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(colour = "black",
                                        size=.5, fill=NA)) +
  theme(plot.margin = unit(c(.25, .25, 0, .575), "cm"))
meandif2010.gg


#| 2019
#------------------

### Mean difference variables
IMD_2019_agg.df$IMD_dif <- IMD_2019_agg.df$IMD_tadj - IMD_2019_agg.df$IMD
IMD_2019_agg.df$IMD_mean <- (IMD_2019_agg.df$IMD + IMD_2019_agg.df$IMD_tadj)/2

### Mean + SD values 
meanline <- round(mean(IMD_2019_agg.df$IMD_dif), digits=2)
difSD <- sd(IMD_2019_agg.df$IMD_dif)
lbciline <- round(meanline - (1.96*difSD), digits=2)
ubciline <- round(meanline + (1.96*difSD), digits=2)


### Plot
meandif2019.gg <- ggplot(IMD_2019_agg.df,aes(x=IMD_mean, y=IMD_dif)) + 
  geom_point(shape = 21, size = 1, fill = "#a0a0a0", color = "black", alpha = .3, stroke = .03,
             position = position_jitter(w = 0.5, h = 0.75)) + 
  geom_hline(yintercept=meanline, size= 0.8, color="#efc000FF") +
  xlim(0, 100) + theme(plot.margin = unit(c(.25,.25,.25,.25), "cm")) + 
  xlab("Mean of IMD score and ") +
  geom_hline(yintercept = c(lbciline,ubciline), 
             linetype="solid", color="#3b3b3b",
             size=0.6) +
  annotate("text", x = 96.75, y = .24, 
           label = paste0("Mean = ", meanline),
           size=2.75, fontface="bold") +
  annotate("text", x = 94, y = -1.25, 
           label = paste0("-1.96 S.D. = ", lbciline), size=2.75, fontface="bold") +
  annotate("text", x = 94.25, y = 1.1, 
           label = paste0("+1.96 S.D. = ", ubciline), 
           size=2.75, fontface="bold") +
  coord_cartesian(xlim = c(0, 100), ylim = c(-5, 5)) +
  theme_bw()+             
  theme(axis.title.y=element_blank(),
        axis.title.x=element_text(hjust=0.28),
        panel.background = element_rect(colour = "black",
                                        size=.5, fill=NA)) +
  theme(plot.margin = unit(c(0, .25, .1, .575), "cm"))
meandif2019.gg

#  Combined plot
#--------------------

# Combine
meandifC.gg <- cowplot::plot_grid(meandif2010.gg, meandif2019.gg, ncol=1,
                                  rel_heights = c(.875, 1), labels = c(2010, 2019),
                                  label_x = c(.065, 0.065), label_y = c(.92, .97), 
                                  label_size = 16, label_fontfamily = "serif")

#P2_line_1 <- expression(paste(italic("Note"),". Mean difference plot of 2010/2019 IMD score versus time-adjusted 2010/2019 IMD score. X-axis represents")) 
#P2_line_2 <- ("mean score. Y-axis represent difference between scores. A solid yellow line represents the mean difference")
#P2_line_3 <- expression(paste("across all LSOAs, while solid black lines represent upper and lower distribution limits at the ", 95^th," percentile."))

# Add spanning y-axis title  (5.29x5.27)
meandifC.gg <- cowplot::ggdraw(meandifC.gg) + 
  annotate(geom = "richtext", x = 0.02, y=0.525, 
           label = "<i>t</i>-adjusted IMD score minus IMD score",
           angle = 90, size = 3.88,
           fill = NA, label.color = NA) +
  annotate(geom = "richtext", x = .708, y=.0225, 
           label = "<i>t</i>-adjusted IMD score",
           size = 3.88,
           fill = NA, label.color = NA) +
  
  theme(plot.margin = margin(2,2, 2,1, "mm")) 
  #draw_label(P2_line_1, x = .5025, y = -0.0225, size = 7.5) +
  #draw_label(P2_line_2, x = .472, y = -0.0465, size = 7.5) +
  #draw_label(P2_line_3, x = .471, y = -0.0725, size = 7.5)
meandifC.gg


# Bar chart
################################################################################


### Decile ranks
IMD_2010_agg.df <- IMD_2010_agg.df %>%
  dplyr::mutate(IMD_2010_agg.df, IMD_dec = ntile(IMD_2010_agg.df$IMD,10)) %>%
  dplyr::mutate(IMD_2010_agg.df, IMD_tadj_dec = ntile(IMD_2010_agg.df$IMD_tadj,10))
IMD_2015_agg.df <- IMD_2015_agg.df %>%
  dplyr::mutate(IMD_2015_agg.df, IMD_dec = ntile(IMD_2015_agg.df$IMD,10))
IMD_2019_agg.df <- IMD_2019_agg.df %>%
  dplyr::mutate(IMD_2019_agg.df, IMD_dec = ntile(IMD_2019_agg.df$IMD,10)) %>%
  dplyr::mutate(IMD_2019_agg.df, IMD_tadj_dec = ntile(IMD_2019_agg.df$IMD_tadj,10))

###  Combine Years 
IMD_all.df <- bind_rows(IMD_2010_agg.df, IMD_2015_agg.df, IMD_2019_agg.df)

### Fill missing t-adjusted IMD rank with standard IMD rank (for 2015 only)
IMD_all.df$IMD_tadj_dec = ifelse(!(is.na(IMD_all.df$IMD_tadj_dec)),
                                         IMD_all.df$IMD_tadj_dec,
                                         IMD_all.df$IMD_dec)
### 5 year lag/lead
IMD_all.df <- IMD_all.df %>%                            
  group_by(LSOA_code_agg) %>%
  dplyr::mutate(IMD_dec_LAG = dplyr::lag(IMD_dec, n = 1)) %>%
  dplyr::mutate(IMD_tadj_dec_LAG = dplyr::lag(IMD_tadj_dec, n = 1)) %>%
  dplyr::mutate(IMD_dec_LEAD = dplyr::lead(IMD_dec, n = 1)) %>%
  dplyr::mutate(IMD_tadj_dec_LEAD = dplyr::lead(IMD_tadj_dec, n = 1)) %>%
  ungroup()

### Drop redundant 2010/2019 obs
IMD_dif.df <- IMD_all.df[!(IMD_all.df$year !="2015"),]

### Difference in decile rank (2019 minus 2015/2010)
attach(IMD_dif.df)
IMD_dif.df$IMD_dec_LAG_D <- IMD_dec - IMD_dec_LAG
IMD_dif.df$IMD_tadj_dec_LAG_D <- IMD_tadj_dec - IMD_tadj_dec_LAG
IMD_dif.df$IMD_dec_LEAD_D <- IMD_dec - IMD_dec_LEAD
IMD_dif.df$IMD_tadj_dec_LEAD_D <- IMD_tadj_dec - IMD_tadj_dec_LEAD
detach(IMD_dif.df)

### Difference in difference (t-adj IMD minus IMD)
attach(IMD_dif.df)
IMD_dif.df$IMD_dec_LAG_DinD <- IMD_tadj_dec_LAG_D - IMD_dec_LAG_D
IMD_dif.df$IMD_dec_LEAD_DinD <- IMD_tadj_dec_LEAD_D - IMD_dec_LEAD_D
detach(IMD_dif.df)

### Tag obs. by difference
IMD_dif.df <- IMD_dif.df %>%
  dplyr::mutate(change_LAG = case_when(IMD_dec_LAG_DinD < 0 ~ 2,
                                     IMD_dec_LAG_DinD > 0 ~ 1,
                                     IMD_dec_LAG_DinD == 0 ~ 3)) %>%
  dplyr::mutate(change_LEAD = case_when(IMD_dec_LEAD_DinD < 0 ~ 2,
                                        IMD_dec_LEAD_DinD > 0 ~ 1,
                                        IMD_dec_LEAD_DinD == 0 ~ 3))

### Sum of tagged obs. by decile rank/tag
IMD_dif.df <- IMD_dif.df %>%
  group_by(IMD_dec, change_LAG) %>%
  add_count(change_LAG, name = "change_LAG_cnt") %>%
  ungroup() %>%
  group_by(IMD_dec, change_LEAD) %>%
  add_count(change_LEAD, name = "change_LEAD_cnt") %>%
  ungroup()

### Total no. of obs by decile rank
IMD_dif.df <- IMD_dif.df %>%                            
  group_by(IMD_dec) %>%
  dplyr::mutate(dec_cnt =n()) %>%
  ungroup()
detach(IMD_dif.df)

### Difference in difference as proportion of decile
IMD_dif.df$DinD_LAG_perc <- (IMD_dif.df$change_LAG_cnt / IMD_dif.df$dec_cnt)*100
IMD_dif.df$DinD_LEAD_perc <- (IMD_dif.df$change_LEAD_cnt / IMD_dif.df$dec_cnt)*100

### Plot
dind2010.gg <-ggplot(data=IMD_dif.df, mapping = aes(
  x = factor(IMD_dec), y=DinD_LAG_perc, fill=factor(change_LAG), group=factor(change_LAG))) + 
  # X/Y/by
  geom_bar( stat="unique") + # Bars
  scale_fill_manual(labels = c("Higher", "Lower", "Same"), # Legend labels
                    values = c("1" = "#EDC000", # Yellow bars
                               "2" = "#0073c2", # Red bars
                               "3" = "#868686")) + # Grey bars
  scale_y_continuous(expand = c(.04,.04)) + # Reduce plot margin
  scale_x_discrete(expand = c(.075,.075)) +
  xlab("2015 IMD decile rank") +
  guides(fill = guide_legend(title =
                               "Change in <i>t</i>-adjusted <br>",
                             title.position = "top")) + # Legend title in ggtext markdown
  
  coord_cartesian(xlim = c(1, 10), ylim = c(0, 100), clip = "off")+ # Allow tag to float
  theme_bw() +
  theme(legend.position="top", legend.title.align=0.05, # Legend position
        legend.title=element_markdown(),                                    # Declare markdown legend
        plot.tag.position = c(.5, .93),# Position tag as second line in legend title
        plot.tag=element_text(size=11),
        axis.title.y = element_blank(),
        #axis.title.x=element_blank(),
        # panel.border = element_blank(),
        
        plot.margin = unit(c(.575, .1, .1, .575), "cm"))# Same font size as legend title

dind2010.gg

### Plot
dind2019.gg <-ggplot(data=IMD_dif.df, mapping = aes(x=factor(IMD_dec), 
                                          y=DinD_LEAD_perc, fill=factor(change_LEAD), group=factor(change_LEAD))) + # X/Y/by
  geom_bar(stat="unique") + # Bars
  scale_fill_manual(values = c("1" = "#EDC000FF", # Yellow bars
                               "2" = "#0073c2FF", # Red bars
                               "3" = "#868686FF")) + # Grey bars
  scale_y_continuous(expand = c(.04,.04)) + # Reduce plot margin
  scale_x_discrete(expand = c(.075,.075)) +
  xlab("2015 IMD decile rank")  + # Y-title
  coord_cartesian(xlim = c(1, 10), ylim = c(0, 100), clip = "off")+ # Allow tag to float
  theme_bw() +
  theme(legend.position="off",
        # Panel borde # Declare markdown legend
        # Same font size as legend titl
        axis.title.y = element_blank(),
        #axis.title.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # panel.border = element_blank(),
        
        plot.margin = unit(c(.575, .25, .1, .1), "cm"))

dind2019.gg


# Combine
dindC.gg <- cowplot::plot_grid(
  dind2010.gg + theme(legend.position = "none"),
  dind2019.gg + theme(legend.position = "none"),
  ncol=2,rel_widths = c(1, .89), labels = c(2010, 2019),
  label_x = c(0.1, 0.7625), label_y = c(1.005, 1.005),
  label_fontfamily = "serif")

dindC.gg

legend <- get_legend(
  # create some space to the left of the legend
  dind2010.gg + theme(legend.box.margin=margin(11.5,0,4.25,0),
                      legend.position = "top",
                      legend.title.align = 0.25
                      
  ))

#P3_line_1 <- expression(paste(italic("Note"),". Bar chart of the change in IMD score versus time-adjusted IMD score between ",italic(a),") 2015 and 2010 and ", italic(b), ") 2015 and 2019."))
# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
dindC.gg <- plot_grid(legend,dindC.gg, nrow =2, rel_heights = c(.175, .8)) +

  annotate(geom ="text", label = "IMD",
           x = .6055, y = .9635, size = 3.88) +
  
  annotate(geom = "text", label = "versus change in IMD", 
           x = 0.4945, y=.9175,
           size = 3.88) +
  annotate(geom = "text", x = 0.0225, y=0.5, 
           label = "% of LSOAs",
           angle = 90, size = 3.88)  +
  theme(panel.border = element_blank(),
        plot.margin = unit(c(.075, .075, 0.75, .075), "mm")) 
  #draw_label(P3_line_1, x = .4655, y = -0.028, size = 7.5) 


dindC.gg 

# Time series dataset
################################################################################

# Expand function (mimics Stata's expand)
expand <- function(df, ...) {
  as.data.frame(lapply(df, rep, ...))
}

# Expand up to (and including) 2018
IMD_2010_14_agg.df <- expand(IMD_2010_agg.df, times = 5)
IMD_2015_agg.df$IMD_tadj <- IMD_2015_agg.df$IMD
IMD_2015_18_agg.df <- expand(IMD_2015_agg.df, times = 4)

# Append
IMD_TS_agg.df <- dplyr::bind_rows(IMD_2010_14_agg.df, IMD_2015_18_agg.df) %>%
  group_by(LSOA_code_agg) %>%
  dplyr::mutate(obno = row_number()) %>%
  ungroup()

# Map year values
IMD_TS_agg.df$year = plyr::mapvalues(IMD_TS_agg.df$obno, from = c(1:9),
                                 to = c(2010:2018))
IMD_TS_agg.df$year <- as.numeric(as.character(IMD_TS_agg.df$year))

# Set IMD score as missing for added years
IMD_TS_agg.df <- IMD_TS_agg.df %>%
  dplyr::mutate(IMD = case_when(year == 2010 | year == 2015 ~ as.numeric(IMD),
                                TRUE ~ as.numeric(NA))) %>%
  dplyr::mutate(IMD_tadj = case_when(year == 2010 | year == 2015 ~ as.numeric(IMD_tadj),
                                     TRUE ~ as.numeric(NA)))

# Add 2019 values 
IMD_TS_agg.df <- dplyr::bind_rows(IMD_TS_agg.df, IMD_2019_agg.df) 

# Linear interpolation of IMD score
IMD_TS_agg.df <- IMD_TS_agg.df %>%
  group_by(LSOA_code_agg) %>%
  dplyr::mutate(IMD_ipol = na.approx(IMD, na.rm=FALSE)) %>%
  dplyr::mutate(IMD_tadj_ipol = na.approx(IMD_tadj, na.rm=FALSE)) %>%
  ungroup() %>%
  # Decile rank of interpolated scores (by year)
  group_by(year) %>%
  mutate(IMD_dec_ipol= ntile(IMD_ipol, 10)) %>%
  mutate(IMD_tadj_dec_ipol = ntile(IMD_tadj_ipol, 10)) %>%
  ungroup()

# Drop redundant variables
IMD_TS_agg.df <- IMD_TS_agg.df[c("LSOA_code_agg", "year", "IMD", "IMD_tadj",
                                 "IMD_ipol","IMD_dec_ipol", "IMD_tadj_ipol", 
                                 "IMD_tadj_dec_ipol")]
# Save dataframe
saveRDS(IMD_TS_agg.df,file = "Time Adjusted IMD/Data/Derived/IMD_TS_agg.rds")






################################################################################




