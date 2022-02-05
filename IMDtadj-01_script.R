
# Author: Matthew Tibbles 
#-----------------------------

# Description
################################################################################




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
LSOA_bound.df <- LSOA_bound.df[c(1, 3, 5)]

# Rename 
new_names2 <- c("LSOA_code_old", "LSOA_code", "change")
names(LSOA_bound.df) <- new_names2

# Trim LSOA strings 
IMD_2010.df$LSOA_code_old <- stringr::str_trim(IMD_2010.df$LSOA_code_old)
LSOA_bound.df$LSOA_code_old <- stringr::str_trim(LSOA_bound.df$LSOA_code_old)
LSOA_bound.df$LSOA_code <- stringr::str_trim(LSOA_bound.df$LSOA_code)

# # Mark LSOAs that were a) split or b) fragmented in 2011 LSOA boundary
#LSOA_bound.df <- LSOA_bound.df %>%
# dplyr::mutate(tochange = case_when(change = change == "S" | change == "X"~ 1,
#                                            change == "M" ~ 2, TRUE ~ 3))
# Save for later use
saveRDS(LSOA_bound.df,file = "Time Adjusted IMD/Data/Derived/LSOA_bound.rds")

# Add boundary changes to IMD data
IMD_2010_bound.df <- dplyr::left_join(IMD_2010.df, LSOA_bound.df,
                                      by = "LSOA_code_old")
# Drop split/fragmented LSOAs
IMD_2010_bound.df<-subset(IMD_2010_bound.df, change == "U" | change == "M")


attach(IMD_2010_bound.df)
IMD_2010_2011B.df <- IMD_2010_bound.df %>%
  dplyr::group_by(LSOA_code) %>%
  dplyr::mutate(across(c(IMD, health, educ, barr, crime, livenv), ~
                         case_when(change == "M" ~ mean(.),
                                   TRUE ~ .)))  %>%
  dplyr::distinct(LSOA_code, .keep_all = TRUE)  %>%
  dplyr::ungroup()
detach(IMD_2010_bound.df)

# Drop pre-2011 LSOA code and boundary change indicator
IMD_2010_2011B.df <- IMD_2010_2011B.df[, -c(1,9)]

# Year identifier
IMD_2010_2011B.df$year <- as.numeric(as.character(2010))


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

# Drop split/fragmented LSOAs
IMD_2015.df <- dplyr::left_join(IMD_2015.df, LSOA_bound.df[, c("LSOA_code", "change")],
                                by = "LSOA_code", all.x=TRUE) %>%
  dplyr::distinct(LSOA_code, .keep_all = TRUE)  
IMD_2015.df <- subset(IMD_2015.df, change == "U" | change == "M")
IMD_2015.df <- IMD_2015.df[-c(9)]

# Year identifier
IMD_2015.df$year <- as.numeric(as.character(2015))

# Clean LSOA string
IMD_2015.df$LSOA_code <- stringr::str_trim(IMD_2015.df$LSOA_code)


# 2019
#---------------------

# Load 2019 scores
IMD_2019.df <- readxl::read_excel("Time Adjusted IMD/Data/Source/IMD2019_SCORE_DOMS.xlsx",
                                  sheet="IoD2019 Scores")

# Drop redundant variables
IMD_2019.df <- IMD_2019.df[c(1,2,5,8:12)]

# Rename
names(IMD_2019.df) <- new_names3

# Drop split/fragmented LSOAs
IMD_2019.df <- dplyr::left_join(IMD_2019.df, LSOA_bound.df[, c("LSOA_code", "change")],
                                by = "LSOA_code", all.x=TRUE) %>%
  dplyr::distinct(LSOA_code, .keep_all = TRUE)  
IMD_2019.df <- subset(IMD_2019.df, change == "U" | change == "M")
IMD_2019.df <- IMD_2019.df[-c(9)]

# Year identifier
IMD_2019.df$year <- as.numeric(as.character(2019))

# Clean LSOA string
IMD_2019.df$LSOA_code <- stringr::str_trim(IMD_2019.df$LSOA_code)




# Fit OLS regression of IMD on domain scores by year
###############################################################################

# 2010
#-----------------------

# OLS regression
lm.2010 <- lm(IMD ~ educ + health + crime + barr + livenv, data = IMD_2010_2011B.df)

# Beta coefficients for Table 1
lm.2010.bcoef <- lm.beta(lm.2010)


# 2015 
#----------------------

# OLS regression
lm.2015 <- lm(IMD ~ educ + health + crime + barr + livenv, data = IMD_2015.df)

# Beta coefficients for Table 1
lm.2015.bcoef <- lm.beta(lm.2015)


# 2019
#-----------------------

# OLS regression
lm.2019 <- lm(IMD ~ educ + health + crime + barr + livenv, data = IMD_2019.df)

# Beta coefficients for Table 1
lm.2019.bcoef <- lm.beta(lm.2019)


# Residual plot for IMD 2015
################################################################################


# Residuals to data frame
IMD_2015.df$residual <- residuals(lm.2015)

# Domains input vector
doms = names(IMD_2015.df)[4:8]
doms = set_names(doms)
domnames.vec <-c("Education", "Health", "Crime", "Barriers", "Living environment")


### Function to plot residuals by domain
residplot.fun = function(xF, yF) {
  
  f <- paste("residual ~", xF)
  domcount <<- domcount + 1 
  loess <- loess(f, data = IMD_2015.df, span = 0.25) 
  loess_fit <- as.data.frame(predict(loess))
  names(loess_fit) <- c("fit")
  IMD_2015.df$fit <- loess_fit$fit
  IMD_2015.df <- IMD_2015.df %>%
    dplyr::arrange(xF)
  
resdom.gg <- ggplot(IMD_2015.df, aes(x = .data[[xF]], y = .data[[yF]]) ) +
    geom_hline(yintercept=0, size= 0.25, color="black", linetype = "dashed") +
    
    geom_point(shape = 1, size = .2, color = "#868686", alpha = .4, 
               position = position_jitter(w = 0.2)) +
    
    geom_line(IMD_2015.df, 
              mapping = aes(x = .data[[xF]], y = fit), col = "#EFC000", size = 1) +
    
    labs(x = domnames.vec[domcount], y = "Residual") +
    theme_bw() +
    theme(
      plot.margin = unit(c(.1, .1, .1, .1), "cm"))
  
  if (xF == "educ" || xF == "barr") {
    
    resdom.gg <- resdom.gg + theme(axis.title.y=element_text(vjust = -0.05))
    
    return(resdom.gg)
  } 
  
  else {
    
    resdom.gg <- resdom.gg + theme(axis.title.y=element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())
    return(resdom.gg)
  }
  
}

domcount <- 0   
domresplots <- lapply(doms, FUN = residplot.fun, y= "residual")


resdomC.gg <- cowplot::plot_grid(domresplots[[1]], domresplots[[2]],
                           domresplots[[3]], domresplots[[4]],
                           domresplots[[5]],nrow=2, 
                           rel_widths = c(1.06, .94, .94, 1.06, .94)) +
  
  
  theme(plot.margin = margin(2,2, 7.5,1, "mm"))


P1_line_1 <- expression(paste(italic("Note"),". Plot of residuals by domain from OLS regression of 2019 IMD score on 2019 IMD domain scores. Solid yellow lines represent locally smoothed fit from")) 
P1_line_2 <- ("LOESS regression using a bandwidth of 0.25.")


resdomC.gg <- resdomC.gg + # allows plotting anywhere on the canvas
  draw_label(P1_line_1, x = .485, y = -0.0275, size = 7.5) +
  draw_label(P1_line_2, x = .1445, y = -0.0575, size = 7.5)

resdomC.gg



# Fit MFP regression of IMD on domains scores by year
################################################################################

# 2015
#---------------

# MFP procedure
mfp2015 <- mfp(IMD ~ fp(educ) + fp(health) + fp(crime) + fp(barr) + fp(livenv)
               , family = gaussian, data = IMD_2015.df, verbose = TRUE)

# MFP transformations
attach(IMD_2015.df)
IMD_2015.df$educ_FP1 <- educ/10
IMD_2015.df$educ_FP2 <- (educ/10) * log((educ/10))
IMD_2015.df$health_FP1 <- (health + 3.4)/10
IMD_2015.df$health_FP2 <- ((health + 3.4)/10)^2
IMD_2015.df$crime_FP1 <- crime + 2.8
IMD_2015.df$crime_FP2 <- ((crime +2.8)/10)^2
IMD_2015.df$barr_FP1 <- (barr / 10)^3
IMD_2015.df$barr_FP2 <- ((barr / 10)^3)*log(barr/10)
IMD_2015.df$livenv_FP1 <- (livenv / 10)^2
IMD_2015.df$livenv_FP2 <- ((livenv / 10)^2) * log(livenv/10)
detach(IMD_2015.df)

# MFP regression
lm.mfp.2015 <- lm(IMD ~ educ_FP1 + educ_FP2 + health_FP1 + health_FP2 +
                    crime_FP1 + crime_FP2 + barr_FP1 + barr_FP2 + 
                    livenv_FP1 + livenv_FP2, data = IMD_2015.df)

# Beta coefficients for Table 1
lm.mfp.2015.bcoef <- lm.beta(lm.mfp.2015)

# Store standard deviation of residuals
MFPresSD2015 <- sigma(lm.mfp.2015)

# Matrix of coefficients
coef_2015.mat <- lm.mfp.2015$coefficients


# 2010
#--------------------

# MFP transformations
attach(IMD_2010_2011B.df)
IMD_2010_2011B.df$educ_FP1 <- educ/10
IMD_2010_2011B.df$educ_FP2 <- (educ/10) * log((educ/10))
IMD_2010_2011B.df$health_FP1 <- (health + 3.4)/10
IMD_2010_2011B.df$health_FP2 <- ((health + 3.4)/10)^2
IMD_2010_2011B.df$crime_FP1 <- crime + 2.8
IMD_2010_2011B.df$crime_FP2 <- ((crime +2.8)/10)^2
IMD_2010_2011B.df$barr_FP1 <- (barr / 10)^3
IMD_2010_2011B.df$barr_FP2 <- ((barr / 10)^3)*log(barr/10)
IMD_2010_2011B.df$livenv_FP1 <- (livenv / 10)^2
IMD_2010_2011B.df$livenv_FP2 <- ((livenv / 10)^2) * log(livenv/10)
detach(IMD_2010_2011B.df)

# MFP regression
lm.mfp.2010 <- lm(IMD ~ educ_FP1 + educ_FP2 + health_FP1 + health_FP2 +
                    crime_FP1 + crime_FP2 + barr_FP1 + barr_FP2 + 
                    livenv_FP1 + livenv_FP2, data = IMD_2010_2011B.df)

# Beta coefficients for Table 1
lm.mfp.2010.bcoef <- lm.beta(lm.mfp.2010)

# Residuals to data frame
IMD_2010_2011B.df$MFPres <- residuals(lm.mfp.2010)

# Store standard deviation of residuals
MFPresSD2010 <- sigma(lm.mfp.2010)


# 2019
#--------------------

# MFP transformations
attach(IMD_2019.df)
IMD_2019.df$educ_FP1 <- educ/10
IMD_2019.df$educ_FP2 <- (educ/10) * log((educ/10))
IMD_2019.df$health_FP1 <- (health + 3.4)/10
IMD_2019.df$health_FP2 <- ((health + 3.4)/10)^2
IMD_2019.df$crime_FP1 <- crime + 2.8
IMD_2019.df$crime_FP2 <- ((crime +2.8)/10)^2
IMD_2019.df$barr_FP1 <- (barr / 10)^3
IMD_2019.df$barr_FP2 <- ((barr / 10)^3)*log(barr/10)
IMD_2019.df$livenv_FP1 <- (livenv / 10)^2
IMD_2019.df$livenv_FP2 <- ((livenv / 10)^2) * log(livenv/10)
detach(IMD_2019.df)

# MFP regression
lm.mfp.2019 <- lm(IMD ~ educ_FP1 + educ_FP2 + health_FP1 + health_FP2 +
                    crime_FP1 + crime_FP2 + barr_FP1 + barr_FP2 + 
                    livenv_FP1 + livenv_FP2, data = IMD_2019.df)

# Beta coefficients for Table 1
lm.mfp.2019.bcoef <- lm.beta(lm.mfp.2019)

# Residuals to data frame
IMD_2019.df$MFPres <- residuals(lm.mfp.2019)

# Store standard deviation of residuals
MFPresSD2019 <- sigma(lm.mfp.2019)



# Create time-adjusted IMD (with 2015 IMD as reference)
#---------------------------------------------------------------

# 2010
#---------------

# Scale by 2015 coefficients/residual SD
IMD_2010_2011B.df$IMD_tadj <-
  #| 2019 Constant
  (coef_2015.mat[1] +
     #| plus 2010 values scaled by 2015 FP coefficient 
     (coef_2015.mat[2]*IMD_2010_2011B.df$educ_FP1)  + (coef_2015.mat[3]*IMD_2010_2011B.df$educ_FP2) +
     (coef_2015.mat[4]*IMD_2010_2011B.df$health_FP1) + (coef_2015.mat[5]*IMD_2010_2011B.df$health_FP2) +
     (coef_2015.mat[6]*IMD_2010_2011B.df$crime_FP1) + (coef_2015.mat[7]*IMD_2010_2011B.df$crime_FP2) +
     (coef_2015.mat[8]*IMD_2010_2011B.df$barr_FP1) + (coef_2015.mat[9]*IMD_2010_2011B.df$barr_FP2) +
     (coef_2015.mat[10]*IMD_2010_2011B.df$livenv_FP1) + (coef_2015.mat[11]*IMD_2010_2011B.df$livenv_FP2) +
     #| plus 2010 standardized residual scaled by by 2015 residual SD
     + ((IMD_2010_2011B.df$MFPres*MFPresSD2015)/MFPresSD2010))


# 2019
#---------------

# Scale by 2015 coefficients/residual SD
IMD_2019.df$IMD_tadj <-
  #| 2015 Constant
  (coef_2015.mat[1] +
     #| plus 2019 values scaled by 2015 FP coefficient 
     (coef_2015.mat[2]*IMD_2019.df$educ_FP1)  + (coef_2015.mat[3]*IMD_2019.df$educ_FP2) +
     (coef_2015.mat[4]*IMD_2019.df$health_FP1) + (coef_2015.mat[5]*IMD_2019.df$health_FP2) +
     (coef_2015.mat[6]*IMD_2019.df$crime_FP1) + (coef_2015.mat[7]*IMD_2019.df$crime_FP2) +
     (coef_2015.mat[8]*IMD_2019.df$barr_FP1) + (coef_2015.mat[9]*IMD_2019.df$barr_FP2) +
     (coef_2015.mat[10]*IMD_2019.df$livenv_FP1) + (coef_2015.mat[11]*IMD_2019.df$livenv_FP2) +
     #| plus 2019 standardized residual scaled by by 2015 residual SD
     + ((IMD_2019.df$MFPres*MFPresSD2015)/MFPresSD2019))



# Table 2
################################################################################

# Matrix to store coefficients
domreg.mat <- matrix(nrow = 16, ncol = 6,
                     dimnames = list(c("Linear", "FP1", "FP2",
                                       "Linear", "FP1", "FP2",
                                       "Linear", "FP1", "FP2",
                                       "Linear", "FP1", "FP2",
                                       "Linear", "FP1", "FP2", "$R^{2}$"),
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
                  align = c(rep("c", 5))) %>%
  add_header_above(c(" " = 1, "2010" = 2, "2015" = 2, "2019" = 2),
                   line = T, line_sep = 7.5) %>%
  
  kable_classic(full_width = F,html_font = "Cambria", font_size = 11) %>%
  pack_rows("Education", 1, 3, bold= T, italic = T) %>%
  pack_rows("Health", 4, 6, bold = T, italic = T) %>%
  pack_rows("Crime", 7, 9,  bold = T, italic = T) %>%
  pack_rows("Barriers", 10, 12,  bold = T, italic = T) %>%
  pack_rows("Environment",13, 15, bold = T, italic = T) %>%
  
  row_spec(1, extra_css = "border-top: .8px solid; border-color: lightgrey") %>%
  row_spec(4, extra_css = "border-top: .8px solid; border-color: lightgrey") %>%
  row_spec(7, extra_css = "border-top: .8px solid; border-color: lightgrey") %>%
  row_spec(10, extra_css = "border-top: .8px solid; border-color: lightgrey") %>%
  row_spec(13, extra_css = "border-top: .8px solid; border-color: lightgrey") %>%
  row_spec(16, extra_css = "border-top: 1px solid; border-color: black") %>%
  footnote(general = "Standarized beta coefficients.",
           general_title = "Note.",
           footnote_as_chunk = T, title_format = c("italic")) 

domreg.tab


# Mean difference plot
################################################################################


# 2010
#------------

# Mean and mean difference vars
IMD_2010_2011B.df$IMD_dif <- IMD_2010_2011B.df$IMD_tadj - IMD_2010_2011B.df$IMD
IMD_2010_2011B.df$IMD_mean <- (IMD_2010_2011B.df$IMD + IMD_2010_2011B.df$IMD_tadj)/2

# Mean + SD values 
meanline <- round(mean(IMD_2010_2011B.df$IMD_dif), digits=2)
difSD <- sd(IMD_2010_2011B.df$IMD_dif)
lbciline <- round(meanline - (1.96*difSD), digits=2)
ubciline <- round(meanline + (1.96*difSD), digits=2)

# Plot
meandif2010.gg <- ggplot(IMD_2010_2011B.df,aes(x=IMD_mean, y=IMD_dif)) + 
  geom_point(color="#868686FF", size=.6, shape=1, alpha=0.4, 
             position = position_jitter(w = 0.5, h = 0.75)) + 
  geom_hline(yintercept=meanline, size=0.8, color="#efc000") +
  xlim(0, 100)  + 
  geom_hline(yintercept = c(lbciline,ubciline), 
             linetype="solid", color="#3b3b3b",
             size=0.6) +
  annotate("text", x = 96.5, y = 0.41, 
           label = paste0("Mean = ", meanline),
           size=2.75, fontface="bold") +
  annotate("text", x = 93.9, y = -1.65, 
           label = paste0("-1.96 S.D. = ", lbciline), size=2.75, fontface="bold") +
  annotate("text", x = 94.25, y = 1.775, 
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
IMD_2019.df$IMD_dif <- IMD_2019.df$IMD_tadj - IMD_2019.df$IMD
IMD_2019.df$IMD_mean <- (IMD_2019.df$IMD + IMD_2019.df$IMD_tadj)/2

### Mean + SD values 
meanline <- round(mean(IMD_2019.df$IMD_dif), digits=2)
difSD <- sd(IMD_2019.df$IMD_dif)
lbciline <- round(meanline - (1.96*difSD), digits=2)
ubciline <- round(meanline + (1.96*difSD), digits=2)


### Plot
meandif2019.gg <- ggplot(IMD_2019.df,aes(x=IMD_mean, y=IMD_dif)) + 
  geom_point(color="#868686", size=.6, shape=1, alpha=0.4, 
             position = position_jitter(w = 0.5, h = 0.75)) + 
  geom_hline(yintercept=meanline, size= 0.8, color="#efc000FF") +
  xlim(0, 100) + theme(plot.margin = unit(c(.25,.25,.25,.25), "cm")) + 
  xlab("Mean of IMD score and ") +
  geom_hline(yintercept = c(lbciline,ubciline), 
             linetype="solid", color="#3b3b3b",
             size=0.6) +
  annotate("text", x = 96.75, y = .3985, 
           label = paste0("Mean = ", meanline),
           size=2.75, fontface="bold") +
  annotate("text", x = 94, y = -1.35, 
           label = paste0("-1.96 S.D. = ", lbciline), size=2.75, fontface="bold") +
  annotate("text", x = 94.25, y = 1.4, 
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

# # Combine
meandifC.gg <- cowplot::plot_grid(meandif2010.gg, meandif2019.gg, ncol=1,
                                  rel_heights = c(.875, 1), labels = c(2010, 2019),
                                  label_x = c(.065, 0.065), label_y = c(.92, .97), 
                                  label_size = 16, label_fontfamily = "serif")

P2_line_1 <- expression(paste(italic("Note"),". Mean difference plot of 2010/2019 IMD score versus time-adjusted 2010/2019 IMD score. X-axis represents")) 
P2_line_2 <- ("mean score. Y-axis represent difference between scores. A solid yellow line represents the mean difference")
P2_line_3 <- expression(paste("across all LSOAs, while solid black lines represent upper and lower distribution limits at the ", 95^th," percentile."))

# add note



# Add spanning y-axis title  (5.49x5.02)
meandifC.gg <- cowplot::ggdraw(meandifC.gg) + 
  annotate(geom = "richtext", x = 0.02, y=0.525, 
           label = "<i>t</i>-adjusted IMD score minus IMD score",
           angle = 90, size = 3.88,
           fill = NA, label.color = NA) +
  annotate(geom = "richtext", x = .6945, y=.026, 
           label = "<i>t</i>-adjusted IMD score",
           size = 3.88,
           fill = NA, label.color = NA) +
  
  theme(plot.margin = margin(2,2, 10,1, "mm")) +
  draw_label(P2_line_1, x = .5025, y = -0.0225, size = 7.5) +
  draw_label(P2_line_2, x = .472, y = -0.0465, size = 7.5) +
  draw_label(P2_line_3, x = .471, y = -0.0725, size = 7.5)
meandifC.gg

#(5.49x5.02)#######################


# Bar chart
################################################################################


### Decile ranks
IMD_2010_2011B.df <- IMD_2010_2011B.df %>%
  ungroup() %>%
  dplyr::mutate(IMD_2010_2011B.df, IMD_dec = ntile(IMD_2010_2011B.df$IMD,10)) %>%
  dplyr::mutate(IMD_2010_2011B.df, IMD_tadj_dec = ntile(IMD_2010_2011B.df$IMD_tadj,10))
IMD_2015.df <- IMD_2015.df %>%
  ungroup() %>%
  dplyr::mutate(IMD_2015.df, IMD_dec = ntile(IMD_2015.df$IMD,10))
IMD_2019.df <- IMD_2019.df %>%
  ungroup() %>%
  dplyr::mutate(IMD_2019.df, IMD_dec = ntile(IMD_2019.df$IMD,10)) %>%
  dplyr::mutate(IMD_2019.df, IMD_tadj_dec = ntile(IMD_2019.df$IMD_tadj,10))

###  Combine Years 
IMD_all.df <- bind_rows(IMD_2010_2011B.df, IMD_2015.df, IMD_2019.df)

### Fill missing t-adjusted IMD rank with standard IMD rank (for 2015 only)
IMD_all.df$IMD_tadj_dec = ifelse(!(is.na(IMD_all.df$IMD_tadj_dec)),
                                 IMD_all.df$IMD_tadj_dec,
                                 IMD_all.df$IMD_dec)


### 5 year lag/lead
IMD_all.df <- IMD_all.df %>%                            
  group_by(LSOA_code) %>%
  dplyr::mutate(IMD_dec_LAG = lag(IMD_dec, n = 1)) %>%
  dplyr::mutate(IMD_tadj_dec_LAG = lag(IMD_tadj_dec, n = 1)) %>%
  dplyr::mutate(IMD_dec_LEAD = lead(IMD_dec, n = 1)) %>%
  dplyr::mutate(IMD_tadj_dec_LEAD = lead(IMD_tadj_dec, n = 1)) %>%
  ungroup()


### Drop redundant 2010/2019 obs
IMD_dif.df <- IMD_all.df[!(IMD_all.df$year !="2015"),]
attach(IMD_dif.df)

### Difference in decile rank (2019 minus 2015/2010)
IMD_dif.df$IMD_dec_LAG_D <- IMD_dec - IMD_dec_LAG
IMD_dif.df$IMD_tadj_dec_LAG_D <- IMD_tadj_dec - IMD_tadj_dec_LAG
IMD_dif.df$IMD_dec_LEAD_D <- IMD_dec - IMD_dec_LEAD
IMD_dif.df$IMD_tadj_dec_LEAD_D <- IMD_tadj_dec - IMD_tadj_dec_LEAD

attach(IMD_dif.df)
### Difference in difference (t-adj IMD minus IMD)
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
                    values = c("1" = "#EDC000FF", # Yellow bars
                               "2" = "#0073c2FF", # Red bars
                               "3" = "#868686FF")) + # Grey bars
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

#0, .25, .1, .575



# COmbine

# # Combine
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

P3_line_1 <- expression(paste(italic("Note"),". Bar chart of the change in IMD score versus time-adjusted IMD score between ",italic(a),") 2015 and 2010 and ", italic(b), ") 2015 and 2019."))
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
        plot.margin = unit(c(.075, .075, 4, .075), "mm")) +
  draw_label(P3_line_1, x = .4655, y = -0.028, size = 7.5) 


dindC.gg 


# Expand function (mimics Stata's expand)
expand <- function(df, ...) {
  as.data.frame(lapply(df, rep, ...))
}

# Expand up to (and including) 2018
IMD_2010_14.df <- expand(IMD_2010_2011B.df, times = 5)
IMD_2015.df$IMD_tadj <- IMD_2015.df$IMD
IMD_2015_18.df <- expand(IMD_2015.df, times = 4)

# Append
IMD_TS.df <- dplyr::bind_rows(IMD_2010_14.df, IMD_2015_18.df) %>%
  group_by(LSOA_code) %>%
  dplyr::mutate(obno = row_number()) %>%
  ungroup()

# Map year values
IMD_TS.df$year = plyr::mapvalues(IMD_TS.df$obno, from = c(1:9),
                                 to = c(2010:2018))
IMD_TS.df$year <- as.numeric(as.character(IMD_TS.df$year))

# Set IMD score as missing for added years
IMD_TS.df <- IMD_TS.df %>%
  dplyr::mutate(IMD = case_when(year == 2010 | year == 2015 ~ as.numeric(IMD),
                                TRUE ~ as.numeric(NA))) %>%
  dplyr::mutate(IMD_tadj = case_when(year == 2010 | year == 2015 ~ as.numeric(IMD_tadj),
                                     TRUE ~ as.numeric(NA)))

# Add 2019 values 
IMD_TS.df <- dplyr::bind_rows(IMD_TS.df, IMD_2019.df) 

# Linear interpolation of IMD score
IMD_TS.df <- IMD_TS.df %>%
  group_by(LSOA_code) %>%
  dplyr::mutate(IMD_ipol = na.approx(IMD, na.rm=FALSE)) %>%
  dplyr::mutate(IMD_tadj_ipol = na.approx(IMD_tadj, na.rm=FALSE)) %>%
  ungroup() %>%
  # Decile rank of interpolated scores (by year)
  group_by(year) %>%
  mutate(IMD_dec_ipol= ntile(IMD_ipol, 10)) %>%
  mutate(IMD_tadj_dec_ipol = ntile(IMD_tadj_ipol, 10)) %>%
  ungroup()

# Drop redundant variables
IMD_TS.df <- IMD_TS.df[c("LSOA_code", "year", "IMD", "IMD_tadj",
                         "IMD_ipol","IMD_dec_ipol", "IMD_tadj_ipol", 
                         "IMD_tadj_dec_ipol")]
# Save dataframe
saveRDS(IMD_TS.df,file = "Time Adjusted IMD/Data/Derived/IMD_TS.rds")

View(IMD_TS.df)




################################################################################




