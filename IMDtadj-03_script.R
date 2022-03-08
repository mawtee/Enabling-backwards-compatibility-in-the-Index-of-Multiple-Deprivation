################################################################################
############################  TIME-ADJUSTED IMD ( 03 )  ########################
################################################################################

# Author: Matthew Tibbles 

# Description
################################################################################
# Estimation script (full description pending)



# Load packages/data
################################################################################

#| Clear environment
#---------------------
rm(list=ls())

#| Load required libraries 
#--------------------------
library(tidyverse)                           # General
library(dplyr)                               # General
library(plm)                                 # Panel estimator
library(margins)                             # Marginal predictions
library(clubSandwich)                        # Clustered SEs
library(weights)                             # Number formatting
library(kableExtra)
library(ggplot2)                             # Plotting
library(ggsci)                               # not sure
library(ggtext)                              # Markdown plot text
library(cowplot)
library(modelsummary)

#| Main data frame 
#-------------------
IMD_samhi_TS_agg.df <- readRDS("Time Adjusted IMD/Data/Derived/IMD_samhi_TS_agg.rds")

# Drop missing values
IMD_samhi_TS_agg.df <- subset(IMD_samhi_TS_agg.df, !is.na(resarea_code))


# Pooled regression residuals
################################################################################

# Pooled regression
pool <- lm(samhi ~ IMD_tadj_dec_ipol + as.factor(resarea_code) + medage + male_share
                       + popdens + moRz,
           data = IMD_samhi_TS_agg.df)

# Residuals to data frame
IMD_samhi_TS_agg.df$residual <- residuals(pool)

# Domains input vector
covars <- names(IMD_samhi_TS_agg.df)[c(8, 12:15)]
covars <- set_names(covars)
covnames.vec <- c("IMD t-adj decile", "Median age", "Male share", "Population density", "Mortality rate")

### Function to plot residuals by domain
residplot.fun = function(xF, yF) {
  
  covcount <<- covcount + 1  
  #f <- paste("residual ~", xF)
  #loess <- loess(f, data = IMD_samhi_TS_agg.df, span = 0.25) 
  #loess_fit <- as.data.frame(predict(loess))
  #names(loess_fit) <- c("fit")
  #IMD_samhi_TS_agg.df$fit <- loess_fit$fit
  #IMD_samhi_TS_agg.df <- IMD_samhi_TS_agg.df %>%
   # dplyr::arrange(xF)
  
  covres.gg <- ggplot(IMD_samhi_TS_agg.df, aes(x = .data[[xF]], y = .data[[yF]]) ) +
    geom_hline(yintercept=0, size= 0.25, color="black", linetype = "dashed") +
    
    geom_point(shape = 21, size = 1, fill = "#a0a0a0", color = "black", alpha = .1, stroke = .05,
               position = position_jitter(w = 0.2)) + 
    
    geom_rug(alpha = 0.1, color = "#a0a0a0",
             outside = TRUE, sides = "tr",
             length = unit(0.2,"cm")) +
    
    coord_cartesian(clip = "off") +
    
    #geom_line(IMD_samhi_TS_agg.df, 
     #         mapping = aes(x = .data[[xF]], y = fit), col = "#EFC000", size = 1) +
    
    labs(x = covnames.vec[covcount], y = "Residual") +
    theme_bw() +
    theme(
      plot.margin = unit(c(.2, .5, .2, .2), "cm"))
  
  #covres.gg <- ggmarginal(covres.gg, type = "histogram")
  
  if (xF == "IMD_tadj_dec_ipol" || xF == "male_share" || xF == "moRz") {
    
    covres.gg <- covres.gg + theme(axis.title.y=element_text(vjust = -0.05))
    
    return(covres.gg)
  } 
  
  else {
    
    covres.gg <- covres.gg + theme(axis.title.y=element_blank(),
                                   axis.text.y = element_blank(),
                                   axis.ticks.y = element_blank())
    return(covres.gg)
  }
  
}


covcount <- 0   
covresplots <- lapply(covars, FUN = residplot.fun, y= "residual")


covresC.gg <- cowplot::plot_grid(covresplots[[1]], covresplots[[2]],
                                 covresplots[[3]], covresplots[[4]],
                                 covresplots[[5]], nrow=3,
                                 rel_widths = c(1.06, .94, .94, 1.06, .94)) 

covresC.gg <- covresC.gg +
  theme(plot.margin = margin(t=2, r=2, b=7, l=1, "mm"))

#P4_line_1 <- expression(paste(italic("Note"),". Plot of residuals for continuous model covariates from pooled regression on mental health index. Solid yellow lines represent")) 
#P4_line_2 <- ("locally smoothed fit from LOESS regression using a bandwidth of 0.25.")
#covresC.gg <- covresC.gg + # allows plotting anywhere on the canvas
 # draw_label(P4_line_1, x = .5, y = -0.01725, size = 7.5) +
  #draw_label(P4_line_2, x = .275, y = -0.045, size = 7.5)

covresC.gg


# REWB estimation / prediction    
################################################################################

#| Within-Between components of IMD deciles 
#--------------------------------------------

IMD_samhi_TS_agg.df <- IMD_samhi_TS_agg.df %>%
  group_by(LSOA_code_agg) %>%
  dplyr::mutate(across(c(IMD_dec_ipol, IMD_tadj_dec_ipol, medage,
                         male_share, popdens, moRz), ~ mean(.),
                         .names = "B_{.col}")) %>%
  ungroup()

IMD_samhi_TS_agg.df <- IMD_samhi_TS_agg.df %>%
  dplyr::mutate(across(c(IMD_dec_ipol, IMD_tadj_dec_ipol, medage,
                         male_share, popdens, moRz),
                        ~ . - pull(IMD_samhi_TS_agg.df, paste0("B_",cur_column()) ),
                         .names = "W_{.col}"))

#| REWB original IMD 
#---------------------------
rewb <- plm(samhi ~ W_IMD_dec_ipol + B_IMD_dec_ipol+
                          W_medage + B_medage + W_male_share +
                          B_male_share + W_popdens + B_popdens +
                          W_moRz + B_moRz + as.factor(resarea_code),
            data = IMD_samhi_TS_agg.df, model = "random",
            random.method = "walhus", index = c("LSOA_code_agg", "year"))

rewb_vcov <- clubSandwich::vcovCR(rewb, type = "CR1S")      # Clustered SEs

for (i in 1:18) { 
  for(j in 1:18) {                         
    rewb$vcov[ i, j ] <- rewb_vcov[ i, j]                   # Update VCV matrix
  }
}


#| REWB time-adjusted IMD 
#---------------------------
rewbtadj <- plm(samhi ~ W_IMD_tadj_dec_ipol + B_IMD_tadj_dec_ipol+
              W_medage + B_medage + W_male_share +
              B_male_share + W_popdens + B_popdens +
              W_moRz + B_moRz + as.factor(resarea_code),
            data = IMD_samhi_TS_agg.df, model = "random",
            random.method = "walhus", index = c("LSOA_code_agg", "year"))

rewbtadj_vcov <- clubSandwich::vcovCR(rewbtadj, type = "CR1S")      # Clustered SEs

for (i in 1:18) { 
  for(j in 1:18) {                         
    rewbtadj$vcov[ i, j ] <- rewbtadj_vcov[ i, j]                   # Update VCV matrix
  }
}


# REWB table
################################################################################

#| Model summaries
#----------------------------

coefnames <- c( "W_IMD_dec_ipol" = "Within", "B_IMD_dec_ipol" = "Between",
                "W_IMD_tadj_dec_ipol" = "Within ", "B_IMD_tadj_dec_ipol" = "Between ",
                "W_medage" = "Within  ", "B_medage" = "Between  ",
                "W_male_share" = "Within   ", "B_male_share" = "Between   ",
                "W_popdens" = "Within    ", "B_popdens" = "Between    ", 
                "W_moRz" = "Within     ", "B_moRz" = "Between     ",
                "as.factor(resarea_code)2" = "Countryside living",
                "as.factor(resarea_code)3" = "Ethnically diverse professionals",
                "as.factor(resarea_code)4" = "Hard-pressed communities",
                "as.factor(resarea_code)5" = "Industrious communities",
                "as.factor(resarea_code)6" = "Inner city cosmopolitan",
                "as.factor(resarea_code)7" = "Multicultural living",
                "as.factor(resarea_code)8" = "Surburban living",
                "(Intercept)" = "Constant" )
  
  
cap <- "Table of coefficients for REWB panel regression of IMD versus time-adjusted IMD on
        Small Area Mental Health Index with controls"

gm <- tibble::tribble(
  ~raw,         ~clean,      ~omit,
  "nobs",         "obs",       T,
  "r.squared",    "R",       T    
  )

 tab <- modelsummary(list("coef." = rewb,
                         "s.e."   = rewb,
                         "coef."  = rewbtadj,
                         "s.e."   = rewbtadj),
                    estimate  = c("estimate","std.error",
                                  "estimate","std.error"),
                    statistic = NULL,
                    coef_map = coefnames,
                    gof_map = gm,
                    output = "kableExtra", escape=FALSE, 
                    align = "lcccc", stars = FALSE)

 rewbtab <- tab %>%
  
  add_header_above(c(" " = 1, "With IMD" = 2, "With <i>t</i>-adjusted IMD" = 2),
                   line = T, line_sep = 5, escape = FALSE) %>%

   kable_classic(html_font = "Cambria", font_size = 10) %>%
   kable_styling(full_width = T) %>%
   

  pack_rows("IMD", 1, 2, bold= F, italic = T) %>%
  pack_rows("IMD <i>t</i>-adjusted", 3, 4, bold = F, italic = T, escape = FALSE) %>%
  pack_rows("Median age", 5, 6, bold = F, italic = T) %>%
  pack_rows("Male residents (%)", 7, 8, bold = F, italic = T) %>%
  pack_rows("Population density", 9, 10, bold = F, italic = T) %>%
  pack_rows("Mortality rate", 11, 12, bold = F, italic = T) %>%
  pack_rows("Area classification \n (ref. Cosmopolitian student)", 13, 19, bold = F, italic = T) %>%
   
  row_spec(1, extra_css = "border-top: .8px solid; border-color: white") %>%
  row_spec(2, extra_css = "border-top: .8px solid; border-color: white") %>%
  row_spec(3, extra_css = "border-top: .8px solid; border-color: white") %>%
  row_spec(4, extra_css = "border-top: .8px solid; border-color: white") %>%
  row_spec(5, extra_css = "border-top: .8px solid; border-color: white") %>%
  row_spec(6, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(7, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(8, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(9, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(10, extra_css = "border-top: .8px solid; border-color: white") %>%
 row_spec(11, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(12, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(13, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(14, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(15, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(16, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(17, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(18, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(19, extra_css = "border-top: .8px solid; border-color: white") %>%
   row_spec(20, extra_css = "border-top: .8px solid; border-color: lightgrey") %>%
  # row_spec(21, extra_css = "border-top: .2px solid; border-color: black") %>%
  # row_spec(22, extra_css = "border-top: .8px solid; border-color: white") %>%
   
   #row_spec(1:2, bold = T, color = "white", background = "lightgreen") %>%
   #row_spec(3:4, bold = T, color = "white", background = "lightblue")  %>%
 
  footnote(general = "Coefficients with cluster-robust standard errors. <br> <i>p</i> < 0.01 for all model coefficients.",
           general_title = "Note.",
           footnote_as_chunk = T, title_format = c("italic"), escape=F) 
rewbtab
 
 
# REWBI table
################################################################################
 
# Export to Stata
write.csv(IMD_samhi_TS_agg.df, "Time Adjusted IMD/Data/Derived/IMD_samhi_TS_agg.csv", row.names = TRUE)

# Import
rewbi_tab.df <- read.csv("Time Adjusted IMD/Data/Derived/table_rewbi.csv")

# Drop redundant observation
rewbi_tab.df <- rewbi_tab.df[, -c(1, 3, 5, 7, 9)]
rewbi_tab.df <- rewbi_tab.df[-13,]

                  
                  
# Round estimates to 3dp
rewbi_tab.df <- rewbi_tab.df[] %>%
  mutate(mutate(across(c(1:4), ~  round(., 3))))

# Column names
colnames <- c("coef.", "coef. ", 
               "coef.  ", 
              "coef.   ")
              colnames(rewbi_tab.df) <- colnames
 
# Re-order            
rewbi_tab.df <- rewbi_tab.df[c(1,2,3,4,5,9,6,10,7,11,8,12,13:27),]

rewbi_tab.df <- rewbi_tab.df %>%
 add_row("coef." = NA,  "coef. " = NA ,
          "coef.  " = NA, 
         "coef.   " = NA,  .before=20)


# Row names
rownames <- c("Within", "Between", "Within ", "Between ",
              "Within  ", "Between  ", "Within   ", "Between   ",
              "Within    ", "Between    ", "Within     ", "Between     ",
              "Countryside living", "Ethnically diverse professionals", 
              "Hard-pressed communities", "Industrious communities",
              "Inner city cosmpolitan", "Multicultural living", "Surburban living",
              "Within         ", "Countryside living ", "Ethnically diverse professionals ", 
              "Hard-pressed communities ",
              "Industrious communities ", "Inner city cosmpolitan ", 
              "Multicultural living ", "Surburban living ", "Constant ")

rownames(rewbi_tab.df) <- rownames


# p vales
pvals.mat <- matrix(nrow = 28, ncol = 4)
pvals.mat[] <- NA

pvals.mat[4,1] <- ""
pvals.mat[4,2] <- ""
pvals.mat[3,3] <- "*"
pvals.mat[4,3] <- ""
pvals.mat[1,4] <- "*"
pvals.mat[3,4] <- ""
pvals.mat[4,4] <- ""

for (r in 1:28) {
  
  for (c in 1:4) {
    
    
    if (!is.na(pvals.mat[r,c])) {
      
      stars <- pvals.mat[r,c]
      
      #print(paste(pmodels.mat[r,c], stars))
      
      
      
      rewbi_tab.df[r,c] <- paste0(rewbi_tab.df[r,c], stars)
    }
    
    else if (!is.na(rewbi_tab.df[r, c])) {
      
      stars <- "**"
      rewbi_tab.df[r,c] <- paste0(rewbi_tab.df[r,c], stars)
      
    }
  }
}





options(knitr.kable.NA = "")
kbl(rewbi_tab.df, align = rep("c", 8)) %>%
  add_header_above(c(" " = 1, "Original IMD" = 1, "<i>t</i>-adjusted IMD" = 1,
                     "Original IMD" = 1, "<i>t</i>-adjusted IMD" = 1 ),
                   line = T, line_sep = 5, escape = FALSE) %>%
  add_header_above(c(" ", "(1)" = 2, "(2)" = 2)) %>%
  
  kable_classic(html_font = "Cambria", font_size = 10) %>%
  kable_styling(full_width = T) %>%
  
  
  pack_rows("IMD", 1, 2, bold= F, italic = T) %>%
  pack_rows("Within IMD X Between IMD", 3, 4, bold = F, italic = T) %>%
  pack_rows("Median age", 5, 6, bold = F, italic = T) %>%
  pack_rows("Male residents (%)", 7, 8, bold = F, italic = T) %>%
  pack_rows("Population density", 9, 10, bold = F, italic = T) %>%
  pack_rows("Mortality rate", 11, 12, bold = F, italic = T) %>%
  pack_rows("Area classification \n (ref. Cosmopolitian student)", 13, 19, bold = F, italic = T) %>%
  pack_rows("Within IMD X Between IMD \n X Area classification \n (ref. Cosmopolitian student)", 20, 27, bold = F, italic = T) %>% 
  
  add_indent(positions = c(21:27)) %>%
  
  
  row_spec(1, bold = T,  extra_css = "border-top: .8px solid; background-color: lightgrey;  border-color: lightgrey") %>%
  row_spec(2, extra_css = "border-top: .8px solid; border-color: white") %>%
  row_spec(3, bold = T, extra_css = "border-top: .8px solid; background-color: lightgrey;  border-color: lightgrey") %>%
  row_spec(4:20, extra_css = "border-top: .8px solid; border-color: white") %>%
  row_spec(21, bold = T,  extra_css = "border-top: .8px solid; background-color: lightgrey;  border-color: lightgrey") %>%
  row_spec(22, bold = T, extra_css = "border-top: .2px solid; background-color: darkgrey;  border-color: darkgrey") %>%
  row_spec(23, bold = T, extra_css = "border-top: .8px solid; background-color: lightgrey;  border-color: lightgrey") %>%
  row_spec(24, bold = T, extra_css = "border-top: .8px solid; background-color: darkgrey;  border-color: darkgrey") %>%
  row_spec(25, bold = T, extra_css = "border-top: .8px solid; background-color: lightgrey;  border-color: lightgrey") %>%
  row_spec(26, bold = T, extra_css = "border-top: .8px solid; background-color: darkgrey;  border-color: darkgrey") %>%
  row_spec(27, bold = T, extra_css = "border-top: .8px solid; background-color: lightgrey;  border-color: lightgrey") %>%
  
  footnote(general = "* <i>p</i> <0.05; ** <i>p</i> <0.01",
           general_title = "<i>Note</i>.",
           footnote_as_chunk = T, escape = F) 



# PLOT REWBI
#################################################################################   
    
  
 rewbipred.df <- read.csv("Time Adjusted IMD/Data/Derived/pred_rewbi.csv")
 
 
rewbi.gg <- 
  
  ggplot(data=rewbipred.df, aes(x= bdec, 
                                          y=X_e, group = m,color = as.factor(m), fill = as.factor(m))) +
  
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
  geom_line(size = 0.75, position = position_dodge(width = 0.5)) +
  
  geom_pointrange(mapping =aes(ymin = X_lb, ymax = X_ub),
                  shape =21, size = .7, stroke = .2, fill = "white",position = position_dodge(width = 0.5)) +
  

  #geom_point(shape =21, size = 1.75, stroke = .1, fill = "white",position = position_dodge(width = 0.5))  +
  
  scale_color_manual(values = c("1" ="#868686",
                                "2" = "#efc000")) +
  
  
  scale_fill_manual(values = c("1" ="#868686",
                               "2" = "#efc000FF")) +
  
  expand_limits(y = c(0, 0.5)) +
  
  
  expand_limits(y = c(-0.5, 1.5)) +
  scale_x_continuous( breaks = seq(1, 9, by = 1))  +
  ylab("Within effect on mortality rate\n ") +
  xlab(expression(paste("IMD decile rank (",italic(beta)[1],italic(bar(x)[i]),")")))   +
  annotate("text",x =-0.3, y = 0.21,parse = T, label = as.character(expression(paste(
    "(",italic(beta)[1],italic(x[it])," = ", italic(beta)[1],italic(bar(x)[i])," +1)"))),
    size = 3.88, angle=90)  +
  
  
  coord_cartesian(ylim = c(-0.5, 1.5), xlim=c(0.75, 9),clip = "off") +
  
  annotate(geom = "richtext", x = 1.1, y = 0.24, 
           label = "**IMD**", color = "#868686",
           label.padding = unit(c(.3, 1, .15, 1), "lines"),
           size = 3.5, label.size = 0.3) +
  
  annotate("richtext", x = 1.35, y = -0.1,              # have to plot last to stop markdown bug
           label = "**<i>t</i>-adj IMD**", color = "#efc000",
           label.padding = unit(c(0.325,0.29,0.29,0.29), "lines"),
           fill = "white", size = 3.5, label.size = 0.3) +
  
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = margin(2,2, 2,2, "mm")
        
  )

rewbi.gg


#P5_line_1 <- expression(paste(italic("Note"),". Plot of the within-LSOA effect of IMD versus time-adjusted IMD across between-LSOA decile ranks. Estimates are from a"))
#P5_line_2 <- expression(paste("REWB model with a cross-level interaction term between the within and between effect of IMD/time-adjusted IMD. Estimates"))
#P5_line_3 <- expression(paste("represent the effect of a 1 unit change in the within component of IMD and the within component of the cross-level interaction term"))
#P5_line_4 <- expression(paste(", switching corresponding between components from the ", 1^st," to ", 9^th," decile."))


# Plot REWBI3
###############################################################################
rewbi3pred.df <- read.csv("Time Adjusted IMD/Data/Derived/pred_rewbi3.csv")

### Plot
rewbi3.gg <- ggplot(data=rewbi3pred.df, aes(x= bdec, y=X_e, group = m,color = as.factor(m))) +

  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  
  geom_line(size = .75, position = position_dodge(width = 0.7)) +
  
  geom_point(shape =21, size = 1.75, stroke = .2, fill = "white",position = position_dodge(width = 0.7))  +
  
   scale_color_manual(values = c("1" ="#868686",
                             "2" = "#efc000"), 
                      labels = c("IMD", "t-adjusted IMD")) +
  
  scale_x_continuous( breaks = seq(1, 9, by = 1)) +
  scale_y_continuous( breaks = seq(-.25, .75, by = .5)) +
  ylab("Within effect on mental health") +
  xlab(expression(paste("IMD decile rank (",italic(beta)[1],italic(bar(x)[i]),")")))  +

  coord_cartesian(ylim = c(-0.25, .94), xlim=c(.75, 9),clip = "off") +
  
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = margin(2,2, 2,1, "mm")
  )


### Facet plot
area.labs <- c("Cosmopolitan student", "Countryside living", "Ethnically diverse professionals", 
               "Hard-pressed communities", "Industrious communities",
               "Inner city cosmpolitan", "Multicultural living", "Surburban living")
names(area.labs) <- c("1", "2", "3", "4", "5", "6", "7", "8")
  
rewbi3F.gg <- rewbi3.gg + facet_wrap(~ area, nrow=3,
                        labeller = labeller(area = area.labs)) +
  theme_bw() +
  theme(strip.background =element_rect(fill="white", color = "black", size = .8))+
  theme(strip.text = element_text(colour = 'black')) +
  theme(legend.position = c(.94, .05),
        legend.justification = c(1.1, -0.1),
        legend.title = element_blank(),
        legend.margin=margin(t = .2, unit='cm'),
        ) 
rewbi3F.gg


save.image (file = "IMDtadj_space2.RData")

