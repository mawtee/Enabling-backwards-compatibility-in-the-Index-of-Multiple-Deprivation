################################################################################
############################  TIME-ADJUSTED IMD 03  ############################
################################################################################

# Author: Matthew Tibbles 

# Description
################################################################################





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

#| Main data frame 
#-------------------
IMD_TS_moRz.df <- readRDS("Time Adjusted IMD/Data/Derived/IMD_TS_moRz.rds")



# RE estimation / prediction   
################################################################################

#| RE original IMD 
#---------------------

re <- plm(moRz ~ IMD_dec_ipol,                           # Panel estimation
          data = IMD_TS_moRz.df,
          model = "random",
          random.method = "walhus",
          index = c("LSOA_code", "year"))

re_vcov <- clubSandwich::vcovCR(re, type = "CR1S")         # Clustered SEs

for (i in 1:2) {  
  for (j in 1:2) {  
    re$vcov[ i, j ] <- re_vcov[ i, j] 
  }                                                     
}

re_pr.df <- summary(margins(re))                           # AME

re_pr.df$m <- factor(1)                                    # Identifiers
re_pr.df$e <- factor(2)


#| RE t-adjusted IMD 
#---------------------                                     # Panel estimation
retadj <- plm(moRz ~ IMD_tadj_dec_ipol,
              data = IMD_TS_moRz.df,
              model = "random",
              random.method = "walhus",
              index = c("LSOA_code", "year"))

retadj_vcov <- clubSandwich::vcovCR(retadj, type = "CR1S") # Clustered SEs

for (i in 1:2) {
  for (j in 1:2) {
    retadj$vcov[ i, j ] <- retadj_vcov[ i, j]                # Update VCV matrix
  }
}

retadj_pr.df <- summary(margins(retadj))                   # AME           

retadj_pr.df$m <- factor(2)                                # Identifiers
retadj_pr.df$e <- factor(2)



#    REWB estimation / prediction    
################################################################################

#| Within-Between components of IMD deciles 
#--------------------------------------------
IMD_TS_moRz.df <- IMD_TS_moRz.df %>%
  group_by(LSOA_code) %>%
  dplyr::mutate(B_IMD_dec_ipol = mean(IMD_dec_ipol)) %>%
  ungroup() %>%
  dplyr::mutate(W_IMD_dec_ipol = IMD_dec_ipol - B_IMD_dec_ipol) %>% 
  group_by(LSOA_code) %>%
  dplyr::mutate(B_IMD_tadj_dec_ipol = mean(IMD_tadj_dec_ipol)) %>% 
  ungroup() %>%
  dplyr::mutate(W_IMD_tadj_dec_ipol = IMD_tadj_dec_ipol -
                  B_IMD_tadj_dec_ipol) 


#| REWB original IMD 
#---------------------------
rewb <- plm(moRz ~ W_IMD_dec_ipol + B_IMD_dec_ipol,     # Panel estimation
            data = IMD_TS_moRz.df, model = "random",
            random.method = "walhus", index = c("LSOA_code", "year"))

rewb_vcov <- clubSandwich::vcovCR(rewb, type = "CR1S")      # Clustered SEs

for (i in 1:3) { 
  for(j in 1:3) {                         
    rewb$vcov[ i, j ] <- rewb_vcov[ i, j]                   # Update VCV matrix
  }
}

bAV <- mean(IMD_TS_moRz.df$B_IMD_dec_ipol)
rewb_pr.df <- summary(margins(                              # AME 
  rewb, at = list(B_IMD_dec_ipol = bAV))) 

rewb_pr.df$m <- factor(1)
rewb_pr.df$e <- factor(c(3,1))                              # Identifiers


#| REWB t-adjusted IMD 
#--------------------------
rewbtadj <- plm(moRz ~ W_IMD_tadj_dec_ipol + B_IMD_tadj_dec_ipol,
                data = IMD_TS_moRz.df,                    
                model = "random",                           # Panel estimation
                random.method = "walhus",
                index = c("LSOA_code", "year"))

rewbtadj_vcov <- clubSandwich::vcovCR(rewbtadj, 
                                      type = "CR1S")        # Clustered SEs
for (i in 1:3) { 
  for(j in 1:3) {                         
    rewbtadj$vcov[ i, j ] <- rewbtadj_vcov[ i, j]           # Update VCV matrix
  }
}

bAVtadj <- mean(IMD_TS_moRz.df$B_IMD_tadj_dec_ipol)
rewbtadj_pr.df <- summary(margins(rewbtadj, at =            # AME
                                    list(B_IMD_tadj_dec_ipol =  bAVtadj))) 
rewbtadj_pr.df$m <- factor(2) 
rewbtadj_pr.df$e <- factor(c(3,1))                          # Identifiers




#| Join RE/REWB predictions plus re-order 
#|----------------------------------------
rewbpred.df <- dplyr::bind_rows(re_pr.df, retadj_pr.df, rewb_pr.df,
                                rewbtadj_pr.df) 
rewbpred.df <- rewbpred.df[order(rewbpred.df$factor),]


#| Get % change estimates
#-------------------------
rewbpred.df <- rewbpred.df %>%
  arrange(factor)

REpchange <- round(((rewbpred.df[4,2]/rewbpred.df[3,2])-1)*100, digits=2)
Wpchange <- round(((rewbpred.df[6,2]/rewbpred.df[5,2])-1)*100, digits=2)
Bpchange <- round(((rewbpred.df[2,2]/rewbpred.df[1,2])-1)*100, digits=2)


#| Export/import to/from Stata 
#--------------------------------------
# To perform Wald test and REWB analysis with cross level interaction
# of coefficients from separate models via GSEM
write.csv(IMD_TS_moRz.df, "Time Adjusted IMD/Data/Derived/IMDtadj_moRz.csv", row.names = TRUE)


#| # Get Wald chi squared estimates from Stata
wald.df <- read.csv("Time Adjusted IMD/Data/Derived/Wald.csv")
REwald <- weights::rd(wald.df[1,1], digits = 3)
Wwald <- weights::rd(wald.df[2,1], digits = 3)
Bwald <- weights::rd(wald.df[3,1], digits = 3)

# Get REWBi regress tab results/predictions from Stata
rsq.df <- read.csv("Time Adjusted IMD/Data/Derived/rsq_12.csv")
rewbicoef.df <- read.csv("Time Adjusted IMD/Data/Derived/coef_rewbi_12.csv")
rewbipred.df <- read.csv("Time Adjusted IMD/Data/Derived/pred_rewbi_12.csv")




# Coefficient table
################################################################################

# Create matrix
#------------------

# Matrix to store coefficients
pmodels.mat <- matrix(nrow = 15, ncol = 6,
                      dimnames = list(c("Random", "", "Within", "", "Between","",
                                        "Within", "", "Between","", "Intercept","","Within $R^{2}$", "Overall $R^{2}$",  "Between $R^{2}$" ),
                                      c("(1)", "(2)", "(3)",
                                        "(1)", "(2)", "(3)")))
# Populate
pmodels.mat[1,1] <- re$coefficients[[2]]
pmodels.mat[2,1] <- sqrt(re$vcov[[2,2]])
pmodels.mat[11,1] <- re$coefficients[[1]]
pmodels.mat[12,1] <- sqrt(re$vcov[[1,1]])
pmodels.mat[14,1] <- rsq.df[1,1]

pmodels.mat[3,2] <- rewb$coefficients[[2]]
pmodels.mat[4,2] <- sqrt(rewb$vcov[[2,2]])
pmodels.mat[5,2] <- rewb$coefficients[[3]]
pmodels.mat[6,2] <- sqrt(rewb$vcov[[3,3]])
pmodels.mat[11,2] <- rewb$coefficients[[1]]
pmodels.mat[12,2] <- sqrt(rewb$vcov[[1,1]])
pmodels.mat[13,2] <- rsq.df[2,1]
pmodels.mat[14,2] <- rsq.df[3,1]
pmodels.mat[15,2] <- rsq.df[4,1]

pmodels.mat[3,3] <- rewbicoef.df[1,1] 
pmodels.mat[4,3] <- rewbicoef.df[2,1] 
pmodels.mat[5,3] <- rewbicoef.df[1,2] 
pmodels.mat[6,3] <- rewbicoef.df[2,2] 
pmodels.mat[7,3] <- rewbicoef.df[1,3] 
pmodels.mat[8,3] <- rewbicoef.df[2,3] 
pmodels.mat[9,3] <- rewbicoef.df[1,4] 
pmodels.mat[10,3] <- rewbicoef.df[2,4]
pmodels.mat[11,3] <- rewbicoef.df[1,5]
pmodels.mat[12,3] <- rewbicoef.df[2,5]
pmodels.mat[13,3] <- rsq.df[5,1]
pmodels.mat[14,3] <- rsq.df[6,1]
pmodels.mat[15,3] <- rsq.df[7,1]


pmodels.mat[1,4] <- retadj$coefficients[[2]]
pmodels.mat[2,4] <- sqrt(retadj$vcov[[2,2]])
pmodels.mat[11,4] <- retadj$coefficients[[1]]
pmodels.mat[12,4] <- sqrt(retadj$vcov[[1,1]])
pmodels.mat[14,4] <- rsq.df[8,1]

pmodels.mat[3,5] <- rewbtadj$coefficients[[2]]
pmodels.mat[4,5] <- sqrt(rewbtadj$vcov[[2,2]])
pmodels.mat[5,5] <- rewbtadj$coefficients[[3]]
pmodels.mat[6,5] <- sqrt(rewbtadj$vcov[[3,3]])
pmodels.mat[11,5] <- rewbtadj$coefficients[[1]]
pmodels.mat[12,5] <- sqrt(rewbtadj$vcov[[1,1]])
pmodels.mat[13,5] <- rsq.df[9,1]
pmodels.mat[14,5] <- rsq.df[10,1]
pmodels.mat[15,5] <- rsq.df[11,1]

pmodels.mat[3,6] <- rewbicoef.df[3,1] 
pmodels.mat[4,6] <- rewbicoef.df[4,1] 
pmodels.mat[5,6] <- rewbicoef.df[3,2] 
pmodels.mat[6,6] <- rewbicoef.df[4,2] 
pmodels.mat[7,6] <- rewbicoef.df[3,3] 
pmodels.mat[8,6] <- rewbicoef.df[4,3] 
pmodels.mat[9,6] <- rewbicoef.df[2,4] 
pmodels.mat[10,6] <- rewbicoef.df[4,4]
pmodels.mat[11,6] <- rewbicoef.df[3,5]
pmodels.mat[12,6] <- rewbicoef.df[4,5]
pmodels.mat[13,6] <- rsq.df[12,1]
pmodels.mat[14,6] <- rsq.df[13,1]
pmodels.mat[15,6] <- rsq.df[14,1]

# Program to format matrix cells
#-----------------------------------

# Loop over rows
for (r in 1:15) {
  
  # Loop over cols
  for (c in 1:6) {
    
    
    # Do the following for SE cells
    if (r %in% seq(from = 2, to = 12, by = 2)) {
      # Put parentheses around SE and round to 4dp
      if (!is.na(pmodels.mat[r,c])) {
        num <- as.numeric(pmodels.mat[r,c])
        print (num)
        if (num > 1000 | num < -1000) {
          pmodels.mat[r,c] <- sprintf("(%.1f)", num)
        }
        else {
          pmodels.mat[r,c] <- sprintf("(%.4f)", num)
        }
      }
      # Blank if NA
      else {
        pmodels.mat[r,c] <- ""
      } 
    }
    
    
    # Do the following for coefficient cells
    else  {
      # Round to 4dp
      if (!is.na(pmodels.mat[r,c])) {
        num <- as.numeric(pmodels.mat[r,c])
        if (num >1000 | num < -1000) {
          pmodels.mat[r,c] <- sprintf("%.1f", num)
        }
        else {
          pmodels.mat[r,c] <- sprintf("%.4f", num)
        }
      }
      # Blank if NA
      else {
        pmodels.mat[r,c] <- ""
      }
    }
  }
}



# p vales
pvals.mat <- matrix(nrow = 15, ncol = 6)
pvals.mat[1,1] <- "**"
pvals.mat[11,1] <- "**"
pvals.mat[3,2] <- "**"
pvals.mat[5,2] <- "**"
pvals.mat[11,2] <- "**"
pvals.mat[7,3] <- "**"
pvals.mat[11,3] <- "**"

pvals.mat[1,4] <- "**"
pvals.mat[11,4] <- "**"
pvals.mat[3,5] <- "**"
pvals.mat[5,5] <- "**"
pvals.mat [11,5] <- "**"
pvals.mat[3,6] <- "*"
pvals.mat[5,6] <- "**"
pvals.mat[7,6] <- "**"
pvals.mat[9,6] <- "*"
pvals.mat[11,6] <- "**"


for (r in 1:11) {
  
  for (c in 1:6) {
    
    if (!is.na(pvals.mat[r,c])) {
      
      stars <- pvals.mat[r,c]
      
      print(paste(pmodels.mat[r,c], stars))
      
      pmodels.mat[r,c] <- paste0(pmodels.mat[r,c], stars)
    }
  }
}




kbl(pmodels.mat, align = c(rep("c", 6))) %>%
  add_header_above(c(" " = 1, "With IMD" = 3, "With \\emph{t}-adjusted IMD" = 3),
                   line = T, line_sep = 7.5) %>% 
  
  kable_classic(full_width = F,html_font = "Cambria", font_size = 11) %>%
  pack_rows("IMD", 1, 6, bold= T, italic = T) %>%
  pack_rows("Within X Between IMD", 7, 10, bold = T, italic = T) %>%
  # pack_rows("$R^{2}$", 13, 15, bold = T, italic = F) %>%
  
  row_spec(1, extra_css = "border-top: .8px solid; border-color: lightgrey") %>%
  row_spec(7, extra_css = "border-top: .8px solid; border-color: lightgrey") %>%
  row_spec(11, extra_css = "border-top: .8px solid; border-color: lightgrey") %>%
  row_spec(13, extra_css = "border-top: 1px solid; border-color: black") %>%
  
  footnote(general = "Raw coefficients; cluster-robust standard errors in parentheses. \n* p <0.05; ** p <0.01",
           general_title = "Note.",
           footnote_as_chunk = T, title_format = c("italic")) 

#kbl(pmodels.mat,format = "latex", booktabs = T,linesep = "\\addlinespace",
#escape = FALSE, align = c(rep("c", 6))) %>%
# add_header_above(c(" " = 1, "With IMD" = 3, "With \\emph{t}-adjusted IMD" = 3),
#line = T, line_sep = 7.5) %>%

#    Plot 1   
################################################################################

attach(rewbpred.df)
rewbpred.df <- rewbpred.df[order(m, e),]
rewbpred.df$me <- interaction(m, e)


rewb.gg <-ggplot(data=rewbpred.df, aes(x= m, 
                                      y=AME, color=factor(e, c(1,2,3)), fill = factor(e, c(1,2,3)))) +
  geom_pointrange(aes(ymin = lower, ymax=upper), 
                  position = position_dodge(width = .5)) +
  geom_line(aes(group = factor(e, c(1,2,3))), 
            linetype = "dotted", position = position_dodge(width = .5)) +
  scale_color_manual(values = c("3" = "#004b81", # red bars
                                "2" = "#3b3b3b", # grey bars
                                "1" = "#8f7700FF")) +  # blue bars 
  geom_point(shape = 21, color = "white", size = 3,
             position = position_dodge(width = .5), stroke = 1) +
  scale_fill_manual(values = c("3" = "#004b81", # red bars
                               "2" = "#3b3b3b", # grey bars
                               "1" = "#8f7700FF")) + # blue bars 
  
  scale_x_discrete(labels = c("IMD", "<i>t</i>-adjusted IMD"),
                   expand=c(0,0.35)) +
  ylab("Effect on mortality rate") +
  expand_limits(y = c(0, 0.15)) +
  scale_y_continuous(breaks = seq(0, 0.15, by = 0.025)) +
  annotate(geom = "richtext", x = 1.25, y = 0.0295, 
           label = paste0("**Within** <br> % change = ", Wpchange,"<br> Wald (<i>\u03b2<sub>1</sub>x<sub>it</sub></i>IMD = <i>\u03b2<sub>1</sub>x<sub>it</sub></i><i>t</i>-adj), <i>p</i> = ", Wwald),
           color = "#8f7700", size = 3) +
  annotate(geom = "richtext", x = 1.45, y = 0.0925, 
           label = paste0("**Random** <br> % change = ", REpchange,"<br> Wald (<i>\u03b2<sub>1</sub>IMD = <i>\u03b2<sub>1</sub></i><i>t</i>-adj), <i>p</i> ", REwald),
           color = "#3b3b3b", size = 3) +
  annotate(geom = "richtext", x = 1.8, y = 0.1375, 
           label = paste0("**Between** <br> % change = ", Bpchange,"<br> Wald (<i>\u03b2<sub>1</sub>\u0305x<sub>i</sub></i>IMD = <i>\u03b2<sub>1</sub>\u0305x<sub>i</sub></i><i>t</i>-adj), <i>p</i> ", Bwald),
           color = "#004b81", size = 3) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  
  theme(axis.text.x = element_markdown(color = "black", size = 11),
        
        axis.title.y = element_text(color = "black", size = 11),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(1,1,10,1), "mm"))

rewb.gg


# add note

P4_line_1 <- expression(paste(italic("Note"),". Plot of within, random and between effect of IMD versus time-adjusted IMD on standarized mortality rate. Estimates are average"))
P4_line_2 <- expression(paste("marginal effects with cluster-robust 95% confidence intervals. ", italic(p)," values from a Wald ",chi^2, " test of the equality of coefficients are based on"))
P4_line_3 <- expression(paste("joint estimation of models with IMD and time-adjusted IMD using a Generalised Structual Equation."))

rewb.gg <- rewb.gg +  # allows plotting anywhere on the canvas..6.24x3.82
  draw_label(P4_line_1, x = 1.416, y = -0.024, size = 7.5) + # use absolute coordinates for positioning
  draw_label(P4_line_2, x = 1.411, y = -0.02905, size = 7.5) +
  draw_label(P4_line_3, x = 1.1715, y = -0.0355, size = 7.5)
rewb.gg


# Plot 2
################################################################################

rewbipred_main.df<-subset(rewbipred.df, !((bdec == 8 | bdec == 9) & m == 1) )

rewbipred_main.df <- rewbipred_main.df %>%
  dplyr::mutate(bdec = case_when(bdec == 8 | bdec == 9 ~ as.numeric(bdec + 0.15),
                                 TRUE ~ as.numeric(bdec)))


rewbipred_inset.df<-subset(rewbipred.df, ((bdec == 8 | bdec == 9) & m == 1) )

rewbipred_inset.df$bdec = c(7.875, 8.875)




rewbi.gg <- ggplot(data=rewbipred.df, aes(x= bdec, 
                                      y=X_e, group = m,color = as.factor(m), fill = as.factor(m))) +
  
  geom_hline(yintercept=0, size= 0.25, color="black", linetype = "dashed") +
  
  
  geom_pointrange(ymin = rewbipred.df$X_lb, ymax = rewbipred.df$X_ub, position = position_dodge(width = .5)) +
  
  
  scale_color_manual(values = c("1" ="#868686",
                                "2" = "#efc000")) +
  
  geom_point(shape =21, size = 2.25, color = "white", stroke = .9, position = position_dodge(width = .5))  +
  
  scale_fill_manual(values = c("1" ="#868686",
                               "2" = "#efc000FF")) +
  
  #expand_limits(x = c(1, 9.5)) +
  
  
  #expand_limits(y = c(-0.2, 0.6)) +
  scale_x_continuous( breaks = seq(1, 9, by = 1)) +
  ylab("Within effect on mortality rate\n ") +
  xlab(expression(paste("IMD decile rank (",italic(beta)[1],italic(bar(x)[i]),")")))   +
  annotate("text",x =-0.09, y = 0.21,parse = T, label = as.character(expression(paste(
    "(",italic(beta)[1],italic(x[it])," = ", italic(beta)[1],italic(bar(x)[i])," +1)"))),
    size = 3.88, angle=90) +
  
  
  coord_cartesian(ylim = c(-0.2, 0.62), xlim=c(1, 9.5),clip = "off") +
  
  annotate(geom = "richtext", x = 1.35, y = 0.12, 
           label = "**IMD**", color = "#868686",
           label.padding = unit(c(.3, 1, .15, 1), "lines"),
           size = 3.5, label.size = 0.3) +
  
  annotate("richtext", x = 1.6, y = -0.095,              # have to plot last to stop markdown bug
           label = "**<i>t</i>-adj IMD**", color = "#efc000",
           label.padding = unit(c(0.325,0.29,0.29,0.29), "lines"),
           fill = "white", size = 3.5, label.size = 0.3) +
  
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = margin(2,2, 13.5,1, "mm")
        
  )
rewbi.gg



# add note 

P5_line_1 <- expression(paste(italic("Note"),". Plot of the within-LSOA effect of IMD versus time-adjusted IMD across between-LSOA decile ranks. Estimates are from a"))
P5_line_2 <- expression(paste("REWB model with a cross-level interaction term between the within and between effect of IMD/time-adjusted IMD. Estimates"))
P5_line_3 <- expression(paste("represent the effect of a 1 unit change in the within component of IMD and the within component of the cross-level interaction term"))
P5_line_4 <- expression(paste(", switching corresponding between components from the ", 1^st," to ", 9^th," decile."))

#while switching corresponding between components from the 1st to 9th IMD decile.")


rewbi.gg <- rewbi.gg + # allows plotting anywhere on the canvas
  draw_label(P5_line_1, x = 4.605, y = -0.404, size = 7.5) +
  
  
  # use absolute coordinates for positioning
  draw_label(P5_line_2, x = 4.485, y = -0.4435, size = 7.5)  +
  
  
  draw_label(P5_line_3, x = 4.725, y = -0.4835, size = 7.5)  +
  
  draw_label(P5_line_4, x = 2.4, y = -0.52, size = 7.5)
rewbi.gg



#6.05 x 3.53

save.image (file = "IMDtadj_space.RData")

