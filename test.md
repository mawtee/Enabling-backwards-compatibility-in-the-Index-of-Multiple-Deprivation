Enabling backwards compatibility in the Index of Multiple Deprivation
with an illustrative analysis of mortality rates
================
Matthew Tibbles
06/02/2022

<style>
p.caption {
  color: #777;
  margin-top: 10px;
  padding: 0px 0px 40px 0px;
}
div.sourceCode {
    margin: 0px 0px;
    margin-top: -15px;
    margin-right: 0px;
    margin-bottom: 1px;
    margin-left: 0px;
}
</style>

# Summary

## Context

The Index of Multiple Deprivation (IMD) is the standard measure of
deprivation and poverty in England. It is widely used by government and
researchers to allocate resources and inform recommendations on health
and social policy. Since 2004 the IMD has been based on the weighted
measurement of seven “domains” of deprivation, each of which is
compromised of multiple indicators based on official government data for
around 32,000 neighborhoods - Lower Level Super Output Areas (LSOA). The
weight attributed to each domain has remained consistent over time.
However, the composition and measurement of the underlying indicators of
each domain has undergone multiple changes - often in successive
versions of the IMD. On the one hand, this adaptive design philosophy
makes the IMD highly robust to policy changes and shifts in research and
data standards, ensuring that each version of the IMD captures the
determinants of deprivation as defined by the contexts of that time. On
the other hand, differences between each IMD preclude reliable
statistical analysis of the effects of deprivation across time.
Therefore, in a distinct lack of understanding of the effects of
relative change in deprivation levels. Put another way, while the IMD is
a rich source for analyzing the effects of *being* more or less
deprived, its widespread usage has somewhat stymied our understanding of
the effects of *becoming* more or less deprived, as well as of possible
inter-dependencies between these effects.

## Objectives

This research is guided by three main objectives:

1.  To devise a method for creating a time-adjusted IMD, enabling
    analysis of the effects of deprivation across time
2.  To apply of this method to the analysis of mortality rates,
    leveraging the explanatory potential of Random Effects Within
    Between panel models # Enabling backwards compatibility

## Method

### Crtiteria

In the 2015 and 2019 technical reports, the architects of the IMD state
that previous versions “should not be used as a time series” for the
following reasons:

1.  The addition of new indicators
2.  Changes to the measurement of existing indicators
3.  Changes to LSOA boundaries

I thus present a method to enable backwards compatibility via the
removal/minimization of the impacts of the above.

### Regression scaling

The method I employ is adapted from the work of Abel and colleagues, who
have constructed a UK-wide IMD which enables between-country comparisons
despite variation in the weighting and measurement of the domains which
form each country’s IMD. At its core, their method is designed to
exploit the fact that the weighting and measurement of the income and
employment domains is “approximately comparable across countries”. They
first fit a simple linear regression for each country, with IMD score as
the outcome variable and income and employment domain scores as
covariates. They then scale the income and employment scores - and the
regression residuals, which “represent the unique contribution of the
domains, other than income and employment, to the overall IMD” - of each
country by the regression equation of a given reference country.

Taking Northern Ireland, *n*, as the reference, adjusted IMD for country
*c* can be expressed as:

$$IMD\_{c-ADJUSTEDi} = \\beta\_{n0} + \\beta\_{n1}I\_{ci} + \\beta\_{n2}E\_{ci} + \\frac{\\epsilon\_{ci} \\sigma\_{n}} {\\sigma\_{c}}  $$
where *β*<sub>0</sub>, *β*<sub>1</sub> and *β*<sub>2</sub> are the
regression coefficients, *I*<sub>*i*</sub> is income score,
*E*<sub>*i*</sub> is employment score, *ϵ*<sub>*i*</sub> is the residual
value and *σ* is the residual S.D.

The resulting country-adjusted IMD is thus predominately based on
domains which are comparable between countries and which together
account for approximately 95% of within-country variation in IMD score.
The 5% of variation which is not explained by these domains is captured
by the residuals from each country regression, which are standardized
and scaled by the standard deviation of the residuals from the reference
country regression. In this way, the contribution of the other five
domains is reduced from 50% to roughly 5%.

### Reducing the influence of time-variant domains

The potential transferability of this method to the issue of backwards
compatibility is clear, given that variation in the measurement of
domain scores is the primary barrier to the use of IMD in both time
series and inter-country analysis. The particular exigencies of
backwards compatibility are, however, different to that of
between-country comparability. And so an adapted version of the above
method is required.

In the first instance, the domains which are comparable across countries
vary considerably across time. Specifically, the measurement of nearly
every indicator used for the income and employment domains was changed
in the 2019 IMD to account for the transformation of the benefit system
under UC. While these two domains alone have a combined weight of 50%
within the overall IMD, it is significant that the weighting and
measurement of the other five domains - the remaining 50% - is largely
the same across the 2010, 2015 and 2019 indices.

Table 1: By-year comparison of the weighting and underlying indicators
of five domains of IMD
<img src="C:/Users/Matt/Documents/Papers/Time Adjusted IMD/Scripts/tab1.jpg" title="FALSE" alt="FALSE" width="100%" style="display: block; margin: auto;" />
In fact, even after accounting for a few minor differences (see Table 1
footnotes), the weighting and measurement of these five domains across
time is more consistent than that of the income and employment domains
across countries. *Prima facie*, time-adjusted IMD can thus be expressed
as:

$$
IMD\_{t-ADJUSTEDi} =
\\beta\_{y0} + \\beta\_{y1}E\_{ti} + \\beta\_{y2}H\_{ti} +\\beta\_{y3}C\_{ti} + \\beta\_{y4}B\_{ti} + \\beta\_{y5}L\_{ti} + \\frac{\\epsilon\_{ti} \\sigma\_{y}} {\\sigma\_{t}}
$$
where *y* is the reference year, *E*<sub>*i*</sub> is education score,
*H*<sub>*i*</sub> is health score, *C*<sub>*i*</sub> is crime score,
*B*<sub>*i*</sub> is barriers score and *L*<sub>*i*</sub> is living
environment score.

Yet domain measurement is not only the source of temporal variation.
Changes to LSOA boundaries were made in 2011; this means the 2015 and
2019 indices are not geographically compatible with the 2010 IMD. In
total, 1,398 of 34,917 pre-2011 England LSOAs were affected by these
changes. 163 of these were subject to “complex correction”, for which it
is not possible to map to a 2011 equivalent. A further 922 were split
into two separate LSOAs, while 313 were merged into a single LSOA. It is
possible to map both split and merged LSOAs to 2011 equivalents.
However, this results in a “hybrid” LSOA geography, which is not readily
compatible with other UK area-based datasets. Therefore, with a view to
creating a time-adjusted IMD with out-of-the-box functionality, I drop
the 922 split LSOAs, and and their post-2011 equivalents. But keep the
313 merged LSOAs by aggregating 2010 IMD scores for affected pair-wise
areas. Overall, 1085 LSOAs were omitted, which equates to 3.1% of
England LSOAs.

## Implementation

### Data preparation and geographical aggregation

The 2010 IMD datset with domain scores:

``` r
head(IMD_2010.df, 10)
```

    ##    LSOA_code_old   IMD health  educ  barr crime livenv
    ## 1      E01000001  6.16  -2.11  0.21 32.60 -1.64  26.28
    ## 2      E01000002  5.59  -2.78  0.26 30.26 -1.93  25.73
    ## 3      E01000003 13.29  -0.97  7.16 40.32 -1.21  36.48
    ## 4      E01000004 11.17  -1.04  1.76 37.92 -1.32  46.72
    ## 5      E01000005 21.36   0.07 20.24 34.66 -1.02  40.94
    ## 6      E01000006 17.08  -0.38 13.84 32.81  0.42  19.39
    ## 7      E01000007 37.24   0.63 29.22 34.52  1.23  49.82
    ## 8      E01000008 45.22   0.55 29.85 34.25  0.95  51.89
    ## 9      E01000009 29.41   0.34 20.75 30.60  1.06  33.99
    ## 10     E01000010 38.45   0.41 22.72 30.68  1.50  43.65

Trim LSOA strings and merge with LSOA boundary changes lookup table:

``` r
IMD_2010.df$LSOA_code_old <- stringr::str_trim(IMD_2010.df$LSOA_code_old)
LSOA_bound.df$LSOA_code_old <- stringr::str_trim(LSOA_bound.df$LSOA_code_old)
LSOA_bound.df$LSOA_code <- stringr::str_trim(LSOA_bound.df$LSOA_code)

IMD_2010_bound.df <- dplyr::left_join(IMD_2010.df, LSOA_bound.df,
                                      by = "LSOA_code_old")
```

Aggregate merged LSOAs:

``` r
attach(IMD_2010_bound.df)
IMD_2010_2011B.df <- IMD_2010_bound.df %>%
  dplyr::group_by(LSOA_code) %>%
  dplyr::mutate(across(c(IMD, health, educ, barr, crime, livenv), ~
                         case_when(change == "M" ~ mean(.),
                                   TRUE ~ .)))  %>%
  dplyr::distinct(LSOA_code, .keep_all = TRUE)  %>%
  dplyr::ungroup()
detach(IMD_2010_bound.df)
```

### OLS regression

I first fit OLS regressions models of IMD score on IMD domains by year.

2010:

``` r
lm.2010 <- lm(IMD ~ educ + health + crime + barr + livenv, data = IMD_2010_2011B.df)
paste("$R^{2}$ = ", round(summary(lm.2010)$r.squared,3))
```

\[1\] “*R*<sup>2</sup> = 0.921” <br /> <br /> 2015:

``` r
lm.2015 <- lm(IMD ~ educ + health + crime + barr + livenv, data = IMD_2015.df)
paste("$R^{2}$ = ", round(summary(lm.2015)$r.squared,3))
```

\[1\] “*R*<sup>2</sup> = 0.928” <br /> <br /> 2019:

``` r
lm.2019 <- lm(IMD ~ educ + health + crime + barr + livenv, data = IMD_2019.df)
paste("$R^{2}$ = ",round(summary(lm.2019)$r.squared,3))
```

\[1\] “*R*<sup>2</sup> = 0.919” <br /> <br /> Two things to note here:

1.  2015 has the highest *R*<sup>2</sup>, and so will be used as the
    reference (scaling) year.
2.  *R*<sup>2</sup> values fall short of the 95% benchmark set by Abel
    and colleagues.

### Inspecting residuals

None of the models above are able to explain more than 92.28% of
variance in IMD. This amount of unexplained variance is potentially
problematic. Recall that the percentage of variation which is not
explained by the model is represented by the residual, which is
equivalent to the influence of the time-varying income and employment
domains. I thus plot residuals by domain score so as to assess model fit
and whether variable transformation is required.

Residual plot function with LOESS smoother:

``` r
# Define function
residplot.fun = function(xF, yF) {
  
  # Get loess smmothed predictions
  f <- paste("residual ~", xF)              
  domcount <<- domcount + 1                 
  loess <- loess(f, data = IMD_2015.df, span = 0.25)
  loess_fit <- as.data.frame(predict(loess))
  names(loess_fit) <- c("fit")                        
  IMD_2015.df$fit <- loess_fit$fit          
  IMD_2015.df <- IMD_2015.df %>%            
    dplyr::arrange(xF)
  
  # Residual plot
  resdom.gg <- 
    ggplot(IMD_2015.df, 
               aes(x = .data[[xF]], y = .data[[yF]]) ) +
    geom_hline(yintercept=0, size= 0.25, 
               color="black", linetype = "dashed") +
    geom_point(shape = 1, size = .2, color = "#868686",
               alpha = .4, position = position_jitter(w = 0.2)) +
    geom_line(IMD_2015.df, mapping = 
              aes(x = .data[[xF]], y = fit), col = "#EFC000",size= 1) +
    labs(x = domnames.vec[domcount], y = "Residual") +
    theme_bw() +
    theme(
      plot.margin = unit(c(.1, .1, .1, .1), "cm"))
  
  # Axis options depending on plot position
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

# Apply function to each IMD domain
domcount <- 0   
domresplots <- lapply(doms, FUN = residplot.fun, y= "residual")
```

Final combined plot:

``` r
# Combine residual plots
resdomC.gg <- cowplot::plot_grid(domresplots[[1]], domresplots[[2]],
                                 domresplots[[3]], domresplots[[4]],
                                 domresplots[[5]],nrow=2, 
                                 rel_widths = c(1.115, .94, 
                                                .94, 1.15, .94)) +
                       theme(plot.margin = margin(2,2, 8.5,1,"mm"))

# Add plot note
resdomC.gg <- resdomC.gg + 
  draw_label(P1_line_1, x = .485, y = -0.02, size = 7.5) +
  draw_label(P1_line_2, x = .1625, y = -0.05, size = 7.5)

resdomC.gg
```

![Residual plot by domain
score](test_files/figure-gfm/unnamed-chunk-12-1.png) From the above plot
we can see that the residuals are generally not well behaved. The health
domain exhibits a near-perfect parabolic curve, while a less severe
parabola can be seen in the crime domain. Even the education and living
environment domains exhibit some semblance of non-uniformity, with
systemic under-prediction occurring at the far end of both left and
right tails.

### MFP

I employ the Multivariable Fractional Polynomial procedure to determine
the best-fitting functional form for each domain score. A generalization
of polynomials, fractional polynomials provide a more flexible class of
transformation functions. Although it is true that the inclusion of a
simple square polynomial term would lead to a significant reduction in
nonlinearity, I prefer the added power of MFP given the paramount
importance of model fit in minimizing the influence of time-varying
domains.

Fit MFP model with 4 degrees of freedom (FP2-max):

``` r
mfp2015 <- mfp(IMD ~ fp(educ) + fp(health) + fp(crime) + fp(barr) +
                     fp(livenv), family = gaussian, data = IMD_2015.df, verbose = FALSE)
```

Best fitting forms:

``` r
mfp2015$powers
```

    ##        power1 power2
    ## educ        1      1
    ## health      1      2
    ## crime       1      2
    ## barr        3      3
    ## livenv      2      2

Coefficients

``` r
mfp2015$coefficients
```

    ##   Intercept      educ.1      educ.2    health.1    health.2     crime.1 
    ##   7.0713011   2.5423613   0.4072984 -77.9982958 226.6046843  -2.6348268 
    ##     crime.2      barr.1      barr.2    livenv.1    livenv.2 
    ##   1.1142323   0.3385441  -0.1510216   0.4942839  -0.1831674

### OLS versus MFP

Matrix to store coefficients:

``` r
domreg.mat <- matrix(nrow = 16, ncol = 6,
                     dimnames = list(c(rep(c("Linear", "FP1", "FP2"),5) ,"$R^{2}$"),
                                     c(rep(c("(OLS)", "(MFP)"),3))))
```

Get coefficients:

``` r
# Set increments
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
    
    # OLS loop
    if (mcount == 1) {
      for (i in seq(from = 1, to = 13, by =3)) {
        print(i)
        pcount <- pcount + 1
        domreg.mat[i,ycount] <-  eval(parse(text=paste0("lm.",y,".bcoef[[1]][[pcount]]")))
      }
      domreg.mat[16,ycount] <- eval(parse(text=paste0("summary(lm.",y,")$r.squared")))
      ycount <- ycount + 2
    }
    
    # MFP loop
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

# Round values and replace recode NAs as blank
domreg.mat <- round(domreg.mat, 3)       
domreg.mat[is.na(domreg.mat)] <- ""
```

Create table:

``` r
# Table structure/style
domreg.tab <- kbl(domreg.mat, align = c(rep("c", 5))) %>%
  add_header_above(c(" " = 1, "2010" = 2, "2015" = 2, "2019" = 2),
                   line = T, line_sep = 7.5
                   #caption = "Table 2: Table of coefficients for OLS versus MFP regression models by year"
                   ) %>%
  kable_classic(full_width = F,html_font = "Cambria", font_size = 11)%>%
  
  # Group rows by domains
  pack_rows("Education", 1, 3, bold= T, italic = T) %>%
  pack_rows("Health", 4, 6, bold = T, italic = T) %>%
  pack_rows("Crime", 7, 9,  bold = T, italic = T) %>%
  pack_rows("Barriers", 10, 12,  bold = T, italic = T) %>%
  pack_rows("Environment",13, 15, bold = T, italic = T) %>%
  
  # Rule lines to separate domains
  row_spec(1, extra_css = "border-top: .7px solid; border-color: grey") %>%
  row_spec(4, extra_css = "border-top: .7px solid; border-color: grey") %>%
  row_spec(7, extra_css = "border-top: .7px solid; border-color: grey") %>%
  row_spec(10, extra_css = "border-top: .7px solid; border-color: grey") %>%
  row_spec(13, extra_css = "border-top: .7px solid; border-color: grey") %>%
  row_spec(16, extra_css = "border-top: .7px solid; border-color: grey") %>%
  
  # Table note 
  footnote(general = "Standarized beta coefficients.",
           general_title = "Note.",
           footnote_as_chunk = T, title_format = c("italic")) 

domreg.tab
```

<table class=" lightable-classic" style="font-size: 11px; font-family: Cambria; width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
<thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1">
</th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:7.5px;padding-right:7.5px;text-align: center; " colspan="2">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

2010

</div>

</th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:7.5px;padding-right:7.5px;text-align: center; " colspan="2">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

2015

</div>

</th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:7.5px;padding-right:7.5px;text-align: center; " colspan="2">

<div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">

2019

</div>

</th>
</tr>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:center;">
(OLS)
</th>
<th style="text-align:center;">
(MFP)
</th>
<th style="text-align:center;">
(OLS)
</th>
<th style="text-align:center;">
(MFP)
</th>
<th style="text-align:center;">
(OLS)
</th>
<th style="text-align:center;">
(MFP)
</th>
</tr>
</thead>
<tbody>
<tr grouplength="3">
<td colspan="7" style="border-bottom: 0;">
<em><strong>Education</strong></em>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;border-top: .7px solid; border-color: grey" indentlevel="1">
Linear
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.366
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.388
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.392
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP1
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
3.22
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
2.549
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
2.765
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP2
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
0.009
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
0.399
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
0.293
</td>
</tr>
<tr grouplength="3">
<td colspan="7" style="border-bottom: 0;">
<em><strong>Health</strong></em>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;border-top: .7px solid; border-color: grey" indentlevel="1">
Linear
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
7.565
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
6.955
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
7.053
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP1
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-96.067
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-73.206
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-85.631
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP2
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
266.073
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
226.344
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
247.067
</td>
</tr>
<tr grouplength="3">
<td colspan="7" style="border-bottom: 0;">
<em><strong>Crime</strong></em>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;border-top: .7px solid; border-color: grey" indentlevel="1">
Linear
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
2.671
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
3.358
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
3.354
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP1
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-18.915
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-42.444
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-30.45
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP2
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
67.334
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
111.976
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
91.35
</td>
</tr>
<tr grouplength="3">
<td colspan="7" style="border-bottom: 0;">
<em><strong>Barriers</strong></em>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;border-top: .7px solid; border-color: grey" indentlevel="1">
Linear
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.236
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.261
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.244
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP1
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
0.576
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
0.643
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
0.73
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP2
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-0.023
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-0.029
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-0.05
</td>
</tr>
<tr grouplength="3">
<td colspan="7" style="border-bottom: 0;">
<em><strong>Environment</strong></em>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;border-top: .7px solid; border-color: grey" indentlevel="1">
Linear
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.157
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.122
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.135
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP1
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-2.431
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-1.52
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
-2.261
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
FP2
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
2.476
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
1.752
</td>
<td style="text-align:center;">
</td>
<td style="text-align:center;">
2.192
</td>
</tr>
<tr>
<td style="text-align:left;border-top: .7px solid; border-color: grey">
*R*<sup>2</sup>
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.921
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.956
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.928
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.962
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.919
</td>
<td style="text-align:center;border-top: .7px solid; border-color: grey">
0.954
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<span style="font-style: italic;">Note.</span> <sup></sup> Standarized
beta coefficients.
</td>
</tr>
</tfoot>
</table>

Table 2 compares coefficients and model performance for OLS versus MFP
regressions. Across all years, the MFP model provides a substantially
better fit than OLS. For IMD 2015, the *R*<sup>2</sup> value jumps from
0.928 to 0.962, which amounts to a 50% reduction in the influence of the
time-varying income and employment domains from 7.72% to 3.8%. This
meets and exceeds the 5% benchmark set by Abel and colleagues. As
expected, it is the transformation of the health and crime domains which
is the major stimulus of this performance increase. Beta coefficients
for these domains are drastically different for MFP as compared to OLS
models; the 1<sup>*s**t*</sup>-degree FP turns negative (capturing the
downwards slope on the left-hand side of the parabola), while the the
2<sup>*n**d*</sup>-degree FP is positive (capturing the the right-hand
side of the curve). Coefficients are highly consistent across years.
This is significant because it implies that model residuals are
correlated, and that the regression scaling will, *prima facie* ,
produce reliable results.

### The adjustment

I create time-adjusted IMD score by scaling the 2010 and 2019 domain
scores and MFP model residuals by the coefficient values residual SD for
the 2019 MFP model.

2010 adjustment:

``` r
IMD_2010_2011B.df$IMD_tadj <-
  #| 2015 Constant
  (coef_2015.mat[1] +
  #| Plus 2010 domain values scaled by 2015 MFP coefficients
  (coef_2015.mat[2]*IMD_2010_2011B.df$educ_FP1)  + (coef_2015.mat[3]*IMD_2010_2011B.df$educ_FP2) +
  (coef_2015.mat[4]*IMD_2010_2011B.df$health_FP1) + (coef_2015.mat[5]*IMD_2010_2011B.df$health_FP2) +
  (coef_2015.mat[6]*IMD_2010_2011B.df$crime_FP1) + (coef_2015.mat[7]*IMD_2010_2011B.df$crime_FP2) +
  (coef_2015.mat[8]*IMD_2010_2011B.df$barr_FP1) + (coef_2015.mat[9]*IMD_2010_2011B.df$barr_FP2) +
  (coef_2015.mat[10]*IMD_2010_2011B.df$livenv_FP1) + (coef_2015.mat[11]*IMD_2010_2011B.df$livenv_FP2) +
  #| Plus 2010 standardized residual scaled by by 2015 residual SD
  ((IMD_2010_2011B.df$MFPres*MFPresSD2015)/MFPresSD2010))
```

2019 adjustment:

``` r
IMD_2019.df$IMD_tadj <-
  #| 2015 Constant
  (coef_2015.mat[1] +
  #| Plus 2019 values scaled by 2015 MFP coefficients 
  (coef_2015.mat[2]*IMD_2019.df$educ_FP1)  + (coef_2015.mat[3]*IMD_2019.df$educ_FP2) +
  (coef_2015.mat[4]*IMD_2019.df$health_FP1) + (coef_2015.mat[5]*IMD_2019.df$health_FP2) +
  (coef_2015.mat[6]*IMD_2019.df$crime_FP1) + (coef_2015.mat[7]*IMD_2019.df$crime_FP2) +
  (coef_2015.mat[8]*IMD_2019.df$barr_FP1) + (coef_2015.mat[9]*IMD_2019.df$barr_FP2)+
  (coef_2015.mat[10]*IMD_2019.df$livenv_FP1) + (coef_2015.mat[11]*IMD_2019.df$livenv_FP2) +
  #| Plus 2019 standardized residual scaled by 2015 residual SD
  ((IMD_2019.df$MFPres*MFPresSD2015)/MFPresSD2019))
```

I then expand the 2010 and 2015 dataset to include observations for each
year between 2010 and 2019:

``` r
# Expand function (mimics expand function in Stata)
expand <- function(df, ...) {
  as.data.frame(lapply(df, rep, ...))
}
# Expand up to (and including) 2018
IMD_2010_14.df <- expand(IMD_2010_2011B.df, times = 5)
IMD_2015.df$IMD_tadj <- IMD_2015.df$IMD
IMD_2015_18.df <- expand(IMD_2015.df, times = 4)
```

Combine the resulting datasets:

``` r
# Append
IMD_TS.df <- dplyr::bind_rows(IMD_2010_14.df, IMD_2015_18.df) %>%
  dplyr::group_by(LSOA_code) %>%
  dplyr::mutate(obno = row_number()) %>%
  dplyr::ungroup()
# Map year values
IMD_TS.df$year = plyr::mapvalues(IMD_TS.df$obno, from = c(1:9),
                                 to = c(2010:2018))
IMD_TS.df$year <- as.numeric(as.character(IMD_TS.df$year))
```

And generate estimated IMD and time-adjusted IMD scores for missing
years via linear interpolation:

``` r
# Set IMD score as missing for added years
IMD_TS.df <- IMD_TS.df %>%
  dplyr::mutate(IMD = case_when(year == 2010 | year == 2015 ~ as.numeric(IMD),
                                TRUE ~ as.numeric(NA))) %>%
  dplyr::mutate(IMD_tadj = case_when(year == 2010 | year == 2015 ~ as.numeric(IMD_tadj),
                                     TRUE ~ as.numeric(NA)))
# Add 2019 values 
IMD_TS.df <- dplyr::bind_rows(IMD_TS.df, IMD_2019.df) 
# Linear interpolation 
IMD_TS.df <- IMD_TS.df %>%
  dplyr::group_by(LSOA_code) %>%
  dplyr::mutate(IMD_ipol = na.approx(IMD, na.rm=FALSE)) %>%
  dplyr::mutate(IMD_tadj_ipol = na.approx(IMD_tadj, na.rm=FALSE)) %>%
  dplyr::ungroup() %>%
  # Decile rank of interpolated scores (by year)
  dplyr::group_by(year) %>%
  mutate(IMD_dec_ipol= ntile(IMD_ipol, 10)) %>%
  mutate(IMD_tadj_dec_ipol = ntile(IMD_tadj_ipol, 10)) %>%
  arrange(LSOA_code, year) %>%
  dplyr::ungroup()
  head(select(IMD_TS.df, LSOA_code, year, IMD_ipol, IMD_tadj_ipol), 10)
```

    ## # A tibble: 10 x 4
    ##    LSOA_code  year IMD_ipol IMD_tadj_ipol
    ##    <chr>     <dbl>    <dbl>         <dbl>
    ##  1 E01000001  2010     6.16          6.13
    ##  2 E01000001  2011     6.14          6.12
    ##  3 E01000001  2012     6.12          6.10
    ##  4 E01000001  2013     6.10          6.09
    ##  5 E01000001  2014     6.08          6.08
    ##  6 E01000001  2015     6.06          6.06
    ##  7 E01000001  2016     6.10          6.08
    ##  8 E01000001  2017     6.14          6.10
    ##  9 E01000001  2018     6.17          6.12
    ## 10 E01000001  2019     6.21          6.14
