---
title: "TestVersionThesis"
params:
  viridis_palette: viridis
output:
  html_document:
    highlighter: null
    theme: "flatly"
    code_download: TRUE
    toc: true
    toc_float: true
    code_folding: "hide"
    keep_md: true
---





```r
library(haven)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(stargazer)
library(qwraps2)
library(AER)
install.packages("lme4", repo = "https://mac.R-project.org")
```


The downloaded binary packages are in
	/var/folders/1q/z4qm6zl51_91h5bt8xh_b4cm0000gn/T//RtmpU2sMkL/downloaded_packages

```r
library(lme4)
options(qwraps2_markup = "markdown")
```

## R Markdown

#### TABLE 1 - SUMMARY STATISTICS


```r
#  panel A
# for var offer4 yearlong termshown4 termshown6 termshown12 tookup applied loansize: sum X \ sum X if applied==1 \ sum X if tookup==1 \ sum X if ((low==1 | med==1) & onetermshown==1)
# 
# * panel B
# for var female married age edhi rural dependants grossincome trcount dormancy low med high: sum X \ sum X if applied==1 \ sum X if tookup==1 \ sum X if ((low==1 | med==1) & onetermshown==1)



# Einlesen der Stata-Daten
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")

# FunctionTable1 - Übersicht und Kontrolle des Datensatzes
testfunction <- function(stata_data1){
  list("Panel A: Experimental variables" =
         list("Interest rate" = ~ mean_sd(stata_data1$offer4)),
         list("Dynamic repayment incentive" = ~ mean_sd(stata_data1$yearlong)),
         list("Example loan term = 4 months" = ~ mean_sd(stata_data1$termshown4, na_rm = TRUE)),
         list("Example loan term = 6 months" = ~ mean_sd(stata_data1$termshown6, na_rm = TRUE)),
         list("Example loan term = 12 months" = ~ mean_sd(stata_data1$termshown12, na_rm =TRUE)),
         list("Borrowed" = ~ mean_sd(stata_data1$tookup)),
         list("Applied" = ~ mean_sd(stata_data1$applied)),
         list("Loan size" = ~ mean_sd(stata_data1$loansize)),
       "Panel B: Demographic characteristics" =
         list("Female" = ~ mean_sd(stata_data1$female)),
         list("Married" = ~ mean_sd(stata_data1$married, na_rm = TRUE)),
         list("Age" = ~ mean_sd(stata_data1$age)),
         list("More educated" = ~ mean_sd(stata_data1$edhi)),
         list("Rural" = ~ mean_sd(stata_data1$rural)),
         list("Number of dependents" = ~ mean_sd(stata_data1$dependants, na_rm = TRUE)),
         list("Gross monthly income (000s of rand)" = ~ mean_sd(stata_data1$grossincome, na_rm = TRUE)),
         list("Number of loans with the lender" = ~ mean_sd(stata_data1$trcount, na_rm = TRUE))
  )
}


# Spalte1 - All
all_data <- testfunction(stata_data)
summary_all <- summary_table(stata_data, all_data)

# Spalte2 - Applied
applied_data <- stata_data %>% filter(applied == 1)
applied_list <- testfunction(applied_data) 
summary_applied <- summary_table(applied_data, applied_list)

# Spalte3 - Borrowed
borrowed_data <- stata_data %>% filter(tookup == 1)
borrowed_list <- testfunction(borrowed_data) 
summary_borrowed <- summary_table(borrowed_data, borrowed_list)

# Spalte4 - Eligible for maturity suggestion randomizition
maturity_data <- stata_data %>% filter(onetermshown == 1)
maturity_list <- testfunction(maturity_data) 
summary_maturity <- summary_table(maturity_data, maturity_list)

# Zusammenfassen der Listen
summary <- cbind(summary_all, summary_applied, summary_borrowed, summary_maturity)
summary
```



|                                                 |stata_data (N = 53,810)     |applied_data (N = 4,540)   |borrowed_data (N = 3,887)  |maturity_data (N = 3,096) |
|:------------------------------------------------|:---------------------------|:--------------------------|:--------------------------|:-------------------------|
|**Panel A: Experimental variables**              |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Interest rate                       |8.03 &plusmn; 2.47          |7.41 &plusmn; 2.37         |7.35 &plusmn; 2.35         |6.44 &plusmn; 1.72        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Dynamic repayment incentive         |0.43 &plusmn; 0.49          |0.47 &plusmn; 0.50         |0.47 &plusmn; 0.50         |0.44 &plusmn; 0.50        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Example loan term = 4 months        |3,096; 0.51 &plusmn; 0.50   |544; 0.52 &plusmn; 0.50    |506; 0.52 &plusmn; 0.50    |0.51 &plusmn; 0.50        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Example loan term = 6 months        |3,096; 0.25 &plusmn; 0.44   |544; 0.24 &plusmn; 0.43    |506; 0.23 &plusmn; 0.42    |0.25 &plusmn; 0.44        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Example loan term = 12 months       |3,096; 0.24 &plusmn; 0.43   |544; 0.24 &plusmn; 0.43    |506; 0.25 &plusmn; 0.43    |0.24 &plusmn; 0.43        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Borrowed                            |0.07 &plusmn; 0.26          |0.86 &plusmn; 0.35         |1.00 &plusmn; 0.00         |0.16 &plusmn; 0.37        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Applied                             |0.08 &plusmn; 0.28          |1.00 &plusmn; 0.00         |1.00 &plusmn; 0.00         |0.18 &plusmn; 0.38        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Loan size                           |103.35 &plusmn; 506.43      |1,224.96 &plusmn; 1,290.81 |1,430.74 &plusmn; 1,285.18 |269.02 &plusmn; 880.11    |
|**Panel B: Demographic characteristics**         |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Female                              |0.48 &plusmn; 0.50          |0.49 &plusmn; 0.50         |0.49 &plusmn; 0.50         |0.48 &plusmn; 0.50        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Married                             |53,554; 0.44 &plusmn; 0.50  |4,503; 0.45 &plusmn; 0.50  |3,855; 0.46 &plusmn; 0.50  |0.47 &plusmn; 0.50        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Age                                 |41.17 &plusmn; 11.59        |40.82 &plusmn; 11.24       |40.84 &plusmn; 11.26       |42.21 &plusmn; 10.97      |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; More educated                       |0.39 &plusmn; 0.49          |0.41 &plusmn; 0.49         |0.42 &plusmn; 0.49         |0.40 &plusmn; 0.49        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Rural                               |0.16 &plusmn; 0.36          |0.15 &plusmn; 0.36         |0.15 &plusmn; 0.36         |0.19 &plusmn; 0.40        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Number of dependents                |53,554; 1.55 &plusmn; 1.73  |4,503; 1.83 &plusmn; 1.74  |3,855; 1.87 &plusmn; 1.74  |2.22 &plusmn; 1.75        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Gross monthly income (000s of rand) |48,852; 3.41 &plusmn; 20.50 |3,996; 3.37 &plusmn; 2.12  |3,455; 3.41 &plusmn; 2.16  |3.55 &plusmn; 4.71        |
|****                                             |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |&nbsp;&nbsp;              |
|&nbsp;&nbsp; Number of loans with the lender     |53,554; 4.20 &plusmn; 3.85  |4,503; 4.82 &plusmn; 4.23  |3,855; 4.79 &plusmn; 4.23  |5.96 &plusmn; 4.18        |





#### TABLE 2 - EXPERIMENTAL VALIDATION REGRESSIONS


```r
# Table 2 - Experimetal validation Regressions

# en itcscore_100 = itcscore/100
# gen appscore_100 = appscore/100
# 
# reg offer4 dormancy lntrcount female dependants married lnage rural edhi itcscore_100 itczero appscore_100 low med waved2 waved3, cluster(branchuse)
# estimates store m1, title((1))
# sum offer4 if e(sample)
# 
# dprobit tookup_afterdead_enforced offer4 low med waved2 waved3, cluster(branchuse)
# estimates store m2, title((2))
# sum tookup_afterdead if e(sample)
# 
# dprobit reject offer4 low med waved2 waved3 if applied==1, cluster(branchuse)
# estimates store m3, title((3))
# sum reject if e(sample)

# Regression kürzt viele Werte raus, daher NAs = 0?

stata_data$lntrcount[is.na(stata_data$lntrcount)] <- 0
stata_data <- stata_data %>% mutate(itcscore_100 = itcscore/100, appscore_100 = appscore/100)

reg2_1 <- lm(offer4 ~ dormancy + lntrcount + female + dependants + married + lnage + rural + edhi + itcscore_100 + itczero + appscore_100 + low + med + waved2 + waved3, data=stata_data,)

# appscore_100 = -0,065 - falsch grundet?

reg2_2 <- lm(tookup_afterdead_enforced ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=stata_data)

stata_data_reg3 <- stata_data %>% filter(applied == 1)
reg2_3 <- lm(rejected ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, applied == 1))

stargazer(reg2_1, reg2_2, reg2_3, type="html", header=FALSE)
```


<table style="text-align:center"><tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>offer4</td><td>tookup_afterdead_enforced</td><td>rejected</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">dormancy</td><td>0.001</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.002)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">lntrcount</td><td>0.004</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.013)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">female</td><td>0.024</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.022)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">dependants</td><td>0.0002</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.007)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">married</td><td>0.017</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.023)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">lnage</td><td>-0.002</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.048)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">rural</td><td>0.020</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.029)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">edhi</td><td>-0.013</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.022)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">itcscore_100</td><td>0.005</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.014)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">itczero</td><td>0.035</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.097)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscore_100</td><td>-0.065</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.135)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">offer4</td><td></td><td>-0.0001</td><td>0.002</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.001)</td><td>(0.002)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">low</td><td>-2.486<sup>***</sup></td><td>0.193<sup>***</sup></td><td>-0.120<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.039)</td><td>(0.005)</td><td>(0.014)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">med</td><td>-1.075<sup>***</sup></td><td>0.145<sup>***</sup></td><td>-0.062<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.041)</td><td>(0.005)</td><td>(0.014)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved2</td><td>-0.291<sup>***</sup></td><td>-0.076<sup>***</sup></td><td>-0.039<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.039)</td><td>(0.006)</td><td>(0.017)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved3</td><td>-0.297<sup>***</sup></td><td>-0.072<sup>***</sup></td><td>-0.129<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.038)</td><td>(0.005)</td><td>(0.017)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>8.651<sup>***</sup></td><td>0.179<sup>***</sup></td><td>0.247<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.171)</td><td>(0.008)</td><td>(0.025)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>53,554</td><td>53,810</td><td>4,540</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.112</td><td>0.047</td><td>0.038</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.112</td><td>0.046</td><td>0.037</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>2.327 (df = 53538)</td><td>0.346 (df = 53804)</td><td>0.344 (df = 4534)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>450.744<sup>***</sup> (df = 15; 53538)</td><td>525.782<sup>***</sup> (df = 5; 53804)</td><td>35.664<sup>***</sup> (df = 5; 4534)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


#### TABLE 3 - THE EXTENSIVE MARGIN: PRICE SENSITIVES OF LOAN TAKE-UP



```r
# dprobit applied offer4 low med waved2 waved3 if (normrate_less==1), cluster(branchuse)
# estimates store m1, title((1))
# sum offer4 applied if e(sample)
# 
# dprobit applied normrate_more low med waved2 waved3, cluster(branchuse)
# estimates store m2, title((2))
# 
# dprobit applied offer4 low med waved2 waved3 if (normrate_less==0), cluster(branchuse)
# estimates store m3, title((3))
# sum offer4 if e(sample)
# 
# dprobit tookup_outside_only offer4 low med waved2 waved3 if (normrate_less==1), cluster(branchuse)
# estimates store m4, title((4))
# 
# dprobit tookup_outside_only normrate_more low med waved2 waved3, cluster(branchuse)
# estimates store m5, title((5))
# 
# dprobit tookup_outside_only offer4 low med waved2 waved3 if (normrate_less==0), cluster(branchuse)
# estimates store m6, title((6))
# 
# dprobit tookup_after offer4 low med waved2 waved3 if (normrate_less==1), cluster(branchuse)
# estimates store m7, title((7))
# 
# dprobit tookup_after normrate_more low med waved2 waved3, cluster(branchuse)
# estimates store m8, title((8))
# 
# dprobit tookup_after offer4 low med waved2 waved3 if (normrate_less==0), cluster(branchuse)
# estimates store m9, title((9))

reg3_1 <- lm(applied ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less == 1), family = binomial(link = "probit"))

reg3_2 <- lm(applied ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data=stata_data)

reg3_3 <- lm(applied ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 0))

reg3_4 <- lm(tookup_outside_only ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 1))

reg3_5 <- lm(tookup_outside_only ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data = stata_data)

reg3_6 <- lm(tookup_outside_only ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 0))

reg3_7 <- lm(tookup_afterdead_enforced ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 1))

reg3_8 <- lm(tookup_afterdead_enforced ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data = stata_data)

reg3_9 <- lm(tookup_afterdead_enforced ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 0))

stargazer(reg3_1, reg3_2, reg3_3, reg3_4, reg3_5, reg3_6, reg3_7, reg3_8, reg3_9, type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="9"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="9" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="3">applied</td><td colspan="3">tookup_outside_only</td><td colspan="3">tookup_afterdead_enforced</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td><td>(7)</td><td>(8)</td><td>(9)</td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-0.003<sup>***</sup></td><td></td><td>-0.019<sup>**</sup></td><td>0.001</td><td></td><td>-0.009</td><td>0.0005</td><td></td><td>-0.013</td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td></td><td>(0.010)</td><td>(0.001)</td><td></td><td>(0.018)</td><td>(0.001)</td><td></td><td>(0.015)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">normrate_more</td><td></td><td>-0.038<sup>***</sup></td><td></td><td></td><td>0.006</td><td></td><td></td><td>-0.053<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.012)</td><td></td><td></td><td>(0.018)</td><td></td><td></td><td>(0.015)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">low</td><td>0.111<sup>***</sup></td><td>0.117<sup>***</sup></td><td>0.032</td><td>0.028<sup>***</sup></td><td>0.025<sup>***</sup></td><td>0.009</td><td>0.193<sup>***</sup></td><td>0.193<sup>***</sup></td><td>0.182<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.004)</td><td>(0.004)</td><td>(0.043)</td><td>(0.006)</td><td>(0.006)</td><td>(0.079)</td><td>(0.005)</td><td>(0.005)</td><td>(0.066)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">med</td><td>0.112<sup>***</sup></td><td>0.114<sup>***</sup></td><td>0.028</td><td>-0.005</td><td>-0.005</td><td>0.068</td><td>0.145<sup>***</sup></td><td>0.144<sup>***</sup></td><td>0.142<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.004)</td><td>(0.004)</td><td>(0.035)</td><td>(0.006)</td><td>(0.006)</td><td>(0.063)</td><td>(0.005)</td><td>(0.005)</td><td>(0.053)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved2</td><td>-0.009<sup>**</sup></td><td>-0.010<sup>**</sup></td><td></td><td>-0.058<sup>***</sup></td><td>-0.057<sup>***</sup></td><td></td><td>-0.083<sup>***</sup></td><td>-0.083<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.005)</td><td>(0.005)</td><td></td><td>(0.007)</td><td>(0.007)</td><td></td><td>(0.006)</td><td>(0.006)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved3</td><td>-0.014<sup>***</sup></td><td>-0.015<sup>***</sup></td><td></td><td>-0.052<sup>***</sup></td><td>-0.052<sup>***</sup></td><td></td><td>-0.079<sup>***</sup></td><td>-0.079<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.005)</td><td>(0.005)</td><td></td><td>(0.007)</td><td>(0.007)</td><td></td><td>(0.006)</td><td>(0.006)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.093<sup>***</sup></td><td>0.072<sup>***</sup></td><td>0.305<sup>**</sup></td><td>0.262<sup>***</sup></td><td>0.270<sup>***</sup></td><td>0.390</td><td>0.181<sup>***</sup></td><td>0.184<sup>***</sup></td><td>0.296</td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td>(0.004)</td><td>(0.133)</td><td>(0.009)</td><td>(0.007)</td><td>(0.244)</td><td>(0.008)</td><td>(0.005)</td><td>(0.203)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>53,178</td><td>53,810</td><td>632</td><td>53,178</td><td>53,810</td><td>632</td><td>53,178</td><td>53,810</td><td>632</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.030</td><td>0.030</td><td>0.030</td><td>0.002</td><td>0.002</td><td>0.004</td><td>0.047</td><td>0.047</td><td>0.058</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.030</td><td>0.030</td><td>0.026</td><td>0.002</td><td>0.002</td><td>-0.001</td><td>0.047</td><td>0.047</td><td>0.053</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.274 (df = 53172)</td><td>0.274 (df = 53804)</td><td>0.246 (df = 628)</td><td>0.416 (df = 53172)</td><td>0.416 (df = 53804)</td><td>0.450 (df = 628)</td><td>0.346 (df = 53172)</td><td>0.346 (df = 53804)</td><td>0.374 (df = 628)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>331.073<sup>***</sup> (df = 5; 53172)</td><td>328.817<sup>***</sup> (df = 5; 53804)</td><td>6.535<sup>***</sup> (df = 3; 628)</td><td>19.520<sup>***</sup> (df = 5; 53172)</td><td>21.498<sup>***</sup> (df = 5; 53804)</td><td>0.820 (df = 3; 628)</td><td>519.689<sup>***</sup> (df = 5; 53172)</td><td>528.439<sup>***</sup> (df = 5; 53804)</td><td>12.871<sup>***</sup> (df = 3; 628)</td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="9" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

#### TABLE 4 - PRICE SENSITIVITIES OF LOAN SIZE


```r
# *UNCONDITIONAL
# regress loansize offer4 low med waved2 waved3 if (normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# estimates store m1, title((1))
# sum loansize offer4 if e(sample)
# 
# * with controls
# xi: regress loansize offer4 low med waved2 waved3 grossincome grossincomesq dormancy trcount female dependants married age agesq rural edhi i.province i.branchuse appscore appscoresq appscore0 itcscore itcscoresq itczero if (normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m2, title((2))
# 
# *CONDITIONAL
# regress loansize offer4 low med waved2 waved3 if (tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m3, title((3))
# 
# * add controls
# xi: regress loansize offer4 low med waved2 waved3 sales_netincome sales_netincomesq sales_grossincome sales_grossincomesq appscore appscoresq appscore0 itcscore itcscoresq itczero dormancy trcount female dependants married age agesq rural edhi i.province i.branchuse if (tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m4, title((4))
# 
# * tobit
# xi: tobit loansize offer4 low med waved2 waved3 sales_netincome sales_netincomesq sales_grossincome sales_grossincomesq appscore appscoresq appscore0 itcscore itcscoresq itczero dormancy trcount female dependants married age agesq rural edhi i.province if (tookup==1 & normrate_less== 1 & (offer4==final4)), ul ll
# estimates store m5, title((5))
# sum loansize offer4 if e(sample)
# 
# *log
# regress lnloansize lnoffer4 low med waved2 waved3 if (tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# estimates store m6, title((6))
# 
# * add controls
# xi: regress lnloansize lnoffer4 low med waved2 waved3 sales_netincome sales_netincomesq sales_grossincome sales_grossincomesq appscore appscoresq appscore0 itcscore itcscoresq itczero dormancy trcount female dependants married age agesq rural edhi i.province i.branchuse if (tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# estimates store m7, title((7))
# 
# xi: tobit lnloansize lnoffer4 low med waved2 waved3 sales_netincome sales_netincomesq sales_grossincome sales_grossincomesq appscore appscoresq appscore0 itcscore itcscoresq itczero dormancy trcount female dependants married age agesq rural edhi i.province if (tookup==1 & normrate_less== 1 & (offer4==final4)), ul ll
# estimates store m8, title((8))

stata_data <- stata_data %>% mutate(grossincomesq = grossincome^2, agesq = age^2, appscoresq = appscore^2, itcscoresq = itcscore^2)

reg4_1 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1))

reg4_2 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + appscore0 + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1))

reg4_3 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_4 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_5 <- tobit(loansize ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_6 <- lm(lnloansize ~ lnoffer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1))

reg4_7 <- lm(lnloansize ~ lnoffer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1, grossincome!=0))

reg4_8 <- tobit(lnloansize ~ lnoffer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1, grossincome!=0))


stargazer(reg4_1, reg4_2,type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">loansize</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-4.368<sup>***</sup></td><td>-4.394<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.229)</td><td>(1.273)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">low</td><td>247.238<sup>***</sup></td><td>207.747<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(9.299)</td><td>(11.862)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">med</td><td>171.509<sup>***</sup></td><td>120.032<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(10.110)</td><td>(12.368)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved2</td><td>16.442</td><td>48.842</td></tr>
<tr><td style="text-align:left"></td><td>(10.526)</td><td>(83.299)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved3</td><td>29.524<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(10.106)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">grossincome</td><td></td><td>10.649<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(1.823)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">grossincomesq</td><td></td><td>-0.101<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.030)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">dormancy</td><td></td><td>-5.146<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.602)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">trcount</td><td></td><td>1.493</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.954)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">female</td><td></td><td>1.792</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(6.738)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">dependants</td><td></td><td>-3.859<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(2.087)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">married</td><td></td><td>12.176<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(6.870)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">age</td><td></td><td>5.977<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(1.890)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">agesq</td><td></td><td>-0.071<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.020)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">rural</td><td></td><td>224.128</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.048)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">edhi</td><td></td><td>28.651<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(7.004)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscore</td><td></td><td>-0.200</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(1.982)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscoresq</td><td></td><td>0.018</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.032)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscore0</td><td></td><td>-76.879</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(252.135)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">itcscore</td><td></td><td>0.242</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.551)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">itcscoresq</td><td></td><td>-0.0003</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.0004)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">itczero</td><td></td><td>45.944</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(170.490)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceFree State</td><td></td><td>-124.674</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(506.852)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceGauteng</td><td></td><td>19.473</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(30.237)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceKwazulu-Natal</td><td></td><td>-258.388</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(363.803)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceLimpopo Province</td><td></td><td>-340.390</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(506.866)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceMpumalanga</td><td></td><td>-135.813</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.088)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceNorth West</td><td></td><td>-131.135</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.174)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceWestern Cape</td><td></td><td>30.067</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(34.358)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAD</td><td></td><td>-67.903<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(38.537)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAE</td><td></td><td>161.359</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.026)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAI</td><td></td><td>66.148</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(49.508)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAV</td><td></td><td>-30.892</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(41.641)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBE</td><td></td><td>-131.952</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(613.548)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBF</td><td></td><td>-8.748</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(28.300)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBG</td><td></td><td>-160.180</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(612.006)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBH</td><td></td><td>-67.787</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.477)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBK</td><td></td><td>1.412</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(30.430)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBM</td><td></td><td>414.819</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(550.984)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBS</td><td></td><td>-276.999</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(355.597)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBV</td><td></td><td>-31.257</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(34.649)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBY</td><td></td><td>10.829</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(24.739)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCK</td><td></td><td>-10.066</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(58.642)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCM</td><td></td><td>-31.731</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(62.602)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCP</td><td></td><td>-337.530</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.987)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCS</td><td></td><td>622.496</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(390.353)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCT</td><td></td><td>-4.514</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(33.470)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCV</td><td></td><td>-192.665</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.374)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCW</td><td></td><td>211.437</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.927)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDM</td><td></td><td>274.821</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.394)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDP</td><td></td><td>248.671</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.834)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDS</td><td></td><td>211.562</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.832)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDU</td><td></td><td>202.096</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.833)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCEL</td><td></td><td>-10.257</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(35.301)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCEM</td><td></td><td>200.770</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.540)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCER</td><td></td><td>-92.059</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(501.192)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGA</td><td></td><td>696.266<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.988)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGD</td><td></td><td>-396.805</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(711.676)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGK</td><td></td><td>135.776</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(363.422)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGM</td><td></td><td>4.617</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(23.382)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGO</td><td></td><td>-291.285</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.820)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGP</td><td></td><td>-21.656</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(26.462)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGR</td><td></td><td>-19.223</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(25.062)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGS</td><td></td><td>148.881</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(178.428)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGT</td><td></td><td>9,401.419<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(612.638)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGY</td><td></td><td>333.742</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(507.410)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCHL</td><td></td><td>-180.763</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(409.096)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCHV</td><td></td><td>-395.969</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(706.764)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCIM</td><td></td><td>210.527</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.802)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJA</td><td></td><td>-5.063</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(29.002)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJB</td><td></td><td>-13.072</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(24.645)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJC</td><td></td><td>-39.340</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(30.916)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJG</td><td></td><td>1.360</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(22.940)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJJ</td><td></td><td>305.965</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(364.106)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJM</td><td></td><td>6.208</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(21.166)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJP</td><td></td><td>-7.159</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(151.292)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJR</td><td></td><td>-35.952</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(28.021)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJW</td><td></td><td>-26.151</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(38.520)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKD</td><td></td><td>-73.857</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.392)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKM</td><td></td><td>-46.842<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(27.840)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKP</td><td></td><td>-35.018</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(23.256)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKR</td><td></td><td>-174.879</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(613.448)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKS</td><td></td><td>51.905</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(48.721)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKW</td><td></td><td>-242.814</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(356.173)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKY</td><td></td><td>93.615</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(500.110)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCLM</td><td></td><td>-51.042</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(34.361)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCLT</td><td></td><td>92.169</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(618.520)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCLY</td><td></td><td>-147.013</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(706.648)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMA</td><td></td><td>109.656</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(167.148)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMB</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMD</td><td></td><td>-2.179</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(27.461)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMG</td><td></td><td>163.984</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.997)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMI</td><td></td><td>41.124</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(152.591)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMK</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMP</td><td></td><td>45.461</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(32.178)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMT</td><td></td><td>39.901</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(38.525)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMV</td><td></td><td>-171.288</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(288.671)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMZ</td><td></td><td>-23.761</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.937)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNG</td><td></td><td>18.149</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(41.457)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNL</td><td></td><td>3,746.531<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(794.183)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNM</td><td></td><td>211.816</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.671)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNS</td><td></td><td>316.402</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(711.032)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPA</td><td></td><td>-13.347</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(36.486)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPB</td><td></td><td>332.018</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(506.566)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPC</td><td></td><td>226.821</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(455.828)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPE</td><td></td><td>39.898</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(33.360)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPF</td><td></td><td>35.204</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(710.970)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPG</td><td></td><td>54.462</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(45.001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPL</td><td></td><td>357.998</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(394.909)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPN</td><td></td><td>211.695</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.500)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPO</td><td></td><td>557.019</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(564.393)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPS</td><td></td><td>-79.626<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(37.238)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPT</td><td></td><td>16.900</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(36.360)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPW</td><td></td><td>74.694</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(618.985)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCQT</td><td></td><td>-46.360</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(357.202)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRB</td><td></td><td>-52.986<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(31.833)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRL</td><td></td><td>-28.067</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(250.134)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRM</td><td></td><td>270.326</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.953)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRP</td><td></td><td>13.673</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(159.498)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSD</td><td></td><td>114.359</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.616)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSM</td><td></td><td>224.636</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.105)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSP</td><td></td><td>101.492<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(30.031)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSW</td><td></td><td>18.565</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.758)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCTZ</td><td></td><td>99.986</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(618.404)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCUL</td><td></td><td>-151.009</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(168.364)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCUP</td><td></td><td>99.238</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(502.203)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCUT</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVD</td><td></td><td>-233.642</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(612.479)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVP</td><td></td><td>-305.184</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.540)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVR</td><td></td><td>-126.133</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(355.807)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVS</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCWB</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCWK</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCWY</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCZE</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>71.625<sup>***</sup></td><td>-78.715</td></tr>
<tr><td style="text-align:left"></td><td>(13.424)</td><td>(178.235)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>31,231</td><td>28,197</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.033</td><td>0.062</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.033</td><td>0.058</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>506.350 (df = 31225)</td><td>499.170 (df = 28070)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>212.779<sup>***</sup> (df = 5; 31225)</td><td>14.677<sup>***</sup> (df = 126; 28070)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

##### TABLE 5 - GROSS REVENUE AND REPAYMENT SENSITIVITIES TO INTEREST RATES


```r
# **REVENUES TABLE, COLUMN 1
# regress grossinterest offer4 low med waved2 waved3 if ((normrate_less==1) & (offer4==final4)), cluster(branchuse)
# 
# **REVENUES TABLE, COLUMN 2
# regress pstdue_average offer4 low med waved2 waved3 if ((normrate_less==1) & (offer4==final4) & tookup==1), cluster(branchuse)
# 
# **REVENUES TABLE, COLUMN 3
# tobit pstdue_average offer4 low med waved2 waved3 if ((normrate_less==1) & (offer4==final4) & tookup==1), ll(0)


reg5_1 <- lm(grossinterest ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1))

reg5_2 <- lm(pstdue_average ~ offer4,data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg5_3 <- tobit(pstdue_average ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))


stargazer(reg5_1, reg5_2, reg5_3, type="html",align=TRUE, dep.var.labels=c("Gross interest revenue","Average past due", "Average past due"), covariate.labels=c("interest rate in pp terms (e.g., 8.2)"),no.space=TRUE)
```


<table style="text-align:center"><tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>Gross interest revenue</td><td colspan="2">Average past due</td></tr>
<tr><td style="text-align:left"></td><td><em>OLS</em></td><td><em>OLS</em></td><td><em>Tobit</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">interest rate in pp terms (e.g., 8.2)</td><td>-2.374<sup>***</sup></td><td>21.880<sup>***</sup></td><td>40.048<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.607)</td><td>(3.234)</td><td>(5.514)</td></tr>
<tr><td style="text-align:left">Constant</td><td>56.286<sup>***</sup></td><td>4.062</td><td>-355.864<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(4.985)</td><td>(24.493)</td><td>(43.080)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>31,231</td><td>2,325</td><td>2,325</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.0005</td><td>0.019</td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.0005</td><td>0.019</td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td></td><td>-10,250.390</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>266.777 (df = 31229)</td><td>370.893 (df = 2323)</td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>15.307<sup>***</sup> (df = 1; 31229)</td><td>45.771<sup>***</sup> (df = 1; 2323)</td><td></td></tr>
<tr><td style="text-align:left">Wald Test</td><td></td><td></td><td>52.752<sup>***</sup> (df = 1)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

#### TABLE 6 - PRICE SENSITIVITY OF LOAN SIZE FOR GROUPS ASSUMED MOST LIKELY TO READ THE SOLICIATION


```r
# *UNCONDITIONAL loan size
# * with controls
# xi: regress loansize offer4 low med waved2 waved3 grossincome grossincomesq dormancy trcount female dependants married age agesq rural edhi i.province i.branchuse appscore appscoresq appscore0 itcscore itcscoresq itczero if (normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m1, title((1))
# 
# xi: regress loansize offer4 low med waved2 waved3 grossincome grossincomesq dormancy trcount female dependants married age agesq rural i.province i.branchuse appscore appscoresq appscore0 itcscore itcscoresq itczero if (normrate_less== 1 & edhi==1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m2, title((2))
# 
# xi: regress loansize offer4 low med waved2 waved3 grossincome grossincomesq trcount female dependants married age agesq rural edhi i.province i.branchuse appscore appscoresq appscore0 itcscore itcscoresq itczero if (normrate_less== 1 & (offer4==final4) & dormancy<10), cluster(branchuse)
# estimates store m3, title((3))
# sum loansize offer4 if e(sample)
# 
# xi: regress loansize offer4 low med waved2 waved3 grossincome grossincomesq dormancy female dependants married age agesq rural edhi i.province i.branchuse appscore appscoresq appscore0 itcscore itcscoresq itczero if (normrate_less== 1 & (offer4==final4) & trcount>2), cluster(branchuse)
# estimates store m4, title((4))
# sum loansize offer4 if e(sample)

# grossincome -> additional controls
reg6_1 <- lm(loansize ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1, grossincome!=0))

# edhi=1 -> High education
reg6_2 <- lm(loansize ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1, grossincome!=0, edhi==1))

# dormancy<=9 -> Borrowed in last 9 months
reg6_3 <- lm(loansize ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1, grossincome!=0, dormancy<=9))

# trcount>2 -> #Past loans more than 2
reg6_4 <- lm(loansize ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1, grossincome!=0, trcount>2))


stargazer(reg6_1, reg6_2, reg6_3, reg6_4, type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="4">loansize</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-15.857<sup>***</sup></td><td>-21.034<sup>***</sup></td><td>-23.274<sup>***</sup></td><td>-21.697<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.232)</td><td>(2.412)</td><td>(2.500)</td><td>(2.050)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>229.812<sup>***</sup></td><td>303.509<sup>***</sup></td><td>349.405<sup>***</sup></td><td>295.054<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(10.230)</td><td>(20.070)</td><td>(19.702)</td><td>(16.604)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>28,197</td><td>11,275</td><td>13,201</td><td>14,806</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.006</td><td>0.007</td><td>0.007</td><td>0.008</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.006</td><td>0.007</td><td>0.006</td><td>0.007</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>512.703 (df = 28195)</td><td>631.689 (df = 11273)</td><td>686.115 (df = 13199)</td><td>612.113 (df = 14804)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>165.751<sup>***</sup> (df = 1; 28195)</td><td>76.040<sup>***</sup> (df = 1; 11273)</td><td>86.634<sup>***</sup> (df = 1; 13199)</td><td>112.015<sup>***</sup> (df = 1; 14804)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


#### TABLE 7 - PRICE SENSITIVITIES FOR FEMALE AND LOW-INCOME CLIENTS


```r
# * EXTENSIVE
# dprobit applied offer4 low med waved2 waved3 if (female==1 & normrate_less==1), cluster(branchuse)
# estimates store m1, title((1))
# sum offer4 applied if e(sample)
# 
# dprobit applied offer4 low med waved2 waved3 if (grossincomecat2==1 & normrate_less==1), cluster(branchuse)
# estimates store m2, title((2))
# sum offer4 applied if e(sample)
# 
# dprobit applied offer4 low med waved2 waved3 if (grossincomecat2==1 & female==1 & normrate_less==1), cluster(branchuse)
# estimates store m3, title((3))
# sum offer4 applied if e(sample)
# 
# *UNCONDITIONAL LOAN SIZE
# regress loansize offer4 low med waved2 waved3 if (female==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m4, title((4))
# 
# regress loansize offer4 low med waved2 waved3 if (grossincomecat2==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m5, title((5))
# 
# regress loansize offer4 low med waved2 waved3 if (grossincomecat2==1 & female==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m6, title((6))
# 
# *CONDITIONAL LOAN SIZE
# regress loansize offer4 low med waved2 waved3 if (female==1 & tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m7, title((7))
# 
# regress loansize offer4 low med waved2 waved3 if (sales_grossincomecat2==1 & tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m8, title((8))
# 
# regress loansize offer4 low med waved2 waved3 if (sales_grossincomecat2==1 & female==1 & tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m9, title((9))
```


#### TABLE 8A - MATURITY ELASTICITY FIRST-STAGE: THE POWER OF PURE SUGGESTION


```r
# * 1ST-STAGE: Maturity chosen on maturity suggested in mailer
# ** linear IV
# xi: regress term offer4 final4 yearlong termshown low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1), cluster(branchuse)
# estimates store m1, title((1))
# 
# xi: regress term offer4 final4 yearlong termshown low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1 & sales_grossincomecat2==1), cluster(branchuse)
# estimates store m2, title((2))
# 
# xi: regress term offer4 final4 yearlong termshown low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1 & sales_grossincomecat2==2), cluster(branchuse)
# estimates store m3, title((3))
# 
# ** categorical IV
# xi: regress term offer4 final4 yearlong termshown6 termshown12 low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1), cluster(branchuse)
# estimates store m4, title((4))
# 
# xi: regress term offer4 final4 yearlong termshown6 termshown12 low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1 & sales_grossincomecat2==1), cluster(branchuse)
# estimates store m5, title((5))
# 
# xi: regress term offer4 final4 yearlong termshown6 termshown12 low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1 & sales_grossincomecat2==2), cluster(branchuse)
# estimates store m6, title((6))
```


#### TABLE 8B - MATURITY ELASTICITIES OF LOAN DEMAND: OLS AND IV ESTIMATES


```r
# * OLS
# xi: regress lnloansize term offer4 final4 yearlong low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1), cluster(branchuse)
# estimates store m1, title((1))
# 
# xi: regress lnloansize term offer4 final4 yearlong low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1 & sales_grossincomecat2==1), cluster(branchuse)
# estimates store m2, title((2))
# 
# xi: regress lnloansize term offer4 final4 yearlong low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1 & sales_grossincomecat2==2), cluster(branchuse)
# estimates store m3, title((3))
# 
# xi: ivreg lnloansize offer4 final4 yearlong (term= termshown6 termshown12) low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1), first cluster(branchuse)
# estimates store m4, title((4))
# sum loansize offer4 if e(sample)
# 
# xi: ivreg lnloansize offer4 final4 yearlong (term= termshown6 termshown12) low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1 & sales_grossincomecat2==1), first cluster(branchuse)
# estimates store m5, title((5))
# sum loansize offer4 if e(sample)
# 
# xi: ivreg lnloansize offer4 final4 yearlong (term= termshown6 termshown12) low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1 & sales_grossincomecat2==2), first cluster(branchuse)
# estimates store m6, title((6))
# sum loansize offer4 if e(sample)
```

