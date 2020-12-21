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
library(VGAM)
library(censReg)
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

reg3_1 <- lm(applied ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less == 1), family = binomial(link = "probit"))

# dprobit applied normrate_more low med waved2 waved3, cluster(branchuse)
# estimates store m2, title((2))

reg3_2 <- lm(applied ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data=stata_data)

# dprobit applied offer4 low med waved2 waved3 if (normrate_less==0), cluster(branchuse)
# estimates store m3, title((3))
# sum offer4 if e(sample)

reg3_3 <- glm(applied ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 0))

# dprobit tookup_outside_only offer4 low med waved2 waved3 if (normrate_less==1), cluster(branchuse)
# estimates store m4, title((4))

reg3_4 <- glm(tookup_outside_only ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 1))

# dprobit tookup_outside_only normrate_more low med waved2 waved3, cluster(branchuse)
# estimates store m5, title((5))

reg3_5 <- glm(tookup_outside_only ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data = stata_data)

# dprobit tookup_outside_only offer4 low med waved2 waved3 if (normrate_less==0), cluster(branchuse)
# estimates store m6, title((6))

reg3_6 <- glm(tookup_outside_only ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 0))

# dprobit tookup_after offer4 low med waved2 waved3 if (normrate_less==1), cluster(branchuse)
# estimates store m7, title((7))

reg3_7 <- glm(tookup_afterdead_enforced ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 1))

# dprobit tookup_after normrate_more low med waved2 waved3, cluster(branchuse)
# estimates store m8, title((8))

reg3_8 <- glm(tookup_afterdead_enforced ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data = stata_data)

# dprobit tookup_after offer4 low med waved2 waved3 if (normrate_less==0), cluster(branchuse)
# estimates store m9, title((9))

reg3_9 <- glm(tookup_afterdead_enforced ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 0))

stargazer(reg3_1, reg3_2, reg3_3, reg3_4, reg3_5, reg3_6, reg3_7, reg3_8, reg3_9, type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="9"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="9" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="3">applied</td><td colspan="3">tookup_outside_only</td><td colspan="3">tookup_afterdead_enforced</td></tr>
<tr><td style="text-align:left"></td><td colspan="2"><em>OLS</em></td><td><em>probit</em></td><td colspan="3"><em>probit</em></td><td colspan="3"><em>probit</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td><td>(7)</td><td>(8)</td><td>(9)</td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-0.003<sup>***</sup></td><td></td><td>-0.149<sup>*</sup></td><td>0.004</td><td></td><td>-0.028</td><td>0.002</td><td></td><td>-0.049</td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td></td><td>(0.077)</td><td>(0.003)</td><td></td><td>(0.053)</td><td>(0.003)</td><td></td><td>(0.058)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">normrate_more</td><td></td><td>-0.038<sup>***</sup></td><td></td><td></td><td>0.018</td><td></td><td></td><td>-0.183<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.012)</td><td></td><td></td><td>(0.057)</td><td></td><td></td><td>(0.063)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">low</td><td>0.111<sup>***</sup></td><td>0.117<sup>***</sup></td><td>0.136</td><td>0.091<sup>***</sup></td><td>0.083<sup>***</sup></td><td>0.022</td><td>0.710<sup>***</sup></td><td>0.706<sup>***</sup></td><td>0.597<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.004)</td><td>(0.004)</td><td>(0.325)</td><td>(0.020)</td><td>(0.018)</td><td>(0.234)</td><td>(0.020)</td><td>(0.018)</td><td>(0.250)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">med</td><td>0.112<sup>***</sup></td><td>0.114<sup>***</sup></td><td>0.186</td><td>-0.017</td><td>-0.017</td><td>0.195</td><td>0.564<sup>***</sup></td><td>0.562<sup>***</sup></td><td>0.518<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.004)</td><td>(0.004)</td><td>(0.264)</td><td>(0.022)</td><td>(0.021)</td><td>(0.184)</td><td>(0.021)</td><td>(0.021)</td><td>(0.195)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved2</td><td>-0.009<sup>**</sup></td><td>-0.010<sup>**</sup></td><td></td><td>-0.184<sup>***</sup></td><td>-0.182<sup>***</sup></td><td></td><td>-0.311<sup>***</sup></td><td>-0.310<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.005)</td><td>(0.005)</td><td></td><td>(0.023)</td><td>(0.023)</td><td></td><td>(0.024)</td><td>(0.024)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved3</td><td>-0.014<sup>***</sup></td><td>-0.015<sup>***</sup></td><td></td><td>-0.165<sup>***</sup></td><td>-0.164<sup>***</sup></td><td></td><td>-0.293<sup>***</sup></td><td>-0.292<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.005)</td><td>(0.005)</td><td></td><td>(0.022)</td><td>(0.022)</td><td></td><td>(0.023)</td><td>(0.023)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.093<sup>***</sup></td><td>0.072<sup>***</sup></td><td>0.287</td><td>-0.641<sup>***</sup></td><td>-0.614<sup>***</sup></td><td>-0.244</td><td>-0.968<sup>***</sup></td><td>-0.953<sup>***</sup></td><td>-0.488</td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td>(0.004)</td><td>(1.027)</td><td>(0.030)</td><td>(0.021)</td><td>(0.719)</td><td>(0.033)</td><td>(0.022)</td><td>(0.788)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>53,178</td><td>53,810</td><td>632</td><td>53,178</td><td>53,810</td><td>632</td><td>53,178</td><td>53,810</td><td>632</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.030</td><td>0.030</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.030</td><td>0.030</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td></td><td>-145.896</td><td>-28,187.630</td><td>-28,563.230</td><td>-373.569</td><td>-21,101.540</td><td>-21,383.760</td><td>-281.566</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td></td><td></td><td>299.792</td><td>56,387.250</td><td>57,138.450</td><td>755.138</td><td>42,215.080</td><td>42,779.520</td><td>571.133</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.274 (df = 53172)</td><td>0.274 (df = 53804)</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>331.073<sup>***</sup> (df = 5; 53172)</td><td>328.817<sup>***</sup> (df = 5; 53804)</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="9" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

#### TABLE 4 - PRICE SENSITIVITIES OF LOAN SIZE


```r
stata_data <- stata_data %>% mutate(grossincomesq = grossincome^2, agesq = age^2, appscoresq = appscore^2, itcscoresq = itcscore^2, sales_netincomesq = sales_netincome^2, sales_grossincomesq = sales_grossincome^2)

# *UNCONDITIONAL
# regress loansize offer4 low med waved2 waved3 if (normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# estimates store m1, title((1))
# sum loansize offer4 if e(sample)

reg4_1 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1))

# * with controls
# xi: regress loansize offer4 low med waved2 waved3 grossincome grossincomesq dormancy trcount female dependants married age agesq rural edhi i.province i.branchuse appscore appscoresq appscore0 itcscore itcscoresq itczero if (normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m2, title((2))

reg4_2 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1))

# *CONDITIONAL
# regress loansize offer4 low med waved2 waved3 if (tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m3, title((3))

reg4_3 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

# * add controls
# xi: regress loansize offer4 low med waved2 waved3 sales_netincome sales_netincomesq sales_grossincome sales_grossincomesq appscore appscoresq appscore0 itcscore itcscoresq itczero dormancy trcount female dependants married age agesq rural edhi i.province i.branchuse if (tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m4, title((4))

reg4_4 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

# * tobit
# xi: tobit loansize offer4 low med waved2 waved3 sales_netincome sales_netincomesq sales_grossincome sales_grossincomesq appscore appscoresq appscore0 itcscore itcscoresq itczero dormancy trcount female dependants married age agesq rural edhi i.province if (tookup==1 & normrate_less== 1 & (offer4==final4)), ul ll
# estimates store m5, title((5))
# sum loansize offer4 if e(sample)

reg4_5 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))


# *log
# regress lnloansize lnoffer4 low med waved2 waved3 if (tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# estimates store m6, title((6))

reg4_6 <- lm(lnloansize ~ lnoffer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1))

# * add controls
# xi: regress lnloansize lnoffer4 low med waved2 waved3 sales_netincome sales_netincomesq sales_grossincome sales_grossincomesq appscore appscoresq appscore0 itcscore itcscoresq itczero dormancy trcount female dependants married age agesq rural edhi i.province i.branchuse if (tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# estimates store m7, title((7))

reg4_7 <- lm(lnloansize ~ lnoffer4 + low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province + branchuse, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1))

# xi: tobit lnloansize lnoffer4 low med waved2 waved3 sales_netincome sales_netincomesq sales_grossincome sales_grossincomesq appscore appscoresq appscore0 itcscore itcscoresq itczero dormancy trcount female dependants married age agesq rural edhi i.province if (tookup==1 & normrate_less== 1 & (offer4==final4)), ul ll
# estimates store m8, title((8))

reg4_8 <- lm(lnloansize ~ lnoffer4 + low + med + waved2 + waved3+ sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1, grossincome!=0))


stargazer(reg4_1, reg4_2, reg4_3, reg4_4, reg4_5, reg4_6, reg4_7, reg4_8, type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="8"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="8" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="5">loansize</td><td colspan="3">lnloansize</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td><td>(7)</td><td>(8)</td></tr>
<tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-4.368<sup>***</sup></td><td>-4.394<sup>***</sup></td><td>-25.876<sup>**</sup></td><td>-33.715<sup>***</sup></td><td>-31.362<sup>***</sup></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.229)</td><td>(1.273)</td><td>(12.156)</td><td>(11.216)</td><td>(11.226)</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">lnoffer4</td><td></td><td></td><td></td><td></td><td></td><td>-0.113<sup>**</sup></td><td>-0.143<sup>***</sup></td><td>-0.133<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td>(0.046)</td><td>(0.040)</td><td>(0.042)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">low</td><td>247.238<sup>***</sup></td><td>207.747<sup>***</sup></td><td>717.636<sup>***</sup></td><td>574.278<sup>***</sup></td><td>604.860<sup>***</sup></td><td>0.354<sup>***</sup></td><td>0.318<sup>***</sup></td><td>0.358<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(9.299)</td><td>(11.862)</td><td>(68.001)</td><td>(77.040)</td><td>(76.893)</td><td>(0.038)</td><td>(0.042)</td><td>(0.045)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">med</td><td>171.509<sup>***</sup></td><td>120.032<sup>***</sup></td><td>283.580<sup>***</sup></td><td>245.197<sup>***</sup></td><td>264.997<sup>***</sup></td><td>0.152<sup>***</sup></td><td>0.172<sup>***</sup></td><td>0.201<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(10.110)</td><td>(12.368)</td><td>(70.021)</td><td>(77.376)</td><td>(77.451)</td><td>(0.040)</td><td>(0.042)</td><td>(0.045)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved2</td><td>16.442</td><td>48.842</td><td>124.251</td><td>250.307</td><td>218.190<sup>**</sup></td><td>0.060</td><td>0.049</td><td>-0.209<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(10.526)</td><td>(83.299)</td><td>(86.664)</td><td>(420.729)</td><td>(87.436)</td><td>(0.049)</td><td>(0.230)</td><td>(0.114)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved3</td><td>29.524<sup>***</sup></td><td></td><td>328.387<sup>***</sup></td><td>175.720</td><td>417.776<sup>**</sup></td><td>0.213<sup>***</sup></td><td>0.085</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(10.106)</td><td></td><td>(83.168)</td><td>(441.171)</td><td>(208.393)</td><td>(0.047)</td><td>(0.241)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">grossincome</td><td></td><td>10.649<sup>***</sup></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(1.823)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">grossincomesq</td><td></td><td>-0.101<sup>***</sup></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.030)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">sales_grossincome</td><td></td><td></td><td></td><td>130.392<sup>***</sup></td><td>129.191<sup>***</sup></td><td></td><td>0.080<sup>***</sup></td><td>0.076<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(17.946)</td><td>(17.828)</td><td></td><td>(0.010)</td><td>(0.010)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">sales_grossincomesq</td><td></td><td></td><td></td><td>-1.446<sup>***</sup></td><td>-1.446<sup>***</sup></td><td></td><td>-0.001<sup>***</sup></td><td>-0.001<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.257)</td><td>(0.258)</td><td></td><td>(0.0001)</td><td>(0.0001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">sales_netincome</td><td></td><td></td><td></td><td>0.103<sup>***</sup></td><td>0.100<sup>***</sup></td><td></td><td>0.0001<sup>***</sup></td><td>0.0001<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.020)</td><td>(0.020)</td><td></td><td>(0.00001)</td><td>(0.00001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">sales_netincomesq</td><td></td><td></td><td></td><td>-0.00000<sup>***</sup></td><td>-0.00000<sup>***</sup></td><td></td><td>-0.000<sup>***</sup></td><td>-0.000<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td>(0.00000)</td><td>(0.00000)</td><td></td><td>(0.000)</td><td>(0.000)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">dormancy</td><td></td><td>-5.146<sup>***</sup></td><td></td><td>-0.379</td><td>-0.763</td><td></td><td>0.005</td><td>0.006<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.602)</td><td></td><td>(5.349)</td><td>(5.300)</td><td></td><td>(0.003)</td><td>(0.003)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">trcount</td><td></td><td>1.493</td><td></td><td>6.407</td><td>3.966</td><td></td><td>-0.003</td><td>-0.002</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.954)</td><td></td><td>(6.963)</td><td>(6.952)</td><td></td><td>(0.004)</td><td>(0.004)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">female</td><td></td><td>1.792</td><td></td><td>110.447<sup>**</sup></td><td>107.746<sup>*</sup></td><td></td><td>0.066<sup>**</sup></td><td>0.069<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(6.738)</td><td></td><td>(55.485)</td><td>(55.354)</td><td></td><td>(0.030)</td><td>(0.032)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">dependants</td><td></td><td>-3.859<sup>*</sup></td><td></td><td>-9.321</td><td>-10.308</td><td></td><td>0.001</td><td>0.001</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(2.087)</td><td></td><td>(16.693)</td><td>(16.409)</td><td></td><td>(0.009)</td><td>(0.009)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">married</td><td></td><td>12.176<sup>*</sup></td><td></td><td>114.144<sup>**</sup></td><td>102.406<sup>*</sup></td><td></td><td>0.098<sup>***</sup></td><td>0.090<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(6.870)</td><td></td><td>(54.614)</td><td>(54.535)</td><td></td><td>(0.030)</td><td>(0.031)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">age</td><td></td><td>5.977<sup>***</sup></td><td></td><td>0.850</td><td>9.251</td><td></td><td>0.013</td><td>0.021<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(1.890)</td><td></td><td>(15.428)</td><td>(15.272)</td><td></td><td>(0.008)</td><td>(0.009)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">agesq</td><td></td><td>-0.071<sup>***</sup></td><td></td><td>-0.039</td><td>-0.137</td><td></td><td>-0.0002<sup>**</sup></td><td>-0.0003<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.020)</td><td></td><td>(0.163)</td><td>(0.161)</td><td></td><td>(0.0001)</td><td>(0.0001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">rural</td><td></td><td>224.128</td><td></td><td>-343.108</td><td>-83.070</td><td></td><td>-0.204</td><td>-0.106<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.048)</td><td></td><td>(481.824)</td><td>(77.941)</td><td></td><td>(0.263)</td><td>(0.042)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">edhi</td><td></td><td>28.651<sup>***</sup></td><td></td><td>60.462</td><td>80.200</td><td></td><td>0.055<sup>*</sup></td><td>0.069<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(7.004)</td><td></td><td>(57.375)</td><td>(57.482)</td><td></td><td>(0.031)</td><td>(0.033)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscore</td><td></td><td>-0.200</td><td></td><td>3.171</td><td>-3.288</td><td></td><td>0.005</td><td>0.002</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(1.982)</td><td></td><td>(16.151)</td><td>(16.137)</td><td></td><td>(0.009)</td><td>(0.009)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscoresq</td><td></td><td>0.018</td><td></td><td>0.001</td><td>0.142</td><td></td><td>-0.00000</td><td>0.0001</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.032)</td><td></td><td>(0.262)</td><td>(0.262)</td><td></td><td>(0.0001)</td><td>(0.0002)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscore0</td><td></td><td>-76.879</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(252.135)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">itcscore</td><td></td><td>0.242</td><td></td><td>3.966</td><td>4.452</td><td></td><td>0.002</td><td>0.003</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.551)</td><td></td><td>(4.532)</td><td>(4.549)</td><td></td><td>(0.002)</td><td>(0.003)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">itcscoresq</td><td></td><td>-0.0003</td><td></td><td>-0.003</td><td>-0.003</td><td></td><td>-0.00000</td><td>-0.00000</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.0004)</td><td></td><td>(0.004)</td><td>(0.004)</td><td></td><td>(0.00000)</td><td>(0.00000)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">itczero</td><td></td><td>45.944</td><td></td><td>1,375.616</td><td>1,553.887</td><td></td><td>0.751</td><td>0.929</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(170.490)</td><td></td><td>(1,406.737)</td><td>(1,410.445)</td><td></td><td>(0.768)</td><td>(0.814)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceFree State</td><td></td><td>-124.674</td><td></td><td>-387.804</td><td>-239.357</td><td></td><td>-0.297</td><td>-0.141</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(506.852)</td><td></td><td>(683.105)</td><td>(281.089)</td><td></td><td>(0.373)</td><td>(0.155)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceGauteng</td><td></td><td>19.473</td><td></td><td>37.039</td><td>-134.487</td><td></td><td>0.186</td><td>-0.040</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(30.237)</td><td></td><td>(266.421)</td><td>(99.597)</td><td></td><td>(0.146)</td><td>(0.054)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceKwazulu-Natal</td><td></td><td>-258.388</td><td></td><td>-264.538</td><td>-15.432</td><td></td><td>-0.057</td><td>0.118</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(363.803)</td><td></td><td>(520.043)</td><td>(215.994)</td><td></td><td>(0.284)</td><td>(0.121)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceLimpopo Province</td><td></td><td>-340.390</td><td></td><td>90.233</td><td>-356.883<sup>**</sup></td><td></td><td>0.041</td><td>-0.229<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(506.866)</td><td></td><td>(585.874)</td><td>(143.130)</td><td></td><td>(0.320)</td><td>(0.078)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceMpumalanga</td><td></td><td>-135.813</td><td></td><td>534.036</td><td>121.078</td><td></td><td>0.282</td><td>0.003</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.088)</td><td></td><td>(617.181)</td><td>(335.165)</td><td></td><td>(0.337)</td><td>(0.181)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceNorth West</td><td></td><td>-131.135</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.174)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceWestern Cape</td><td></td><td>30.067</td><td></td><td>-59.649</td><td>-213.019</td><td></td><td>0.103</td><td>-0.019</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(34.358)</td><td></td><td>(283.887)</td><td>(133.010)</td><td></td><td>(0.155)</td><td>(0.072)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAD</td><td></td><td>-67.903<sup>*</sup></td><td></td><td>159.253</td><td></td><td></td><td>0.221</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(38.537)</td><td></td><td>(367.463)</td><td></td><td></td><td>(0.201)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAE</td><td></td><td>161.359</td><td></td><td>-488.633</td><td></td><td></td><td>0.060</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.026)</td><td></td><td>(1,133.892)</td><td></td><td></td><td>(0.619)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAI</td><td></td><td>66.148</td><td></td><td>871.480<sup>**</sup></td><td></td><td></td><td>0.571<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(49.508)</td><td></td><td>(356.009)</td><td></td><td></td><td>(0.194)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAV</td><td></td><td>-30.892</td><td></td><td>138.234</td><td></td><td></td><td>0.168</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(41.641)</td><td></td><td>(300.711)</td><td></td><td></td><td>(0.164)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBE</td><td></td><td>-131.952</td><td></td><td>335.560</td><td></td><td></td><td>0.356</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(613.548)</td><td></td><td>(772.983)</td><td></td><td></td><td>(0.422)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBF</td><td></td><td>-8.748</td><td></td><td>-181.734</td><td></td><td></td><td>-0.112</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(28.300)</td><td></td><td>(261.410)</td><td></td><td></td><td>(0.143)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBG</td><td></td><td>-160.180</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(612.006)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBH</td><td></td><td>-67.787</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.477)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBK</td><td></td><td>1.412</td><td></td><td>-160.891</td><td></td><td></td><td>-0.179</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(30.430)</td><td></td><td>(256.582)</td><td></td><td></td><td>(0.140)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBM</td><td></td><td>414.819</td><td></td><td>904.719</td><td></td><td></td><td>0.991</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(550.984)</td><td></td><td>(1,292.278)</td><td></td><td></td><td>(0.706)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBS</td><td></td><td>-276.999</td><td></td><td>325.516</td><td></td><td></td><td>0.167</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(355.597)</td><td></td><td>(587.135)</td><td></td><td></td><td>(0.321)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBV</td><td></td><td>-31.257</td><td></td><td>229.338</td><td></td><td></td><td>0.247</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(34.649)</td><td></td><td>(289.422)</td><td></td><td></td><td>(0.158)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBY</td><td></td><td>10.829</td><td></td><td>296.043</td><td></td><td></td><td>0.211<sup>*</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(24.739)</td><td></td><td>(218.098)</td><td></td><td></td><td>(0.119)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCK</td><td></td><td>-10.066</td><td></td><td>-284.536</td><td></td><td></td><td>-0.088</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(58.642)</td><td></td><td>(379.840)</td><td></td><td></td><td>(0.208)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCM</td><td></td><td>-31.731</td><td></td><td>-51.086</td><td></td><td></td><td>-0.036</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(62.602)</td><td></td><td>(430.070)</td><td></td><td></td><td>(0.235)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCP</td><td></td><td>-337.530</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.987)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCS</td><td></td><td>622.496</td><td></td><td>134.999</td><td></td><td></td><td>0.266</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(390.353)</td><td></td><td>(716.976)</td><td></td><td></td><td>(0.392)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCT</td><td></td><td>-4.514</td><td></td><td>25.867</td><td></td><td></td><td>0.114</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(33.470)</td><td></td><td>(259.376)</td><td></td><td></td><td>(0.142)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCV</td><td></td><td>-192.665</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.374)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCW</td><td></td><td>211.437</td><td></td><td>-1,266.715</td><td></td><td></td><td>-1.010</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.927)</td><td></td><td>(1,133.720)</td><td></td><td></td><td>(0.619)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDM</td><td></td><td>274.821</td><td></td><td>597.271</td><td></td><td></td><td>0.350</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.394)</td><td></td><td>(459.570)</td><td></td><td></td><td>(0.251)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDP</td><td></td><td>248.671</td><td></td><td>134.241</td><td></td><td></td><td>0.155</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.834)</td><td></td><td>(479.521)</td><td></td><td></td><td>(0.262)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDS</td><td></td><td>211.562</td><td></td><td>-149.091</td><td></td><td></td><td>0.037</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.832)</td><td></td><td>(487.054)</td><td></td><td></td><td>(0.266)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDU</td><td></td><td>202.096</td><td></td><td>118.266</td><td></td><td></td><td>0.255</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.833)</td><td></td><td>(491.050)</td><td></td><td></td><td>(0.268)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCEL</td><td></td><td>-10.257</td><td></td><td>-356.501</td><td></td><td></td><td>-0.069</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(35.301)</td><td></td><td>(306.399)</td><td></td><td></td><td>(0.167)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCEM</td><td></td><td>200.770</td><td></td><td>-39.218</td><td></td><td></td><td>0.080</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.540)</td><td></td><td>(473.751)</td><td></td><td></td><td>(0.259)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCER</td><td></td><td>-92.059</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(501.192)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGA</td><td></td><td>696.266<sup>**</sup></td><td></td><td>389.656</td><td></td><td></td><td>0.538</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.988)</td><td></td><td>(1,134.279)</td><td></td><td></td><td>(0.620)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGD</td><td></td><td>-396.805</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(711.676)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGK</td><td></td><td>135.776</td><td></td><td>-417.921</td><td></td><td></td><td>-0.253</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(363.422)</td><td></td><td>(717.245)</td><td></td><td></td><td>(0.392)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGM</td><td></td><td>4.617</td><td></td><td>83.577</td><td></td><td></td><td>-0.010</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(23.382)</td><td></td><td>(200.277)</td><td></td><td></td><td>(0.109)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGO</td><td></td><td>-291.285</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.820)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGP</td><td></td><td>-21.656</td><td></td><td>-258.204</td><td></td><td></td><td>-0.209<sup>*</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(26.462)</td><td></td><td>(231.214)</td><td></td><td></td><td>(0.126)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGR</td><td></td><td>-19.223</td><td></td><td>-261.229</td><td></td><td></td><td>-0.167</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(25.062)</td><td></td><td>(228.113)</td><td></td><td></td><td>(0.125)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGS</td><td></td><td>148.881</td><td></td><td>-2,142.568<sup>*</sup></td><td></td><td></td><td>-1.087<sup>*</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(178.428)</td><td></td><td>(1,139.870)</td><td></td><td></td><td>(0.623)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGT</td><td></td><td>9,401.419<sup>***</sup></td><td></td><td>7,450.596<sup>***</sup></td><td></td><td></td><td>1.674<sup>**</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(612.638)</td><td></td><td>(1,241.138)</td><td></td><td></td><td>(0.678)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGY</td><td></td><td>333.742</td><td></td><td>-521.008</td><td></td><td></td><td>-0.091</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(507.410)</td><td></td><td>(597.411)</td><td></td><td></td><td>(0.326)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCHL</td><td></td><td>-180.763</td><td></td><td>259.571</td><td></td><td></td><td>0.269</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(409.096)</td><td></td><td>(1,224.144)</td><td></td><td></td><td>(0.669)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCHV</td><td></td><td>-395.969</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(706.764)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCIM</td><td></td><td>210.527</td><td></td><td>-202.092</td><td></td><td></td><td>-0.015</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.802)</td><td></td><td>(473.879)</td><td></td><td></td><td>(0.259)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJA</td><td></td><td>-5.063</td><td></td><td>-72.779</td><td></td><td></td><td>-0.085</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(29.002)</td><td></td><td>(232.688)</td><td></td><td></td><td>(0.127)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJB</td><td></td><td>-13.072</td><td></td><td>-12.641</td><td></td><td></td><td>-0.063</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(24.645)</td><td></td><td>(220.781)</td><td></td><td></td><td>(0.121)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJC</td><td></td><td>-39.340</td><td></td><td>-20.713</td><td></td><td></td><td>0.046</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(30.916)</td><td></td><td>(330.317)</td><td></td><td></td><td>(0.180)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJG</td><td></td><td>1.360</td><td></td><td>-162.683</td><td></td><td></td><td>-0.235<sup>**</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(22.940)</td><td></td><td>(197.260)</td><td></td><td></td><td>(0.108)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJJ</td><td></td><td>305.965</td><td></td><td>435.904</td><td></td><td></td><td>0.399</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(364.106)</td><td></td><td>(512.746)</td><td></td><td></td><td>(0.280)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJM</td><td></td><td>6.208</td><td></td><td>244.845</td><td></td><td></td><td>0.161</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(21.166)</td><td></td><td>(192.745)</td><td></td><td></td><td>(0.105)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJP</td><td></td><td>-7.159</td><td></td><td>-838.914</td><td></td><td></td><td>-0.619</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(151.292)</td><td></td><td>(1,126.359)</td><td></td><td></td><td>(0.615)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJR</td><td></td><td>-35.952</td><td></td><td>-36.506</td><td></td><td></td><td>-0.021</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(28.021)</td><td></td><td>(264.231)</td><td></td><td></td><td>(0.144)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJW</td><td></td><td>-26.151</td><td></td><td>-78.669</td><td></td><td></td><td>-0.156</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(38.520)</td><td></td><td>(351.956)</td><td></td><td></td><td>(0.192)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKD</td><td></td><td>-73.857</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.392)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKM</td><td></td><td>-46.842<sup>*</sup></td><td></td><td>-365.830</td><td></td><td></td><td>-0.223</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(27.840)</td><td></td><td>(257.107)</td><td></td><td></td><td>(0.140)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKP</td><td></td><td>-35.018</td><td></td><td>-102.520</td><td></td><td></td><td>-0.029</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(23.256)</td><td></td><td>(226.096)</td><td></td><td></td><td>(0.124)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKR</td><td></td><td>-174.879</td><td></td><td>166.627</td><td></td><td></td><td>-0.157</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(613.448)</td><td></td><td>(948.783)</td><td></td><td></td><td>(0.518)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKS</td><td></td><td>51.905</td><td></td><td>783.412<sup>**</sup></td><td></td><td></td><td>0.444<sup>**</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(48.721)</td><td></td><td>(367.190)</td><td></td><td></td><td>(0.201)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKW</td><td></td><td>-242.814</td><td></td><td>35.260</td><td></td><td></td><td>0.133</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(356.173)</td><td></td><td>(582.170)</td><td></td><td></td><td>(0.318)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKY</td><td></td><td>93.615</td><td></td><td>-37.934</td><td></td><td></td><td>0.146</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(500.110)</td><td></td><td>(558.052)</td><td></td><td></td><td>(0.305)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCLM</td><td></td><td>-51.042</td><td></td><td>64.887</td><td></td><td></td><td>0.171</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(34.361)</td><td></td><td>(300.021)</td><td></td><td></td><td>(0.164)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCLT</td><td></td><td>92.169</td><td></td><td>15.828</td><td></td><td></td><td>0.095</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(618.520)</td><td></td><td>(374.777)</td><td></td><td></td><td>(0.205)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCLY</td><td></td><td>-147.013</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(706.648)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMA</td><td></td><td>109.656</td><td></td><td>-195.163</td><td></td><td></td><td>-0.207</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(167.148)</td><td></td><td>(802.151)</td><td></td><td></td><td>(0.438)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMB</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMD</td><td></td><td>-2.179</td><td></td><td>-263.323</td><td></td><td></td><td>-0.161</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(27.461)</td><td></td><td>(223.690)</td><td></td><td></td><td>(0.122)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMG</td><td></td><td>163.984</td><td></td><td>-1,268.324</td><td></td><td></td><td>-1.353<sup>**</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.997)</td><td></td><td>(1,133.266)</td><td></td><td></td><td>(0.619)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMI</td><td></td><td>41.124</td><td></td><td>-347.906</td><td></td><td></td><td>0.087</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(152.591)</td><td></td><td>(810.266)</td><td></td><td></td><td>(0.443)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMK</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMP</td><td></td><td>45.461</td><td></td><td>163.055</td><td></td><td></td><td>0.045</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(32.178)</td><td></td><td>(243.846)</td><td></td><td></td><td>(0.133)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMT</td><td></td><td>39.901</td><td></td><td>419.752</td><td></td><td></td><td>0.383<sup>**</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(38.525)</td><td></td><td>(291.402)</td><td></td><td></td><td>(0.159)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMV</td><td></td><td>-171.288</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(288.671)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMZ</td><td></td><td>-23.761</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.937)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNG</td><td></td><td>18.149</td><td></td><td>289.047</td><td></td><td></td><td>0.324<sup>**</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(41.457)</td><td></td><td>(291.189)</td><td></td><td></td><td>(0.159)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNL</td><td></td><td>3,746.531<sup>***</sup></td><td></td><td>1,723.007</td><td></td><td></td><td>0.905</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(794.183)</td><td></td><td>(1,144.179)</td><td></td><td></td><td>(0.625)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNM</td><td></td><td>211.816</td><td></td><td>74.367</td><td></td><td></td><td>0.214</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.671)</td><td></td><td>(473.812)</td><td></td><td></td><td>(0.259)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNS</td><td></td><td>316.402</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(711.032)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPA</td><td></td><td>-13.347</td><td></td><td>-95.466</td><td></td><td></td><td>-0.012</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(36.486)</td><td></td><td>(294.202)</td><td></td><td></td><td>(0.161)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPB</td><td></td><td>332.018</td><td></td><td>-218.690</td><td></td><td></td><td>-0.120</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(506.566)</td><td></td><td>(565.639)</td><td></td><td></td><td>(0.309)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPC</td><td></td><td>226.821</td><td></td><td>392.948<sup>*</sup></td><td></td><td></td><td>0.241<sup>*</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(455.828)</td><td></td><td>(238.210)</td><td></td><td></td><td>(0.130)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPE</td><td></td><td>39.898</td><td></td><td>308.254</td><td></td><td></td><td>0.386<sup>**</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(33.360)</td><td></td><td>(288.297)</td><td></td><td></td><td>(0.157)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPF</td><td></td><td>35.204</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(710.970)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPG</td><td></td><td>54.462</td><td></td><td>871.482<sup>**</sup></td><td></td><td></td><td>0.504<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(45.001)</td><td></td><td>(356.047)</td><td></td><td></td><td>(0.194)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPL</td><td></td><td>357.998</td><td></td><td>152.989</td><td></td><td></td><td>0.125</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(394.909)</td><td></td><td>(184.375)</td><td></td><td></td><td>(0.101)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPN</td><td></td><td>211.695</td><td></td><td>81.114</td><td></td><td></td><td>0.242</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.500)</td><td></td><td>(453.183)</td><td></td><td></td><td>(0.248)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPO</td><td></td><td>557.019</td><td></td><td>72.260</td><td></td><td></td><td>0.493</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(564.393)</td><td></td><td>(1,243.151)</td><td></td><td></td><td>(0.679)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPS</td><td></td><td>-79.626<sup>**</sup></td><td></td><td>40.062</td><td></td><td></td><td>0.028</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(37.238)</td><td></td><td>(379.854)</td><td></td><td></td><td>(0.208)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPT</td><td></td><td>16.900</td><td></td><td>-78.940</td><td></td><td></td><td>0.020</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(36.360)</td><td></td><td>(297.003)</td><td></td><td></td><td>(0.162)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPW</td><td></td><td>74.694</td><td></td><td>-329.203</td><td></td><td></td><td>-0.206</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(618.985)</td><td></td><td>(427.685)</td><td></td><td></td><td>(0.234)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCQT</td><td></td><td>-46.360</td><td></td><td>1,831.700<sup>***</sup></td><td></td><td></td><td>0.905<sup>***</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(357.202)</td><td></td><td>(607.293)</td><td></td><td></td><td>(0.332)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRB</td><td></td><td>-52.986<sup>*</sup></td><td></td><td>-274.916</td><td></td><td></td><td>-0.246</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(31.833)</td><td></td><td>(299.473)</td><td></td><td></td><td>(0.164)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRL</td><td></td><td>-28.067</td><td></td><td>-451.838</td><td></td><td></td><td>-0.366</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(250.134)</td><td></td><td>(1,129.409)</td><td></td><td></td><td>(0.617)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRM</td><td></td><td>270.326</td><td></td><td>254.131</td><td></td><td></td><td>0.241</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.953)</td><td></td><td>(479.517)</td><td></td><td></td><td>(0.262)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRP</td><td></td><td>13.673</td><td></td><td>-256.428</td><td></td><td></td><td>-0.237</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(159.498)</td><td></td><td>(802.839)</td><td></td><td></td><td>(0.439)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSD</td><td></td><td>114.359</td><td></td><td>-607.036</td><td></td><td></td><td>-1.034<sup>*</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.616)</td><td></td><td>(1,125.915)</td><td></td><td></td><td>(0.615)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSM</td><td></td><td>224.636</td><td></td><td>162.076</td><td></td><td></td><td>0.292</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(354.105)</td><td></td><td>(512.249)</td><td></td><td></td><td>(0.280)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSP</td><td></td><td>101.492<sup>***</sup></td><td></td><td>607.123<sup>**</sup></td><td></td><td></td><td>0.081</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(30.031)</td><td></td><td>(236.426)</td><td></td><td></td><td>(0.129)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSW</td><td></td><td>18.565</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(499.758)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCTZ</td><td></td><td>99.986</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(618.404)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCUL</td><td></td><td>-151.009</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(168.364)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCUP</td><td></td><td>99.238</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(502.203)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCUT</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVD</td><td></td><td>-233.642</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(612.479)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVP</td><td></td><td>-305.184</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(353.540)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVR</td><td></td><td>-126.133</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(355.807)</td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVS</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCWB</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCWK</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCWY</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCZE</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>71.625<sup>***</sup></td><td>-78.715</td><td>1,143.207<sup>***</sup></td><td>-1,040.444</td><td>-1,399.176</td><td>6.934<sup>***</sup></td><td>5.196<sup>***</sup></td><td>5.125<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(13.424)</td><td>(178.235)</td><td>(122.195)</td><td>(1,531.064)</td><td>(1,477.391)</td><td>(0.100)</td><td>(0.839)</td><td>(0.847)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>31,231</td><td>28,197</td><td>2,325</td><td>2,304</td><td>2,304</td><td>2,325</td><td>2,304</td><td>2,035</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.033</td><td>0.062</td><td>0.068</td><td>0.293</td><td>0.237</td><td>0.058</td><td>0.342</td><td>0.302</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.033</td><td>0.058</td><td>0.066</td><td>0.259</td><td>0.227</td><td>0.056</td><td>0.310</td><td>0.292</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>506.350 (df = 31225)</td><td>499.170 (df = 28070)</td><td>1,250.354 (df = 2319)</td><td>1,115.016 (df = 2197)</td><td>1,138.580 (df = 2274)</td><td>0.714 (df = 2319)</td><td>0.609 (df = 2197)</td><td>0.616 (df = 2006)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>212.779<sup>***</sup> (df = 5; 31225)</td><td>14.677<sup>***</sup> (df = 126; 28070)</td><td>33.679<sup>***</sup> (df = 5; 2319)</td><td>8.580<sup>***</sup> (df = 106; 2197)</td><td>24.320<sup>***</sup> (df = 29; 2274)</td><td>28.383<sup>***</sup> (df = 5; 2319)</td><td>10.761<sup>***</sup> (df = 106; 2197)</td><td>30.997<sup>***</sup> (df = 28; 2006)</td></tr>
<tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="8" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

##### TABLE 5 - GROSS REVENUE AND REPAYMENT SENSITIVITIES TO INTEREST RATES


```r
# **REVENUES TABLE, COLUMN 1
# regress grossinterest offer4 low med waved2 waved3 if ((normrate_less==1) & (offer4==final4)), cluster(branchuse)

reg5_1 <- lm(grossinterest ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1))

# **REVENUES TABLE, COLUMN 2
# regress pstdue_average offer4 low med waved2 waved3 if ((normrate_less==1) & (offer4==final4) & tookup==1), cluster(branchuse)

reg5_2 <- lm(pstdue_average ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

# **REVENUES TABLE, COLUMN 3
# tobit pstdue_average offer4 low med waved2 waved3 if ((normrate_less==1) & (offer4==final4) & tookup==1), ll(0)

reg5_3 <- lm(pstdue_average ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))


stargazer(reg5_1, reg5_2, reg5_3, type="html",align=TRUE, dep.var.labels=c("Gross interest revenue","Average past due", "Average past due"), covariate.labels=c("interest rate in pp terms (e.g., 8.2)"),no.space=TRUE)
```


<table style="text-align:center"><tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>Gross interest revenue</td><td colspan="2">Average past due</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">interest rate in pp terms (e.g., 8.2)</td><td>2.553<sup>***</sup></td><td>12.161<sup>***</sup></td><td>12.161<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.641)</td><td>(3.553)</td><td>(3.553)</td></tr>
<tr><td style="text-align:left">low</td><td>109.053<sup>***</sup></td><td>-99.383<sup>***</sup></td><td>-99.383<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(4.852)</td><td>(19.874)</td><td>(19.874)</td></tr>
<tr><td style="text-align:left">med</td><td>72.098<sup>***</sup></td><td>36.996<sup>*</sup></td><td>36.996<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(5.275)</td><td>(20.464)</td><td>(20.464)</td></tr>
<tr><td style="text-align:left">waved2</td><td>11.277<sup>**</sup></td><td>-2.260</td><td>-2.260</td></tr>
<tr><td style="text-align:left"></td><td>(5.493)</td><td>(25.328)</td><td>(25.328)</td></tr>
<tr><td style="text-align:left">waved3</td><td>20.908<sup>***</sup></td><td>78.238<sup>***</sup></td><td>78.238<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(5.274)</td><td>(24.307)</td><td>(24.307)</td></tr>
<tr><td style="text-align:left">Constant</td><td>-18.168<sup>***</sup></td><td>53.227</td><td>53.227</td></tr>
<tr><td style="text-align:left"></td><td>(7.005)</td><td>(35.712)</td><td>(35.712)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>31,231</td><td>2,325</td><td>2,325</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.020</td><td>0.050</td><td>0.050</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.020</td><td>0.048</td><td>0.048</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>264.218 (df = 31225)</td><td>365.427 (df = 2319)</td><td>365.427 (df = 2319)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>125.512<sup>***</sup> (df = 5; 31225)</td><td>24.233<sup>***</sup> (df = 5; 2319)</td><td>24.233<sup>***</sup> (df = 5; 2319)</td></tr>
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
# full

reg6_1 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1))

# xi: regress loansize offer4 low med waved2 waved3 grossincome grossincomesq dormancy trcount female dependants married age agesq rural i.province i.branchuse appscore appscoresq appscore0 itcscore itcscoresq itczero if (normrate_less== 1 & edhi==1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m2, title((2))
# 
# edhi=1 -> High education

reg6_2 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + trcount + female + dependants + married + age + agesq + rural + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, edhi==1))

# xi: regress loansize offer4 low med waved2 waved3 grossincome grossincomesq trcount female dependants married age agesq rural edhi i.province i.branchuse appscore appscoresq appscore0 itcscore itcscoresq itczero if (normrate_less== 1 & (offer4==final4) & dormancy<10), cluster(branchuse)
# estimates store m3, title((3))
# sum loansize offer4 if e(sample)
#
# dormancy<10 -> Borrowed in last 9 months

reg6_3 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + trcount + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, dormancy<10))

# xi: regress loansize offer4 low med waved2 waved3 grossincome grossincomesq dormancy female dependants married age agesq rural edhi i.province i.branchuse appscore appscoresq appscore0 itcscore itcscoresq itczero if (normrate_less== 1 & (offer4==final4) & trcount>2), cluster(branchuse)
# estimates store m4, title((4))
# sum loansize offer4 if e(sample)
#
# trcount>2 -> #Past loans more than 2

reg6_4 <- lm(loansize ~ offer4+ low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, trcount>2))


stargazer(reg6_1, reg6_2, reg6_3, reg6_4, type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="4">loansize</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-4.394<sup>***</sup></td><td>-5.397<sup>**</sup></td><td>-6.591<sup>**</sup></td><td>-5.498<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.273)</td><td>(2.478)</td><td>(2.732)</td><td>(2.169)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">low</td><td>207.747<sup>***</sup></td><td>290.921<sup>***</sup></td><td>217.114<sup>***</sup></td><td>203.315<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(11.862)</td><td>(23.001)</td><td>(16.873)</td><td>(19.128)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">med</td><td>120.032<sup>***</sup></td><td>161.366<sup>***</sup></td><td>129.626<sup>***</sup></td><td>129.120<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(12.368)</td><td>(24.017)</td><td>(16.667)</td><td>(19.829)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved2</td><td>48.842</td><td>46.996</td><td>-3.268</td><td>133.057</td></tr>
<tr><td style="text-align:left"></td><td>(83.299)</td><td>(148.079)</td><td>(163.354)</td><td>(127.097)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved3</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">grossincome</td><td>10.649<sup>***</sup></td><td>11.454<sup>***</sup></td><td>16.513<sup>***</sup></td><td>13.252<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.823)</td><td>(2.812)</td><td>(3.439)</td><td>(3.081)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">grossincomesq</td><td>-0.101<sup>***</sup></td><td>-0.103<sup>***</sup></td><td>-0.134<sup>***</sup></td><td>-0.110<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.030)</td><td>(0.039)</td><td>(0.044)</td><td>(0.039)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">dormancy</td><td>-5.146<sup>***</sup></td><td>-6.460<sup>***</sup></td><td></td><td>-5.710<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.602)</td><td>(1.162)</td><td></td><td>(1.211)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">trcount</td><td>1.493</td><td>2.428</td><td>2.720</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.954)</td><td>(1.964)</td><td>(1.757)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">female</td><td>1.792</td><td>26.880<sup>**</sup></td><td>1.275</td><td>8.627</td></tr>
<tr><td style="text-align:left"></td><td>(6.738)</td><td>(13.014)</td><td>(13.264)</td><td>(11.065)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">dependants</td><td>-3.859<sup>*</sup></td><td>-9.603<sup>**</sup></td><td>-1.238</td><td>-4.310</td></tr>
<tr><td style="text-align:left"></td><td>(2.087)</td><td>(4.674)</td><td>(4.175)</td><td>(3.331)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">married</td><td>12.176<sup>*</sup></td><td>10.824</td><td>12.708</td><td>11.551</td></tr>
<tr><td style="text-align:left"></td><td>(6.870)</td><td>(13.039)</td><td>(13.624)</td><td>(11.159)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">age</td><td>5.977<sup>***</sup></td><td>14.655<sup>***</sup></td><td>9.297<sup>**</sup></td><td>5.389<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.890)</td><td>(4.657)</td><td>(3.756)</td><td>(3.229)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">agesq</td><td>-0.071<sup>***</sup></td><td>-0.174<sup>***</sup></td><td>-0.113<sup>***</sup></td><td>-0.065<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.020)</td><td>(0.053)</td><td>(0.040)</td><td>(0.033)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">rural</td><td>224.128</td><td>-56.789</td><td>235.313</td><td>236.547</td></tr>
<tr><td style="text-align:left"></td><td>(354.048)</td><td>(81.930)</td><td>(477.194)</td><td>(596.293)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">edhi</td><td>28.651<sup>***</sup></td><td></td><td>54.942<sup>***</sup></td><td>40.833<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(7.004)</td><td></td><td>(13.905)</td><td>(11.498)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscore</td><td>-0.200</td><td>-5.555</td><td>-3.298</td><td>0.689</td></tr>
<tr><td style="text-align:left"></td><td>(1.982)</td><td>(4.173)</td><td>(3.914)</td><td>(3.243)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscoresq</td><td>0.018</td><td>0.090</td><td>0.081</td><td>0.009</td></tr>
<tr><td style="text-align:left"></td><td>(0.032)</td><td>(0.066)</td><td>(0.065)</td><td>(0.052)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">appscore0</td><td>-76.879</td><td>-255.116</td><td></td><td>-86.766</td></tr>
<tr><td style="text-align:left"></td><td>(252.135)</td><td>(441.498)</td><td></td><td>(348.847)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">itcscore</td><td>0.242</td><td>0.179</td><td>0.184</td><td>0.904</td></tr>
<tr><td style="text-align:left"></td><td>(0.551)</td><td>(1.024)</td><td>(1.077)</td><td>(0.923)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">itcscoresq</td><td>-0.0003</td><td>-0.0002</td><td>-0.0003</td><td>-0.001</td></tr>
<tr><td style="text-align:left"></td><td>(0.0004)</td><td>(0.001)</td><td>(0.001)</td><td>(0.001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">itczero</td><td>45.944</td><td>55.343</td><td>11.548</td><td>251.203</td></tr>
<tr><td style="text-align:left"></td><td>(170.490)</td><td>(311.516)</td><td>(334.405)</td><td>(285.128)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceFree State</td><td>-124.674</td><td>-155.090</td><td>83.234</td><td>-253.397</td></tr>
<tr><td style="text-align:left"></td><td>(506.852)</td><td>(631.620)</td><td>(219.514)</td><td>(609.991)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceGauteng</td><td>19.473</td><td>-13.308</td><td>-35.707</td><td>-12.774</td></tr>
<tr><td style="text-align:left"></td><td>(30.237)</td><td>(64.251)</td><td>(60.245)</td><td>(51.499)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceKwazulu-Natal</td><td>-258.388</td><td>-73.361</td><td>-225.088</td><td>-388.133</td></tr>
<tr><td style="text-align:left"></td><td>(363.803)</td><td>(171.213)</td><td>(504.756)</td><td>(610.141)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceLimpopo Province</td><td>-340.390</td><td>8.661</td><td>-306.820</td><td>-268.466</td></tr>
<tr><td style="text-align:left"></td><td>(506.866)</td><td>(110.186)</td><td>(692.832)</td><td>(600.075)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceMpumalanga</td><td>-135.813</td><td>-154.729</td><td>-147.600</td><td>-198.706</td></tr>
<tr><td style="text-align:left"></td><td>(354.088)</td><td>(436.276)</td><td>(673.307)</td><td>(597.104)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceNorth West</td><td>-131.135</td><td>-270.374</td><td>-375.009</td><td>-185.506</td></tr>
<tr><td style="text-align:left"></td><td>(354.174)</td><td>(614.868)</td><td>(674.674)</td><td>(423.433)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">provinceWestern Cape</td><td>30.067</td><td>-25.294</td><td>15.455</td><td>-20.065</td></tr>
<tr><td style="text-align:left"></td><td>(34.358)</td><td>(75.213)</td><td>(67.830)</td><td>(60.347)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAD</td><td>-67.903<sup>*</sup></td><td></td><td>-169.715<sup>**</sup></td><td>-78.558</td></tr>
<tr><td style="text-align:left"></td><td>(38.537)</td><td></td><td>(72.501)</td><td>(57.492)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAE</td><td>161.359</td><td>576.799</td><td>152.249</td><td>163.348</td></tr>
<tr><td style="text-align:left"></td><td>(354.026)</td><td>(613.755)</td><td>(477.157)</td><td>(422.546)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAI</td><td>66.148</td><td>136.719<sup>*</sup></td><td>66.362</td><td>96.555</td></tr>
<tr><td style="text-align:left"></td><td>(49.508)</td><td>(82.915)</td><td>(94.697)</td><td>(77.160)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCAV</td><td>-30.892</td><td>29.912</td><td>-44.679</td><td>-19.176</td></tr>
<tr><td style="text-align:left"></td><td>(41.641)</td><td>(80.105)</td><td>(74.389)</td><td>(64.410)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBE</td><td>-131.952</td><td>173.307</td><td>-305.055</td><td>-126.391</td></tr>
<tr><td style="text-align:left"></td><td>(613.548)</td><td>(621.320)</td><td>(517.683)</td><td>(845.321)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBF</td><td>-8.748</td><td>-0.237</td><td>24.426</td><td>10.487</td></tr>
<tr><td style="text-align:left"></td><td>(28.300)</td><td>(46.104)</td><td>(61.384)</td><td>(45.315)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBG</td><td>-160.180</td><td>122.741</td><td>-187.940</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(612.006)</td><td>(616.656)</td><td>(1,062.468)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBH</td><td>-67.787</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(499.477)</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBK</td><td>1.412</td><td>2.686</td><td>53.385</td><td>6.446</td></tr>
<tr><td style="text-align:left"></td><td>(30.430)</td><td>(57.384)</td><td>(61.891)</td><td>(48.392)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBM</td><td>414.819</td><td>416.503</td><td>-347.142</td><td>1,071.369</td></tr>
<tr><td style="text-align:left"></td><td>(550.984)</td><td>(680.055)</td><td>(705.134)</td><td>(739.537)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBS</td><td>-276.999</td><td>-46.203</td><td>-285.341</td><td>-352.353</td></tr>
<tr><td style="text-align:left"></td><td>(355.597)</td><td>(104.173)</td><td>(481.658)</td><td>(599.026)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBV</td><td>-31.257</td><td>-43.334</td><td>-33.938</td><td>-6.435</td></tr>
<tr><td style="text-align:left"></td><td>(34.649)</td><td>(65.684)</td><td>(68.822)</td><td>(62.807)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCBY</td><td>10.829</td><td>-29.392</td><td>25.506</td><td>9.762</td></tr>
<tr><td style="text-align:left"></td><td>(24.739)</td><td>(49.973)</td><td>(49.784)</td><td>(42.940)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCK</td><td>-10.066</td><td>19.355</td><td>3.342</td><td>-103.184</td></tr>
<tr><td style="text-align:left"></td><td>(58.642)</td><td>(115.057)</td><td>(93.461)</td><td>(149.963)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCM</td><td>-31.731</td><td>-61.290</td><td>-53.836</td><td>-165.913</td></tr>
<tr><td style="text-align:left"></td><td>(62.602)</td><td>(126.792)</td><td>(98.061)</td><td>(202.430)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCP</td><td>-337.530</td><td>-337.459</td><td>-361.591</td><td>-357.391</td></tr>
<tr><td style="text-align:left"></td><td>(499.987)</td><td>(613.663)</td><td>(673.462)</td><td>(596.688)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCS</td><td>622.496</td><td></td><td>918.562</td><td>908.430</td></tr>
<tr><td style="text-align:left"></td><td>(390.353)</td><td></td><td>(613.073)</td><td>(665.087)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCT</td><td>-4.514</td><td>48.337</td><td>-9.134</td><td>29.049</td></tr>
<tr><td style="text-align:left"></td><td>(33.470)</td><td>(66.151)</td><td>(65.638)</td><td>(63.599)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCV</td><td>-192.665</td><td>-97.854</td><td>-242.628</td><td>-196.430</td></tr>
<tr><td style="text-align:left"></td><td>(353.374)</td><td>(611.830)</td><td>(672.307)</td><td>(421.407)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCCW</td><td>211.437</td><td>202.275</td><td>198.558</td><td>198.796</td></tr>
<tr><td style="text-align:left"></td><td>(499.927)</td><td>(613.483)</td><td>(673.335)</td><td>(596.562)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDM</td><td>274.821</td><td>86.180</td><td>322.341</td><td>286.554</td></tr>
<tr><td style="text-align:left"></td><td>(353.394)</td><td>(68.794)</td><td>(475.754)</td><td>(595.313)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDP</td><td>248.671</td><td>29.044</td><td>277.240</td><td>267.197</td></tr>
<tr><td style="text-align:left"></td><td>(353.834)</td><td>(82.948)</td><td>(476.838)</td><td>(596.194)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDS</td><td>211.562</td><td>-33.494</td><td>212.427</td><td>251.989</td></tr>
<tr><td style="text-align:left"></td><td>(353.832)</td><td>(77.167)</td><td>(477.063)</td><td>(596.087)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCDU</td><td>202.096</td><td>-45.776</td><td>181.033</td><td>218.618</td></tr>
<tr><td style="text-align:left"></td><td>(353.833)</td><td>(85.483)</td><td>(476.817)</td><td>(596.019)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCEL</td><td>-10.257</td><td>-22.862</td><td>-80.461</td><td>-88.425</td></tr>
<tr><td style="text-align:left"></td><td>(35.301)</td><td>(72.437)</td><td>(66.172)</td><td>(60.927)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCEM</td><td>200.770</td><td>2.850</td><td>174.263</td><td>209.517</td></tr>
<tr><td style="text-align:left"></td><td>(353.540)</td><td>(73.069)</td><td>(475.954)</td><td>(595.408)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCER</td><td>-92.059</td><td>132.170</td><td>-99.149</td><td>-40.780</td></tr>
<tr><td style="text-align:left"></td><td>(501.192)</td><td>(444.282)</td><td>(826.505)</td><td>(844.476)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGA</td><td>696.266<sup>**</sup></td><td>1,526.480<sup>**</sup></td><td></td><td>699.987<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(353.988)</td><td>(613.643)</td><td></td><td>(422.503)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGD</td><td>-396.805</td><td>-148.803</td><td>-382.617</td><td>-482.933</td></tr>
<tr><td style="text-align:left"></td><td>(711.676)</td><td>(767.596)</td><td>(1,075.354)</td><td>(1,039.496)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGK</td><td>135.776</td><td>-76.526</td><td>126.811</td><td>153.039</td></tr>
<tr><td style="text-align:left"></td><td>(363.422)</td><td>(187.692)</td><td>(490.111)</td><td>(631.119)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGM</td><td>4.617</td><td>29.496</td><td>21.428</td><td>29.410</td></tr>
<tr><td style="text-align:left"></td><td>(23.382)</td><td>(39.906)</td><td>(47.579)</td><td>(39.153)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGO</td><td>-291.285</td><td>-306.784</td><td>-315.826</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(499.820)</td><td>(613.176)</td><td>(673.018)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGP</td><td>-21.656</td><td>-51.624</td><td>5.205</td><td>-14.643</td></tr>
<tr><td style="text-align:left"></td><td>(26.462)</td><td>(45.457)</td><td>(54.236)</td><td>(46.946)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGR</td><td>-19.223</td><td>-51.156</td><td>2.656</td><td>-39.200</td></tr>
<tr><td style="text-align:left"></td><td>(25.062)</td><td>(44.909)</td><td>(52.387)</td><td>(41.405)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGS</td><td>148.881</td><td>287.395</td><td>-47.380</td><td>222.617</td></tr>
<tr><td style="text-align:left"></td><td>(178.428)</td><td>(276.289)</td><td>(304.006)</td><td>(245.114)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGT</td><td>9,401.419<sup>***</sup></td><td>9,573.989<sup>***</sup></td><td>9,358.945<sup>***</sup></td><td>9,342.464<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(612.638)</td><td>(619.674)</td><td>(825.272)</td><td>(843.542)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCGY</td><td>333.742</td><td>-46.656</td><td>282.789</td><td>192.412</td></tr>
<tr><td style="text-align:left"></td><td>(507.410)</td><td>(110.105)</td><td>(694.058)</td><td>(600.814)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCHL</td><td>-180.763</td><td>205.433</td><td>-108.368</td><td>-147.974</td></tr>
<tr><td style="text-align:left"></td><td>(409.096)</td><td>(366.757)</td><td>(564.816)</td><td>(667.677)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCHV</td><td>-395.969</td><td>-168.128</td><td>-452.650</td><td>-411.563</td></tr>
<tr><td style="text-align:left"></td><td>(706.764)</td><td>(753.237)</td><td>(1,062.988)</td><td>(1,031.559)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCIM</td><td>210.527</td><td>0.492</td><td>189.015</td><td>204.699</td></tr>
<tr><td style="text-align:left"></td><td>(353.802)</td><td>(88.179)</td><td>(476.626)</td><td>(595.866)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJA</td><td>-5.063</td><td>20.331</td><td>46.141</td><td>-31.738</td></tr>
<tr><td style="text-align:left"></td><td>(29.002)</td><td>(58.387)</td><td>(57.216)</td><td>(46.036)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJB</td><td>-13.072</td><td>-53.058</td><td>13.882</td><td>-6.732</td></tr>
<tr><td style="text-align:left"></td><td>(24.645)</td><td>(46.600)</td><td>(52.664)</td><td>(39.250)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJC</td><td>-39.340</td><td>-30.913</td><td>-15.096</td><td>-66.847</td></tr>
<tr><td style="text-align:left"></td><td>(30.916)</td><td>(56.070)</td><td>(62.808)</td><td>(62.316)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJG</td><td>1.360</td><td>24.849</td><td>45.607</td><td>-14.624</td></tr>
<tr><td style="text-align:left"></td><td>(22.940)</td><td>(45.140)</td><td>(49.397)</td><td>(39.797)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJJ</td><td>305.965</td><td>47.032</td><td>257.455</td><td>401.795</td></tr>
<tr><td style="text-align:left"></td><td>(364.106)</td><td>(168.722)</td><td>(504.659)</td><td>(614.000)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJM</td><td>6.208</td><td>8.995</td><td>71.817</td><td>10.094</td></tr>
<tr><td style="text-align:left"></td><td>(21.166)</td><td>(39.300)</td><td>(45.189)</td><td>(34.359)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJP</td><td>-7.159</td><td>64.765</td><td>72.798</td><td>218.262</td></tr>
<tr><td style="text-align:left"></td><td>(151.292)</td><td>(306.754)</td><td>(276.114)</td><td>(421.369)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJR</td><td>-35.952</td><td>-80.901</td><td>-17.998</td><td>-49.385</td></tr>
<tr><td style="text-align:left"></td><td>(28.021)</td><td>(50.468)</td><td>(59.540)</td><td>(43.978)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCJW</td><td>-26.151</td><td>-70.146</td><td>24.199</td><td>-51.139</td></tr>
<tr><td style="text-align:left"></td><td>(38.520)</td><td>(75.402)</td><td>(75.407)</td><td>(72.351)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKD</td><td>-73.857</td><td>0.972</td><td>22.637</td><td>-191.592</td></tr>
<tr><td style="text-align:left"></td><td>(499.392)</td><td>(749.357)</td><td>(950.511)</td><td>(728.827)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKM</td><td>-46.842<sup>*</sup></td><td>-73.378</td><td>-40.116</td><td>-56.048</td></tr>
<tr><td style="text-align:left"></td><td>(27.840)</td><td>(51.655)</td><td>(59.981)</td><td>(45.011)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKP</td><td>-35.018</td><td>-48.065</td><td>-8.929</td><td>-55.159</td></tr>
<tr><td style="text-align:left"></td><td>(23.256)</td><td>(43.682)</td><td>(48.914)</td><td>(39.116)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKR</td><td>-174.879</td><td>94.380</td><td>-408.635</td><td>-175.999</td></tr>
<tr><td style="text-align:left"></td><td>(613.448)</td><td>(624.108)</td><td>(526.616)</td><td>(845.057)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKS</td><td>51.905</td><td>124.463</td><td>66.969</td><td>111.810</td></tr>
<tr><td style="text-align:left"></td><td>(48.721)</td><td>(87.097)</td><td>(91.895)</td><td>(78.706)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKW</td><td>-242.814</td><td>-13.576</td><td>-297.817</td><td>-286.323</td></tr>
<tr><td style="text-align:left"></td><td>(356.173)</td><td>(109.972)</td><td>(482.967)</td><td>(599.811)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCKY</td><td>93.615</td><td>87.980</td><td>-123.840</td><td>126.660</td></tr>
<tr><td style="text-align:left"></td><td>(500.110)</td><td>(614.358)</td><td>(165.329)</td><td>(597.222)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCLM</td><td>-51.042</td><td>13.698</td><td>-99.883</td><td>-39.062</td></tr>
<tr><td style="text-align:left"></td><td>(34.361)</td><td>(68.355)</td><td>(66.463)</td><td>(52.961)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCLT</td><td>92.169</td><td>10.271</td><td>8.166</td><td>-95.644</td></tr>
<tr><td style="text-align:left"></td><td>(618.520)</td><td>(68.578)</td><td>(841.654)</td><td>(68.507)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCLY</td><td>-147.013</td><td></td><td>-104.646</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(706.648)</td><td></td><td>(1,062.693)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMA</td><td>109.656</td><td>-80.262</td><td>-53.273</td><td>355.414</td></tr>
<tr><td style="text-align:left"></td><td>(167.148)</td><td>(433.074)</td><td>(337.449)</td><td>(298.667)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMB</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMD</td><td>-2.179</td><td>4.953</td><td>23.010</td><td>-16.964</td></tr>
<tr><td style="text-align:left"></td><td>(27.461)</td><td>(49.089)</td><td>(55.782)</td><td>(46.876)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMG</td><td>163.984</td><td>240.317</td><td>90.167</td><td>156.806</td></tr>
<tr><td style="text-align:left"></td><td>(499.997)</td><td>(613.707)</td><td>(673.308)</td><td>(596.276)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMI</td><td>41.124</td><td>54.229</td><td>-112.053</td><td>-25.760</td></tr>
<tr><td style="text-align:left"></td><td>(152.591)</td><td>(310.221)</td><td>(303.918)</td><td>(201.978)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMK</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMP</td><td>45.461</td><td>-16.921</td><td>105.531<sup>*</sup></td><td>78.494</td></tr>
<tr><td style="text-align:left"></td><td>(32.178)</td><td>(56.712)</td><td>(58.397)</td><td>(73.719)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMT</td><td>39.901</td><td>79.654</td><td>12.271</td><td>47.060</td></tr>
<tr><td style="text-align:left"></td><td>(38.525)</td><td>(83.834)</td><td>(75.725)</td><td>(58.086)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMV</td><td>-171.288</td><td>-113.621</td><td>-122.260</td><td>-319.816</td></tr>
<tr><td style="text-align:left"></td><td>(288.671)</td><td>(611.997)</td><td>(389.091)</td><td>(595.419)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCMZ</td><td>-23.761</td><td>69.351</td><td></td><td>-15.421</td></tr>
<tr><td style="text-align:left"></td><td>(353.937)</td><td>(613.500)</td><td></td><td>(596.276)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNG</td><td>18.149</td><td>101.625</td><td>-15.129</td><td>-13.327</td></tr>
<tr><td style="text-align:left"></td><td>(41.457)</td><td>(80.706)</td><td>(82.234)</td><td>(71.194)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNL</td><td>3,746.531<sup>***</sup></td><td>3,574.007<sup>***</sup></td><td>3,642.690<sup>***</sup></td><td>3,613.213<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(794.183)</td><td>(613.214)</td><td>(1,075.136)</td><td>(597.045)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNM</td><td>211.816</td><td>-30.836</td><td>184.916</td><td>223.547</td></tr>
<tr><td style="text-align:left"></td><td>(353.671)</td><td>(72.425)</td><td>(476.182)</td><td>(595.632)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCNS</td><td>316.402</td><td>-60.966</td><td></td><td>196.971</td></tr>
<tr><td style="text-align:left"></td><td>(711.032)</td><td>(618.424)</td><td></td><td>(844.022)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPA</td><td>-13.347</td><td>21.010</td><td>20.801</td><td>-16.663</td></tr>
<tr><td style="text-align:left"></td><td>(36.486)</td><td>(77.639)</td><td>(72.519)</td><td>(64.058)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPB</td><td>332.018</td><td>-52.303</td><td>259.405</td><td>216.252</td></tr>
<tr><td style="text-align:left"></td><td>(506.566)</td><td>(97.870)</td><td>(691.917)</td><td>(599.336)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPC</td><td>226.821</td><td>-251.799</td><td>-74.534</td><td>240.707</td></tr>
<tr><td style="text-align:left"></td><td>(455.828)</td><td>(615.795)</td><td>(671.507)</td><td>(687.015)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPE</td><td>39.898</td><td>-22.086</td><td>31.202</td><td>-16.002</td></tr>
<tr><td style="text-align:left"></td><td>(33.360)</td><td>(69.497)</td><td>(65.796)</td><td>(57.568)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPF</td><td>35.204</td><td></td><td>-148.705</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(710.970)</td><td></td><td>(704.400)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPG</td><td>54.462</td><td>157.398</td><td>118.223</td><td>27.734</td></tr>
<tr><td style="text-align:left"></td><td>(45.001)</td><td>(102.335)</td><td>(90.105)</td><td>(91.270)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPL</td><td>357.998</td><td>-9.600</td><td>105.944</td><td>481.155</td></tr>
<tr><td style="text-align:left"></td><td>(394.909)</td><td>(312.201)</td><td>(548.341)</td><td>(643.003)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPN</td><td>211.695</td><td>33.801</td><td>208.284</td><td>219.609</td></tr>
<tr><td style="text-align:left"></td><td>(353.500)</td><td>(75.106)</td><td>(475.921)</td><td>(595.504)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPO</td><td>557.019</td><td>420.564</td><td>562.355</td><td>21.521</td></tr>
<tr><td style="text-align:left"></td><td>(564.393)</td><td>(442.267)</td><td>(792.374)</td><td>(843.413)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPS</td><td>-79.626<sup>**</sup></td><td>-49.007</td><td>-169.043<sup>**</sup></td><td>-101.945<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(37.238)</td><td>(72.117)</td><td>(72.900)</td><td>(57.215)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPT</td><td>16.900</td><td>13.797</td><td>24.436</td><td>-95.389</td></tr>
<tr><td style="text-align:left"></td><td>(36.360)</td><td>(74.004)</td><td>(66.902)</td><td>(66.551)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCPW</td><td>74.694</td><td>-43.464</td><td>-33.784</td><td>-65.033</td></tr>
<tr><td style="text-align:left"></td><td>(618.985)</td><td>(91.376)</td><td>(843.195)</td><td>(84.829)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCQT</td><td>-46.360</td><td>280.902<sup>**</sup></td><td>7.193</td><td>57.583</td></tr>
<tr><td style="text-align:left"></td><td>(357.202)</td><td>(116.226)</td><td>(484.400)</td><td>(602.829)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRB</td><td>-52.986<sup>*</sup></td><td>-105.778<sup>*</sup></td><td>-42.203</td><td>-72.458</td></tr>
<tr><td style="text-align:left"></td><td>(31.833)</td><td>(62.009)</td><td>(60.902)</td><td>(65.045)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRL</td><td>-28.067</td><td>-283.973</td><td>-34.849</td><td>-31.395</td></tr>
<tr><td style="text-align:left"></td><td>(250.134)</td><td>(433.156)</td><td>(389.233)</td><td>(421.763)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRM</td><td>270.326</td><td>81.632</td><td>315.470</td><td>313.646</td></tr>
<tr><td style="text-align:left"></td><td>(353.953)</td><td>(86.895)</td><td>(477.315)</td><td>(596.181)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCRP</td><td>13.673</td><td>-112.791</td><td>102.928</td><td>-201.127</td></tr>
<tr><td style="text-align:left"></td><td>(159.498)</td><td>(306.782)</td><td>(339.864)</td><td>(268.429)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSD</td><td>114.359</td><td>-30.955</td><td>275.747</td><td>125.145</td></tr>
<tr><td style="text-align:left"></td><td>(353.616)</td><td>(614.614)</td><td>(672.223)</td><td>(422.029)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSM</td><td>224.636</td><td></td><td>213.711</td><td>236.203</td></tr>
<tr><td style="text-align:left"></td><td>(354.105)</td><td></td><td>(478.153)</td><td>(596.296)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSP</td><td>101.492<sup>***</sup></td><td>200.354<sup>***</sup></td><td>239.873<sup>***</sup></td><td>219.560<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(30.031)</td><td>(54.892)</td><td>(58.166)</td><td>(50.254)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCSW</td><td>18.565</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(499.758)</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCTZ</td><td>99.986</td><td></td><td>34.613</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(618.404)</td><td></td><td>(841.282)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCUL</td><td>-151.009</td><td>-188.470</td><td>-281.090</td><td>-182.239</td></tr>
<tr><td style="text-align:left"></td><td>(168.364)</td><td>(278.393)</td><td>(303.982)</td><td>(269.043)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCUP</td><td>99.238</td><td>150.960</td><td></td><td>66.382</td></tr>
<tr><td style="text-align:left"></td><td>(502.203)</td><td>(634.339)</td><td></td><td>(605.787)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCUT</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVD</td><td>-233.642</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(612.479)</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVP</td><td>-305.184</td><td>-403.143</td><td>-309.525</td><td>-276.231</td></tr>
<tr><td style="text-align:left"></td><td>(353.540)</td><td>(612.194)</td><td>(476.242)</td><td>(595.565)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVR</td><td>-126.133</td><td>-159.098</td><td>-82.626</td><td>-50.463</td></tr>
<tr><td style="text-align:left"></td><td>(355.807)</td><td>(629.441)</td><td>(482.895)</td><td>(595.361)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCVS</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCWB</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCWK</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCWY</td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">branchuseCZE</td><td></td><td>65.649</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(79.259)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-78.715</td><td>-91.520</td><td>-101.087</td><td>-235.589</td></tr>
<tr><td style="text-align:left"></td><td>(178.235)</td><td>(335.049)</td><td>(348.816)</td><td>(296.646)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>28,197</td><td>11,275</td><td>13,201</td><td>14,806</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.062</td><td>0.080</td><td>0.058</td><td>0.071</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.058</td><td>0.071</td><td>0.049</td><td>0.063</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>499.170 (df = 28070)</td><td>610.970 (df = 11157)</td><td>671.184 (df = 13083)</td><td>594.650 (df = 14688)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>14.677<sup>***</sup> (df = 126; 28070)</td><td>8.332<sup>***</sup> (df = 117; 11157)</td><td>6.840<sup>***</sup> (df = 117; 13083)</td><td>9.546<sup>***</sup> (df = 117; 14688)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


#### TABLE 7 - PRICE SENSITIVITIES FOR FEMALE AND LOW-INCOME CLIENTS


```r
gi_median <- round(median(stata_data$grossincome, na.rm = T),2)
s_gi_median <- round(median(stata_data$sales_grossincome, na.rm = T),2)

stata_data <- stata_data %>% mutate(grossincomecat2 = ifelse(grossincome<gi_median,1,0))
stata_data <- stata_data %>% mutate(sales_grossincomecat2 = ifelse(sales_grossincome<s_gi_median,1,0))
                                    
# * EXTENSIVE
# dprobit applied offer4 low med waved2 waved3 if (female==1 & normrate_less==1), cluster(branchuse)
# estimates store m1, title((1))
# sum offer4 applied if e(sample)

reg7_1 <- lm(applied ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, female==1, normrate_less==1))

# dprobit applied offer4 low med waved2 waved3 if (grossincomecat2==1 & normrate_less==1), cluster(branchuse)
# estimates store m2, title((2))
# sum offer4 applied if e(sample)

reg7_2 <- lm(applied ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less==1, grossincomecat2==1))

# dprobit applied offer4 low med waved2 waved3 if (grossincomecat2==1 & female==1 & normrate_less==1), cluster(branchuse)
# estimates store m3, title((3))
# sum offer4 applied if e(sample)

reg7_3 <- lm(applied ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less==1, grossincomecat2==1, female==1))

# *UNCONDITIONAL LOAN SIZE
# regress loansize offer4 low med waved2 waved3 if (female==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m4, title((4))

reg7_4 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less==1, offer4==final4, female==1, grossincomecat2==1))

# regress loansize offer4 low med waved2 waved3 if (grossincomecat2==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m5, title((5))

reg7_5 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less==1, offer4==final4, grossincomecat2==1))

# regress loansize offer4 low med waved2 waved3 if (grossincomecat2==1 & female==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m6, title((6))

reg7_6 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less==1, offer4==final4, grossincomecat2==1, female==1))

# *CONDITIONAL LOAN SIZE
# regress loansize offer4 low med waved2 waved3 if (female==1 & tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m7, title((7))

reg7_7 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, tookup==1, normrate_less==1, offer4==final4, female==1))

# regress loansize offer4 low med waved2 waved3 if (sales_grossincomecat2==1 & tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m8, title((8))

reg7_8 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less==1, offer4==final4, sales_grossincomecat2==1, tookup==1))

# regress loansize offer4 low med waved2 waved3 if (sales_grossincomecat2==1 & female==1 & tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m9, title((9))

reg7_9 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less==1, offer4==final4, sales_grossincomecat2==1, female==1, tookup==1))


stargazer(reg7_1, reg7_2, reg7_3, reg7_4, reg7_5, reg7_6, reg7_7, reg7_8, reg7_9, type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="9"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="9" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="3">applied</td><td colspan="6">loansize</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td><td>(7)</td><td>(8)</td><td>(9)</td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-0.003<sup>***</sup></td><td>-0.003<sup>***</sup></td><td>-0.003<sup>***</sup></td><td>-5.147<sup>***</sup></td><td>-3.989<sup>***</sup></td><td>-5.147<sup>***</sup></td><td>-29.242</td><td>-28.934<sup>***</sup></td><td>-22.968<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.001)</td><td>(0.001)</td><td>(1.817)</td><td>(1.313)</td><td>(1.817)</td><td>(17.841)</td><td>(9.413)</td><td>(12.440)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">low</td><td>0.109<sup>***</sup></td><td>0.116<sup>***</sup></td><td>0.109<sup>***</sup></td><td>189.530<sup>***</sup></td><td>201.695<sup>***</sup></td><td>189.530<sup>***</sup></td><td>681.932<sup>***</sup></td><td>492.496<sup>***</sup></td><td>499.001<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td>(0.006)</td><td>(0.009)</td><td>(15.058)</td><td>(10.878)</td><td>(15.058)</td><td>(98.960)</td><td>(55.154)</td><td>(70.284)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">med</td><td>0.112<sup>***</sup></td><td>0.110<sup>***</sup></td><td>0.108<sup>***</sup></td><td>153.095<sup>***</sup></td><td>144.109<sup>***</sup></td><td>153.095<sup>***</sup></td><td>267.171<sup>***</sup></td><td>259.034<sup>***</sup></td><td>265.167<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td>(0.006)</td><td>(0.009)</td><td>(15.597)</td><td>(11.276)</td><td>(15.597)</td><td>(102.204)</td><td>(55.397)</td><td>(71.897)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved2</td><td>-0.006</td><td>0.005</td><td>0.002</td><td>6.770</td><td>11.903<sup>*</sup></td><td>6.770</td><td>121.835</td><td>49.490</td><td>102.636</td></tr>
<tr><td style="text-align:left"></td><td>(0.007)</td><td>(0.004)</td><td>(0.005)</td><td>(9.571)</td><td>(6.730)</td><td>(9.571)</td><td>(129.136)</td><td>(69.143)</td><td>(90.732)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved3</td><td>-0.009</td><td></td><td></td><td></td><td></td><td></td><td>281.650<sup>**</sup></td><td>219.968<sup>***</sup></td><td>288.434<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.007)</td><td></td><td></td><td></td><td></td><td></td><td>(124.292)</td><td>(66.760)</td><td>(88.077)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.097<sup>***</sup></td><td>0.084<sup>***</sup></td><td>0.088<sup>***</sup></td><td>86.515<sup>***</sup></td><td>76.989<sup>***</sup></td><td>86.515<sup>***</sup></td><td>1,232.452<sup>***</sup></td><td>897.648<sup>***</sup></td><td>762.638<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.009)</td><td>(0.007)</td><td>(0.010)</td><td>(15.992)</td><td>(11.631)</td><td>(15.992)</td><td>(180.954)</td><td>(95.100)</td><td>(125.431)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>25,323</td><td>24,440</td><td>11,709</td><td>6,773</td><td>14,181</td><td>6,773</td><td>1,132</td><td>1,188</td><td>606</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.030</td><td>0.029</td><td>0.027</td><td>0.041</td><td>0.039</td><td>0.041</td><td>0.061</td><td>0.109</td><td>0.121</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.029</td><td>0.029</td><td>0.026</td><td>0.040</td><td>0.038</td><td>0.040</td><td>0.056</td><td>0.105</td><td>0.114</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.278 (df = 25317)</td><td>0.269 (df = 24435)</td><td>0.271 (df = 11704)</td><td>351.486 (df = 6768)</td><td>367.546 (df = 14176)</td><td>351.486 (df = 6768)</td><td>1,283.396 (df = 1126)</td><td>711.264 (df = 1182)</td><td>663.220 (df = 600)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>154.751<sup>***</sup> (df = 5; 25317)</td><td>182.274<sup>***</sup> (df = 4; 24435)</td><td>79.870<sup>***</sup> (df = 4; 11704)</td><td>72.286<sup>***</sup> (df = 4; 6768)</td><td>142.247<sup>***</sup> (df = 4; 14176)</td><td>72.286<sup>***</sup> (df = 4; 6768)</td><td>14.519<sup>***</sup> (df = 5; 1126)</td><td>28.800<sup>***</sup> (df = 5; 1182)</td><td>16.553<sup>***</sup> (df = 5; 600)</td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="9" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


#### TABLE 8A - MATURITY ELASTICITY FIRST-STAGE: THE POWER OF PURE SUGGESTION


```r
# * 1ST-STAGE: Maturity chosen on maturity suggested in mailer
# ** linear IV
# xi: regress term offer4 final4 yearlong termshown low waved2 lnlastamount if (wave>1 & (risk=="LOW" | risk=="MEDIUM") & onetermshown==1 & normrate_less==1 & tookup==1), cluster(branchuse)
# estimates store m1, title((1))

reg8a_1 <- lm(term ~ offer4 + final4 +  yearlong + termshown + low + waved2 + lnlastamount, data=filter(stata_data, wave>1, (risk=="LOW"|risk=="MEDIUM"), onetermshown==1, normrate_less==1, tookup==1))

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


stargazer(reg8a_1, type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>term</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-0.062</td></tr>
<tr><td style="text-align:left"></td><td>(0.149)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">final4</td><td>-0.056</td></tr>
<tr><td style="text-align:left"></td><td>(0.147)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">yearlong</td><td>-0.294</td></tr>
<tr><td style="text-align:left"></td><td>(0.274)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">termshown</td><td>0.131<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.041)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">low</td><td>0.421</td></tr>
<tr><td style="text-align:left"></td><td>(0.324)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">waved2</td><td>-0.992<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.288)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">lnlastamount</td><td>0.818<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.220)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.090</td></tr>
<tr><td style="text-align:left"></td><td>(1.720)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>506</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.077</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.064</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>3.039 (df = 498)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>5.925<sup>***</sup> (df = 7; 498)</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


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

