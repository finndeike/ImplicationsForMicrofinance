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

reg2_1 <- lm(offer4 ~ dormancy + lntrcount + female + dependants + married + lnage + rural + edhi + lnitcscore + itczero + lnappscore + low + med + waved2 + waved3, data=stata_data)

# Ergebnis nicht komplett richtig, woran liegt es? Falsch regressiert?

# teilweise inkorrekte Werte, was ist mit itczero(Wert deutlich zu groß) ?

reg2_2 <- glm(tookup_afterdead_enforced ~ offer4, family = binomial(link = "probit"), data=stata_data)

stata_data_reg3 <- stata_data %>% filter(applied == 1)
reg2_3 <- glm(rejected ~ offer4, family = binomial(link = "probit"), data=stata_data_reg3)

stargazer(reg2_1, reg2_2, reg2_3, type="html", header=FALSE)
```


<table style="text-align:center"><tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>offer4</td><td>tookup_afterdead_enforced</td><td>rejected</td></tr>
<tr><td style="text-align:left"></td><td><em>OLS</em></td><td><em>probit</em></td><td><em>probit</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">dormancy</td><td>0.002</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.002)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">lntrcount</td><td>0.001</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.015)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">female</td><td>0.027</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.024)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">dependants</td><td>-0.001</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.007)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">married</td><td>0.030</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.024)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">lnage</td><td>0.013</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.052)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">rural</td><td>0.011</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.031)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">edhi</td><td>0.007</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.023)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">lnitcscore</td><td>0.035</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.088)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">itczero</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">lnappscore</td><td>-0.034</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.038)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">low</td><td>-2.480<sup>***</sup></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.041)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">med</td><td>-1.060<sup>***</sup></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.043)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved2</td><td>-0.302<sup>***</sup></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.045)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">waved3</td><td>-0.316<sup>***</sup></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.043)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">offer4</td><td></td><td>-0.041<sup>***</sup></td><td>0.043<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.003)</td><td>(0.010)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>8.495<sup>***</sup></td><td>-0.724<sup>***</sup></td><td>-1.389<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.569)</td><td>(0.022)</td><td>(0.077)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>47,033</td><td>53,810</td><td>4,540</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.111</td><td></td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.110</td><td></td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td>-22,361.210</td><td>-1,859.831</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td></td><td>44,726.420</td><td>3,723.661</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>2.320 (df = 47018)</td><td></td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>417.757<sup>***</sup> (df = 14; 47018)</td><td></td><td></td></tr>
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

reg3_1 <- lm(applied ~ offer4, data=filter(stata_data, normrate_less == 1))

reg3_2 <- lm(applied ~ normrate_more, data=stata_data)

reg3_3 <- lm(applied ~ offer4, data=filter(stata_data, normrate_less == 0))

reg3_4 <- lm(tookup_outside_only ~ offer4, data = filter(stata_data, normrate_less == 1))

reg3_5 <- lm(tookup_outside_only ~ normrate_more, data = stata_data)

reg3_6 <- lm(tookup_outside_only ~ offer4, data = filter(stata_data, normrate_less == 0))

reg3_7 <- lm(tookup_afterdead_enforced ~ offer4, data=filter(stata_data, normrate_less == 1))

reg3_8 <- lm(tookup_afterdead_enforced ~ normrate_more, data = stata_data)

reg3_9 <- lm(tookup_afterdead_enforced ~ offer4, data = filter(stata_data, normrate_less == 0))

stargazer(reg3_1, reg3_2, reg3_3, reg3_4, reg3_5, reg3_6, reg3_7, reg3_8, reg3_9, type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="9"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="9" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="3">applied</td><td colspan="3">tookup_outside_only</td><td colspan="3">tookup_afterdead_enforced</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td><td>(7)</td><td>(8)</td><td>(9)</td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-0.009<sup>***</sup></td><td></td><td>-0.025<sup>***</sup></td><td>-0.001</td><td></td><td>-0.012</td><td>-0.010<sup>***</sup></td><td></td><td>-0.047<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.0005)</td><td></td><td>(0.006)</td><td>(0.001)</td><td></td><td>(0.011)</td><td>(0.001)</td><td></td><td>(0.009)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">normrate_more</td><td></td><td>-0.018</td><td></td><td></td><td>0.057<sup>***</sup></td><td></td><td></td><td>0.034<sup>**</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.011)</td><td></td><td></td><td>(0.017)</td><td></td><td></td><td>(0.014)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.154<sup>***</sup></td><td>0.085<sup>***</sup></td><td>0.392<sup>***</sup></td><td>0.228<sup>***</sup></td><td>0.223<sup>***</sup></td><td>0.432<sup>***</sup></td><td>0.226<sup>***</sup></td><td>0.147<sup>***</sup></td><td>0.781<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.004)</td><td>(0.001)</td><td>(0.076)</td><td>(0.006)</td><td>(0.002)</td><td>(0.138)</td><td>(0.005)</td><td>(0.002)</td><td>(0.116)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>53,178</td><td>53,810</td><td>632</td><td>53,178</td><td>53,810</td><td>632</td><td>53,178</td><td>53,810</td><td>632</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.006</td><td>0.00005</td><td>0.029</td><td>0.00001</td><td>0.0002</td><td>0.002</td><td>0.005</td><td>0.0001</td><td>0.041</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.006</td><td>0.00003</td><td>0.027</td><td>-0.00001</td><td>0.0002</td><td>0.0004</td><td>0.005</td><td>0.0001</td><td>0.040</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>0.277 (df = 53176)</td><td>0.278 (df = 53808)</td><td>0.246 (df = 630)</td><td>0.416 (df = 53176)</td><td>0.417 (df = 53808)</td><td>0.449 (df = 630)</td><td>0.353 (df = 53176)</td><td>0.354 (df = 53808)</td><td>0.377 (df = 630)</td></tr>
<tr><td style="text-align:left">F Statistic</td><td>304.878<sup>***</sup> (df = 1; 53176)</td><td>2.657 (df = 1; 53808)</td><td>18.750<sup>***</sup> (df = 1; 630)</td><td>0.534 (df = 1; 53176)</td><td>11.620<sup>***</sup> (df = 1; 53808)</td><td>1.222 (df = 1; 630)</td><td>249.115<sup>***</sup> (df = 1; 53176)</td><td>5.628<sup>**</sup> (df = 1; 53808)</td><td>27.214<sup>***</sup> (df = 1; 630)</td></tr>
<tr><td colspan="10" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="9" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

#### TABLE 4 - PRICE SENSITIVITIES OF LOAN SIZE


```r
# xtile sales_netincomecat= sales_netincome, n(10)
# replace sales_netincome=sales_netincome/1000
# label var sales_netincome "net income, in 000s"
# 
# for var age grossincome sales_grossincome sales_netincome appscore itcscore: capture drop Xsq \ gen Xsq=X^2
# 
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

# Dataset -> r(interest rate) = rc (contract rate) -> offer4 = final4, normrate_less == 1
# Conditional on borrowing? -> tookup
# Additional controls? + Branch fixed effects? -> grossincome != 0
# Additional controls? -> ???
# Branch fixed effects? -> ???

reg4_1 <- lm(loansize ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1))

reg4_2 <- lm(loansize ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1, grossincome!=0))

reg4_3 <- lm(loansize ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_4 <- lm(loansize ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_5 <- tobit(loansize ~ offer4, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_6 <- lm(lnloansize ~ lnoffer4, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1))

reg4_7 <- lm(lnloansize ~ lnoffer4, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1, grossincome!=0))

reg4_8 <- tobit(lnloansize ~ lnoffer4, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1, grossincome!=0))


stargazer(reg4_1, reg4_2, reg4_3, reg4_4, reg4_5, reg4_6, reg4_7, reg4_8, type="html", header = FALSE)
```


<table style="text-align:center"><tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="8"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="8" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="5">loansize</td><td colspan="3">lnloansize</td></tr>
<tr><td style="text-align:left"></td><td colspan="4"><em>OLS</em></td><td><em>Tobit</em></td><td colspan="2"><em>OLS</em></td><td><em>Tobit</em></td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td><td>(7)</td><td>(8)</td></tr>
<tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">offer4</td><td>-15.923<sup>***</sup></td><td>-15.857<sup>***</sup></td><td>-72.792<sup>***</sup></td><td>-72.792<sup>***</sup></td><td>-72.792<sup>***</sup></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.168)</td><td>(1.232)</td><td>(11.181)</td><td>(11.181)</td><td>(11.176)</td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">lnoffer4</td><td></td><td></td><td></td><td></td><td></td><td>-0.242<sup>***</sup></td><td>-0.268<sup>***</sup></td><td>-0.268<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td>(0.043)</td><td>(0.045)</td><td>(0.045)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>230.979<sup>***</sup></td><td>229.812<sup>***</sup></td><td>1,951.646<sup>***</sup></td><td>1,951.646<sup>***</sup></td><td>1,951.646<sup>***</sup></td><td>7.444<sup>***</sup></td><td>7.507<sup>***</sup></td><td>7.507<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(9.592)</td><td>(10.230)</td><td>(84.673)</td><td>(84.673)</td><td>(84.636)</td><td>(0.083)</td><td>(0.089)</td><td>(0.089)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
<tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>31,231</td><td>28,197</td><td>2,325</td><td>2,325</td><td>2,325</td><td>2,325</td><td>2,035</td><td>2,035</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.006</td><td>0.006</td><td>0.018</td><td>0.018</td><td></td><td>0.014</td><td>0.017</td><td></td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.006</td><td>0.006</td><td>0.017</td><td>0.017</td><td></td><td>0.013</td><td>0.016</td><td></td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td></td><td></td><td></td><td></td><td>-19,936.500</td><td></td><td></td><td>-2,234.956</td></tr>
<tr><td style="text-align:left">Residual Std. Error</td><td>513.345 (df = 31229)</td><td>512.703 (df = 28195)</td><td>1,282.195 (df = 2323)</td><td>1,282.195 (df = 2323)</td><td></td><td>0.730 (df = 2323)</td><td>0.726 (df = 2033)</td><td></td></tr>
<tr><td style="text-align:left">F Statistic</td><td>185.935<sup>***</sup> (df = 1; 31229)</td><td>165.751<sup>***</sup> (df = 1; 28195)</td><td>42.388<sup>***</sup> (df = 1; 2323)</td><td>42.388<sup>***</sup> (df = 1; 2323)</td><td></td><td>32.427<sup>***</sup> (df = 1; 2323)</td><td>34.928<sup>***</sup> (df = 1; 2033)</td><td></td></tr>
<tr><td style="text-align:left">Wald Test (df = 1)</td><td></td><td></td><td></td><td></td><td>42.424<sup>***</sup></td><td></td><td></td><td>34.962<sup>***</sup></td></tr>
<tr><td colspan="9" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="8" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
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

