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




## R Markdown


```r
library(haven)
library(tidyverse)
```

```
## -- Attaching packages ------------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.1     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   0.8.5
## v tidyr   1.0.2     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
```

```
## -- Conflicts ---------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(stargazer)
```

```
## 
## Please cite as:
```

```
##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
```

```
##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer
```

```r
library(qwraps2)
options(qwraps2_markup = "markdown")
```
rameter was added to the code chunk to prevent printing of the R code that generated the plot.


```r
stata_data <- read_dta("~/GitHub/BA/thesis_code_rep/thesis_code_rep/kz_demandelasts_aer08.dta")

testfunction <- function(stata_data1){
  list("Panel A: Experimental variables" =
         list("Interest rate" = ~ mean_sd(stata_data1$offer4)),
         list("Dynamic repayment incentive" = ~ mean_sd(stata_data1$yearlong)),
         list("Example loan term = 4 months" = ~ mean_sd(stata_data1$termshown4, na_rm = TRUE)),
         list("Example loan term = 6 months" = ~ mean_sd(stata_data1$termshown6, na_rm = TRUE)),
         list("Example loan term = 12 months" = ~ mean_sd(stata_data1$termshown12, na_rm = TRUE)),
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


all_data <- testfunction(stata_data)
summary_all <- summary_table(stata_data, all_data)

applied_data <- stata_data %>% filter(applied == 1)
applied_list <- testfunction(applied_data) 
summary_applied <- summary_table(applied_data, applied_list)

borrowed_data <- stata_data %>% filter(tookup == 1)
borrowed_list <- testfunction(borrowed_data) 
summary_borrowed <- summary_table(borrowed_data, borrowed_list)

maturity_data <- stata_data %>% filter(onetermshown == 1)
maturity_list <- testfunction(maturity_data) 
summary_maturity <- summary_table(maturity_data, maturity_list)

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
