---
title: "Credit Elasticities in Less-Developed Economies"
description: "Implications for Microfinance"
author:
  name: "Finn Deike"
  url: https://github.com/finndeike/thesis_code_rep
  affiliation: University of Ulm
output: 
  html_document:
    keep_md: yes
    toc: yes

---



**An Interactive Anaylsis in R**


Welcome to my interactive RTutor Problemset, containing the main results from the paper **Credit Elasticities in Less-Developed Economies: Implications for Microfinance** by Dean S. Karlan (Department of Economics, Yale University) and Jonathan Zinma (Department of Economics, Dartmouth College) published by the American Economic Review in 2008.


# Exercise Content

1. Introduction 

2. Data Overview

3. Theoretical Model

4. Regression Analysis

5. Estimation

6. Conclusion

7. Appendix

8. References



## Introduction


## Data Overview

***
### How do we read a Stata File in R?
`read_dta` from the package `haven` reads a file in Stata version 5-12 binary format into a data frame in R. This funtion only supports Stata formats after 12, but since we have a 5-12 format it can be used.
***


**Exercise 1.1:** Load the data file _kz_demandelasts_aer08.dta_ and store it in the variable `stata_data`. To do so, first load the package `haven` with the `library()` command. Now you can use the command `read_dta()` to load the data file. If you think your answer is right press `check`.


```r
# Not yet solved...
# Press 'edit' to enter your code.

# Load the Package
library(haven)

# Read the datafile
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")
```

After loading in the data, we can now have a deeper look into our data and the according parameters. But first let's have a rough look at our data stored in `stata_data`.

There are a variaty of ways to get an overview on a data set. We will use the R function `head()`to show the first six rows of our data set stored in `stata_data`. An alternative would be to show six random sample rows,  use the command `sample_n(data,#rows)`. This will help us to get familiar with the variables and understand how the data set is structured.

**Exercise 1.2** Show the first six rows of our data file stored in `stata_data`. Are there any inconsistencies? If yes, try `sample_n()` inorder to show six random rows of our data set `stata_data`. Afterwards press the `check` button.


```r
# Not yet solved...
# Press 'edit' to enter your code.

# Show the first five rows
head(stata_data)

# Show six random rows
sample_n(stata_data,6)
```
















