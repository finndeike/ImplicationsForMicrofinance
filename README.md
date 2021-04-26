This package constitutes an interactive R problem set based on the RTutor package (https://github.com/skranz/RTutor).

Welcome! This is a RTutor Problem Set, containing the main results from the paper “**Credit Elasticities in Less-Developed Economies: Implications for Microfinance** by Dean S. Karlan (Department of Economics, Yale University) and Jonathan Zinma (Department of Economics, Dartmouth College) published in the American Economic Review, 98(3) in 2008. They test the assumption of price inelastic demand using randomized trials conducted by a consumer lender in South Africa. 

The paper can be downloaded here: https://www.aeaweb.org/articles?id=10.1257/aer.98.3.1040


## 1. Installation

RTutor and this package is hosted on Github. To install everything, run the following code in your R console.
```s
if (!require(devtools))
  install.packages("devtools")
source_gist("gist.github.com/skranz/fad6062e5462c9d0efe4")
install.rtutor(update.github=TRUE)

devtools::install_github("finndeike/ImplicationsForMicrofinance", upgrade_dependencies=FALSE)
```

## 2. Show and work on the problem set
To start the problem set first create a working directory in which files like the data sets and your solution will be stored. Then adapt and run the following code.
```s
library(ImplicationsForMicrofinance)

# Adapt your working directory to an existing folder
setwd("C:/problemsets/ImplicationsForMicrofinance")
# Adapt your user name
run.ps(user.name="Jon Doe", package="ImplicationsForMicrofinance",
       load.sav=TRUE, sample.solution=FALSE)
```
If everything works fine, a browser window should open, in which you can start exploring the problem set.
