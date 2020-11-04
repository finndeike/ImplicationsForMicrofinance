#code_rep_stata

install.packages("haven")
library("haven")

stata_data <- read_dta("kz_demandelasts_aer08.dta")

stata_data
