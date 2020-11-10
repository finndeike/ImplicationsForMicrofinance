#code_rep_stata

install.packages("haven")
library(haven)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(stargazer)
install.packages("qwraps2")
library(qwraps2)


stata_data <- read_dta("kz_demandelasts_aer08.dta")

stata_data


lm(formula=offer4 ~ yearlong, data=stata_data )

applied_panelA <- stata_data %>% filter(applied == 1)

round(mean(stata_data$offer4),3)
sd(stata_data$offer4)

test <- mean_sd(stata_data$offer4, denote_sd = "paren")
test

table1 <-
  list("Test1" = 
    list("mean" = mean(stata_data$offer4),
         "sd" = sd(stata_data$offer4)),
    "Test2" =
    list("mean" = mean(stata_data$yearlong),
         "sd" = sd(stata_data$yearlong))
  )


statistics_table <- summary_table(stata_data, table1)

test232 <- 
  list("Interest rate" =
         list("mean (sd)" = ~ mean_sd(stata_data$offer4)),
       "Rate" =
         list("mean (sd)" = ~ mean_sd(stata_data$yearlong))
  )

summary <- summary_table(stata_data, test232)
       
       
       