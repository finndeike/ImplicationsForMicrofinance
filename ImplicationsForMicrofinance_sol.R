## ----"0_1"-------------------------------------------------------------------------------------------------
#library(restorepoint)
# facilitates error detection
#set.restore.point.options(display.restore.point=TRUE)

library(RTutor)
library(yaml)
#library(restorepoint)

setwd("~/Documents/GitHub/thesis_code_rep/")

ps.name = "ImplicationsForMicrofinance"
sol.file = paste0(ps.name, "_sol.Rmd")
libs = c("ggplot2", "haven", "statar", "dplyr", "weights", "lmtest", "lfe", "stargazer", "regtools", "tidyverse", "readr", "knitr", "qwraps2", "AER", "VGAM", "censReg","sf", "ggplot2", "tmap", "tmaptools", "leaflet", "dplyr")

create.ps(sol.file = sol.file, ps.name = ps.name, user.name=NULL, libs = libs, addons = "quiz")

show.ps(ps.name, launch.browser = TRUE, auto.save.code = FALSE, sample.solution = FALSE)
stop()



## ----"1 a) "-----------------------------------------------------------------------------------------------
#< task
# Load the package with the library() command
#>
library(haven)
#< hint
display("library(package_name)")
#>


## ----"1 a) 2"----------------------------------------------------------------------------------------------
#< fill_in
# 
___ <- ___("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")
#>
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")
#< hint
display("Just fill in the command stata_data in the first place holder and read_dta() in the second")
#>


## ----"1 b) "-----------------------------------------------------------------------------------------------
#< task
# Use the function head() to show the first six rows of the data frame
head(stata_data)
#>
#< hint
display("head(...)")
#>


## ----"1 b) 2"----------------------------------------------------------------------------------------------
#< task
sample_n(stata_data,6)
#>
#< hint
display("sample_n(...,6)")
#>


## ----"1 c) 1"----------------------------------------------------------------------------------------------
#< fill_in
___ <- stata_data %>% filter(___)
#>
borrowed <- stata_data %>% filter(tookup == 1)
#< hint
display("Enter the name of the sample data frame in the first empty space, then the condition in the brackets of the function `filter()`")
#>


## ----"1 d) 1"----------------------------------------------------------------------------------------------
#< fill_in
___ <- ___ %>% ___ %>% ___("Average Interest Rate" = round(mean(offer4, na.rm=TRUE),3),
                                                      "Average Offer Rate" = round(mean(final4, na.rm=TRUE),3),
                                                      "Average Dynamic Repayment Incentive" = round(mean(yearlong, na.rm=TRUE),3),
                                                      "Average Loansize" = round(mean(loansize, na.rm=TRUE),3),
                                                      "Average Maturity" = round(mean(term, na.rm=TRUE),3),
                                                      "Number of Clients" = sum(tookup),
                           
                                                      "Urban Proportion" =              percent(sum(rural)/sum(tookup))
                                                      )

# show the data frame

#>
provinces <- borrowed %>% group_by(province) %>% summarise("Average Interest Rate" = round(mean(offer4, na.rm=TRUE),3),
                                                      "Average Offer Rate" = round(mean(final4, na.rm=TRUE),3),
                                                      "Average Dynamic Repayment Incentive" = round(mean(yearlong, na.rm=TRUE),3),
                                                      "Average Loansize" = round(mean(loansize, na.rm=TRUE),3),
                                                      "Average Maturity" = round(mean(term, na.rm=TRUE),3),
                                                      "Number of Clients" = sum(tookup),
                           
                                                      "Rural Proportion" = percent(sum(rural)/sum(tookup))
                                                      )
                                                      
provinces



## ----"1 d) 2"----------------------------------------------------------------------------------------------
#< task_notest
provinces_ <- provinces
provinces <- rbind(provinces_, c("Northern Cape", NA, NA, NA , NA, NA, NA))
#>
#< notest
provinces$province <- c("Eastern Cape", "Free State", "Gauteng", "KwaZulu-Natal", "Limpopo", "Mpumalanga", "North West", "Western Cape", "Northern Cape")
names(provinces)[names(provinces) == "province"] <- "NAME_1"
#>


## ----"1 e) 1"----------------------------------------------------------------------------------------------
#< fill_in
# Replace the question marks with your answer
provinces <- provinces %>% ???(??? = ???)
#>
provinces <- provinces %>% mutate(population = c(6734001, 2928903, 15488137, 11531628, 5852553, 4679786, 4108816, 7005741, 1292786))



## ----"1 f) 1", output="htmlwidget", widget="leaflet"-------------------------------------------------------
#< task_notest

library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
# set options so numbers don't display as scientific notation
options(scipen=999)

# reads the features from the shapefile
mymap <- st_read("~/Documents/GitHub/thesis_code_rep/gadm36_ZAF_shp/gadm36_ZAF_1.shp", stringsAsFactors=FALSE)

# joining the data of the shapefile and our dataframe
map_data <- inner_join(mymap, provinces)

map_data$`Average Interest Rate` <- as.numeric(map_data$`Average Interest Rate`)
map_data$`Average Loansize` <- as.numeric(map_data$`Average Loansize`)

mymap <- tm_shape(map_data) +
  tm_polygons("Average Interest Rate",
              id = "NAME_1",
              style = "quantile",
              palette="Greens",
              popup.vars=c("population", "Number of Clients", "Average Interest Rate",  "Average Offer Rate", "Average Dynamic Repayment Incentive","Average Loansize", "Average Maturity", "Rural Proportion")) +
  tm_bubbles(size = "population", col= "black", id="NAME_1", scale = 2) +
  tm_borders() +
  tm_scale_bar() +
  tm_compass(type="8star", size = 2) +
  tm_credits("Source: GADM database (www.gadm.org)") # not support by view - change how?

tmap_leaflet(mymap)
# tmap mode set to interactive leaflet map

#>


## ----"2 a) 1"----------------------------------------------------------------------------------------------
#< task_notest
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")
stata_data <- stata_data %>% mutate(itcscore_100 = itcscore/100, appscore_100 = appscore/100)

#>



## ----"2 a) 2"----------------------------------------------------------------------------------------------
#< task_notest
stata_data %>%
ggplot(aes(x=offer4, group=risk, fill=risk)) + 
  geom_density(adjust = 1.5, alpha = 0.4) +
  scale_fill_discrete(name = "Risk Category", breaks = c("LOW", "MEDIUM", "HIGH")) +
  xlab("Randomized Offer Rate (%)") +
  ylab("Density") +
  geom_vline(xintercept= c(7.75, 9.75, 11.75), col = c("green", "blue", "red"))
#>



## ----"2 a) 3"----------------------------------------------------------------------------------------------
#< task
# create a data frame `values` which contains the min and max interest rate values of each risk category
#>
values <- stata_data %>% group_by(risk) %>% summarise(min_value=min(offer4), max_value=max(offer4))
#< hint
display("??? <- stata_data %>% group_by(???) %>% summarise(min_value=???(offer4), max_value=???(offer4))")
#>


## ----"2 a) 4"----------------------------------------------------------------------------------------------
#< task
# 
#>
sum(stata_data$normrate_less)/nrow(stata_data)
sum(stata_data$normrate_more)/nrow(stata_data)

#< hint
display("???(stata_data$normrate_???)/???(stata_data)")
#>


## ----"2 a) 5"----------------------------------------------------------------------------------------------
#< fill_in
reg2_1 <- ???(??? ~ dormancy + lntrcount + female + dependants + married + lnage + rural + edhi + itcscore_100 + itczero + appscore_100 + low + med + waved2 + waved3, data=stata_data)

summary(???)
#>
reg2_1 <- lm(offer4 ~ dormancy + lntrcount + female + dependants + married + lnage + rural + edhi + itcscore_100 + itczero + appscore_100 + low + med + waved2 + waved3, data=stata_data)

summary(reg2_1)


## ----"2 a) 6"----------------------------------------------------------------------------------------------
#< fill_in
reg2_2 <- lm(tookup_afterdead_enforced ~ offer4 + low + med + waved2 + waved3, data=stata_data)
summary(reg2_2)
#>
reg2_2 <- glm(tookup_afterdead_enforced ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=stata_data)
summary(reg2_2)


## ----"2 a) 7", error=FALSE, results='asis'-----------------------------------------------------------------
#< task_notest
ggplot(stata_data,aes(x=offer4,y=tookup_afterdead_enforced))+ 
  stat_smooth(method='glm',family=binomial(link='probit'))+
  ylim(min=0, max=1)
#>


## ----"2 a) 8", error=FALSE, results='asis'-----------------------------------------------------------------
#< fill_in
reg2_3 <- glm(rejected ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, applied == 1))

stargazer(???)
#>
reg2_3 <- glm(rejected ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, applied == 1))

stargazer(reg2_1, reg2_2, reg2_3, "branchuse", type="html", header=FALSE)


## ----"3 a) 1"----------------------------------------------------------------------------------------------
#< task_notest
library(gridExtra)

lm_plot <- ggplot(mtcars, aes(x=mpg, y=vs)) + geom_point() + 
  stat_smooth(method="lm", se=TRUE) +
  geom_hline(yintercept=0, col="red") +
  geom_hline(yintercept=1, col="red") +
  ylim(-0.25, 1.5) +
  ggtitle("Linear Probability Model")

probit_plot <- ggplot(mtcars, aes(x=mpg, y=vs)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"(link="probit")), se=TRUE) +
  geom_hline(yintercept=0, col="red") +
  geom_hline(yintercept=1, col="red") +
  ylim(-0.25, 1.5) +
  ggtitle("Probit Model")

logit_plot <- ggplot(mtcars, aes(x=mpg, y=vs)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"(link="logit")), se=TRUE) +
  geom_hline(yintercept=0, col="red") +
  geom_hline(yintercept=1, col="red") +
  ylim(-0.25, 1.5) +
  ggtitle("Logit Model")

grid.arrange(lm_plot, probit_plot, logit_plot, ncol=2)
#>



## ----"4.1 a) 1"--------------------------------------------------------------------------------------------
#< task_notest
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")
#>


## ----"4.1.2 a)", error=FALSE-------------------------------------------------------------------------------
#< task
# Perform a Probit regression and show the regression results
#>
reg3_1 <- glm(applied ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 1))
summary(reg3_1)

#< hint
display("reg3_1 <- glm(??? ~ offer4 + low + med + waved2 + waved3, family = binomial(link = ???), data=filter(stata_data, ???))
summary(???)")
#>


## ----"4.1.2 b)", error=FALSE, results='asis'---------------------------------------------------------------
#< task_notest
reg3_2 <- glm(applied ~ offer4 + low + med, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 0))

reg3_3 <- glm(applied ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data=stata_data)

showreg(list("(1)"=reg3_1,"(2)"=reg3_2, "(3)"=reg3_3), coef.transform=c("mfx", "mfx", "mfx"), omit.coef = "(Intercept)", digits=3)
#>


## ----"4.1.2 c)", error=FALSE-------------------------------------------------------------------------------

reg_offer <- lm(offer4 ~ low + med + waved2 + waved3, data = filter(stata_data, normrate_less == 0))
reg_takeup <- lm(applied ~ low + med + waved2 + waved3, data = filter(stata_data, normrate_less == 0))
test <- showreg(list(reg3_2), coef.transform="mfx", omit.coef = "(Intercept)", digits=5)



## ----"4.1.3 a)", error=FALSE, results='asis'---------------------------------------------------------------
#< task_notest
reg3_4 <- glm(tookup_outside_only ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 1))

reg3_5 <- glm(tookup_outside_only ~ offer4 + low + med, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 0))

reg3_6 <- glm(tookup_outside_only ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data = stata_data)

showreg(list("(4)"=reg3_4,"(5)"=reg3_5, "(6)"=reg3_6), coef.transform=c("mfx", "mfx", "mfx"), omit.coef = "(Intercept)",  digits=3)
#>


## ----"4.1 d)", error=FALSE, results='asis'-----------------------------------------------------------------
#< fill_in
reg3_7 <- glm(??? ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 1))

reg3_8 <- glm(??? ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data = stata_data)

reg3_9 <- glm(??? ~ offer4 + low + med, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 0))

showreg(list("(7)"=reg3_7,"(8)"=reg3_8, "(9)"=reg3_9), coef.transform=c("mfx", "mfx", "mfx"), omit.coef = "(Intercept)", output = "html", digits=3)
#>

reg3_7 <- glm(tookup_afterdead_enforced ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 1))

reg3_8 <- glm(tookup_afterdead_enforced ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data = stata_data)

reg3_9 <- glm(tookup_afterdead_enforced ~ offer4 + low + med, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 0))

showreg(list("(7)"=reg3_7,"(8)"=reg3_8, "(9)"=reg3_9), coef.transform=c("mfx", "mfx", "mfx"), omit.coef = "(Intercept)", output = "html", digits=3)

#< hint
display("The variable is tookup_afterdead_enforced")
#>


## ----"4.2 a) 1"--------------------------------------------------------------------------------------------
#< task_notest
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")
#>


## ----"4.2 b)", warning=FALSE, results='asis'---------------------------------------------------------------
#< task_notest
stata_data <- stata_data %>% mutate(grossincomesq = grossincome^2, agesq = age^2, appscoresq = appscore^2, itcscoresq = itcscore^2, sales_netincomesq = sales_netincome^2, sales_grossincomesq = sales_grossincome^2)

reg4_1 <- felm(loansize ~ offer4 |low + med + waved2 + waved3 |0| branchuse,  data = filter(stata_data, offer4==final4, normrate_less==1))

reg4_2 <- felm(loansize ~ offer4 + grossincome + grossincomesq + appscore + appscoresq + itcscore + itcscoresq + trcount + age + dormancy + dependants + agesq | low + med + waved2 + waved3 + female + married  + rural + edhi + appscore0 + itczero + branchuse + province |0| branchuse, data = filter(stata_data, offer4==final4, normrate_less==1))

stargazer(reg4_1, reg4_2, omit = c("grossincome", "grossincomesq", "dormancy", "trcount", "dependants","age", "agesq", "appscore", "appscoresq" ,"itcscore" , "itcscoresq", "trcount"), type="html", header = FALSE, se=list(coef(summary(reg4_1, reg4_2, cluster = c("html")))[, 2]))
#>


## ----"4.2 c)", warning=FALSE, results='asis'---------------------------------------------------------------
#< task_notest
reg4_3 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_4 <- felm(loansize ~ offer4 | low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + as.factor(province) + as.factor(branchuse) |0| branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_5 <- censReg(loansize ~ offer4 + low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province+ branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

stargazer(reg4_3, reg4_4, reg4_5, omit = c("low", "med", "waved2" ,"waved3","grossincome" , "grossincomesq" , "dormancy" , "trcount" , "female" , "dependants" , "married" ,"age" , "agesq" , "rural" ,"edhi" , "appscore", "appscoresq" , "appscore0" , "itcscore" , "itcscoresq" , "itczero" , "branchuse" , "province"), type="html", header = FALSE)
#>


## ----"4.2 d)", warning=FALSE, results='asis'---------------------------------------------------------------
#< task_notest
reg4_6 <- felm(lnloansize ~ lnoffer4 | low + med + waved2 + waved3 |0| branchuse, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1))

reg4_7 <- felm(lnloansize ~ lnoffer4 | low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province + branchuse |0| branchuse, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1))

reg4_8 <- felm(lnloansize ~ lnoffer4 | low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscore0 + appscoresq + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province |0| branchuse, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1, grossincome!=0))

stargazer(reg4_6, reg4_7, reg4_8, omit = c("low", "med", "waved2" ,"waved3","grossincome" , "grossincomesq" , "dormancy" , "trcount" , "female" , "dependants" , "married" ,"age" , "agesq" , "rural" ,"edhi" , "appscore", "appscoresq" , "appscore0" , "itcscore" , "itcscoresq" , "itczero" , "branchuse" , "province"), type="html", header = FALSE)
#>


## ----"5.1 a) 1"--------------------------------------------------------------------------------------------
#< task_notest
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")
#>


## ----"5.1.1 a)", warning=FALSE, results='asis'-------------------------------------------------------------
#< task_notest
reg5_1 <- lm(grossinterest ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1))

reg5_2 <- lm(pstdue_average ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg5_3 <- censReg(pstdue_average ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))


stargazer(reg5_1, reg5_2, reg5_3, type="html",align=TRUE, dep.var.labels=c("Gross interest revenue","Average past due", "Average past due"), covariate.labels=c("interest rate in pp terms (e.g., 8.2)"),no.space=TRUE)
#>


## ----"5.1.2 a)", warning=FALSE, results='asis'-------------------------------------------------------------
#< task_notest
reg6_1 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1))

summary(reg6_1)

# edhi=1 -> High education

reg6_2 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + trcount + female + dependants + married + age + agesq + rural + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, edhi==1))

# dormancy<10 -> Borrowed in last 9 months

reg6_3 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + trcount + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, dormancy<10))

reg6_4 <- lm(loansize ~ offer4+ low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, trcount>2))


stargazer(reg6_1, reg6_2, reg6_3, reg6_4, type="html", header = FALSE)
#>


## ----"5.2 a) 1"--------------------------------------------------------------------------------------------
#< task_notest
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")
#>


## ----"5.2.1 a)", warning=FALSE, results='asis'-------------------------------------------------------------

stata_data <- stata_data %>% mutate(grossincomecat2 = xtile(grossincome, n=2))
stata_data <- stata_data %>% mutate(sales_grossincomecat2 = xtile(grossincome, n=10))
                                    
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

reg7_4 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, normrate_less==1, offer4==final4, female==1))

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

reg7_8 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, sales_grossincomecat2==1, normrate_less==1, offer4==final4, tookup==1))

# regress loansize offer4 low med waved2 waved3 if (sales_grossincomecat2==1 & female==1 & tookup==1 & normrate_less== 1 & (offer4==final4)), cluster(branchuse)
# sum loansize offer4 if e(sample)
# estimates store m9, title((9))

reg7_9 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data=filter(stata_data, sales_grossincomecat2==1, normrate_less==1, offer4==final4, female==1, tookup==1))


# stargazer(reg7_1, reg7_2, reg7_3, reg7_4, reg7_5, reg7_6, reg7_7, reg7_8, reg7_9, type="html", header = FALSE)



## ---- echo=FALSE-------------------------------------------------------------------------------------------
options(knitr.purl.inline = TRUE)
list.functions.in.file(purl("ImplicationsForMicrofinance_sol.Rmd"))


