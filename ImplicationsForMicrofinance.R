## ----'check_ps', include=FALSE-----------------------------------------------------------------------------

user.name = ''


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("How do we read a Stata File in R?")


## ----"3_1"-------------------------------------------------------------------------------------------------
# Load the package with the library() command


## ----"3_2"-------------------------------------------------------------------------------------------------
# 
___ <- ___("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")


## ----"3_3"-------------------------------------------------------------------------------------------------
# Use the function head() to show the first six rows of the data frame
head(stata_data)


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("Function: `sample_n()`")


## ----"3_4"-------------------------------------------------------------------------------------------------
sample_n(stata_data,6)


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("Function: `filter()`")


## ----"3_5"-------------------------------------------------------------------------------------------------
___ <- stata_data %>% filter(___)


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run line to answer the quiz above
## answer.quiz("Borrowers")


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("Function: group_by() & summarise()")


## ----"3_6"-------------------------------------------------------------------------------------------------
___ <- ___ %>% ___ %>% ___("Average Interest Rate" = round(mean(offer4, na.rm=TRUE),3),
                                                      "Average Offer Rate" = round(mean(final4, na.rm=TRUE),3),
                                                      "Average Dynamic Repayment Incentive" = round(mean(yearlong, na.rm=TRUE),3),
                                                      "Average Loansize" = round(mean(loansize, na.rm=TRUE),3),
                                                      "Average Maturity" = round(mean(term, na.rm=TRUE),3),
                                                      "Number of Clients" = sum(tookup),
                           
                                                      "Urban Proportion" =              percent(sum(rural)/sum(tookup))
                                                      )

# show the data frame




## ----"3_7"-------------------------------------------------------------------------------------------------
provinces_ <- provinces
provinces <- rbind(provinces_, c("Northern Cape", NA, NA, NA , NA, NA, NA))


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run line to answer the quiz above
## answer.quiz("Urbanization")


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("Function: mutate()")


## ----"3_8"-------------------------------------------------------------------------------------------------
# Replace the question marks with your answer
provinces <- provinces %>% ???(??? = ???)


## ----"3_9",output='htmlwidget', widget='leaflet'-----------------------------------------------------------

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



## ----"4_1"-------------------------------------------------------------------------------------------------
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")
stata_data <- stata_data %>% mutate(itcscore_100 = itcscore/100, appscore_100 = appscore/100)



## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("ggplot()")


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("geom_density()")


## ----"4_2"-------------------------------------------------------------------------------------------------
stata_data %>%
ggplot(aes(x=offer4, group=risk, fill=risk)) + 
  geom_density(adjust = 1.5, alpha = 0.4) +
  scale_fill_discrete(name = "Risk Category", breaks = c("LOW", "MEDIUM", "HIGH")) +
  xlab("Randomized Offer Rate (%)") +
  ylab("Density") +
  geom_vline(xintercept= c(7.75, 9.75, 11.75), col = c("green", "blue", "red"))


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("Mathematic Operations")


## ----"4_3"-------------------------------------------------------------------------------------------------
# create a data frame `values` which contains the min and max interest rate values of each risk category



## ----"4_4"-------------------------------------------------------------------------------------------------
# 



## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("lm() & summary()")


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("Evaluation of a Linear Regression")


## ----"4_5"-------------------------------------------------------------------------------------------------
reg2_1 <- ???(??? ~ dormancy + lntrcount + female + dependants + married + lnage + rural + edhi + itcscore_100 + itczero + appscore_100 + low + med + waved2 + waved3, data=stata_data)

summary(???)



## ----"4_6"-------------------------------------------------------------------------------------------------
reg2_2 <- lm(tookup_afterdead_enforced ~ offer4 + low + med + waved2 + waved3, data=stata_data)
summary(reg2_2)



## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("stat_smooth()")


## ----"4_7",error=FALSE, results='asis'---------------------------------------------------------------------
ggplot(stata_data,aes(x=offer4,y=tookup_afterdead_enforced))+ 
  stat_smooth(method='glm',family=binomial(link='probit'))+
  ylim(min=0, max=1)


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("stargazer()")


## ----"4_8",error=FALSE, results='asis'---------------------------------------------------------------------
reg2_3 <- glm(rejected ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, applied == 1))

stargazer(???)



## ----"5_1"-------------------------------------------------------------------------------------------------
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


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run line to answer the quiz above
## answer.quiz("LPM")


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run line to answer the quiz above
## answer.quiz("Basic Interpretation")


## ----"6_1"-------------------------------------------------------------------------------------------------
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("Probit Regression with glm()")


## ----"6_2",error=FALSE-------------------------------------------------------------------------------------
# Perform a Probit regression and show the regression results



## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run for additional info in the Viewer pane
## info("showreg()")


## ----"6_3",error=FALSE, results='asis'---------------------------------------------------------------------
reg3_2 <- glm(applied ~ offer4 + low + med, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 0))

reg3_3 <- glm(applied ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data=stata_data)

showreg(list("(1)"=reg3_1,"(2)"=reg3_2, "(3)"=reg3_3), coef.transform=c("mfx", "mfx", "mfx"), omit.coef = "(Intercept)", digits=3)


## ----"6_4",error=FALSE-------------------------------------------------------------------------------------
# Enter your code here.


## ----"6_5",error=FALSE, results='asis'---------------------------------------------------------------------
reg3_4 <- glm(tookup_outside_only ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 1))

reg3_5 <- glm(tookup_outside_only ~ offer4 + low + med, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 0))

reg3_6 <- glm(tookup_outside_only ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data = stata_data)

showreg(list("(4)"=reg3_4,"(5)"=reg3_5, "(6)"=reg3_6), coef.transform=c("mfx", "mfx", "mfx"), omit.coef = "(Intercept)",  digits=3)


## ----eval=FALSE--------------------------------------------------------------------------------------------
## # Run line to answer the quiz above
## answer.quiz("Borrowed Elsewhere")


## ----"6_6",error=FALSE, results='asis'---------------------------------------------------------------------
reg3_7 <- glm(??? ~ offer4 + low + med + waved2 + waved3, family = binomial(link = "probit"), data=filter(stata_data, normrate_less == 1))

reg3_8 <- glm(??? ~ normrate_more + low + med + waved2 + waved3, family = binomial(link = "probit"), data = stata_data)

reg3_9 <- glm(??? ~ offer4 + low + med, family = binomial(link = "probit"), data = filter(stata_data, normrate_less == 0))

showreg(list("(7)"=reg3_7,"(8)"=reg3_8, "(9)"=reg3_9), coef.transform=c("mfx", "mfx", "mfx"), omit.coef = "(Intercept)", output = "html", digits=3)



## ----"7_1"-------------------------------------------------------------------------------------------------
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")


## ----"7_2",warning=FALSE, results='asis'-------------------------------------------------------------------
stata_data <- stata_data %>% mutate(grossincomesq = grossincome^2, agesq = age^2, appscoresq = appscore^2, itcscoresq = itcscore^2, sales_netincomesq = sales_netincome^2, sales_grossincomesq = sales_grossincome^2)

reg4_1 <- felm(loansize ~ offer4 |low + med + waved2 + waved3 |0| branchuse,  data = filter(stata_data, offer4==final4, normrate_less==1))

reg4_2 <- felm(loansize ~ offer4 + grossincome + grossincomesq + appscore + appscoresq + itcscore + itcscoresq + trcount + age + dormancy + dependants + agesq | low + med + waved2 + waved3 + female + married  + rural + edhi + appscore0 + itczero + branchuse + province |0| branchuse, data = filter(stata_data, offer4==final4, normrate_less==1))

stargazer(reg4_1, reg4_2, omit = c("grossincome", "grossincomesq", "dormancy", "trcount", "dependants","age", "agesq", "appscore", "appscoresq" ,"itcscore" , "itcscoresq", "trcount"), type="html", header = FALSE, se=list(coef(summary(reg4_1, reg4_2, cluster = c("html")))[, 2]))


## ----"7_3",warning=FALSE, results='asis'-------------------------------------------------------------------
reg4_3 <- lm(loansize ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_4 <- felm(loansize ~ offer4 | low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + as.factor(province) + as.factor(branchuse) |0| branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg4_5 <- censReg(loansize ~ offer4 + low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province+ branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

stargazer(reg4_3, reg4_4, reg4_5, omit = c("low", "med", "waved2" ,"waved3","grossincome" , "grossincomesq" , "dormancy" , "trcount" , "female" , "dependants" , "married" ,"age" , "agesq" , "rural" ,"edhi" , "appscore", "appscoresq" , "appscore0" , "itcscore" , "itcscoresq" , "itczero" , "branchuse" , "province"), type="html", header = FALSE)


## ----"7_4",warning=FALSE, results='asis'-------------------------------------------------------------------
reg4_6 <- felm(lnloansize ~ lnoffer4 | low + med + waved2 + waved3 |0| branchuse, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1))

reg4_7 <- felm(lnloansize ~ lnoffer4 | low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscoresq + appscore0 + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province + branchuse |0| branchuse, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1))

reg4_8 <- felm(lnloansize ~ lnoffer4 | low + med + waved2 + waved3 + sales_grossincome + sales_grossincomesq + sales_netincome + sales_netincomesq + appscore + appscore0 + appscoresq + itcscore + itczero + itcscoresq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + province |0| branchuse, data = filter(stata_data, offer4==final4, tookup==1, normrate_less==1, grossincome!=0))

stargazer(reg4_6, reg4_7, reg4_8, omit = c("low", "med", "waved2" ,"waved3","grossincome" , "grossincomesq" , "dormancy" , "trcount" , "female" , "dependants" , "married" ,"age" , "agesq" , "rural" ,"edhi" , "appscore", "appscoresq" , "appscore0" , "itcscore" , "itcscoresq" , "itczero" , "branchuse" , "province"), type="html", header = FALSE)


## ----"8_1"-------------------------------------------------------------------------------------------------
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")


## ----"8_2",warning=FALSE, results='asis'-------------------------------------------------------------------
reg5_1 <- lm(grossinterest ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1))

reg5_2 <- lm(pstdue_average ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))

reg5_3 <- censReg(pstdue_average ~ offer4 + low + med + waved2 + waved3, data = filter(stata_data, offer4==final4, normrate_less==1, tookup==1))


stargazer(reg5_1, reg5_2, reg5_3, type="html",align=TRUE, dep.var.labels=c("Gross interest revenue","Average past due", "Average past due"), covariate.labels=c("interest rate in pp terms (e.g., 8.2)"),no.space=TRUE)


## ----"8_3",warning=FALSE, results='asis'-------------------------------------------------------------------
reg6_1 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + trcount + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1))

summary(reg6_1)

# edhi=1 -> High education

reg6_2 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + trcount + female + dependants + married + age + agesq + rural + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, edhi==1))

# dormancy<10 -> Borrowed in last 9 months

reg6_3 <- lm(loansize ~ offer4 + low + med + waved2 + waved3 + grossincome + grossincomesq + trcount + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, dormancy<10))

reg6_4 <- lm(loansize ~ offer4+ low + med + waved2 + waved3 + grossincome + grossincomesq + dormancy + female + dependants + married + age + agesq + rural + edhi + appscore + appscoresq + appscore0 + itcscore + itcscoresq + itczero + province + branchuse, data = filter(stata_data, offer4==final4, normrate_less==1, trcount>2))


stargazer(reg6_1, reg6_2, reg6_3, reg6_4, type="html", header = FALSE)


## ----"9_1"-------------------------------------------------------------------------------------------------
stata_data <- read_dta("~/Documents/GitHub/thesis_code_rep/kz_demandelasts_aer08.dta")


## ----"9_2",warning=FALSE, results='asis'-------------------------------------------------------------------
# Enter your code here.

