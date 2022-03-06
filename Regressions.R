if(!require("devtools")) install.packages("devtools")
if(!require("plm"))install.packages("plm") # panel data regression
if(!require("lfe"))install.packages("lfe") # HDFE regression
if(!require("quantreg"))install.packages("quantreg") # quantile
if(!require("webshot"))install.packages("webshot") # obtaining images from html
library(tidyverse,devtools,plm,lfe,quantreg,webshot)
install_github("strengejacke/strengejacke")
library(sjPlot) # nice outputs
webshot::install_phantomjs()


# Free unused memory
gc()

# Check Data 
head(data)

# 1.  Pooled OLS ------------------------------------------------------

OLS1 <- lm(wage ~ age + age2 + female + highschool + college + portuguese + 
     firmS + firmM + firmB + firmL +
     manufL + manufM + manufH +
     energy + construction + services + finance_realestate + publicadmin + education + health + year,
   data)

tab_model (OLS1, file = "OLS1.html")
webshot("OLS1.html", "OLS1.png")
