if(!require("tidyverse")) install.packages("tidyverse")
if(!require("easypackages")) install.packages("easypackages") # easier to load multiple packages
library(tidyverse,readr,dplyr)
library(haven)

# 0.  Data Import  --------------------------------------------------------
# Free unused memory
gc()

# Set working directory and file name
dir <- getwd()
list.files(path = dir)


# *0.1. Using Haven -------------------------------------------------------

filedta <- "QP2002-19_small.dta"
 
# Import 
dat <- read_dta(filedta, encoding = NULL,col_select = NULL, skip = 0,
                n_max = Inf,
                .name_repair = "unique")

# filter obs year & clear duplicates
# dat <- dat %>% distinct(ano, id,.keep_all=TRUE)
#   
# dat <- dat %>% filter(ano > 2001)


# Use sample of data (10,000 ids)
# nID <- length(unique(dat$id))
# p = 10000/nrow(dat)
# set.seed(5)
# inSampleID <- sample(unique(dat$id), round(nID * p), replace=FALSE)
# dat <- dat[dat$id %in% inSampleID, ] 



