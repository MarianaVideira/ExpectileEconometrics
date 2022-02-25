if(!require("tidyverse")) install.packages("tidyverse")
library("tidyverse", "readr", "haven") 

# 0.  Data Import  --------------------------------------------------------

# Set working directory and file name
dir <- getwd()
  list.files(path = dir)


# *0.1. Using Haven -------------------------------------------------------

filedta <- "marlon_data.dta"
 
# Alternatively 
dat <- read_dta(filedta, encoding = NULL,col_select = NULL, skip = 0,
                n_max = Inf,
                .name_repair = "unique")


# *0.2. Using CSV ---------------------------------------------------------

# filename <- "qp_data.csv"
  
# Data set for wide data and select the observations from 1995-2009
