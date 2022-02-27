if(!require("tidyverse")) install.packages("tidyverse")
library(c("tidyverse", "readr","dplyr"))
library(haven)

# 0.  Data Import  --------------------------------------------------------

# Set working directory and file name
dir <- getwd()
  list.files(path = dir)


# *0.1. Using Haven -------------------------------------------------------

filedta <- "marlon_data.dta"
 
# Import 
dat <- read_dta(filedta, encoding = NULL,col_select = NULL, skip = 0,
                n_max = Inf,
                .name_repair = "unique")

# filter obs year & clear duplicates
dat <- dat %>% 
  filter(ano > 2001) %>%
  distinct()


# *0.2. Using CSV ---------------------------------------------------------

# filename <- "qp_data.csv"
  
# Data set for wide data and select the observations from 1995-2009
