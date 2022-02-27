# 0.  Data Wrangling ------------------------------------------------------
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("data.table")) install.packages("data.table")
library(tidyverse, data.table)

# Check Data 
head(dat)

# Renaming Columns and Adding Columns
data1 <- dat %>% 
  mutate(bonus = sum(remhextra,outrasprest,subsidios),
         wage = sum(bonus, rembase),
         lwage = log(wage),
         lbase = log(rembase),
         lbonus = log(bonus),
         hours = sum(hnormais, hextra),
         salesK = vendas_euro/1000,
         salespc = salesK/pessoas,
         gender = case_when(
           sexo == 1 ~ 1, # male
           sexo == 2 ~ 0), # female
         firmS = case_when(
           pessoas > 9 & pessoas < 20 ~ 1,
           pessoas > 19 ~ 0,
           pessoas < 10 ~ 0),
         firmM = case_when(
           pessoas > 19 & pessoas < 100 ~ 1,
           pessoas < 20 ~ 0,
           pessoas > 99 ~ 0),
         firmB = case_when(
           pessoas > 99 & pessoas < 500 ~ 1,
           pessoas < 100 ~ 0,
           pessoas > 499 ~ 0),
         firmL = case_when(
           pessoas > 499 ~ 1,
           pessoas < 500 ~ 0),
         portuguese = case_when(
           nacion < 101 ~ 1,
           nacion > 100 ~ 0,
           is.na(nacion) ~1),
         primary = case_when(
           sect_comp > 0 & sect_comp <= 3 ~ 1, 
           sect_comp > 3 ~ 0),
         manufL = case_when(
           sect_comp >= 4 & sect_comp <= 8 ~ 1,
           sect_comp > 8 ~ 0,
           sect_comp < 4 ~ 0),
         manufM = case_when(
           sect_comp >= 9 & sect_comp <= 14 ~ 1,
           sect_comp > 14 ~ 0,
           sect_comp < 9 ~ 0),
         manufH = case_when(
           sect_comp >= 15 & sect_comp <= 18 ~ 1,
           sect_comp > 18 ~ 0,
           sect_comp < 15 ~ 0),
         energy = case_when(
           sect_comp == 19 ~ 1,
           sect_comp != 19 ~ 0),
         construction = case_when(
           sect_comp == 20 ~ 1,
           sect_comp != 20 ~ 0),
         services = case_when(
           sect_comp >= 21 & sect_comp <= 23 ~ 1,
           sect_comp > 23 ~ 0,
           sect_comp < 21 ~ 0),
         finance_realestate = case_when(
           sect_comp >= 24 & sect_comp <= 25 ~ 1,
           sect_comp > 25 ~ 0,
           sect_comp < 24 ~ 0),
         publicadmin = case_when(
           sect_comp == 26 ~ 1,
           sect_comp != 26 ~ 0),
         education = case_when(
           sect_comp == 27 ~ 1,
           sect_comp != 27 ~ 0),
         health = case_when(
           sect_comp == 28 ~ 1,
           sect_comp != 28 ~ 0),
         others = case_when(
           sect_comp >= 29 & sect_comp <= 31 ~ 1,
           sect_comp > 31 ~ 0,
           sect_comp < 29 ~ 0),
         primary_others = case_when(
           sect_comp >= 29 & sect_comp <= 31 ~ 1,
           sect_comp >= 1 & sect_comp <= 3 ~ 1, 
           sect_comp > 3 & sect_comp < 29 ~ 0,
           sect_comp > 31 ~ 0)
         ) %>%
  select(-sexo, -vendas_euro, -pessoas, -hnormais, -hextra, -remhextra,-outrasprest,
         -subsidios, -nacion) %>%
  rename(year = ano, firm = empresa, wage_base = rembase, college = hab_8910) %>%
  arrange(id, year)

# Analyze first observations
head(data)

# Create the first differences
data <- data1 %>% 
  group_by(id,year) %>%
  mutate(dwage = lag(wage, n=1, default = NA))

#  dbonus = , 
# dwage = ,
# dlbase = ,
# dlbonus = ,
# dlwage = 
