# 0.  Data Wrangling ------------------------------------------------------
if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# Check Data 
head(dat)

# filter obs year 
dat <- dat %>% 
  filter(ano > 1995)

# Renaming Columns and Adding Columns
data <- dat %>% 
  mutate(bonus = sum(remhextra,outrasprest,subsidios),
         salary = sum(bonus, rembase),
         log_salary = log(salary),
         log_salarybase = log(rembase),
         log_bonus = log(bonus),
         hours = sum(hnormais, hextra),
         sales_thousand_euros = vendas_euro/1000,
         salespc = sales_thousand_euros/pessoas,
         gender = case_when(
           sexo == 1 ~ "male",
           sexo == 2 ~ "female"),
         dummy_gender = case_when(
           sexo == 1 ~ 1,
           sexo == 2 ~ 0,
           TRUE ~ as.numeric(NA)),
         size_small = case_when(
           pessoas > 9 & pessoas < 20 ~ 1,
           pessoas > 19 ~ 0,
           pessoas < 10 ~ 0),
         size_medium = case_when(
           pessoas > 19 & pessoas < 100 ~ 1,
           pessoas < 20 ~ 0,
           pessoas > 99 ~ 0),
         size_big = case_when(
           pessoas > 99 & pessoas < 500 ~ 1,
           pessoas < 100 ~ 0,
           pessoas > 499 ~ 0),
         size_large = case_when(
           pessoas > 499 ~ 1,
           pessoas < 500 ~ 0),
         portuguese = case_when(
           nacion < 101 ~ 1,
           nacion > 100 ~ 0,
           is.na(nacion) ~1),
         primary = case_when(
           sect_comp > 0 & sect_comp <= 3 ~ 1, 
           sect_comp > 3 ~ 0),
         low_manufactoring = case_when(
           sect_comp >= 4 & sect_comp <= 8 ~ 1,
           sect_comp > 8 ~ 0,
           sect_comp < 4 ~ 0),
         medium_manufactoring = case_when(
           sect_comp >= 9 & sect_comp <= 14 ~ 1,
           sect_comp > 14 ~ 0,
           sect_comp < 9 ~ 0),
         high_manufactoring = case_when(
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
         publicadministration = case_when(
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
  rename(year = ano, firm = empresa, salarybase = rembase, college = hab_8910) %>%
  arrange(id, year)

# Analyze first observations
head(data)

