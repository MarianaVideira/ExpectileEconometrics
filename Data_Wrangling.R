# 0.  Data Wrangling ------------------------------------------------------
if(!require("data.table")) install.packages("data.table")
library(tidyverse, data.table)

# Free unused memory
gc()

# Check Data 
head(dat)

# Summing Rows
dat <- dat %>% 
  rowwise() %>%
  mutate(bonus = sum(c(remhextra,outrasprest,subsidios)),
         wage = sum(c(bonus, rembase)),
         hours = sum(c(hnormais, hextra)),
  )

dat <- dat %>%
  mutate(vendas_euros = case_when(
    vvend >= 0 ~ vvend,
    vn >= 0 ~ vn,
    is.na(vvend)&is.na(vn) ~ 0
  ))

# Renaming Columns and Adding Columns
dat <- dat %>%
  mutate(
    age2 = age*age,
    lwage = log(wage),
    lbase = log(rembase),
    lbonus = log(bonus),
    salesK = vendas_euros/1000,
    salespc = salesK/pemp,
    #female = case_when(
      #sexo == 1 ~ 0, # male
      #sexo == 2 ~ 1), # female
    firmS = case_when(
    pemp > 9 & pemp < 20 ~ 1,
    pemp > 19 ~ 0,
    pemp < 10 ~ 0),
    firmM = case_when(
      pemp > 19 & pemp < 100 ~ 1,
      pemp < 20 ~ 0,
      pemp > 99 ~ 0),
    firmB = case_when(
      pemp > 99 & pemp < 500 ~ 1,
      pemp < 100 ~ 0,
      pemp > 499 ~ 0),
    firmL = case_when(
      pemp > 499 ~ 1,
      pemp < 500 ~ 0),
    portuguese = case_when(
      nacio == 0 ~ 1,
      nacio != 0 ~ 0,
      nacio1019 == "PT" ~ 1,
      nacio1019 != "PT" ~ 0))
    # primary = case_when(
    #   sect_comp > 0 & sect_comp <= 3 ~ 1, 
    #   sect_comp > 3 ~ 0),
    # manufL = case_when(
    #   sect_comp >= 4 & sect_comp <= 8 ~ 1, 
    #   sect_comp > 8 ~ 0,
    #   sect_comp < 4 ~ 0),
    # manufM = case_when(
    #   sect_comp >= 9 & sect_comp <= 14 ~ 1,
    #   sect_comp > 14 ~ 0,
    #   sect_comp < 9 ~ 0),
    # manufH = case_when(
    #   sect_comp >= 15 & sect_comp <= 18 ~ 1,
    #   sect_comp > 18 ~ 0,
    #   sect_comp < 15 ~ 0),
    # energy = case_when(
    #   sect_comp == 19 ~ 1,
    #   sect_comp != 19 ~ 0),
    # construction = case_when(
    #   sect_comp == 20 ~ 1,
    #   sect_comp != 20 ~ 0),
    # services = case_when(
    #   sect_comp >= 21 & sect_comp <= 23 ~ 1,
    #   sect_comp > 23 ~ 0,
    #   sect_comp < 21 ~ 0),
    # finance_realestate = case_when(
    #   sect_comp >= 24 & sect_comp <= 25 ~ 1,
    #   sect_comp > 25 ~ 0,
    #   sect_comp < 24 ~ 0),
    # publicadmin = case_when(
    #   sect_comp == 26 ~ 1,
    #   sect_comp != 26 ~ 0),
    # education = case_when(
    #   sect_comp == 27 ~ 1,
    #   sect_comp != 27 ~ 0),
    # health = case_when(
    #   sect_comp == 28 ~ 1,
    #   sect_comp != 28 ~ 0),
    # others = case_when(
    #   sect_comp >= 29 & sect_comp <= 31 ~ 1,
    #   sect_comp > 31 ~ 0,
    #   sect_comp < 29 ~ 0),
    # primary_others = case_when(
    #   sect_comp >= 29 & sect_comp <= 31 ~ 1,
    #   sect_comp >= 1 & sect_comp <= 3 ~ 1, 
    #   sect_comp > 3 & sect_comp < 29 ~ 0,
    #   sect_comp > 31 ~ 0),
    # ) %>%
  # select(-sexo, -vendas_euro, -pessoas, -hnormais, -hextra, -remhextra,-outrasprest,-subsidios, -nacion) %>%
  # rename(year = ano, firm = empresa, base = rembase, college = hab_8910) %>%
  # arrange(id, year)

dat <- dat %>%
    mutate(
      highschool = case_when(
        habil > 499 & habil < 700 ~ 1,
        habil < 500 | habil > 699 ~ 0),
      college = case_when(
        habil > 699 ~ 1,
        habil < 700 ~ 0))

# Analyze first observations
head(dat)


# Create the first differences
dat <- dat %>% 
  group_by(nss) %>%
  mutate(dwage = lag(wage, n=1, default = NA),
         dbonus = lag(bonus, n=1, default = NA),
         dbase = lag(rembase, n=1, default = NA),
         dlbase = lag(lbase, n=1, default = NA),
         dlbonus = lag(lbonus, n=1, default = NA),
         dlwage = lag(lwage, n=1, default = NA)) %>%
  ungroup()

# Create year as factor (to include as dummies)
dat <- dat %>% rename(year = ano)
dat$year <- factor(dat$year) 
levels(dat$year)

