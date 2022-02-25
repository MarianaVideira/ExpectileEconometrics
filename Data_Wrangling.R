# 0.  Data Wrangling ------------------------------------------------------

# Check Data 
head(dat)

# filter obs year 
dat <- dat %>% 
  filter(ano > 1995)

# Renaming Columns and Adding Columns
data <- dat %>% 
  mutate(bonus = sum(remhextra,outrasprest,subsidios), 
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
           pessoas < 19 ~ 0),
         size_medium = case_when(
           pessoas > 19 & pessoas < 100 ~ 1,
           pessoas < 20 & pessoas > 99 ~ 0),
         size_big = case_when(
           pessoas > 99 & pessoas < 500 ~ 1,
           pessoas < 100 & pessoas > 499 ~ 0),
         size_large = case_when(
           pessoas > 499 ~ 1,
           pessoas < 500 ~ 0)) %>%
  select(-sexo, -vendas_euro, -pessoas, -remhextra,-outrasprest,-subsidios) %>%
  rename(year = ano, firm = empresa, salarybase = rembase, college = hab_8910) 

# Analyze first observations
head(data)


