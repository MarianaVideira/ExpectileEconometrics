if(!require("devtools")) install.packages("devtools")
if(!require("plm"))install.packages("plm") # panel data regression
if(!require("lfe"))install.packages("lfe") # HDFE regression
if(!require("quantreg"))install.packages("quantreg") # quantile
install.packages("rqpd", repos="http://R-Forge.R-project.org") # quantile with fixed effects
if(!require("expectreg"))install.packages("expectreg") # expectile
if(!require("webshot"))install.packages("webshot") # obtaining images from html
library(tidyverse,devtools,plm,lfe,quantreg,webshot,rqpd,expectreg)
install_github("strengejacke/strengejacke")
library(sjPlot) # nice outputs
webshot::install_phantomjs(force = TRUE)
if(!require("Rcpp"))install.packages("Rcpp") # C++ 
if(!require("RcppArmadillo"))install.packages("RcppArmadillo") # C++ Armadillo
webshot::install_lgfortran(force = TRUE)
library(Rcpp,RcppArmadillo)

remove.packages("RcppArmadillo")
install.packages("RcppArmadillo", dependencies = TRUE)
install.packages("ggpubr", dependencies = TRUE)

# Free unused memory
gc()

# Check Data 
head(dat)


#  1. POOLED OLS ---------------------------------------------------------------

# 1.1  Pooled OLS on Nominal Wages----------------------------------------------

# 1.1.1 OLS on Wage
OLS1_1 <- lm(wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat)
tab_model (OLS1_1, file = "OLS1_1.html")
webshot("OLS1_1.html", "OLS1_1.png")

# 1.1.2 OLS on Log Wage
OLS1_2 <- lm(lwage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat)
tab_model (OLS1_2, file = "OLS1_2.html")
webshot("OLS1_2.html", "OLS1_2.png")

# 1.1.3 OLS on Hourly Wage
OLS1_3 <- lm(t_h_wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat)
tab_model (OLS1_3, file = "OLS1_3.html")
webshot("OLS1_3.html", "OLS1_3.png")

rm(OLS1_1,OLS1_2,OLS1_3)
gc()

# 1.2  Pooled OLS on Real Wages------------------------------------------------

# 1.2.1 OLS on Wage
OLS2_1 <- lm(t_real_wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat)
tab_model (OLS2_1, file = "OLS2_1.html")
webshot("OLS2_1.html", "OLS2_1.png")

# 1.2.2 OLS on Log Wage
OLS2_2 <- lm(log_t_real_wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat)
tab_model (OLS2_2, file = "OLS2_2.html")
webshot("OLS2_2.html", "OLS2_2.png")

# 1.2.3 OLS on Hourly Wage
OLS2_3 <- lm(t_real_h_wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat)
tab_model (OLS2_3, file = "OLS2_3.html")
webshot("OLS2_3.html", "OLS2_3.png")

# 1.2.4 OLS on Log Hourly Wage
OLS2_4 <- lm(log_t_real_h_wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat)
tab_model (OLS2_4, file = "OLS2_4.html")
webshot("OLS2_4.html", "OLS2_4.png")

rm(OLS2_1,OLS2_2,OLS2_3,OLS2_4)
gc()


# 2. FIXED EFFECTS -------------------------------------------------------------

# 2.1  Fixed Effects (individual level)-----------------------------------------

# 2.1.1 FE on Log Wage
FE1_1 <- plm(lwage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL,
             dat, model="within", effect = "individual")
tab_model (FE1_1, file = "FE1_1.html")
webshot("FE1_1.html", "FE1_1.png")

# 2.1.2 FE on Log Real Wage
FE1_2 <- plm(log_t_real_wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL,
             dat, model="within", effect = "individual")
tab_model (FE1_2, file = "FE1_2.html")
webshot("FE1_2.html", "FE1_2.png")

# 2.1.3 FE on Log Real Hourly Wage
FE1_3 <- plm(log_t_real_h_wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL,
             dat, model="within", effect = "individual")
tab_model (FE1_3, file = "FE1_3.html")
webshot("FE1_3.html", "FE1_3.png")

rm(FE1_1,FE1_2,FE1_3)
gc()



# 2.2  Fixed Effects (individual and time level)--------------------------------

# 2.2.1 FE on Log Wage
FE2_1 <- plm(lwage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat, model="within", effect = "individual")
tab_model (FE2_1, file = "FE2_1.html")
webshot("FE2_1.html", "FE2_1.png")

# 2.2.2 FE on Log Real Wage
FE2_2 <- plm(log_t_real_wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat, model="within", effect = "individual")
tab_model (FE2_2, file = "FE2_2.html")
webshot("FE2_2.html", "FE2_2.png")

# 2.2.3 FE on Log Real Hourly Wage
FE2_3 <- plm(log_t_real_h_wage ~ age + age2 + male + highschool + college + portuguese + 
               firmS + firmM + firmB + firmL + year,
             dat, model="within", effect = "individual")
tab_model (FE2_3, file = "FE2_3.html")
webshot("FE2_3.html", "FE2_3.png")

rm(FE2_1,FE2_2,FE2_3)
gc()



# 3. HIGH-DIMENSIONAL FIXED EFFECTS ---------------------------------------

# 3.1  High-Dimensional Fixed-Effects: individual and firm----------------------

# 3.1.1 HFE on Log Wage
HFE1_1 <- felm(lwage ~ age + age2 +  highschool + college + portuguese + 
                 firmS + firmM + firmB + firmL + year | nss + irc, dat)
tab_model (HFE1_1, file = "HFE1_1.html")
webshot("HFE1_1.html", "HFE1_1.png")

# 3.1.2 HFE on Log Real Wage
HFE1_2 <- felm(log_t_real_wage ~ age + age2 +  highschool + college + portuguese +
                 firmS + firmM + firmB + firmL + year | nss + irc, dat)
tab_model (HFE1_2, file = "HFE1_2.html")
webshot("HFE1_2.html", "HFE1_2.png")

# 3.1.3 HFE on Log Real Hourly Wage
HFE1_3 <- felm(log_t_real_h_wage ~ age + age2 +  highschool + college + portuguese +
                 firmS + firmM + firmB + firmL + year | nss + irc, dat)
tab_model (HFE1_3, file = "HFE1_3.html")
webshot("HFE1_3.html", "HFE1_3.png")

rm(HFE1_1,HFE1_2,HFE1_3)
gc()


# 3.2  High-Dimensional Fixed-Effects: individual, firm and job category--------

# 3.2.1 HFE on Log Wage
HFE2_1 <- felm(lwage ~ age + age2 +  highschool + college + portuguese + 
                 firmS + firmM + firmB + firmL + year | nss + irc + prof_1d, dat)
tab_model (HFE2_1, file = "HFE2_1.html")
webshot("HFE2_1.html", "HFE2_1.png")

# 3.2.2 HFE on Log Real Wage
HFE2_2 <- felm(log_t_real_wage ~ age + age2 +  highschool + college + portuguese +
                 firmS + firmM + firmB + firmL + year | nss + irc + prof_1d, dat)
tab_model (HFE2_2, file = "HFE2_2.html")
webshot("HFE2_2.html", "HFE2_2.png")

# 3.1.3 HFE on Log Real Hourly Wage
HFE2_3 <- felm(log_t_real_h_wage ~ age + age2 +  highschool + college + portuguese +
                 firmS + firmM + firmB + firmL + year | nss + irc + prof_1d, dat)
tab_model (HFE2_3, file = "HFE2_3.html")
webshot("HFE2_3.html", "HFE2_3.png")

rm(HFE2_1,HFE2_2,HFE2_3)
gc()



# 4.  QUANTILE REGRESSIONS ------------------------------------------------

# 4.1 Quantile regression-------------------------------------------------------

# 4.1.1 QR on Log Wage
QR1_1 <- rq(lwage ~ age + age2 +  male + highschool + college + portuguese +
              firmS + firmM + firmB + firmL + year,
            tau = seq(0.1, 0.9, by = 0.2),
            dat)
# Plotting data
QR1_1.plot <- summary(QR1_1)
plot(QR1_1.plot, parm = c("highschool","college","age","age2","male","portuguese"))
gc()

# 4.1.3 QR on Log Real Wage
QR1_2 <- rq(log_t_real_wage ~ age + age2 +  male + highschool + college + portuguese +
              firmS + firmM + firmB + firmL + year,
            tau = seq(0.1, 0.9, by = 0.2),
            dat)
# Plotting data
QR1_2.plot <- summary(QR1_2)
plot(QR1_2.plot, parm = c("highschool","college","age","age2","male","portuguese"))
gc()

# 4.1.3 QR on Log Real Hourly Wage
QR1_3 <- rq(log_t_real_h_wage ~ age + age2 +  male + highschool + college + portuguese +
              firmS + firmM + firmB + firmL + year,
            tau = seq(0.1, 0.9, by = 0.2),
            dat)
# Plotting data
QR1_3.plot <- summary(QR1_3)
plot(QR1_3.plot, parm = c("highschool","college","age","age2","male","portuguese"))
gc()



# 4.2 Quantile regression with individual fixed effects-------------------------

# 4.2.1 QR on Log Wage
QR2_1 <- rqpd(lwage ~ age + age2 +  male + highschool + college + portuguese +
                firmS + firmM + firmB + firmL + year | nss,
              panel(taus = c(0.1,0.3,0.5,0.7,0.9), tauw = rep(1/5,5)),
              data= dat)
# Plotting data
QR2_1.plot <- rqpd::summary.rqpd(QR2_1)
print(QR2_1.plot, parm = c("highschool","college","age","age2","male","portuguese"))
gc()


# 4.2.3 QR on Log Real Wage
QR2_2 <- rqpd(log_t_real_wage ~ age + age2 +  male + highschool + college + portuguese +
                firmS + firmM + firmB + firmL + year | nss,
              panel(taus = c(0.1,0.3,0.5,0.7,0.9), tauw = rep(1/5,5)),
              data= dat)
# Plotting data
QR2_2.plot <- rqpd::summary.rqpd(QR2_2)
print(QR2_2.plot, parm = c("highschool","college","age","age2","male","portuguese"))
gc()

# 4.2.3 QR on Log Real Hourly Wage
QR2_3 <- rqpd(log_t_real_h_wage ~ age + age2 +  male + highschool + college + portuguese +
                firmS + firmM + firmB + firmL + year | nss,
              panel(taus = c(0.1,0.3,0.5,0.7,0.9), tauw = rep(1/5,5)),
              data= dat)
# Plotting data
QR2_3.plot <- rqpd::summary.rqpd(QR2_3)
print(QR2_3.plot, parm = c("highschool","college","age","age2","male","portuguese"))
gc()



# 5.  EXPECTILE REGRESSIONS -----------------------------------------------

# 5.1 Expectile regression------------------------------------------------------

u = seq(0.1,0.9,by=0.2)

# 5.1.1 ER on Log Wage
coefstd_ER1_1 = function(u) summary(expectreg.ls(lwage ~ age + age2 +  male + highschool + college + portuguese +
                                                   firmS + firmM + firmB + firmL + year, 
                                                 data = dat, expectiles =u, ci = TRUE)) [,2]

coefest_ER1_1 = function(u) summary(expectreg.ls(lwage ~ age + age2 +  male + highschool + college + portuguese +
                                                   firmS + firmM + firmB + firmL + year, 
                                                 data = dat, expectiles =u, ci = TRUE)) [,1]
CS1 = Vectorize (coefstd_ER1_1) (u)
CE1 = Vectorize (coefest_ER1_1) (u)


# 5.1.2 ER on Log Real Wage
coefstd_ER1_2 = function(u) summary(expectreg.ls(log_t_real_wage ~ age + age2 +  male + highschool + college + portuguese +
                                                   firmS + firmM + firmB + firmL + year, 
                                                 data = dat, expectiles =u, ci = TRUE)) [,2]

coefest_ER1_2 = function(u) summary(expectreg.ls(log_t_real_wage ~ age + age2 +  male + highschool + college + portuguese +
                                                   firmS + firmM + firmB + firmL + year, 
                                                 data = dat, expectiles =u, ci = TRUE)) [,1]
CS2 = Vectorize (coefstd_ER1_2) (u)
CE2 = Vectorize (coefest_ER1_2) (u)


# 5.1.3 ER on Log Real Hourly Wage
ER1_3 = function(u) summary(expectreg.ls(log_t_real_h_wage ~ age + age2 +  male + highschool + college + portuguese +
                                           firmS + firmM + firmB + firmL + year, 
                                         data = dat, expectiles =u, ci = TRUE)) 
coefstd_ER1_3 = function(u) summary(expectreg.ls(log_t_real_h_wage ~ age + age2 +  male + highschool + college + portuguese +
                                                   firmS + firmM + firmB + firmL + year, 
                                                 data = dat, expectiles =u, ci = TRUE)) [,2]

coefest_ER1_3 = function(u) summary(expectreg.ls(log_t_real_h_wage ~ age + age2 +  male + highschool + college + portuguese +
                                                   firmS + firmM + firmB + firmL + year, 
                                                 data = dat, expectiles =u, ci = TRUE)) [,1]
CS3 = Vectorize (coefstd_ER1_3) (u)
CE3 = Vectorize (coefest_ER1_3) (u)

plot(coefstd_ER1_3)


# Plots
# lwage
library(ggplot2)
# Basic density
theme_update(plot.title = element_text (hjust = 0.5))
summary(dat$log_t_real_wage)
plot1 <- ggplot(dat, aes(x=log_t_real_wage)) + 
  geom_density(stat = "density", fill = "darkblue", alpha =0.25, color = NA) +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(4.65, 8.15), breaks = seq(4.65, 8.15, 0.5)) +
  geom_line(stat = "density", color = "darkblue") +
  geom_vline(aes(xintercept=mean(log_t_real_wage)),
             color="blue", linetype="dashed", size=0.25, alpha = 0.75 ) +
  labs(x= element_blank(), y = element_blank()) +
  ggtitle("Weight Density Curve", "Log Real Wage") + 
  theme(plot.title = element_text(size = 7, family = "Courier", face = "bold", color = "darkblue")) + 
  annotate("text", x = 6.7, y = 0.85, label = "Mean of Log Real Wage (5.645)", size = 4, family = "Courier", color = "darkblue") +
  theme_classic()
plot1

summary(dat$log_t_real_h_wage)
plot2 <- ggplot(dat, aes(x=log_t_real_h_wage)) + 
  geom_density(stat = "density", fill = "darkblue", alpha =0.25, color = NA) +
  expand_limits(y = 0) +
  scale_x_continuous(limits = c(-0.5, 3), breaks = seq(-0.5, 3, 0.5)) +
  geom_line(stat = "density", color = "darkblue") +
  geom_vline(aes(xintercept=mean(log_t_real_h_wage)),
             color="blue", linetype="dashed", size=0.25, alpha = 0.75 ) +
  labs(x= element_blank(), y = element_blank()) +
  ggtitle("Weight Density Curve", "Log Real Hourly Wage") + 
  theme(plot.title = element_text(size = 7, family = "Courier", face = "bold", color = "darkblue")) + 
  annotate("text", x = 1.75, y = 0.85, label = "Mean of Log Real Hourly Wage (0.503)", size = 4, family = "Courier", color = "darkblue") +
  theme_classic()
plot2







               