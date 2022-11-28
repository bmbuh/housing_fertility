#Coded by: Brian Buh
#Started on: 21.11.2022
#Last Updated: 28.11.2022

## This script examines periods individually to look at possible changes
### The conclusion from this script is that the pattern observed in script 10 are consistent when stratifying by period
### It is hard to observe a period specific effect

library(tidyverse)
library(haven)
library(texreg)
library(sjPlot) #Use for the plot_model
library(lme4)
library(effects)
library(jtools) #summ function
library(broom.mixed) #plot_summs function 
library(margins)
library(interactions) #for using cat_plot()
library(modelsummary)
library(huxtable)
library(openxlsx) #For printing huxtable outputs in xlsx
library(mediation)

#Load data  hhpart (s7)
hhpart <- file.choose()
hhpart <- readRDS(hhpart)

# DF for individuals not living with no parents in the household
hhpart2 <- hhpart %>% 
  dplyr::filter(parenthh == 0)

# DF for individuals not living with no parents in the household AND co-residing with a partner
hhpart3 <- hhpart2 %>% 
  dplyr::filter(partner != "single")

# DF for periodspecific models
hhpart3p90 <- hhpart3 %>% filter(period == "1991-1999")
    hhpart3p90p1 <- hhpart3p90 %>% filter(parity == 1)
    hhpart3p90p2 <- hhpart3p90 %>% filter(parity == 2)
    hhpart3p90p3 <- hhpart3p90 %>% filter(parity == 3)
hhpart3pe20 <- hhpart3 %>% filter(period == "2000-2007")
    hhpart3pe20p1 <- hhpart3pe20 %>% filter(parity == 1)
    hhpart3pe20p2 <- hhpart3pe20 %>% filter(parity == 2)
    hhpart3pe20p3 <- hhpart3pe20 %>% filter(parity == 3)
hhpart3pr20 <- hhpart3 %>% filter(period == "2008-2012")
    hhpart3pr20p1 <- hhpart3pr20 %>% filter(parity == 1)
    hhpart3pr20p2 <- hhpart3pr20 %>% filter(parity == 2)
    hhpart3pr20p3 <- hhpart3pr20 %>% filter(parity == 3)
hhpart3pl20 <- hhpart3 %>% filter(period == "2013-2021")
    hhpart3pl20p1 <- hhpart3pl20 %>% filter(parity == 1)
    hhpart3pl20p2 <- hhpart3pl20 %>% filter(parity == 2)
    hhpart3pl20p3 <- hhpart3pl20 %>% filter(parity == 3)



############################################################################
# Models ------------------------------------------------------------------
############################################################################

## 1991-1999
# c1p90: raw effect
# c2p90: + housing tenure
# c3p90: + emp
# c4p90: + oci
# c1p90p1: parity 1  
# c2p90p1: parity 1
# c3p90p1: parity 1
# c4p90p1: parity 1
# c4p90p2: parity 2
# c4p90p3: parity 3
    
## 2000-2007
# c1pe20: raw effect
# c2pe20: + housing tenure
# c3pe20: + emp
# c4pe20: + oci
    # c1pe20p1: parity 1  
    # c2pe20p1: parity 1
    # c3pe20p1: parity 1
    # c4pe20p1: parity 1
# c4pe20p2: parity 2
# c4pe20p3: parity 3
    
## 2008-2012
# c1pr20: raw effect
# c2pr20: + housing tenure
# c3pr20: + emp
# c4pr20: + oci
    # c1pr20p1: parity 1  
    # c2pr20p1: parity 1
    # c3pr20p1: parity 1
    # c4pr20p1: parity 1
# c4pr20p2: parity 2
# c4pr20p3: parity 3
    
## 2013-2021
# c1pl20: raw effect
# c2pl20: + housing tenure
# c3pl20: + emp
# c4pl20: + oci
    # c1pl20p1: parity 1  
    # c2pl20p1: parity 1
    # c3pl20p1: parity 1
    # c4pl20p1: parity 1
# c4pl20p2: parity 2
# c4pl20p3: parity 3

    
# -------------------------------------------------------------------------
# Period 1991-1999 --------------------------------------------------------
# -------------------------------------------------------------------------

#Steps 1 and 2 on continuous variable
c1p90 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn 
              + (1|pidp) + (1|code),
              data = hhpart3p90,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(c1p90)
summ(c1p90, exp = TRUE)
saveRDS(c1p90,"c1p90.rds")
mc1p90 <- margins(c1p90)
saveRDS(mc1p90,"mc1p90.rds")


c2p90 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure
              + (1|pidp) + (1|code),
              data = hhpart3p90,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(c2p90)
summ(c2p90, exp = TRUE)
saveRDS(c2p90,"c2p90.rds")
mc2p90 <- margins(c2p90)
saveRDS(mc2p90,"mc2p90.rds")


c3p90 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure + emp 
              + (1|pidp) + (1|code),
              data = hhpart3p90,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(c3p90)
summ(c3p90, exp = TRUE)
saveRDS(c3p90,"c3p90.rds")
mc3p90 <- margins(c3p90)
saveRDS(mc3p90,"mc3p90.rds")


c4p90 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci 
              + (1|pidp) + (1|code),
              data = hhpart3p90,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(c4p90)
summ(c4p90, exp = TRUE)
saveRDS(c4p90,"c4p90.rds")
mc4p90 <- margins(c4p90)
saveRDS(mc4p90,"mc4p90.rds")


# Parity Specific -------------------------------------------------------------

c1p90p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + (1|pidp) + (1|code),
                  data = hhpart3p90p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c1p90p1)
summ(c1p90p1, exp = TRUE)
saveRDS(c1p90p1,"c4p90p1.rds")
mc1p90p1 <- margins(c1p90p1)
saveRDS(mc1p90p1,"mc1p90p1.rds")

c2p90p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + (1|pidp) + (1|code),
                  data = hhpart3p90p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c2p90p1)
summ(c2p90p1, exp = TRUE)
saveRDS(c2p90p1,"c2p90p1.rds")
mc2p90p1 <- margins(c2p90p1)
saveRDS(mc2p90p1,"mc2p90p1.rds")

c3p90p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp  + (1|pidp) + (1|code),
                  data = hhpart3p90p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c3p90p1)
summ(c3p90p1, exp = TRUE)
saveRDS(c3p90p1,"c3p90p1.rds")
mc3p90p1 <- margins(c3p90p1)
saveRDS(mc3p90p1,"mc3p90p1.rds")

c4p90p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
               data = hhpart3p90p1,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(c4p90p1)
summ(c4p90p1, exp = TRUE)
saveRDS(c4p90p1,"c4p90p1.rds")
mc4p90p1 <- margins(c4p90p1)
saveRDS(mc4p90p1,"mc4p90p1.rds")

c4p90p2 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                 data = hhpart3p90p2,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(c4p90p2)
summ(c4p90p2, exp = TRUE)
saveRDS(c4p90p2,"c4p90p2.rds")
mc4p90p2 <- margins(c4p90p2)
saveRDS(mc4p90p2,"mc4p90p2.rds")

c4p90p3 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                 data = hhpart3p90p3,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(c4p90p3)
summ(c4p90p3, exp = TRUE)
saveRDS(c4p90p3,"c4p90p3.rds")
mc4p90p3 <- margins(c4p90p3)
saveRDS(mc4p90p3,"mc4p90p3.rds")



# -------------------------------------------------------------------------
# Period 2000-2007 --------------------------------------------------------
# -------------------------------------------------------------------------

c1pe20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn 
               + (1|pidp) + (1|code),
               data = hhpart3pe20,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(c1pe20)
summ(c1pe20, exp = TRUE)
saveRDS(c1pe20,"c1pe20.rds")
mc1pe20 <- margins(c1pe20)
saveRDS(mc1pe20,"mc1pe20.rds")


c2pe20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure
               + (1|pidp) + (1|code),
               data = hhpart3pe20,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(c2pe20)
summ(c2pe20, exp = TRUE)
saveRDS(c2pe20,"c2pe20.rds")
mc2pe20 <- margins(c2pe20)
saveRDS(mc2pe20,"mc2pe20.rds")


c3pe20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure + emp 
               + (1|pidp) + (1|code),
               data = hhpart3pe20,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(c3pe20)
summ(c3pe20, exp = TRUE)
saveRDS(c3pe20,"c3pe20.rds")
mc3pe20 <- margins(c3pe20)
saveRDS(mc3pe20,"mc3pe20.rds")


c4pe20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci 
               + (1|pidp) + (1|code),
               data = hhpart3pe20,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(c4pe20)
summ(c4pe20, exp = TRUE)
saveRDS(c4pe20,"c4pe20.rds")
mc4pe20 <- margins(c4pe20)
saveRDS(mc4pe20,"mc4pe20.rds")


# Parity Specific -------------------------------------------------------------

c1pe20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + (1|pidp) + (1|code),
                  data = hhpart3pe20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c1pe20p1)
summ(c1pe20p1, exp = TRUE)
saveRDS(c1pe20p1,"c4pe20p1.rds")
mc1pe20p1 <- margins(c1pe20p1)
saveRDS(mc1pe20p1,"mc1pe20p1.rds")

c2pe20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + (1|pidp) + (1|code),
                  data = hhpart3pe20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c2pe20p1)
summ(c2pe20p1, exp = TRUE)
saveRDS(c2pe20p1,"c2pe20p1.rds")
mc2pe20p1 <- margins(c2pe20p1)
saveRDS(mc2pe20p1,"mc2pe20p1.rds")

c3pe20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp  + (1|pidp) + (1|code),
                  data = hhpart3pe20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c3pe20p1)
summ(c3pe20p1, exp = TRUE)
saveRDS(c3pe20p1,"c3pe20p1.rds")
mc3pe20p1 <- margins(c3pe20p1)
saveRDS(mc3pe20p1,"mc3pe20p1.rds")

c4pe20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                 data = hhpart3pe20p1,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(c4pe20p1)
summ(c4pe20p1, exp = TRUE)
saveRDS(c4pe20p1,"c4pe20p1.rds")
mc4pe20p1 <- margins(c4pe20p1)
saveRDS(mc4pe20p1,"mc4pe20p1.rds")

c4pe20p2 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                 data = hhpart3pe20p2,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(c4pe20p2)
summ(c4pe20p2, exp = TRUE)
saveRDS(c4pe20p2,"c4pe20p2.rds")
mc4pe20p2 <- margins(c4pe20p2)
saveRDS(mc4pe20p2,"mc4pe20p2.rds")

c4pe20p3 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                 data = hhpart3pe20p3,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(c4pe20p3)
summ(c4pe20p3, exp = TRUE)
saveRDS(c4pe20p3,"c4pe20p3.rds")
mc4pe20p3 <- margins(c4pe20p3)
saveRDS(mc4pe20p3,"mc4pe20p3.rds")




# -------------------------------------------------------------------------
# Period 2008-2012 --------------------------------------------------------
# -------------------------------------------------------------------------

#Steps 1 and 2 on continuous variable
c1pr20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn 
                + (1|pidp) + (1|code),
                data = hhpart3pr20,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(c1pr20)
summ(c1pr20, exp = TRUE)
saveRDS(c1pr20,"c1pr20.rds")
mc1pr20 <- margins(c1pr20)
saveRDS(mc1pr20,"mc1pr20.rds")


c2pr20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure
                + (1|pidp) + (1|code),
                data = hhpart3pr20,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(c2pr20)
summ(c2pr20, exp = TRUE)
saveRDS(c2pr20,"c2pr20.rds")
mc2pr20 <- margins(c2pr20)
saveRDS(mc2pr20,"mc2pr20.rds")


c3pr20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure + emp 
                + (1|pidp) + (1|code),
                data = hhpart3pr20,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(c3pr20)
summ(c3pr20, exp = TRUE)
saveRDS(c3pr20,"c3pr20.rds")
mc3pr20 <- margins(c3pr20)
saveRDS(mc3pr20,"mc3pr20.rds")


c4pr20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci 
                + (1|pidp) + (1|code),
                data = hhpart3pr20,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(c4pr20)
summ(c4pr20, exp = TRUE)
saveRDS(c4pr20,"c4pr20.rds")
mc4pr20 <- margins(c4pr20)
saveRDS(mc4pr20,"mc4pr20.rds")


# Parity Specific -------------------------------------------------------------

c1pr20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + (1|pidp) + (1|code),
                  data = hhpart3pr20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c1pr20p1)
summ(c1pr20p1, exp = TRUE)
saveRDS(c1pr20p1,"c4pr20p1.rds")
mc1pr20p1 <- margins(c1pr20p1)
saveRDS(mc1pr20p1,"mc1pr20p1.rds")

c2pr20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + (1|pidp) + (1|code),
                  data = hhpart3pr20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c2pr20p1)
summ(c2pr20p1, exp = TRUE)
saveRDS(c2pr20p1,"c2pr20p1.rds")
mc2pr20p1 <- margins(c2pr20p1)
saveRDS(mc2pr20p1,"mc2pr20p1.rds")

c3pr20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp  + (1|pidp) + (1|code),
                  data = hhpart3pr20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c3pr20p1)
summ(c3pr20p1, exp = TRUE)
saveRDS(c3pr20p1,"c3pr20p1.rds")
mc3pr20p1 <- margins(c3pr20p1)
saveRDS(mc3pr20p1,"mc3pr20p1.rds")

c4pr20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                  data = hhpart3pr20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c4pr20p1)
summ(c4pr20p1, exp = TRUE)
saveRDS(c4pr20p1,"c4pr20p1.rds")
mc4pr20p1 <- margins(c4pr20p1)
saveRDS(mc4pr20p1,"mc4pr20p1.rds")

c4pr20p2 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                  data = hhpart3pr20p2,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c4pr20p2)
summ(c4pr20p2, exp = TRUE)
saveRDS(c4pr20p2,"c4pr20p2.rds")
mc4pr20p2 <- margins(c4pr20p2)
saveRDS(mc4pr20p2,"mc4pr20p2.rds")

c4pr20p3 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                  data = hhpart3pr20p3,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c4pr20p3)
summ(c4pr20p3, exp = TRUE)
saveRDS(c4pr20p3,"c4pr20p3.rds")
mc4pr20p3 <- margins(c4pr20p3)
saveRDS(mc4pr20p3,"mc4pr20p3.rds")



# -------------------------------------------------------------------------
# Period 2013-2021 --------------------------------------------------------
# -------------------------------------------------------------------------

#Steps 1 and 2 on continuous variable
c1pl20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn 
                + (1|pidp) + (1|code),
                data = hhpart3pl20,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(c1pl20)
summ(c1pl20, exp = TRUE)
saveRDS(c1pl20,"c1pl20.rds")
mc1pl20 <- margins(c1pl20)
saveRDS(mc1pl20,"mc1pl20.rds")


c2pl20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure
                + (1|pidp) + (1|code),
                data = hhpart3pl20,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(c2pl20)
summ(c2pl20, exp = TRUE)
saveRDS(c2pl20,"c2pl20.rds")
mc2pl20 <- margins(c2pl20)
saveRDS(mc2pl20,"mc2pl20.rds")


c3pl20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure + emp 
                + (1|pidp) + (1|code),
                data = hhpart3pl20,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(c3pl20)
summ(c3pl20, exp = TRUE)
saveRDS(c3pl20,"c3pl20.rds")
mc3pl20 <- margins(c3pl20)
saveRDS(mc3pl20,"mc3pl20.rds")


c4pl20 <- glmer(formula = event ~ clock*parity + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci 
                + (1|pidp) + (1|code),
                data = hhpart3pl20,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(c4pl20)
summ(c4pl20, exp = TRUE)
saveRDS(c4pl20,"c4pl20.rds")
mc4pl20 <- margins(c4pl20)
saveRDS(mc4pl20,"mc4pl20.rds")


# Parity Specific -------------------------------------------------------------

c1pl20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + (1|pidp) + (1|code),
                  data = hhpart3pl20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c1pl20p1)
summ(c1pl20p1, exp = TRUE)
saveRDS(c1pl20p1,"c4pl20p1.rds")
mc1pl20p1 <- margins(c1pl20p1)
saveRDS(mc1pl20p1,"mc1pl20p1.rds")

c2pl20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + (1|pidp) + (1|code),
                  data = hhpart3pl20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c2pl20p1)
summ(c2pl20p1, exp = TRUE)
saveRDS(c2pl20p1,"c2pl20p1.rds")
mc2pl20p1 <- margins(c2pl20p1)
saveRDS(mc2pl20p1,"mc2pl20p1.rds")

c3pl20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp  + (1|pidp) + (1|code),
                  data = hhpart3pl20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c3pl20p1)
summ(c3pl20p1, exp = TRUE)
saveRDS(c3pl20p1,"c3pl20p1.rds")
mc3pl20p1 <- margins(c3pl20p1)
saveRDS(mc3pl20p1,"mc3pl20p1.rds")

c4pl20p1 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                  data = hhpart3pl20p1,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c4pl20p1)
summ(c4pl20p1, exp = TRUE)
saveRDS(c4pl20p1,"c4pl20p1.rds")
mc4pl20p1 <- margins(c4pl20p1)
saveRDS(mc4pl20p1,"mc4pl20p1.rds")

c4pl20p2 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                  data = hhpart3pl20p2,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c4pl20p2)
summ(c4pl20p2, exp = TRUE)
saveRDS(c4pl20p2,"c4pl20p2.rds")
mc4pl20p2 <- margins(c4pl20p2)
saveRDS(mc4pl20p2,"mc4pl20p2.rds")

c4pl20p3 <- glmer(formula = event ~ clock + ratio + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
                  data = hhpart3pl20p3,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 2e5)),
                  weights = weight) 

summary(c4pl20p3)
summ(c4pl20p3, exp = TRUE)
saveRDS(c4pl20p3,"c4pl20p3.rds")
mc4pl20p3 <- margins(c4pl20p3)
saveRDS(mc4pl20p3,"mc4pl20p3.rds")


###########################################################################
# Outputs -----------------------------------------------------------------
###########################################################################

cm_cont_per <- c("clock" = "Clock",
                     "ratio" = "Housing Expenditure",
                     "parity1" = "Parity 1",
                     "parity2" = "Parity 2",
                     "parity3" = "Parity 3",
                     "tenurerent" = "Private Rent",
                     "tenuresocial" = "Social Housing",
                     "empunemp" = "Unemployed",
                     "empinactive" = "Inactive",
                     "empstudent" = "Student",
                     "oci" = "Overcrowding Index",
                     "shareequal" = "Equal",
                     "sharefb" = "Femalebreadwinner",
                     "age_cat25-28" = "25-28",
                     "age_cat30-34" = "30-34",
                     "age_cat25-39" = "35-39",
                     "age_cat40-45" = "40-45",
                     "agesq" = "Age Squared",
                     "edulow" = "Low",
                     "edumedium" = "Medium",
                     "ukborn1" = "UK Born")

mc1_output_allperiod <- list(mc1p90, mc1pe20, mc1pr20, mc1pl20)
modelsummary(mc1_output_allperiod, coef_map = cm_cont_per, output = "mc1_output_allperiod_AME_s11_28-11-2022.html", stars = TRUE)

mc2_output_allperiod <- list(mc2p90, mc2pe20, mc2pr20, mc2pl20)
modelsummary(mc2_output_allperiod, coef_map = cm_cont_per, output = "mc2_output_allperiod_AME_s11_28-11-2022.html", stars = TRUE)

mc3_output_allperiod <- list(mc3p90, mc3pe20, mc3pr20, mc3pl20)
modelsummary(mc3_output_allperiod, coef_map = cm_cont_per, output = "mc3_output_allperiod_AME_s11_28-11-2022.html", stars = TRUE)

mc4_output_allperiod <- list(mc4p90, mc4pe20, mc4pr20, mc4pl20)
modelsummary(mc4_output_allperiod, coef_map = cm_cont_per, output = "mc4_output_allperiod_AME_s11_28-11-2022.html", stars = TRUE)


mcp90_output_p90 <- list(mc1p90, mc2p90, mc3p90, mc4p90)
modelsummary(mcp90_output_p90, coef_map = cm_cont_per, output = "mcp90_output_p90_AME_s11_28-11-2022.html", stars = TRUE)

mcpe20_output_pe20 <- list(mc1pe20, mc2pe20, mc3pe20, mc4pe20)
modelsummary(mcpe20_output_pe20, coef_map = cm_cont_per, output = "mcpe20_output_pe20_AME_s11_28-11-2022.html", stars = TRUE)

mcpr20_output_pr20 <- list(mc1pr20, mc2pr20, mc3pr20, mc4pr20)
modelsummary(mcpr20_output_pr20, coef_map = cm_cont_per, output = "mcpr20_output_pr20_AME_s11_28-11-2022.html", stars = TRUE)

mcpl20_output_pl20 <- list(mc1pl20, mc2pl20, mc3pl20, mc4pl20)
modelsummary(mcpl20_output_pl20, coef_map = cm_cont_per, output = "mcpl20_output_pl20_AME_s11_28-11-2022.html", stars = TRUE)



#Continous - parity specific
#Parity 1
mcp_output_period_parity1 <- list(mc4p90p1, mc4pe20p1, mc4pr20p1, mc4pl20p1)
cm_cont_par_per <- c("clock" = "Clock",
                 "ratio" = "Housing Expenditure",
                 "tenurerent" = "Private Rent",
                 "tenuresocial" = "Social Housing",
                 "empunemp" = "Unemployed",
                 "empinactive" = "Inactive",
                 "empstudent" = "Student",
                 "oci" = "Overcrowding Index",
                 "shareequal" = "Equal",
                 "sharefb" = "Femalebreadwinner",
                 "age_cat25-28" = "25-28",
                 "age_cat30-34" = "30-34",
                 "age_cat25-39" = "35-39",
                 "age_cat40-45" = "40-45",
                 "agesq" = "Age Squared",
                 "edulow" = "Low",
                 "edumedium" = "Medium",
                 "ukborn1" = "UK Born")
modelsummary(mcp_output_period_parity1, coef_map = cm_cont_par_per, output = "mcp_output_period_p1cont_AME_s11_28-11-2022.html", stars = TRUE)
mcp_cont_period_parity1 <- modelsummary(mcp_output_period_parity1, output = "huxtable", stars = TRUE)
# quick_docx(mcp_cont_period_parity1, file = "mcp_output_period_p1cont_AME_s11_28-11-2022.docx", open = FALSE)
quick_xlsx(mcp_cont_period_parity1, file = "mcp_output_period_p1cont_AME_s11_28-11-2022.xlsx", open = FALSE)

#Parity 2
mcp_output_period_parity2 <- list(mc4p90p2, mc4pe20p2, mc4pr20p2, mc4pl20p2)
modelsummary(mcp_output_period_parity2, coef_map = cm_cont_par_per, output = "mcp_output_period_p2cont_AME_s11_28-11-2022.html", stars = TRUE)
mcp_cont_period_parity2 <- modelsummary(mcp_output_period_parity2, output = "huxtable", stars = TRUE)
# quick_docx(mcp_cont_period_parity2, file = "mcp_output_period_p2cont_AME_s11_28-11-2022.docx", open = FALSE)
quick_xlsx(mcp_cont_period_parity2, file = "mcp_output_period_p2cont_AME_s11_28-11-2022.xlsx", open = FALSE)

#Parity 3
mcp_output_period_parity3 <- list(mc4p90p3, mc4pe20p3, mc4pr20p3, mc4pl20p3)
modelsummary(mcp_output_period_parity3, coef_map = cm_cont_par_per, output = "mcp_output_period_p3cont_AME_s11_28-11-2022.html", stars = TRUE)
mcp_cont_period_parity3 <- modelsummary(mcp_output_period_parity3, output = "huxtable", stars = TRUE)
# quick_docx(mcp_cont_period_parity3, file = "mcp_output_period_p3cont_AME_s11_28-11-2022.docx", open = FALSE)
quick_xlsx(mcp_cont_period_parity3, file = "mcp_output_period_p3cont_AME_s11_28-11-2022.xlsx", open = FALSE)

## Only Parity 1

#p90
mcp_output_p90_parity1 <- list(mc1p90p1, mc2p90p1, mc3p90p1, mc4p90p1)
modelsummary(mcp_output_p90_parity1, coef_map = cm_cont_par_per, output = "mcp_output_p90_p1cont_AME_s11_28-11-2022.html", stars = TRUE)

#pe20
mcp_output_pe20_parity1 <- list(mc1pe20p1, mc2pe20p1, mc3pe20p1, mc4pe20p1)
modelsummary(mcp_output_pe20_parity1, coef_map = cm_cont_par_per, output = "mcp_output_pe20_p1cont_AME_s11_28-11-2022.html", stars = TRUE)

#pr20
mcp_output_pr20_parity1 <- list(mc1pr20p1, mc2pr20p1, mc3pr20p1, mc4pr20p1)
modelsummary(mcp_output_pr20_parity1, coef_map = cm_cont_par_per, output = "mcp_output_pr20_p1cont_AME_s11_28-11-2022.html", stars = TRUE)

#pl20
mcp_output_pl20_parity1 <- list(mc1pl20p1, mc2pl20p1, mc3pl20p1, mc4pl20p1)
modelsummary(mcp_output_pl20_parity1, coef_map = cm_cont_par_per, output = "mcp_output_pl20_p1cont_AME_s11_28-11-2022.html", stars = TRUE)


