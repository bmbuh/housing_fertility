#Coded by: Brian Buh
#Started on: 21.11.2022
#Last Updated:

## This script looks at the relationship between property prices and likelihood of having a birth

library(tidyverse)
library(arsenal)
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

hhpart3 %>% count(is.na(lowquar)) #63177 (86.5%) of observations can be linked to a LAD level PP

# # DF exclusing observations in which no data is available for LAD level PP
hhpartpp <- hhpart3 %>%
  filter(!is.na(lowquar)) %>% 
  mutate(lq2 = lowquar/1000)

summary(hhpartpp$lowquar)

hhpartppp1 <- hhpartpp %>% filter(parity == 1)
hhpartppp2 <- hhpartpp %>% filter(parity == 2)
hhpartppp3 <- hhpartpp %>% filter(parity == 3)


###########################################################################
# Descriptive Property Price ----------------------------------------------
###########################################################################

#This uses hhpartpp! Separated by parity
mycontrols <- tableby.control(test = FALSE)
hhpartppstats <-arsenal::tableby(parity ~ event + clock + ratio + ratio_cat2 + period + tenure + age + partner + edu + ukborn + emp + share + oci + lowquar, 
                                data = hhpartpp, 
                                weights = weight,
                                control = mycontrols)
labels(hhpartppstats) <-  c(parity = "Parity", event = "Event", clock = "Exposure", age = "Age",
                           ratio = "Ratio of Housing to Income", ratio_cat2 = "Ratio of Housing to Income (Cat.)", period = "Period",
                           tenure = "Housing type", partner = "Partnership status", edu = "Educational attainment", ukborn = "UK Born",
                           emp = "Activity status", share = "Household income share", oci = "Overcrowding Index", lowquar = "Lower Quartile Housing Price")
summary(hhpartppstats)
write2html(hhpartppstats, "hhpartppstats_parity_21-11-2022.html") #UPDATE DATE
write2word(hhpartppstats, "hhpartppstats_parity_21-11-2022.docx") #UPDATE DATE


# Modelling ---------------------------------------------------------------

#Analysis d1
d1 <- glmer(formula = event ~ clock*parity + lq2 + period + (1|pidp) + (1|code),
            data = hhpartpp,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(d1)
summ(d1, exp = TRUE)
saveRDS(d1,"d1.rds")
md1 <- margins(d1)
summary(md1)

#Analysis d2
d2 <- glmer(formula = event ~ clock*parity + lq2 + period + age_cat + agesq + edu + ukborn + tenure + (1|pidp) + (1|code),
            data = hhpartpp,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(d2)
summ(d2, exp = TRUE)
saveRDS(d2,"d2.rds")
md2 <- margins(d2)
summary(md2)

#Analysis d3
d3 <- glmer(formula = event ~ clock*parity + lq2*parity + period + age_cat + agesq + edu + ukborn + tenure + (1|pidp) + (1|code),
            data = hhpartpp,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(d3)
summ(d3, exp = TRUE)
saveRDS(d3,"d3.rds")
md3 <- margins(d3)
summary(md3)

#Analysis d4
d4 <- glmer(formula = event ~ clock*parity + lq2*parity + period + age_cat + agesq + edu + ukborn + tenure + emp + (1|pidp) + (1|code),
            data = hhpartpp,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(d4)
summ(d4, exp = TRUE)
saveRDS(d4,"d4.rds")
md4 <- margins(d4)
summary(md4)


#Analysis d5
d5 <- glmer(formula = event ~ clock*parity + lq2*parity + period + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
            data = hhpartpp,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(d5)
summ(d5, exp = TRUE)
saveRDS(d5,"d5.rds")
md5 <- margins(d5)
summary(md5)

# -------------------------------------------------------------------------
# Parity Specific ---------------------------------------------------------
# -------------------------------------------------------------------------


#Analysis d1p1
d1p1 <- glmer(formula = event ~ clock + lq2 + period + age_cat + agesq + edu + ukborn + tenure + (1|pidp) + (1|code),
            data = hhpartppp1,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(d1p1)
summ(d1p1, exp = TRUE)
saveRDS(d1p1,"d1p1.rds")
md1p1 <- margins(d1p1)
summary(md1p1)


#Analysis d2p1
d2p1 <- glmer(formula = event ~ clock + lq2 + period + age_cat + agesq + edu + ukborn + tenure + emp + (1|pidp) + (1|code),
              data = hhpartppp1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(d2p1)
summ(d2p1, exp = TRUE)
saveRDS(d2p1,"d2p1.rds")
md2p1 <- margins(d2p1)
summary(md2p1)


#Analysis d3p1
d3p1 <- glmer(formula = event ~ clock + lq2 + period + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|pidp) + (1|code),
              data = hhpartppp1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(d3p1)
summ(d3p1, exp = TRUE)
saveRDS(d3p1,"d3p1.rds")
md3p1 <- margins(d3p1)
summary(md3p1)



#Analysis d4p1
d4p1 <- glmer(formula = event ~ clock + lq2 + period + age_cat + agesq + edu + ukborn + tenure  + (1|code),
              data = hhpartppp1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(d4p1)
summ(d4p1, exp = TRUE)
saveRDS(d4p1,"d4p1.rds")
md4p1 <- margins(d4p1)
summary(md4p1)


#Analysis d5p1
d5p1 <- glmer(formula = event ~ clock + lq2 + period + age_cat + agesq + edu + ukborn + tenure + emp + (1|code),
              data = hhpartppp1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(d5p1)
summ(d5p1, exp = TRUE)
saveRDS(d5p1,"d5p1.rds")
md5p1 <- margins(d5p1)
summary(md5p1)



