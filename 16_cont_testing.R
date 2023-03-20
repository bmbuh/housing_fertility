#Coded by: Brian Buh
#Started on: 20.03.2023 
#Last Updated: 

## This script was created to examine the shape of curves using the continuous measure "ratio" 
# and plotting the PP using effect_plots

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
library(janitor) #For tabyl

#Load data  hhpart (s7)
hhpart <- file.choose()
hhpart <- readRDS(hhpart)

str(hhpart)

# DF for individuals not living with no parents in the household
hhpart2 <- hhpart %>% 
  dplyr::filter(parenthh == 0) %>% 
  mutate(oci2 = as.numeric(oci2)) #needed for mediation

# DF for individuals not living with no parents in the household AND co-residing with a partner
hhpart3 <- hhpart2 %>% 
  dplyr::filter(partner != "single")

# DF for removing parity 3
hhpart4 <- hhpart3 %>% 
  dplyr::filter(parity != 3)

hhpart4 %>% count(ratio_cat3)

# Heterogeneity testing df
## Age
u29 <- hhpart4 %>% filter(age_cat2 == "u29")
o30 <- hhpart4 %>% filter(age_cat2 == "o30")

## Income
umedinc <- hhpart4 %>% filter(medinc == 0)
umedinc %>% count(event, parity, tenure, ratio_cat3)
omedinc <- hhpart4 %>% filter(medinc == 1)
omedinc %>% count(event, parity, tenure)

#Urban/rural
urban <- hhpart4 %>% filter(urban == 1)
rural <- hhpart4 %>% filter(urban == 2)

# High Price LAD
highlad <- hhpart4 %>% filter(medlowquar == 1)
lowlad <- hhpart4 %>% 
  filter(!is.na(lowquar), is.na(medlowquar))

# Housing crisis
precrisis <- hhpart4 %>% filter(period == "1992-1999" | period == "2000-2007")
postcrisis <- hhpart4 %>% filter(period == "2008-2012" | period == "2013-2022")

# DF for parity specific models
hhpart4p1 <- hhpart4 %>% filter(parity == 1)
hhpart4p2 <- hhpart4 %>% filter(parity == 2)


# DF for housing type
owned <- hhpart4 %>% filter(tenure == "owned")
rent <- hhpart4 %>% filter(tenure == "rent")

hhpart4p1rent <- hhpart4p1 %>% filter(tenure == "rent")
hhpart4p2rent <- hhpart4p2 %>% filter(tenure == "rent")
  

# -------------------------------------------------------------------------
# Full Analysis -----------------------------------------------------------
# -------------------------------------------------------------------------

#analysis g1
g1 <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(g1)
summ(g1, exp = TRUE)
saveRDS(g1,"g1.rds")
# mg1 <- margins(g1)
# summary(mg1)

effect_plot(g1, pred = ratio,
            interval = TRUE,
            main.title = "full")



#analysis g2
g2 <- glmer(formula = event ~ clock*parity + ratio*parity*hhemp + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(g2)
summ(g2, exp = TRUE)
saveRDS(g2,"g2.rds")
# mg2 <- margins(g2)
# summary(mg2)

effect_plot(g2, pred = ratio,
            interval = TRUE,
            main.title = "hhemp")



# housing type ------------------------------------------------------------

#analysis g1 - owned
g1owned <- glmer(formula = event ~ clock*parity + ratio*parity + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = owned,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(g1owned)
summ(g1owned, exp = TRUE)
saveRDS(g1owned,"g1owned.rds")
# mg1 <- margins(g1)
# summary(mg1)

effect_plot(g1owned, pred = ratio,
            interval = TRUE,
            main.title = "owned")

#analysis g1 - rent
g1rent <- glmer(formula = event ~ clock*parity + ratio*parity + period + age_cat + agesq + edu + ukborn + hhemp 
                 + (1|pidp) + (1|code),
                 data = rent,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(g1rent)
summ(g1rent, exp = TRUE)
saveRDS(g1rent,"g1rent.rds")
# mg1 <- margins(g1)
# summary(mg1)

effect_plot(g1rent, pred = ratio,
            interval = TRUE,
            main.title = "rent")

#analysis g1 - rent parity 1
g1hhpart4p1rent <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + hhemp 
                + (1|pidp) + (1|code),
                data = hhpart4p2rent,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(g1hhpart4p1rent)
summ(g1hhpart4p1rent, exp = TRUE)
saveRDS(g1hhpart4p1rent,"g1hhpart4p1rent.rds")
# mg1 <- margins(g1)
# summary(mg1)

effect_plot(g1hhpart4p1rent, pred = ratio,
            interval = TRUE,
            main.title = "rentp1")

#analysis g1 - rent parity 2
g1hhpart4p2rent <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + hhemp 
                + (1|pidp) + (1|code),
                data = hhpart4p2rent,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)),
                weights = weight) 

summary(g1hhpart4p2rent)
summ(g1hhpart4p2rent, exp = TRUE)
saveRDS(g1hhpart4p2rent,"g1hhpart4p2rent.rds")
# mg1 <- margins(g1)
# summary(mg1)

effect_plot(g1hhpart4p2rent, pred = ratio,
            interval = TRUE,
            main.title = "rentp2")

# -------------------------------------------------------------------------
# Parity Specific ---------------------------------------------------------
# -------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parity 1 ----------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis p1g1
p1g1 <- glmer(formula = event ~ clock + ratio*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart4p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p1g1)
summ(p1g1, exp = TRUE)
saveRDS(p1g1,"p1g1.rds")
# mp1g1 <- margins(p1g1)
# summary(mp1g1)

effect_plot(p1g1, pred = ratio,
            interval = TRUE,
            main.title = "p1g1")



#analysis p1g2
p1g2 <- glmer(formula = event ~ clock + ratio*hhemp + period + age_cat + agesq + edu + ukborn + tenure
              + (1|pidp) + (1|code),
              data = hhpart4p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p1g2)
summ(p1g2, exp = TRUE)
saveRDS(p1g2,"p1g2.rds")
# mp1g2 <- margins(p1g2)
# summary(mp1g2)

effect_plot(p1g2, pred = ratio)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parity 2 ----------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis p2g1
p2g1 <- glmer(formula = event ~ clock + ratio*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart4p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p2g1)
summ(p2g1, exp = TRUE)
saveRDS(p2g1,"p2g1.rds")
# mp2g1 <- margins(p2g1)
# summary(mp2g1)

effect_plot(p2g1, pred = ratio,
            interval = TRUE,
            main.title = "p2g1")


#analysis p2g2
p2g2 <- glmer(formula = event ~ clock + ratio*hhemp + period + age_cat + agesq + edu + ukborn + tenure
              + (1|pidp) + (1|code),
              data = hhpart4p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p2g2)
summ(p2g2, exp = TRUE)
saveRDS(p2g2,"p2g2.rds")
# mp2g2 <- margins(p2g2)
# summary(mp2g2)

effect_plot(p2g2, pred = ratio)


#Categorical
parity_output <- list(p1g1, p1g2, p2g1, p2g2)
# cm_cat <- c("clock" = "Clock",
#             "parity2" = "Parity 2",
#             "parity3" = "Parity 3",
#             "ratio_cat30-10" = "0-10%",
#             "ratio_cat320-30" = "20-30%",
#             "ratio_cat330-40" = "30-40%",
#             "ratio_cat340-100" = "40-100%",
#             "tenurerent" = "Private Rent",
#             "tenuresocial" = "Social Housing",
#             "empunemp" = "Unemployed",
#             "empinactive" = "Inactive",
#             "empstudent" = "Student",
#             "oci21" = "Overcrowding Index",
#             "shareequal" = "Equal",
#             "sharefb" = "Femalebreadwinner",
#             "period2000-2007" = "2000-2007",
#             "period2008-2012" = "2008-2012",
#             "period2013-2022" = "2013-2022",
#             "age_cat25-28" = "25-28",
#             "age_cat30-34" = "30-34",
#             "age_cat25-39" = "35-39",
#             "age_cat40-45" = "40-45",
#             "agesq" = "Age Squared",
#             "edulow" = "Low",
#             "edumedium" = "Medium",
#             "ukborn1" = "UK Born")
parity <- modelsummary(parity_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(parity, file = "parity_S14_20-03-2023-2023.html", open = FALSE)




###########################################################################
# Heterogeneity testing ---------------------------------------------------
###########################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## Age --------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### g1u29 = g1 with observations equal to or under 29
#analysis g1u29
g1u29 <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = u29,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(g1u29)
summ(g1u29, exp = TRUE)
saveRDS(g1u29,"g1u29.rds")
# mg1u29 <- margins(g1u29)
# summary(mg1u29)

effect_plot(g1u29, pred = ratio,
            interval = TRUE,
            main.title = "u29")


### g1o30 = g1 with observations over 30
g1o30 <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = o30,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(g1o30)
summ(g1o30, exp = TRUE)
saveRDS(g1o30,"g1o30.rds")
# mg1o30 <- margins(g1o30)
# summary(mg1o30)

effect_plot(g1o30, pred = ratio,
            interval = TRUE,
            main.title = "o30")





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Income level ------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### g1umedinc = g1 with observations under yearly median hh income
g1umedinc <- glmer(formula = event ~ clock*parity + ratio*parity + tenure + period + age_cat + agesq + edu + ukborn + hhemp
                   + (1|pidp) + (1|code),
                   data = umedinc,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight)

summary(g1umedinc)
summ(g1umedinc, exp = TRUE)
saveRDS(g1umedinc,"g1umedinc.rds")
# mg1umedinc <- margins(g1umedinc)
# summary(mg1umedinc)

effect_plot(g1umedinc, pred = ratio,
            interval = TRUE,
            main.title = "umedinc")


### g1omedinc = g1 with observations equal to or over yearly median hh income
g1omedinc <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                   + (1|pidp) + (1|code),
                   data = omedinc,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight) 

summary(g1omedinc)
summ(g1omedinc, exp = TRUE)
saveRDS(g1omedinc,"g1omedinc.rds")
# mg1omedinc <- margins(g1omedinc)
# summary(mg1omedinc)

effect_plot(g1omedinc, pred = ratio,
            interval = TRUE,
            main.title = "omedinc")






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Urban/rural -------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### g1urban = g1 with only urban observations
g1urban <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                 + (1|pidp) + (1|code),
                 data = urban,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(g1urban)
summ(g1urban, exp = TRUE)
saveRDS(g1urban,"g1urban.rds")
# mg1urban <- margins(g1urban)
# summary(mg1urban)

effect_plot(g1urban, pred = ratio,
            interval = TRUE,
            main.title = "urban")


### g1rural = g1 with only rural observations
g1rural <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                 + (1|pidp) + (1|code),
                 data = rural,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(g1rural)
summ(g1rural, exp = TRUE)
saveRDS(g1rural,"g1rural.rds")
# mg1rural <- margins(g1rural)
# summary(mg1rural)

effect_plot(g1rural, pred = ratio,
            interval = TRUE,
            main.title = "rural")






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# LAD ---------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### g1highlad = g1 with only LAD with housing above the median low quartile home price
g1highlad <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                   + (1|pidp) + (1|code),
                   data = highlad,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight) 

summary(g1highlad)
summ(g1highlad, exp = TRUE)
saveRDS(g1highlad,"g1highlad.rds")
# mg1highlad <- margins(g1highlad)
# summary(mg1highlad)

effect_plot(g1highlad, pred = ratio,
            interval = TRUE,
            main.title = "highlad")



### g1lowload = g1 with only LAD with housing above the median low quartile home price
g1lowlad <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                   + (1|pidp) + (1|code),
                   data = lowlad,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight) 

summary(g1lowlad)
summ(g1lowlad, exp = TRUE)
saveRDS(g1lowlad,"g1lowlad.rds")
# mg1lowload <- margins(g1lowload)
# summary(mg1lowload)

effect_plot(g1lowlad, pred = ratio,
            interval = TRUE,
            main.title = "lowlad")








# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Crisis -----------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


### g1precrisis = g1 with only precrisis observations (aka BHPS)
g1precrisis <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + age_cat + agesq + edu + ukborn + hhemp 
                     + (1|pidp) + (1|code),
                     data = precrisis,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight) 

summary(g1precrisis)
summ(g1precrisis, exp = TRUE)
saveRDS(g1precrisis,"g1precrisis.rds")
# mg1precrisis <- margins(g1precrisis)
# summary(mg1precrisis)

effect_plot(g1precrisis, pred = ratio,
            interval = TRUE,
            main.title = "precrisis")




### g1postcrisis = g1 with only postcrisis observations (aka UKHLS)
g1postcrisis <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + age_cat + agesq + edu + ukborn + hhemp 
                      + (1|pidp) + (1|code),
                      data = postcrisis,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5)),
                      weights = weight) 

summary(g1postcrisis)
summ(g1postcrisis, exp = TRUE)
saveRDS(g1postcrisis,"g1postcrisis.rds")
# mg1postcrisis <- margins(g1postcrisis)
# summary(mg1postcrisis)

effect_plot(g1postcrisis, pred = ratio,
            interval = TRUE,
            main.title = "postcrisis")

