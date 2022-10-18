#Coded by: Brian Buh
#Started on: 06.10.2022
#Last Updated: 17.10.2022

# This script continues from S8 after determining the best fit multilevel model was M15
###Note: For this script I order the model runs using A#

# install.packages("mediation")

library(tidyverse)
library(haven)
library(arsenal)
library(survival)
library(survminer)
library(texreg)
library(sjPlot) #Use for the plot_model
library(stargazer)
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
library(stargazer)

#Load data  hhpart (s7)
hhpart <- file.choose()
hhpart <- readRDS(hhpart)

str(hhpart)

# DF for individuals not living with no parents in the household
hhpart2 <- hhpart %>% 
  dplyr::filter(parenthh == 0)

# DF for individuals not living with no parents in the household AND co-residing with a partner
hhpart3 <- hhpart2 %>% 
  dplyr::filter(partner != "single")


# DF for parity specific models
hhpart3p1 <- hhpart3 %>% filter(parity == 1)
hhpart3p2 <- hhpart3 %>% filter(parity == 2)
hhpart3p3 <- hhpart3 %>% filter(parity == 3)

###########################################################################
# Models ------------------------------------------------------------------
###########################################################################

## Control testing
### Note: the model already controls for age, agesq, and housing tenure
# A1: + emp
# A2: + edu
# A3: + emp + edu
# A4: + UK Born
# A5: + emp + UK Born
# A6: + emp + edu + UK Born 
# A6alt: same as A6 with a different ratio_cat2 reference group (20-30%)
# A6alt2: same as A6 with a different ratio_cat2 reference group (0.1-10%)

## Weights
# A7: M15 + weights
### For comparing weights versus non-weighted
# n1: + edu + UK Born
# w1: + edu + UK Born + weight
# n2: + edu + UK Born + tenure
# w2: + edu + UK Born + tenure + weight
# n3: + edu + UK Born + tenure + emp + oci
# w3: + edu + UK Born + tenure + emp + oci + weight

## Significance testing
# A8: continuous ratio + edu + UK Born
# A9: continuous ratio + edu + UK Born + tenure
# A10: continuous ratio + edu + UK Born + tenure + emp
# A11: continuous ratio + edu + UK Born + tenure + emp + oci
# A12: continuous ratio + edu + UK Born + tenure + emp + oci + share

## Mediation analysis
# Med1.1: Equation without employment (X -> Y)
# Med1.2: oci on event (M -> Y)
# Med1.3: Add in the mediator (X -> M -> Y)
##NOTE: mEDIATION DOES NOT WORK WITH THREE-LEVEL MODELS

## Parity Specific
# p1.1: Only parity 1, full model, unweighted
# p2.1: Only parity 2, full model, unweighted
# p3.1: Only parity 3, full model, unweighted
#
# p1.2: Only parity 1, no oci, weighted
# p2.2: Only parity 2, no oci, weighted
# p3.2: Only parity 3, no oci, weighted
# 
# p1.3: Only parity 1, full model, weighted
# p2.3: Only parity 2, full model, weighted
# p3.3: Only parity 3, full model, weighted

# medp1x: Mediation analysis of osi - parity 1 only
# medp1m
# medp1y

# medp1xtest: Mediation analysis test effect of ownout dummy
# medp1mtest
# medp1ytest

# medp2x: Mediation analysis of osi - parity 2 only
# medp2m
# medp2y
# 
# medp3x: : Mediation analysis of osi - parity 3 only
# medp3m
# medp3y


###########################################################################
# Control testing ---------------------------------------------------------
###########################################################################


# Analysis A1 --------------------------------------------------------------
# emp
a1 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + (1|pidp) + (1|code),
             data = hhpart3,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(a1)
summ(a1, exp = TRUE)

#save
saveRDS(a1,"a1.rds")
# readRDS("a1.rds")
# a1 <- readRDS("S:/r_projects/housing_fertility/model_objects/a1.rds")


#Parity Predicted Probability Plots
effect_plot(a1, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a1:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp",
            y.label = "Experencing a Live Birth")

cat_plot(a1, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "a1:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a1_control_emp_S9_06_10-2022.png", dpi = 300)



# Analysis A2 --------------------------------------------------------------
# edu
a2 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + edu + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(a2)
summ(a2, exp = TRUE)

#save
saveRDS(a2,"a2.rds")
# readRDS("a2.rds")
# a2 <- readRDS("S:/r_projects/housing_fertility/model_objects/a2.rds")


#Parity Predicted Probability Plots
effect_plot(a2, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a2:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Edu",
            y.label = "Experencing a Live Birth")

cat_plot(a2, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "a2:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Edu",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a2_control_edu_S9_06_10-2022.png", dpi = 300)


# Analysis A3 --------------------------------------------------------------
# emp + edu
a3 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + edu + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(a3)
summ(a3, exp = TRUE)

#save
saveRDS(a3,"a3.rds")
# readRDS("a3.rds")
# a3 <- readRDS("S:/r_projects/housing_fertility/model_objects/a3.rds")


#Parity Predicted Probability Plots
effect_plot(a3, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a3:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + Edu",
            y.label = "Experencing a Live Birth")

cat_plot(a3, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "a3:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + Edu",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a3_control_emp+edu_S9_06_10-2022.png", dpi = 300)


# Analysis A4 --------------------------------------------------------------
# ukborn
a4 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + ukborn + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(a4)
summ(a4, exp = TRUE)

#save
saveRDS(a4,"a4.rds")
# readRDS("a4.rds")
# a4 <- readRDS("S:/r_projects/housing_fertility/model_objects/a4.rds")


#Parity Predicted Probability Plots
effect_plot(a4, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a4:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + UKborn",
            y.label = "Experencing a Live Birth")

cat_plot(a4, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "a4:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + UKborn",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a4_control_ukborn_S9_06_10-2022.png", dpi = 300)




# Analysis A5 --------------------------------------------------------------
# emp + ukborn
a5 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + ukborn + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(a5)
summ(a5, exp = TRUE)

#save
saveRDS(a5,"a5.rds")
# readRDS("a5.rds")
# a5 <- readRDS("S:/r_projects/housing_fertility/model_objects/a5.rds")


#Parity Predicted Probability Plots
effect_plot(a5, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a5:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + UKborn",
            y.label = "Experencing a Live Birth")

cat_plot(a5, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "a5:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + UKborn",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a5_control_emp+ukborn_S9_06_10-2022.png", dpi = 300)


# Analysis A6 --------------------------------------------------------------
# emp + edu + ukborn
a6 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + edu + ukborn + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(a6)
summ(a6, exp = TRUE)

#save
saveRDS(a6,"a6.rds")
# readRDS("a6.rds")
# a6 <- readRDS("S:/r_projects/housing_fertility/model_objects/a6.rds")


#Parity Predicted Probability Plots
effect_plot(a6, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a6:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + Edu + UKborn",
            y.label = "Experencing a Live Birth")

cat_plot(a6, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "a6:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + Edu + UKborn",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a6_control_emp+edu+ukborn_S9_06_10-2022.png", dpi = 300)


# Analysis A6  Alternative --------------------------------------------------------------
# emp + edu + ukborn (I switch the reference group for ratio_cat2 to 20-30% to see how it affects the p-values)
# the variable is called "ratio_cat2alt"
a6alt <- glmer(formula = event ~ clock*parity + ratio_cat2alt*parity + ratio_cat2alt*period + tenure + age + agesq + emp + edu + ukborn + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(a6alt)
summ(a6alt, exp = TRUE)

#save
saveRDS(a6alt,"a6alt.rds")
# readRDS("a6alt.rds")
# a6alt <- readRDS("S:/r_projects/housing_fertility/model_objects/a6alt.rds")


#Parity Predicted Probability Plots
effect_plot(a6alt, 
            pred = ratio_cat2alt, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a6alt:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + Edu + UKborn",
            y.label = "Experencing a Live Birth")

cat_plot(a6alt, pred = parity, modx = ratio_cat2alt,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "a6alt:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + Edu + UKborn; ref. 20-30%",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a6alt_control_emp+edu+ukborn_S9_10_10-2022.png", dpi = 300)

#This makes little difference (actually has worse statistical significance)

# Analysis A6  Alternative  2--------------------------------------------------------------
# emp + edu + ukborn (I switch the reference group for ratio_cat2 to 0.1-10%% to see how it affects the p-values)
# the variable is called "ratio_cat2alt2"
a6alt2 <- glmer(formula = event ~ clock*parity + ratio_cat2alt2*parity + ratio_cat2alt2*period + tenure + age + agesq + emp + edu + ukborn + (1|pidp) + (1|code),
               data = hhpart3,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5))) 

summary(a6alt2)
summ(a6alt2, exp = TRUE)

#save
saveRDS(a6alt2,"a6alt2.rds")
# readRDS("a6alt2.rds")
# a6alt2 <- readRDS("S:/r_projects/housing_fertility/model_objects/a6alt2.rds")


#Parity Predicted Probability Plots
effect_plot(a6alt2, 
            pred = ratio_cat2alt2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a6alt2:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + Edu + UKborn",
            y.label = "Experencing a Live Birth")

cat_plot(a6alt2, pred = parity, modx = ratio_cat2alt2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "a6alt2:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp + Edu + UKborn; ref. 0.1-10%",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a6alt2_control_emp+edu+ukborn_S9_10_10-2022.png", dpi = 300)

#This makes little difference (actually has worse statistical significance)

# -------------------------------------------------------------------------
# Output ------------------------------------------------------------------
# -------------------------------------------------------------------------

# Tables Combined
# M15 is the base model; A1-A6 add controls stepwise
stargazer(m15, a1, a2, a3, a4, a5, a6,
          align = TRUE,
          out = "control_testing.html",
          column.labels = c("m15", "a1", "a2", "a3", "a4", "a5", "a6"))




###########################################################################
# Weights -----------------------------------------------------------------
###########################################################################


# Analysis A7 --------------------------------------------------------------
# weights
a7 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(a7)
summ(a7, exp = TRUE)

#save
saveRDS(a7,"a7.rds")
# readRDS("a7.rds")
# a7 <- readRDS("S:/r_projects/housing_fertility/model_objects/a7.rds")


#Parity Predicted Probability Plots
effect_plot(a7, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a7:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age; weights",
            y.label = "Experencing a Live Birth")

cat_plot(a7, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "a7:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age; weights",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("a7_weights_S9_06_10-2022.png", dpi = 300)


# -------------------------------------------------------------------------
# Weight comparison on model choices --------------------------------------
# -------------------------------------------------------------------------

# Analysis 1 - Tenure

n1 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(n1)
summ(n1, exp = TRUE)
saveRDS(n1,"n1.rds")

cat_plot(n1, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "n1: nonweighted tenure",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("n1_nonweighted_emp_S9_13_10-2022.png", dpi = 300)


w1 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(w1)
summ(w1, exp = TRUE)
saveRDS(w1,"w1.rds")

cat_plot(w1, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "w1: weighted tenure",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("w1_weighted_tenure_S9_13_10-2022.png", dpi = 300)

# Analysis 2 - Emp

n2 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(n2)
summ(n2, exp = TRUE)
saveRDS(n2,"n2.rds")

cat_plot(n2, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "n1: nonweighted emp",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("n2_nonweighted_emp_S9_13_10-2022.png", dpi = 300)


w2 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(w2)
summ(w2, exp = TRUE)
saveRDS(w2,"w2.rds")

cat_plot(w2, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "w2: weighted emp",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("w2_weighted_emp_S9_13_10-2022.png", dpi = 300)

# Analysis 3 - plus OCI

n3 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + oci + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(n3)
summ(n3, exp = TRUE)
saveRDS(n3,"n3.rds")

cat_plot(n3, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "n3: nonweighted oci",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("n3_nonweighted_oci_S9_13_10-2022.png", dpi = 300)


w3 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + oci + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(w3)
summ(w3, exp = TRUE)
saveRDS(w3,"w3.rds")

cat_plot(w3, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "w3: weighted tenure",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("w3_weighted_oci_S9_13_10-2022.png", dpi = 300)


# -------------------------------------------------------------------------
# Output ------------------------------------------------------------------
# -------------------------------------------------------------------------

# Tables Combined
stargazer(n1, w1, n2, w2, n3, w3,
          align = TRUE,
          out = "weight_testing.html",
          column.labels = c( "n1", "w1", "n2", "w2", "n3", "w3"))




###########################################################################
# Significance Testing ----------------------------------------------------
###########################################################################

# A8: continuous ratio + edu + UK Born
a8 <- glmer(formula = event ~ clock*parity + ratio*parity + ratio*period + age + agesq + edu + ukborn 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 
summary(a8)
summ(a8, exp = TRUE)
saveRDS(a8,"a8.rds")

effect_plot(a8, 
            pred = ratio, 
            interval = TRUE, 
            cat.geom = "line",
            main.title = "A8: continuous ratio",
            y.label = "Experencing a Live Birth")
ggsave("a8_continious_S9_11_10-2022.png", dpi = 300)

# A9: continuous ratio + tenure + edu + UK Born
a9 <- glmer(formula = event ~ clock*parity + ratio*parity + ratio*period + tenure + age + agesq + edu + ukborn 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 
summary(a9)
summ(a9, exp = TRUE)
saveRDS(a9,"a9.rds")

effect_plot(a9, 
            pred = ratio, 
            # pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "A9: continuous ratio + tenure + edu + UK Born",
            y.label = "Experencing a Live Birth")
ggsave("a9_continious+tenure_S9_11_10-2022.png", dpi = 300)


# A10: continuous ratio + tenure + edu + UK Born + emp
a10 <- glmer(formula = event ~ clock*parity + ratio*parity + ratio*period + tenure + age + agesq + edu + ukborn + emp 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 
summary(a10)
summ(a10, exp = TRUE)
saveRDS(a10,"a10.rds")

effect_plot(a10, 
            pred = ratio, 
            interval = TRUE, 
            cat.geom = "line",
            main.title = "A10: continuous ratio + tenure + emp",
            y.label = "Experencing a Live Birth")
ggsave("a10_continious_S9_11_10-2022.png", dpi = 300)

# a11: continuous ratio + tenure + edu + UK Born + emp + oci
a11 <- glmer(formula = event ~ clock*parity + ratio*parity + ratio*period + tenure + age + agesq + edu + ukborn + emp + oci 
             + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 
summary(a11)
summ(a11, exp = TRUE)
saveRDS(a11,"a11.rds")

effect_plot(a11, 
            pred = ratio, 
            interval = TRUE, 
            cat.geom = "line",
            main.title = "A11: continuous ratio + tenure + emp + oci ",
            y.label = "Experencing a Live Birth")
ggsave("a11_continious_S9_11_10-2022.png", dpi = 300)

# a12: continuous ratio + tenure + edu + UK Born + emp + oci + share
a12 <- glmer(formula = event ~ clock*parity + ratio*parity + ratio*period + tenure + age + agesq + edu + ukborn + emp + oci + share
             + (1|pidp) + (1|code),
             data = hhpart3,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 
summary(a12)
summ(a12, exp = TRUE)
saveRDS(a12,"a12.rds")

effect_plot(a12, 
            pred = ratio, 
            interval = TRUE, 
            cat.geom = "line",
            main.title = "A12: continuous ratio + tenure + emp + oci + share",
            y.label = "Experencing a Live Birth")
ggsave("a12_continious_S9_11_10-2022.png", dpi = 300)

# -------------------------------------------------------------------------
# Output ------------------------------------------------------------------
# -------------------------------------------------------------------------

# Tables Combined
stargazer(a8, a9, a10, a11, a12, 
          align = TRUE,
          out = "significance_testing.html",
          column.labels = c( "a8", "a9", "a10", "a11", "a12"))


###########################################################################
# Mediation Analysis ------------------------------------------------------
###########################################################################

# There is one potential mediator of housing cost on fertility
# 1. Available Space


# -------------------------------------------------------------------------
# Available Space ---------------------------------------------------------
# -------------------------------------------------------------------------

# Step 1: X -> Y (ratio_cat2 -> event)
med1.1 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + edu + ukborn + (1|pidp) + (1|code),
               data = hhpart3,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5))) 
summary(med1.1)
saveRDS(med1.1,"med1.1.rds")


# Step 2: X -> M (ratio_cat2 -> oci)
med1.2 <- glmer(formula = oci ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + edu + ukborn + (1|pidp) + (1|code),
                data = hhpart3,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(med1.2)
summ(med1.2, exp = TRUE)
saveRDS(med1.2,"med1.2.rds")

# Step 3: X -> M -> Y (ratio_cat2 -> oci -> event)
med1.3 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + oci + tenure + age + agesq + edu + ukborn + (1|pidp) + (1|code),
                data = hhpart3,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(med1.3)
summ(med1.3, exp = TRUE)
saveRDS(med1.3,"med1.3.rds")

# Mediation Analysis
##NOTE: IT IS NOT POSSIBLE TO USE THE MEDIATE FUNCTION WITH A THREE-LEVEL MODEL
med1.result <- mediate(med1.2, med1.3, treat = "ratio_cat2", mediator = "oci")
summary(med1.result)
saveRDS(med1.result,"med1.result.rds")



###########################################################################
# Separate parity testing -------------------------------------------------
###########################################################################

# For the parity specific models the following is done:
# 1. Create df specifric for each risk parity
# 2. remove interactions for parity
# 3. remove the individual level random effect

hhpart3p1 <- hhpart3 %>% filter(parity == 1)
hhpart3p2 <- hhpart3 %>% filter(parity == 2)
hhpart3p3 <- hhpart3 %>% filter(parity == 3)

# p1.1: parity 1 - all controls for now
p1.1 <- glmer(formula = event ~ clock + ratio_cat2 + ratio_cat2*period + tenure + age + agesq + edu + ukborn + emp + oci  + (1|code),
             data = hhpart3p1,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 
summary(p1.1)
summ(p1.1, exp = TRUE)
saveRDS(p1.1,"p1.1.rds")

# p2.1
p2.1 <- glmer(formula = event ~ clock + ratio_cat2 + ratio_cat2*period + tenure + age + agesq + edu + ukborn + emp + oci  + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))) 
summary(p2.1)
summ(p2.1, exp = TRUE)
saveRDS(p2.1,"p2.1.rds")

# p3.1
p3.1 <- glmer(formula = event ~ clock + ratio_cat2 + ratio_cat2*period + tenure + age + agesq + edu + ukborn + emp + oci  + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))) 
summary(p3.1)
summ(p3.1, exp = TRUE)
saveRDS(p3.1,"p3.1.rds")


# -------------------------------------------------------------------------
# Output ------------------------------------------------------------------
# -------------------------------------------------------------------------

# Tables Combined
stargazer(p1.1, p2.1, p3.1, 
          align = TRUE,
          out = "parity_testing.html",
          column.labels = c("Parity 1", "Parity 2", "Parity 3"))



# ------------------------------------------------------------------------
# With weights ------------------------------------------------------------
# ------------------------------------------------------------------------

# p1.2: parity 1 - all controls NO OCI
p1.2 <- glmer(formula = event ~ clock + ratio_cat2 + ratio_cat2*period + tenure +  agesq + edu + ukborn + emp + (1|code),
              data = hhpart3p1,
              family = binomial,
              weights = weight,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))) 
summary(p1.2)
summ(p1.2, exp = TRUE)
saveRDS(p1.2,"p1.2.rds")
mp1.2 <- margins(p1.2)
summary(mp1.2)
saveRDS(mp1.2,"mp1.2.rds")


# p2.2
p2.2 <- glmer(formula = event ~ clock + ratio_cat2 + ratio_cat2*period + tenure + age + agesq + edu + ukborn + emp + (1|code),
              data = hhpart3p2,
              family = binomial,
              weights = weight,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))) 
summary(p2.2)
summ(p2.2, exp = TRUE)
saveRDS(p2.2,"p2.2.rds")
mp2.2 <- margins(p2.2)
summary(mp2.2)
saveRDS(mp2.2,"mp2.2.rds")


# p3.2
p3.2 <- glmer(formula = event ~ clock + ratio_cat2 + ratio_cat2*period + tenure + age + agesq + edu + ukborn + emp + (1|code),
              data = hhpart3p3,
              family = binomial,
              weights = weight,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))) 
summary(p3.2)
summ(p3.2, exp = TRUE)
saveRDS(p3.2,"p3.2.rds")
mp3.2 <- margins(p3.2)
summary(mp3.2)
saveRDS(mp3.2,"mp3.2.rds")


# p1.3: parity 1 - all controls WITH OCI
p1.3 <- glmer(formula = event ~ clock + ratio_cat2 + ratio_cat2*period + tenure +  agesq + edu + ukborn + emp + oci  + (1|code),
              data = hhpart3p1,
              family = binomial,
              weights = weight,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))) 
summary(p1.3)
summ(p1.3, exp = TRUE)
saveRDS(p1.3,"p1.3.rds")
mp1.3 <- margins(p1.3)
summary(mp1.3)
saveRDS(mp1.3,"mp1.3.rds")


# p2.3
p2.3 <- glmer(formula = event ~ clock + ratio_cat2 + ratio_cat2*period + tenure + age + agesq + edu + ukborn + emp + oci  + (1|code),
              data = hhpart3p2,
              family = binomial,
              weights = weight,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))) 
summary(p2.3)
summ(p2.3, exp = TRUE)
saveRDS(p2.3,"p2.3.rds")
mp2.3 <- margins(p2.3)
summary(mp2.3)
saveRDS(mp2.3,"mp2.3.rds")

# p3.3
p3.3 <- glmer(formula = event ~ clock + ratio_cat2 + ratio_cat2*period + tenure + age + agesq + edu + ukborn + emp + oci  + (1|code),
              data = hhpart3p3,
              family = binomial,
              weights = weight,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))) 
summary(p3.3)
summ(p3.3, exp = TRUE)
saveRDS(p3.3,"p3.3.rds")
mp3.3 <- margins(p3.3)
summary(mp3.3)
saveRDS(mp3.3,"mp3.3.rds")


# -------------------------------------------------------------------------
# Output ------------------------------------------------------------------
# -------------------------------------------------------------------------

# Tables Combined
stargazer(p1.3, p2.3, p3.3, 
          apply.coef = exp,
          apply.se   = exp,
          apply.p = exp,
          out = "parity_testing_weights.html",
          column.labels = c("Parity 1", "Parity 2", "Parity 3"))


# Tables Combined (Weighted and Unweighted)
stargazer(p1.1, p1.3, p2.1, p2.3, p3.1, p3.3,
          apply.coef = exp,
          apply.se   = exp,
          out = "parity_testing_weights_compare.html",
          column.labels = c("Parity 1", "Parity 1", "Parity 2", "Parity 2", "Parity 3", "Parity 3"))


test2 <- list(p1.2, p1.3, p2.2, p2.3, p3.2, p3.3)
modelsummary(test2,  output = "test2.html", stars = TRUE, exponentiate = TRUE)

marginstest2 <- list(mp1.2, mp1.3, mp2.2, mp2.3, mp3.2, mp3.3)
modelsummary(marginstest2,  output = "marginstest2.html", stars = TRUE)





# -------------------------------------------------------------------------
# Mediation: Available Space ----------------------------------------------
# -------------------------------------------------------------------------

#Parity 1:

# Step 1: X -> Y (ratio -> event)
medp1x <- glmer(formula = event ~ clock + ratio + ratio*period + tenure + age_cat + agesq + edu + ukborn + emp + (1|code),
              data = hhpart3p1,
              family = binomial,
              weights = weight,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5))) 
summary(medp1x)
summ(medp1x, exp = TRUE)
saveRDS(medp1x,"medp1x.rds")
medp1xm <- margins(medp1x)
summary(medp1xm)

#Testing for adding an "own outright" dummy
medp1xtest <- glmer(formula = event ~ clock + ratio + ratio*period + tenure + agesq + edu + ukborn + emp + ownout + (1|code),
                data = hhpart3p1,
                family = binomial,
                weights = weight,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summ(medp1xtest, exp = TRUE)
saveRDS(medp1xtest,"medp1xtest.rds")



# Step 2: X -> M (ratio -> oci)
medp1m <- glmer(formula = oci ~ clock + ratio + ratio*period + tenure + agesq + edu + ukborn + emp + (1|code),
                data = hhpart3p1,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(medp1m)
summ(medp1m, exp = TRUE)
saveRDS(medp1m,"medp1m.rds")
medp1mm <- margins(medp1m)
summary(medp1mm)

medp1mtest <- glmer(formula = oci ~ clock + ratio + ratio*period + tenure + agesq + edu + ukborn + emp + ownout + (1|code),
                data = hhpart3p1,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summ(medp1mtest, exp = TRUE)
saveRDS(medp1mtest,"medp1mtest.rds")


# Step 3: X -> M -> Y (ratio -> oci -> event)
medp1y <- glmer(formula = event ~ clock + ratio + ratio*period + oci + tenure  + agesq + edu + ukborn  + emp + (1|code),
                data = hhpart3p1,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(medp1y)
summ(medp1y, exp = TRUE)
saveRDS(medp1y,"medp1y.rds")
medp1ym <- margins(medp1y)
summary(medp1ym)

medp1ytest <- glmer(formula = event ~ clock + ratio + ratio*period + oci + tenure  + agesq + edu + ukborn  + emp + ownout + (1|code),
                data = hhpart3p1,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summ(medp1ytest, exp = TRUE)
saveRDS(medp1ytest,"medp1ytest.rds")


# Mediation Analysis
medp1.result <- mediate(medp1m, medp1y, treat = "ratio", mediator = "oci")
summary(medp1.result)
saveRDS(medp1.result,"medp1.result.rds")

# Mediation Analysis: test for "ownout"
medp1test.result <- mediate(medp1mtest, medp1ytest, treat = "ratio", mediator = "oci")
summary(medp1test.result)
saveRDS(medp1test.result,"medp1test.result.rds")







#Parity 2:

# Step 1: X -> Y (ratio -> event)
medp2x <- glmer(formula = event ~ clock + ratio + ratio*period + tenure + agesq + edu + ukborn + emp + (1|code),
                data = hhpart3p2,
                family = binomial,
                weights = weight,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(medp2x)
summ(medp2x, exp = TRUE)
saveRDS(medp2x,"medp2x.rds")


medp2xtest <- glmer(formula = event ~ clock + ratio + ratio*period + tenure + agesq + edu + ukborn + emp + ownout + (1|code),
                data = hhpart3p2,
                family = binomial,
                weights = weight,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 

summ(medp2xtest, exp = TRUE)

# Step 2: X -> M (ratio -> oci)
medp2m <- glmer(formula = oci ~ clock + ratio + ratio*period + tenure + agesq + edu + ukborn + emp + (1|code),
                data = hhpart3p2,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(medp2m)
summ(medp2m, exp = TRUE)
saveRDS(medp2m,"medp2m.rds")

# Step 3: X -> M -> Y (ratio -> oci -> event)
medp2y <- glmer(formula = event ~ clock + ratio + ratio*period + oci + tenure  + agesq + edu + ukborn  + emp + (1|code),
                data = hhpart3p2,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(medp2y)
summ(medp2y, exp = TRUE)
saveRDS(medp2y,"medp2y.rds")

# Mediation Analysis
medp2.result <- mediate(medp2m, medp2y, treat = "ratio", mediator = "oci")
summary(medp2.result)
saveRDS(medp2.result,"medp2.result.rds")








#Parity 3:

# Step 1: X -> Y (ratio -> event)
medp3x <- glmer(formula = event ~ clock + ratio + ratio*period + tenure + agesq + edu + ukborn + emp + (1|code),
                data = hhpart3p3,
                family = binomial,
                weights = weight,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(medp3x)
summ(medp3x, exp = TRUE)
saveRDS(medp3x,"medp3x.rds")

medp3xtest <- glmer(formula = event ~ clock + ratio + ratio*period + tenure + agesq + edu + ukborn + emp + ownout + (1|code),
                    data = hhpart3p3,
                    family = binomial,
                    weights = weight,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5))) 

summ(medp3xtest, exp = TRUE)


# Step 2: X -> M (ratio -> oci)
medp3m <- glmer(formula = oci ~ clock + ratio + ratio*period + tenure + agesq + edu + ukborn + emp + (1|code),
                data = hhpart3p3,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(medp3m)
summ(medp3m, exp = TRUE)
saveRDS(medp3m,"medp3m.rds")

# Step 3: X -> M -> Y (ratio -> oci -> event)
medp3y <- glmer(formula = event ~ clock + ratio + ratio*period + oci + tenure  + agesq + edu + ukborn  + emp + (1|code),
                data = hhpart3p3,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) 
summary(medp3y)
summ(medp3y, exp = TRUE)
saveRDS(medp3y,"medp3y.rds")

# Mediation Analysis
medp3.result <- mediate(medp3m, medp3y, treat = "ratio", mediator = "oci")
summary(medp3.result)
saveRDS(medp3.result,"medp3.result.rds")


# -----------------------------------------------------------------------------
# Output: Mediation Analysis  -------------------------------------------------
# -----------------------------------------------------------------------------


# Tables Combined (unmediated and mediated)
stargazer(medp1x, medp1y, medp2x, medp2y, medp3x, medp3y, 
          apply.coef = exp,
          # apply.se   = exp,
          # align = TRUE,
          out = "mediation_space_byparity.html",
          column.labels = c("Parity 1", "Parity 1", "Parity 2", "Parity 2", "Parity 3", "Parity 3"))


test <- list(medp1x, medp1y, medp2x, medp2y, medp3x, medp3y)
modelsummary(test,  output = "test.html", stars = TRUE, exponentiate = TRUE)
# a1diff_women <- modelsummary(a1modf, coef_map = cm3, output = "huxtable", stars = TRUE)
# quick_docx(a1diff_women, file = "A1diff_Women_AME_S10_20-07-2022.docx", open = FALSE)
# quick_xlsx(a1diff_women, file = "A1diff_Women_AME_S10_20-07-2022.xlsx", open = FALSE)





# Tables Combined (unmediated and mediated)
stargazer(medp1m, medp2m, medp3m,
          apply.coef = exp,
          apply.se   = exp,
          out = "mediation_DV_oci_byparity.html",
          column.labels = c("Parity 1", "Parity 2", "Parity 3"))


# Tables Combined (Weighted and Unweighted)
stargazer(medp1xtest, medp2xtest, medp3xtest,
          apply.coef = exp,
          apply.se   = exp,
          out = "mediation_space_byparity_ownout.html",
          column.labels = c("Parity 1", "Parity 2", "Parity 3"))

effect_plot(medp1x, pred = ratio,  interval = TRUE)
effect_plot(medp1y, pred = ratio)
effect_plot(medp1xtest, pred = ratio)

effect_plot(medp2x, pred = ratio, interval = TRUE)
effect_plot(medp2y, pred = ratio, interval = TRUE)
effect_plot(medp3x, pred = ratio)
effect_plot(medp3y, pred = ratio)



effect_plot(a1, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "a1:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age + Emp",
            y.label = "Experencing a Live Birth")

