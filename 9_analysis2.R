#Coded by: Brian Buh
#Started on: 06.10.2022
#Last Updated: 

# This script continues from S8 after determining the best fit multilevel model was M15
###Note: For this script I order the model runs using A#


library(tidyverse)
library(haven)
library(arsenal)
library(survival)
library(survminer)
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

#Load data  hhpart (s7)
hhpart <- file.choose()
hhpart <- readRDS(hhpart)

str(hhpart)

# DF for individuals not living with no parents in the household
hhpart2 <- hhpart %>% 
  filter(parenthh == 0)

# DF for individuals not living with no parents in the household AND co-residing with a partner
hhpart3 <- hhpart2 %>% 
  filter(partner != "single")

# DF with weights
hhpart5 <- hhpart3 %>% 
  left_join(., weights, by = c("pidp", "wave"))


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

## Weights
# A7: M15 + weights


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
# emp + ukborn
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





###########################################################################
# Weights -----------------------------------------------------------------
###########################################################################


# Analysis A7 --------------------------------------------------------------
# weights
a7 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + emp + (1|pidp) + (1|code),
            data = hhpart5,
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

























