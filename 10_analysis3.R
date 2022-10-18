#Coded by: Brian Buh
#Started on: 17.10.2022
#Last Updated: 

## This script is final models used in the paper

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

## Three level Models (models b1-b5 add moderators/mediators stepwise)
# b0: First model with a period*ratio_cat2 interaction
# b1: raw effect (event ~ clock*parity + ratio_cat2*parity + period + age_cat + agesq + edu + ukborn)
# b2: b1 + tenure
# b3: b2 + emp
# b4: b3 + oci
# b5: b4 + share



###########################################################################
# Analysis ----------------------------------------------------------------
###########################################################################
b0 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + age_cat + agesq + edu + ukborn 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b0)
summ(b0, exp = TRUE)
saveRDS(b0,"b0.rds")

cat_plot(b0, pred = period, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Step 0: Examining period",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b0_raw_effect_S10_17_10-2022.png", dpi = 300)



#Analysis b1
b1 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + age_cat + agesq + edu + ukborn 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b1)
summ(b1, exp = TRUE)
saveRDS(b1,"b1.rds")

cat_plot(b1, pred = parity, modx = ratio_cat2,
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
         main.title = "Step 1: Raw Effect",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b1_raw_effect_S10_17_10-2022.png", dpi = 300)


#analysis b2
b2 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b2)
summ(b2, exp = TRUE)
saveRDS(b2,"b2.rds")

cat_plot(b2, pred = parity, modx = ratio_cat2,
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
         main.title = "Step 2: + Housing Tenure",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b2_plus_tenure_S10_17_10-2022.png", dpi = 300)

#analysis b3
b3 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + age_cat + agesq + edu + ukborn + tenure + emp 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b3)
summ(b3, exp = TRUE)
saveRDS(b3,"b3.rds")

cat_plot(b3, pred = parity, modx = ratio_cat2,
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
         main.title = "Step 3: + Activity Status",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b3_plus_emp_S10_17_10-2022.png", dpi = 300)

#analysis b4
b4 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b4)
summ(b4, exp = TRUE)
saveRDS(b4,"b4.rds")

cat_plot(b4, pred = parity, modx = ratio_cat2,
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
         main.title = "Step 4: + Overcrowding Index",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b4_plus_oci_S10_17_10-2022.png", dpi = 300)

#analysis b5
b5 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + age_cat + agesq + edu + ukborn + tenure + emp + oci + share 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b5)
summ(b5, exp = TRUE)
saveRDS(b5,"b5.rds")

cat_plot(b5, pred = parity, modx = ratio_cat2,
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
         main.title = "Step 5: + Household Income Share",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b5_plus_share_S10_17_10-2022.png", dpi = 300)



b_output <- list(b1, b2, b3, b4, b5)
modelsummary(b_output,  output = "b_output.html", stars = TRUE, exponentiate = TRUE)
# a1diff_women <- modelsummary(a1modf, coef_map = cm3, output = "huxtable", stars = TRUE)
# quick_docx(a1diff_women, file = "A1diff_Women_AME_S10_20-07-2022.docx", open = FALSE)
# quick_xlsx(a1diff_women, file = "A1diff_Women_AME_S10_20-07-2022.xlsx", open = FALSE)

