#Coded by: Brian Buh
#Started on: 17.10.2022
#Last Updated: 24.10.2022

## This script is final models used in the paper

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

hhpart4 <- hhpart3 %>% 
  mutate(parity = as.factor(parity),
         parity = fct_relevel(parity, c("2", "1", "3")))




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
# b6: = b4 but with parity releveled so that parity 2 is the reference (hhpart4)
# b7: =b6 but with a continous ratio variable

#Parity specific results
# b0p1: parity 1, testing for period interaction (none)
# b1p1: parity 1, no oci
# b2p1: parity 1, with oci
# b3p1: parity 1, categorical ratio
# b4p1: parity 1, cat ratio + oci
# b5p1: parity 1, raw effect
# b6p1: parity 1, + housing tenure
# b7p1: parity 1, raw effect - categorical
# b8p1: parity 1, + housing tenure - categorical

# b0p2: parity 1, testing for period interaction (none)
# b1p2: parity 2, no oci
# b2p2: parity 2, with oci
# b3p2: parity 2, categorical ratio
# b4p2: parity 2, cat ratio + oci
# b5p2: parity 2, raw effect
# b6p2: parity 2, + housing tenure
# b7p2: parity 1, raw effect - categorical
# b8p2: parity 1, + housing tenure - categorical

# b0p3: parity 1, testing for period interaction (none)
# b1p3: parity 3, no oci
# b2p3: parity 3, with oci
# b3p1: parity 1, categorical ratio
# b4p3: parity 3, cat ratio + oci
# b5p3: parity 3, raw effect
# b6p3: parity 3, + housing tenure
# b7p3: parity 1, raw effect - categorical
# b8p3: parity 1, + housing tenure - categorical



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
ggsave("b0_period_S10_17_10-2022.png", dpi = 300)

cat_plot(b0, pred = period, modx = ratio_cat2, mod2 = parity,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         mod2.labels = c("Parity 1", "Parity 2", "Parity 3"),
         pred.labels = c("1999", "2007",  "2012", "2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Step 0: Examining period",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b0_periodbyparity_S10_19_10-2022.png", dpi = 300)



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
mb1 <- margins(b1)
summary(mb1)

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
mb2 <- margins(b2)
summary(mb2)

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
mb3 <- margins(b3)
summary(mb3)

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
mb4 <- margins(b4)
summary(mb4)

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
mb5 <- margins(b5)
summary(mb5)

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
cm_cat <- c("clock" = "Clock",
            "parity2" = "Parity 2",
            "parity3" = "Parity 3",
            "ratio_cat20" = "0%",
            "ratio_cat20.1-10" = "0.1-10%",
            "ratio_cat220-30" = "20-30%",
            "ratio_cat230-40" = "30-40%",
            "ratio_cat240-100" = "40-100%",
            "tenurerent" = "Private Rent",
            "tenuresocial" = "Social Housing",
            "empunemp" = "Unemployed",
            "empinactive" = "Inactive",
            "empstudent" = "Student",
            "oci" = "Overcrowding Index",
            "shareequal" = "Equal",
            "sharefb" = "Femalebreadwinner",
            "period2000-2007" = "2000-2007",
            "period2008-2012" = "2008-2012",
            "period2013-2021" = "2013-2021",
            "age_cat25-28" = "25-28",
            "age_cat30-34" = "30-34",
            "age_cat25-39" = "35-39",
            "age_cat40-45" = "40-45",
            "agesq" = "Age Squared",
            "edulow" = "Low",
            "edumedium" = "Medium",
            "ukborn1" = "UK Born",
            "clock × parity2" = "clock*parity2")
modelsummary(b_output,  output = "b_output2.html", stars = TRUE, exponentiate = TRUE)
# coef_map = cm_cat,
# a1diff_women <- modelsummary(a1modf, coef_map = cm3, output = "huxtable", stars = TRUE)
# quick_docx(a1diff_women, file = "A1diff_Women_AME_S10_20-07-2022.docx", open = FALSE)
# quick_xlsx(a1diff_women, file = "A1diff_Women_AME_S10_20-07-2022.xlsx", open = FALSE)



#analysis b6: same analysis as b4 but with a releveled parity so that parity 2 is the reference
b6 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b6)
summ(b6, exp = TRUE)
saveRDS(b6,"b6.rds")


#analysis b7: b6 but using a continuous variable for ratio
b7 <- glmer(formula = event ~ clock*parity + ratio*parity + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b7)
summ(b7, exp = TRUE)
saveRDS(b7,"b7.rds")

# -----------------------------------------------------------------  --------
# Continuous variable -----------------------------------------------------
# -------------------------------------------------------------------------

#Analysis b8
b8 <- glmer(formula = event ~ clock*parity + ratio*parity + period + age_cat + agesq + edu + ukborn 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b8)
summ(b8, exp = TRUE)
saveRDS(b8,"b8.rds")



#analysis b9
b9 <- glmer(formula = event ~ clock*parity + ratio*parity + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b9)
summ(b9, exp = TRUE)
saveRDS(b9,"b9.rds")


#analysis b10
b10 <- glmer(formula = event ~ clock*parity + ratio*parity + period + age_cat + agesq + edu + ukborn + tenure + emp 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b10)
summ(b10, exp = TRUE)
saveRDS(b10,"b10.rds")

#analysis b11
b11 <- glmer(formula = event ~ clock*parity + ratio*parity + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(b11)
summ(b11, exp = TRUE)
saveRDS(b11,"b11.rds")



b_output_cont <- list(b8, b9, b10, b11)
modelsummary(b_output_cont,  output = "b_output_cont.html", stars = TRUE, exponentiate = TRUE)
# b_output_cont <- modelsummary(a1modf, output = "huxtable", stars = TRUE)
# quick_docx(b_output_cont, file = "b_output_cont_AME_S10_21-07-2022.docx", open = FALSE)
# quick_xlsx(b_output_cont, file = "b_output_cont_AME_S10_21-07-2022.xlsx", open = FALSE)

###########################################################################
# parity specific analysis ------------------------------------------------
###########################################################################

b0p1 <- glmer(formula = event ~ clock + ratio*period + age_cat + agesq + edu + ukborn + tenure + emp 
              + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b0p1)
summ(b0p1, exp = TRUE)
saveRDS(b0p1,"b0p1.rds")
mb0p1 <- margins(b0p1)
saveRDS(mb0p1,"mb0p1.rds")

# -------------------------------------------------------------------------
# Parity 1 ----------------------------------------------------------------
# -------------------------------------------------------------------------

b1p1 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure + emp 
               + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b1p1)
summ(b1p1, exp = TRUE)
saveRDS(b1p1,"b1p1.rds")
mb1p1 <- margins(b1p1)
saveRDS(mb1p1,"mb1p1.rds")


b2p1 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
               + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b2p1)
summ(b2p1, exp = TRUE)
saveRDS(b2p1,"b2p1.rds")
mb2p1 <- margins(b2p1)
saveRDS(mb2p1,"mb2p1.rds")


b3p1 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn + tenure + emp 
              + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b3p1)
summ(b3p1, exp = TRUE)
saveRDS(b3p1,"b3p1.rds")
mb3p1 <- margins(b3p1)
saveRDS(mb3p1,"mb3p1.rds")

cat_plot(b3p1, pred = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         pred.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         x.label = "",
         y.label = "Pr(Experencing a Live First Birth)",
         main.title = "Parity 1, no OCI",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b3p1_parity1_no_oci_S10_20_10-2022.png", dpi = 300)


b4p1 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
              + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b4p1)
summ(b4p1, exp = TRUE)
saveRDS(b4p1,"b4p1.rds")
mb4p1 <- margins(b4p1)
saveRDS(mb4p1,"mb4p1.rds")


cat_plot(b4p1, pred = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         pred.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         x.label = "",
         y.label = "Pr(Experencing a Live First Birth)",
         main.title = "Parity 1, with OCI",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b4p1_parity1_no_oci_S10_20_10-2022.png", dpi = 300)



#How much does OCI impact the likelihood to have the first child if we don't factor in housing costs?
ocitest<- glmer(formula = event ~ clock + age_cat + agesq + edu + ukborn + tenure + emp + oci + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 
summary(ocitest)
mocitest <- margins(ocitest)
summary(mocitest)
#Results - the OCI increases from 0.05*** to 0.08*** which is a marginal increase (when adding controls, this goes back down to 0.04***)

#Steps 1 and 2 on continuous variable
b5p1 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn 
              + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b5p1)
summ(b5p1, exp = TRUE)
saveRDS(b5p1,"b5p1.rds")
mb5p1 <- margins(b5p1)
saveRDS(mb5p1,"mb5p1.rds")


b6p1 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure
              + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b6p1)
summ(b6p1, exp = TRUE)
saveRDS(b6p1,"b6p1.rds")
mb6p1 <- margins(b6p1)
saveRDS(mb6p1,"mb6p1.rds")

#Steps 1 and 2 on categorical variable
b7p1 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn 
              + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b7p1)
summ(b7p1, exp = TRUE)
saveRDS(b7p1,"b7p1.rds")
mb7p1 <- margins(b7p1)
saveRDS(mb7p1,"mb7p1.rds")


b8p1 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn + tenure
              + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b8p1)
summ(b8p1, exp = TRUE)
saveRDS(b8p1,"b8p1.rds")
mb8p1 <- margins(b8p1)
saveRDS(mb8p1,"mb8p1.rds")

# -------------------------------------------------------------------------
# Parity 2 ----------------------------------------------------------------
# -------------------------------------------------------------------------

b0p2 <- glmer(formula = event ~ clock + ratio*period + age_cat + agesq + edu + ukborn + tenure + emp 
              + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b0p2)
summ(b0p2, exp = TRUE)
saveRDS(b0p2,"b0p2.rds")
mb0p2 <- margins(b0p2)
saveRDS(mb0p2,"mb0p2.rds")

b1p2 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure + emp 
               + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b1p2)
summ(b1p2, exp = TRUE)
saveRDS(b1p2,"b1p2.rds")
mb1p2 <- margins(b1p2)
saveRDS(mb1p2,"mb1p2.rds")



b2p2 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
               + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b2p2)
summ(b2p2, exp = TRUE)
saveRDS(b2p2,"b2p2.rds")
mb2p2 <- margins(b2p2)
saveRDS(mb2p2,"mb2p2.rds")

b3p2 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn + tenure + emp 
              + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b3p2)
summ(b3p2, exp = TRUE)
saveRDS(b3p2,"b3p2.rds")
mb3p2 <- margins(b3p2)
saveRDS(mb3p2,"mb3p2.rds")


cat_plot(b3p2, pred = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         pred.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         x.label = "",
         y.label = "Pr(Experencing a Live Second Birth)",
         main.title = "Parity 2, no OCI",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b3p2_parity1_no_oci_S10_20_10-2022.png", dpi = 300)


b4p2 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
              + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b4p2)
summ(b4p2, exp = TRUE)
saveRDS(b4p2,"b4p2.rds")
mb4p2 <- margins(b4p2)
summary(mb4p2)
saveRDS(mb4p2,"mb4p2.rds")


cat_plot(b4p2, pred = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         pred.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         x.label = "",
         y.label = "Pr(Experencing a Live Second Birth)",
         main.title = "Parity 2, with OCI",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b4p2_parity1_no_oci_S10_20_10-2022.png", dpi = 300)

#Steps 1 and 2 on continuous variable
b5p2 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn 
              + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b5p2)
summ(b5p2, exp = TRUE)
saveRDS(b5p2,"b5p2.rds")
mb5p2 <- margins(b5p2)
saveRDS(mb5p2,"mb5p2.rds")


b6p2 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure
              + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b6p2)
summ(b6p2, exp = TRUE)
saveRDS(b6p2,"b6p2.rds")
mb6p2 <- margins(b6p2)
saveRDS(mb6p2,"mb6p2.rds")

#Steps 1 and 2 on categorical variable
b7p2 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn 
              + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b7p2)
summ(b7p2, exp = TRUE)
saveRDS(b7p2,"b7p2.rds")
mb7p2 <- margins(b7p2)
saveRDS(mb7p2,"mb7p2.rds")


b8p2 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn + tenure
              + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b8p2)
summ(b8p2, exp = TRUE)
saveRDS(b8p2,"b8p2.rds")
mb8p2 <- margins(b8p2)
saveRDS(mb8p2,"mb8p2.rds")

# -------------------------------------------------------------------------
# Parity 3 ----------------------------------------------------------------
# -------------------------------------------------------------------------

b0p3 <- glmer(formula = event ~ clock + ratio*period + age_cat + agesq + edu + ukborn + tenure + emp 
              + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b0p3)
summ(b0p3, exp = TRUE)
saveRDS(b0p3,"b0p3.rds")
mb0p3 <- margins(b0p3)
saveRDS(mb0p3,"mb0p3.rds")

b1p3 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure + emp 
               + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b1p3)
summ(b1p3, exp = TRUE)
saveRDS(b1p3,"b1p3.rds")
mb1p3 <- margins(b1p3)
saveRDS(mb1p3,"mb1p3.rds")



b2p3 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
               + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b2p3)
summ(b2p3, exp = TRUE)
saveRDS(b2p3,"b2p3.rds")
mb2p3 <- margins(b2p3)
saveRDS(mb2p3,"mb2p3.rds")

b3p3 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn + tenure + emp 
              + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b3p3)
summ(b3p3, exp = TRUE)
saveRDS(b3p3,"b3p3.rds")
mb3p3 <- margins(b3p3)
saveRDS(mb3p3,"mb3p3.rds")

cat_plot(b3p3, pred = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         pred.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         x.label = "",
         y.label = "Pr(Experencing a Live Third Birth)",
         main.title = "Parity 3, no OCI",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b3p3_parity1_no_oci_S10_20_10-2022.png", dpi = 300)


b4p3 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
              + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b4p3)
summ(b4p3, exp = TRUE)
saveRDS(b4p3,"b4p3.rds")
mb4p3 <- margins(b4p3)
summary(mb4p3)
saveRDS(mb4p3,"mb4p3.rds")


cat_plot(b4p3, pred = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         pred.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         x.label = "",
         y.label = "Pr(Experencing a Live Third Birth)",
         main.title = "Parity 3, with OCI",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("b4p3_parity1_no_oci_S10_30_10-2022.png", dpi = 300)

#Steps 1 and 2 on continuous variable
b5p3 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn 
              + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b5p3)
summ(b5p3, exp = TRUE)
saveRDS(b5p3,"b5p3.rds")
mb5p3 <- margins(b5p3)
saveRDS(mb5p3,"mb5p3.rds")


b6p3 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure
              + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b6p3)
summ(b6p3, exp = TRUE)
saveRDS(b6p3,"b6p3.rds")
mb6p3 <- margins(b6p3)
summary(mb6p3)
saveRDS(mb6p3,"mb6p3.rds")

#Steps 1 and 2 on categorical variable
b7p3 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn 
              + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b7p3)
summ(b7p3, exp = TRUE)
saveRDS(b7p3,"b7p3.rds")
mb7p3 <- margins(b7p3)
saveRDS(mb7p3,"mb7p3.rds")


b8p3 <- glmer(formula = event ~ clock + ratio_cat2 + period + age_cat + agesq + edu + ukborn + tenure
              + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b8p3)
summ(b8p3, exp = TRUE)
saveRDS(b8p3,"b8p3.rds")
mb8p3 <- margins(b8p3)
saveRDS(mb8p3,"mb8p3.rds")

# -------------------------------------------------------------------------
# Output ------------------------------------------------------------------
# -------------------------------------------------------------------------

#Continious
mbp_output <- list(mb1p1, mb2p1, mb1p2, mb2p2, mb1p3, mb2p3)
cm_cont <- c("clock" = "Clock",
            "parity2" = "Parity 2",
            "parity3" = "Parity 3",
            "ratio" = "Ratio of Housing Costs to Household Income",
            "tenurerent" = "Private Rent",
            "tenuresocial" = "Social Housing",
            "empunemp" = "Unemployed",
            "empinactive" = "Inactive",
            "empstudent" = "Student",
            "oci" = "Overcrowding Index",
            "shareequal" = "Equal",
            "sharefb" = "Femalebreadwinner",
            "period2000-2007" = "2000-2007",
            "period2008-2012" = "2008-2012",
            "period2013-2021" = "2013-2021",
            "age_cat25-28" = "25-28",
            "age_cat30-34" = "30-34",
            "age_cat25-39" = "35-39",
            "age_cat40-45" = "40-45",
            "agesq" = "Age Squared",
            "edulow" = "Low",
            "edumedium" = "Medium",
            "ukborn1" = "UK Born")
modelsummary(mbp_output, coef_map = cm_cont, output = "mbp_output_cont_AME_s10_21-10-2022.html", stars = TRUE)
mbp_cont <- modelsummary(mbp_output, output = "huxtable", stars = TRUE)
# quick_docx(mbp_cont, file = "mbp_cont_AME_S10_21-10-2022.docx", open = FALSE)
quick_xlsx(mbp_cont, file = "mbp_cont_AME_S10_21-10-2022.xlsx", open = FALSE)


#Categorical
mbp_output_cat <- list(mb3p1, mb4p1, mb3p2, mb4p2, mb3p3, mb4p3)
cm_cat <- c("clock" = "Clock",
             "parity2" = "Parity 2",
             "parity3" = "Parity 3",
             "ratio_cat20" = "0%",
             "ratio_cat20.1-10" = "0.1-10%",
             "ratio_cat220-30" = "20-30%",
             "ratio_cat230-40" = "30-40%",
             "ratio_cat240-100" = "40-100%",
             "tenurerent" = "Private Rent",
             "tenuresocial" = "Social Housing",
             "empunemp" = "Unemployed",
             "empinactive" = "Inactive",
             "empstudent" = "Student",
             "oci" = "Overcrowding Index",
             "shareequal" = "Equal",
             "sharefb" = "Femalebreadwinner",
             "period2000-2007" = "2000-2007",
             "period2008-2012" = "2008-2012",
             "period2013-2021" = "2013-2021",
             "age_cat25-28" = "25-28",
             "age_cat30-34" = "30-34",
             "age_cat25-39" = "35-39",
             "age_cat40-45" = "40-45",
             "agesq" = "Age Squared",
             "edulow" = "Low",
             "edumedium" = "Medium",
             "ukborn1" = "UK Born")
modelsummary(mbp_output_cat, coef_map = cm_cat, output = "mbp_output_cat_AME_s10_21-10-2022.html", stars = TRUE)
mbp_cat <- modelsummary(mbp_output_cat, output = "huxtable", stars = TRUE)
# quick_docx(mbp, file = "mbp_cat_AME_S10_19-10-2022.docx", open = FALSE)
quick_xlsx(mbp, file = "mbp_cat_AME_S10_19-10-2022.xlsx", open = FALSE)

#Continous
#Parity 1
mbp_output_parity1 <- list(mb5p1, mb6p1, mb1p1, mb2p1)
cm_cont_par <- c("clock" = "Clock",
             "ratio" = "Ratio of Housing Costs to Household Income",
             "tenurerent" = "Private Rent",
             "tenuresocial" = "Social Housing",
             "empunemp" = "Unemployed",
             "empinactive" = "Inactive",
             "empstudent" = "Student",
             "oci" = "Overcrowding Index",
             "shareequal" = "Equal",
             "sharefb" = "Femalebreadwinner",
             "period2000-2007" = "2000-2007",
             "period2008-2012" = "2008-2012",
             "period2013-2021" = "2013-2021",
             "age_cat25-28" = "25-28",
             "age_cat30-34" = "30-34",
             "age_cat25-39" = "35-39",
             "age_cat40-45" = "40-45",
             "agesq" = "Age Squared",
             "edulow" = "Low",
             "edumedium" = "Medium",
             "ukborn1" = "UK Born")
modelsummary(mbp_output_parity1, coef_map = cm_cont_par, output = "mbp_output_p1cont_AME_s10_21-10-2022.html", stars = TRUE)
mbp_cont_parity1 <- modelsummary(mbp_output_parity1, output = "huxtable", stars = TRUE)
# quick_docx(mbp_cont_parity1, file = "mbp1_cont_AME_S10_21-10-2022.docx", open = FALSE)
quick_xlsx(mbp_cont_parity1, file = "mbp1_cont_AME_S10_21-10-2022.xlsx", open = FALSE)

#Parity 2
mbp_output_parity2 <- list(mb5p2, mb6p2, mb1p2, mb2p2)
modelsummary(mbp_output_parity2, coef_map = cm_cont_par, output = "mbp_output_p2cont_AME_s10_21-10-2022.html", stars = TRUE)
mbp_cont_parity2 <- modelsummary(mbp_output_parity2, output = "huxtable", stars = TRUE)
# quick_docx(mbp_cont_parity2, file = "mbp2_cont_AME_S10_21-10-2022.docx", open = FALSE)
quick_xlsx(mbp_cont_parity2, file = "mbp2_cont_AME_S10_21-10-2022.xlsx", open = FALSE)

#Parity 3
mbp_output_parity3 <- list(mb5p3, mb6p3, mb1p3, mb2p3)
modelsummary(mbp_output_parity3, coef_map = cm_cont_par, output = "mbp_output_p3cont_AME_s10_21-10-2022.html", stars = TRUE)
mbp_cont_parity3 <- modelsummary(mbp_output_parity3, output = "huxtable", stars = TRUE)
# quick_docx(mbp_cont_parity3, file = "mbp3_cont_AME_S10_21-10-2022.docx", open = FALSE)
quick_xlsx(mbp_cont_parity3, file = "mbp3_cont_AME_S10_21-10-2022.xlsx", open = FALSE)


#Categorical
#Parity 1
mbp_output_parity1_cat <- list(mb7p1, mb8p1, mb3p1, mb4p1)
cm_cat_par <- c("clock" = "Clock",
            "ratio_cat20" = "0%",
            "ratio_cat20.1-10" = "0.1-10%",
            "ratio_cat220-30" = "20-30%",
            "ratio_cat230-40" = "30-40%",
            "ratio_cat240-100" = "40-100%",
            "tenurerent" = "Private Rent",
            "tenuresocial" = "Social Housing",
            "empunemp" = "Unemployed",
            "empinactive" = "Inactive",
            "empstudent" = "Student",
            "oci" = "Overcrowding Index",
            "shareequal" = "Equal",
            "sharefb" = "Femalebreadwinner",
            "period2000-2007" = "2000-2007",
            "period2008-2012" = "2008-2012",
            "period2013-2021" = "2013-2021",
            "age_cat25-28" = "25-28",
            "age_cat30-34" = "30-34",
            "age_cat25-39" = "35-39",
            "age_cat40-45" = "40-45",
            "agesq" = "Age Squared",
            "edulow" = "Low",
            "edumedium" = "Medium",
            "ukborn1" = "UK Born")
modelsummary(mbp_output_parity1_cat, coef_map = cm_cat_par, output = "mbp_output_p1cat_AME_s10_24-10-2022.html", stars = TRUE)
mbp_cat_parity1 <- modelsummary(mbp_output_parity1_cat, output = "huxtable", stars = TRUE)
# quick_docx(mbp_cat_parity1, file = "mbp1_cat_AME_S10_24-10-2022.docx", open = FALSE)
quick_xlsx(mbp_cat_parity1, file = "mbp1_cat_AME_S10_24-10-2022.xlsx", open = FALSE)

#Parity 2
mbp_cat_parity2 <- list(mb7p2, mb8p2, mb3p2, mb4p2)
modelsummary(mbp_cat_parity2, coef_map = cm_cat_par, output = "mbp_output_p2cat_AME_s10_24-10-2022.html", stars = TRUE)
mbp_cat_parity2_output <- modelsummary(mbp_cat_parity2, output = "huxtable", stars = TRUE)
# quick_docx(mbp_cat_parity2, file = "mbp2_cat_AME_S10_24-10-2022.docx", open = FALSE)
quick_xlsx(mbp_cat_parity2_output, file = "mbp2_cat_AME_S10_24-10-2022.xlsx", open = FALSE)

#Parity 3
mbp_cat_parity3 <- list(mb7p3, mb8p3, mb3p3, mb4p3)
modelsummary(mbp_cat_parity3, coef_map = cm_cat_par, output = "mbp_output_p3cat_AME_s10_24-10-2022.html", stars = TRUE)
mbp_cat_parity3_output <- modelsummary(mbp_cat_parity3, output = "huxtable", stars = TRUE)
# quick_docx(mbp_cat_parity3, file = "mbp3_cat_AME_S10_24-10-2022.docx", open = FALSE)
quick_xlsx(mbp_cat_parity3_output, file = "mbp3_cat_AME_S10_24-10-2022.xlsx", open = FALSE)



############################################################################
# Worrisome Test ----------------------------------------------------------
############################################################################


b10p1 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure
              + (1|pidp) + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b10p1)
summ(b10p1, exp = TRUE)
saveRDS(b10p1,"b10p1.rds")
mb10p1 <- margins(b10p1)
summary(mb10p1)
saveRDS(mb10p1,"mb10p1.rds")


b11p1 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure + emp 
               + (1|pidp) + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b11p1)
summ(b11p1, exp = TRUE)
saveRDS(b11p1,"b11p1.rds")
mb11p1 <- margins(b11p1)
saveRDS(mb11p1,"mb11p1.rds")


b12p1 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure + emp + oci 
               + (1|pidp) + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(b12p1)
summ(b12p1, exp = TRUE)
saveRDS(b12p1,"b12p1.rds")
mb12p1 <- margins(b12p1)
saveRDS(mb12p1,"mb12p1.rds")

#Testing the 3level model
#Parity 1
mbp_output_parity1_3level <- list(mb10p1, mb11p1, mb12p1)
cm_cont_par <- c("clock" = "Clock",
                 "ratio" = "Ratio of Housing Costs to Household Income",
                 "tenurerent" = "Private Rent",
                 "tenuresocial" = "Social Housing",
                 "empunemp" = "Unemployed",
                 "empinactive" = "Inactive",
                 "empstudent" = "Student",
                 "oci" = "Overcrowding Index",
                 "oci2" = "Overcrowding Index 2",
                 "shareequal" = "Equal",
                 "sharefb" = "Femalebreadwinner",
                 "period2000-2007" = "2000-2007",
                 "period2008-2012" = "2008-2012",
                 "period2013-2021" = "2013-2021",
                 "age_cat25-28" = "25-28",
                 "age_cat30-34" = "30-34",
                 "age_cat25-39" = "35-39",
                 "age_cat40-45" = "40-45",
                 "agesq" = "Age Squared",
                 "edulow" = "Low",
                 "edumedium" = "Medium",
                 "ukborn1" = "UK Born")
modelsummary(mbp_output_parity1_3level, coef_map = cm_cont_par, output = "mbp_output_p1cont_3level_AME_s10_23-11-2022.html", stars = TRUE)
mbp_cont_parity1_3level <- modelsummary(mbp_output_parity1_3level, output = "huxtable", stars = TRUE)
# quick_docx(mbp_cont_parity1, file = "mbp_output_p1cont_3level_AME_s10_23-11-2022.docx", open = FALSE)
quick_xlsx(mbp_cont_parity1, file = "mbp_output_p1cont_3level_AME_s10_23-11-2022.xlsx", open = FALSE)



#Testing oci2
b13p1 <- glmer(formula = event ~ clock + ratio + period + age_cat + agesq + edu + ukborn + tenure + emp + oci2 
               + (1|pidp) + (1|code),
               data = hhpart3p1,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(b13p1)
summ(b13p1, exp = TRUE)
saveRDS(b13p1,"b13p1.rds")
mb13p1 <- margins(b13p1)
saveRDS(mb13p1,"mb13p1.rds")

#Continous
#Parity 1
mbp_output_parity1_ocitest <- list(mb12p1, mb13p1)
modelsummary(mbp_output_parity1_ocitest, coef_map = cm_cont_par, output = "mbp_output_p1cont_ocitest_AME_s10_23-11-2022.html", stars = TRUE)
