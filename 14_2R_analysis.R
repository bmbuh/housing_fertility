#Coded by: Brian Buh
#Started on: 14.02.2023
#Last Updated: 16.03.2023 (heterogeneity testing)

## This script looks at the paper after the revision by MWD

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

hhpart3 %>% count(hhemp)

# DF for parity specific models
hhpart3p1 <- hhpart3 %>% filter(parity == 1)
hhpart3p2 <- hhpart3 %>% filter(parity == 2)
hhpart3p3 <- hhpart3 %>% filter(parity == 3)

hhpart3p1 %>% tabyl(tenure) #owned = 68.2%, #rent = 23.4%, social = 8.4%
hhpart3p2 %>% tabyl(tenure) #owned = 73.3%, #rent = 14.1%, social = 12.6%
hhpart3p3 %>% tabyl(tenure) #owned = 77.5%, #rent = 9.1%, social = 13.4%

# DF for removing parity 3
hhpart4 <- hhpart3 %>% 
  dplyr::filter(parity != 3)


# Heterogeneity testing df
## Age
u29 <- hhpart3 %>% filter(age_cat2 == "u29")
o30 <- hhpart3 %>% filter(age_cat2 == "o30")

## Income
umedinc <- hhpart3 %>% filter(medinc == 0)
umedinc %>% count(event, parity, tenure, ratio_cat2)
omedinc <- hhpart3 %>% filter(medinc == 1)
omedinc %>% count(event, parity, tenure)

#Urban/rural
urban <- hhpart3 %>% filter(urban == 1)
rural <- hhpart3 %>% filter(urban == 2)

# High Price LAD
highlad <- hhpart3 %>% filter(medlowquar == 1)

# Housing crisis
precrisis <- hhpart3 %>% filter(period == "1992-1999" | period == "2000-2007")
postcrisis <- hhpart3 %>% filter(period == "2008-2012" | period == "2013-2022")


###########################################################################
# Analysis ----------------------------------------------------------------
###########################################################################


# Full Analysis
## e1 = interaction ratio*parity*tenure
## e2 = interaction ratio*parity*hhemp

## e3 = e1 no parity 3
## e4 = e2 no parity 3

# Parity specific
## parity 1
### p1e1 = ratio*tenure
### p1e2 = ratio*hhemp

## parity 2
### p2e1 = ratio*tenure
### p2e2 = ratio*hhemp

## parity 3
### p3e1 = ratio*tenure
### p3e2 = ratio*hhemp


# Heterogeneity testing
## Age
### e1u29 = e1 with observations equal to or under 29
### e1o30 = e1 with observations over 30

## Income-level
### e1umedinc = e1 with observations under yearly median hh income
### e1omedinc = e1 with observations equal to or over yearly median hh income

## Urban/rural
### e1urban = e1 with only urban observations
### e1rural = e1 with only rural observations

## Top LAD
### e1highlad = e1 with only LAD with housing above the median low quartile home price

##Crisis
### e1precrisis = e1 with only precrisis observations (aka BHPS)
### e1postcrisis = e1 with only postcrisis observations (aka UKHLS)


# -------------------------------------------------------------------------
# Full Analysis -----------------------------------------------------------
# -------------------------------------------------------------------------

#analysis e1
e1 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(e1)
summ(e1, exp = TRUE)
saveRDS(e1,"e1.rds")
me1 <- margins(e1)
summary(me1)

cat_plot(e1, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         mod2.labels = c("Owned", "Private Rent", "Social Rent"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1_parity_tenure_S14_14-02-2023.png", dpi = 500)


#analysis e2
e2 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*hhemp + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart3,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(e2)
summ(e2, exp = TRUE)
saveRDS(e2,"e2.rds")
me2 <- margins(e2)
summary(me2)

cat_plot(e2, pred = parity, modx = ratio_cat2, mod2 = hhemp,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         mod2.values = c("bothemp", "egoinactive", "egoemp", "bothunemp", "egounemp"),
         mod2.labels = c("Both Employed", "Woman Inactive", "Woman Emp., Man Not", "Both Unemployed", "Woman Unemployed"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e2_parity_hhemp_S14_14-02-2023.png", dpi = 500)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Output ------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Categorical
full_output <- list(e1, e2)
full <- modelsummary(full_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(full, file = "full_S14_14-02-2023.html", open = FALSE)
# quick_docx(mbp, file = "mbp_cat_AME_S10_28-11-2022.docx", open = FALSE)
# quick_xlsx(mbp, file = "mbp_cat_AME_S10_28-11-2022.xlsx", open = FALSE)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# No Parity 3 -------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis e3
e3 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(e3)
summ(e3, exp = TRUE)
saveRDS(e3,"e3.rds")
me3 <- margins(e3)
summary(me3)

cat_plot(e3, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2"),
         mod2.labels = c("Owned", "Private Rent", "Social Rent"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e3_parity_tenure_S14_14-02-2023.png", dpi = 500)


#analysis e4 (!!! the interaction term is NOT significant; it does not improve the model fit. No interact term)
e4 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*hhemp + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(e4)
summ(e4, exp = TRUE)
saveRDS(e4,"e4.rds")
me4 <- margins(e4)
summary(me4)

cat_plot(e4, #I use e3 without the interaction parity*hhemp because I won't use it in my paper
         pred = parity, modx = ratio_cat2, mod2 = hhemp,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2"),
         mod2.values = c("bothemp", "egoinactive", "egoemp", "bothunemp", "egounemp"),
         mod2.labels = c("Both Employed", "Woman Inactive", "Woman Emp., Man Not", "Both Unemployed", "Woman Unemployed"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e4_parity_hhemp_S14_14-02-2023.png", dpi = 500)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Period figure -----------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

e5 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*period + age_cat + agesq + edu + ukborn + tenure + hhemp
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(e5)
summ(e5, exp = TRUE)
saveRDS(e5,"e5.rds")
# me5 <- margins(e5)
# summary(me5)

cat_plot(e5, pred = parity, modx = ratio_cat2, mod2 = period,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2"),
         mod2.labels = c("1992-1999", "2000-2007","2008-2012","2013-2022"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Household income used for housing",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e5_parity_period_S14_14-02-2023.png", dpi = 500)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Output ------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Categorical
full_output <- list(e3, e4, e5)
# cm_cat <- c("clock" = "Clock",
#             "parity2" = "Parity 2",
#             "parity3" = "Parity 3",
#             "ratio_cat20" = "0%",
#             "ratio_cat20.1-10" = "0.1-10%",
#             "ratio_cat220-30" = "20-30%",
#             "ratio_cat230-40" = "30-40%",
#             "ratio_cat240-100" = "40-100%",
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
full <- modelsummary(full_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(full, file = "full(noparity3)_S14_14-02-2023.html", open = FALSE)
# quick_docx(mbp, file = "full(noparity3)_S14_14-02-2023.docx", open = FALSE)
# quick_xlsx(mbp, file = "full(noparity3)_S14_14-02-2023.xlsx", open = FALSE)










# -------------------------------------------------------------------------
# Parity Specific ---------------------------------------------------------
# -------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parity 1 ----------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis p1e1
p1e1 <- glmer(formula = event ~ clock + ratio_cat2*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart3p1,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(p1e1)
summ(p1e1, exp = TRUE)
saveRDS(p1e1,"p1e1.rds")
mp1e1 <- margins(p1e1)
summary(mp1e1)

cat_plot(p1e1, pred = tenure, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 1 - Housing Tenure",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p1e1_p1_tenure_S14_14-02-2023.png", dpi = 500)

#analysis p1e2
p1e2 <- glmer(formula = event ~ clock + ratio_cat2*hhemp + period + age_cat + agesq + edu + ukborn + tenure
              + (1|pidp) + (1|code),
              data = hhpart3p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p1e2)
summ(p1e2, exp = TRUE)
saveRDS(p1e2,"p1e2.rds")
mp1e2 <- margins(p1e2)
summary(mp1e2)

cat_plot(p1e2, pred = hhemp, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 1 - Household Employment",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p1e2_p1_hhemp_S14_14-02-2023.png", dpi = 500)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parity 2 ----------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis p2e1
p2e1 <- glmer(formula = event ~ clock + ratio_cat2*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p2e1)
summ(p2e1, exp = TRUE)
saveRDS(p2e1,"p2e1.rds")
mp2e1 <- margins(p2e1)
summary(mp2e1)

cat_plot(p2e1, pred = tenure, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 2 - Housing Tenure",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p2e1_p1_tenure_S14_14-02-2023.png", dpi = 500)

#analysis p2e2
p2e2 <- glmer(formula = event ~ clock + ratio_cat2*hhemp + period + age_cat + agesq + edu + ukborn + tenure
              + (1|pidp) + (1|code),
              data = hhpart3p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p2e2)
summ(p2e2, exp = TRUE)
saveRDS(p2e2,"p2e2.rds")
mp2e2 <- margins(p2e2)
summary(mp2e2)

cat_plot(p2e2, pred = hhemp, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 2 - Household Employment",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p2e2_p1_hhemp_S14_14-02-2023.png", dpi = 500)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parity 3 ----------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis p3e1
p3e1 <- glmer(formula = event ~ clock + ratio_cat2*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p3e1)
summ(p3e1, exp = TRUE)
saveRDS(p3e1,"p3e1.rds")
mp3e1 <- margins(p3e1)
summary(mp3e1)

cat_plot(p3e1, pred = tenure, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 3 - Housing Tenure",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p3e1_p1_tenure_S14_14-02-2023.png", dpi = 500)

#analysis p3e2
p3e2 <- glmer(formula = event ~ clock + ratio_cat2*hhemp + period + age_cat + agesq + edu + ukborn + tenure
              + (1|pidp) + (1|code),
              data = hhpart3p3,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p3e2)
summ(p3e2, exp = TRUE)
saveRDS(p3e2,"p3e2.rds")
mp3e2 <- margins(p3e2)
summary(mp3e2)

cat_plot(p3e2, pred = hhemp, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 3 - Household Employment",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p3e2_p1_hhemp_S14_14-02-2023.png", dpi = 500)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Output ------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Categorical
parity_output <- list(p1e1, p1e2, p2e1, p2e2, p3e1, p3e2)
# cm_cat <- c("clock" = "Clock",
#             "parity2" = "Parity 2",
#             "parity3" = "Parity 3",
#             "ratio_cat20" = "0%",
#             "ratio_cat20.1-10" = "0.1-10%",
#             "ratio_cat220-30" = "20-30%",
#             "ratio_cat230-40" = "30-40%",
#             "ratio_cat240-100" = "40-100%",
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
quick_html(parity, file = "parity_S14_14-02-2023.html", open = FALSE)






###########################################################################
# Heterogeneity testing ---------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
## Age --------------------------------------------------------------------
# -------------------------------------------------------------------------

### e1u29 = e1 with observations equal to or under 29
#analysis e1u29
e1u29 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = u29,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(e1u29)
summ(e1u29, exp = TRUE)
saveRDS(e1u29,"e1u29.rds")
me1u29 <- margins(e1u29)
summary(me1u29)

cat_plot(e1u29, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Under 29 years old",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1u29_parity_tenure_S14_16-03-2023.png", dpi = 500)

### e1o30 = e1 with observations over 30
e1o30 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = o30,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(e1o30)
summ(e1o30, exp = TRUE)
saveRDS(e1o30,"e1o30.rds")
me1o30 <- margins(e1o30)
summary(me1o30)

cat_plot(e1o30, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Over 30 years old",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1o30_parity_tenure_S14_16-03-2023.png", dpi = 500)

## Income-level
### e1umedinc = e1 with observations under yearly median hh income
e1umedinc <- glmer(formula = event ~ clock*parity + ratio*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp
               + (1|pidp) + (1|code),
               data = umedinc,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight)

summary(e1umedinc)
summ(e1umedinc, exp = TRUE)
saveRDS(e1umedinc,"e1umedinc.rds")
me1umedinc <- margins(e1umedinc)
summary(me1umedinc)

effect_plot(e1umedinc, pred = ratio, force.cat = TRUE)

cat_plot(e1umedinc, pred = parity, modx = ratio, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         # modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         # modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Under the median income",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1umedinc_parity_tenure_S14_16-03-2023.png", dpi = 500)

### e1omedinc = e1 with observations equal to or over yearly median hh income
e1omedinc <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = omedinc,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(e1omedinc)
summ(e1omedinc, exp = TRUE)
saveRDS(e1omedinc,"e1omedinc.rds")
me1omedinc <- margins(e1omedinc)
summary(me1omedinc)

cat_plot(e1omedinc, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Over the median income",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1omedinc_parity_tenure_S14_16-03-2023.png", dpi = 500)

## Urban/rural
### e1urban = e1 with only urban observations
e1urban <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = urban,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(e1urban)
summ(e1urban, exp = TRUE)
saveRDS(e1urban,"e1urban.rds")
me1urban <- margins(e1urban)
summary(me1urban)

cat_plot(e1urban, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Urban",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1urban_parity_tenure_S14_16-03-2023.png", dpi = 500)

### e1rural = e1 with only rural observations
e1rural <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = rural,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(e1rural)
summ(e1rural, exp = TRUE)
saveRDS(e1rural,"e1rural.rds")
me1rural <- margins(e1rural)
summary(me1rural)

cat_plot(e1rural, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Rural",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1rural_parity_tenure_S14_16-03-2023.png", dpi = 500)


## Top LAD
### e1highlad = e1 with only LAD with housing above the median low quartile home price
e1highlad <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = highlad,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(e1highlad)
summ(e1highlad, exp = TRUE)
saveRDS(e1highlad,"e1highlad.rds")
me1highlad <- margins(e1highlad)
summary(me1highlad)

cat_plot(e1highlad, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Only LAD with housing prices over the median lower quartile",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1highlad_parity_tenure_S14_16-03-2023.png", dpi = 500)

##Crisis
### e1precrisis = e1 with only precrisis observations (aka BHPS)
e1precrisis <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = precrisis,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(e1precrisis)
summ(e1precrisis, exp = TRUE)
saveRDS(e1precrisis,"e1precrisis.rds")
me1precrisis <- margins(e1precrisis)
summary(me1precrisis)

cat_plot(e1precrisis, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Precrisis",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1precrisis_parity_tenure_S14_16-03-2023.png", dpi = 500)

### e1postcrisis = e1 with only postcrisis observations (aka UKHLS)
e1postcrisis <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*tenure + age_cat + agesq + edu + ukborn + hhemp 
                     + (1|pidp) + (1|code),
                     data = postcrisis,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight) 

summary(e1postcrisis)
summ(e1postcrisis, exp = TRUE)
saveRDS(e1postcrisis,"e1postcrisis.rds")
me1postcrisis <- margins(e1postcrisis)
summary(me1postcrisis)

cat_plot(e1postcrisis, pred = parity, modx = ratio_cat2, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "0.1-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Post crisis",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("e1postcrisis_parity_tenure_S14_16-03-2023.png", dpi = 500)
