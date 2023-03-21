#Coded by: Brian Buh
#Started on: 20.03.2023 
#Last Updated: 21.03.2023

## After analyzing the feedback from MWD and the colloquium I have made the following decisions for the manuscript:
# 1. Drop overcrowding and mediation analysis
# 2. Drop parity 3
# 3. Always include a tenure interaction
# 4. Focus on heterogeneity analysis
#    a. tenure
#    b. hhemp
#    c. ???

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

# DF for parity specific models
hhpart4p1 <- hhpart4 %>% filter(parity == 1)
hhpart4p2 <- hhpart4 %>% filter(parity == 2)


# Heterogeneity testing df
## Age
u29 <- hhpart4 %>% filter(age_cat2 == "u29")
u29 %>% count(ratio_cat3)
o30 <- hhpart4 %>% filter(age_cat2 == "o30")
o30 %>% count(ratio_cat3)

## Income
umedinc <- hhpart4 %>% filter(medinc == 1)
umedinc %>% count(ratio_cat3)
umedinc %>% tabyl(tenure)
summary(umedinc$hhinc)
omedinc <- hhpart4 %>% filter(medinc == 0)
omedinc %>% count(ratio_cat3)
omedinc %>% tabyl(tenure)
summary(omedinc$hhinc)

#Urban/rural
urban <- hhpart4 %>% filter(urban == 1)
rural <- hhpart4 %>% filter(urban == 2)

# High Price LAD
highlad <- hhpart4 %>% 
   filter(!is.na(lowquar), is.na(medlowquar))
summary(highlad$lowquar)
highladnosocial <- highlad %>% 
  filter(tenure != "social")
lowlad <- hhpart4 %>% 
  filter(medlowquar == 1)
summary(lowlad$lowquar)
lowladnosocial <- lowlad %>% 
  filter(tenure != "social")

# Housing crisis
precrisis <- hhpart4 %>% filter(period == "1992-1999" | period == "2000-2007")
postcrisis <- hhpart4 %>% filter(period == "2008-2012" | period == "2013-2022")



# -------------------------------------------------------------------------
# Categorical Variable Tests ----------------------------------------------
# -------------------------------------------------------------------------

#Wald test

wald <- glm(formula = event ~ clock*parity + period + age_cat + agesq + edu + ukborn + hhemp + tenure*parity - 1,
            data = hhpart4,
            family = binomial,
            weights = weight) 

summary(wald)
summ(wald, exp = TRUE)

wald3 <- glm(formula = event ~ clock*parity + period + age_cat + agesq + edu + ukborn + hhemp + tenure*parity*ratio_cat3 - 1,
             data = hhpart4,
             family = binomial,
             weights = weight) 

summary(wald3)
summ(wald3, exp = TRUE)

anova(wald, wald3, test = "LRT")



###########################################################################
# Analysis ----------------------------------------------------------------
###########################################################################


# Full Analysis
## f1 = interaction ratio*parity*tenure
## f2 = interaction ratio*parity*hhemp

# Parity specific
## parity 1
### p1f1 = ratio*tenure
### p1f2 = ratio*hhemp

## parity 2
### p2f1 = ratio*tenure
### p2f2 = ratio*hhemp


# Heterogeneity testing
## Age
### f1u29 = f1 with observations equal to or under 29
### f1o30 = f1 with observations over 30

## Income-level
### f1umedinc = f1 with observations under yearly median hh income
### f1omedinc = f1 with observations equal to or over yearly median hh income

## Urban/rural
### f1urban = f1 with only urban observations
### f1rural = f1 with only rural observations

## LAD
### f1highlad = f1 with only LAD with housing above the median low quartile home price
### f1highladnosocial = f1 with only LAD with housing above the median lower quartile home price not including social housing
### f1lowlad = f1 with only LAD with housing under the median low quartile home price
### f1lowladnosocial = f1 with only LAD with housing under the median lower quartile home price not including social housing


##Crisis
### f1precrisis = f1 with only precrisis observations (aka BHPS)
### f1postcrisis = f1 with only postcrisis observations (aka UKHLS)


# -------------------------------------------------------------------------
# Full Analysis -----------------------------------------------------------
# -------------------------------------------------------------------------

#analysis f1
f1 <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(f1)
summ(f1, exp = TRUE)
saveRDS(f1,"f1.rds")
# mf1 <- margins(f1)
# summary(mf1)

cat_plot(f1, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         mod2.labels = c("Owned", "Private Rent", "Social Rent"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1_parity_tenure_s15_20-03-2023.png", dpi = 500)

cat_plot(f1, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Full model",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1_parity_full_s15_20-03-2023.png", dpi = 500)


#analysis f2
f2 <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*hhemp + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(f2)
summ(f2, exp = TRUE)
saveRDS(f2,"f2.rds")
# mf2 <- margins(f2)
# summary(mf2)

cat_plot(f2, pred = parity, modx = ratio_cat3, mod2 = hhemp,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         mod2.values = c("bothemp", "egoinactive", "egoemp", "bothunemp", "egounemp"),
         mod2.labels = c("Both Employed", "Woman Inactive", "Woman Emp., Man Not", "Both Unemployed", "Woman Unemployed"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f2_parity_hhemp_s15_20-03-2023.png", dpi = 500)


# -------------------------------------------------------------------------
# Parity Specific ---------------------------------------------------------
# -------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parity 1 ----------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis p1f1
p1f1 <- glmer(formula = event ~ clock + ratio_cat3*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart4p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p1f1)
summ(p1f1, exp = TRUE)
saveRDS(p1f1,"p1f1.rds")
# mp1f1 <- margins(p1f1)
# summary(mp1f1)

cat_plot(p1f1, pred = tenure, modx = ratio_cat3, 
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 1 - Housing Tenure",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p1f1_p1_tenure_s15_20-03-2023.png", dpi = 500)

#analysis p1f2
p1f2 <- glmer(formula = event ~ clock + ratio_cat3*hhemp + period + age_cat + agesq + edu + ukborn + tenure
              + (1|pidp) + (1|code),
              data = hhpart4p1,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p1f2)
summ(p1f2, exp = TRUE)
saveRDS(p1f2,"p1f2.rds")
# mp1f2 <- margins(p1f2)
# summary(mp1f2)

cat_plot(p1f2, pred = hhemp, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.values = c("bothemp", "egoinactive", "egoemp", "bothunemp", "egounemp"),
         pred.labels = c("Both Employed", "Woman Inactive", "Woman Emp., Man Not", "Both Unemployed", "Woman Unemployed"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 1 - Household Employment",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p1f2_p1_hhemp_s15_20-03-2023.png", dpi = 500)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parity 2 ----------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis p2f1
p2f1 <- glmer(formula = event ~ clock + ratio_cat3*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart4p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p2f1)
summ(p2f1, exp = TRUE)
saveRDS(p2f1,"p2f1.rds")
# mp2f1 <- margins(p2f1)
# summary(mp2f1)

cat_plot(p2f1, pred = tenure, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         pred.values = c("bothemp", "egoinactive", "egoemp", "bothunemp", "egounemp"),
         pred.labels = c("Both Employed", "Woman Inactive", "Woman Emp., Man Not", "Both Unemployed", "Woman Unemployed"),
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 2 - Housing Tenure",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p2f1_p1_tenure_s15_20-03-2023.png", dpi = 500)

#analysis p2f2
p2f2 <- glmer(formula = event ~ clock + ratio_cat3*hhemp + period + age_cat + agesq + edu + ukborn + tenure
              + (1|pidp) + (1|code),
              data = hhpart4p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(p2f2)
summ(p2f2, exp = TRUE)
saveRDS(p2f2,"p2f2.rds")
# mp2f2 <- margins(p2f2)
# summary(mp2f2)

cat_plot(p2f2, pred = hhemp, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         # pred.labels = c("First birth", "Second birth", "Third birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Parity 2 - Household Employment",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("p2f2_p1_hhemp_s15_20-03-2023.png", dpi = 500)


#Categorical
parity_output <- list(p1f1, p1f2, p2f1, p2f2)
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
quick_html(parity, file = "parity_s15_21-03-2023.html", open = FALSE)
quick_xlsx(parity, file = "parity_s15_21-03-2023.xlsx", open = FALSE)






###########################################################################
# Heterogeneity testing ---------------------------------------------------
###########################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## Age --------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### f1u29 = f1 with observations equal to or under 29
#analysis f1u29
f1u29 <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = u29,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(f1u29)
summ(f1u29, exp = TRUE)
saveRDS(f1u29,"f1u29.rds")
# mf1u29 <- margins(f1u29)
# summary(mf1u29)

cat_plot(f1u29, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Under 29 years old by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1u29_parity_tenure_s15_20-03-2023.png", dpi = 500)

cat_plot(f1u29, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Under 29 years old",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1u29_parity_s15_20-03-2023.png", dpi = 500)



### f1o30 = f1 with observations over 30
f1o30 <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
               + (1|pidp) + (1|code),
               data = o30,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 2e5)),
               weights = weight) 

summary(f1o30)
summ(f1o30, exp = TRUE)
saveRDS(f1o30,"f1o30.rds")
# mf1o30 <- margins(f1o30)
# summary(mf1o30)

#By tenure
cat_plot(f1o30, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Over 30 years old by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1o30_parity_tenure_s15_20-03-2023.png", dpi = 500)

#All housing
cat_plot(f1o30, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Over 30 years old",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1o30_parity_s15_20-03-2023.png", dpi = 500)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Income level ------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### f1umedinc = f1 with observations under yearly median hh income
f1umedinc <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp
                   + (1|pidp) + (1|code),
                   data = umedinc,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight)

summary(f1umedinc)
summ(f1umedinc, exp = TRUE)
saveRDS(f1umedinc,"f1umedinc.rds")
# mf1umedinc <- margins(f1umedinc)
# summary(mf1umedinc)

cat_plot(f1umedinc, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Under the median income",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1umedinc_tenure_s15_20-03-2023.png", dpi = 500)

cat_plot(f1umedinc, pred = parity, modx = ratio_cat3, 
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Under the median income",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1umedinc_parity_s15_20-03-2023.png", dpi = 500)


### f1omedinc = f1 with observations equal to or over yearly median hh income
f1omedinc <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                   + (1|pidp) + (1|code),
                   data = omedinc,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight) 

summary(f1omedinc)
summ(f1omedinc, exp = TRUE)
saveRDS(f1omedinc,"f1omedinc.rds")
# mf1omedinc <- margins(f1omedinc)
# summary(mf1omedinc)

cat_plot(f1omedinc, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Over the median income",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1omedinc_parity_tenure_s15_20-03-2023.png", dpi = 500)

cat_plot(f1omedinc, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Over the median income",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1omedinc_parity_s15_20-03-2023.png", dpi = 500)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Urban/rural -------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

### f1urban = f1 with only urban observations
f1urban <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                 + (1|pidp) + (1|code),
                 data = urban,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(f1urban)
summ(f1urban, exp = TRUE)
saveRDS(f1urban,"f1urban.rds")
# mf1urban <- margins(f1urban)
# summary(mf1urban)

cat_plot(f1urban, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Urban by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1urban_parity_tenure_s15_20-03-2023.png", dpi = 500)

cat_plot(f1urban, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Urban",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1urban_parity_s15_20-03-2023.png", dpi = 500)

### f1rural = f1 with only rural observations
f1rural <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                 + (1|pidp) + (1|code),
                 data = rural,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(f1rural)
summ(f1rural, exp = TRUE)
saveRDS(f1rural,"f1rural.rds")
# mf1rural <- margins(f1rural)
# summary(mf1rural)

cat_plot(f1rural, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Rural by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1rural_parity_tenure_s15_20-03-2023.png", dpi = 500)

cat_plot(f1rural, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Rural",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1rural_parity_s15_20-03-2023.png", dpi = 500)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# LAD ---------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### f1highlad = f1 with only LAD with housing above the median low quartile home price
f1highlad <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                   + (1|pidp) + (1|code),
                   data = highlad,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight) 

summary(f1highlad)
summ(f1highlad, exp = TRUE)
saveRDS(f1highlad,"f1highlad.rds")
# mf1highlad <- margins(f1highlad)
# summary(mf1highlad)

cat_plot(f1highlad, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "highlad: Only LAD with housing prices over the median lower quartile by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1highlad_parity_tenure_s15_20-03-2023.png", dpi = 500)

cat_plot(f1highlad, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "highlad: Only LAD with housing prices over the median lower quartile",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1highlad_parity_s15_20-03-2023.png", dpi = 500)



### f1highladnosocial = f1 dropped social housing to see details
f1highladnosocial <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                   + (1|pidp) + (1|code),
                   data = highladnosocial,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight) 

summary(f1highladnosocial)
summ(f1highladnosocial, exp = TRUE)
saveRDS(f1highladnosocial,"f1highladnosocial.rds")
# mf1highlad <- margins(f1highlad)
# summary(mf1highlad)

cat_plot(f1highladnosocial, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "highlad: Only LAD with housing prices over the median lower quartile by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1highladnosocial_parity_tenure_s15_20-03-2023.png", dpi = 500)




### f1lowlad = f1 with only LAD with housing above the median low quartile home price

f1lowlad <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                   + (1|pidp) + (1|code),
                   data = lowlad,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight) 

summary(f1lowlad)
summ(f1lowlad, exp = TRUE)
saveRDS(f1lowlad,"f1lowlad.rds")
# mf1lowlad <- margins(f1lowlad)
# summary(mf1lowlad)

cat_plot(f1lowlad, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "lowlad: Only LAD with housing prices under the median lower quartile by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1lowlad_parity_tenure_s15_20-03-2023.png", dpi = 500)

cat_plot(f1lowlad, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "lowlad: Only LAD with housing prices under the median lower quartile",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1lowlad_parity_s15_20-03-2023.png", dpi = 500)


f1lowladnosocial <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
                           + (1|pidp) + (1|code),
                           data = lowladnosocial,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 2e5)),
                           weights = weight) 

summary(f1lowladnosocial)
summ(f1lowladnosocial, exp = TRUE)
saveRDS(f1lowladnosocial,"f1lowladnosocial.rds")
# mf1lowlad <- margins(f1lowlad)
# summary(mf1lowlad)



cat_plot(f1lowladnosocial, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "lowlad: Only LAD with housing prices under the median lower quartile by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1lowladnosocial_parity_tenure_s15_20-03-2023.png", dpi = 500)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Crisis -----------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


### f1precrisis = f1 with only precrisis observations (aka BHPS)
f1precrisis <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + age_cat + agesq + edu + ukborn + hhemp 
                     + (1|pidp) + (1|code),
                     data = precrisis,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight) 

summary(f1precrisis)
summ(f1precrisis, exp = TRUE)
saveRDS(f1precrisis,"f1precrisis.rds")
# mf1precrisis <- margins(f1precrisis)
# summary(mf1precrisis)

cat_plot(f1precrisis, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Precrisis by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1precrisis_parity_tenure_s15_20-03-2023.png", dpi = 500)

cat_plot(f1precrisis, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Precrisis",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1precrisis_parity__s15_20-03-2023.png", dpi = 500)

### f1postcrisis = f1 with only postcrisis observations (aka UKHLS)
f1postcrisis <- glmer(formula = event ~ clock*parity + ratio_cat3*parity*tenure + age_cat + agesq + edu + ukborn + hhemp 
                      + (1|pidp) + (1|code),
                      data = postcrisis,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5)),
                      weights = weight) 

summary(f1postcrisis)
summ(f1postcrisis, exp = TRUE)
saveRDS(f1postcrisis,"f1postcrisis.rds")
# mf1postcrisis <- margins(f1postcrisis)
# summary(mf1postcrisis)

cat_plot(f1postcrisis, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Post crisis by housing type",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1postcrisis_parity_tenure_s15_20-03-2023.png", dpi = 500)


cat_plot(f1postcrisis, pred = parity, modx = ratio_cat3,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "Post crisis",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("f1postcrisis_parity_s15_20-03-2023.png", dpi = 500)
