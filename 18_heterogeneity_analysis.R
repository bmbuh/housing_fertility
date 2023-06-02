#Coded by: Brian Buh
#Started on: 19.04.2023 
#Last Updated: 

# This script prepares the final parity specific runs for draft 6 for the heterogeneity analysis
# Median income
# pre/post crisis
# urban/rural

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
library(cowplot) #For plot_grid

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
  dplyr::filter(parity != 3) %>% 
  mutate(event.chr = as.character(event), #For descriptive stats
         crisis = case_when(period == "1991-1999" | period == "2000-2007" ~ "pre",
                            period == "2008-2012" | period == "2013-2022" ~ "post"),
         medlowquar = ifelse(!is.na(lowquar) & is.na(medlowquar), 0, medlowquar),
         medlad = case_when(medlowquar == 0 ~ "high",
                            medlowquar == 1 ~ "low"),
         hhemp2 = fct_relevel(hhemp, c("bothemp", "egoinactive", "egoemp",  "bothunemp", "egounemp")))

# DF for parity specific models
hhpart4p1 <- hhpart4 %>% filter(parity == 1)
hhpart4p2 <- hhpart4 %>% filter(parity == 2)

## Income
umedincp1 <- hhpart4p1 %>% filter(medinc == 1, tenure != "social")
umedincp2 <- hhpart4p2 %>% filter(medinc == 1, tenure != "social")

omedincp1 <- hhpart4p1 %>% filter(medinc == 0, tenure != "social")
omedincp2 <- hhpart4p2 %>% filter(medinc == 0, tenure != "social")

# Housing crisis
precrisisp1 <- hhpart4p1 %>% filter(period == "1991-1999" | period == "2000-2007") %>% filter(tenure != "social")
precrisisp2 <- hhpart4p2 %>% filter(period == "1991-1999" | period == "2000-2007") %>% filter(tenure != "social")

postcrisisp1 <- hhpart4p1 %>% filter(period == "2008-2012" | period == "2013-2022") %>% filter(tenure != "social")
postcrisisp2 <- hhpart4p2 %>% filter(period == "2008-2012" | period == "2013-2022") %>% filter(tenure != "social")

#Urban/rural
urbanp1 <- hhpart4p1 %>% filter(urban == 1, tenure != "social")
urbanp2 <- hhpart4p2 %>% filter(urban == 1, tenure != "social")

ruralp1 <- hhpart4p1 %>% filter(urban == 2, tenure != "social")
ruralp2 <- hhpart4p2 %>% filter(urban == 2, tenure != "social")


###########################################################################
# Model runs --------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# median income -----------------------------------------------------------
# -------------------------------------------------------------------------

### g1umedinc = under median income parity 1
g1umedincp1 <- glmer(formula = event ~ clock + ratio_cat3*tenure + period + age_cat + agesq + edu + ukborn + hhemp
                   + (1|pidp) + (1|code),
                   data = umedincp1,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight)

summary(g1umedincp1)
summ(g1umedincp1, exp = TRUE)
saveRDS(g1umedincp1,"g1umedincp1.rds")


### g1umedinc = under median income parity 2
g1umedincp2 <- glmer(formula = event ~ clock + ratio_cat3*tenure + period + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = umedincp2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(g1umedincp2)
summ(g1umedincp2, exp = TRUE)
saveRDS(g1umedincp2,"g1umedincp2.rds")

### g1omedinc = over median income parity 1
g1omedincp1 <- glmer(formula = event ~ clock + ratio_cat3*tenure + period + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = omedincp1,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(g1omedincp1)
summ(g1omedincp1, exp = TRUE)
saveRDS(g1omedincp1,"g1omedincp1.rds")


### g1omedinc = over median income parity 2
g1omedincp2 <- glmer(formula = event ~ clock + ratio_cat3*tenure + period + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = omedincp2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(g1omedincp2)
summ(g1omedincp2, exp = TRUE)
saveRDS(g1omedincp2,"g1omedincp2.rds")


# -------------------------------------------------------------------------
# pre/post crisis -----------------------------------------------------------
# -------------------------------------------------------------------------

### g1precrisis = precrisis parity 1
g1precrisisp1 <- glmer(formula = event ~ clock + ratio_cat3*tenure  + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = precrisisp1,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(g1precrisisp1)
summ(g1precrisisp1, exp = TRUE)
saveRDS(g1precrisisp1,"g1precrisisp1.rds")


### g1precrisis = precrisis parity 2
g1precrisisp2 <- glmer(formula = event ~ clock + ratio_cat3*tenure  + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = precrisisp2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(g1precrisisp2)
summ(g1precrisisp2, exp = TRUE)
saveRDS(g1precrisisp2,"g1precrisisp2.rds")

### g1postcrisis = postcrisis parity 1
g1postcrisisp1 <- glmer(formula = event ~ clock + ratio_cat3*tenure  + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = postcrisisp1,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(g1postcrisisp1)
summ(g1postcrisisp1, exp = TRUE)
saveRDS(g1postcrisisp1,"g1postcrisisp1.rds")


### g1postcrisis = postcrisis parity 2
g1postcrisisp2 <- glmer(formula = event ~ clock + ratio_cat3*tenure  + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = postcrisisp2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(g1postcrisisp2)
summ(g1postcrisisp2, exp = TRUE)
saveRDS(g1postcrisisp2,"g1postcrisisp2.rds")



# -------------------------------------------------------------------------
# urban/rural -------------------------------------------------------------
# -------------------------------------------------------------------------

### g1urban = urban parity 1
g1urbanp1 <- glmer(formula = event ~ clock + ratio_cat3*tenure  + age_cat + agesq + edu + ukborn + hhemp
                       + (1|pidp) + (1|code),
                       data = urbanp1,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5)),
                       weights = weight)

summary(g1urbanp1)
summ(g1urbanp1, exp = TRUE)
saveRDS(g1urbanp1,"g1urbanp1.rds")


### g1urban = urban parity 2
g1urbanp2 <- glmer(formula = event ~ clock + ratio_cat3*tenure  + age_cat + agesq + edu + ukborn + hhemp
                       + (1|pidp) + (1|code),
                       data = urbanp2,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5)),
                       weights = weight)

summary(g1urbanp2)
summ(g1urbanp2, exp = TRUE)
saveRDS(g1urbanp2,"g1urbanp2.rds")

### g1rural = rural parity 1
g1ruralp1 <- glmer(formula = event ~ clock + ratio_cat3*tenure  + age_cat + agesq + edu + ukborn + hhemp
                        + (1|pidp) + (1|code),
                        data = ruralp1,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)),
                        weights = weight)

summary(g1ruralp1)
summ(g1ruralp1, exp = TRUE)
saveRDS(g1ruralp1,"g1ruralp1.rds")


### g1rural = rural parity 2
g1ruralp2 <- glmer(formula = event ~ clock + ratio_cat3*tenure  + age_cat + agesq + edu + ukborn + hhemp
                        + (1|pidp) + (1|code),
                        data = ruralp2,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)),
                        weights = weight)

summary(g1ruralp2)
summ(g1ruralp2, exp = TRUE)
saveRDS(g1ruralp2,"g1ruralp2.rds")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Output ------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#median income
medinc_output <- list(g1umedincp1, g1umedincp2, g1omedincp1, g1omedincp2)
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
medinc <- modelsummary(medinc_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(medinc, file = "medinc_S18_18-04-2023.html", open = FALSE)
quick_xlsx(medinc, file = "medinc_S18_18-04-2023.xlsx", open = FALSE)

#Pre/post crisis
crisis_output <- list(g1precrisisp1, g1precrisisp2, g1postcrisisp1, g1postcrisisp2)
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
crisis <- modelsummary(crisis_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(crisis, file = "crisis_S18_18-04-2023.html", open = FALSE)
quick_xlsx(crisis, file = "crisis_S18_18-04-2023.xlsx", open = FALSE)


#Pre/post crisis
urban_output <- list(g1urbanp1, g1urbanp2, g1ruralp1, g1ruralp2)
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
urban <- modelsummary(urban_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(urban, file = "urban_S18_18-04-2023.html", open = FALSE)
quick_xlsx(urban, file = "urban_S18_18-04-2023.xlsx", open = FALSE)


###########################################################################
# Figures - Cowplot -------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Median income -----------------------------------------------------------
# -------------------------------------------------------------------------

below <- cat_plot(f1umedincnosocial, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         mod2.labels = c("Owned", "Private Rent"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "none", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))

above <- cat_plot(f1omedincnosocial, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         mod2.labels = c("Owned", "Private Rent"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))

plot_grid(below, above, labels = c("Below median household income", "Above median household income"), ncol = 1)
ggsave("medinc_tenure_s18_20-04-2023.png", dpi = 500)

# -------------------------------------------------------------------------
# Crisis ------------------------------------------------------------------
# -------------------------------------------------------------------------


pre <- cat_plot(f1prenosocial, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         mod2.labels = c("Owned", "Private Rent"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "none", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))


post <- cat_plot(f1postnosocial, pred = parity, modx = ratio_cat3, mod2 = tenure,
                 point.size = 2,
                 line.thickness = 0.8,
                 geom.alpha = 1,
                 dodge.width = 0.4,
                 errorbar.width = 0.25,
                 modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
                 modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
                 pred.labels = c("First birth", "Second birth"),
                 mod2.labels = c("Owned", "Private Rent"),
                 x.label = "",
                 y.label = "Pr(Experencing a Live Birth)",
                 main.title = "",
                 legend.main = "Proportion of household income dedicated to housing expenditure",
                 colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))

plot_grid(pre, post, labels = c("Pre-crisis", "Post-crisis"), ncol = 1)
ggsave("crisis_tenure_s18_20-04-2023.png", dpi = 500)


# -------------------------------------------------------------------------
# Urban -------------------------------------------------------------------
# -------------------------------------------------------------------------


urban <- cat_plot(f1urbannosocial, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         mod2.labels = c("Owned", "Private Rent"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "none", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))


rural <- cat_plot(f1ruralnosocial, pred = parity, modx = ratio_cat3, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat3
         modx.labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-100%"),
         pred.labels = c("First birth", "Second birth"),
         mod2.labels = c("Owned", "Private Rent"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure",
         colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))

plot_grid(urban, rural, labels = c("Urban", "Rural"), ncol = 1)
ggsave("urban_tenure_s18_20-04-2023.png", dpi = 500)

