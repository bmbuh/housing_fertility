#Coded by: Brian Buh
#Started on: 30.05.2023
#Last Updated: 

# This script looks at Eva's comments
## Focused on above/below 30%

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
library(ggeffects)
library(ggpubr) #For ggarrange
library(grid) #for testGrob
library(gridExtra)

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
         hhemp2 = fct_relevel(hhemp, c("bothemp", "egoinactive", "egoemp",  "bothunemp", "egounemp"))) %>% 
  mutate(age_cat = case_when(age_cat == "18-24" ~ "18-24", #fix the labelling issue
                             age_cat == "25-28" ~ "25-29",
                             age_cat == "30-34" ~ "30-34",
                             age_cat == "25-39" ~ "35-39",
                             age_cat == "40-45" ~"40-45")) %>% 
  mutate(crisis = case_when(period == "1991-1999" | period == "2000-2007" ~ "Pre-crisis",
                            period == "2008-2012" | period == "2013-2022" ~ "Post-crisis"),
         crisis = fct_relevel(crisis, c("Pre-crisis", "Post-crisis")),
         urban = case_when(urban == 1 ~ "Urban",
                           urban == 2 ~"Rural"),
         urban = fct_relevel(urban, c("Urban", "Rural")),
         birth = case_when(parity == 1 ~ "First Birth",
                           parity == 2 ~ "Second Birth"),
         medinclabel = case_when(medinc == 0 ~ "Above Median HH Income",
                                 medinc == 1 ~ "Below Median HH Income"),
         medinclabel = fct_relevel(medinclabel, c("Below Median HH Income", "Above Median HH Income")))

hhpart4 %>% count(age_cat)
hhpart4 %>% count(crisis)
hhpart4 %>% count(urban)
hhpart4 %>% count(birth)
hhpart4 %>% count(medinclabel)


# DF for parity specific models
hhpart4p1 <- hhpart4 %>% filter(parity == 1)
hhpart4p2 <- hhpart4 %>% filter(parity == 2)

hhpart4p1owned <- hhpart4p1 %>% filter(tenure == "owned")
summary(hhpart4p1owned$ratio)
hhpart4p1rent <- hhpart4p1 %>% filter(tenure == "rent")
summary(hhpart4p1rent$ratio)



# DF for parity specific models with No Social Rent
hhpart4p1ns <- hhpart4 %>% filter(parity == 1, tenure != "social")
hhpart4p2ns <- hhpart4 %>% filter(parity == 2, tenure != "social")


## Income
umedincp1 <- hhpart4p1 %>% filter(medinc == 1, tenure != "social")
umedincp2 <- hhpart4p2 %>% filter(medinc == 1, tenure != "social")

summary(umedincp1$hhinc) #correct

omedincp1 <- hhpart4p1 %>% filter(medinc == 0, tenure != "social")
omedincp2 <- hhpart4p2 %>% filter(medinc == 0, tenure != "social")

summary(omedincp1$hhinc) #correct

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
# Analysis ----------------------------------------------------------------
###########################################################################


# Models
# i0 - base model no interactions
# i0p1 - base parity 1
# i0p2 - base parity 2

# i1 - tenure interaction
# i1p1 - tenure parity 1
# i1p2 - tenure parity 2

# i2 - hhemp interaction
# i2p1 - hhemp parity 1
# 12p2 - hhemp parity 2

# i3p1 - under median parity 1
# i3p2 - under median parity 2
# i4p1 - over median parity 1
# i4p2 - over median parity 2
# medincp1 - interaction ratio*tenure*medinc parity 1
# medincp2 - interaction ratio*tenure*medinc parity 2


# i5p1 - precrisis parity 1
# i5p2 - precrisis parity 2
# i6p1 - postcrisis parity 1
# i6p2 - postcrisis parity 2
# crisisp1 - interaction ratio*tenure*crisis parity 1
# crsisisp2 - interaction ratio*tenure*crisis parity 2

# i7p1 - urban parity 1
# i7p2 - urban parity 2
# i8p1 - rural parity 1
# i8p2 - rural parity 2
# urp1 = interaction ratio*tenure*urban parity 1
# urp2 = interaction ratio*tenure*urban parity 2



# -------------------------------------------------------------------------
# Full Analysis -----------------------------------------------------------
# -------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# i0 ----------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis i0
i0 <- glmer(formula = event ~ clock*parity + ratio*parity + tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(i0)
summ(i0, exp = TRUE)
saveRDS(i0,"i0.rds")


effect_plot(i0, pred = ratio,
            interval = TRUE,
            main.title = "full")


#analysis i0p1
i0p1 <- glmer(formula = event ~ clock + ratio + tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart4p1,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(i0p1)
summ(i0p1, exp = TRUE)
saveRDS(i0p1,"i0p1.rds")


#analysis i0p2
i0p2 <- glmer(formula = event ~ clock + ratio + tenure + period + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart4p2,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(i0p2)
summ(i0p2, exp = TRUE)
saveRDS(i0p2,"i0p2.rds")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# i1 ----------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis i1 = tenure interaction
i1 <- glmer(formula = event ~ clock*birth + ratio*birth*tenure + period + age_cat + agesq + edu + ukborn + hhemp
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(i1)
summ(i1, exp = TRUE)
saveRDS(i1,"i1.rds")

effect_plot(i1, pred = ratio,
            interval = TRUE,
            main.title = "tenure interaction")



#analysis i1p1 = tenure interaction parity 1
i1p1 <- glmer(formula = event ~ clock + ratio*tenure + period + age_cat + agesq + edu + ukborn + hhemp
            + (1|pidp) + (1|code),
            data = hhpart4p1,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(i1p1)
summ(i1p1, exp = TRUE)
saveRDS(i1p1,"i1p1.rds")



#analysis i1p2 = tenure interaction parity 2
i1p2 <- glmer(formula = event ~ clock + ratio*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart4p2,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(i1p2)
summ(i1p2, exp = TRUE)
saveRDS(i1p2,"i1p2.rds")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# i2 ----------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#analysis i2 
i2 <- glmer(formula = event ~ clock*parity + ratio*parity*hhemp + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart4,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(i2)
summ(i2, exp = TRUE)
saveRDS(i2,"i2.rds")

effect_plot(i2, pred = ratio,
            interval = TRUE,
            main.title = "hhemp interaction")


#analysis i2p1 parity 1
i2p1 <- glmer(formula = event ~ clock + ratio*hhemp + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart4p1,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(i2p1)
summ(i2p1, exp = TRUE)
saveRDS(i2p1,"i2p1.rds")


#analysis i2p2 parity 2
i2p2 <- glmer(formula = event ~ clock + ratio*hhemp + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart4p2,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(i2p2)
summ(i2p2, exp = TRUE)
saveRDS(i2p2,"i2p2.rds")




# -------------------------------------------------------------------------
# median income -----------------------------------------------------------
# -------------------------------------------------------------------------

### i3p1 = under median income parity 1
i3p1 <- glmer(formula = event ~ clock + ratio*tenure + period + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = umedincp1,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(i3p1)
summ(i3p1, exp = TRUE)
saveRDS(i3p1,"i3p1.rds")


### i3p2 = under median income parity 2
i3p2 <- glmer(formula = event ~ clock + ratio*tenure + period + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = umedincp2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(i3p2)
summ(i3p2, exp = TRUE)
saveRDS(i3p2,"i3p2.rds")

### i4p1 = over median income parity 1
i4p1 <- glmer(formula = event ~ clock + ratio*tenure + period + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = omedincp1,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(i4p1)
summ(i4p1, exp = TRUE)
saveRDS(i4p1,"i4p1.rds")


### i4p2= over median income parity 2
i4p2<- glmer(formula = event ~ clock + ratio*tenure + period + age_cat + agesq + edu + ukborn + hhemp
                     + (1|pidp) + (1|code),
                     data = omedincp2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5)),
                     weights = weight)

summary(i4p2)
summ(i4p2, exp = TRUE)
saveRDS(i4p2,"i4p2.rds")



#analysis medincp1 = medinc interaction parity 1
medincp1 <- glmer(formula = event ~ clock + ratio*tenure*medinclabel + period + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart4p1ns,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(medincp1)
summ(medincp1, exp = TRUE)
saveRDS(medincp1,"medincp1.rds")



#analysis medincp2 = medinc interaction parity 2
medincp2 <- glmer(formula = event ~ clock + ratio*tenure*medinclabel + period + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart4p2ns,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(medincp2)
summ(medincp2, exp = TRUE)
saveRDS(medincp2,"medincp2.rds")





# -------------------------------------------------------------------------
# pre/post crisis -----------------------------------------------------------
# -------------------------------------------------------------------------

### i5p1= precrisis parity 1
i5p1<- glmer(formula = event ~ clock + ratio*tenure  + age_cat + agesq + edu + ukborn + hhemp
                       + (1|pidp) + (1|code),
                       data = precrisisp1,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5)),
                       weights = weight)

summary(i5p1)
summ(i5p1, exp = TRUE)
saveRDS(i5p1,"i5p1.rds")


### i5p2 = precrisis parity 2
i5p2 <- glmer(formula = event ~ clock + ratio*tenure  + age_cat + agesq + edu + ukborn + hhemp
                       + (1|pidp) + (1|code),
                       data = precrisisp2,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5)),
                       weights = weight)

summary(i5p2)
summ(i5p2, exp = TRUE)
saveRDS(i5p2,"i5p2.rds")

### i6p1 = postcrisis parity 1
i6p1 <- glmer(formula = event ~ clock + ratio*tenure + age_cat + agesq + edu + ukborn + hhemp
                        + (1|pidp) + (1|code),
                        data = postcrisisp1,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)),
                        weights = weight)

summary(i6p1)
summ(i6p1, exp = TRUE)
saveRDS(i6p1,"i6p1.rds")


### i6p2 = postcrisis parity 2
i6p2 <- glmer(formula = event ~ clock + ratio*tenure + age_cat + agesq + edu + ukborn + hhemp
                        + (1|pidp) + (1|code),
                        data = postcrisisp2,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)),
                        weights = weight)

summary(i6p2)
summ(i6p2, exp = TRUE)
saveRDS(i6p2,"i6p2.rds")

#analysis crisisp1 = tenure interaction parity 1
crisisp1 <- glmer(formula = event ~ clock + ratio*tenure*crisis + age_cat + agesq + edu + ukborn + hhemp 
                 + (1|pidp) + (1|code),
                 data = hhpart4p1ns,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(crisisp1)
summ(crisisp1, exp = TRUE)
saveRDS(crisisp1,"crisisp1.rds")

#analysis crisisp2 = tenure interaction parity 2
crisisp2 <- glmer(formula = event ~ clock + ratio*tenure*crisis + age_cat + agesq + edu + ukborn + hhemp 
                 + (1|pidp) + (1|code),
                 data = hhpart4p2ns,
                 family = binomial,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)),
                 weights = weight) 

summary(crisisp2)
summ(crisisp2, exp = TRUE)
saveRDS(crisisp2,"crisisp2.rds")





# -------------------------------------------------------------------------
# urban/rural -------------------------------------------------------------
# -------------------------------------------------------------------------

### i7p1 = urban parity 1
i7p1 <- glmer(formula = event ~ clock + ratio*tenure  + age_cat + agesq + edu + ukborn + hhemp
                   + (1|pidp) + (1|code),
                   data = urbanp1,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight)

summary(i7p1)
summ(i7p1, exp = TRUE)
saveRDS(i7p1,"i7p1.rds")


### i7p2 = urban parity 2
i7p2 <- glmer(formula = event ~ clock + ratio*tenure  + age_cat + agesq + edu + ukborn + hhemp
                   + (1|pidp) + (1|code),
                   data = urbanp2,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight)

summary(i7p2)
summ(i7p2, exp = TRUE)
saveRDS(i7p2,"i7p2.rds")

### i8p1 = rural parity 1
i8p1 <- glmer(formula = event ~ clock + ratio*tenure  + age_cat + agesq + edu + ukborn + hhemp
                   + (1|pidp) + (1|code),
                   data = ruralp1,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight)

summary(i8p1)
summ(i8p1, exp = TRUE)
saveRDS(i8p1,"i8p1.rds")


### i8p2 = rural parity 2
i8p2 <- glmer(formula = event ~ clock + ratio*tenure  + age_cat + agesq + edu + ukborn + hhemp
                   + (1|pidp) + (1|code),
                   data = ruralp2,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)),
                   weights = weight)

summary(i8p2)
summ(i8p2, exp = TRUE)
saveRDS(i8p2,"i8p2.rds")



#analysis urp1 = tenure interaction parity 1
urp1 <- glmer(formula = event ~ clock + ratio*tenure*urban + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart4p1ns,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(urp1)
summ(urp1, exp = TRUE)
saveRDS(urp1,"urp1.rds")

#analysis urp2 = tenure interaction parity 2
urp2 <- glmer(formula = event ~ clock + ratio*tenure*urban + age_cat + agesq + edu + ukborn + hhemp 
              + (1|pidp) + (1|code),
              data = hhpart4p2ns,
              family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl = list(maxfun = 2e5)),
              weights = weight) 

summary(urp2)
summ(urp2, exp = TRUE)
saveRDS(urp2,"urp2.rds")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Output ------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#full [not used]
full_output <- list(i0, i1, i2)
full <- modelsummary(full_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(full, file = "full_S20_30-05-2023.html", open = FALSE)
quick_xlsx(full, file = "full_S20_30-05-2023.xlsx", open = FALSE)

#parity [not used]
parity_output <- list(i1p1, i1p2, i2p1, i2p2)
parity <- modelsummary(parity_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(parity, file = "parity_S20_30-05-2023.html", open = FALSE)
quick_xlsx(parity, file = "parity_S20_30-05-2023.xlsx", open = FALSE)

#pbasevtenure [used]
basevtenure_output <- list(i0p1, i1p1, i0p2, i1p2)
basevtenure <- modelsummary(basevtenure_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(basevtenure, file = "basevtenure_S20_30-05-2023.html", open = FALSE)
quick_xlsx(basevtenure, file = "basevtenure_S20_30-05-2023.xlsx", open = FALSE)

#median income [used]
medinc_output <- list(i3p1, i3p2, i4p1, i4p2)
medinc <- modelsummary(medinc_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(medinc, file = "medinc_S20_30-05-2023.html", open = FALSE)
quick_xlsx(medinc, file = "medinc_S20_30-05-2023.xlsx", open = FALSE)

#Pre/post crisis [used]
crisis_output <- list(i5p1, i5p2, i6p1, i6p2)
crisis <- modelsummary(crisis_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(crisis, file = "crisis_S20_30-05-2023.html", open = FALSE)
quick_xlsx(crisis, file = "crisis_S20_30-05-2023.xlsx", open = FALSE)

#Pre/post crisis
urban_output <- list(i7p1, i7p2, i8p1, i8p2)
urban <- modelsummary(urban_output, output = "huxtable", exponentiate = TRUE, stars = TRUE)
quick_html(urban, file = "urban_S20_30-05-2023.html", open = FALSE)
quick_xlsx(urban, file = "urban_S20_30-05-2023.xlsx", open = FALSE)



###########################################################################
# Figures -----------------------------------------------------------------
###########################################################################

# Note: change legend.position from "none" to "bottom" for individual plots.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# full  -------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

show_pals()

#i0
ggpredict(i0, c("ratio")) %>% 
  plot()

#i1
hhpart4 %>% 
  group_by(tenure, parity) %>% 
  summarise(p20 = quantile(ratio, probs = 0.2), p80 = quantile(ratio, probs = 0.8))

ggpredict(i1, c("ratio [all]", "tenure", "birth")) %>% 
  plot(ci.style = c("ribbon"), alpha = 0.2, line.size = 1.1) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  scale_x_continuous(breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  labs(colour = "Tenure") +
  scale_fill_manual(values = c("#003049", "#e63946", "#e9c46a")) +
  scale_colour_manual(values = c("#003049", "#e63946", "#ffb703"), labels = c("Owned", "Private rent", "Social rent")) +
  ggtitle("") +
  xlab("Housing expenditure: Housing costs as a proportion of net household income") +
  ylab("Pr(Live birth)")
ggsave("tenure_cont_S20_30-05-2023.png", dpi = 500)



#i2
ggpredict(i2, c("ratio [all]", "hhemp", "parity")) %>% 
  plot() +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("hhemp_cont_S20_30-05-2023.png", dpi = 500)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# medinc ------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## parity 1
medincplot1 <- ggpredict(medincp1, c("ratio [all]", "tenure", "medinclabel")) %>% 
  plot(ci.style = c("ribbon"), alpha = 0.2, line.size = 1.1) +
  theme_bw() +
  theme(legend.position = "none", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 10, vjust = 0.1), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  scale_x_continuous(breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  labs(colour = "Tenure") +
  scale_fill_manual(values = c("#003049", "#e63946")) +
  scale_colour_manual(values = c("#003049", "#e63946"), labels = c("Owned", "Private rent", "Social rent")) +
  ggtitle("") +
  xlab("") +
  ylab("Pr(First birth)")
# ggsave("medincp1_cont_S20_30-05-2023.png", dpi = 500)

## parity 2
medincplot2 <- ggpredict(medincp2, c("ratio [all]", "tenure", "medinclabel")) %>% 
  plot(ci.style = c("ribbon"), alpha = 0.2, line.size = 1.1) +
  theme_bw() +
  theme(legend.position = "none", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 10, vjust = 0.1), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  scale_x_continuous(breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  labs(colour = "Tenure") +
  scale_fill_manual(values = c("#003049", "#e63946")) +
  scale_colour_manual(values = c("#003049", "#e63946"), labels = c("Owned", "Private rent", "Social rent")) +
  ggtitle("") +
  xlab("") +
  ylab("Pr(Second birth)")
# ggsave("medincp2_cont_S20_30-05-2023.png", dpi = 500)

#Combined - cowplot
medinc_cowplot <- plot_grid(medincplot1, medincplot2, ncol = 2)
x.grob <- textGrob("Housing expenditure: Housing costs as a proportion of net household income", gp=gpar(col="black", fontsize=12))
medinc_cowplot2 <- grid.arrange(arrangeGrob(medinc_cowplot, bottom = x.grob))
legend <- cowplot::get_legend(medincplot1 + guides(color = guide_legend(nrow = 1)) +
                                theme(legend.position = "bottom"))
plot_grid(medinc_cowplot2, legend, ncol = 1, rel_heights = c(1, .1))
ggsave("cowplot_medinc_S20_30-05-2023.png", dpi = 500, bg = "white")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# crisis ------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## parity 1
crisisplot1 <- ggpredict(crisisp1, c("ratio [all]", "tenure", "crisis")) %>% 
  plot(ci.style = c("ribbon"), alpha = 0.2, line.size = 1.1) +
  theme_bw() +
  theme(legend.position = "none", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 10, vjust = 0.1), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  labs(colour = "Tenure") +
  scale_fill_manual(values = c("#003049", "#e63946")) +
  scale_colour_manual(values = c("#003049", "#e63946"), labels = c("Owned", "Private rent", "Social rent")) +
  ggtitle("") +
  xlab("") +
  ylab("Pr(First birth)")
# ggsave("crisisp1_cont_S20_30-05-2023.png", dpi = 500)

## parity 2
crisisplot2 <- ggpredict(crisisp2, c("ratio [all]", "tenure", "crisis")) %>% 
  plot(ci.style = c("ribbon"), alpha = 0.2, line.size = 1.1) +
  theme_bw() +
  theme(legend.position = "none", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 10, vjust = 0.1), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  labs(colour = "Tenure") +
  scale_fill_manual(values = c("#003049", "#e63946")) +
  scale_colour_manual(values = c("#003049", "#e63946"), labels = c("Owned", "Private rent", "Social rent")) +
  ggtitle("") +
  xlab("") +
  ylab("Pr(Second birth)")
# ggsave("crisisp2_cont_S20_30-05-2023.png", dpi = 500)

#Combined - cowplot
crisis_cowplot <- plot_grid(crisisplot1, crisisplot2, ncol = 2)
x.grob <- textGrob("Housing expenditure: Housing costs as a proportion of net household income", gp=gpar(col="black", fontsize=12))
crisis_cowplot2 <- grid.arrange(arrangeGrob(crisis_cowplot, bottom = x.grob))
legend <- cowplot::get_legend(crisisplot1 + guides(color = guide_legend(nrow = 1)) +
                                theme(legend.position = "bottom"))
plot_grid(crisis_cowplot2, legend, ncol = 1, rel_heights = c(1, .1))
ggsave("cowplot_crisis_S20_30-05-2023.png", dpi = 500, bg = "white")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# urban -------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## parity 1
urbanplot1 <- ggpredict(urp1, c("ratio [all]", "tenure", "urban")) %>% 
  plot(ci.style = c("ribbon"), alpha = 0.2, line.size = 1.1) +
  theme_bw() +
  theme(legend.position = "none", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 10, vjust = 0.1), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  labs(colour = "Tenure:") +
  scale_fill_manual(values = c("#003049", "#e63946")) +
  scale_colour_manual(values = c("#003049", "#e63946"), labels = c("Owned", "Private rent", "Social rent")) +
  ggtitle("") +
  xlab("") +
  ylab("Pr(First birth)")
# ggsave("urp1_cont_S20_30-05-2023.png", dpi = 500)

## parity 2
urbanplot2 <- ggpredict(urp2, c("ratio [all]", "tenure", "urban")) %>% 
  plot(ci.style = c("ribbon"), alpha = 0.2, line.size = 1.1) +
  theme_bw() +
  theme(legend.position = "none", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 10, vjust = 0.1), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  labs(colour = "Tenure:") +
  scale_fill_manual(values = c("#003049", "#e63946")) +
  scale_colour_manual(values = c("#003049", "#e63946"), labels = c("Owned", "Private rent", "Social rent")) +
  ggtitle("") +
  xlab("") +
  ylab("Pr(Second birth)")
# ggsave("urp2_cont_S20_30-05-2023.png", dpi = 500)


#Combined - cowplot
urban_cowplot <- plot_grid(urbanplot1, urbanplot2, ncol = 2)
x.grob <- textGrob("Housing expenditure: Housing costs as a proportion of net household income", gp=gpar(col="black", fontsize=12))
urban_cowplot2 <- grid.arrange(arrangeGrob(urban_cowplot, bottom = x.grob))
legend <- cowplot::get_legend(urbanplot1 + guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom"))
plot_grid(urban_cowplot2, legend, ncol = 1, rel_heights = c(1, .1))
ggsave("cowplot_urban_S20_30-05-2023.png", dpi = 500, bg = "white")







 # i1plot <- ggpredict(i1, c("ratio [all]", "tenure", "parity"))
# 
# ggplot(i1plot, aes(x = x, y = predicted, color = group)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
#               alpha = 1/5) +
#   facet_wrap(~facet) +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
#         axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
#         legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))

