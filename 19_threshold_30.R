#Coded by: Brian Buh
#Started on: 22.05.2023
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

# DF for 30% threold
hhpart5 <- hhpart4 %>% 
  mutate(hc30 = case_when(ratio < .30 ~ "below 30",
                          ratio >= .30 ~ "above 30"),
         hc30 = fct_relevel(hc30, c("below 30", "above 30"))) %>% 
  mutate(hc40 = case_when(ratio < .40 ~ "below 40",
                          ratio >= .40 ~ "above 40"),
         hc40 = fct_relevel(hc40, c("below 40", "above 40")))

hhpart5 %>% count(hc40)


# DF for parity specific models
hhpart5p1 <- hhpart5 %>% filter(parity == 1)
hhpart5p2 <- hhpart5 %>% filter(parity == 2)

## Income
umedincp1 <- hhpart5p1 %>% filter(medinc == 1, tenure != "social")
umedincp2 <- hhpart5p2 %>% filter(medinc == 1, tenure != "social")

omedincp1 <- hhpart5p1 %>% filter(medinc == 0, tenure != "social")
omedincp2 <- hhpart5p2 %>% filter(medinc == 0, tenure != "social")

# Housing crisis
precrisisp1 <- hhpart5p1 %>% filter(period == "1991-1999" | period == "2000-2007") %>% filter(tenure != "social")
precrisisp2 <- hhpart5p2 %>% filter(period == "1991-1999" | period == "2000-2007") %>% filter(tenure != "social")

postcrisisp1 <- hhpart5p1 %>% filter(period == "2008-2012" | period == "2013-2022") %>% filter(tenure != "social")
postcrisisp2 <- hhpart5p2 %>% filter(period == "2008-2012" | period == "2013-2022") %>% filter(tenure != "social")

#Urban/rural
urbanp1 <- hhpart5p1 %>% filter(urban == 1, tenure != "social")
urbanp2 <- hhpart5p2 %>% filter(urban == 1, tenure != "social")

ruralp1 <- hhpart5p1 %>% filter(urban == 2, tenure != "social")
ruralp2 <- hhpart5p2 %>% filter(urban == 2, tenure != "social")


###########################################################################
# Analysis ----------------------------------------------------------------
###########################################################################

# hc30
### h0 = no interaction
## h1 = interaction ratio*parity*tenure
## h2 = interaction ratio*parity*hhemp

# hc40
### h3 = no interaction
## h4 = interaction ratio*parity*tenure
## h5 = interaction ratio*parity*hhemp



# -------------------------------------------------------------------------
# hc30 Analysis -----------------------------------------------------------
# -------------------------------------------------------------------------

#analysis h0
h0 <- glmer(formula = event ~ clock*parity + hc30*parity + tenure + period + age_cat + agesq + edu + ukborn + emp 
            + (1|pidp) + (1|code),
            data = hhpart5,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(h0)
summ(h0, exp = TRUE)
saveRDS(h0,"h0.rds")
# mh0 <- margins(h0)
# summary(mh0)

cat_plot(h0, pred = parity, modx = hc30,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("below 30", "above 30"), #For hc30
         modx.labels = c("Below 30%", "Above 30%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("Owned", "Private Rent", "Social Rent"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure") +
  # colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("h0_parity_s19_22-05-2023.png", dpi = 500)




#analysis h1
h1 <- glmer(formula = event ~ clock*parity + hc30*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart5,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(h1)
summ(h1, exp = TRUE)
saveRDS(h1,"h1.rds")
# mh1 <- margins(h1)
# summary(mh1)

cat_plot(h1, pred = parity, modx = hc30, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("below 30", "above 30"), #For hc30
         modx.labels = c("Below 30%", "Above 30%"),
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
ggsave("h1_parity_tenure_s19_22-05-2023.png", dpi = 500)

cat_plot(h1, pred = parity, modx = hc30,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("below 30", "above 30"), #For hc30
         modx.labels = c("Below 30%", "Above 30%"),
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
ggsave("h1_parity_full_s19_22-05-2023.png", dpi = 500)


#analysis h2
h2 <- glmer(formula = event ~ clock*parity + hc30*parity*hhemp + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart5,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(h2)
summ(h2, exp = TRUE)
saveRDS(h2,"h2.rds")
# mh2 <- margins(h2)
# summary(mh2)

cat_plot(h2, pred = parity, modx = hc30, mod2 = hhemp,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("below 30", "above 30"), #For hc30
         modx.labels = c("Below 30%", "Above 30%"),
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
ggsave("h2_parity_hhemp_s19_22-05-2023.png", dpi = 500)





# -------------------------------------------------------------------------
# hc40 Analysis -----------------------------------------------------------
# -------------------------------------------------------------------------

#analysis h3
h3 <- glmer(formula = event ~ clock*parity + hc40*parity + tenure + period + age_cat + agesq + edu + ukborn + emp 
            + (1|pidp) + (1|code),
            data = hhpart5,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(h3)
summ(h3, exp = TRUE)
saveRDS(h3,"h3.rds")
# mh3 <- margins(h3)
# summary(mh3)

cat_plot(h3, pred = parity, modx = hc40,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("below 40", "above 40"), #For hc40
         modx.labels = c("Below 40%", "Above 40%"),
         pred.labels = c("First birth", "Second birth"),
         # mod2.labels = c("Owned", "Private Rent", "Social Rent"),
         # mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "",
         legend.main = "Proportion of household income dedicated to housing expenditure") +
  # colors = c("#a3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("h3_parity_s19_22-05-2023.png", dpi = 500)




#analysis h4
h4 <- glmer(formula = event ~ clock*parity + hc40*parity*tenure + period + age_cat + agesq + edu + ukborn + hhemp 
            + (1|pidp) + (1|code),
            data = hhpart5,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(h4)
summ(h4, exp = TRUE)
saveRDS(h4,"h4.rds")
# mh4 <- margins(h4)
# summary(mh4)

cat_plot(h4, pred = parity, modx = hc40, mod2 = tenure,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("below 40", "above 40"), #For hc40
         modx.labels = c("Below 40%", "Above 40%"),
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
ggsave("h4_parity_tenure_s19_22-05-2023.png", dpi = 500)

cat_plot(h4, pred = parity, modx = hc40,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("below 40", "above 40"), #For hc40
         modx.labels = c("Below 40%", "Above 40%"),
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
ggsave("h4_parity_full_s19_22-05-2023.png", dpi = 500)


#analysis h5
h5 <- glmer(formula = event ~ clock*parity + hc40*parity*hhemp + period + age_cat + agesq + edu + ukborn + tenure 
            + (1|pidp) + (1|code),
            data = hhpart5,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)),
            weights = weight) 

summary(h5)
summ(h5, exp = TRUE)
saveRDS(h5,"h5.rds")
# mh5 <- margins(h5)
# summary(mh5)

cat_plot(h5, pred = parity, modx = hc40, mod2 = hhemp,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("below 40", "above 40"), #For hc40
         modx.labels = c("Below 40%", "Above 40%"),
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
ggsave("h5_parity_hhemp_s19_22-05-2023.png", dpi = 500)






