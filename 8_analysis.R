#Coded by: Brian Buh
#Started on: 19.09.2022
#Last Updated: 

#This page starts the analytical strategy of the analysis

library(tidyverse)
library(haven)
library(arsenal)
library(survival)
library(survminer)
library(texreg)
library(sjPlot)
library(lme4)
library(effects)
library(jtools) #summ function
library(broom.mixed) #plot_summs function
library(margins)
library(interactions) #for using cat_plot

#Load data  hhpart (s7)
hhpart <- file.choose()
hhpart <- readRDS(hhpart)

str(hhpart)

hhpart2 <- hhpart %>% 
  filter(parenthh == 0)

###########################################################################
# Descriptive Analysis ----------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
hhpart2 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>%
  ggplot(aes(x = ratio)) + 
  geom_histogram(bins = 10) 

#Distribution of Ratio by Period
hhpart2 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>%
  mutate(ratio_cat = recode(ratio_cat,
                            "0-10" = "10",
                            "10-20" = "20",
                            "20-30" = "30",
                            "30-40" = "40",
                            "40-50" = "50",
                            "50-60" = "60",
                            "60-70" = "70",
                            "70-80" = "80",
                            "80-90" = "90",
                            "90-100" = "100")) %>% 
  group_by(period, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#c58d01", size = 1) +
  facet_wrap(~period) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 12), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  # labs(fill = "Activity Status") +
  # scale_fill_manual(labels = c("Full-time", "Part-time", "Employed - NA", "Self-employed", "Unemployed", "Out of LF"),
  #                   values = c("full time", "part time", "paid - NA", "self-employed", "unemployed", "out of LF")) +
  ggtitle("Distribution of Ratio of Houseing Cost to Household Income by Parity") +
  xlab("") +
  ylab("Percent")
ggsave("ratio_distribution_period_s8_19-09-2022.png", dpi = 300)

#Distribution of Ratio by Parity
hhpart2 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>% 
  mutate(ratio_cat = recode(ratio_cat,
                            "0-10" = "0",
                            "10-20" = "10",
                            "20-30" = "20",
                            "30-40" = "30",
                            "40-50" = "40",
                            "50-60" = "50",
                            "60-70" = "60",
                            "70-80" = "70",
                            "80-90" = "80",
                            "90-100" = "90")) %>% 
  group_by(parity, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#2176AE", size = 1) +
  facet_wrap(~parity) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 12), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  # labs(fill = "Activity Status") +
  # scale_fill_manual(labels = c("Full-time", "Part-time", "Employed - NA", "Self-employed", "Unemployed", "Out of LF"),
  #                   values = c("full time", "part time", "paid - NA", "self-employed", "unemployed", "out of LF")) +
  ggtitle("Ratio of housing cost to net household income") +
  xlab("Ratio of housing cost to net household income") +
  ylab("Percent")
ggsave("ratio_distribution_parity_s8_19-09-2022.png", dpi = 300)


#Age distribution with parents in household
hhpart %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>% 
  mutate(parenthh = as.character(parenthh),
         parenthh = recode(parenthh,
                           "0" = "No",
                           "1" = "Yes")) %>% 
  ggplot(aes(x = age, fill = parenthh)) + 
  geom_histogram(binwidth = 1, color = "black", size = 1) +
  scale_fill_manual(values = c("#CCA43B", "#0f4c5c")) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 12), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Parent(s) in HH",
       title = "Age Distribution of Sample: Parent(s) in Household") +
  xlab("Age") +
  ylab("Count")
ggsave("distribution_age_parenthh_s8_19-09-2022.png", dpi = 300)  




###########################################################################
# Multilevel model --------------------------------------------------------
###########################################################################


#Analytical strategy
# M1: Baseline multilevel model
## The baseline model the event, time function, and an individual and LAD random effect
# M2: Adds the explanatory variable plus age/agesq
# M3: Adds parity and period(no interaction)
# M4: Adds interaction between clock and parity
# M5: Remove observations in which the parent is still in the household
## This uses the df "hhpart2"
# M6: compares M4 to M5 sample (adds clock*parity)
# M7: interacts ratio and parity (hhpart)
# M8: interacts ratio and parity (hhpart2)

# M9: uses ratio_cat2*parity (hhpart2)
# M10: uses ratio_cat2*party and clock*parity (hhpart2)




# -------------------------------------------------------------------------
# Three-level Models ------------------------------------------------------
# -------------------------------------------------------------------------


# Analysis M1 -------------------------------------------------------------
#The baseline model the event, time function, and an individual and LAD random effect
m1 <- glmer(formula = event ~ clock + (1|pidp) + (1|code),
                          family = binomial,
                          data = hhpart,
                          control = glmerControl(optimizer = "bobyqa",
                                                 optCtrl = list(maxfun = 2e5)))

summary(m1)
summ(m1, exp = TRUE)
saveRDS(m1, "m1.rds")




# Analysis M2 -------------------------------------------------------------
m2 <- glmer(formula = event ~ clock + ratio + age + agesq + (1|pidp) + (1|code),
                      data = hhpart,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun = 2e5))) 

summary(m2)
summ(m2, exp = TRUE)
# m2m <- margins(m2, data = hhpart)
saveRDS(m2, "m2.rds")

effect_plot(m2, 
            pred = ratio, 
            pred.values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            interval = TRUE, 
            y.label = "Experencing a Live Birth")
ggsave("m2_s8_19-09-2022.png", dpi = 300)




# Analysis M3 -------------------------------------------------------------
m3 <- glmer(formula = event ~ clock + ratio + parity + period + age + agesq + (1|pidp) + (1|code),
                       data = hhpart,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5))) 

summary(m3)
summ(m3, exp = TRUE)
print(m3)
# m3m <- margins(m3, data = hhpart)
saveRDS(m3, "m3.rds")

effect_plot(m3, 
            pred = ratio, 
            pred.values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            interval = TRUE, 
            y.label = "Experencing a Live Birth")
ggsave("m3_s8_19-09-2022.png", dpi = 300)




# Analysis M4 -------------------------------------------------------------
m4 <- glmer(formula = event ~ clock*parity + ratio + period + age + agesq + (1|pidp) + (1|code),
            data = hhpart,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(m4)
summ(m4, exp = TRUE)
print(m4)
# m4m <- margins(m4, data = hhpart)

#save
saveRDS(m4,"m4.rds")
# readRDS("m4.rds")
# m4 <- readRDS("S:/r_projects/housing_fertility/model_objects/m4.rds")

effect_plot(m4, 
            pred = ratio, 
            pred.values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            interval = TRUE, 
            y.label = "Experencing a Live Birth")
ggsave("m4_s8_19-09-2022.png", dpi = 300)





# Analysis M5 -------------------------------------------------------------
## This removes observations where the individual lives in the household with their parents
m5 <- glmer(formula = event ~ clock + parity + ratio + period + age + agesq + (1|pidp) + (1|code),
            data = hhpart2,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(m5)
summ(m5, exp = TRUE)
print(m5)
# m5m <- margins(m5, data = hhpart)

#save
saveRDS(m5,"m5.rds")
# readRDS("m5.rds")
# m5 <- readRDS("S:/r_projects/housing_fertility/model_objects/m5.rds")

effect_plot(m5, 
            pred = ratio, 
            pred.values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            interval = TRUE, 
            y.label = "Experencing a Live Birth")
ggsave("m5_s8_19-09-2022.png", dpi = 300)

   




# Analysis M6 -------------------------------------------------------------
## Adds the clock*parity interaction to M5
m6 <- glmer(formula = event ~ clock*parity + ratio + period + age + agesq + (1|pidp) + (1|code),
            data = hhpart2,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(m6)
summ(m6, exp = TRUE)
print(m6)
# m6m <- margins(m6, data = hhpart)

#save
saveRDS(m6,"m6.rds")
# readRDS("m6.rds")
# m6 <- readRDS("S:/r_projects/housing_fertility/model_objects/m6.rds")

effect_plot(m6, 
            pred = ratio, 
            pred.values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            interval = TRUE, 
            y.label = "Experencing a Live Birth")
ggsave("m6_s8_19-09-2022.png", dpi = 300)



# Analysis M7 -------------------------------------------------------------
## Adds the ratio*parity interaction
m7 <- glmer(formula = event ~ clock*parity + ratio*parity + period + age + agesq + (1|pidp) + (1|code),
            data = hhpart,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(m7)
summ(m7, exp = TRUE)
print(m7)
# m7m <- margins(m7, data = hhpart)

#save
saveRDS(m7,"m7.rds")
# readRDS("m7.rds")
# m7 <- readRDS("S:/r_projects/housing_fertility/model_objects/m7.rds")

effect_plot(m7, 
            pred = ratio, 
            pred.values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            interval = TRUE, 
            y.label = "Experencing a Live Birth")
ggsave("m7_s8_19-09-2022.png", dpi = 300)



# Analysis M8 -------------------------------------------------------------
## Adds the ratio*parity interaction and the restricted sample
m8 <- glmer(formula = event ~ clock*parity + ratio*parity + period + age + agesq + (1|pidp) + (1|code),
            data = hhpart,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(m8)
summ(m8, exp = TRUE)
print(m8)
# m8m <- margins(m8, data = hhpart)

#save
saveRDS(m8,"m8.rds")
# readRDS("m8.rds")
# m8 <- readRDS("S:/r_projects/housing_fertility/model_objects/m8.rds")

effect_plot(m8, 
            pred = ratio, 
            pred.values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            interval = TRUE, 
            y.label = "Experencing a Live Birth")
ggsave("m8_s8_19-09-2022.png", dpi = 300)



# Analysis M9 --------------------------------------------------------------
#Three-level model with observations living with parents removed with interactions
m9 <- glmer(formula = event ~ clock + ratio_cat2*parity + period + age + agesq + (1|pidp) + (1|code),
                     data = hhpart2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5))) 

summary(m9)
summ(m9, exp = TRUE)
save.model(m9,"m9")

#save
saveRDS(frailtylad,"m9.rds")
# readRDS("m9.rds")
# m9 <- readRDS("S:/r_projects/housing_fertility/model_objects/m9.rds")


#Parity Predicted Probability Plots
effect_plot(m9, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            y.label = "Experencing a Live Birth")

cat_plot(m9, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         # modx.values = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100"), #For ratio_cat3
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # modx.labels = c("Low", "Medium", "High"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m9_S8_19-09-2022.png", dpi = 300)

# Analysis M10 --------------------------------------------------------------
m10 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + age + agesq + (1|pidp) + (1|code),
            data = hhpart2,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))) 

summary(m10)
summ(m10, exp = TRUE)
save.model(m10,"m10")

#save
saveRDS(frailtylad,"m10.rds")
# readRDS("m10.rds")
# m10 <- readRDS("S:/r_projects/housing_fertility/model_objects/m10.rds")


#Parity Predicted Probability Plots
effect_plot(m10, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            y.label = "Experencing a Live Birth")

cat_plot(m10, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         # modx.values = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100"), #For ratio_cat3
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # modx.labels = c("Low", "Medium", "High"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m10_S8_19-09-2022.png", dpi = 300)
























# frailtylad2 --------------------------------------------------------------
#Three-level model with observations living with parents removed
## use df cballlad2

frailtylad2 <- glmer(formula = event ~ clock + sex + ratio_cat2 + parity + period + age + agesq + (1|pidp) + (1|code),
                     data = cballlad2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5))) 

summary(frailtylad2)
summ(frailtylad2, exp = TRUE)

#save
saveRDS(frailtylad,"frailtylad2.rds")
readRDS("frailtylad2.rds")
frailtylad2 <- readRDS("S:/r_projects/housing_fertility/model_objects/frailtylad2.rds")

# frailtylad3 --------------------------------------------------------------
#Three-level model with observations living with parents removed with interactions
frailtylad3 <- glmer(formula = event ~ clock + sex + ratio_cat2*parity + period + age + agesq + (1|pidp) + (1|code),
                     data = cballlad2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5))) 

summary(frailtylad3)
summ(frailtylad3, exp = TRUE)
save.model(frailtylad3,"frailtylad3")

#save
saveRDS(frailtylad,"frailtylad3.rds")
readRDS("frailtylad3.rds")
frailtylad3 <- readRDS("S:/r_projects/housing_fertility/model_objects/frailtylad3.rds")


#Parity Predicted Probability Plots
effect_plot(frailtylad3, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            y.label = "Experencing a Live Birth")

cat_plot(frailtylad3, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         # modx.values = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100"), #For ratio_cat3
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # modx.labels = c("Low", "Medium", "High"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("frailtylad3_S6_09-09-2022.png", dpi = 300)


# frailtylad4 --------------------------------------------------------------
#Three-level model with observations living with parents removed with interactions
frailtylad4 <- glmer(formula = event ~ clock + sex + ratio_cat2*period + ratio_cat2*parity + age + agesq + tenure + (1|pidp) + (1|code),
                     data = cballlad2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5))) 

summary(frailtylad4)
summ(frailtylad4, exp = TRUE)

#save
saveRDS(frailtylad,"frailtylad4.rds")
readRDS("frailtylad4.rds")
frailtylad4 <- readRDS("S:/r_projects/housing_fertility/model_objects/frailtylad4.rds")

#Period Predicted Probability Plots
effect_plot(frailtylad4, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            y.label = "Experencing a Live Birth")

cat_plot(frailtylad4, pred = period, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         # modx.values = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100"), #For ratio_cat3
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         pred.labels = c("1992-1999", "2000-2007", "2008-2012", "2013-2021"),
         # modx.labels = c("Low", "Medium", "High"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("frailtylad4_S6_09-09-2022.png", dpi = 300)


# frailtylad5 --------------------------------------------------------------
#Three-level model with observations living with parents removed with interactions plus controls
frailtylad5 <- glmer(formula = event ~ clock + sex + ratio_cat2*period + ratio_cat2*parity + age + agesq + tenure + 
                       edu + partner + ukborn + emp
                     + (1|pidp) + (1|code),
                     data = cballlad2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5))) 

summary(frailtylad5)
summ(frailtylad5, exp = TRUE)

#save
saveRDS(frailtylad5,"frailtylad5.rds")
readRDS("frailtylad5.rds")
frailtylad5 <- readRDS("S:/r_projects/housing_fertility/model_objects/frailtylad5.rds")

#Period Predicted Probability Plots
effect_plot(frailtylad5, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            y.label = "Experencing a Live Birth")

cat_plot(frailtylad5, pred = period, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         # modx.values = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100"), #For ratio_cat3
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         pred.labels = c("1992-1999", "2000-2007", "2008-2012", "2013-2021"),
         # modx.labels = c("Low", "Medium", "High"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("frailtylad5_S6_10-09-2022.png", dpi = 300)


cat_plot(frailtylad5, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         # modx.values = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100"), #For ratio_cat3
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         # modx.labels = c("Low", "Medium", "High"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("frailtylad5_parity_S6_10-09-2022.png", dpi = 300)




# frailtylad6 -------------------------------------------------------------
#Three-level model with observations living with parents removed with interactions ratio as continuous variable

frailtylogit6 <- glmer(formula = event ~ clock + sex + ratio*parity + ratio*period + age + agesq + tenure + (1|pidp) + (1|code),
                       data = cballlad2,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", #This controls for the warning "Model is nearly unidentifiable"
                                              optCtrl = list(maxfun = 2e5))) 

summary(frailtylogit6)
print(frailtylogit6)

saveRDS(frailtylogit6,"frailtylogit6.rds")


effect_plot(frailtylad6, 
            pred = ratio, 
            pred.values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            interval = TRUE, 
            y.label = "Experencing a Live Birth")