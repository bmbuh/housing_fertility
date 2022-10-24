#Coded by: Brian Buh
#Started on: 19.09.2022
#Last Updated: 14.10.2022

#UPDATE: After talking with Eva I decided the following:
# 1. I will only focus on women
# 2. I will NOT include observations where the women are single
# 3. The model that fits this best after testing is M15. Going forward (Script 9) M15 will be the base model

#This script includes the descriptive and regression analysis

# library(devtools)
# devtools::install_github("drizopoulos/GLMMadaptive")


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
# library(GLMMadaptive) #for using marginal_coefs()

#NOTE: i CANNOT USE THE margins PACKAGE WITH GLMER
## LOOK INTO https://drizopoulos.github.io/GLMMadaptive/articles/Methods_MixMod.html

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

# DF for lagged variables
###NOTE: THIS SHOULD PROBABLY BE PROPERLY ADDED TO THE NON-CUT SET
hhpart4 <-  hhpart3 %>% 
  group_by(pidp) %>% 
  mutate(tp1 = lead(event),
         ratio_cat2_tm1 = lag(ratio_cat2),
         ratio_cat2_tm2 = lag(ratio_cat2_tm1),
         tenure_tm1 = lag(tenure)) %>% 
  # select(pidp, event, tp1, ratio_cat2, ratio_cat2_tm1, ratio_cat2_tm2) %>% #for examining is data quality
  ungroup()

#ratio_cat2_tm1  
hhpart4 %>% count(is.na(ratio_cat2_tm1)) #NA = 13188 [18.0%]
hhpart4 %>% count(is.na(ratio_cat2_tm2)) #NA = 24568 [33.6%]


###########################################################################
# Descriptive Analysis ----------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Descriptive statistics  -------------------------------------------------
# -------------------------------------------------------------------------

event <- hhpart3 %>% count(event, parity) #5,987 events (2,834 parity one; 2,285 parity two; 8,68 parity three)
p1 <- hhpart3 %>% filter(parity == 1, event == 1)
summary(p1$clock)
sd(p1$clock)
p2 <- hhpart3 %>% filter(parity == 2, event == 1)
summary(p2$clock)
sd(p2$clock)
p3 <- hhpart3 %>% filter(parity == 3, event == 1)
summary(p3$clock)
sd(p3$clock)
ind <- hhpart3 %>% distinct(pidp, parity) %>% count(parity) #13,145 unique individuals
eventlad <- hhpart3 %>% distinct(code)

obs <- hhpart3 %>% 
  group_by(pidp) %>% 
  mutate(obs = length(pidp)) %>% 
  ungroup()
summary(obs$obs)
sd(obs$obs)

obsp1 <- hhpart3p1 %>%
  filter(parity == 1) %>% 
  group_by(pidp) %>% 
  mutate(obs = length(pidp)) %>% 
  ungroup()
summary(obsp1$obs)
sd(obsp1$obs)

obsp2 <- hhpart3p2 %>%
  filter(parity == 2) %>% 
  group_by(pidp) %>% 
  mutate(obs = length(pidp)) %>% 
  ungroup()
summary(obsp2$obs)
sd(obsp2$obs)

obsp3 <- hhpart3p3 %>%
  filter(parity == 3) %>% 
  group_by(pidp) %>% 
  mutate(obs = length(pidp)) %>% 
  ungroup()
summary(obsp3$obs)
sd(obsp3$obs)


#This uses hhpart3! Separated by parity
mycontrols <- tableby.control(test = FALSE)
hhpart3stats <-arsenal::tableby(parity ~ event + clock + ratio + ratio_cat2 + period + tenure + age + partner + edu + ukborn + emp + share + oci, 
                                data = hhpart3, 
                                weights = weight,
                                control = mycontrols)
labels(hhpart3stats) <-  c(parity = "Parity", event = "Event", clock = "Exposure", age = "Age",
                         ratio = "Ratio of Housing to Income", ratio_cat2 = "Ratio of Housing to Income (Cat.)", period = "Period",
                         tenure = "Housing type", partner = "Partnership status", edu = "Educational attainment", ukborn = "UK Born",
                         emp = "Activity status", share = "Household income share", oci = "Overcrowding Index")
summary(hhpart3stats)
write2html(hhpart3stats, "hhpart3stats_parity_10-10-2022.html") #UPDATE DATE
write2word(hhpart3stats, "hhpart3stats_parity_10-10-2022.docx") #UPDATE DATE



#This is for hhpart2 - including single women
# write2html(hhpart2stats, "hhpart2stats_parity_21-09-2022.html") #UPDATE DATE
# write2word(hhpart2stats, "hhpart2stats_parity_21-09-2022.docx") #UPDATE DATE

#separated by period
hhpart2stats2 <-arsenal::tableby(period ~ event + clock + ratio + ratio_cat2 + parity + tenure + age + partner + edu + ukborn + emp, data = hhpart2, control = mycontrols)
labels(hhpart2stats2) <-  c(parity = "Parity", event = "Event", clock = "Exposure", age = "Age",
                           ratio = "Ratio of Housing to Income", ratio_cat2 = "Ratio of Housing to Income (Cat.)", period = "Period",
                           tenure = "Housing type", partner = "Partnership status", edu = "Educational attainment", ukborn = "UK Born",
                           emp = "Activity status")

summary(hhpart2stats2)
write2html(hhpart2stats2, "hhpart2stats2_period_21-09-2022.html") #UPDATE DATE
write2word(hhpart2stats2, "hhpart2stats2_period_21-09-2022.docx") #UPDATE DATE


# -------------------------------------------------------------------------
# Descriptive plots -------------------------------------------------------
# -------------------------------------------------------------------------
# UPDATED: 10.10.2022

hhpart3 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>%
  ggplot(aes(x = ratio)) + 
  geom_histogram(bins = 10) 

#Distribution of Ratio by Period
hhpart3 %>% 
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
  ungroup() %>% 
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
  ggtitle("Distribution of Ratio of Housing Cost to Household Income by Period") +
  xlab("") +
  ylab("Percent")
ggsave("ratio_distribution_period_s8_10-10-2022.png", dpi = 300)

#Distribution of Ratio by Parity
hhpart3 %>% 
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
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12, hjust = 0.05, vjust = 0), strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  ggtitle("Ratio of housing cost to net household income by parity") +
  labs(caption = "Parity indicates all households at risk for specific parity. The x-axis scale indicates the lower bound of the group.") +
  xlab("Ratio of housing cost to net household income") +
  ylab("Percent")
ggsave("ratio_distribution_parity_s8_10-10-2022.png", dpi = 300)


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
ggsave("distribution_age_parenthh_s8_10-10-2022.png", dpi = 300)  


#Age distribution hhpart2
hhpart2 %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "#0f4c5c", size = 1) +
  # scale_fill_manual(values = c("#CCA43B", "#0f4c5c")) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 12), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Parent(s) in HH",
       title = "Age Distribution of Sample: All Women") +
  xlab("Age") +
  ylab("Count")
ggsave("distribution_hhpart2_s8_10-10-2022.png", dpi = 300)  


#Age distribution hhpart3
hhpart3 %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "#0f4c5c", size = 1) +
  # scale_fill_manual(values = c("#CCA43B", "#0f4c5c")) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 12), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12), strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  labs(fill = "Parent(s) in HH",
       title = "Age Distribution of Sample: Partnered Women") +
  xlab("Age") +
  ylab("Count")
ggsave("distribution_hhpart3_s8_10-10-2022.png", dpi = 300)   


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
# M11: uses ratio_cat2*parity and clock*parity + tenure (hhpart2)
# M12: uses ratio_cat2*parity and clock*parity + tenure + partner (hhpart2)
# M13: uses ratio_cat2*parity and clock*parity and ratio_cat2*period + tenure + partner (hhpart2)
# M14: uses ratio_cat2*parity and clock*parity + tenure (hhpart3) [removes single observations]
# M15: uses ratio_cat2*parity and clock*parity and ratio_cat2*period + tenure (hhpart3) [removes single observations]
# M16: uses ratio_cat2*parity and clock*parity*period + tenure (hhpart3) [removes single observations]
# M17: uses ratio_cat*parity + clock*parity + tenure (hhpart3)

# M18: M15 - lagged housing cost (ratio_cat2_tm1)
# M19: M15 - lagged housing cost + lagged tenure (ratio_cat2_tm1, tenure_tm1)
# M20: M15 - lagged housing cost 2 waves (ratio_cat2_tm2)



# Note: It appears that the proper model of analysis in M15. Clearly single individuals are biasing the sample.
# Additionally, not including all three interactions appears to give a very different influence (check for period).
# However, the three way interaction makes the relationships go crazy and the confidence intervals very large.
# This may be due to the fact that the ratio already removes much of the period effect from the model by standardizing the effect.


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
            main.title = "M2: Ratio + Age",
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
            main.title = "M3: Ratio + Parity + Period + Age",
            y.label = "Experencing a Live Birth")
ggsave("m3_s8_19-09-2022.png", dpi = 300)

plot_model(m3,
           type = "pred",
           terms = c("ratio", "parity"))




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
            main.title = "M4:Clock*parity + Ratio + Parity + Period + Age",
            y.label = "Experencing a Live Birth")
ggsave("m4_s8_19-09-2022.png", dpi = 300)


plot_model(m4,
           type = "pred",
           terms = c("ratio [all]", "parity"))





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
            main.title = "M5: Ratio + Parity + Period + Age; No parents",
            y.label = "Experencing a Live Birth")
ggsave("m5_s8_19-09-2022.png", dpi = 300)

   
plot_model(m5,
           type = "pred",
           terms = c("ratio [all]", "parity"))



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
            main.title = "M6:Clock*parity + Ratio + Parity + Period + Age; no parents",
            y.label = "Experencing a Live Birth")
ggsave("m6_s8_19-09-2022.png", dpi = 300)

plot_model(m6,
           type = "pred",
           terms = c("ratio [all]", "parity"))



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
            main.title = "M7:Clock*parity + Ratio*Parity + Period + Age; WITH parents",
            y.label = "Experencing a Live Birth")
ggsave("m7_s8_19-09-2022.png", dpi = 300)

plot_model(m7,
           type = "pred",
           terms = c("ratio [all]", "parity"))



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
            main.title = "M8:Clock*parity + Ratio*Parity + Period + Age; no parents",
            y.label = "Experencing a Live Birth")
ggsave("m8_s8_19-09-2022.png", dpi = 300)

plot_model(m8,
           type = "pred",
           terms = c("ratio [all]", "parity"))



# Analysis M9 --------------------------------------------------------------
#Three-level model with observations living with parents removed with interactions
m9 <- glmer(formula = event ~ clock + ratio_cat2*parity + period + age + agesq + (1|pidp) + (1|code),
                     data = hhpart2,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 2e5))) 

summary(m9)
summ(m9, exp = TRUE)

#save
saveRDS(m9,"m9.rds")
# readRDS("m9.rds")
# m9 <- readRDS("S:/r_projects/housing_fertility/model_objects/m9.rds")


#Parity Predicted Probability Plots
effect_plot(m9, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "M9:Clock + Ratio_cat2*Parity + Period + Age; no parents",
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
         main.title = "M9:Clock + Ratio_cat2*Parity + Period + Age; no parents",
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

#save
saveRDS(m10,"m10.rds")
# readRDS("m10.rds")
# m10 <- readRDS("S:/r_projects/housing_fertility/model_objects/m10.rds")


#Parity Predicted Probability Plots
effect_plot(m10, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "M10:Clock*parity + Ratio*Parity + Period + Age; no parents",
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
         main.title = "M10:Clock*parity + Ratio*Parity + Period + Age; no parents",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m10_S8_19-09-2022.png", dpi = 300)








# Analysis M11 --------------------------------------------------------------
# Adds tenure to the M10 model
m11 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + tenure + age + agesq + (1|pidp) + (1|code),
             data = hhpart2,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m11)
summ(m11, exp = TRUE)

#save
saveRDS(m11,"m11.rds")
# readRDS("m11.rds")
# m11 <- readRDS("S:/r_projects/housing_fertility/model_objects/m11.rds")


#Parity Predicted Probability Plots
effect_plot(m11, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m11:Clock*parity + Ratio*Parity + Period + Age; no parents",
            y.label = "Experencing a Live Birth")

cat_plot(m11, pred = parity, modx = ratio_cat2,
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
         main.title = "m11:Clock*parity + Ratio*Parity + Period + Tenure + Age; no parents",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m11_S8_19-09-2022.png", dpi = 300)




# Analysis M12 --------------------------------------------------------------
# Adds partner to the M10 Model
m12 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + tenure + partner + age + agesq + (1|pidp) + (1|code),
             data = hhpart2,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m12)
summ(m12, exp = TRUE)

#save
saveRDS(m12,"m12.rds")
# readRDS("m12.rds")
# m12 <- readRDS("S:/r_projects/housing_fertility/model_objects/m12.rds")


#Parity Predicted Probability Plots
effect_plot(m12, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m12:Clock*parity + Ratio*Parity + Period + Age; no parents",
            y.label = "Experencing a Live Birth")

cat_plot(m12, pred = parity, modx = ratio_cat2,
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
         main.title = "m12:Clock*parity + Ratio*Parity + Period + Tenure + Partner + Age; no parents",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m12_S8_19-09-2022.png", dpi = 300)




# Analysis M13 --------------------------------------------------------------
# Adds partner to the M10 Model
m13 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + partner + age + agesq + (1|pidp) + (1|code),
             data = hhpart2,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m13)
summ(m13, exp = TRUE)

#save
saveRDS(m13,"m13.rds")
# readRDS("m13.rds")
# m13 <- readRDS("S:/r_projects/housing_fertility/model_objects/m13.rds")


#Parity Predicted Probability Plots
effect_plot(m13, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m13:Clock*parity + Ratio*Parity + Period + Age; no parents",
            y.label = "Experencing a Live Birth")

cat_plot(m13, pred = period, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         # modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         # modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         # modx.values = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100"), #For ratio_cat3
         # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
         pred.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         # modx.labels = c("Low", "Medium", "High"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "m13:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Partner + Age; no parents",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m13_S8_21-09-2022.png", dpi = 300)




# Analysis M14 --------------------------------------------------------------
# Use a co-residing only sample
m14 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + period + tenure + age + agesq + (1|pidp) + (1|code),
             data = hhpart3,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m14)
summ(m14, exp = TRUE)

#save
saveRDS(m14,"m14.rds")
# readRDS("m14.rds")
# m14 <- readRDS("S:/r_projects/housing_fertility/model_objects/m14.rds")


#Parity Predicted Probability Plots
effect_plot(m14, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m14:Clock*parity + Ratio*Parity + Period + Tenure + Age; no parents, no singles",
            y.label = "Experencing a Live Birth")

cat_plot(m14, pred = parity, modx = ratio_cat2,
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
         main.title = "m14:Clock*parity + Ratio*Parity + Period + Tenure + Age; no parents, no singles",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m14_S8_19-09-2022.png", dpi = 300)

#Plot for PAA2023 Abstract
cat_plot(m14, pred = parity, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         # modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         # main.title = "Amount of housing income used for housing costs by Parity",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  # labs(caption = "Parity indicates all households at risk for specific parity. The x-axis scale indicates the lower bound of the group.") +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("PAA23_m14_S8_21-09-2022.png", dpi = 300)


# Analysis M15 --------------------------------------------------------------
# Use a co-residing only sample looking at the period interaction
m15 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity + ratio_cat2*period + tenure + age + agesq + (1|pidp) + (1|code),
             data = hhpart3,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m15)
summ(m15, exp = TRUE)

#save
saveRDS(m15,"m15.rds")
# readRDS("m15.rds")
# m15 <- readRDS("S:/r_projects/housing_fertility/model_objects/m15.rds")


#Parity Predicted Probability Plots
effect_plot(m15, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m15:Clock*parity + Ratio*Parity + Period + Tenure + Age; no parents, no singles",
            y.label = "Experencing a Live Birth")

#Only parities
cat_plot(m15, pred = parity, modx = ratio_cat2,
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
         main.title = "m15:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age; no parents, no singles",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m15_3way_parity_S8_06-10-2022.png", dpi = 300)

#With periods
cat_plot(m15, pred = parity, modx = ratio_cat2, mod2 = period,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "m15:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age; no parents, no singles",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m15_3way_parity_period_S8_21-09-2022.png", dpi = 300)

#With periods
cat_plot(m15, pred = period, modx = ratio_cat2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "m15:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age; no parents, no singles",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m15_3way_period_S8_17-10-2022.png", dpi = 300)


# Analysis M16 --------------------------------------------------------------
# Use a co-residing only sample looking at the period interaction
m16 <- glmer(formula = event ~ clock*parity + ratio_cat2*parity*period + tenure + age + agesq + (1|pidp) + (1|code),
             data = hhpart3,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m16)
summ(m16, exp = TRUE)

#save
saveRDS(m16,"m16.rds")
# readRDS("m16.rds")
# m16 <- readRDS("S:/r_projects/housing_fertility/model_objects/m16.rds")


#Parity Predicted Probability Plots
effect_plot(m16, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m16:Clock*parity + Ratio*Parity + Period + Tenure + Age; no parents, no singles",
            y.label = "Experencing a Live Birth")

cat_plot(m16, pred = parity, modx = ratio_cat2, mod2 = period,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "m16:Clock*parity + Ratio*Parity + Ratio*Period + Tenure + Age; no parents, no singles",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m16_3way_parity_period_S8_21-09-2022.png", dpi = 300)


# Analysis M17 --------------------------------------------------------------
# Use a co-residing only sample with ratio_cat
m17 <- glmer(formula = event ~ clock*parity + ratio_cat*parity + period + tenure + age + agesq + (1|pidp) + (1|code),
             data = hhpart3,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m17)
summ(m17, exp = TRUE)

#save
saveRDS(m17,"m17.rds")
# readRDS("m17.rds")
# m17 <- readRDS("S:/r_projects/housing_fertility/model_objects/m17.rds")


#Parity Predicted Probability Plots
effect_plot(m17, 
            pred = ratio_cat, 
            # pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m17:Clock*parity + Ratio_cat*Parity + Period + Tenure + Age; no parents, no singles",
            y.label = "Experencing a Live Birth")

# cat_plot(m17, pred = parity, modx = ratio_cat,
#          point.size = 2,
#          line.thickness = 0.8,
#          geom.alpha = 1,
#          dodge.width = 0.4,
#          errorbar.width = 0.25,
#          # modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
#          # modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
#          # modx.values = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100"), #For ratio_cat3
#          # pred.values = c("out of LF", "unemployment", "self-employed", "part time", "full time"),
#          pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
#          # modx.labels = c("Low", "Medium", "High"),
#          x.label = "",
#          y.label = "Pr(Experencing a Live Birth)",
#          main.title = "m17:Clock*parity + Ratio_cat*Parity + Period + Tenure + Age; no parents, no singles",
#          # colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E",
#          legend.main = "Household income used for housing") +
#   theme_bw() +
#   theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
#         axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
#         legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
# ggsave("m17_S8_19-09-2022.png", dpi = 300)



# M18: M15 - lagged housing cost (ratio_cat2_tm1)
# Analysis M18 --------------------------------------------------------------
# Use a co-residing only sample looking at the period interaction
m18 <- glmer(formula = event ~ clock*parity + ratio_cat2_tm1*parity + ratio_cat2_tm1*period + tenure + age + agesq + (1|pidp) + (1|code),
             data = hhpart4,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m18)
summ(m18, exp = TRUE)

#save
saveRDS(m18,"m18.rds")
# readRDS("m18.rds")
# m18 <- readRDS("S:/r_projects/housing_fertility/model_objects/m18.rds")


#Parity Predicted Probability Plots
effect_plot(m18, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m18:Clock*parity + Ratio*Parity + Period + Tenure + Age; no parents, no singles",
            y.label = "Experencing a Live Birth")

cat_plot(m18, pred = parity, modx = ratio_cat2_tm1,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "m18:Clock*parity + Ratio_tm1*Parity + Ratio_tm1*Period + Tenure + Age; no parents, no singles",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m18_3way_parity_period_hclagged_S8_04-10-2022.png", dpi = 300)



# M19: M15 - lagged housing cost + lagged tenure (ratio_cat2_tm1, tenure_tm1)
# Analysis M19 --------------------------------------------------------------
# Use a co-residing only sample looking at the period interaction
m19 <- glmer(formula = event ~ clock*parity + ratio_cat2_tm1*parity + ratio_cat2_tm1*period + tenure_tm1 + age + agesq + (1|pidp) + (1|code),
             data = hhpart4,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m19)
summ(m19, exp = TRUE)

#save
saveRDS(m19,"m19.rds")
# readRDS("m19.rds")
# m19 <- readRDS("S:/r_projects/housing_fertility/model_objects/m19.rds")


#Parity Predicted Probability Plots
effect_plot(m19, 
            pred = ratio_cat2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m19:Clock*parity + Ratio*Parity + Period + Tenure + Age; no parents, no singles",
            y.label = "Experencing a Live Birth")

cat_plot(m19, pred = parity, modx = ratio_cat2_tm1,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "m19:Clock*parity + Ratio_tm1*Parity + Ratio_tm1*Period + Tenure + Age; no parents, no singles",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m19_3way_parity_period_hclagged_tenurelagged_S8_04-10-2022.png", dpi = 300)

#NOTE: IT DOES NOT CHANGE THE PREDICTED PROBABILITES OF THE PREVIOUS MODEL, SOME REDUCE SIGNIFICANCE (MARGINAL)



# M20: M15 - lagged housing cost 2 waves (ratio_cat2_tm2)
# Analysis M20 --------------------------------------------------------------
# Use a co-residing only sample looking at the period interaction
m20 <- glmer(formula = event ~ clock*parity + ratio_cat2_tm2*parity + ratio_cat2_tm2*period + tenure_tm1 + age + agesq + (1|pidp) + (1|code),
             data = hhpart4,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun = 2e5))) 

summary(m20)
summ(m20, exp = TRUE)

#save
saveRDS(m20,"m20.rds")
# readRDS("m20.rds")
# m20 <- readRDS("S:/r_projects/housing_fertility/model_objects/m20.rds")


#Parity Predicted Probability Plots
effect_plot(m20, 
            pred = ratio_cat2_tm2, 
            pred.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"),
            interval = TRUE, 
            cat.geom = "line",
            main.title = "m20:Clock*parity + Ratio*Parity + Period + Tenure + Age; no parents, no singles",
            y.label = "Experencing a Live Birth")

cat_plot(m20, pred = parity, modx = ratio_cat2_tm2,
         point.size = 2,
         line.thickness = 0.8,
         geom.alpha = 1,
         dodge.width = 0.4,
         errorbar.width = 0.25,
         modx.values = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100"), #For ratio_cat2
         modx.labels = c("0%", "10%", "20%", "30%", "40%", "40-100%"),
         pred.labels = c("Parity 1", "Parity 2",  "Parity 3"),
         mod2.labels = c("1992-1999", "2000-2007",  "2008-2012", "2013-2021"),
         x.label = "",
         y.label = "Pr(Experencing a Live Birth)",
         main.title = "m20:Clock*parity + Ratio_tm2*Parity + Ratio_tm2*Period + Tenure + Age; no parents, no singles",
         legend.main = "Household income used for housing",
         colors = c("#A3D4E0", "#75BFD1", "#3892A8", "#2E778A", "#1F505C", "#0F282E")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15, vjust = 0.1), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text.x = element_text(size = 15))
ggsave("m20_3way_parity_period_hclagged2_S8_04-10-2022.png", dpi = 300)






































# -------------------------------------------------------------------------
# Output ------------------------------------------------------------------
# -------------------------------------------------------------------------


a1 <- list(m10, m11, m12, m13, m14)
cm3 <- c(parity2 = "Parity 2", 
         parity3 = "Parity 3",
         clock = "Exposure", 
         age = "Age",
         agesq = "Age-squared",
         ratio_cat20 = "0",
         "ratio_cat20.1-10" = "0.1-10",
         "ratio_cat220-30" = "20-30", 
         "ratio_cat230-40" = "30-40",
         "ratio_cat240-100" =  "40-100",
         "period2000-2007" = "2000-2007",
         "period2008-2012" = "2008-2012",
         "period2013-2021" = "2013-2021",
         "clock × parity2" = "clock × parity2",
         "clock × parity3" = "clock × parity3")
         # tenure = "Housing type", partner = "Partnership status", edu = "Educational attainment", ukborn = "UK Born",
         # emp = "Activity status")
modelsummary(a1, coef_map = cm3, output = "test.html", stars = TRUE)


a1output <- modelsummary(a1, coef_map = cm3, output = "huxtable", stars = TRUE)
quick_docx(a1diff_women, file = "A1diff_Women_AME_S10_20-07-2022.docx", open = FALSE)
quick_xlsx(a1, file = "test.xlsx", open = FALSE)
