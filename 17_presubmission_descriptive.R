#Coded by: Brian Buh
#Started on: 20.03.2023
#Last Updated: 26.04.2023


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
library(arsenal)

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

hhpart3 %>% tabyl(age_cat2, tenure, period) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

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

hhpart4 %>% count(ratio_cat3)

hhpart4 %>% tabyl(hhemp, tenure)

stats::chisq.test(hhpart4$hhemp, hhpart4$tenure)

# Heterogeneity testing df
## Age
u29 <- hhpart4 %>% filter(age_cat2 == "u29")
o30 <- hhpart4 %>% filter(age_cat2 == "o30")

## Income
umedinc <- hhpart4 %>% filter(medinc == 0)
umedinc %>% count(event, parity, tenure, ratio_cat3)
omedinc <- hhpart4 %>% filter(medinc == 1)
omedinc %>% count(event, parity, tenure)

#Urban/rural
urban <- hhpart4 %>% filter(urban == 1)
rural <- hhpart4 %>% filter(urban == 2)

# High Price LAD
highlad <- hhpart4 %>% filter(medlowquar == 1)
lowlad <- hhpart4 %>% 
  filter(!is.na(lowquar), is.na(medlowquar))

# Housing crisis
precrisis <- hhpart4 %>% filter(period == "1992-1999" | period == "2000-2007")
postcrisis <- hhpart4 %>% filter(period == "2008-2012" | period == "2013-2022")

# DF for parity specific models
hhpart4p1 <- hhpart4 %>% filter(parity == 1)
hhpart4p2 <- hhpart4 %>% filter(parity == 2)


#Ratio Summary Statistics
summary(hhpart4p1$ratio)
summary(hhpart4p2$ratio)

tenure <- hhpart4 %>% 
  group_by(tenure) %>% 
  summarise(mean(ratio), median(ratio))

tenure2 <- hhpart4 %>% 
  group_by(tenure, age_cat2, parity) %>% 
  summarise(n())

hhemp <- hhpart4 %>% 
  group_by(hhemp, parity) %>% 
  summarise(mean(ratio), median(ratio))

period <- hhpart4 %>% 
  group_by(period) %>% 
  summarise(mean(ratio), median(ratio))


hhpart_count <- hhpart4 %>% 
  filter(period == "2000-2007" | period == "2013-2022") 

hhpart4 %>% 
  mutate(cost = ifelse(ratio > 0.25, 1, 0)) %>% 
  tabyl(period, cost, tenure) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


#Steps of analysis
# 1. Reevaluate descriptives
#    a. Ratio by tenure
#    b. Ratio by employment constellation
#    c. Ratio by age
# 2. Examine those with 0 monthly expenditure
# 3. Run tenure and parity specific models
# 4. Evaluate employment constellation


###########################################################################
# Step 1 - Reevaluating descriptives --------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Descriptive statistics  -------------------------------------------------
# -------------------------------------------------------------------------

unique <- hhpart4 %>% 
  distinct(pidp, .keep_all = TRUE)

uniquep1 <- hhpart4 %>% 
  filter(parity == 1) %>% 
  distinct(pidp, .keep_all = TRUE)

uniquep2 <- hhpart4 %>% 
  filter(parity == 2) %>% 
  distinct(pidp, .keep_all = TRUE)

event <- hhpart4 %>% count(event, parity) #5,200 events (p1 = 2,892; p2 = 2,328)
p1 <- hhpart4 %>% filter(parity == 1, event == 1)
summary(p1$clock)
sd(p1$clock)
p2 <- hhpart4 %>% filter(parity == 2, event == 1)
summary(p2$clock)
sd(p2$clock)
p3 <- hhpart4 %>% filter(parity == 3, event == 1)
summary(p3$clock)
sd(p3$clock)
ind <- hhpart4 %>% distinct(pidp, parity) %>% count(parity) #13,225 unique individuals
eventlad <- hhpart4 %>% distinct(code)

obs <- hhpart4 %>% 
  group_by(pidp) %>% 
  mutate(obs = length(pidp),
         numobs = row_number(pidp)) %>% 
  ungroup() 
summary(obs$obs)
sd(obs$obs)


obsp1 <- hhpart4p1 %>%
  filter(parity == 1) %>% 
  group_by(pidp) %>% 
  mutate(obs = length(pidp)) %>% 
  ungroup()
summary(obsp1$obs)
sd(obsp1$obs)

obsp2 <- hhpart4p2 %>%
  filter(parity == 2) %>% 
  group_by(pidp) %>% 
  mutate(obs = length(pidp)) %>% 
  ungroup()
summary(obsp2$obs)
sd(obsp2$obs)

obsp3 <- hhpart4p3 %>%
  filter(parity == 3) %>% 
  group_by(pidp) %>% 
  mutate(obs = length(pidp)) %>% 
  ungroup()
summary(obsp3$obs)
sd(obsp3$obs)


#Tenure
hhpart4p1 %>% tabyl(tenure) #owned = 68.2%, #rent = 23.4%, social = 8.4%
hhpart4p2 %>% tabyl(tenure) #owned = 73.3%, #rent = 14.1%, social = 12.6%

#hh employment
hhpart4p1 %>% tabyl(hhemp) #bothemp = 73.8%, bothunemp = 4.1%, egoemp = 5.4%, egoinactive = 13.6%, egounemp 3.0%
hhpart4p2 %>% tabyl(hhemp) #bothemp = 66.0%, bothunemp = 4.6%, egoemp = 4.5%, egoinactive = 22.8%, egounemp 2.1%


#Output
mycontrols <- tableby.control(test = FALSE)
hhpart4stats <-arsenal::tableby(parity ~ event + clock + ratio + period + tenure + age + partner + edu + ukborn + hhemp, 
                            data = hhpart4, 
                            weights = weight,
                            control = mycontrols)

summary(hhpart4stats)
write2html(hhpart4stats, "hhpart4stats_parity_20-03-2023.html") #UPDATE DATE
write2word(hhpart4stats, "hhpart4stats_parity_20-03-2023.docx") #UPDATE DATE

hhpart4 %>% 
  group_by() %>% 
  summarise(mean(ratio), sd(ratio))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Table A2 Subsample stats ------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Urban
mycontrols <- tableby.control(test = FALSE)
urbanstats <-arsenal::tableby(urban ~ event.chr*parity + ratio + tenure +  hhemp2, 
                                data = hhpart4, 
                                weights = weight,
                                control = mycontrols)

summary(urbanstats)
write2html(urbanstats, "urbanstats_parity_22-03-2023.html") #UPDATE DATE
write2word(urbanstats, "urbanstats_parity_22-03-2023.docx") #UPDATE DATE

hhpart4 %>% 
  group_by(urban, tenure) %>% 
  summarise(mean(ratio), sd(ratio))


#Medinc
mycontrols <- tableby.control(test = FALSE)
medincstats <-arsenal::tableby(medinc ~ event.chr + ratio + tenure +  hhemp2, 
                              data = hhpart4, 
                              weights = weight,
                              control = mycontrols)

summary(medincstats)
write2html(medincstats, "medincstats_parity_22-03-2023.html") #UPDATE DATE
write2word(medincstats, "medincstats_parity_22-03-2023.docx") #UPDATE DATE

hhpart4 %>% 
  group_by(medinc, tenure) %>% 
  summarise(mean(ratio), sd(ratio))


#LAD
mycontrols <- tableby.control(test = FALSE)
ladstats <-arsenal::tableby(medlad ~ event.chr + ratio_cat3 + tenure +  hhemp2, 
                               data = hhpart4, 
                               weights = weight,
                               control = mycontrols)

summary(ladstats)
write2html(ladstats, "ladstats_parity_22-03-2023.html") #UPDATE DATE
write2word(ladstats, "ladstats_parity_22-03-2023.docx") #UPDATE DATE

#crisis
mycontrols <- tableby.control(test = FALSE)
crisisstats <-arsenal::tableby(crisis ~ event.chr + ratio + tenure +  hhemp2, 
                            data = hhpart4, 
                            weights = weight,
                            control = mycontrols)

summary(crisisstats)
write2html(crisisstats, "crisisstats_parity_22-03-2023.html") #UPDATE DATE
write2word(crisisstats, "crisisstats_parity_22-03-2023.docx") #UPDATE DATE

hhpart4 %>% 
  group_by(period, tenure) %>% 
  summarise(mean(ratio), sd(ratio))

#For body paragraph about rent pre- and post-crisis
precrisis %>% group_by(tenure, parity) %>% summarise(mean(ratio))
postcrisis %>% group_by(tenure, parity) %>% summarise(mean(ratio))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Who are the partnered households with parents? --------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

lwp <- hhpart %>% #lwp = livewithparents
  dplyr::filter(partner != "single", parenthh == 1) #2230 individuals

lwp2 <- lwp %>% 
  distinct(pidp) #798 distinct households

mycontrols <- tableby.control(test = FALSE)
lwpstats <-arsenal::tableby(parity ~ event + clock + ratio + ratio_cat2 + period + tenure + age + partner + edu + ukborn + hhemp + share + oci2, 
                            data = lwp, 
                            weights = weight,
                            control = mycontrols)

summary(lwpstats)
write2html(lwpstats, "lwpstats_parity_16-03-2023.html") #UPDATE DATE
write2word(lwpstats, "lwpstats_parity_16-03-2023.docx") #UPDATE DATE

# Who are the households paying 0 per month?
hhpart_ratio0 <- hhpart4 %>%
  filter(ratio_cat2 == "0") %>%  #7436 observations [3882 after weighting]
  mutate(ownout = as.character(ownout))
ratio0 <- hhpart_ratio0 %>% distinct(pidp) #2421 distinct households

hhpart_ratio0 %>% filter(ownout == 0) %>% tabyl(tenure)

mycontrols <- tableby.control(test = FALSE)
lwpstats <-arsenal::tableby(parity ~ event + clock + ratio + ratio_cat2 + period + tenure + age + partner + edu + ukborn + hhemp + share + oci2 + ownout, 
                            data = hhpart_ratio0, 
                            weights = weight,
                            control = mycontrols)

summary(lwpstats)
write2html(lwpstats, "hhpart_ratio0_16-03-2023.html") #UPDATE DATE
write2word(lwpstats, "hhpart_ratio0_16-03-2023.docx") #UPDATE DATE

###########################################################################
# Step 2 - Figures --------------------------------------------------------
###########################################################################


# -------------------------------------------------------------------------
# Sample Age Distribution  -------------------------------------------------
# -------------------------------------------------------------------------

hhpart4 %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "#e36414", size = 1) +
  # scale_fill_manual(values = c("#CCA43B", "#0f4c5c")) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(title = "Age Distribution of Sample") +
  xlab("Women's age") +
  ylab("Count")
ggsave("distribution_age_hhpart4_s13_15-02-2023.png", dpi = 500)  



# -------------------------------------------------------------------------
# Ratio No stratification  ------------------------------------------------
# -------------------------------------------------------------------------


hhpart4 %>% 
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
  group_by(ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#2176AE", size = 1) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15),
        strip.text.x = element_text(size = 15), plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by Parity") +
  # labs(caption = "Parity indicates all households at risk for each parity. The x-axis scale indicates the lower bound of the group") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")

# -------------------------------------------------------------------------
# Ratio by parity  --------------------------------------------------------
# -------------------------------------------------------------------------

hhpart4 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>%
  mutate(parity = recode(parity, 
                         "1" = "Parity 1",
                         "2" = "Parity 2",
                         "3" = "Parity 3")) %>% 
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
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15),
        strip.text.x = element_text(size = 15), plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by parity") +
  # labs(caption = "Parity indicates all households at risk for each parity. The x-axis scale indicates the lower bound of the group") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_parity_s13_14-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)



# -------------------------------------------------------------------------
# Ratio by tenure ---------------------------------------------------------
# -------------------------------------------------------------------------

hhpart4 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>% 
  mutate(tenure = recode(tenure, 
                         "owned" = "Owned",
                         "rent" = "Private Rent",
                         "social" = "Social Rent")) %>% 
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
  group_by(tenure, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#2a9d8f", size = 1) +
  facet_wrap(~tenure) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by housing type") +
  labs(caption = "") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_tenure_s13_13-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)



# -------------------------------------------------------------------------
# Ratio by hhemp ---------------------------------------------------------
# -------------------------------------------------------------------------


hhpart4 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>% 
  mutate(hhemp = recode(hhemp, 
                        "bothemp" = "Both Employed",
                        "bothunemp" = "Both Unemployed",
                        "egoemp" = "Woman Employed, Man Not",
                        "egoinactive" = "Woman Inactive",
                        "egounemp" = "Woman Unemployed")) %>% 
  mutate(hhemp = fct_relevel(hhemp, c("Both Employed",   "Woman Inactive", "Woman Employed, Man Not", "Both Unemployed", "Woman Unemployed"))) %>% 
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
  group_by(hhemp, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#c58d01", size = 1) +
  facet_wrap(~hhemp) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by couples' employment constellation") +
  labs(caption = "") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_hhemp_s17_23-03-2023.png", width = 10, height = 10, units = "in", dpi = 500)



# -------------------------------------------------------------------------
# Ratio by agetenure ------------------------------------------------------
# -------------------------------------------------------------------------

hhpart4 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>% 
  unite(agetenure, age_cat2, tenure, sep = "", remove = FALSE) %>% 
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
  group_by(agetenure, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#2176AE", size = 1) +
  facet_wrap(~agetenure) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by Age and Tenure") +
  labs(caption = "") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_agetenure_s13_14-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)


# -------------------------------------------------------------------------
# Ratio by paritytenure  --------------------------------------------------
# -------------------------------------------------------------------------

hhpart4 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>%
  unite(paritytenure, parity, tenure, sep = "", remove = FALSE) %>% 
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
  group_by(paritytenure, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#2176AE", size = 1) +
  facet_wrap(~paritytenure) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by Parity") +
  labs(caption = "Parity indicates all households at risk for specific parity. The x-axis scale indicates the lower bound of the group.") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_paritytenure_s13_14-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)


# -------------------------------------------------------------------------
# Ratio by period ---------------------------------------------------------
# -------------------------------------------------------------------------

hhpart4 %>% 
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
  mutate(period = recode(period, 
                         "1991-1999" = "1992-1999",
                         "2000-2007" = "2000-2007",
                         "2008-2012" = "2008-2012",
                         "2013-2022" = "2013-2022")) %>% 
  group_by(period, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#c58d01", size = 1) +
  facet_wrap(~period) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by period") +
  labs(caption = "") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_period_s13_14-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)

# -------------------------------------------------------------------------
# Ratio by periodtenure  --------------------------------------------------
# -------------------------------------------------------------------------

hhpart4 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>%
  unite(periodtenure, period, tenure, sep = "", remove = FALSE) %>% 
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
  group_by(periodtenure, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#2176AE", size = 1) +
  facet_wrap(~periodtenure) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by Period and Tenure") +
  labs(caption = "Parity indicates all households at risk for specific parity. The x-axis scale indicates the lower bound of the group.") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_periodtenure_s13_14-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)

#Period-Rent only
hhpart4 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0, tenure == "rent") %>%
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
  group_by(period, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#5BC0BE", size = 1) +
  facet_wrap(~period) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by period - private rent") +
  labs(caption = "") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_periodrent_s13_14-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)


#Period-Owner only
hhpart4 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0, tenure == "owned") %>%
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
  group_by(period, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#2176AE", size = 1) +
  facet_wrap(~period) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by Period - Homeowner") +
  labs(caption = "") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_periodowned_s13_14-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)

#Period-Social only
hhpart4 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0, tenure == "social") %>%
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
  group_by(period, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity", color = "black", fill = "#2176AE", size = 1) +
  facet_wrap(~period) +
  theme_bw()+
  scale_x_discrete(breaks = c(0, 20, 40, 60, 80, 100)) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 15), legend.title = element_text(size = 15), axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 15), axis.title.x = element_text(size = 15), strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 20)) +
  theme(aspect.ratio = 1) +
  ggtitle("Housing expenditure by Period - Social Rent") +
  labs(caption = "") +
  xlab("Percent of net household income going to housing") +
  ylab("Proportion")
ggsave("ratio_distribution_periodsocial_s13_14-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)



# -------------------------------------------------------------------------
# Change is spend in years before birth  ----------------------------------
# -------------------------------------------------------------------------

# Create a data set with lagged housing expenditure and changes from year of birth

hhpart4lag <- hhpart4 %>% 
  dplyr::select(pidp, wave, event, parity, ratio, tenure, period, crisis) %>% 
  group_by(pidp) %>% 
  mutate(lag1 = lag(ratio),
         lag2 = lag(lag1),
         lag3 = lag(lag2),
         tm1 = ratio - lag1,
         tm2 = ratio - lag2,
         tm3 = ratio - lag3) %>% 
  ungroup() %>% 
  filter(event == 1) %>% 
  mutate(tm0 = 0) %>% 
  # filter(tenure == "owned" | tenure == "rent") %>% 
  unite(tpp, c("tenure", "crisis", "parity"), sep = "", remove = FALSE) %>% 
  unite(tp, c("tenure","parity"), sep = "", remove = FALSE) %>% 
  group_by(tenure, period) %>% 
  summarise(tm0 = mean(tm0), tm1 = mean(tm1, na.rm = TRUE), tm2 = mean(tm2, na.rm = TRUE), tm3 = mean(tm3, na.rm = TRUE)) %>% 
  pivot_longer(cols = c("tm0", "tm1", "tm2", "tm3"), names_to = "tm", values_to = "tmnum")

hhpart4lag %>% 
  mutate(tm = case_when(tm == "tm3" ~ "-3",
                        tm == "tm2" ~ "-2",
                        tm == "tm1" ~ "-1",
                        tm ==  "tm0" ~ "0"),
         tm = fct_relevel(tm, c("-3", "-2", "-1", "0")),) %>% 
  ggplot(aes(x = tm, y = tmnum, group = tenure, color = tenure)) +
  geom_line() +
  facet_wrap(~period) +
  theme_bw() +
  scale_color_discrete(name = "Housing type", labels=c('Homeowner', 'Private rent', 'Social rent')) +
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 8), legend.title = element_text(size = 8), axis.title.y = element_text(size = 8),
        legend.text = element_text(size = 8), axis.title.x = element_text(size = 8), strip.text.x = element_text(size = 8),
        plot.title = element_text(size = 10), plot.subtitle = element_text(size = 8)) +
  theme(aspect.ratio = 1) +
  labs(subtitle = "Years before birth") +
  ggtitle("Change in housing expenditure as proportion of income") +
  xlab("Years before birth") +
  ylab("Change compared to year of birth")
ggsave("change_before_birth_s17_24-03-2023.png", width = 10, height = 10, units = "in", dpi = 500)








