#Coded by: Brian Buh
#Started on: 13.02.2023
#Last Updated: 21.02.2023 (updated figures - removed "percentage")

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

hhpart3 %>% tabyl(age_cat2, tenure, period) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

summary(hhpart3$ratio)

# DF for parity specific models
hhpart3p1 <- hhpart3 %>% filter(parity == 1)
hhpart3p2 <- hhpart3 %>% filter(parity == 2)
hhpart3p3 <- hhpart3 %>% filter(parity == 3)


#Ratio Summary Statistics
summary(hhpart3p1$ratio)
summary(hhpart3p2$ratio)
summary(hhpart3p3$ratio)

tenure <- hhpart3 %>% 
  group_by(tenure) %>% 
  summarise(mean(ratio), median(ratio))

tenure2 <- hhpart3 %>% 
  group_by(tenure, age_cat2, parity) %>% 
  summarise(n())

hhemp <- hhpart3 %>% 
  group_by(hhemp, parity) %>% 
  summarise(mean(ratio), median(ratio))

period <- hhpart3 %>% 
  group_by(period) %>% 
  summarise(mean(ratio), median(ratio))

hhpart_ratio0 <- hhpart3 %>%
  filter(ratio_cat2 == "0")

hhpart_ratio0 %>% count(tenure, ownout)


hhpart_count <- hhpart3 %>% 
  filter(period == "2000-2007" | period == "2013-2022") 

hhpart3 %>% 
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

event <- hhpart3 %>% count(event, parity) #6,649 events (2,892 parity one; 2,328 parity two; 892 parity three)
p1 <- hhpart3 %>% filter(parity == 1, event == 1)
summary(p1$clock)
sd(p1$clock)
p2 <- hhpart3 %>% filter(parity == 2, event == 1)
summary(p2$clock)
sd(p2$clock)
p3 <- hhpart3 %>% filter(parity == 3, event == 1)
summary(p3$clock)
sd(p3$clock)
ind <- hhpart3 %>% distinct(pidp, parity) %>% count(parity) #13,225 unique individuals
eventlad <- hhpart3 %>% distinct(code)

obs <- hhpart3 %>% 
  group_by(pidp) %>% 
  mutate(obs = length(pidp),
         numobs = row_number(pidp)) %>% 
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


#Tenure
hhpart3p1 %>% tabyl(tenure) #owned = 68.2%, #rent = 23.4%, social = 8.4%
hhpart3p2 %>% tabyl(tenure) #owned = 73.3%, #rent = 14.1%, social = 12.6%
hhpart3p3 %>% tabyl(tenure) #owned = 77.5%, #rent = 9.1%, social = 13.4%

#hh employment
hhpart3p1 %>% tabyl(hhemp) #bothemp = 73.8%, bothunemp = 4.1%, egoemp = 5.4%, egoinactive = 13.6%, egounemp 3.0%
hhpart3p2 %>% tabyl(hhemp) #bothemp = 66.0%, bothunemp = 4.6%, egoemp = 4.5%, egoinactive = 22.8%, egounemp 2.1%
hhpart3p3 %>% tabyl(hhemp) #bothemp = 68.4%, bothunemp = 4.2%, egoemp = 4.9%, egoinactive = 21.0%, egounemp 2.0%


# -------------------------------------------------------------------------
# Sample Age Dstribution  --------------------------------------------------------
# -------------------------------------------------------------------------

hhpart3 %>% 
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
ggsave("distribution_age_hhpart3_s13_15-02-2023.png", dpi = 500)  



# -------------------------------------------------------------------------
# Ratio No stratification  ------------------------------------------------
# -------------------------------------------------------------------------


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

hhpart3 %>% 
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

hhpart3 %>% 
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


hhpart3 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>% 
  mutate(hhemp = recode(hhemp, 
                         "bothemp" = "Both Employed",
                         "bothunemp" = "Both Unemployed",
                         "egoemp" = "Woman Employed, Man Not",
                         "egoinactive" = "Woman Inactive",
                         "egounemp" = "Woman Unemployed")) %>% 
  mutate(hhemp = fct_relevel(hhemp, c("Both Employed", "Both Unemployed",  "Woman Inactive", "Woman Unemployed", "Woman Employed, Man Not"))) %>% 
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
  geom_bar(stat = "identity", color = "black", fill = "#3d405b", size = 1) +
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
ggsave("ratio_distribution_hhemp_s13_14-02-2023.png", width = 10, height = 10, units = "in", dpi = 500)



# -------------------------------------------------------------------------
# Ratio by agetenure ------------------------------------------------------
# -------------------------------------------------------------------------

hhpart3 %>% 
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

hhpart3 %>% 
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

hhpart3 %>% 
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
hhpart3 %>% 
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
hhpart3 %>% 
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
hhpart3 %>% 
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

