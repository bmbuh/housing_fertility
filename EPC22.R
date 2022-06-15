#Coded by: Brian Buh
#Started on: 15.06.2022
#Last Updated:

library(tidyverse)
library(haven)
library(lubridate)

#Use df: indhhbhps, indhhukhls, allfert

# Sample selection:
# 1. England, Wales, Scotland (no N.I.)
# 2. individual ages 18-45
# 3. Parity 0-3
# 4. observations = 2+

#My goal is to have some basic descriptive and a figure showing the relative ratio of housing costs to house hold income across period and parity

############################################################################
# BHPS --------------------------------------------------------------------
############################################################################


epcbhps <- indhhbhps %>% 
  select(pidp, wave, hhorig, istrtdatm, istrtdaty, sex, age_dv, xphsg, xphsn, hhyneti, gor_dv) %>% 
  mutate(year = wave + 1990,
         hhyneti = ifelse(hhyneti < 0, NA, hhyneti),
         mnhhinc = (hhyneti/12),
         xphsg = ifelse(xphsg < 0, NA, xphsg),
         xphsn = ifelse(xphsn < 0, NA, xphsn),
         ngdiff = xphsg - xphsn,
         ngdiffdummy = ifelse(ngdiff != 0, 1, 0),
         period = case_when(year >= 1991 & year <= 1999 ~ "1991-1999",
                            year >= 2000 & year <= 2007 ~ "2000-2007",
                            year >= 2008 & year <= 2012 ~ "2008-2012",
                            year >= 2013 & year <= 2021 ~ "2013-2021")) %>% 
  relocate("year", .after = "wave") %>% 
  #Variable creation
  group_by(pidp) %>% 
  fill(mnhhinc, .direction = "down") %>% 
  fill(xphsg, .direction = "down") %>% 
  mutate(numobs = length(wave)) %>% 
  ungroup() %>% 
  mutate(hc_inc = xphsg/mnhhinc,
         hc_incdummy = hc_inc > 1 | hc_inc < 0) %>% #Not there are no negative ratios but 505 ratios larger than 1
  #Sample selection
  filter(hhorig != 6, age_dv >= 18, age_dv <=45.99, numobs > 1) %>% 
  rename("hc" = "xphsg") %>% 
  select(pidp, wave, year, period, hhorig, istrtdatm, istrtdaty, sex, age_dv, gor_dv, hc, mnhhinc, hc_inc)

saveRDS(epcbhps, file = "epcbhps.rds")


# -------------------------------------------------------------------------
# Descriptive plots -------------------------------------------------------
# -------------------------------------------------------------------------


epcbhps %>% count(is.na(hc))
epcbhps %>% count(is.na(hc_incdummy))
epcbhps %>% count(wave)

# Capturing the difference between gross and net housing costs
epcbhps %>% count(ngdiffdummy)
## There are 21,874 observations with a difference between net and gross (9.2%) (13,034 NA or 5.5%)
epcbhps %>% 
  filter(!is.na(ngdiff), ngdiff <= 500, ngdiff >= -500) %>% 
  ggplot(aes(x = ngdiff)) + 
  geom_histogram()
## There appears to be a distribution of differences between 0 and 350 pounds/month

#Looking at the distribution of my ratio
##There are 505 cases where the cost of housing exceeds hh income (aka ratio > 1)
epcbhps %>% 
  filter(!is.na(hc_inc), hc_inc <= 1) %>%
  ggplot(aes(x = hc_inc)) + 
  geom_histogram()



############################################################################
# UKHLS -------------------------------------------------------------------
############################################################################

epcukhls <- indhhukhls %>% 
  select(pidp, wave, hhorig, istrtdatm, istrtdaty, sex, age_dv, houscost1_dv, fihhmnnet3_dv, rent, gor_dv) %>% 
  rename("mnhhinc" = "fihhmnnet3_dv") %>% 
  rename("hc" = "houscost1_dv") %>% 
  mutate(year = wave + 1990,
         rent = ifelse(rent < 0, NA, rent),
         hc = ifelse(is.na(rent) & hc == 0, NA, hc),
         # xphsg = ifelse(xphsg < 0, NA, xphsg),
         period = case_when(year >= 1991 & year <= 1999 ~ "1991-1999",
                            year >= 2000 & year <= 2007 ~ "2000-2007",
                            year >= 2008 & year <= 2012 ~ "2008-2012",
                            year >= 2013 & year <= 2021 ~ "2013-2021"),
         mnhhinc = ifelse(mnhhinc < 0, 0, mnhhinc)) %>% #To get rid of negative ratios
  relocate("year", .after = "wave") %>% 
  #Variable creation
  group_by(pidp) %>% 
  fill(mnhhinc, .direction = "down") %>% #There are 68 NA for hc (6.5%)
  fill(hc, .direction = "downup") %>% #There are 11,328 NA for hc (0.001%)
  mutate(numobs = length(wave)) %>% 
  ungroup() %>% 
  mutate(hc_inc = hc/mnhhinc,
         hc_incdummy = hc_inc > 1 | hc_inc < 0) %>% #Not there are 2712 ratios larger than 1 and 755 less than 0 (2.2%)
  #Sample selection
  filter(hhorig != 2, hhorig !=6, hhorig != 8, age_dv >= 18, age_dv <=45.99, numobs > 1) %>% 
  select(pidp, wave, year, period, hhorig, istrtdatm, istrtdaty, sex, age_dv, gor_dv, hc, mnhhinc, hc_inc)

saveRDS(epcukhls, file = "epcukhls.rds")


# -------------------------------------------------------------------------
# Descriptive plots -------------------------------------------------------
# -------------------------------------------------------------------------

#Looking at the distribution of my ratio
##There are 505 cases where the cost of housing exceeds hh income (aka ratio > 1)
epcukhls %>% 
  filter(!is.na(hc_inc), hc_inc < 1, hc_inc > 0) %>%
  ggplot(aes(x = hc_inc)) + 
  geom_histogram()

epcukhls %>% count(hc_inc < 0)



############################################################################
# combined ----------------------------------------------------------------
############################################################################

allfert_cut <- allfert %>% 
  select(pidp, wave, kdob, bno) %>% 
  group_by(pidp) %>% 
  mutate(totchild = length(pidp)) %>% 
  filter(bno <= 3, !is.na(kdob)) #I lose about 6000 higher parity births (exclusively from UKHLS)

# Combining BHPS and UKHLS and adding fertility  
## I make the assumption that housing costs over 100% of household income will be recategorized as 1 and negative ratios will be recatoegirzed as 0
epcall <- 
  bind_rows(epcbhps, epcukhls) %>% 
  arrange(pidp, wave) %>% 
  mutate(hc_inc = ifelse(hc_inc > 1, 1, ifelse(hc_inc < 0, 0, hc_inc)),
         hcinc_cat = cut(hc_inc, 
                         breaks = c(-0.1, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                         labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))) %>% 
  complete(nesting(pidp), wave = seq(min(wave), max(wave), 1L)) %>% 
  left_join(., allfert_cut, by = c("pidp", "wave")) %>% 
  mutate(year = ifelse(is.na(year) & !is.na(kdob), "missing", year), #This process allows me to create a wave 0 for births occurring before the first interview
         kyear = year(ymd(kdob)),
         missingdummy = ifelse(year == "missing", 1, 0), #This amounts to 6555 births!!!!!
         year = ifelse(year == "missing", kyear, year)) %>% 
  filter(!is.na(year)) %>% 
  group_by(pidp) %>% 
  mutate(obsnum = row_number()) %>% 
  ungroup()  %>% 
  mutate(kyear = ifelse(is.na(kyear), year, kyear),
         wave1 = ifelse(obsnum == 1 & kyear < year, 0, wave),
         wave1 = ifelse(obsnum == 2 & kyear < year, 0, wave1),
         wave1 = ifelse(obsnum == 3 & kyear < year, 0, wave1))

#I will extract the wave 0 and readd it to the large df
wave0 <- epcall %>% 
  filter(wave1 == 0) %>%
  select(pidp, wave1, kyear, hhorig, kdob, bno, totchild) %>%
  rename("wave" = "wave1",
         "year" = "kyear")

epcall0 <-  
  bind_rows(epcall, wave0) %>% 
  arrange(pidp, wave)

saveRDS(epcall0, file = "epcall0.rds")




# -------------------------------------------------------------------------
# Descriptive plots -------------------------------------------------------
# -------------------------------------------------------------------------

epcall %>% 
  filter(!is.na(hc_inc), hc_inc < 1, hc_inc > 0) %>%
  # group_by(period) %>% 
  # summarise(count = n(hcinc_cat))
  ggplot(aes(x = hcinc_cat)) + 
  geom_bar() +
  facet_wrap(~period)

