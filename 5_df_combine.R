#Coded by: Brian Buh
#Started on: 16.06.2022
#Last Updated:

###Note: this is left from the EPC draft. The variables need to be tested first and the controls need to be kept.

library(tidyverse)
library(haven)
library(lubridate)
# library(data.table) #Used for dates that dplyr automatically changes to numeric (not used in the end)

#Use df: indhhbhps, indhhukhls, allfert

# Sample selection:
# 1. England, Wales, Scotland (no N.I.)
# 2. individual ages 18-45
# 3. observations = 2+
# 4. Parity 0-3
## Filter 1-3 are done while cleaning the BHPS/UKHLS data individual. Filter 4 is done after combining the surveys and adding fertility.


############################################################################
# BHPS --------------------------------------------------------------------
############################################################################

cbbhps <- indhhbhps %>% 
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
  filter(hhorig != 6, hhorig != 14, hhorig != 15, hhorig != 16, age_dv >= 18, age_dv <=45.99, numobs > 1) %>% 
  rename("hc" = "xphsg") %>% 
  select(pidp, wave, year, period, hhorig, istrtdatm, istrtdaty, sex, age_dv, gor_dv, hc, mnhhinc, hc_inc, hc_incdummy)

saveRDS(cbbhps, file = "cbbhps.rds")


# -------------------------------------------------------------------------
# Descriptive plots -------------------------------------------------------
# -------------------------------------------------------------------------


cbbhps %>% count(is.na(hc))
cbbhps %>% count(is.na(hc_incdummy))
cbbhps %>% count(wave)

# Capturing the difference between gross and net housing costs
cbbhps %>% count(ngdiffdummy)
## There are 21,874 observations with a difference between net and gross (9.2%) (13,034 NA or 5.5%)
cbbhps %>% 
  filter(!is.na(ngdiff), ngdiff <= 500, ngdiff >= -500) %>% 
  ggplot(aes(x = ngdiff)) + 
  geom_histogram()
## There appears to be a distribution of differences between 0 and 350 pounds/month

#Looking at the distribution of my ratio
##There are 505 cases where the cost of housing exceeds hh income (aka ratio > 1)
cbbhps %>% 
  filter(!is.na(hc_inc), hc_inc <= 1) %>%
  ggplot(aes(x = hc_inc)) + 
  geom_histogram()



############################################################################
# UKHLS -------------------------------------------------------------------
############################################################################

cbukhls <- indhhukhls %>% 
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
  ####This has to be fixed post imputation!!!!!
  fill(mnhhinc, .direction = "down") %>% #There are 68 NA for hc (6.5%) 
  fill(hc, .direction = "downup") %>% #There are 11,328 NA for hc (0.001%)
  mutate(numobs = length(wave)) %>% 
  ungroup() %>% 
  mutate(hc_inc = hc/mnhhinc,
         hc_incdummy = hc_inc > 1 | hc_inc < 0) %>% #Not there are 2712 ratios larger than 1 and 755 less than 0 (2.2%)
  #Sample selection
  filter(hhorig != 2, hhorig !=6, hhorig != 8, age_dv >= 18, age_dv <=45.99, numobs > 1) %>% 
  select(pidp, wave, year, period, hhorig, istrtdatm, istrtdaty, sex, age_dv, gor_dv, hc, mnhhinc, hc_inc, hc_incdummy)

saveRDS(cbukhls, file = "cbukhls.rds")


# -------------------------------------------------------------------------
# Descriptive plots -------------------------------------------------------
# -------------------------------------------------------------------------

#Looking at the distribution of my ratio
##There are 505 cases where the cost of housing exceeds hh income (aka ratio > 1)
cbukhls %>% 
  filter(!is.na(hc_inc), hc_inc < 1, hc_inc > 0) %>%
  ggplot(aes(x = hc_inc)) + 
  geom_histogram()

cbukhls %>% count(hc_inc < 0)



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
cball <- 
  bind_rows(cbbhps, cbukhls) %>% 
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
  mutate(before = ifelse(kyear < year, 1, 0),
         before = ifelse(is.na(before), 0, before)) 

#I extract non-wave0 observations to ensure I have the correct obsnum for each wave
realobsnum <- cball %>% 
  filter(before != 1, missingdummy != 1) %>% 
  group_by(pidp) %>% 
  mutate(obsnum = row_number(),
         totobs = length(wave)) %>% 
  ungroup() %>% 
  select(pidp, wave, obsnum, totobs)

#This df fixes several issues:
# 1. it creates an observation number for each interview (note births before first interview and births that occur between interview periods don't count)
# 2. it calculates the total number of interviews per individual
# 3 it deals with the filter #4 issue (3rd births that occur before first interview or waves after the 3rd observed birth)
cball0 <- cball %>% 
  left_join(., realobsnum, by = c("pidp", "wave")) %>% 
  mutate(obsnum = ifelse(before == 1, -1, obsnum), #This sets all births that come before the first interview to observation "-1"
         obsnum = ifelse(missingdummy == 1, -2, obsnum), #This sets all missing intermediate waves in which a birth occurred to obsnum "-2"
         wave = ifelse(obsnum == -1, NA, wave)) %>% 
  distinct(pidp, wave, kdob, .keep_all = TRUE) %>% #with the process of moving the births that happen before the first interview, we end up with some duplicated observations
  group_by(pidp) %>% 
  fill(bno, .direction = "down") %>% 
  fill(totchild, .direction = "down") %>% 
  fill(hhorig, .direction = "downup") %>% 
  mutate(bno = ifelse(is.na(bno), 0, bno)) %>% 
  mutate(totchild = ifelse(is.na(totchild), 0, totchild)) %>% 
  ungroup() %>% 
  group_by(pidp, bno) %>% 
  mutate(parnum = row_number()) %>% #These step is necessary for filter 4 - remove observations past the birth of the third child
  ungroup() %>% 
  mutate(wave1year = ifelse(obsnum == 1, year, NA),
         par3filter = ifelse(bno == 3 & parnum >= 2, 1, 0), #This filter creates a dummy for all observations after the observation in which the 3rd child is born
         kyear = year(ymd(kdob))) %>% 
  group_by(pidp) %>% 
  fill(wave1year, .direction = "downup") %>% 
  mutate(par3before = ifelse(bno == 3 & kyear < wave1year, 1, 0), #This filter creates a dummy indicating that the 3rd child was born before the first interview
         parity = ifelse(!is.na(kdob), bno, NA),
         parity = ifelse(bno == 0, 1, parity)) %>%  #This creates a variable for the parity at risk (our actual variable of interest)
  fill(par3before, .direction = "downup") %>% 
  fill(parity, .direction = "up") %>% 
  mutate(parity = ifelse(is.na(parity), bno + 1, parity)) %>% 
  ungroup() %>% 
  filter(par3filter == 0, par3before == 0) %>% 
  select(-par3filter, -par3before, -parnum, -wave1year, -before, -missingdummy, -kyear)

saveRDS(cball0, file = "cball0.rds")

summary(cball0$hc_inc)
cball0 %>% count(parity)

# -------------------------------------------------------------------------
# Descriptive plots -------------------------------------------------------
# -------------------------------------------------------------------------

cball0 %>% 
  filter(!is.na(hc_inc), hc_inc < 1, hc_inc > 0) %>%
  group_by(period, hcinc_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = hcinc_cat, y = percent)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~period)

cball0 %>% 
  filter(!is.na(hc_inc), hc_inc < 1, hc_inc > 0) %>% 
  group_by(parity, hcinc_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = hcinc_cat, y = percent)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~parity)
