#Coded by: Brian Buh
#Started on: 16.06.2022
#Last Updated: 29.07.2022

###Note: this is left from the EPC draft. The variables need to be tested first and the controls need to be kept.

library(tidyverse)
library(haven)
library(lubridate)
# library(data.table) #Used for dates that dplyr automatically changes to numeric (not used in the end)

#Use df: indhhbhps, indhhukhls, allfert

# Sample selection:
# 1. Remove BHPS Wave 1 (done in first code block "bhps5")!!!! This fucked up the parity count!!!!

# 2. England, Wales, Scotland (no N.I.)
# 3. individual ages 18-45 (done in third code block "cball")
# 4. observations = 2+ (done in third code block "cball")
# 5. Parity 0-3 
## Filter 1 is done while cleaning the BHPS data individual. 
## Filter 2????
## Filter 3-5 is done after combining the surveys and adding fertility.


############################################################################
# BHPS --------------------------------------------------------------------
############################################################################

bhps5 <- indhhbhps %>% 
  select(pidp, wave, hhorig, istrtdatm, istrtdaty, birthm, birthy, sex, age_dv, xphsn, xpmg, rent, rentg_bh, tenure_dv, hsownd_bh, hhyneti, gor_dv) %>% 
  mutate(year = wave + 1990,
         hhyneti = ifelse(hhyneti < 0, NA, hhyneti),
         hhinc = (hhyneti/12),
         xphsn = ifelse(xphsn == -9 | xphsn == -8 | xphsn == -7, NA, xphsn),
         xpmg = ifelse(xpmg < 0, NA, xpmg),
         rent = ifelse(rent == -3 | rent == -4, 0, rent), #"-3" is coded for 100% rent rebate; "-4" is coded as some other one off rent free
         rent = ifelse(rent < 0, NA, rent),
         rentg_bh = ifelse(rentg_bh < 0, NA, rentg_bh),
         tenure_dv = ifelse(tenure_dv < 0, NA, tenure_dv),
         hsownd_bh = ifelse(hsownd_bh < 0, NA, hsownd_bh),
         survey = "bhps",
         period = case_when(year >= 1991 & year <= 1999 ~ "1991-1999",
                            year >= 2000 & year <= 2007 ~ "2000-2007",
                            year >= 2008 & year <= 2012 ~ "2008-2012",
                            year >= 2013 & year <= 2021 ~ "2013-2021")) %>% 
  relocate("year", .after = "wave") %>% 
  # filter(wave != 1) %>% #I have to remove Wave 1, more than 50% of all housing cost variables are missing or wrong
  mutate(hc = xphsn,
         hc = ifelse(is.na(xpmg) & is.na(rent) & is.na(rentg_bh) & hc == 0 & tenure_dv != 1, NA, hc),
         hc2 = ifelse(rent < hc, rent, hc),
         hc2 = ifelse(is.na(hc2), hc, hc2)) %>%
  select(-hc) %>%
  rename("hc" = "hc2") %>%
  rename("hsownd" = "hsownd_bh") %>%
  select(-xphsn, -xpmg, -rent, -rentg_bh, -hhyneti) %>%
  #Variable creation
  group_by(pidp) %>% 
  mutate(numobs = length(wave)) %>% 
  ungroup() %>% 
    mutate(ratio = hc/hhinc,
           ratiodummy = ifelse(ratio < 0 | ratio > 1, 1, 0))
  

saveRDS(bhps5, file = "bhps5.rds")






#Sample selection
  # filter(hhorig != 6, hhorig != 14, hhorig != 15, hhorig != 16, age_dv >= 18, age_dv <=45.99,
         # numobs > 1
         # ) %>% 
  # select(pidp, wave, year, period, hhorig, istrtdatm, istrtdaty, sex, age_dv, gor_dv, hc, mnhhinc, ratio, ratiodummy, numobs)


bhps5 %>% count(numobs)




############################################################################
# UKHLS -------------------------------------------------------------------
############################################################################

ukhls5 <- indhhukhls %>% 
  select(pidp, wave, hhorig, istrtdatm, istrtdaty, birthm, birthy, sex, age_dv, houscost1_dv, rent, rentgrs_dv, tenure_dv, hsownd, fihhmnnet3_dv, gor_dv) %>% 
  mutate(rent2 = ifelse(rent < 0, NA, rent),
         rent2 = ifelse(rent2 > houscost1_dv, NA, rent2),
         hc = ifelse(!is.na(rent2), rent2, houscost1_dv),
         tenure_dv = ifelse(tenure_dv < 0, NA, tenure_dv),
         hsownd = ifelse(hsownd < 0, NA, hsownd),
         rentgrs_dv = ifelse(rentgrs_dv == -8, NA, rentgrs_dv),
         hc = ifelse(is.na(rentgrs_dv) & hsownd != 1 & hsownd != 5 & hc == 0, NA, hc),
         survey = "ukhls") %>%
  rename("hhinc" = "fihhmnnet3_dv") %>%
  select(-houscost1_dv, -rent, -rentgrs_dv, -rent2) %>%
  mutate(year = wave + 1990,
         period = case_when(year >= 1991 & year <= 1999 ~ "1991-1999",
                            year >= 2000 & year <= 2007 ~ "2000-2007",
                            year >= 2008 & year <= 2012 ~ "2008-2012",
                            year >= 2013 & year <= 2021 ~ "2013-2021"),
         hhinc = ifelse(hhinc < 0, 0, hhinc)) %>% #To get rid of negative ratios
  relocate("year", .after = "wave") %>% 
  #Variable creation
  group_by(pidp) %>% 
  mutate(numobs = length(wave)) %>% 
  ungroup() %>% 
  mutate(ratio = hc/hhinc,
         ratiodummy = ifelse(ratio < 0 | ratio > 1, 1, 0)) #Not there are 2712 ratios larger than 1 and 755 less than 0 (2.2%)
  
saveRDS(ukhls5, file = "ukhls5.rds")
  
  # #Sample selection
  # filter(hhorig != 2, hhorig !=6, hhorig != 8, age_dv >= 18, age_dv <=45.99, 
  #        # numobs > 1
  #        ) %>% 
  # select(pidp, wave, year, period, hhorig, istrtdatm, istrtdaty, sex, age_dv, gor_dv, hc, mnhhinc, ratio, ratiodummy, numobs)
# 
# 
# ukhls5 %>% count(numobs)
# # -------------------------------------------------------------------------
# # Descriptive plots -------------------------------------------------------
# # -------------------------------------------------------------------------
# 
# #Looking at the distribution of my ratio
# ##There are 505 cases where the cost of housing exceeds hh income (aka ratio > 1)
# ukhls5 %>% 
#   filter(!is.na(ratio), ratio < 1, ratio > 0) %>%
#   ggplot(aes(x = ratio)) +  
#   geom_histogram()
# 
# ukhls5 %>% count(ratio < 0)
# 


############################################################################
# combined ----------------------------------------------------------------
############################################################################

allfert_cut <- allfert %>% 
  select(pidp, wave, kdob, bno) %>% 
  group_by(pidp) %>% 
  mutate(totchild = length(pidp)) %>% 
  filter(bno <= 3, !is.na(kdob)) #I lose about 6000 higher parity births (exclusively from UKHLS)

# Combining BHPS and UKHLS and adding fertility  
## I make the assumption that housing costs over 100% of household income will be recategorized as 1 and negative ratios will be recatoegirzed as 0.
## The logic is fixed that a person who spends more on house then they earn clearly is at equal to maxed out. Negative ratios come generally from negative income.
cball <- 
  bind_rows(bhps5, ukhls5) %>% 
  arrange(pidp, wave) %>% 
  filter(age_dv < 45.99, age_dv >= 18, numobs > 1) %>% #Filter 3 & 4
  mutate(ratio = ifelse(ratio > 1, 1, ifelse(ratio < 0, 0, ratio)), 
         ratio_cat = cut(ratio, 
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
# 3 it deals with the filter #5 issue (3rd births that occur before first interview or waves after the 3rd observed birth)
cball0 <- cball %>% 
  left_join(., realobsnum, by = c("pidp", "wave")) %>% 
  mutate(obsnum = ifelse(before == 1, -1, obsnum), #This sets all births that come before the first interview to observation "-1"
         obsnum = ifelse(missingdummy == 1, -2, obsnum), #This sets all missing intermediate waves in which a birth occurred to obsnum "-2"
         wave = ifelse(obsnum == -1, NA, wave)) %>% 
  distinct(pidp, wave, kdob, .keep_all = TRUE) %>% #with the process of moving the births that happen before the first interview, we end up with some duplicated observations
  group_by(pidp) %>% 
  mutate(kyear = year(ymd(kdob))) %>%
  fill(kyear, .direction = "down") %>%
  fill(bno, .direction = "down") %>% 
  fill(totchild, .direction = "down") %>% 
  fill(hhorig, .direction = "downup") %>% 
  fill(birthm, .direction = "down") %>% 
  fill(birthy, .direction = "down") %>% 
  mutate(bno = ifelse(is.na(bno), 0, bno)) %>% 
  mutate(totchild = ifelse(is.na(totchild), 0, totchild)) %>% 
  ungroup() %>% 
  group_by(pidp, bno) %>% 
  mutate(parnum = row_number()) %>% #These step is necessary for filter 4 - remove observations past the birth of the third child
  ungroup() %>% 
  mutate(wave1year = ifelse(obsnum == 1, year, NA),
         par3filter = ifelse(bno == 3 & parnum >= 2, 1, 0)) %>% #This filter creates a dummy for all observations after the observation in which the 3rd child is born
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
  select(-par3filter, -par3before, -parnum, -wave1year, -before, -missingdummy) %>%
  mutate(year = as.numeric(year),
         birthy = as.numeric(birthy),
         age_dv = ifelse(is.na(age_dv), year - birthy, age_dv),
         clock = ifelse(parity == 1, age_dv - 18, NA),
         kyear2 = lag(kyear),
         clock = ifelse(is.na(clock), (year - kyear2) + 1, clock),
         event = ifelse(!is.na(kdob), 1, 0))



str(cball0)

saveRDS(cball0, file = "cball0.rds")

summary(cball0$ratio)
cball0 %>% count(parity)

# -------------------------------------------------------------------------
# Descriptive plots -------------------------------------------------------
# -------------------------------------------------------------------------

cball0 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>%
  group_by(period, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~period)

cball0 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>% 
  group_by(parity, ratio_cat) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = ratio_cat, y = percent)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~parity)
