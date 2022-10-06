#Coded by: Brian Buh
#Started on: 16.06.2022
#Last Updated: 04.10.2022

library(tidyverse)
library(haven)
library(lubridate)
library(survival) #for kmtest
library(survminer) #for ggsurvplot
# library(data.table) #Used for dates that dplyr automatically changes to numeric (not used in the end)

#Use df: indhhbhps, indhhukhls, allfert

# Sample selection:
# 1. Remove BHPS Wave 1 (done in third code block cball1)
## indhhbhps %>% filter(wave == 1, sex == 2) %>% count(is.na(xphsg)) [5,341 NA, 52.1%]
# 2. England, Wales, Scotland, N.I. and Ethnic boost)
# 3. individual ages 18-45 (done in third code block "cball")
# 4. observations = 2+ (done in third code block "cball")
# 5. Parity 0-3 (done in third code block "cball0")
# 6. Clock errors (negative, too high, or NA) (done in third code block cball2)
# 7. Event occurs in intermediate wave (Ind. 13) (more than 2 waves missing)
# 8. Individuals (Ind. 14) who have large periods (more than 3 missing consecutive waves) 
# 9. Parents in household
## Filter 1 is done in section 3 df = cball1
## Filter 2????
## Filter 3-5 is done after combining the surveys and adding fertility.
## Filter 6-8 is done after analyzing the extent of the missing values (see notes in "sample_selection")


############################################################################
# BHPS --------------------------------------------------------------------
############################################################################

bhps5 <- indhhbhps %>% 
  select(pidp, wave, hidp, hhorig, istrtdatm, istrtdaty, birthm, birthy, sex, age_dv, xphsn, xpmg, rent, rentg_bh, tenure_dv, hsownd_bh, hsroom, lkmove, hhyneti, 
         gor_dv, qfedhi, mlstat, spinhh, jbstat, plbornc, hgbiom, hgbiof, hhsize, plnowm, plnowy4, paynty) %>% 
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
  mutate(numobs = length(wave),
         qfedhi = ifelse(qfedhi < 0, NA, qfedhi)) %>% 
  fill(qfedhi, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(ratio = hc/hhinc,
         ratiodummy = ifelse(ratio < 0 | ratio > 1, 1, 0)) %>% 
  mutate(isced97 = case_when(
    qfedhi == 1 ~ "6",
    qfedhi >= 2 & qfedhi <= 4 ~ "5",
    qfedhi == 5 | qfedhi == 6  ~ "4",
    qfedhi ==7 ~ "3",
    qfedhi >= 8 & qfedhi <= 13 ~ "2")) %>% 
  mutate(edu = case_when(
    isced97 == 2 ~ "low",
    isced97 == 3 | isced97 == 4  ~ "medium",
    isced97 == 5 | isced97 == 6  ~ "high",
    is.na(isced97) ~ "NA"),
    edu = ifelse(edu == "NA", NA, edu)) %>% 
  mutate(mlstat = ifelse(mlstat < 0, NA, mlstat), #partnership controls
         partner = ifelse(mlstat == 2 | mlstat == 3, "married", "single"),
         partner = ifelse(spinhh == 1 & partner == "single", "cohab", partner),
         emp = ifelse(jbstat < 0, NA, ifelse(jbstat == 1 | jbstat == 2, "emp", ifelse(jbstat == 3, "unemp", ifelse(jbstat == 7, "student", "inactive")))), #employment control
         ukborn = ifelse(plbornc == -8, 1, ifelse(plbornc > 0, 0, NA)),
         parenthh = ifelse(hgbiom > 0 | hgbiof > 0, 1, 0),
         hsroom = ifelse(hsroom < 0, NA, hsroom),
         lkmove = ifelse(lkmove < -1, NA, lkmove)) %>% 
  group_by(pidp) %>% 
  fill(partner, .direction = "downup") %>% 
  fill(emp, .direction = "downup") %>% 
  fill(ukborn, .direction = "downup") %>% 
  ungroup() %>% 
  select(-qfedhi, -mlstat, -spinhh, -jbstat, -plbornc, -hgbiom, -hgbiof) %>% 
  rename("indinc" = "paynty")

  
saveRDS(bhps5, file = "bhps5.rds")

############################################################################
# UKHLS -------------------------------------------------------------------
############################################################################

ukhls5 <- indhhukhls %>% 
  select(pidp, wave, hidp, hhorig, istrtdatm, istrtdaty, birthm, birthy, sex, age_dv, houscost1_dv, rent, rentgrs_dv, tenure_dv, hsownd, hsrooms, hsbeds, lkmove,
         fihhmnnet3_dv, gor_dv, qfhigh_dv, hiqual_dv, f_qfhighoth, marstat_dv, jbstat, plbornc, hgbiom, hgbiof, hgadoptm, hgadoptf,
         hhsize, plnowm, plnowy4, fimnnet_dv) %>% 
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
         ratiodummy = ifelse(ratio < 0 | ratio > 1, 1, 0)) %>%  #Not there are 2712 ratios larger than 1 and 755 less than 0 (2.2%)
    group_by(pidp) %>% 
    fill(qfhigh_dv, .direction = "downup") %>% 
    ungroup() %>% 
    mutate(edu_cat = case_when(
      qfhigh_dv <= 6 ~ "high",
      qfhigh_dv <= 12 & qfhigh_dv >=7 ~ "medium",
      qfhigh_dv >=13 & qfhigh_dv <= 15 ~ "low")) %>% 
    mutate(edu_cat = ifelse(is.na(edu_cat), "other", edu_cat)) %>% #other edu_qf for people at 16, 96 or missing
    mutate(hiqual_edit = ifelse(hiqual_dv == 1 | hiqual_dv == 2, 5.1, ifelse(hiqual_dv == 3, 3.1, ifelse(hiqual_dv == 4, 2.1, ifelse(hiqual_dv == 5, 2.1, NA))))) %>% 
    mutate(hiqual_edit = ifelse(hiqual_edit == 2.1, 2, ifelse(hiqual_edit == 3.1, 3, ifelse(hiqual_edit == 5.1, 5, NA)))) %>% 
    # mutate(edu = ifelse(qfhigh_dv == 96 | qfhigh_dv <= 0, hiqual_edit, qfhigh_dv)) %>% 
    mutate(isced97 = case_when(
      qfhigh_dv == 1 ~ "6",
      qfhigh_dv >= 2 & qfhigh_dv <= 4 | qfhigh_dv == 6 ~ "5",
      # qfhigh_dv == 6 ~ "5",
      qfhigh_dv == 5 ~ "4",
      qfhigh_dv >=7 & qfhigh_dv <= 12 ~ "3",
      qfhigh_dv >= 13 & qfhigh_dv <= 16 ~ "2",
      qfhigh_dv == 96 ~"96",
      qfhigh_dv == -8 ~"-8",
      qfhigh_dv == -9 ~"-9")) %>% 
    mutate(isced97 = ifelse(isced97 == 96 | isced97 <= 0, hiqual_edit, isced97)) %>% 
    mutate(immedu = ifelse(f_qfhighoth >= 1 & f_qfhighoth <= 3, 6, ifelse(f_qfhighoth == 4, 5, ifelse(f_qfhighoth == 5 | f_qfhighoth == 6, 4, 
                                                                                                      ifelse(f_qfhighoth == 7 | f_qfhighoth == 8, 3, 
                                                                                                             ifelse(f_qfhighoth == 9, 2, ifelse(f_qfhighoth == 10, 1, NA))))))) %>%
    mutate(isced97 = ifelse(is.na(isced97), immedu, isced97)) %>% 
    mutate(isced97 = ifelse(is.na(isced97), NA, isced97)) %>% 
    mutate(edu = case_when(
                 isced97 == 2 ~ "low",
                 isced97 == 3 | isced97 == 4  ~ "medium",
                 isced97 == 5 | isced97 == 6  ~ "high",
                 is.na(isced97) ~ "NA"),
           edu = ifelse(edu == "NA", NA, edu),
           edu = ifelse(is.na(edu), edu_cat, edu),
           edu = ifelse(edu == "other", NA, edu)) %>% 
  mutate(partner = ifelse(marstat_dv == 1, "married", ifelse(marstat_dv == 2, "cohab", ifelse(marstat_dv < 0, NA, "single"))), #partnership control
         emp = ifelse(jbstat < 0, NA, ifelse(jbstat == 1 | jbstat == 2, "emp", ifelse(jbstat == 3, "unemp", ifelse(jbstat == 7, "student", "inactive")))), #employment control
         ukborn = ifelse(plbornc == -8, 1, ifelse(plbornc > 0, 0, NA)), #ukborn control
         parenthh = ifelse(hgbiom > 0 | hgadoptm > 0 | hgbiof > 0 | hgadoptf > 0, 1, 0),
         hsroom = hsrooms + hsbeds, #This mirrors the BHPS variable "hsroom" which is not divided like the UKHLS
         hsroom = ifelse(hsroom < 0, NA, hsroom),
         lkmove = ifelse(lkmove < -1, NA, lkmove)) %>% 
  group_by(pidp) %>% 
  fill(partner, .direction = "downup") %>% 
  fill(emp, .direction = "downup") %>% 
  fill(ukborn, .direction = "downup") %>% 
  ungroup() %>% 
  rename("indinc" = "fimnnet_dv") %>% 
  select(-qfhigh_dv, -hiqual_dv, -f_qfhighoth, -hiqual_edit, -immedu, -edu_cat, -marstat_dv, -jbstat, -plbornc, -hgbiom, -hgbiof, -hgadoptm, -hgadoptf, -hsrooms, -hsbeds)
  
saveRDS(ukhls5, file = "ukhls5.rds")


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
## I make the assumption that housing costs over 100% of household income will be recategorized as 1 and negative ratios will be recategorized as 0.
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

saveRDS(cball0, file = "cball0.rds")

# check <- cball0 %>% count(parenthh, age_dv)

# The last dataframe takes a considerable amount of time to process. This continues the data cleaning process with a break to reduce errors
#~ This is completed by steps in the dfs cball1, cball2  ~#
## First, I remove all non wave pre-survey babies since my clock is complete
## Second, I complete the sample selection and filtering
### All observations with a baby before the first interview have wave = NA
### I remove all wave 1 of BHPS observations due to the poor housing cost quality
## Third, I test my clock
### I can see I have some oddities: negative clocks or NA
### I will remove clock numbers that are negative or larger than 27 (see notes for justification)
### The clock NA comes from babies that come after the last interview. If they are less than 2 waves away, I will keep and impute, more than 2 I will remove

cball1 <- cball0 %>% 
  filter(!is.na(wave), wave != 1) %>% 
  #This code block is for testing the clock, the lag and lead variables help determine how big at gaps
  group_by(pidp) %>% 
  mutate(lagwave = lag(wave),
         leadwave = lead(wave),
         wavegap = (leadwave - lagwave) - 1,
         diffwave = wave - leadwave) %>% 
  ungroup() 
  
#These are for testing the quality of the clock and event variables
##The results are in the document "sample_selection"

# Testing for clock outliers
test <- cball1 %>% count(clock)
cball1 %>% count(clock < 0)
filterneg <- cball1 %>% filter(clock < 0)
cball1 %>% count(clock > 27)
filterpos <- cball1 %>% filter(clock > 27)
cball1 %>% count(is.na(clock))

#This is a quicker wave of making sure I keep certain observations with NA in the clock (baby after the last interview)
filterna <- cball1 %>% filter(is.na(clock))
filterna %>% count(diffwave)
##!Make this df for the next step
filterna1 <- filterna %>% 
  select(pidp, wave, diffwave) %>% 
  mutate(nakeep = ifelse(diffwave == -1 | diffwave == -2 | diffwave == 0, 1, 0)) %>% 
  select(-diffwave)

#Testing for NA
test2 <- cball1 %>% filter(event == 1, is.na(istrtdaty))
test2 %>% count(wavegap, obsnum)


#This df filters individuals with clock errors
cball2 <- cball1 %>% 
  left_join(., filterna1, by = c("pidp", "wave")) %>% 
  left_join(., weights, by = c("pidp", "wave")) %>% 
  group_by(pidp) %>% 
  mutate(clockneg = ifelse(clock < 0, 1, NA),
         clockpos = ifelse(clock > 27, 1, NA),
         naevent = ifelse(event == 1 & is.na(istrtdaty), 1, NA), #naevent is events that happened between waves
         largediff = ifelse(diffwave < -3, 1, NA)) %>% #largediff is for individuals that have huge gaps in their records before returning (10301 observations, 3.9% )
  fill(nakeep, .direction = "updown") %>% 
  fill(clockneg, .direction = "downup") %>% 
  fill(clockpos, .direction = "downup") %>% 
  fill(largediff, .direction = "updown") %>% 
  ungroup() %>% 
  #This filters out individuals with clock errors or "ind. 13" with gaps of larger than 2 waves when they experience an event
  #There are also several individuals that simply miss many waves between surveys (similar to Ind 13 but they don't that we know of have a child)
  filter(is.na(clockneg), is.na(clockpos), is.na(nakeep) | nakeep == 1, is.na(naevent), is.na(largediff), !is.na(clock)) %>% #Filter 6, 7, & 8
  mutate(clock2 = clock + 1,
         parity = as.character(parity),
         sex = ifelse(sex < 0, NA, sex),
         sex = as.character(sex),
         sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women"),
         agesq = age_dv*age_dv) %>% 
  group_by(pidp) %>% 
  fill(sex, .direction = "downup") %>% #There are several sex that are NA (690)
  ungroup() %>% 
  filter(!is.na(sex)) %>% #43 observations sex is NA so they are removed
  rename("age" = "age_dv") %>% 
  mutate(ratio_cat2 = cut(ratio, 
                         breaks = c(-0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 1),
                         labels = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100")),
         ratio_cat2 = fct_relevel(ratio_cat2, c("10-20",
                                                "0",
                                                "0.1-10",
                                                "20-30",
                                                "30-40",
                                                "40-100"))) %>% 
  mutate(ratio_cat3 = cut(ratio, 
                          breaks = c(-0.1, 0.0, 0.15, 0.3, 0.45, 0.6, 1),
                          labels = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100")),
         ratio_cat3 = fct_relevel(ratio_cat3, c("15-30",
                                                "0",
                                                "0.1-15",
                                                "30-45",
                                                "45-60",
                                                "60-100")),
         tenure = ifelse(tenure_dv == -9, NA, ifelse(tenure_dv <=2, "owned", ifelse(tenure_dv == 3 | tenure_dv == 4, "social", "rent"))))

# check <- cball2 %>% count(parenthh, age)

saveRDS(cball2, file = "cball2.rds")

# Checking to make sure my filters worked correctly (make cball2 without the filters to see the power of the filters)
cball2 %>% count(nakeep)
cball2 %>% count(clockneg)
cball2 %>% count(clockpos)
cball2 %>% count(naevent)
cball2 %>% count(diffwave)
check <- cball2 %>% filter(diffwave == -3)
cball2 %>% count(largediff)
check <- cball2 %>%  #There is 2 obs with clock as NA still
  group_by(pidp) %>% 
  mutate(check = ifelse(is.na(clock), 1, 0)) %>% 
  fill(check, .direction = "updown") %>% 
  ungroup() %>% 
  filter(check == 1)
#Conclusion the filters remove 16.3% of total observations

#Finally, let's see if my main analysis seems to make sense
clockparitycheck <- cball2 %>% count(clock, event, parity)
#As you would assume, the number of 2&3 children are more relative to 1 children at small numbers but that relationship changes as the number grows


# -------------------------------------------------------------------------
# Adding LAD --------------------------------------------------------------
# -------------------------------------------------------------------------


cballlad <- cball2 %>% 
  left_join(., lad_values, by = c("hidp", "wave")) %>% 
  select(-year.y) %>% 
  rename("year" = "year.x") %>% 
  group_by(pidp) %>% 
  fill(code, .direction = "updown") %>% 
  ungroup()

#There are 618 NA. It looks like missing LAD are due to moves. I will fill "up" to assume the wave happened during the move.
cballlad %>% count(is.na(code))
#After group the NA is reduced to 326

saveRDS(cballlad, file = "cballlad.rds")


# -------------------------------------------------------------------------
# Descriptive plots -------------------------------------------------------
# -------------------------------------------------------------------------
cball2 %>% 
  filter(!is.na(ratio), ratio < 1, ratio > 0) %>%
  ggplot(aes(x = ratio)) + 
  geom_histogram(bins = 10) 

#Distribution of Ratio by Period
cball2 %>% 
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
  ggsave("ratio_distribution_period_s5_29-08-2022.png", dpi = 300)

#Distribution of Ratio by Parity
cball2 %>% 
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
ggsave("ratio_distribution_parity_s5_29-08-2022.png", dpi = 300)


#Age distribution with parents in household
cball2 %>% 
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
ggsave("distribution_age_parenthh_s5_29-08-2022.png", dpi = 300)  

