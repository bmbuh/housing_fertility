#Coded by: Brian Buh
#Started on: 20.06.2022
#Last Updated: 29.07.2021

library(tidyverse)
library(haven)
library(lubridate)

#Notes for the testing process are saved in the docx file "variable_testing_creation_imputation"

### What's been done?
# Outcome: 
## Birth events exists in the correct waves as "kdob"
## event variable called "event" created

# Clock:
## Calculate the time since 18 for parity 1
## Calculate the time since last birth for parity 2&3
## Living with parents in household

# Explanatory:
# Income and housing costs are present have been analyzed and imputed
# The ratio of hc/inc called "ratio"
# Period is done
# Parity is done

# Controls:
## Age is present for all individuals (age/agesq)
## Education
## Number of Rooms
## Housing Tenure
## Partnership
## Job status

# Weights




### What's missing?

# 2. I need to deal with the LAD and the 25% housing price per year (S5)

# 5. I need to look at the controls (S3)
## d. time since moved dummies require work calculating the last move each wave (S3)
## g. Social class (this is frustrating!)



###########################################################################
# Testing BHPS Data -------------------------------------------------------
###########################################################################

# Use the DF "indhhbhps"

# -------------------------------------------------------------------------
# Question 1: Income data quality -----------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
#Testing income data from BHPS/UKHLS files versus UKDA3909

## First Weekly income "hhneti"
ba_hhinc <- ba_hhresp %>% select(ba_hid, ba_hhneti)
ba_hhinc2 <- ba_weekinc %>% 
  select(ahid, ahhneti) %>% 
  rename("ba_hid" = "ahid")

weekinctest <- left_join(ba_hhinc, ba_hhinc2, by = "ba_hid") %>% 
  mutate(ba_hhneti = ifelse(ba_hhneti == -9, NA, ba_hhneti)) %>% 
  mutate(diff = ba_hhneti - ahhneti)
summary(weekinctest$diff) #There is no difference in the net weekly income measures, 685 NA (both sets)

## Second annual income "hhyneti
ba_hhinc3 <- ba_hhresp %>% select(ba_hid, ba_hhyneti)
ba_hhinc4 <- ba_yearinc %>% 
  select(ahid, ahhyneti) %>% 
  rename("ba_hid" = "ahid")

yearinctest <- left_join(ba_hhinc3, ba_hhinc4, by = "ba_hid") %>% 
  mutate(ba_hhyneti = ifelse(ba_hhyneti == -9, NA, ba_hhyneti)) %>% 
  mutate(diff = ba_hhyneti - ahhyneti)
summary(yearinctest$diff) #There are some non-0 numbers
yearinctest2 <- yearinctest %>% filter(diff != 0) #This amounts to 11 observations which would need to removed

yearinctest %>% count(is.na(ahhyneti)) #There are only 6 NA in the full data set, but 685 in the UKDA3909 data set

#In conclusion, it is appropriate to use the BHPS release for net income variables. Generally, it is wise to stick to the 
#current (weekly) measures as they includes local (council) tax which is a significant expense. The data managers of the BHPS
#use a different system to impute the annual income than Jenkins & Levy meaning less NA than the 3909 set, however, this leaves certain
#measures on shakier ground than using the weekly measures. 

# -------------------------------------------------------------------------
#I wanna compare all 18 waves of yearly data from BHPS and UKDA3909 as the annual info in much less in the latter
#I use the year3909 bound DF and the BHPS yearly data

year3909cut <- year3909 %>% 
  select(hid, wave, hhyneti) %>% 
  rename("hidp" = "hid")

#DF inccut created just below
annualcomp <- 
  left_join(inccut, year3909cut, by = c("hidp", "wave")) %>% 
  rename("hhynetibhps" = "hhyneti.x") %>% 
  rename("hhyneti3909" = "hhyneti.y") %>% 
  mutate(annualdiff = hhynetibhps - hhyneti3909)

countdiff <- annualcomp %>% count(is.na(hhynetibhps), is.na(hhyneti3909))
#There are 32377 missing in the 3909 DF that are available in the BHPS data, an additional 5803 NA in both
summary(annualcomp$annualdiff)
annualoutlier <- annualcomp %>% filter(annualdiff != 0) 
# %>% distinct(hid, .keep_all = TRUE)
#There are 193 observations that do not match however these are clustered in 103 HH
summary(annualoutlier$annualdiff)

annualoutlier %>% 
  ggplot(aes(x= annualdiff)) +
  geom_histogram()
#It is a relatively normal distribution around 0 although slightly more negative

# -------------------------------------------------------------------------
#I have included the following income variables:

## Household
# "fihhmngrs_dv" - gross household income: month before interview
# "fihhmb" - HH benefit income month before interview
# "hhneti" - household net income
# "hhyneti" - household annual net income
# "hhnetde" - household current net income equivalent (McClements)
# "hhnetde2" - household current net income equivalent (OECD)
# "bhcinda" - Average inflation over the reference period (Sept 1st - August 31st)

##Individual
# "paynty" - Usual monthly net pay: Sept this year
# "paynti" - Imputation flag

income <- indhhbhps %>% 
  select(pid, wave, hidp, sex, age_dv, fihhmngrs_dv, fihhmb, hhneti, hhyneti, hhnetde, hhnetde2, bhcinda, paynty, paynti, loctax)

#For joining later
inccut <- income %>% select(pid, wave, hidp, hhneti, hhyneti) %>% 
  mutate(hhneti = ifelse(hhneti == -9, NA, hhneti)) %>% 
  mutate(hhyneti = ifelse(hhyneti == -9, NA, hhyneti))

summary(income$age_dv)
summary(income$fihhmngrs_dv) #There are no NA, there are 5706 missing values
summary(income$fihhmb)
summary(income$hhneti) #There are no NA, there can be negative values, there are 38180 missing values
summary(income$hhyneti) #There are no NA, there are 5803 missing values
summary(income$hhnetde) #There are no NA
summary(income$hhnetde2) #There are no NA
summary(income$bhcinda) #There are no NA

summary(income$paynty) #There are no NA, 109109 missing values, missing values at -9, -8, or -7
income %>% filter(hhneti == -9 | hhneti == -7 | hhneti == -8) %>% count(hhneti) #There are no -7 or -8, only -9
#There are 38180 NA

inc2 <- income %>% filter(fihhmngrs_dv == -9) #If this is missing, all income data for the observation is missing
inc3 <- income %>% filter(hhneti > 0) 
summary(inc3$hhneti) #If removing NA and negative hh net income we see a mean of 1192 and a median of 397 pounds per week
inc4 <- income %>% filter(hhyneti == -9) #Much less missing annual than monthly data, however if this is missing almost all other income data is missing (like fihhmngrs_dv)
inc5 <- income %>% filter(paynty <= -7) #Missing values can be -9, -8 or -7

#"hhneti" should equal (hhyneti/365)*7 + loctax
weekyearcomp <- income %>% 
  select(pid, wave, age_dv, hhneti, hhyneti, loctax) %>% 
  mutate(hhneti = ifelse(hhneti == -9, NA, hhneti)) %>% 
  mutate(hhyneti = ifelse(hhyneti == -9, NA, hhyneti)) %>% 
  mutate(loctax = ifelse(loctax == -9, NA, loctax)) %>% 
  mutate(yeartoweek = (hhyneti/365)*7) %>% 
  mutate(diff = yeartoweek-hhneti) %>% 
  mutate(weekminusloctax = yeartoweek-loctax)
summary(weekyearcomp$diff)

weekyearcomp2 <- weekyearcomp %>% filter(!is.na(diff))
sd(weekyearcomp2$diff)


#Histogram of the differences between the self-calculated weekly net hh income and hhneti
weekyearcomp %>% 
  filter(diff > -151.6, diff < 123.3) %>% #This is only keeping values within 1 SD of the mean
  ggplot(aes(x=diff)) +
  geom_histogram()
ggsave("current_annual_bhps_diff_1SD_S4_08-07-2022.png")

# TAKE AWAY:
# Generally, after testing the income measures it appears as in creating a monthly measure from the annual measure “hhyneti” is the best choice. 
# While Jenkins et al. (2010) points out some issues with the use of household proxies for income measures, there is only a small systematic difference 
# between the two measure. On the other hand, needing to impute an additional 13% of the measures can lead to other types of bias. The small systematic difference 
# between the two measures may be due to the addition of the “loctax” (or local tax). However, I do not have the local tax measure available for the UKHLS 
# so in order to have cross survey comparability, it is better that I do not include it in the income measure of the BHPS. 
# In conclusion I will use the hhyneti variable and impute missing waves after selecting a sample.


# -------------------------------------------------------------------------
# Question 2: Housing cost quality ---------------------------------------
# -------------------------------------------------------------------------

#I have included the following housing cost variables:

## Household
# "hcost" - gross housing costs
# "xphsg" - gross monthly housing costs 
# "xphsn" - net monthly housing costs (net of gov't subsidies: "net monthly mortgage or rest. For renters this include partial
# of complete housing benefits. Variable is zero for housing that is rent free or owned outright')
# "rent" - Net amount of last rent payment
# "rentg_bh" - gross rent including housing benefit
# "tenure_dv" - housing tenure
# "hsownd_bh" - house owned or rented
# "hsroom" - number of rooms in accommodation
# "hstype" - type of accommodation

## Individual
# "f139" - Income: housing benefit

# In order to create a housing cost measure that best captures the true cost the following steps must be done:
#   1. After much examination, the most reliable measure appears to be "xphsn" for net housing costs although see notes on complications
#   2. If xpmg, rent, rentg_bh are NA, xphsn = 0, and tenure_dv != 1, then it is falsely reported as 0 even though all information is missing, Change these to NA
#   3. Similar to UKHLS, sometimes the net rent in "rent" is lower then the number in "xphsn". If the number is lower replace it.


hcbhps <- indhhbhps %>% 
  select(pidp, wave, hidp, sex, age_dv, hcost, xphsg, xphsn, xpmg, rent, rentg_bh, tenure_dv, hsownd_bh, hsroom, hstype, f139) %>% 
  mutate(xphsn = ifelse(xphsn == -9 | xphsn == -8 | xphsn == -7, NA, xphsn),
         hcost = ifelse(hcost == -9, NA, hcost),
         xphsg = ifelse(xphsg == -9 | xphsg == -7, NA, xphsg),
         xpmg = ifelse(xpmg < 0, NA, xpmg),
         rent = ifelse(rent == -3 | rent == -4, 0, rent), #"-3" is coded for 100% rent rebate; "-4" is coded as some other one off rent free
         rent = ifelse(rent < 0, NA, rent),
         rentg_bh = ifelse(rentg_bh < 0, NA, rentg_bh),
         tenure_dv = ifelse(tenure_dv < 0, NA, tenure_dv),
         hsownd_bh = ifelse(hsownd_bh < 0, NA, hsownd_bh)) %>%
  filter(wave != 1) %>% #I have to remove Wave 1, more than 50% of all housing cost variables are missing or wrong
  mutate(hc = xphsn,
         hc = ifelse(is.na(xpmg) & is.na(rent) & is.na(rentg_bh) & hc == 0 & tenure_dv != 1, NA, hc),
         hc2 = ifelse(rent < hc, rent, hc),
         hc2 = ifelse(is.na(hc2), hc, hc2)) %>%
  select(-hc) %>%
  rename("hc" = "hc2")

#Many of the inconsistencies with the "xphsn" appear to come from Wave 1
nacount <- hcbhps %>% count(wave, is.na(xphsn))
#After looking at the count data, 5,346 NA (47.3% of all NA are from wave 1). Also 52.1% of Wave 1 hc are NA
nacount1 <- hcbhps %>% mutate(oldage = age_dv >=46) %>%  count(wave, oldage, is.na(xphsn))
#How many NA come from other housing information (tenure) also missing?
nacount2 <- hcbhps %>% count(tenure_dv, is.na(xphsn))
#4,455 NA come from when tenure is also missing; 2292 comes from owned outright, 3433 come from local authority rent
#Owned outright is hc = 0, but local authority rent is a much trickier issue because of housing benefit
#Finally, how many NA are in my final hc measure?
hcbhps %>% count(is.na(hc))
#There are 9,220 NA (4.0%)


# Take away: 
# 1.	If xphsn is missing and there are extremely low numbers in either xpmg, rent, or rentg_bh but a much more normal number in xphsg, assume that xphsg is the closest to the true net housing cost
# 2.	If xphsn is 0 but xpmg and rent are NA, this is not a true 0, remove.
##     a.	Generally, be mistrusting of any housing cost that is 0
# 3.	If rent = hcost, but xphsn is higher, it can be assumed that the true net cost including benefit has not be given. Be careful here!!!!
# 4.	A lot of the mismatches occur in wave 1 of the BHPS. How many wave 1 xphsn are missing? There are 5,346 NA in wave 1 which comprises 47.3% of all NA for the xphsn values and 52.1% of all measures in wave 1. 
# 5.	There are many more odd numbers for older individuals. (65+). For NA values, more are concentrated in higher ages.



###########################################################################
# Testing UKHLS Data ------------------------------------------------------
###########################################################################

# Use the DF "indhhukhls"

# -------------------------------------------------------------------------
# Question 1: Income data quality -----------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
#I have included the following income variables:

## Household
# 1.	"fihhmngrs_dv" -gross household income: month before interview – All Waves
##      a.	I need to add this to UKHLS if I want to use it!!!
# 2.	"fihhmnnet3_dv" – total hh net income – ind/hh deductions
# 3.	"fihhmnnet4_dv" – total hh net income – hb adj & ind/hh deductions


#Step 1:
# What is the difference between fihhmnnet3_dv & fihhmnnet4_dv
incukhls <- indhhukhls %>%
  select(pidp, wave, sex, age_dv, fihhmnnet3_dv, fihhmnnet4_dv) %>%
  mutate(inccheck = fihhmnnet3_dv - fihhmnnet4_dv,
         inccheckdummy = ifelse(inccheck == 0 | is.na(inccheck), 0, 1))


incukhls %>% count(inccheckdummy)
# 15,803 observations (3.3%) have a difference between fihhmnnet3_dv & fihhmnnet4_dv or are NA

incukhls %>% filter(inccheck <= -1000 | inccheck >= 1000)
#There are 360 observations where the difference is larger than 1000 pounds

incukhls %>%
  filter(inccheckdummy == 1, inccheck >= -1000, inccheck <= 1000) %>% 
  ggplot(aes(x= inccheck)) +
  geom_histogram()
#There is an odd distribution. There is a peak centered around 0 and then a second peak centered around -200
#Conclusion:
# while around 3000 observations clearly receive some type of housing benefit, there is too high of a risk of it being double counted 
# in the numerator and denominator. Use the variable fihhmnnet3_dv

incukhls %>% count(is.na(fihhmnnet3_dv))
#There are 3937 NA (0.8%) in the net hh income measure fihhmnnet3_dv


# -------------------------------------------------------------------------
# Question 2: Housing cost quality ----------------------------------------
# -------------------------------------------------------------------------


# 1.	"houscost1_dv" – monthly housing cost including mortgage principal payments
# 2.	"houscost2_dv"– monthly housing cost excluding mortgage principal payments
# 3.	"xpmg" – last total monthly mortgage payment – All Waves
# 4.	"rent" – net amount of last rent payment – All Waves
# 5.	"rentg" – gross rent including housing benefit
##    a.	Does this correspond to the BHPS rentg_dv
# 6.	"rentgrs_dv" – monthly gross rent, including housing benefits
# 7.	"hsownd"- house owned or rented
# 8.	"tenure_dv"– housing tenure – All Waves
##    a.	1 = owned outright, 2 = owned with mortgage, 3 = local authority rent, 4 = housing association rented, 5 = rented from employer, 6 = rented private unfurnished, 7 = rented private furnished, 8 = other rented
# 9.	"hsrooms" – other rooms
# 10.	"hsbeds" – number of bedrooms
##    a.	This is included only in BH01

# In order to create a housing cost measure that best captures the true cost the following steps must be done:
#   1. houscost1_dv is the closest to the correct measure so start with that as a base (houscost1_dv = xpmg + rentgrs_dv)
#   2. Sometime the net hc measure "rent" is clearly lower then the gross measure implying a housing benefit, replace houscost1_dv with rent where rent < houscost1_dv
#   3. There are quite a few people who pay 0 in rent. The possible reasons:
#       a. They own the house outright (hsownd == 1)
#       b. They live rent free (hsownd == 5)
#       c. Housing benefit covers the entire cost
#       d. It is an error from missing data (this is the worrying one)
#   4. If there is a zero where there is an NA in rentgrs_dv, we can assume it is because of error from missing data. Replace with NA and impute late

hcukhls <- indhhukhls %>%
  select(pidp, wave, sex, age_dv, houscost1_dv, xpmg, rent, rentg, rentgrs_dv, tenure_dv, hsownd) %>%
  mutate(rent2 = ifelse(rent < 0, NA, rent),
         rent2 = ifelse(rent2 > houscost1_dv, NA, rent2),
         hc = ifelse(!is.na(rent2), rent2, houscost1_dv),
         tooold = age_dv >= 46,
         rentgrs_dv = ifelse(rentgrs_dv == -8, NA, rentgrs_dv),
         hc = ifelse(is.na(rentgrs_dv) & hsownd != 1 & hsownd != 5 & hc == 0, NA, hc))

#Test how many NA are in my housing cost variable
hcukhls %>% count(is.na(hc))
#There are 7,608 NA (1.6%)

#Test how many hc = 0 are owned outright versus HB
## hc = 0 in 180,390 observations (37.9%)
hcukhls2 <- hcukhls %>%
  filter(hc == 0) %>%
  mutate(nocostcause = case_when(hsownd == 1 ~ "owned",
                                 hsownd == 5 ~ "rentfree",
                                 hsownd != 1 & hsownd != 5 ~ "other"))
  
hcukhls3 %>% count(nocostcause, tooold)
# 154,434 are owned outright; 4,158 are rent free; 21,798 are 0 but not for obvious reason (4.6%)
# Of those other, 11,041 are older then the sample limit (2.3% of total, or 50.7% of others)
hcukhls3 %>% count(tooold)
#Most of those who pay 0 rent are too old for the sample; 141,599 (78.5%)


###########################################################################
# Testing cross survey consistency ----------------------------------------
###########################################################################

# In order to keep clarity I will recreate the income and housing cost variables here
## Use DF indhhbhps & indhhukhls
##Call created datasets here by the survey name and by the script number and section (43)

#BHPS
bhps43 <- indhhbhps %>% 
  select(pidp, wave, hhorig, sex, age_dv, xphsn, xpmg, rent, rentg_bh, tenure_dv, hsownd_bh, hhyneti) %>% 
  mutate(xphsn = ifelse(xphsn == -9 | xphsn == -8 | xphsn == -7, NA, xphsn),
         xpmg = ifelse(xpmg < 0, NA, xpmg),
         rent = ifelse(rent == -3 | rent == -4, 0, rent), #"-3" is coded for 100% rent rebate; "-4" is coded as some other one off rent free
         rent = ifelse(rent < 0, NA, rent),
         rentg_bh = ifelse(rentg_bh < 0, NA, rentg_bh),
         tenure_dv = ifelse(tenure_dv < 0, NA, tenure_dv),
         hsownd_bh = ifelse(hsownd_bh < 0, NA, hsownd_bh),
         survey = "bhps") %>%
  filter(wave != 1) %>% #I have to remove Wave 1, more than 50% of all housing cost variables are missing or wrong
  mutate(hc = xphsn,
         hc = ifelse(is.na(xpmg) & is.na(rent) & is.na(rentg_bh) & hc == 0 & tenure_dv != 1, NA, hc),
         hc2 = ifelse(rent < hc, rent, hc),
         hc2 = ifelse(is.na(hc2), hc, hc2)) %>%
  select(-hc) %>%
  rename("hc" = "hc2") %>%
  rename("hsownd" = "hsownd_bh") %>%
  mutate(hhyneti = ifelse(hhyneti < 0, NA, hhyneti),
         hhinc = hhyneti/12) %>%
  select(-xphsn, -xpmg, -rent, -rentg_bh, -hhyneti)


#UKHLS
ukhls43 <- indhhukhls %>%
  select(pidp, wave, hhorig, sex, age_dv, houscost1_dv, rent, rentgrs_dv, tenure_dv, hsownd, fihhmnnet3_dv) %>%
  mutate(rent2 = ifelse(rent < 0, NA, rent),
         rent2 = ifelse(rent2 > houscost1_dv, NA, rent2),
         hc = ifelse(!is.na(rent2), rent2, houscost1_dv),
         tenure_dv = ifelse(tenure_dv < 0, NA, tenure_dv),
         hsownd = ifelse(hsownd < 0, NA, hsownd),
         rentgrs_dv = ifelse(rentgrs_dv == -8, NA, rentgrs_dv),
         hc = ifelse(is.na(rentgrs_dv) & hsownd != 1 & hsownd != 5 & hc == 0, NA, hc),
         survey = "ukhls") %>%
  rename("hhinc" = "fihhmnnet3_dv") %>%
  select(-houscost1_dv, -rent, -rentgrs_dv, -rent2)

#Combined
# I will keep only those individuals who were in both BHPS and UKHLS and the waves that are three on each side to compare (waves 16, 17, 18, 20, 21)
##Note: Individuals from BHPS who continued were not in USoc Wave 1 (wave 19) since they did BHPS 18
both43 <- 
  bind_rows(bhps43, ukhls43) %>%
  arrange(pidp, wave) %>%
  filter(hhorig ==3 | hhorig == 4 | hhorig == 5, wave == 16 | wave == 17 | wave == 18 | wave == 20 | wave == 21) %>%
  mutate(ukhlsdummy = ifelse(survey == "ukhls", 1, NA)) %>% #The next step help remove individuals who did not continue to UKHLS
  group_by(pidp) %>%
  fill(ukhlsdummy, .direction = "up") %>%
  ungroup() %>%
  filter(ukhlsdummy == 1, age_dv < 46) %>%
  mutate(ratio = hc/hhinc,
         ratiodummy = ifelse(ratio < 0 | ratio > 1, 1, 0))

#How many ratios fall out of the 0 - 1 range (in this small sample)?
both43 %>% count(ratiodummy)
## 372 are out of the range (0.2%) and 877 (0.4%) are NA

#Histogram of monthly hh income
both43 %>%
  filter(!is.na(hhinc), hhinc < 15000) %>%
  # mutate(ratio = ifelse(ratio > 1, 1, ratio),
  #        ratio = ifelse(ratio < 0, 0, ratio)) %>%
  ggplot(aes(x = hhinc, fill = survey)) + 
  geom_histogram()
ggsave("hhinc_distribution_both43_s4_29-07-2022.png")

#Histogram of monthly hc
both43 %>%
  filter(!is.na(hc), hc < 2000) %>%
  ggplot(aes(x = hc, fill = survey)) + 
  geom_histogram()
ggsave("hc_distribution_both43_s4_29-07-2022.png")


#Histogram of the distribution of ratios
both43 %>%
  filter(!is.na(ratio)) %>%
  mutate(ratio = ifelse(ratio > 1, 1, ratio),
         ratio = ifelse(ratio < 0, 0, ratio)) %>%
  ggplot(aes(x = ratio, fill = survey)) + 
  geom_histogram()
ggsave("ratio_distribution_both43_s4_29-07-2022.png")






# -------------------------------------------------------------------------
# Old Code from script 5 looking at BHPS net versus gross housing costs----
# -------------------------------------------------------------------------


bhps5 %>% count(is.na(hc))
bhps5 %>% count(is.na(ratiodummy))
bhps5 %>% count(wave)

# Capturing the difference between gross and net housing costs
bhps5 %>% count(ngdiffdummy)
## There are 21,874 observations with a difference between net and gross (9.2%) (13,034 NA or 5.5%)
bhps5 %>% 
  filter(!is.na(ngdiff), ngdiff <= 500, ngdiff >= -500) %>% 
  ggplot(aes(x = ngdiff)) + 
  geom_histogram()
## There appears to be a distribution of differences between 0 and 350 pounds/month

#Looking at the distribution of my ratio
##There are 505 cases where the cost of housing exceeds hh income (aka ratio > 1)
bhps5 %>% 
  filter(!is.na(hc_inc), hc_inc <= 1) %>%
  ggplot(aes(x = hc_inc)) + 
  geom_histogram()



            