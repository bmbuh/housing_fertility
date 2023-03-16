#Coded by: Brian Buh
#Started on: 13.09.2022
#Last Updated: 14.02.2022

#This script imputs missing data for the following variables:
## hc, hhinc, tenure, hsroom, lkmove, edu, partner, emp, ukborn
### this also includes remaking variables: ratio, ratio_cat, ratio_cat2, ratio_cat3

cballlad <- file.choose()
cballlad <- readRDS(cballlad)

library(tidyverse)
library(mice)

###########################################################################
# Examine level of missing data for all variables -------------------------
###########################################################################


cballlad %>% count(pidp) #No NA
wave <- cballlad %>% count(wave) #No NA
year <- cballlad %>% count(year) #No NA
hhorig <- cballlad %>% count(hhorig) #No NA
birthy <- cballlad %>% mutate(birthy = ifelse(birthy < 0, NA, birthy)) %>% count(birthy) #289 NA

clock <- cballlad %>% count(clock) #No NA
event <- cballlad %>% count(event) #No NA
code <- cballlad %>% mutate(code = ifelse(code < 0, NA, code)) %>% count(code) #335 NA

#Controls
sex <- cballlad %>% count(sex) #No NA
age <- cballlad %>% count(age) #No NA
aqesq <- cballlad %>% count(agesq) #No NA
edu <- cballlad %>% count(edu) #6727 NA
checkedu <- cballlad %>% filter(is.na(edu)) %>% distinct(pidp) #There are 1997 individuals with no education information in any wave
partner <- cballlad %>% count(partner) #10 NA
emp <- cballlad %>% count(emp) #2 NA
ukborn <- cballlad %>% count(ukborn) #601 NA
parenthh <- cballlad %>% count(parenthh) #NO NA


#housing mediators
tenure <- cballlad %>% count(tenure) #4846 NA
hsroom <- cballlad %>% mutate(hsroom = ifelse(hsroom < 0, NA, hsroom)) %>% count(hsroom) #309 NA
tenurehsroom <- cballlad %>% mutate(hsroom = ifelse(hsroom < 0, NA, hsroom)) %>% count(tenure, hsroom) #4068 are NA for both, 307 are NA for hsroom but not tenure, 708 are NA for tenure but not hsroom
lkmove <- cballlad  %>% count(lkmove) #17610 are NA (or proxy)
hhsize <- cballlad  %>% count(hhsize) #171 NA

#explanatory variables
hhinc <- cballlad %>% count(hhinc) #3865 NA
indinc <- cballlad %>% count(is.na(indinc)) #No NA
summary(cballlad$hhinc)
hc <- cballlad %>% count(hc) #6702 NA
summary(cballlad$hc)
ratio <- cballlad %>% count(ratio) #7590 NA
period <- cballlad %>% count(period) #No Na
parity <- cballlad %>% count(parity) #No NA




###########################################################################
# Imputation --------------------------------------------------------------
###########################################################################

#With the very small numbers listwise deletion of "partner", "emp", and "ukborn" makes sense
#I start with LOCF (last observation carried forward)
ladimp <- cballlad %>% 
  group_by(pidp) %>% 
  # fill(hhinc, .direction = "down") %>% 
  # fill(hc, .direction = "down") %>% 
  fill(tenure, .direction = "downup") %>% 
  fill(lkmove, .direction = "down") %>% 
  fill(edu, .direction = "downup") %>% 
  fill(partner, .direction = "downup") %>% 
  fill(emp, .direction = "downup") %>% 
  fill(ukborn, .direction = "downup") %>% 
  fill(hsroom, .direction = "down") %>% 
  fill(hsownd, .direction = "down") %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(partner), !is.na(emp), !is.na(ukborn)) %>% 
  #There are many variables left from the EHA process that I remove here
  dplyr::select(-ratio, -ratiodummy, -ratio_cat, -ratio_cat2, -ratio_cat3, -tenure_dv,
         -istrtdatm, -istrtdaty, -birthm, -birthy, -gor_dv, -survey, -numobs, -isced97,
         -kdob, -bno, -totchild, -kyear, -obsnum, -totobs, -kyear2, -lagwave, -leadwave,
         -wavegap, -diffwave, -nakeep, -clockneg, -clockpos, -naevent, -largediff, -clock2,
         -name, -lowquar, -oci, -indshare, #remove oci and indshare here and recreate after imputation
         -plnowm, -plnowy4, -urban
         ) %>% 
  mutate(paremp = ifelse(partner == "single", "single", paremp)) %>% 
  #change lkmove "I don't know" to NA
  mutate(lkmove = ifelse(lkmove == -1, NA, lkmove),
         lkmove = ifelse(lkmove == 1, 0, ifelse(lkmove == 2, 1, NA)),
         hsroom = as.character(hsroom),
         lkmove = as.character(lkmove),
         ukborn = as.character(ukborn),
         pidp = as.numeric(pidp),
         hidp = as.numeric(hidp),
         hidp = ifelse(hidp <= 0, NA, hidp),
         hhorig = as.numeric(hhorig),
         hhorig = ifelse(hhorig <= 0, NA, hhorig),
         hhsize = as.numeric(hhsize),
         hhsize = ifelse(hhsize <= 0, NA, hhsize),
         hsroom = as.numeric(hsroom)) %>% 
  mutate(edu = as.factor(edu),
         partner = as.factor(partner),
         emp = as.factor(emp),
         paremp = as.factor(paremp),
         tenure = as.factor(tenure))

ladimp %>% count(paremp, partner)

str(ladimp)

#This removes the LAD which we add in later but don't want to compute now
macro <- cballlad %>% 
  dplyr::select(pidp, wave, name, lowquar, plnowm, plnowy4, urban)


#Summary statistics
p_missing <- unlist(lapply(ladimp, function(x) sum(is.na(x))))/nrow(ladimp)
sort(p_missing[p_missing > 0], decreasing = TRUE) #largest missing in 13% with name
str(ladimp)

summary(ladimp$hhinc) #last observation forward keeps the summary statistics close to the stats with NA (308 NA remaining)
summary(cballlad$hc)
summary(ladimp$hc) #last observation forward keeps the summary statistics close to the stats with NA (617 NA remaining)
summary(ladimp$indinc) #No missing!
summary(ladimp$hhsize) #171 NA
summary(ladimp$hsroom) #294 NA
ladimp %>% count(lkmove) #9851 NA

#Split the sample so that MICE doesn't impute single employment to non-single individuals
ladimpcouple <- ladimp %>% 
  filter(partner != "single")

ladimpsingle <- ladimp %>% 
  filter(partner == "single")


# ------------------------------------------------------------------------------------------
#Imputation --------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------

# We run the mice code with 0 iterations 
impcouple <-  mice(ladimpcouple, maxit=0) #"cart" = Classification and regression trees
ladimpcouple2 = complete(impcouple, 3)

p_missing <- unlist(lapply(ladimpcouple2, function(x) sum(is.na(x))))/nrow(ladimpcouple2)
sort(p_missing[p_missing > 0], decreasing = TRUE)

impsingle <-  mice(ladimpsingle, maxit=0) #"cart" = Classification and regression trees
ladimpsingle2 = complete(impsingle, 3)

p_missing <- unlist(lapply(ladimpsingle2, function(x) sum(is.na(x))))/nrow(ladimpsingle2)
sort(p_missing[p_missing > 0], decreasing = TRUE)

ladimp2 <- 
  bind_rows(ladimpcouple2, ladimpsingle2)

ladimp2 %>% count(partner, paremp)


# Extract predictorMatrix and methods of imputation 
predM <- imp$predictorMatrix
meth <- imp$method

# If you like, view the first few rows of the predictor matrix
head(predM)

### Specify a separate imputation model for variables of interest 
# Continuous variable
pmm <- c("hc", "hhinc", "hsroom", "hhsize")

# Ordered categorical variables 
# poly <- c("hsroom") #Not used currently

# Dichotomous variable
log <- c("lkmove")

# Unordered categorical variable 
poly2 <- c("tenure", "edu")

# Turn their methods matrix into the specified imputation models
meth[pmm] <- "pmm"
meth[cart] <- "cart"
# meth[poly] <- "polr"
meth[log] <- "logreg"
meth[poly2] <- "polyreg"

meth

# With this command, we tell mice to impute the ladimp data, create 5
# datasets, use predM as the predictor matrix and don't print the imputation
# process. If you would like to see the process, set print as TRUE
imp2 <- mice(ladimp, maxit = 5, 
             predictorMatrix = predM, 
             print =  FALSE)

# Look at head and tail of imputed values for hc variable 
head(imp2$imp$hc)
tail(imp2$imp$hc)

# First, turn the datasets into long format
imp2_long <- mice::complete(imp2, action="long", include = TRUE)

# Convert two variables into numeric
imp2_long$patriot_amident <- with(imp2_long, 
                                     as.integer(imp2_long$patriot_amident))
imp2_long$pid_x <- with(imp2_long, 
                           as.integer(imp2_long$pid_x))

# Take log of M&A variable 
imp2_long$LogMANO<-log(imp2_long$MANo+1.01)

# Convert back to mids type - mice can work with this type
imp2_long_mids<-as.mids(imp2_long)
# Regression 

fitimp <- with(imp2_long_mids,
               glm(event ~ clock + hc + hhinc + parity + period + age + agesq))

summary(pool(fitimp))

#Select completed df
ladimp2 = complete(imp2, 3)

p_missing <- unlist(lapply(ladimp2, function(x) sum(is.na(x))))/nrow(ladimp2)
sort(p_missing[p_missing > 0], decreasing = TRUE)

ladimp2 %>% count(is.na(lkmove))

summary(cballlad$hhinc)
summary(ladimp2$hhinc) #last observation forward keeps the summary statistics close to the stats with NA (308 NA remaining)
summary(cballlad$hc)
summary(ladimp2$hc) #last observation forward keeps the summary statistics close to the stats with NA (617 NA remaining)
summary(ladimp2$oci2)
ladimp2 %>% count(is.na(hsroom))
ladimp2 %>% count(is.na(tenure))

check <- ladimp2 %>% 
  group_by(pidp) %>% 
  mutate(numobs = length(pidp)) %>% 
  ungroup() %>% 
  filter(is.na(tenure))
#This shows that of the remaining NA in tenure and hsrooms, a lot of the individuals have no housing information in any wave
#I would suggest listwise deletion in these cases

check2 <- ladimp2 %>% 
  group_by(pidp) %>% 
  mutate(numobs = length(pidp)) %>% 
  ungroup() %>% 
  filter(is.na(edu))
#For education we can decide later if we remove NA in education


# -------------------------------------------------------------------------
# Make final df with imputed values ---------------------------------------
# -------------------------------------------------------------------------


ladimp3 <- ladimp2 %>% 
  left_join(., macro, by = c("pidp", "wave")) %>% 
  # filter(!is.na(tenure), !is.na(hsroom)) %>% 
  mutate(oci = ifelse(hsroom <= hhsize, 1, 0), #oci is the overcrowding index
         oci = as.numeric(oci),
         ownout = ifelse(hsownd == 1 & hc == 0, 1, 0),
         indinc = ifelse(indinc < 0, 0, indinc),
         indshare = indinc/hhinc,
         indshare = ifelse(indshare > 1, 1, indshare),
         indshare = ifelse(is.na(indshare), 0, indshare), #some hhinc are 0 resulting in NA (cannot divide by 0)
         share = ifelse(indshare <= 0.45, "mb", ifelse(indshare >= 0.55, "fb", "equal")),
         share = fct_relevel(share, c("mb", "equal","fb")),
         ratio = hc/hhinc,
         ratio = ifelse(ratio > 1, 1, ifelse(ratio < 0, 0, ratio)),
         ratio = ifelse(is.na(ratio), 0, ratio), #there are some observations where the hhinc is 0 (cannot divide by 0)
         ratio_cat = cut(ratio, 
                         breaks = c(-0.1, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                         labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")),
         ratio_cat2 = cut(ratio, 
                          breaks = c(-0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 1),
                          labels = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100")),
         ratio_cat2 = fct_relevel(ratio_cat2, c("10-20",
                                                "0",
                                                "0.1-10",
                                                "20-30",
                                                "30-40",
                                                "40-100")),
         ratio_cat2alt = cut(ratio, 
                          breaks = c(-0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 1),
                          labels = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100")),
         ratio_cat2alt = fct_relevel(ratio_cat2, c("20-30",
                                                "0",
                                                "0.1-10",
                                                "10-20",
                                                "30-40",
                                                "40-100")),
         ratio_cat2alt2 = cut(ratio, 
                             breaks = c(-0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 1),
                             labels = c("0", "0.1-10", "10-20", "20-30", "30-40", "40-100")),
         ratio_cat2alt2 = fct_relevel(ratio_cat2, c("0.1-10",
                                                   "0",
                                                   "10-20",
                                                   "20-30",
                                                   "30-40",
                                                   "40-100")),
         ratio_cat3 = cut(ratio, 
                          breaks = c(-0.1, 0.0, 0.15, 0.3, 0.45, 0.6, 1),
                          labels = c("0", "0.1-15", "15-30", "30-45", "45-60", "60-100")),
         ratio_cat3 = fct_relevel(ratio_cat3, c("15-30",
                                                "0",
                                                "0.1-15",
                                                "30-45",
                                                "45-60",
                                                "60-100")),
         age_cat = case_when(age >= 18 & age <= 24 ~ "18-24",
                            age >= 25 & age <= 29 ~ "25-28",
                            age >= 30 & age <= 34 ~ "30-34",
                            age >= 35 & age <= 39 ~ "25-39",
                            age >= 40 & age <= 45 ~ "40-45"),
         age_cat2 = case_when(age >= 18 & age <= 29 ~ "u29",
                              age >= 30 & age <= 45 ~ "o30"),
         hhemp = case_when(paremp == "single" ~ "single",
                           emp == "emp" & paremp == "emp" ~ "bothemp",
                           emp == "unemp" & paremp == "emp" ~ "egounemp",
                           emp == "student" & paremp == "emp" ~ "egoinactive",
                           emp == "inactive" & paremp == "emp" ~ "egoinactive",
                           emp == "unemp" | emp == "inactive" | emp == "student" & paremp == "unemp" ~ "bothunemp",
                           emp == "student" & paremp == "student" ~ "bothunemp",
                           emp == "student" & paremp == "inactive" ~ "bothunemp",
                           emp == "emp" & paremp == "unemp" | paremp == "student" | paremp == "inactive" ~ "egoemp"))
  
ladimp3 %>% count(hhemp)


#Median yearly hh income
medhhinc <- ladimp3 %>% 
  group_by(wave) %>% 
  summarise(medhhinc = median(hhinc))

#Median low quartile housing price
medlowquar <- ladimp3 %>% 
  filter(!is.na(lowquar)) %>% 
  group_by(wave) %>% 
  summarise(medlowquar = median(lowquar))

hcfert <- ladimp3 %>% 
  left_join(., medhhinc, by = "wave") %>% 
  left_join(., medlowquar, by = "wave") %>% 
  mutate(medinc = ifelse(medhhinc >= hhinc, 1, 0),
         medlowquar = ifelse(medlowquar >= lowquar, 1, NA))

hcfert %>% count(!is.na(lowquar), medlowquar) #Just about 50% of the LAD observations are above the median
hcfert %>% count(wave, medlowquar) #Just about 50% of the LAD observations are above the median


saveRDS(hcfert, file = "hcfert.rds")
str(hcfert)

hcfert %>% count(hhemp)

hcfert %>% count(emp, paremp, hhemp)
hcfert %>% count(partner, paremp)

hcfert %>% count(ownout)

p_missing <- unlist(lapply(hcfert, function(x) sum(is.na(x))))/nrow(hcfert)
sort(p_missing[p_missing > 0], decreasing = TRUE)
#There are still 3.8% of lkmove NA and 2.1% of edu

hcfert %>% count(oci, ratio_cat2)
#Note, if we visualize the data by amount spent and fill with the "oci" dummy, we see that the ratio of housing costs doesn't
# seem to determine if a family is "overcrowded"
hcfert %>% count(share)
hcfert %>% 
  group_by(wave, share) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  mutate(share = fct_relevel(share, c("mb", "equal","fb"))) %>% 
  ggplot(aes(x = wave, y = percent, fill = share)) + 
  geom_bar(stat = "identity", color = "black",  size = 1) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 12), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12, hjust = 0.05, vjust = 0), strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  ggtitle("Share of female income contributions to the household") +
  xlab("Survey Wave") +
  ylab("Percent")
#There is a slight shift at wave 18 (start of the UKHLS) which seems to revert back to the previously observed pattern
ggsave("share_wave_histogram_s7_10-10-2022.png", dpi = 300)


#testing the oci variable
hcfert %>% 
  group_by(wave, oci) %>%
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)) %>% 
  ggplot(aes(x = wave, y = percent, fill = oci)) + 
  geom_bar(stat = "identity", color = "black",  size = 1) +
  theme_bw()+
  theme(legend.position = "bottom", legend.background = element_blank(),legend.box.background = element_rect(colour = "black"),
        axis.text = element_text(size = 12), legend.title = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 12, hjust = 0.05, vjust = 0), strip.text.x = element_text(size = 12)) +
  theme(aspect.ratio = 1) +
  ggtitle("Overcrowding by wave") +
  xlab("Survey Wave") +
  ylab("Percent")
#There is a slight shift at wave 18 (start of the UKHLS) which is likely due to the new sample a slight attrition of house moves in the BHPS
ggsave("oci_wave_histogram_s7_10-10-2022.png", dpi = 300)



###########################################################################
# creating a household design ---------------------------------------------
###########################################################################

# Our initial model testing (script 6) showed that there is no difference in the relationship
# between the explanatory variables and the likelihood of each birth event. We conclude that
# the proper level of analysis is the household rather than the individual.

# We see from our regression women have a higher likelihood of having a child then men. This is largely due
# to the faster transitions. We need to pick a clock for our model so our household will be based around the woman
# with partner variables for the (male) partner added. However, we will actually be exploring the transition of women
# into different births.

# Step 1: Seperate the women
hh <- hcfert %>% 
  filter(sex == "Women")

# Step 2: extract partner's details for future use
partner <- 
  bind_rows(bhps5, ukhls5) %>%
  mutate(age_dv = ifelse(is.na(age_dv), year - birthy, age_dv)) %>% 
  dplyr::select(pidp, wave, hidp, sex, edu, emp, partner, age_dv) %>% 
  rename("partedu" = "edu") %>% 
  rename("partemp" = "emp") %>% 
  rename("partage" = "age_dv") %>% 
  # rename("partclock" = "clock") %>% 
  filter(partner != "single", sex == 1) %>% 
  dplyr::select(-partner, -sex, -pidp)

# Step 3: combine the df
hhpart <- hh %>% 
  left_join(., partner, by = c("hidp", "wave")) %>% 
  mutate(pidp = as.numeric(pidp),
         oci2 = as.character(oci2))

saveRDS(hhpart, file = "hhpart.rds")


# Step 4: how many waves are NA for partner for non-single women?
natest <- hhpart %>% 
  filter(partner != "single", is.na(partage))
#There are 9,588 (7.4%) observations where the woman is NOT single but the partner's information is missing
### This is likely be due to the fact that some partner's were censored out, maybe try to get partner info from larger df
summary(natest$age) #This shows that the median age of missing partner details is 38 (likely censored men)
### Update, using the earlier dfs bhps5 & ukhls5 reduced the NA to 9,587 partners (41.1% or 7.4% of all observations)



