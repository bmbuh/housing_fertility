#Coded by: Brian Buh
#Started on: 02.06.2022
#Last Updated: 13.06.2021

library(tidyverse)
library(haven)
library(lubridate)
library(cowplot)

###########################################################################
# Extract the BHPS fertility histories ------------------------------------
###########################################################################

# Fertility Histories for the BHPS are provided by the UK Data Service 
## "Study Number 5629 - British Household Panel Survey Consolidated Marital, Cohabitation and Fertility Histories, 1991-2009"

#Step 1
fert_cut <- fert_his %>% 
  select(pp, hh1, sex, totalchild, parentbirth_m, parentbirth_yr, child1m, child1yr, child2m, child2yr, child3m, child3yr, last_intvyr) %>% 
  rename("pid" = "pp") %>% 
  mutate(agelastint = last_intvyr - parentbirth_yr) %>% 
  mutate(age91 = 1991 - parentbirth_yr) %>% 
  mutate(agefb = child1yr - parentbirth_yr) %>% #Age at first birth
  mutate(agesb = child2yr - parentbirth_yr) %>% #Age at second birth
  mutate(agetb = child3yr - parentbirth_yr) #Age at third birth


#Step 2 - Add xwave data and transform to long format
fert_transform <- bhpsxwave %>% 
  select(pid, pidp, birthy, hhorig, fwintvd_dv_bh, lwintvd_dv_bh, ba_hid, dcsedw_dv_bh) %>% 
  left_join(. , fert_cut, by = "pid") %>% 
  select(-birthy) %>% 
  unite(dob, c(parentbirth_m, parentbirth_yr), sep = "-") %>% 
  mutate(dob = parse_date_time(dob, "my")) %>% 
  unite(child1dob, c(child1m, child1yr), sep = "-") %>% 
  mutate(child1dob = parse_date_time(child1dob, "my"))%>% 
  unite(child2dob, c(child2m, child2yr), sep = "-") %>% 
  mutate(child2dob = parse_date_time(child2dob, "my"))%>% 
  unite(child3dob, c(child3m, child3yr), sep = "-") %>% 
  mutate(child3dob = parse_date_time(child3dob, "my"))

##It appears the quickest way to transform from long to wide is just simple to make separate dataset for each birth and rebind them together

ft1 <- fert_transform %>% 
  select(-child2dob, -child3dob) %>% 
  filter(!is.na(child1dob)) %>% 
  mutate(bno = 1)

ft2 <- fert_transform %>% 
  select(-child1dob, -child3dob) %>% 
  filter(!is.na(child2dob)) %>% 
  mutate(bno = 2)

ft3 <- fert_transform %>% 
  select(-child1dob, -child2dob) %>% 
  filter(!is.na(child3dob)) %>% 
  mutate(bno = 3)

fertbhps <- 
  bind_rows(ft1, ft2) %>% 
  bind_rows(., ft3) %>% 
  arrange(pidp, bno) %>% 
  mutate(kdob = case_when(!is.na(child1dob) ~ child1dob,
                          !is.na(child2dob) ~ child2dob,
                          !is.na(child3dob) ~ child3dob))


saveRDS(fertbhps, "fertbhps.rds")


###########################################################################
# Extract the UKHLS fertility histories -----------------------------------
###########################################################################

# Fertility Histories in the UKHLS have two parts, births that occur before the first interview (retrospective) and births that occur during the observation period (prospecitve)
# The first code section combines the retrospective birth data
# The second code section extracts the prospective birth data

# -------------------------------------------------------------------------
# Retrospective fertility history -----------------------------------------
# -------------------------------------------------------------------------


#This was updates 09.02.2021 to reflect that people enter waves later
# a_sample <- a_indall %>% 
#   dplyr::select(pidp, pid, a_hhorig, a_sex, a_doby_dv, a_dobm_dv) %>% 
#   rename("mpidp" = "pidp") %>% 
#   rename("mdoby" = "a_doby_dv") %>% 
#   rename("mdobm" = "a_dobm_dv") %>% 
#   unite(mdob, c(mdobm, mdoby), sep = "-") %>% 
#   mutate(mdob = parse_date_time(mdob, "my"))

#Using xwave data to catch all possible respondents
x_sample <- xwave %>% 
  dplyr::select(pidp, pid, hhorig, sex, birthm, birthy, anychild_dv) %>% 
  rename("doby" = "birthy") %>% 
  rename("dobm" = "birthm") %>% 
  unite(dob, c(dobm, doby), sep = "-") %>% 
  mutate(dob = parse_date_time(dob, "my"))

#The DF a_parent comes from the dataset Stata code from Alita Nandi
parent_sample <- a_parent %>% 
  filter(a_mnpid >0) %>% 
  rename("kpidp" = "pidp") %>% 
  rename("mpidp" = "a_mnpid") %>%
  rename("kbirthm" = "a_birthm") %>% 
  rename("kbirthy" = "a_birthy")


res_child <- a_natchild %>% 
  dplyr::select(pidp, pid, a_lchlv, a_lchdoby, a_lchdobm, a_childno) %>%
  filter(a_lchlv == 2) %>% 
  rename("mpidp" = "pidp") %>% 
  rename("kbirthm" = "a_lchdobm") %>% 
  rename("kbirthy" = "a_lchdoby") %>% 
  mutate(kbirthm = ifelse(kbirthm < 0, NA, kbirthm)) %>% 
  mutate(kbirthy = ifelse(kbirthy < 0, NA, kbirthy)) %>% 
  dplyr::select(-a_lchlv)

combined_child <-
  bind_rows(res_child, parent_sample) %>% 
  dplyr::select(mpidp, kbirthy, kbirthm, a_childno) %>% 
  mutate(wave = 1) %>% 
  mutate(fpidp = NA)

# -------------------------------------------------------------------------
# Building prospective fertility histories --------------------------------
# -------------------------------------------------------------------------


b_newchild <- b_child %>% 
  filter(b_ynew == 1) %>% 
  dplyr::select(pidp, pid, b_ynew, b_birthy, b_birthm, b_mnpid, b_fnpid) %>% 
  rename("fpidp" = "b_fnpid") %>% 
  rename("ynew" =  "b_ynew") %>% 
  rename("kbirthy" =  "b_birthy") %>% 
  rename("kbirthm" =  "b_birthm") %>% 
  rename("mpidp" =  "b_mnpid") %>% 
  mutate(wave = 2)

c_newchild <- c_child %>% 
  filter(c_ynew == 1) %>% 
  dplyr::select(pidp, pid, c_ynew, c_birthy, c_birthm, c_mnpid, c_fnpid) %>% 
  rename("fpidp" = "c_fnpid") %>%  
  rename("ynew" =  "c_ynew") %>% 
  rename("kbirthy" =  "c_birthy") %>% 
  rename("kbirthm" =  "c_birthm") %>% 
  rename("mpidp" =  "c_mnpid") %>% 
  mutate(wave = 3)

d_newchild <- d_child %>% 
  filter(d_ynew == 1) %>% 
  dplyr::select(pidp, pid, d_ynew, d_birthy, d_birthm, d_mnpid, d_fnpid) %>% 
  rename("fpidp" = "d_fnpid") %>% 
  rename("ynew" =  "d_ynew") %>% 
  rename("kbirthy" =  "d_birthy") %>% 
  rename("kbirthm" =  "d_birthm") %>% 
  rename("mpidp" =  "d_mnpid") %>% 
  mutate(wave = 4)

e_newchild <- e_child %>% 
  filter(e_ynew == 1) %>% 
  dplyr::select(pidp, pid, e_ynew, e_birthy, e_birthm, e_mnpid, e_fnpid) %>% 
  rename("fpidp" = "e_fnpid") %>%  
  rename("ynew" =  "e_ynew") %>% 
  rename("kbirthy" =  "e_birthy") %>% 
  rename("kbirthm" =  "e_birthm") %>% 
  rename("mpidp" =  "e_mnpid") %>% 
  mutate(wave = 5)

f_newchild <- f_child %>% 
  filter(f_ynew == 1) %>% 
  dplyr::select(pidp, pid, f_ynew, f_birthy, f_birthm, f_mnpid, f_fnpid) %>% 
  rename("fpidp" = "f_fnpid") %>%  
  rename("ynew" =  "f_ynew") %>% 
  rename("kbirthy" =  "f_birthy") %>% 
  rename("kbirthm" =  "f_birthm") %>% 
  rename("mpidp" =  "f_mnpid") %>% 
  mutate(wave = 6)

g_newchild <- g_child %>% 
  filter(g_ynew == 1) %>% 
  dplyr::select(pidp, pid, g_ynew, g_birthy, g_birthm, g_mnpid, g_fnpid) %>% 
  rename("fpidp" = "g_fnpid") %>%  
  rename("ynew" =  "g_ynew") %>% 
  rename("kbirthy" =  "g_birthy") %>% 
  rename("kbirthm" =  "g_birthm") %>% 
  rename("mpidp" =  "g_mnpid") %>% 
  mutate(wave = 7)

h_newchild <- h_child %>% 
  filter(h_ynew == 1) %>% 
  dplyr::select(pidp, pid, h_ynew, h_birthy, h_birthm, h_mnpid, h_fnpid) %>% 
  rename("fpidp" = "h_fnpid") %>% 
  rename("ynew" =  "h_ynew") %>% 
  rename("kbirthy" =  "h_birthy") %>% 
  rename("kbirthm" =  "h_birthm") %>% 
  rename("mpidp" =  "h_mnpid") %>% 
  mutate(wave = 8)

i_newchild <- i_child %>% 
  filter(i_ynew == 1) %>% 
  dplyr::select(pidp, pid, i_ynew, i_birthy, i_birthm, i_mnpid, i_fnpid) %>% 
  rename("fpidp" = "i_fnpid") %>% 
  rename("ynew" =  "i_ynew") %>% 
  rename("kbirthy" =  "i_birthy") %>% 
  rename("kbirthm" =  "i_birthm") %>% 
  rename("mpidp" =  "i_mnpid") %>% 
  mutate(wave = 9)

j_newchild <- j_child %>% 
  filter(j_ynew == 1) %>% 
  dplyr::select(pidp, pid, j_ynew, j_birthy, j_birthm, j_mnpid, j_fnpid) %>% 
  rename("fpidp" = "j_fnpid") %>% 
  rename("ynew" =  "j_ynew") %>% 
  rename("kbirthy" =  "j_birthy") %>% 
  rename("kbirthm" =  "j_birthm") %>% 
  rename("mpidp" =  "j_mnpid") %>% 
  mutate(wave = 10)

k_newchild <- k_child %>% 
  filter(k_ynew == 1) %>%  
  dplyr::select(pidp, pid, k_ynew, k_birthy, k_birthm, k_mnpid, k_fnpid) %>% 
  rename("fpidp" = "k_fnpid") %>% 
  rename("ynew" =  "k_ynew") %>% 
  rename("kbirthy" =  "k_birthy") %>% 
  rename("kbirthm" =  "k_birthm") %>% 
  rename("mpidp" =  "k_mnpid") %>% 
  mutate(wave = 11)

child <- 
  bind_rows(b_newchild, c_newchild) %>% 
  bind_rows(., d_newchild) %>% 
  bind_rows(., e_newchild) %>% 
  bind_rows(., f_newchild) %>% 
  bind_rows(., g_newchild) %>% 
  bind_rows(., h_newchild) %>% 
  bind_rows(., i_newchild) %>% 
  bind_rows(., j_newchild) %>% 
  bind_rows(., k_newchild) %>% 
  dplyr::select(-ynew, -pidp)

# -------------------------------------------------------------------------
# Combined fertility histories --------------------------------------------
# -------------------------------------------------------------------------

combined_child2 <- 
  bind_rows(combined_child, child) %>% 
  rename("pidp" = "mpidp") %>% 
  arrange(pidp, kbirthy)

fathers_child <- combined_child2 %>% 
  filter(fpidp > 0) %>% 
  dplyr::select(2:7) %>% 
  rename("pidp" = "fpidp")

combined_child3 <- 
  bind_rows(combined_child2, fathers_child)

combined_child4 <- 
  left_join(combined_child3, x_sample, by = "pidp") %>% 
  unite(kdob, c(kbirthm, kbirthy), sep = "-") %>% 
  mutate(kdob = parse_date_time(kdob, "my")) %>% 
  arrange(pidp, kdob) %>% 
  group_by(pidp) %>% 
  mutate(bno = row_number()) %>% 
  mutate(check = a_childno - bno) %>% 
  ungroup()

saveRDS(combined_child4, file = "fertukhls.rds")
fertukhls <- file.choose()
fertukhls <- readRDS(fertukhls)


###########################################################################
# Combine BHPS & UKHLS Fertility ------------------------------------------
###########################################################################

# I need a common set of variables for both BHPS & UKHLS:
## pidp, pid, wave, hhorig, sex, dob, kdob, bno
### Note: Because of the nature of abstracting fertility histories, BHPS does not have a set wave variable


# Step 1: Remove unneeded variables and rename to standardize

## - BHPS
### - Interviews start in September of each year in the BHPS, lasting about 5 months
#### - Births from Sept. to Jan will be set to the corresponding yearly wave, Feb- Aug the next wave
fertbhps2 <- fertbhps %>%
  select(pidp, pid, hhorig, sex, dob, kdob, bno) %>%
  mutate(year = year(ymd(kdob))) %>% #year is extracted from "kdob" to find which year the interview occurred in
  rename("dummywave" = "year") %>%
  mutate(dummywave = dummywave- 1990,
         dummywave = ifelse(dummywave <= 0, 1, dummywave)) %>% #The wave now corresponds with the calendar year in which the interview occurred
  mutate(month = month(ymd(kdob)),
         monthadj = ifelse(month == 1, NA, ifelse(month <= 8, 1, 0)),
         monthadj = ifelse(is.na(monthadj), 0, monthadj), #This allows for births from Feb - Aug to be discounted as the previous wave
         wave = dummywave - monthadj,
         wave = ifelse(wave == 0, 1, wave)) %>% #All births before the first interview are counted as wave 1
  select(pidp, pid, wave, hhorig, sex, dob, kdob, bno)

fertbhps2 %>% count(dummywave)

## - UKHLS
fertukhls2 <- fertukhls %>%
  select(pidp, pid, wave, hhorig, sex, dob, kdob, bno) %>%
  rename("ukhlswave" = "wave") %>%
  mutate(wave = ukhlswave + 18) %>%
  relocate(wave, .after = "pid")

## - Combined
allfert <- 
  bind_rows(fertukhls2, fertbhps2) %>%
  filter(!is.na(hhorig)) %>%
  arrange(pidp, wave)
### - Note: this df does not have "wave" variable for BHPS respondents


saveRDS(allfert, file = "allfert.rds")
allfert <- file.choose()
allfert <- readRDS(allfert)








###############################################################################


#Step 3
compfert <-  fertbhps %>%
  filter(!is.na(totalchild)) %>% #Cut1
  # filter(sex == 2) %>% #Cut2
  filter(age91 <= 40 & age91 >= 16) %>% #Cut3 
  filter(agelastint >=40) %>% #Cut4
  # filter(fwintvd_dv_bh == 1) %>%
  filter(child2yr >= 1991 | is.na(child2yr)) %>% 
  filter(hhorig_bh == 1)
# %>%
  # filter(lwintvd_dv_bh >= 10)

#Step 4
agefb <- compfert %>% count(agefb)
age91 <- compfert %>% count(age91)
compfert %>% count(totalchild, sex)

compfert %>% count(lwintvd_dv_bh)

#Step 5 Histogram of Distribution of Completed Fertility
compfert %>%
  mutate(sex = as.character(sex)) %>% 
  mutate(totalchild2 = as.character(totalchild)) %>% 
  mutate(sex = recode(sex,
                         "1" = "Men",
                         "2" = "Women")) %>% 
  ggplot(aes(x = totalchild, color = sex)) +
  geom_histogram(binwidth = 1, fill = "white", alpha = 0.5, position  = "identity") +
  facet_wrap(~sex) +
  # annotate("text", x=.15, y=5000, size = 6, label= "First Births are concentrated here") +
  # annotate("text", x=.25, y=2000, size = 6, label= "0 = No Jobless Spells") +
  # annotate("text", x=.8, y=1200, size = 6, label= "1 = Completely Jobless") +
  scale_color_manual(values = c("#5a189a", "#2a9d8f")) +
  theme_minimal()+
  theme(legend.position = c(.9,.8), plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15, vjust=-1), axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(color = "Sex") +
  ggtitle("Distribution of Completed Fertility", subtitle =  "BHPS") +
  xlab("Number of Births, Completed Fertility") +
  ylab("Count") +
  ggsave("paper2_compfert_hist_03-01-22.png", dpi = 300)




# Plot 2 - Age of different births ----------------------------------------

# Step 1 - Use DF fertbhps created in Steps 1 & 2 in Plot 1
# Step 2 - Remove births that happened before first interview
## Indicator created here, removal done in steps 4 & 5
# Step 3 - Look at cross-tab of ages
## There are 68 observations in which the child's birth year is registered at "1899"
### This is also true for some 2nd and 3rd births, but not all
# Step 4 - Remove birthdate that have been recorded as 1899
## Dates with 1899 are transformed to NA
### Warning, this data set works for the desired plot, but would having important missing births is used for completed fertility
# Step 5 - Transform the dates to a long format in order to use facetwrap
# Step 6 - Make plot


#Step 2
fertbhps2 <- fertbhps %>% 
  mutate(testfb = ifelse(child1yr < 1991, 0, 1)) %>% 
  mutate(testsb = ifelse(child2yr < 1991, 0, 1)) %>% 
  mutate(testtb = ifelse(child3yr < 1991, 0, 1))

#Step 3
child1yr <- fertbhps2 %>% count(child1yr)
agefb <- fertbhps2 %>% count(agefb, sex)
#???? Why are there negative numbers
agefbtest <- fertbhps2 %>% filter(agefb < 2)
#Clearly some births are recorded as the year 1899
agefbtest2 <- fertbhps2 %>% filter(agefb >= 14 & agefb <= 20) %>% count(child1yr, parentbirth_yr)
#The second test shows that is is just these 1899 births that are a problem
agesb <- fertbhps2 %>% count(agesb, sex) #Age second birth
agetb <- fertbhps2 %>% count(agetb, sex) #Age third birth
#Similar issue with birthdates not recorded correctly

#Step 4 & 5
fertbhps3 <- fertbhps2 %>% 
  filter(!is.na(sex)) %>% #NA in sex are also NA in fertility history
  mutate(agefb = ifelse(agefb < 2, NA, agefb)) %>% 
  mutate(agesb = ifelse(agesb < 2, NA, agesb)) %>% 
  mutate(agetb = ifelse(agetb < 2, NA, agetb)) %>% 
  mutate(agefb = ifelse(testfb == 0, NA, agefb)) %>% #Removes FB ages before first interview
  mutate(agesb = ifelse(testsb == 0, NA, agesb)) %>% #Removes SB ages before first interview
  mutate(agetb = ifelse(testtb == 0, NA, agetb)) %>% #Removes TB ages before first interview
  select(pid, sex, agefb, agesb, agetb) %>% 
  pivot_longer(cols = c('agefb', 'agesb', 'agetb'), names_to = "birth", values_to = "age")

agedist <- fertbhps3 %>% count(age)

#Step 6
birthlabs <- c("1st Birth", "2nd Birth", "3rd Birth")
names(birthlabs) <- c("agefb", "agesb", "agetb")

birthage<- fertbhps3 %>% 
  mutate(sex = as.character(sex)) %>% 
  mutate(sex = recode(sex,
                      "1" = "Men",
                      "2" = "Women")) %>% 
  ggplot(aes(x = sex, y = age, color = sex)) +
  geom_violin() +
  coord_flip() +
  stat_summary(fun.data = "mean_sdl", mult=1,
               geom = "pointrange") +
  geom_hline(yintercept = 30, linetype = "dotted", size = 1) +
  facet_wrap(~birth, labeller = labeller(birth = birthlabs)) +
  # scale_color_manual(values = c("#5a189a", "#2a9d8f")) +
  theme_minimal()+
  theme(legend.position = "none",
        # legend.position = c(.85,.88), 
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15, vjust=-1), axis.title.y = element_text(size = 15), 
        # legend.key.size = unit(1, 'cm'),
        # legend.title = element_text(size = 15),
        # legend.text = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15)) +
  theme(aspect.ratio = 1) +
  labs(color = "Sex") +
  ggtitle("Age at Birth Parity") +
  xlab("") +
  ylab("Age") +
  ggsave("paper2_birthage_violin_05-01-22.png", dpi = 300)







