#Coded by: Brian Buh
#Started on: 29.08.2022
#Last Updated: 30.08.2022

library(tidyverse)
library(haven)
library(lubridate)

#This script uses the LAD identification from SN6666 plus the years lower quartile housing price

###########################################################################
# Load SN6666 files and combine -------------------------------------------
###########################################################################


# -------------------------------------------------------------------------
# BHPS --------------------------------------------------------------------
# -------------------------------------------------------------------------

ba_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/ba_oslaua_protect.dta")
bb_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bb_oslaua_protect.dta")
bc_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bc_oslaua_protect.dta")
bd_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bd_oslaua_protect.dta")
be_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/be_oslaua_protect.dta")
bf_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bf_oslaua_protect.dta")
bg_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bg_oslaua_protect.dta")
bh_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bh_oslaua_protect.dta")
bi_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bi_oslaua_protect.dta")
bj_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bj_oslaua_protect.dta")
bk_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bk_oslaua_protect.dta")
bl_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bl_oslaua_protect.dta")
bm_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bm_oslaua_protect.dta")
bn_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bn_oslaua_protect.dta")
bo_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bo_oslaua_protect.dta")
bp_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bp_oslaua_protect.dta")
bq_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/bq_oslaua_protect.dta")
br_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/bhps/br_oslaua_protect.dta")

#removing wave specific variable name
ba_lad <- ba_lad %>% select(-ba_hid) %>% rename("hidp" = "ba_hidp") %>% rename("oslaua" = "ba_oslaua") %>% mutate(wave = 1) 
bb_lad <- bb_lad %>% select(-bb_hid) %>% rename("hidp" = "bb_hidp") %>% rename("oslaua" = "bb_oslaua") %>% mutate(wave = 2)
bc_lad <- bc_lad %>% select(-bc_hid) %>% rename("hidp" = "bc_hidp") %>% rename("oslaua" = "bc_oslaua") %>% mutate(wave = 3)
bd_lad <- bd_lad %>% select(-bd_hid) %>% rename("hidp" = "bd_hidp") %>% rename("oslaua" = "bd_oslaua") %>% mutate(wave = 4)
be_lad <- be_lad %>% select(-be_hid) %>% rename("hidp" = "be_hidp") %>% rename("oslaua" = "be_oslaua") %>% mutate(wave = 5)
bf_lad <- bf_lad %>% select(-bf_hid) %>% rename("hidp" = "bf_hidp") %>% rename("oslaua" = "bf_oslaua") %>% mutate(wave = 6)
bg_lad <- bg_lad %>% select(-bg_hid) %>% rename("hidp" = "bg_hidp") %>% rename("oslaua" = "bg_oslaua") %>% mutate(wave = 7)
bh_lad <- bh_lad %>% select(-bh_hid) %>% rename("hidp" = "bh_hidp") %>% rename("oslaua" = "bh_oslaua") %>% mutate(wave = 8)
bi_lad <- bi_lad %>% select(-bi_hid) %>% rename("hidp" = "bi_hidp") %>% rename("oslaua" = "bi_oslaua") %>% mutate(wave = 9)
bj_lad <- bj_lad %>% select(-bj_hid) %>% rename("hidp" = "bj_hidp") %>% rename("oslaua" = "bj_oslaua") %>% mutate(wave = 10)
bk_lad <- bk_lad %>% select(-bk_hid) %>% rename("hidp" = "bk_hidp") %>% rename("oslaua" = "bk_oslaua") %>% mutate(wave = 11)
bl_lad <- bl_lad %>% select(-bl_hid) %>% rename("hidp" = "bl_hidp") %>% rename("oslaua" = "bl_oslaua") %>% mutate(wave = 12)
bm_lad <- bm_lad %>% select(-bm_hid) %>% rename("hidp" = "bm_hidp") %>% rename("oslaua" = "bm_oslaua") %>% mutate(wave = 13)
bn_lad <- bn_lad %>% select(-bn_hid) %>% rename("hidp" = "bn_hidp") %>% rename("oslaua" = "bn_oslaua") %>% mutate(wave = 14)
bo_lad <- bo_lad %>% select(-bo_hid) %>% rename("hidp" = "bo_hidp") %>% rename("oslaua" = "bo_oslaua") %>% mutate(wave = 15)
bp_lad <- bp_lad %>% select(-bp_hid) %>% rename("hidp" = "bp_hidp") %>% rename("oslaua" = "bp_oslaua") %>% mutate(wave = 16)
bq_lad <- bq_lad %>% select(-bq_hid) %>% rename("hidp" = "bq_hidp") %>% rename("oslaua" = "bq_oslaua") %>% mutate(wave = 17)
br_lad <- br_lad %>% select(-br_hid) %>% rename("hidp" = "br_hidp") %>% rename("oslaua" = "br_oslaua") %>% mutate(wave = 18)

#bind each wave specific dataframe together
lad_bhps <-
  bind_rows(ba_lad, bb_lad) %>%
  bind_rows(., bc_lad) %>%
  bind_rows(., bd_lad) %>%
  bind_rows(., be_lad) %>%
  bind_rows(., bf_lad) %>%
  bind_rows(., bg_lad) %>%
  bind_rows(., bh_lad) %>%
  bind_rows(., bi_lad) %>%
  bind_rows(., bj_lad) %>% 
  bind_rows(., bk_lad) %>%
  bind_rows(., bl_lad) %>%
  bind_rows(., bm_lad) %>%
  bind_rows(., bn_lad) %>%
  bind_rows(., bo_lad) %>%
  bind_rows(., bp_lad) %>%
  bind_rows(., bq_lad) %>%
  bind_rows(., br_lad) %>% 
  arrange(hidp, wave)




# -------------------------------------------------------------------------
# UKHLS -------------------------------------------------------------------
# -------------------------------------------------------------------------

a_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/a_oslaua_protect.dta")
b_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/b_oslaua_protect.dta")
c_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/c_oslaua_protect.dta")
d_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/d_oslaua_protect.dta")
e_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/e_oslaua_protect.dta")
f_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/f_oslaua_protect.dta")
g_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/g_oslaua_protect.dta")
h_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/h_oslaua_protect.dta")
i_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/i_oslaua_protect.dta")
j_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/j_oslaua_protect.dta")
k_lad <- read_dta("S:/questionnaires/UKHLS/UKDA-6666-stata/stata/stata13/ukhls/k_oslaua_protect.dta")

a_lad <- a_lad %>% rename("hidp" = "a_hidp") %>% rename("oslaua" = "a_oslaua") %>% mutate(wave = 19)
b_lad <- b_lad %>% rename("hidp" = "b_hidp") %>% rename("oslaua" = "b_oslaua") %>% mutate(wave = 20)
c_lad <- c_lad %>% rename("hidp" = "c_hidp") %>% rename("oslaua" = "c_oslaua") %>% mutate(wave = 21)
d_lad <- d_lad %>% rename("hidp" = "d_hidp") %>% rename("oslaua" = "d_oslaua") %>% mutate(wave = 22)
e_lad <- e_lad %>% rename("hidp" = "e_hidp") %>% rename("oslaua" = "e_oslaua") %>% mutate(wave = 23)
f_lad <- f_lad %>% rename("hidp" = "f_hidp") %>% rename("oslaua" = "f_oslaua") %>% mutate(wave = 24)
g_lad <- g_lad %>% rename("hidp" = "g_hidp") %>% rename("oslaua" = "g_oslaua") %>% mutate(wave = 25)
h_lad <- h_lad %>% rename("hidp" = "h_hidp") %>% rename("oslaua" = "h_oslaua") %>% mutate(wave = 26)
i_lad <- i_lad %>% rename("hidp" = "i_hidp") %>% rename("oslaua" = "i_oslaua") %>% mutate(wave = 27)
j_lad <- j_lad %>% rename("hidp" = "j_hidp") %>% rename("oslaua" = "j_oslaua") %>% mutate(wave = 28)
k_lad <- k_lad %>% rename("hidp" = "k_hidp") %>% rename("oslaua" = "k_oslaua") %>% mutate(wave = 29)


lad_ukhls <-
  bind_rows(a_lad, b_lad) %>%
  bind_rows(., c_lad) %>%
  bind_rows(., d_lad) %>%
  bind_rows(., e_lad) %>%
  bind_rows(., f_lad) %>%
  bind_rows(., g_lad) %>%
  bind_rows(., h_lad) %>%
  bind_rows(., i_lad) %>%
  bind_rows(., j_lad) %>% 
  bind_rows(., k_lad) %>% 
  arrange(hidp, wave)


#Combines BHPS and UKHLS
lad <- 
  bind_rows(lad_bhps, lad_ukhls) %>% 
  mutate(year = wave + 1990) %>% 
  relocate("oslaua", .after = "year") %>%
  rename("code" = "oslaua")

saveRDS(lad, file = "lad.rds")
 
###########################################################################
# lower quartile house price data and combine ----------------------------
###########################################################################

#England and Wales
engwal_lad <- read_csv("S:/papers/paper_2_housing_costs/lad_housing_prices/engwal_lowerquartile_edit.csv")

engwal_lad_long <- engwal_lad %>% 
  pivot_longer(cols = c("end1995", "end1996", "end1997", "end1998", "end1999", "end2000", "end2001", "end2002", "end2003", "end2004",
               "end2005", "end2006", "end2007", "end2008", "end2009", "end2010", "end2011", "end2012", "end2013", "end2014",
               "end2015", "end2016", "end2017", "end2018", "end2019", "end2020", "end2021"), names_to = "year", values_to = "lowquar") %>%
  separate(year, into = c("end", "year"), sep = 3, convert = TRUE) %>%
  dplyr::select(-end)
  # rename("code" = "Local.authority.code") %>%
  # rename("name" = "Local.authority.name")

#Scotland
scot_lad <- read_csv("S:/papers/paper_2_housing_costs/lad_housing_prices/scot_lowerquartile_edit.csv")

scot_lad_long <- scot_lad %>% 
  pivot_longer(cols = c("end1993", "end1994", "end1995", "end1996", "end1997", "end1998", "end1999", "end2000", "end2001", "end2002", "end2003",
                        "end2004", "end2005", "end2006", "end2007", "end2008", "end2009", "end2010", "end2011", "end2012", "end2013", "end2014",
                        "end2015", "end2016", "end2017", "end2018"), names_to = "year", values_to = "lowquar") %>%
  separate(year, into = c("end", "year"), sep = 3, convert = TRUE) %>%
  dplyr::select(-end)

#Northern Ireland
ni_lad <- read_csv("S:/papers/paper_2_housing_costs/lad_housing_prices/ni_lowerquartile_edit.csv")


#Join into 1 file
lad_values <- left_join(lad, engwal_lad_long, by = c("code", "year")) %>%
  left_join(., scot_lad_long, by = c("code", "year")) %>%
  mutate(lowquar.x = ifelse(is.na(lowquar.x), lowquar.y, lowquar.x)) %>% #this is necessary because of the double names when joining (Scotland)
  mutate(name.x = ifelse(is.na(name.x), name.y, name.x)) %>%
  dplyr::select(-name.y, -lowquar.y) %>%
  rename("name" = "name.x") %>%
  rename("lowquar" = "lowquar.x") %>%
  left_join(., ni_lad, by = c("code", "year")) %>%
  mutate(lowquar.x = ifelse(is.na(lowquar.x), lowquar.y, lowquar.x)) %>% #this is necessary because of the double names when joining (NI)
  mutate(name.x = ifelse(is.na(name.x), name.y, name.x)) %>%
  dplyr::select(-name.y, -lowquar.y) %>%
  rename("name" = "name.x") %>%
  rename("lowquar" = "lowquar.x")

summary(lad_values$lowquar)

saveRDS(lad_values, file = "lad_values.rds")

#Count the number of missing LAD Years
lad_values %>% count(is.na(lowquar))















