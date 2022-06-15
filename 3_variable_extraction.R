#Coded by: Brian Buh
#Started on: 13.06.2022
#Last Updated:

library(tidyverse)
library(haven)
library(lubridate)

###########################################################################
# BHPS --------------------------------------------------------------------
###########################################################################

#All hhresp and indresp files waves 1-18 plus xwave from the BSPS must be loaded

# -------------------------------------------------------------------------
# Household datasets ------------------------------------------------------
# -------------------------------------------------------------------------

#Selecting variables and give them wave specific names
hhvar <- c("hid", "hidp", "ivfho_bh", # Not in Wave 1
           "fihhmngrs_dv", "fihhmb", "hhneti", "hhyneti", "hhnetde", "hhnetde2", "bhcinda", "loctax", #These are the income variables
           "hcost", "xphsg", "xphsn", "xpmg", "rent", "rentg_bh", "tenure_dv", "hsownd_bh", "hsroom", "hstype") #These are housing cost variables

#The variable "ivfho_bh" are not in Wave 1
hhvar1 <- c("hid", "hidp",
           "fihhmngrs_dv", "fihhmb", "hhneti", "hhyneti", "hhnetde", "hhnetde2", "bhcinda", "loctax", #These are the income variables
           "hcost", "xphsg", "xphsn", "xpmg", "rent", "rentg_bh", "tenure_dv", "hsownd_bh", "hsroom", "hstype") #These are housing cost variables

w1hhvar <- paste0('ba_', hhvar1)

w2hhvar <- paste0('bb_', hhvar)

w3hhvar <- paste0('bc_', hhvar)

w4hhvar <- paste0('bd_', hhvar)

w5hhvar <- paste0('be_', hhvar)

w6hhvar <- paste0('bf_', hhvar)

w7hhvar <- paste0('bg_', hhvar)

w8hhvar <- paste0('bh_', hhvar)

w9hhvar <- paste0('bi_', hhvar)

w10hhvar <- paste0('bj_', hhvar)

w11hhvar <- paste0('bk_', hhvar)

w12hhvar <- paste0('bl_', hhvar)

w13hhvar <- paste0('bm_', hhvar)

w14hhvar <- paste0('bn_', hhvar)

w15hhvar <- paste0('bo_', hhvar)

w16hhvar <- paste0('bp_', hhvar)

w17hhvar <- paste0('bq_', hhvar)

w18hhvar <- paste0('br_', hhvar)

#Preparing the variables for merging; extracting from each wave
ba_hh <- ba_hhresp %>% 
  dplyr::select(w1hhvar) %>% 
  rename_with(~ hhvar[which(w1hhvar == .x)], .cols = w1hhvar) %>% 
  mutate(wave = 1) %>% 
  mutate(ivfho_bh = NA)

bb_hh <- bb_hhresp %>% 
  dplyr::select(w2hhvar) %>% 
  rename_with(~ hhvar[which(w2hhvar == .x)], .cols = w2hhvar) %>% 
  mutate(wave = 2)

bc_hh <- bc_hhresp %>% 
  dplyr::select(w3hhvar) %>% 
  rename_with(~ hhvar[which(w3hhvar == .x)], .cols = w3hhvar) %>% 
  mutate(wave = 3)

bd_hh <- bd_hhresp %>% 
  dplyr::select(w4hhvar) %>% 
  rename_with(~ hhvar[which(w4hhvar == .x)], .cols = w4hhvar) %>% 
  mutate(wave = 4)

be_hh <- be_hhresp %>% 
  dplyr::select(w5hhvar) %>% 
  rename_with(~ hhvar[which(w5hhvar == .x)], .cols = w5hhvar) %>% 
  mutate(wave = 5)

bf_hh <- bf_hhresp %>% 
  dplyr::select(w6hhvar) %>% 
  rename_with(~ hhvar[which(w6hhvar == .x)], .cols = w6hhvar) %>% 
  mutate(wave = 6)

bg_hh <- bg_hhresp %>% 
  dplyr::select(w7hhvar) %>% 
  rename_with(~ hhvar[which(w7hhvar == .x)], .cols = w7hhvar) %>% 
  mutate(wave = 7)

bh_hh <- bh_hhresp %>% 
  dplyr::select(w8hhvar) %>% 
  rename_with(~ hhvar[which(w8hhvar == .x)], .cols = w8hhvar) %>% 
  mutate(wave = 8)

bi_hh <- bi_hhresp %>% 
  dplyr::select(w9hhvar) %>% 
  rename_with(~ hhvar[which(w9hhvar == .x)], .cols = w9hhvar) %>% 
  mutate(wave = 9)

bj_hh <- bj_hhresp %>% 
  dplyr::select(w10hhvar) %>% 
  rename_with(~ hhvar[which(w10hhvar == .x)], .cols = w10hhvar) %>% 
  mutate(wave = 10)

bk_hh <- bk_hhresp %>% 
  dplyr::select(w11hhvar) %>% 
  rename_with(~ hhvar[which(w11hhvar == .x)], .cols = w11hhvar) %>% 
  mutate(wave = 11)

bl_hh <- bl_hhresp %>% 
  dplyr::select(w12hhvar) %>% 
  rename_with(~ hhvar[which(w12hhvar == .x)], .cols = w12hhvar) %>% 
  mutate(wave = 12)

bm_hh <- bm_hhresp %>% 
  dplyr::select(w13hhvar) %>% 
  rename_with(~ hhvar[which(w13hhvar == .x)], .cols = w13hhvar) %>% 
  mutate(wave = 13)

bn_hh <- bn_hhresp %>% 
  dplyr::select(w14hhvar) %>% 
  rename_with(~ hhvar[which(w14hhvar == .x)], .cols = w14hhvar) %>% 
  mutate(wave = 14)

bo_hh <- bo_hhresp %>% 
  dplyr::select(w15hhvar) %>% 
  rename_with(~ hhvar[which(w15hhvar == .x)], .cols = w15hhvar) %>% 
  mutate(wave = 15)

bp_hh <- bp_hhresp %>% 
  dplyr::select(w16hhvar) %>% 
  rename_with(~ hhvar[which(w16hhvar == .x)], .cols = w16hhvar) %>% 
  mutate(wave = 16)

bq_hh <- bq_hhresp %>% 
  dplyr::select(w17hhvar) %>% 
  rename_with(~ hhvar[which(w17hhvar == .x)], .cols = w17hhvar) %>% 
  mutate(wave = 17)

br_hh <- br_hhresp %>% 
  dplyr::select(w18hhvar) %>% 
  rename_with(~ hhvar[which(w18hhvar == .x)], .cols = w18hhvar) %>% 
  mutate(wave = 18)

#bind each wave specific dataframe together
hh_bhps <-
  bind_rows(ba_hh, bb_hh) %>%
  bind_rows(., bc_hh) %>%
  bind_rows(., bd_hh) %>%
  bind_rows(., be_hh) %>%
  bind_rows(., bf_hh) %>%
  bind_rows(., bg_hh) %>%
  bind_rows(., bh_hh) %>%
  bind_rows(., bi_hh) %>%
  bind_rows(., bj_hh) %>% 
  bind_rows(., bk_hh) %>%
  bind_rows(., bl_hh) %>%
  bind_rows(., bm_hh) %>%
  bind_rows(., bn_hh) %>%
  bind_rows(., bo_hh) %>%
  bind_rows(., bp_hh) %>%
  bind_rows(., bq_hh) %>%
  bind_rows(., br_hh) %>% 
  relocate("wave", .after = "hidp") %>%
  arrange(hidp, wave)

saveRDS(hh_bhps, file = "hh_bhps.rds")

# -------------------------------------------------------------------------
# Individual datasets -----------------------------------------------------
# -------------------------------------------------------------------------

ba_indresp %>% count(ba_age_dv)

#Selecting variables and give them wave specific names
indvar <- c("hid", "hidp", "hhorig", "sex", "birthm", "birthy", "age_dv", "istrtdatm", "istrtdaty", #basic individual variables
           "paynty", "paynti", #These are the income variables
           "xpchcf", "xpchc", "f135", #These are childcare cost variables
           "f139", #housing benefit
           "plbornc", "gor_dv", "mlstat", "jbstat", "qfedhi") #controls

#Some variables are not included in wave 1: "birthm", "birthy", "istrtdaty"
indvar1 <- c("hid", "hidp", "hhorig", "sex", "age_dv", "istrtdatm", #basic individual variables
            "paynty", "paynti", #These are the income variables
            "xpchcf", "xpchc", "f135", #These are childcare cost variables
            "f139", #housing benefit
            "plbornc", "gor_dv", "mlstat", "jbstat", "qfedhi") #controls

w1indvar <- paste0('ba_', indvar1)

w2indvar <- paste0('bb_', indvar)

w3indvar <- paste0('bc_', indvar)

w4indvar <- paste0('bd_', indvar)

w5indvar <- paste0('be_', indvar)

w6indvar <- paste0('bf_', indvar)

w7indvar <- paste0('bg_', indvar)

w8indvar <- paste0('bh_', indvar)

w9indvar <- paste0('bi_', indvar)

w10indvar <- paste0('bj_', indvar)

w11indvar <- paste0('bk_', indvar)

w12indvar <- paste0('bl_', indvar)

w13indvar <- paste0('bm_', indvar)

w14indvar <- paste0('bn_', indvar)

w15indvar <- paste0('bo_', indvar)

w16indvar <- paste0('bp_', indvar)

w17indvar <- paste0('bq_', indvar)

w18indvar <- paste0('br_', indvar)

#Preparing the variables for merging; extracting from each wave
ba_ind <- ba_indresp %>% 
  dplyr::select("pidp", "pid",  w1indvar) %>% 
  rename_with(~ indvar[which(w1indvar == .x)], .cols = w1indvar) %>% 
  mutate(wave = 1) %>% 
  mutate(birthm = NA) %>% 
  mutate(birthy = NA) %>% 
  mutate(istrtdaty = NA) %>% 
  mutate(age_dv = NA)


bb_ind <- bb_indresp %>% 
  dplyr::select("pidp", "pid",  w2indvar) %>% 
  rename_with(~ indvar[which(w2indvar == .x)], .cols = w2indvar) %>% 
  mutate(wave = 2)

bc_ind <- bc_indresp %>% 
  dplyr::select("pidp", "pid",  w3indvar) %>% 
  rename_with(~ indvar[which(w3indvar == .x)], .cols = w3indvar) %>% 
  mutate(wave = 3)

bd_ind <- bd_indresp %>% 
  dplyr::select("pidp", "pid",  w4indvar) %>% 
  rename_with(~ indvar[which(w4indvar == .x)], .cols = w4indvar) %>% 
  mutate(wave = 4)

be_ind <- be_indresp %>% 
  dplyr::select("pidp", "pid",  w5indvar) %>% 
  rename_with(~ indvar[which(w5indvar == .x)], .cols = w5indvar) %>% 
  mutate(wave = 5)

bf_ind <- bf_indresp %>% 
  dplyr::select("pidp", "pid",  w6indvar) %>% 
  rename_with(~ indvar[which(w6indvar == .x)], .cols = w6indvar) %>% 
  mutate(wave = 6)

bg_ind <- bg_indresp %>% 
  dplyr::select("pidp", "pid",  w7indvar) %>% 
  rename_with(~ indvar[which(w7indvar == .x)], .cols = w7indvar) %>% 
  mutate(wave = 7)

bh_ind <- bh_indresp %>% 
  dplyr::select("pidp", "pid",  w8indvar) %>% 
  rename_with(~ indvar[which(w8indvar == .x)], .cols = w8indvar) %>% 
  mutate(wave = 8)

bi_ind <- bi_indresp %>% 
  dplyr::select("pidp", "pid",  w9indvar) %>% 
  rename_with(~ indvar[which(w9indvar == .x)], .cols = w9indvar) %>% 
  mutate(wave = 9)

bj_ind <- bj_indresp %>% 
  dplyr::select("pidp", "pid",  w10indvar) %>% 
  rename_with(~ indvar[which(w10indvar == .x)], .cols = w10indvar) %>% 
  mutate(wave = 10)

bk_ind <- bk_indresp %>% 
  dplyr::select("pidp", "pid",  w11indvar) %>% 
  rename_with(~ indvar[which(w11indvar == .x)], .cols = w11indvar) %>% 
  mutate(wave = 11)

bl_ind <- bl_indresp %>% 
  dplyr::select("pidp", "pid",  w12indvar) %>% 
  rename_with(~ indvar[which(w12indvar == .x)], .cols = w12indvar) %>% 
  mutate(wave = 12)

bm_ind <- bm_indresp %>% 
  dplyr::select("pidp", "pid",  w13indvar) %>% 
  rename_with(~ indvar[which(w13indvar == .x)], .cols = w13indvar) %>% 
  mutate(wave = 13)

bn_ind <- bn_indresp %>% 
  dplyr::select("pidp", "pid",  w14indvar) %>% 
  rename_with(~ indvar[which(w14indvar == .x)], .cols = w14indvar) %>% 
  mutate(wave = 14)

bo_ind <- bo_indresp %>% 
  dplyr::select("pidp", "pid",  w15indvar) %>% 
  rename_with(~ indvar[which(w15indvar == .x)], .cols = w15indvar) %>% 
  mutate(wave = 15)

bp_ind <- bp_indresp %>% 
  dplyr::select("pidp", "pid",  w16indvar) %>% 
  rename_with(~ indvar[which(w16indvar == .x)], .cols = w16indvar) %>% 
  mutate(wave = 16)

bq_ind <- bq_indresp %>% 
  dplyr::select("pidp", "pid",  w17indvar) %>% 
  rename_with(~ indvar[which(w17indvar == .x)], .cols = w17indvar) %>% 
  mutate(wave = 17)

br_ind <- br_indresp %>% 
  dplyr::select("pidp", "pid",  w18indvar) %>% 
  rename_with(~ indvar[which(w18indvar == .x)], .cols = w18indvar) %>% 
  mutate(wave = 18)

#bind each wave specific dataframe together
ind_bhps <-
  bind_rows(ba_ind, bb_ind) %>%
  bind_rows(., bc_ind) %>%
  bind_rows(., bd_ind) %>%
  bind_rows(., be_ind) %>%
  bind_rows(., bf_ind) %>%
  bind_rows(., bg_ind) %>%
  bind_rows(., bh_ind) %>%
  bind_rows(., bi_ind) %>%
  bind_rows(., bj_ind) %>% 
  bind_rows(., bk_ind) %>%
  bind_rows(., bl_ind) %>%
  bind_rows(., bm_ind) %>%
  bind_rows(., bn_ind) %>%
  bind_rows(., bo_ind) %>%
  bind_rows(., bp_ind) %>%
  bind_rows(., bq_ind) %>%
  bind_rows(., br_ind) %>% 
  relocate("wave", .after = "pid") %>%
  ungroup() %>%  #Note: arrange doesn't work here since it appears that each "hid" is unique to each wave
  arrange(pidp, wave) %>% 
  group_by(pidp) %>% 
  fill(birthm, .direction = "up") %>% 
  fill(birthy, .direction = "up") %>% 
  ungroup() %>% 
  mutate(age_dv = ifelse(wave == 1 & birthm <=8, 0, ifelse(wave ==1 & birthm >= 9, 1, age_dv)),
         age_dv = ifelse(wave == 1, (age_dv + 1990) - birthy, age_dv))
  
saveRDS(ind_bhps, file = "ind_bhps.rds")


# -------------------------------------------------------------------------
# Combined dataset --------------------------------------------------------
# -------------------------------------------------------------------------

indhhbhps <- left_join(ind_bhps, hh_bhps, by= c("hidp", "wave")) %>% 
  arrange(pidp, wave) %>%
  select(-hid.x, -hid.y)
#The result is the same if you merge using "hid" or "hidp"

saveRDS(indhhbhps, file = "indhhbhps.rds")



###########################################################################
# Making an 18-wave data set from the 3909 data ---------------------------
###########################################################################
# 
# #The data is seperated by weekly or yearly household net income data by wave. 
# #It uses the prefix "w" for each wave for each variable. The first step is to chop off the prefix
# 
# ba_yi <- ba_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bb_yi <- bb_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bc_yi <- bc_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bd_yi <- bd_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# be_yi <- be_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bf_yi <- bf_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bg_yi <- bg_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bh_yi <- bh_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bi_yi <- bi_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bj_yi <- bj_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bk_yi <- bk_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bl_yi <- bl_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bm_yi <- bm_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bn_yi <- bn_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bo_yi <- bo_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bp_yi <- bp_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# bq_yi <- bq_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# br_yi <- br_yearinc %>%
#   rename_at(1:length(.), list(~ substr(., 2, nchar(.))))
# 
# year3909 <-
#   bind_rows(ba_yi, bb_yi) %>%
#   bind_rows(., bc_yi) %>%
#   bind_rows(., bd_yi) %>%
#   bind_rows(., be_yi) %>%
#   bind_rows(., bf_yi) %>%
#   bind_rows(., bg_yi) %>%
#   bind_rows(., bh_yi) %>%
#   bind_rows(., bi_yi) %>%
#   bind_rows(., bj_yi) %>% 
#   bind_rows(., bk_yi) %>%
#   bind_rows(., bl_yi) %>%
#   bind_rows(., bm_yi) %>%
#   bind_rows(., bn_yi) %>%
#   bind_rows(., bo_yi) %>%
#   bind_rows(., bp_yi) %>%
#   bind_rows(., bq_yi) %>%
#   bind_rows(., br_yi) %>% 
#   relocate("ave", .after = "hid") %>%
#   ungroup() %>%  #Note: arrange doesn't work here since it appears that each "hid" is unique to each wave
#   rename("wave" = "ave") %>% 
#   rename("basepi" = "asepi") %>% 
#   rename("bhcinda" = "hcinda") %>% 
#   rename("eq_moecd" = "q_moecd")
# 
# 
# saveRDS(year3909, "year3909.rds")



###########################################################################
# UKHLS -------------------------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# Household datasets ------------------------------------------------------
# -------------------------------------------------------------------------

#Sorting out needed variables from indresp
#Changes in these lists allow for much quick adding and subtracting variables
wave_varhh <- c("hidp", "fihhmnnet3_dv", "fihhmnnet4_dv", "houscost1_dv", "houscost2_dv", "xpmg", "rent", "rentg", "rentgrs_dv", "hsownd", "tenure_dv", "hsrooms", "hsbeds")

#Add the wave prefix to the variable list
w1_varhh <- paste0('a_', wave_varhh)

w2_varhh <- paste0('b_', wave_varhh)

w3_varhh <- paste0('c_', wave_varhh)

w4_varhh <- paste0('d_', wave_varhh)

w5_varhh <- paste0('e_', wave_varhh)

w6_varhh <- paste0('f_', wave_varhh)

w7_varhh <- paste0('g_', wave_varhh)

w8_varhh <- paste0('h_', wave_varhh)

w9_varhh <- paste0('i_', wave_varhh)

w10_varhh <- paste0('j_', wave_varhh)

w11_varhh <- paste0('k_', wave_varhh)

hhcol_order <- c("hidp", "wave", "fihhmnnet3_dv", "fihhmnnet4_dv", "houscost1_dv", "houscost2_dv", "xpmg", "rent", "rentg", "rentgrs_dv", "hsownd", "tenure_dv", "hsrooms", "hsbeds")

#Preparing the variables for merging
a_hh <- a_hhresp %>% 
  dplyr::select(w1_varhh) %>% 
  rename_with(~ wave_varhh[which(w1_varhh == .x)], .cols = w1_varhh) %>% 
  mutate(wave = 1)

b_hh <- b_hhresp %>% 
  dplyr::select(w2_varhh)%>% 
  rename_with(~ wave_varhh[which(w2_varhh == .x)], .cols = w2_varhh) %>% 
  mutate(wave = 2)

c_hh <- c_hhresp %>% 
  dplyr::select(w3_varhh) %>% 
  rename_with(~ wave_varhh[which(w3_varhh == .x)], .cols = w3_varhh) %>% 
  mutate(wave = 3)

d_hh <- d_hhresp %>% 
  dplyr::select(w4_varhh)%>% 
  rename_with(~ wave_varhh[which(w4_varhh == .x)], .cols = w4_varhh) %>% 
  mutate(wave = 4)

e_hh <- e_hhresp %>% 
  dplyr::select(w5_varhh) %>% 
  rename_with(~ wave_varhh[which(w5_varhh == .x)], .cols = w5_varhh) %>% 
  mutate(wave = 5)

f_hh <- f_hhresp %>% 
  dplyr::select(w6_varhh)%>% 
  rename_with(~ wave_varhh[which(w6_varhh == .x)], .cols = w6_varhh) %>% 
  mutate(wave = 6)

g_hh <- g_hhresp %>% 
  dplyr::select(w7_varhh) %>% 
  rename_with(~ wave_varhh[which(w7_varhh == .x)], .cols = w7_varhh) %>% 
  mutate(wave = 7)

h_hh <- h_hhresp %>% 
  dplyr::select(w8_varhh)%>% 
  rename_with(~ wave_varhh[which(w8_varhh == .x)], .cols = w8_varhh) %>% 
  mutate(wave = 8)

i_hh <- i_hhresp %>% 
  dplyr::select(w9_varhh) %>% 
  rename_with(~ wave_varhh[which(w9_varhh == .x)], .cols = w9_varhh) %>% 
  mutate(wave = 9)

j_hh <- j_hhresp %>% 
  dplyr::select(w10_varhh)%>% 
  rename_with(~ wave_varhh[which(w10_varhh == .x)], .cols = w10_varhh) %>% 
  mutate(wave = 10)

k_hh <- k_hhresp %>% 
  dplyr::select(w11_varhh)%>% 
  rename_with(~ wave_varhh[which(w11_varhh == .x)], .cols = w11_varhh) %>% 
  mutate(wave = 11)

hh_ukhls <-
  bind_rows(a_hh, b_hh) %>%
  bind_rows(., c_hh) %>%
  bind_rows(., d_hh) %>%
  bind_rows(., e_hh) %>%
  bind_rows(., f_hh) %>%
  bind_rows(., g_hh) %>%
  bind_rows(., h_hh) %>%
  bind_rows(., i_hh) %>%
  bind_rows(., j_hh) %>% 
  bind_rows(., k_hh) %>%
  relocate("wave", .after = "hidp") %>%
  arrange(hidp, wave)

saveRDS(hh_ukhls, file = "hh_ukhls.rds")


# -------------------------------------------------------------------------
# Individual datasets  ----------------------------------------------------
# -------------------------------------------------------------------------

### xwavedat
### For adding variables that are easier to use from the xwave dataset
ind_ukhls_xwave <- xwave %>%
  select(pidp, ukborn, plbornc)

#Sorting out needed variables from indresp
#Changes in these lists allow for much quick adding and subtracting variables
wave_var <- c("hhorig", "hidp", "sex", "birthm", "birthy", "istrtdatm", "istrtdaty", "age_dv", "qfhigh_dv", "hiqual_dv", "gor_dv", "marstat_dv", "jbstat", "jbisco88_cc", "pbnft8")

#Add the wave prefix to the variable list
w1_var <- paste0('a_', wave_var)

w2_var <- paste0('b_', wave_var)

w3_var <- paste0('c_', wave_var)

w4_var <- paste0('d_', wave_var)

w5_var <- paste0('e_', wave_var)

w6_var <- paste0('f_', wave_var)

w7_var <- paste0('g_', wave_var)

w8_var <- paste0('h_', wave_var)

w9_var <- paste0('i_', wave_var)

w10_var <- paste0('j_', wave_var)

w11_var <- paste0('k_', wave_var)

col_order <- c("pidp", "wave", "hhorig", "hidp", "sex", "birthm", "birthy", "istrtdatm", "istrtdaty", "age_dv", "qfhigh_dv", "hiqual_dv", "gor_dv", "marstat_dv", "mlstat", "jbstat", "jbisco88_cc", "pbnft8")


#Preparing the variables for merging
a_ind <- a_indresp %>% 
  dplyr::select("pidp", w1_var) %>% 
  rename_with(~ wave_var[which(w1_var == .x)], .cols = w1_var) %>% 
  mutate(wave = 1)

b_ind <- b_indresp %>% 
  dplyr::select("pidp", w2_var)%>% 
  rename_with(~ wave_var[which(w2_var == .x)], .cols = w2_var) %>% 
  mutate(wave = 2)

c_ind <- c_indresp %>% 
  dplyr::select("pidp", w3_var) %>% 
  rename_with(~ wave_var[which(w3_var == .x)], .cols = w3_var) %>% 
  mutate(wave = 3)

d_ind <- d_indresp %>% 
  dplyr::select("pidp", w4_var)%>% 
  rename_with(~ wave_var[which(w4_var == .x)], .cols = w4_var) %>% 
  mutate(wave = 4)

e_ind <- e_indresp %>% 
  dplyr::select("pidp", w5_var) %>% 
  rename_with(~ wave_var[which(w5_var == .x)], .cols = w5_var) %>% 
  mutate(wave = 5)

f_ind <- f_indresp %>% 
  dplyr::select("pidp", "f_qfhighoth", w6_var)%>% 
  rename_with(~ wave_var[which(w6_var == .x)], .cols = w6_var) %>% 
  mutate(wave = 6)

g_ind <- g_indresp %>% 
  dplyr::select("pidp", w7_var) %>% 
  rename_with(~ wave_var[which(w7_var == .x)], .cols = w7_var) %>% 
  mutate(wave = 7)

h_ind <- h_indresp %>% 
  dplyr::select("pidp", w8_var)%>% 
  rename_with(~ wave_var[which(w8_var == .x)], .cols = w8_var) %>% 
  mutate(wave = 8)

i_ind <- i_indresp %>% 
  dplyr::select("pidp", w9_var) %>% 
  rename_with(~ wave_var[which(w9_var == .x)], .cols = w9_var) %>% 
  mutate(wave = 9)

j_ind <- j_indresp %>% 
  dplyr::select("pidp", w10_var)%>% 
  rename_with(~ wave_var[which(w10_var == .x)], .cols = w10_var) %>% 
  mutate(wave = 10)

k_ind <- k_indresp %>% 
  dplyr::select("pidp", w11_var)%>% 
  rename_with(~ wave_var[which(w11_var == .x)], .cols = w11_var) %>% 
  mutate(wave = 11)

ind_ukhls <-
  bind_rows(a_ind, b_ind) %>%
  bind_rows(., c_ind) %>%
  bind_rows(., d_ind) %>%
  bind_rows(., e_ind) %>%
  bind_rows(., f_ind) %>%
  bind_rows(., g_ind) %>%
  bind_rows(., h_ind) %>%
  bind_rows(., i_ind) %>%
  bind_rows(., j_ind) %>% 
  bind_rows(., k_ind) %>%
  relocate("wave", .after = "pidp") %>%
  # relocate("hhorig", .after = "wave") %>% 
  arrange(pidp, wave) %>%
  left_join(., ind_ukhls_xwave, by = "pidp")

saveRDS(ind_ukhls, file = "ind_ukhls.rds")

# -------------------------------------------------------------------------
# Combined dataset --------------------------------------------------------
# -------------------------------------------------------------------------

# ind_ukhls <- ind_ukhls %>%
#   # select(-ukborn.x, -plbornc.x) %>%
#   rename("ukborn" = "ukborn.y") %>%
#   rename("plbornc" = "plbornc.y")

indhhukhls <- left_join(ind_ukhls, hh_ukhls, by= c("hidp","wave")) %>% 
  arrange(pidp, wave) %>%
  rename("ukhlswave" = "wave") %>%
  mutate(wave = ukhlswave + 18) %>%
  relocate(wave, .after = "pidp")

saveRDS(indhhukhls, file = "indhhukhls.rds")

