#Coded by: Brian Buh
#Started on: 30.08.2022
#Last Updated: 06.10.2022

library(tidyverse)
library(haven)
library(lubridate)
library(stargazer)
library(arsenal)

# This script is for uploading cross-sectional and longitudinal survey weights

#The names in the BHPS datafiles do not match those of the UKHLS

###########################################################################
# Extracting Weights ------------------------------------------------------
###########################################################################


test <- ba_indresp %>% select(ba_xrwght)

test2 <- b_indresp %>% select(b_indinus_xw)

# -------------------------------------------------------------------------
# BHPS --------------------------------------------------------------------
# -------------------------------------------------------------------------


#Selecting variables and give them wave specific names
indvar1 <- c("xrwght", "xewght")

indvar26 <- c("xrwght", "xewght", "lrwght", "lewght")

indvar79 <- c("xrwght", "xewght", "lrwght", "lewght", "xrwghte", "xewghte")

indvar10 <- c("xrwght", "xewght", "lrwght", "lewght", "xrwtsw1", "xewtsw1", "xrwtsw2", "xewtsw2")

indvar1118 <- c("xrwght", "xewght", "lrwght", "lewght", "xrwtuk1", "xewtuk1", "xrwtuk2", "xewtuk2")

w1indvar <- paste0('ba_', indvar1)

w2indvar <- paste0('bb_', indvar26)

w3indvar <- paste0('bc_', indvar26)

w4indvar <- paste0('bd_', indvar26)

w5indvar <- paste0('be_', indvar26)

w6indvar <- paste0('bf_', indvar26)

w7indvar <- paste0('bg_', indvar79)

w8indvar <- paste0('bh_', indvar79)

w9indvar <- paste0('bi_', indvar79)

w10indvar <- paste0('bj_', indvar10)

w11indvar <- paste0('bk_', indvar1118)

w12indvar <- paste0('bl_', indvar1118)

w13indvar <- paste0('bm_', indvar1118)

w14indvar <- paste0('bn_', indvar1118)

w15indvar <- paste0('bo_', indvar1118)

w16indvar <- paste0('bp_', indvar1118)

w17indvar <- paste0('bq_', indvar1118)

w18indvar <- paste0('br_', indvar1118)

#Preparing the variables for merging; extracting from each wave
ba_wght <- ba_indresp %>% 
  dplyr::select("pidp", "pid",  w1indvar) %>% 
  rename_with(~ indvar1[which(w1indvar == .x)], .cols = w1indvar) %>%
  mutate(wave = 1)

bb_wght <- bb_indresp %>% 
  dplyr::select("pidp", "pid",  w2indvar) %>% 
  rename_with(~ indvar26[which(w2indvar == .x)], .cols = w2indvar) %>% 
  mutate(wave = 2)

bc_wght <- bc_indresp %>% 
  dplyr::select("pidp", "pid",  w3indvar) %>% 
  rename_with(~ indvar26[which(w3indvar == .x)], .cols = w3indvar) %>% 
  mutate(wave = 3)

bd_wght <- bd_indresp %>% 
  dplyr::select("pidp", "pid",  w4indvar) %>% 
  rename_with(~ indvar26[which(w4indvar == .x)], .cols = w4indvar) %>% 
  mutate(wave = 4)

be_wght <- be_indresp %>% 
  dplyr::select("pidp", "pid",  w5indvar) %>% 
  rename_with(~ indvar26[which(w5indvar == .x)], .cols = w5indvar) %>% 
  mutate(wave = 5)

bf_wght <- bf_indresp %>% 
  dplyr::select("pidp", "pid",  w6indvar) %>% 
  rename_with(~ indvar26[which(w6indvar == .x)], .cols = w6indvar) %>% 
  mutate(wave = 6)

bg_wght <- bg_indresp %>% 
  dplyr::select("pidp", "pid",  w7indvar) %>% 
  rename_with(~ indvar79[which(w7indvar == .x)], .cols = w7indvar) %>% 
  mutate(wave = 7)

bh_wght <- bh_indresp %>% 
  dplyr::select("pidp", "pid",  w8indvar) %>% 
  rename_with(~ indvar79[which(w8indvar == .x)], .cols = w8indvar) %>% 
  mutate(wave = 8)

bi_wght <- bi_indresp %>% 
  dplyr::select("pidp", "pid",  w9indvar) %>% 
  rename_with(~ indvar79[which(w9indvar == .x)], .cols = w9indvar) %>% 
  mutate(wave = 9)

bj_wght <- bj_indresp %>% 
  dplyr::select("pidp", "pid",  w10indvar) %>% 
  rename_with(~ indvar10[which(w10indvar == .x)], .cols = w10indvar) %>% 
  mutate(wave = 10)

bk_wght <- bk_indresp %>% 
  dplyr::select("pidp", "pid",  w11indvar) %>% 
  rename_with(~ indvar1118[which(w11indvar == .x)], .cols = w11indvar) %>% 
  mutate(wave = 11)

bl_wght <- bl_indresp %>% 
  dplyr::select("pidp", "pid",  w12indvar) %>% 
  rename_with(~ indvar1118[which(w12indvar == .x)], .cols = w12indvar) %>% 
  mutate(wave = 12)

bm_wght <- bm_indresp %>% 
  dplyr::select("pidp", "pid",  w13indvar) %>% 
  rename_with(~ indvar1118[which(w13indvar == .x)], .cols = w13indvar) %>% 
  mutate(wave = 13)

bn_wght <- bn_indresp %>% 
  dplyr::select("pidp", "pid",  w14indvar) %>% 
  rename_with(~ indvar1118[which(w14indvar == .x)], .cols = w14indvar) %>% 
  mutate(wave = 14)

bo_wght <- bo_indresp %>% 
  dplyr::select("pidp", "pid",  w15indvar) %>% 
  rename_with(~ indvar1118[which(w15indvar == .x)], .cols = w15indvar) %>% 
  mutate(wave = 15)

bp_wght <- bp_indresp %>% 
  dplyr::select("pidp", "pid",  w16indvar) %>% 
  rename_with(~ indvar1118[which(w16indvar == .x)], .cols = w16indvar) %>% 
  mutate(wave = 16)

bq_wght <- bq_indresp %>% 
  dplyr::select("pidp", "pid",  w17indvar) %>% 
  rename_with(~ indvar1118[which(w17indvar == .x)], .cols = w17indvar) %>% 
  mutate(wave = 17)

br_wght <- br_indresp %>% 
  dplyr::select("pidp", "pid",  w18indvar) %>% 
  rename_with(~ indvar1118[which(w18indvar == .x)], .cols = w18indvar) %>% 
  mutate(wave = 18)

#bind each wave specific dataframe together
wght_bhps <-
  bind_rows(ba_wght, bb_wght) %>%
  bind_rows(., bc_wght) %>%
  bind_rows(., bd_wght) %>%
  bind_rows(., be_wght) %>%
  bind_rows(., bf_wght) %>%
  bind_rows(., bg_wght) %>%
  bind_rows(., bh_wght) %>%
  bind_rows(., bi_wght) %>%
  bind_rows(., bj_wght) %>% 
  bind_rows(., bk_wght) %>%
  bind_rows(., bl_wght) %>%
  bind_rows(., bm_wght) %>%
  bind_rows(., bn_wght) %>%
  bind_rows(., bo_wght) %>%
  bind_rows(., bp_wght) %>%
  bind_rows(., bq_wght) %>%
  bind_rows(., br_wght)  %>% 
  relocate("wave", .after = "pid") %>%
  arrange(pidp, wave) 

saveRDS(wght_bhps, file = "wght_bhps.rds")




# -------------------------------------------------------------------------
# UKHLS -------------------------------------------------------------------
# -------------------------------------------------------------------------

#Selecting variables and give them wave specific names
indvar1 <- c("indinus_xw")

indvar2 <- c("indinus_xw", "indinbh_xw", "indinus_lw")

indvar35 <- c("indinub_xw", "indinub_lw")

indvar611 <- c("indinub_xw", "indinub_lw", "indinui_xw")


#Add the wave prefix to the variable list
w1_var <- paste0('a_', indvar1)

w2_var <- paste0('b_', indvar2)

w3_var <- paste0('c_', indvar35)

w4_var <- paste0('d_', indvar35)

w5_var <- paste0('e_', indvar35)

w6_var <- paste0('f_', indvar611)

w7_var <- paste0('g_', indvar611)

w8_var <- paste0('h_', indvar611)

w9_var <- paste0('i_', indvar611)

w10_var <- paste0('j_', indvar611)

w11_var <- paste0('k_', indvar611)


#Preparing the variables for merging
a_wght <- a_indresp %>% 
  dplyr::select("pidp", w1_var) %>% 
  rename_with(~ indvar1[which(w1_var == .x)], .cols = w1_var) %>% 
  mutate(wave = 1)

b_wght <- b_indresp %>% 
  dplyr::select("pidp", w2_var)%>% 
  rename_with(~ indvar2[which(w2_var == .x)], .cols = w2_var) %>% 
  mutate(wave = 2)

c_wght <- c_indresp %>% 
  dplyr::select("pidp", w3_var) %>% 
  rename_with(~ indvar35[which(w3_var == .x)], .cols = w3_var) %>% 
  mutate(wave = 3)

d_wght <- d_indresp %>% 
  dplyr::select("pidp", w4_var)%>% 
  rename_with(~ indvar35[which(w4_var == .x)], .cols = w4_var) %>% 
  mutate(wave = 4)

e_wght <- e_indresp %>% 
  dplyr::select("pidp", w5_var) %>% 
  rename_with(~ indvar35[which(w5_var == .x)], .cols = w5_var) %>% 
  mutate(wave = 5)

f_wght <- f_indresp %>% 
  dplyr::select("pidp", w6_var)%>% 
  rename_with(~ indvar611[which(w6_var == .x)], .cols = w6_var) %>% 
  mutate(wave = 6)

g_wght <- g_indresp %>% 
  dplyr::select("pidp", w7_var) %>% 
  rename_with(~ indvar611[which(w7_var == .x)], .cols = w7_var) %>% 
  mutate(wave = 7)

h_wght <- h_indresp %>% 
  dplyr::select("pidp", w8_var)%>% 
  rename_with(~ indvar611[which(w8_var == .x)], .cols = w8_var) %>% 
  mutate(wave = 8)

i_wght <- i_indresp %>% 
  dplyr::select("pidp", w9_var) %>% 
  rename_with(~ indvar611[which(w9_var == .x)], .cols = w9_var) %>% 
  mutate(wave = 9)

j_wght <- j_indresp %>% 
  dplyr::select("pidp", w10_var)%>% 
  rename_with(~ indvar611[which(w10_var == .x)], .cols = w10_var) %>% 
  mutate(wave = 10)

k_wght <- k_indresp %>% 
  dplyr::select("pidp", w11_var)%>% 
  rename_with(~ indvar611[which(w11_var == .x)], .cols = w11_var) %>% 
  mutate(wave = 11)

wght_ukhls <-
  bind_rows(a_wght, b_wght) %>%
  bind_rows(., c_wght) %>%
  bind_rows(., d_wght) %>%
  bind_rows(., e_wght) %>%
  bind_rows(., f_wght) %>%
  bind_rows(., g_wght) %>%
  bind_rows(., h_wght) %>%
  bind_rows(., i_wght) %>%
  bind_rows(., j_wght) %>% 
  bind_rows(., k_wght) %>%
  relocate("wave", .after = "pidp") %>%
  # relocate("hhorig", .after = "wave") %>% 
  arrange(pidp, wave) 
# 
# test <- wght_ukhls2 %>% filter(wave == 1)


saveRDS(wght_ukhls, file = "wght_ukhls.rds") 


#--------------------------------------------------------------------------
# Combined ----------------------------------------------------------------
#--------------------------------------------------------------------------

#BHPS
wght_bhps2 <- wght_bhps %>%
  select(pidp, wave, xrwght, xewght, xrwtsw1, xewtsw1, xrwtuk1, xewtuk1) %>% 
  rename("weight" = "xewght") 


# %>% 
  select(pidp, wave, weight)
  

#When the weight equals 0.00 the observation is not included for sampling reasons
wght_bhps2 %>% count(xrwght == 0)
#There are 84,716 0.00 weights
wght_bhps2 %>% count(is.na(xewght))
summary(wght_bhps2$weight)
wght_bhps2 %>% 
  ggplot(aes(x = weight)) +
  geom_histogram()





#UKHLS
wght_ukhls2 <- wght_ukhls%>%
  select(pidp, wave, indinus_xw, indinbh_xw, indinub_xw) %>% 
  mutate(weight = ifelse(is.na(indinub_xw), indinbh_xw, indinub_xw),
         weight = ifelse(is.na(weight), indinus_xw, weight),
         wave = wave + 18) %>% 
  select(pidp, wave, weight)

#When the weight equals 0.00 the observation is not included for sampling reasons
wght_ukhls2 %>% count(indinub_xw == 0)
#There are 89,037 0.00 weights
wght_ukhls2 %>% count(is.na(weight))
wght_ukhls2 %>% count(wave)
summary(wght_ukhls2$weight)
wght_ukhls2 %>% 
  ggplot(aes(x = weight)) +
  geom_histogram()


#The summary of the weight measures from the BHPS and UKHLS have a very similar distribution!


weights <-  
  bind_rows(wght_bhps2, wght_ukhls2) %>% 
  arrange(pidp, wave)

weights %>% 
  ggplot(aes(x = weight)) +
  geom_histogram()


###########################################################################
# Basic Weights Testing ---------------------------------------------------
###########################################################################


baselinelogit <- glm(formula = event ~ clock + ratio_cat2 + parity + period + age + agesq + tenure,
                     family = binomial(link = "logit"),
                     data = hhpart5)

baselineweight <- glm(formula = event ~ clock + ratio_cat2 + parity + period + age + agesq + tenure,
                     family = binomial(link = "logit"),
                     data = hhpart5,
                     weights = weight)


summary(baselinelogit)
summary(baselineweight)

stargazer(baselinelogit, baselineweight, align = TRUE, out = "weighttest.html")


### Descriptives
#separated by parity
mycontrols <- tableby.control(test = FALSE)
hhpartstatswght <-arsenal::tableby(parity ~ event + clock + ratio + ratio_cat2 + period + tenure + age + partner + edu + ukborn + emp, 
                                data = hhpart5, 
                                weights = weight,
                                control = mycontrols)
labels(hhpartstatswght) <-  c(parity = "Parity", event = "Event", clock = "Exposure", age = "Age",
                           ratio = "Ratio of Housing to Income", ratio_cat2 = "Ratio of Housing to Income (Cat.)", period = "Period",
                           tenure = "Housing type", partner = "Partnership status", edu = "Educational attainment", ukborn = "UK Born",
                           emp = "Activity status")
summary(hhpartstatswght)
write2html(hhpartstatswght, "hhpartstatswght_parity_06-10-2022.html") #UPDATE DATE
write2word(hhpartstatswght, "hhpartstatswght_parity_06-10-2022.docx") #UPDATE DATE


