#Coded by: Brian Buh
#Started on: 02.06.2022
#Last Updated: 13.06.2022

### Packages
library(tidyverse)
library(haven)
library(lubridate)
library(usethis)


###########################################################################
# Load Files for Project --------------------------------------------------
###########################################################################

# -------------------------------------------------------------------------
# BHPS --------------------------------------------------------------------
# -------------------------------------------------------------------------

#wave 1
ba_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w1/ba_indresp_protect.dta")
ba_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w1/ba_hhresp_protect.dta")
ba_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w1/ba_jobhist_protect.dta")
ba_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/a_nethh.dta")
ba_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/a_neta.dta")

#wave 2
bb_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w2/bb_jobhist_protect.dta")
bb_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w2/bb_indresp_protect.dta")
bb_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w2/bb_hhresp_protect.dta")
bb_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/b_nethh.dta")
bb_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/b_neta.dta")

#wave 3
bc_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w3/bc_jobhist_protect.dta")
bc_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w3/bc_indresp_protect.dta")
bc_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w3/bc_hhresp_protect.dta")
bc_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/c_nethh.dta")
bc_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/c_neta.dta")

#wave 4
bd_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w4/bd_jobhist_protect.dta")
bd_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w4/bd_indresp_protect.dta")
bd_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w4/bd_hhresp_protect.dta")
bd_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/d_nethh.dta")
bd_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/d_neta.dta")

#wave 5
be_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w5/be_jobhist_protect.dta")
be_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w5/be_indresp_protect.dta")
be_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w5/be_hhresp_protect.dta")
be_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/e_nethh.dta")
be_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/e_neta.dta")

#wave6
bf_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w6/bf_jobhist_protect.dta")
bf_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w6/bf_indresp_protect.dta")
bf_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w6/bf_hhresp_protect.dta")
bf_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/f_nethh.dta")
bf_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/f_neta.dta")

#wave 7
bg_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w7/bg_jobhist_protect.dta")
bg_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w7/bg_indresp_protect.dta")
bg_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w7/bg_hhresp_protect.dta")
bg_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/g_nethh.dta")
bg_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/g_neta.dta")

#wave 8
bh_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w8/bh_jobhist_protect.dta")
bh_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w8/bh_indresp_protect.dta")
bh_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w8/bh_hhresp_protect.dta")
bh_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/h_nethh.dta")
bh_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/h_neta.dta")

#wave 9
bi_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w9/bi_jobhist_protect.dta")
bi_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w9/bi_indresp_protect.dta")
bi_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w9/bi_hhresp_protect.dta")
bi_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/i_nethh.dta")
bi_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/i_neta.dta")

#wave 10
bj_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w10/bj_jobhist_protect.dta")
bj_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w10/bj_indresp_protect.dta")
bj_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w10/bj_hhresp_protect.dta")
bj_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/j_nethh.dta")
bj_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/j_neta.dta")

#wave 11
bk_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w11/bk_jobhist_protect.dta")
bk_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w11/bk_indresp_protect.dta")
bk_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w11/bk_hhresp_protect.dta")
bk_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/k_nethh.dta")
bk_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/k_neta.dta")

#wave 12
bl_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w12/bl_jobhist_protect.dta")
bl_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w12/bl_indresp_protect.dta")
bl_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w12/bl_hhresp_protect.dta")
bl_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/l_nethh.dta")
bl_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/l_neta.dta")

#wave 13
bm_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w13/bm_jobhist_protect.dta")
bm_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w13/bm_indresp_protect.dta")
bm_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w13/bm_hhresp_protect.dta")
bm_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/m_nethh.dta")
bm_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/m_neta.dta")

#wave 14
bn_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w14/bn_jobhist_protect.dta")
bn_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w14/bn_indresp_protect.dta")
bn_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w14/bn_hhresp_protect.dta")
bn_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/n_nethh.dta")
bn_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/n_neta.dta")

#wave 15
bo_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w15/bo_jobhist_protect.dta")
bo_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w15/bo_indresp_protect.dta")
bo_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w15/bo_hhresp_protect.dta")
bo_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/o_nethh.dta")
bo_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/o_neta.dta")

#wave 16
bp_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w16/bp_jobhist_protect.dta")
bp_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w16/bp_indresp_protect.dta")
bp_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w16/bp_hhresp_protect.dta")
bp_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/p_nethh.dta")
bp_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/p_neta.dta")

#wave 17
bq_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w17/bq_jobhist_protect.dta")
bq_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w17/bq_indresp_protect.dta")
bq_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w17/bq_hhresp_protect.dta")
bq_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/q_nethh.dta")
bq_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/q_neta.dta")

#wave 18
br_jobhist <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w18/br_jobhist_protect.dta")
br_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w18/br_indresp_protect.dta")
br_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_w18/br_hhresp_protect.dta")
br_weekinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/dacurrent/r_nethh.dta")
br_yearinc <- read_dta("S:/Questionnaires/UKHLS/UKDA-3909-stata10/stata10/daannual/r_neta.dta")

#xwavedata
bhpsxwave <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/bhps_wx/xwaveid_bh_protect.dta")

# Fertility Histories
fert_his <- read_dta("S:/questionnaires/UKHLS/bhps_fertility_partnership_histories/stata8/family_cleaned_dates.dta")


# -------------------------------------------------------------------------
# UKHLS -------------------------------------------------------------------
# -------------------------------------------------------------------------

#wave 1
a_indall <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_indall_protect.dta")
a_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_child_protect.dta")
a_natchild <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_natchild_protect.dta")
a_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_indresp_protect.dta")
a_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_hhresp_protect.dta")

#wave 2
b_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w2/b_child_protect.dta")
b_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w2/b_indresp_protect.dta")
b_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w2/b_hhresp_protect.dta")

#wave 3
c_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w3/c_child_protect.dta")
c_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w3/c_indresp_protect.dta")
c_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w3/c_hhresp_protect.dta")

#wave 4
d_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w4/d_child_protect.dta")
d_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w4/d_indresp_protect.dta")
d_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w4/d_hhresp_protect.dta")

#wave 5
e_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w5/e_child_protect.dta")
e_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w5/e_indresp_protect.dta")
e_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w5/e_hhresp_protect.dta")

#wave6
f_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w6/f_child_protect.dta")
f_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w6/f_indresp_protect.dta")
f_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w6/f_hhresp_protect.dta")

#wave 7
g_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w7/g_child_protect.dta")
g_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w7/g_indresp_protect.dta")
g_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w7/g_hhresp_protect.dta")

#wave 8
h_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w8/h_child_protect.dta")
h_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w8/h_indresp_protect.dta")
h_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w8/h_hhresp_protect.dta")

#wave 9
i_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w9/i_child_protect.dta")
i_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w9/i_indresp_protect.dta")
i_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w9/i_hhresp_protect.dta")

#wave 10
j_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w10/j_child_protect.dta")
j_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w10/j_indresp_protect.dta")
j_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w10/j_hhresp_protect.dta")

#wave 11
k_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w11/k_child_protect.dta")
k_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w11/k_indresp_protect.dta")
k_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w11/k_hhresp_protect.dta")


#xwavedata
xwave <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_wx/xwavedat_protect.dta")

###########################################################################
# Parent data (Alita Nandi) -----------------------------------------------
###########################################################################

#Retrospective Fertility History
a_parent <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_parent_protect.dta")


###########################################################################
# Employment and Education History -----------------------------------------
###########################################################################
#File stata_work_life_histories produced from the Stata code provided by
#Liam Wright on the UK Data service

#!!! There is an issue with the transformation fo dates from the Stata file to the R
# file that needs to be fixed. The simplest answer would be to separate the month
# and years to separate variables in STATA and then bring the files into R

#Merged
merged_hist <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/Merged Dataset.dta")

#UKHLS
annual_his <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/UKHLS Annual History.dta")
life_his <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/UKHLS Life History.dta")
edu_his <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/UKHLS Education History.dta")
#Left Full Time Education
left_edu <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/Left Full Time Education.dta")
# edu_his_var <- read_dta("S:/Questionnaires/UKHLS/stata_work_life_histories/Education Variables - Cleaned.dta")



###########################################################################
# Partnership Histories ---------------------------------------------------
###########################################################################

part_his <- read_dta("S:/Questionnaires/UKHLS/partnership-histories/stata/stata13/phistory_long.dta")





