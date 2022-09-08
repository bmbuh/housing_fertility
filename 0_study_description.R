# Project description:
#   
# This project was initiated by Brian Buh at the Vienna Institute of Demography; Wittgenstein Centre for Global Human Capital (IIASA, OeAW, University of Vienna)
# This research is part of the BIRTHLIFE project funded by the Austrian Science Fund (FWF, p 31357-G29) https://www.oeaw.ac.at/vid/research/research-projects/birthlife
# Scripts are available here: https://github.com/bmbuh/housing_fertility
# Please contact Brian Buh for any questions or comments. brian.buh@oeaw.ac.at

####
## Research Questions:
# How does the relative cost of housing associate with the likelihood of having a child? Has this changed over time? Does it differ by parity?
# We aim to capture the change in the relation of housing costs and fertility over time and by birth order.

####
## Data:
# Data is provided by the UK Data Service
# We use two yearly household longitudinal panel studies:
# 1. British Household Panel Study (1991-2008)
# 2. United Kingdom Household Longitudinal Study (2009-present) (Understanding Society)

# Script Table of Contents
# s1. Load data frames of raw data from the UK Data Service
# s2. Extract fertility data from fertility records to build fertility histories
# s3. Extract variables from the household and individual response yearly datasets
# s3-1. Extract the local authority district data and combine with lower quartile housing price data
# s3-2. Extract survey weight data for all waves
# s4. Test housing costs and household income data for irregularities. Select the best measure
# s5. Combine dataframes, create uniform variables, create EHA aspects (event, clock, parity)
# s6. Test model fit and convergence  