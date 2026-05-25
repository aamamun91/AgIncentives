
# -------------------------------------------------------------------------------

# This is code part of code for the Ag-Incentives Consortium: 
# TOTALS and NonMPS Commodities

# Last Updated by Tess Lallemant in 08/2018

# Source Versions
#   OECD: July 2016
#   MAFAP: December 2016
#   Agrimonitor:  December 2016
#   World Bank (SA): November 2016

# Ag-Incentives Version: Ag-Incentives Consortium (August 2017)

# -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)

source("AgIncentivesProcessing.R")

# Use FAO total values of production TRY TO USE LATEST AVAILABLE
FAO_TVP <- read.csv("./Intermediary/FAO_TOTAL_VP-2018-03-19.csv") %>%
  rename(TOTAL = Value)

###############################################################################
# Map Commodities by Source ---------------------------------------------------

# OECD provides NonMPS data 
OECD <- SOURCE_OECD %>%
  left_join(COMMODITY, by = c("commodity_code" = "OECD_CODE")) %>%
  rename(COMMODITY_CODE = commodity_code) %>%
  filter(COMMODITY_CODE %in% c('XE', 'NONMPS'), year > 2005) %>%
  mutate(NUMSOURCE = 1,
         VP_NMPS = VP/ ER_OFFICIAL ) %>%
  select(NUMSOURCE, country_code, COMMODITY_CODE, year, VP_NMPS) %>%
  rename(COUNTRY_CODE = country_code, 
         YEAR = year)

# CALCULATE VALUE OF PRODUCTION OF NONMPS COMMODITIES
COUNTRY_NMPS <- AG_PRIO %>%
  filter(NUMSOURCE != 1) %>%
  select(COUNTRY_CODE, NUMSOURCE, YEAR, VP_PROP) %>%
  group_by(COUNTRY_CODE, NUMSOURCE, YEAR) %>%
  summarise(VP_PROP = sum(VP_PROP, na.rm = TRUE)) %>%
  left_join(FAO_TVP, by = c("YEAR" = "Year", "COUNTRY_CODE" = "ISO3CODE")) %>%
  mutate(VP_NMPS = (TOTAL* 1000000) - VP_PROP,
         COMMODITY_CODE = "NONMPS") %>%
  bind_rows(OECD) %>%
  select(NUMSOURCE, COUNTRY_CODE, YEAR, VP_NMPS) 

# NONMPS as PERCENTAGE OF TOTAL 
PCT_NMPS <- COUNTRY_NMPS %>%
left_join(FAO_TVP, by = c("YEAR" = "Year", "COUNTRY_CODE" = "ISO3CODE")) %>%
  mutate(PCT_NMPS = VP_NMPS/(TOTAL*1000000) *100)

  
  

  


