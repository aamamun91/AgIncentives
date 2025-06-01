# -----------------------------------------------------------------------------
# This is code to create tables needed for the Ag-Incentives Consortium: 
# PublicView, VerificationTool, WorldMap
# Note the final tables are not created here
# Ag-Incentives Version: Ag-Incentives Consortium (August 2018)

# PLEASE UPDATE the source timestamp when relevant ln28 & 33
# -----------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(ggplot2)
source("Mapping.R")
# Importing Source Data -------------------------------------------------------
## Files have to be be imported manually to the source folder
SOURCE_OECD <- read.csv("./Source/Input_files_2024/OECD_input_file.csv", stringsAsFactors=FALSE)
TIME_OECD <-file.info("./Source/Input_files_2024/OECD_input_file.csv")$ctime
SOURCE_WBSA <- read.csv("./Source/Input_files_2024/WBSA_input_file.csv", stringsAsFactors=FALSE)
TIME_WBSA <-file.info("./Source/Input_files_2024/WBSA_input_file.csv")$ctime
SOURCE_MAFAP <- read.csv("./Source/Input_files_2024/mafap_input_file.csv", stringsAsFactors=FALSE)
TIME_MAFAP <-file.info("./Source/Input_files_2024/mafap_input_file.csv")$ctime
SOURCE_IDB <- read.csv("./Source/Input_files_2024/IDB_input_file.csv", stringsAsFactors=FALSE) %>%
  mutate(ccode=as.character(COMMODITY_CODE)) %>%
  select(-COMMODITY_CODE) %>%
  mutate(COMMODITY_CODE=paste0(ccode)) %>%
  select(-ccode)
TIME_IDB <-file.info("./Source/Input_files_2024/IDB_input_file.csv")$ctime


###############################################################################
YEARMAX<-2023  
YEARSTART<-2005
AGCOMMODITY <- c('XE', 'NONMPS','50','TOTAL')
###############################################################################
# Create and Update Source Tables ---------------------------------------------
SOURCE = c("OECD", "MAFAP", "Agrimonitor (IDB)", "World Bank (SA)", "Ag-Incentives Consortium") 
SOURCE_VERSION  = c("OECD (PSE December 2024)", "FAO (December 2024)", 
                    "Agrimonitor (IDB, December 2024)",
                    "World Bank (SA, December 2016)", 
                    "Ag-Incentives Consortium (May 2020)" ) 
NUMSOURCE= c(1,2,3,4,5) 

SOURCEMAP = data.frame(SOURCE, SOURCE_VERSION, NUMSOURCE)
###############################################################################
# Map Commodities by Source ---------------------------------------------------
TONSUNIT = c("MT", "Tonnes","Metric Tonnes","Metric Tons","Metric tons","Tonne")

OECD <- SOURCE_OECD %>%
  left_join(COMMODITY_M, by = c("COMMODITY_CODE" = "OECD_CODE")) 
names(OECD)<- toupper(names(OECD))


WBSA <- SOURCE_WBSA %>%
        mutate(COMMODITY_CODE = as.factor(COMMODITY_CODE), TIMESTAMP="04/04/2018") %>%
        left_join(COMMODITY_M, by = c("COMMODITY_CODE" = "AGCOMCODE")) %>%
        mutate(AGCOMCODE=COMMODITY_CODE,
               PRODQ_PHY_UNIT=ifelse(PRODQ_PHY_UNIT %in% TONSUNIT, "MT", paste0(PRODQ_PHY_UNIT)),
               REFP_PHY_UNIT=ifelse(REFP_PHY_UNIT %in% TONSUNIT, "MT", paste0(REFP_PHY_UNIT)),
               PRODQ_PHY_UNIT="MT",
               PROP_PHY_UNIT=REFP_PHY_UNIT,
               PROP_MON_UNIT=REFP_MON_UNIT) %>%
        select(-NOTES) %>%
        mutate(NPC_SOURCE=1)


MAFAP <-  SOURCE_MAFAP %>%
  mutate(COMMODITY_CODE = as.character(COMMODITY_CODE)) %>%
  left_join(COMMODITY_M, by = c("COMMODITY_CODE" = "MAFAP_CODE")) 


IDB <-  SOURCE_IDB %>%
  left_join(COMMODITY_M, by = c("COMMODITY_CODE" = "IADB_CODE")) %>%
  mutate(SOURCE="Agrimonitor (IDB)") 


# propose to filter commodity code 50 here. 



AGDATA <- bind_rows(OECD, MAFAP, IDB, WBSA)

XE.TOTAL <- AGDATA %>% filter(COMMODITY_CODE %in% AGCOMMODITY) %>% filter(!is.na(VP)) %>% mutate(VP=VP/ER_OFFICIAL) %>% 
              group_by(SOURCE, COUNTRY_LABEL, COUNTRY_CODE, COMMODITY_CODE, YEAR) %>% 
            summarise(VP=sum(VP, na.rm = T)) %>% ungroup() %>% spread(COMMODITY_CODE, VP) %>% 
            filter(YEAR>=YEARSTART & YEAR<=YEARMAX)

MPS.ONLY <- AGDATA %>% filter(!COMMODITY_CODE %in% AGCOMMODITY) %>% mutate(VP=VP/ER_OFFICIAL) %>% 
            group_by(SOURCE, COUNTRY_LABEL, COUNTRY_CODE, YEAR) %>% 
            summarise(VP_MPS=sum(VP, na.rm = T)) %>% ungroup() %>% 
            filter(YEAR>=YEARSTART & YEAR<=YEARMAX)

XE.FINAL <- XE.TOTAL %>% left_join(MPS.ONLY) %>% 
                  mutate(VP_XE_rec = TOTAL-VP_MPS, NMPS_rat1 = NONMPS/VP_XE_rec, XE_rat2 = XE/VP_XE_rec) %>% 
                  mutate(COMMODITY_LABEL='NonMPS from Workbook', COMMODITY_CODE='XE') %>% rename(VP_XE=NONMPS) %>% 
                  select(COUNTRY_CODE, YEAR, VP_XE)
            


