# -----------------------------------------------------------------------------
# This is code to create tables needed for the Ag-Incentives Consortium: 
# PublicView, VerificationTool, WorldMap
# Note the final tables are not created here
# Ag-Incentives Version: Ag-Incentives Consortium (August 2018)

# PLEASE UPDATE the source timestamp when relevant ln28 & 33
# -----------------------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(readxl)
library(ggplot2)
source("Functions.R")
source("Mapping.R")
# Importing Source Data -------------------------------------------------------
## Files have to be be imported manually to the source folder
 SOURCE_OECD <- read.csv("./Source/Input_files_2024/OECD_input_file.csv", stringsAsFactors=FALSE)
 TIME_OECD <-file.info("./Source/Input_files_2024/OECD_input_file.csv")$ctime
 SOURCE_WBSA <- read.csv("./Source/Input_files_2024/WBSA_input_file.csv", stringsAsFactors=FALSE)
 TIME_WBSA <-file.info("./Source/Input_files_2024/WBSA_input_file.csv")$ctime
 SOURCE_FAO <- read.csv("./Source/Input_files_2024/MAFAP_input_file.csv", stringsAsFactors=FALSE)
 TIME_FAO <-file.info("./Source/Input_files_2024/MAFAP_input_file.csv")$ctime
 SOURCE_IDB <- read.csv("./Source/Input_files_2024/IDB_input_file.csv", stringsAsFactors=FALSE) %>%
                mutate(ccode=as.character(COMMODITY_CODE)) %>%
                select(-COMMODITY_CODE) %>%
                mutate(COMMODITY_CODE=paste0(ccode)) %>%
                select(-ccode)
 TIME_IDB <-file.info("./Source/Input_files_2024/IDB_input_file.csv")$ctime
 

###############################################################################
 YEARMAX<-2024  
 YEARSTART<-2005
 AGCOMMODITY <- c('XE', 'NONMPS','50','TOTAL')
###############################################################################
# Create and Update Source Tables ---------------------------------------------
SOURCE = c("OECD", "FAO", "Agrimonitor (IDB)", "World Bank (SA)", "Ag-Incentives Consortium") 
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

CHECK_OECD <- OECD %>% 
              select(COUNTRY_CODE,YEAR,COMMODITY_CODE) %>%
              group_by(COUNTRY_CODE,YEAR,COMMODITY_CODE) %>%
              summarize(OBS=n()) %>%
              ungroup() %>%
              filter(OBS>1)
head(CHECK_OECD)

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

CHECK_WBSA <- WBSA %>% 
              select(COUNTRY_CODE,YEAR,COMMODITY_CODE) %>%
              group_by(COUNTRY_CODE,YEAR,COMMODITY_CODE)%>%
              summarize(OBS=n())%>%
              ungroup()%>%
              filter(OBS>1)
head(CHECK_WBSA)

FAO <-  SOURCE_FAO %>%
          mutate(COMMODITY_CODE = as.character(COMMODITY_CODE)) %>%
          left_join(COMMODITY_M, by = c("COMMODITY_CODE" = "MAFAP_CODE")) 


CHECK_FAO<- FAO %>% 
              select(COUNTRY_CODE,YEAR,COMMODITY_CODE) %>%
              group_by(COUNTRY_CODE,YEAR,COMMODITY_CODE) %>%
              summarize(OBS=n())%>%
              ungroup()%>%
              filter(OBS>1)
head(CHECK_FAO)

IDB <-  SOURCE_IDB %>%
        left_join(COMMODITY_M, by = c("COMMODITY_CODE" = "IADB_CODE")) %>%
        mutate(SOURCE="Agrimonitor (IDB)") 


# propose to filter commodity code 50 here. 

CHECK_IDB<- IDB %>% 
            select(COUNTRY_CODE,YEAR,COMMODITY_CODE) %>%
            group_by(COUNTRY_CODE,YEAR,COMMODITY_CODE) %>%
            summarize(OBS=n()) %>%
            ungroup() %>%
            filter(OBS>1)
head(CHECK_IDB)

###############################################################################
# Consolidate the Sources ----------------------------------------------------- 

AGDATA <- bind_rows(OECD, FAO, IDB, WBSA)

CPT_AGDATA <- AGDATA %>% 
              select(COUNTRY_CODE,COMMODITY_CODE,YEAR,SOURCE) %>%
              group_by(COUNTRY_CODE,COMMODITY_CODE,YEAR,SOURCE) %>%
              summarize(cpt=n()) %>%
              filter(cpt>1)

if (NROW(CPT_AGDATA)>0) {stop("Duplicates in source files")}

AGDATA_notes <- AGDATA %>% 
                select(starts_with("NOTE"),SOURCE) %>%
                distinct

{
TIMES_AG <- AGDATA %>% 
            select (SOURCE,TIMESTAMP) %>% 
            distinct()
TIMES_AG


TEST_AGDATA <- AGDATA %>%
                filter(YEAR >= YEARSTART) %>%
                filter(!COMMODITY_CODE %in% AGCOMMODITY) %>% 
                select(SOURCE,COUNTRY_CODE,COMMODITY_CODE,COMMODITY_LABEL,AGCOMCODE,TIMESTAMP,REFP,PROP,REFP_MON_UNIT,REFP_PHY_UNIT,
                       PROP_PHY_UNIT,PROP_MON_UNIT,PRODQ,PRODQ_PHY_UNIT) %>%
                mutate(
                  NA_COMCODE=ifelse(is.na(AGCOMCODE),1,0),
                  NA_TIMESTAMP=ifelse(is.na(TIMESTAMP),1,0),
                  NA_REFP=ifelse(is.na(REFP),1,0),
                  NA_PROP=ifelse(is.na(PROP),1,0),
                  NA_MUNIT=ifelse(is.na(REFP_MON_UNIT),1,0),
                  NA_PUNIT=ifelse(is.na(REFP_PHY_UNIT),1,0),
                  MIS_MUNIT=ifelse(!(REFP_MON_UNIT==PROP_MON_UNIT) | is.na(REFP_MON_UNIT) | is.na(PROP_MON_UNIT),1,0),
                  MIS_PUNIT=ifelse(!(REFP_PHY_UNIT==PROP_PHY_UNIT) | is.na(REFP_PHY_UNIT) | is.na(PROP_PHY_UNIT),1,0),
                  MIS_PQUNIT=ifelse(!(PRODQ_PHY_UNIT==PROP_PHY_UNIT) & PRODQ>0 | is.na(PRODQ_PHY_UNIT) | is.na(PROP_PHY_UNIT),1,0),
                  ERRORCOUNT=NA_COMCODE+NA_TIMESTAMP+NA_REFP+NA_PROP+NA_MUNIT+NA_PUNIT+MIS_MUNIT+MIS_PUNIT+MIS_PQUNIT
                       ) %>%
              select(SOURCE,COUNTRY_CODE,COMMODITY_CODE,COMMODITY_LABEL,NA_COMCODE,NA_TIMESTAMP,NA_REFP,NA_PROP,
                     NA_MUNIT,NA_PUNIT,MIS_MUNIT,MIS_PUNIT,MIS_PQUNIT,ERRORCOUNT) %>%
              filter(ERRORCOUNT>=1)

TEST_AGDATA2 <- TEST_AGDATA %>%
                gather(ERRORS,NBERRORS, 5:14)%>%
                filter(NBERRORS>=1) %>%
                distinct() 
head(TEST_AGDATA2,10)

TEST_AGDATA_SUM <- TEST_AGDATA %>%
                    select(-COUNTRY_CODE,-COMMODITY_CODE,-COMMODITY_LABEL) %>%
                    group_by(SOURCE) %>%
                    summarise_all(funs(sum)) %>%
                    ungroup()



LIST_PUNIT_FARM <- AGDATA%>% select(PROP_PHY_UNIT) %>% distinct()
LIST_PUNIT_FARM
LIST_PUNIT_REF <- AGDATA %>% select(REFP_PHY_UNIT) %>% distinct()
LIST_PUNIT_REF
LIST_PUNIT_PQ <- AGDATA %>% select(PRODQ_PHY_UNIT) %>% distinct()
LIST_PUNIT_PQ

if (NROW(TEST_AGDATA_SUM)>0) {stop("Unsuccesful Data Imports- Check TEST_AGDATA_SUM and TEST_AGDATA")    
} else {  print("Succesful Import!")}

# Keep Records of Consolidated Source Data
write.csv(AGDATA, paste0("./Intermediary/AGDATA-", Sys.Date(), ".csv", sep = ""), row.names = FALSE)
}
###############################################################################
# Exchange Rate Table ---------------------------------------------------------
{
FOREX <-  AGDATA %>%
          select(SOURCE,
                 COUNTRY_CODE, 
                 YEAR,
                 ER_OFFICIAL, 
                 EROFFICIAL_SOURCE,
                 EROFFICIAL_UNIT) 

TEST_FOREX <- FOREX %>%
              group_by(SOURCE,COUNTRY_CODE, YEAR) %>%
              summarise(ER_OFFICIAL_X = n_distinct(ER_OFFICIAL),
                        EROFFICIAL_UNIT_X = n_distinct(EROFFICIAL_UNIT),
                        EROFFICIAL_UNIT_X = n_distinct(EROFFICIAL_SOURCE)) %>%
              filter(ER_OFFICIAL_X > 1| EROFFICIAL_UNIT_X>1 | EROFFICIAL_UNIT_X>1) %>%
              ungroup() %>%
              left_join(FOREX)

if (NROW(TEST_FOREX)>0) {stop("Duplicate for Exchange Rate values")    
} else {  print("Succesful Import of Exchange rates!")}
}

###############################################################################
# Missing Units, Harmonization and aggregates

# Price Conversions, Treatment, and Cases for tests -------------------------------------

XNONMPS <- AGDATA %>%
            filter(COMMODITY_CODE %in% c('XE', 'NONMPS','50')) 
XTOTAL <- AGDATA %>%
          filter(COMMODITY_CODE %in% c('TOTAL')) 

MPDZERO_IDB<- AGDATA %>%
              filter( REFP>PROP+0.1 & !(COMMODITY_CODE %in% AGCOMMODITY) ) %>% 
              left_join(SOURCEMAP) %>%
              left_join(COMMODITY_A, by = c("AGCOMCODE")) %>%
              filter(NUMSOURCE==3 ) %>%
              mutate(MPD=(PROP-REFP)) %>%
              select(COUNTRY_LABEL,COMMODITY_LABEL,YEAR,MPD) %>%
              group_by(COUNTRY_LABEL,COMMODITY_LABEL) %>%
              summarize(checkneg=n()) %>%
              spread(COUNTRY_LABEL,checkneg)


CONS_MAPPED0 <- AGDATA %>%
                filter(!(COMMODITY_CODE %in% AGCOMMODITY) ) %>%
                left_join(SOURCEMAP) %>%
                left_join(COMMODITY_A, by = c("AGCOMCODE")) %>%
                mutate(KEYSOURCE = paste0(NUMSOURCE,COUNTRY_CODE, YEAR, AGCOMCODE),
                       SOURCE = as.character(SOURCE), 
                       PRODQ = case_when(PRODQ_PHY_UNIT == "HEAD" ~ PRODQ *.28, TRUE ~ PRODQ),
                       PRODQ_PHY_UNIT = case_when(PRODQ_PHY_UNIT == "HEAD" ~ "MT", 
                                                  TRUE ~ as.character(PRODQ_PHY_UNIT)),
                       PROP = case_when(PRODQ_PHY_UNIT == "HEAD" ~ PROP/.28/ER_OFFICIAL, 
                                        TRUE ~ PROP/ER_OFFICIAL),
                       REFP = case_when(PRODQ_PHY_UNIT == "HEAD" ~ REFP/.28/ER_OFFICIAL, 
                                        TRUE ~ REFP/ER_OFFICIAL),
                       EFC = case_when(PRODQ_PHY_UNIT == "HEAD" ~ EFC/.28/ER_OFFICIAL, 
                                       TRUE ~ EFC/ER_OFFICIAL),
                       MPS = case_when(PRODQ_PHY_UNIT == "HEAD" ~ MPS/.28/ER_OFFICIAL, 
                                       TRUE ~ MPS/ER_OFFICIAL),
                       PSCT = case_when(PRODQ_PHY_UNIT == "HEAD" ~ PSCT/.28/ER_OFFICIAL,
                                        TRUE ~ PSCT/ER_OFFICIAL),
                       NPC  = NPC_SOURCE,
                       CASE_A = case_when(MPS == 0 & (PROP - REFP) != 0 & EFC == 0  ~  1),
                       CASE_B = case_when(MPS == -(EFC) & (PROP - REFP) != 0 & EFC != 0 ~  1),
                       CASE_C = case_when(MPS == 0 & (PROP - REFP) != 0 & EFC != 0  ~  1),
                       CASE_D = case_when(MPS < 0 & (PROP - REFP) > 0 & EFC <= 0 ~  1),
                       CASE_E = case_when(MPS > 0 & (PROP - REFP) < 0 & EFC >= 0 ~  1),
                       REFP = case_when(MPS == 0 & (PROP - REFP) != 0 & EFC == 0  ~  PROP,
                                        MPS == -(EFC) & (PROP - REFP) != 0 & EFC != 0 ~  PROP,
                                        TRUE ~ REFP),
                       NOTE_4 = case_when(CASE_A == 1 ~ 4,
                                          CASE_B == 1 ~ 4,
                                          TRUE ~ as.numeric(NOTE_4)) ) %>%
                filter(YEAR >=YEARSTART) %>%
                select(NUMSOURCE, KEYSOURCE, SOURCE, COUNTRY_CODE, YEAR, 
                       COMMODITY_CODE, AGCOMCODE, AGCOMNAME,
                       PRODQ_PHY_UNIT, PRODQ, PROP, REFP, EFC, MPS, PSCT, NPC,
                       starts_with("NOTE"), starts_with("CASE"),-NOTE_0) %>%
                distinct()

# no correction ot treatment done for case c

###############################################################################
# Create Notes Mapping - Processing of Notes that can be accompanied with Note 0
###############################################################################

CONS_MAPPED_NOTESx <- CONS_MAPPED0 %>%
                      left_join(COMMODITY_A, by = c("AGCOMCODE")) %>%
                      mutate(KEYSOURCE2=paste0(NUMSOURCE,COUNTRY_CODE, YEAR, AGPROCODE)) %>%
                      select(KEYSOURCE2, starts_with("NOTE_")) %>%
                      group_by(KEYSOURCE2) %>%
                      summarize_all(funs(max))

CONS_MAPPED_NOTESy <- CONS_MAPPED_NOTESx
CONS_MAPPED_NOTESy[is.na(CONS_MAPPED_NOTESy)] <- 0

CONS_MAPPED_NOTES0 <-   CONS_MAPPED_NOTESy %>%
                        # check notes with modifications  
                        mutate(NOTE_X=NOTE_1+NOTE_2+NOTE_3+NOTE_4+NOTE_5+NOTE_6+NOTE_7+NOTE_9+NOTE_10+NOTE_11+NOTE_12+NOTE_13+NOTE_14+NOTE_15+NOTE_16,
                               NOTE_0=ifelse(NOTE_X==0,0,NA_real_)) %>%
                        select(KEYSOURCE2,NOTE_0)


CONS_MAPPED_NOTES <- CONS_MAPPED_NOTESx %>%
                      left_join(CONS_MAPPED_NOTES0) %>%
                      unite(NOTES,starts_with("NOTE_"),sep=";",remove=TRUE) %>%
                      mutate(NOTES=str_replace_all(NOTES,"NA;",""),
                             NOTES=str_replace_all(NOTES,";NA",""),
                             NOTES=str_replace_all(NOTES,"NA",""))

CONS_MAPPED <-  CONS_MAPPED0 %>%
                left_join(COMMODITY_A) %>%
                mutate(KEYSOURCE2=paste0(NUMSOURCE,COUNTRY_CODE, YEAR, AGPROCODE)) %>%
                left_join(CONS_MAPPED_NOTES) %>%
                distinct()


###############################################################################  
# CASES & DUPLICATES TEST -----------------------------------------------------

TEMP_NOTECASEA <- CONS_MAPPED %>%
                  select(SOURCE,KEYSOURCE,starts_with("CASE")) %>%
                  gather(CASE,CAVEV,-KEYSOURCE,-SOURCE) %>%
                  na.omit()

TEMP_NOTECASEB <- CONS_MAPPED%>%
                  select(SOURCE,KEYSOURCE,starts_with("NOTE")) %>%
                  gather(NOTE,NOTEV,-KEYSOURCE,-SOURCE) %>%
                  na.omit()

TEST_NOTECASE <- TEMP_NOTECASEA %>%
                full_join(TEMP_NOTECASEB) %>%
                group_by(SOURCE,CASE,NOTE) %>%
                summarize(FREQ=n()) %>%
                ungroup()
write.csv(TEST_NOTECASE, "./Tests/TEST_CASESvsNOTES.csv")


TEST_CASES <- CONS_MAPPED %>%
              select(NUMSOURCE, SOURCE, COUNTRY_CODE, AGCOMCODE, AGCOMNAME, YEAR, starts_with("CASE")) %>%
              group_by(NUMSOURCE) %>%
              summarise(A = sum(CASE_A, na.rm=TRUE),
                        B = sum(CASE_B, na.rm=TRUE),
                        C = sum(CASE_C, na.rm=TRUE),
                        D = sum(CASE_D, na.rm=TRUE),
                        E = sum(CASE_E, na.rm=TRUE))
write.csv(TEST_CASES, "./Tests/TEST_CASES.csv")

TEST_CASES_D <- CONS_MAPPED %>%
                select(NUMSOURCE, SOURCE, COUNTRY_CODE, AGCOMCODE, AGCOMNAME, YEAR, starts_with("CASE")) %>%
                filter(!is.na(CASE_A) | !is.na(CASE_B)|!is.na(CASE_C)|!is.na(CASE_D)|!is.na(CASE_E))
write.csv(TEST_CASES_D, "./Tests/TEST_CASES_D.csv")

TEST_DUP_COM <- CONS_MAPPED %>%
                group_by(SOURCE,COUNTRY_CODE, YEAR, AGCOMCODE) %>%
                summarise(count = n()) %>%
                filter(count != 1)
head(TEST_DUP_COM)

CONS_MAPPED %>%
  select(SOURCE,COUNTRY_CODE, YEAR, AGCOMCODE) %>%
  anyDuplicated() %>%
  if (.) stop("Duplicates!")

###############################################################################
# Prioritize Sources ----------------------------------------------------------

CONS_PRIO1 <- CONS_MAPPED %>%
              group_by(COUNTRY_CODE, YEAR) %>%
              summarise(NUMSOURCE = min(NUMSOURCE)) %>%
              ungroup()

CONS_PRIO <- CONS_PRIO1 %>%
              left_join(CONS_MAPPED, by = c("COUNTRY_CODE", "YEAR", "NUMSOURCE")) %>%
              left_join(COUNTRY_A, by = c("COUNTRY_CODE" = "ISO3CODE")) %>%
              mutate(COUNTRYNUM = 1,
                     COMNUM = 1,
                     VP_PROP = PROP * PRODQ,
                     VP_REFP = REFP * PRODQ,
                     NRP = ((VP_PROP/VP_REFP) -1) * 100 )

TEST_DUP_PROD <-  CONS_PRIO %>%
                  group_by(COUNTRY_CODE, YEAR, AGPROCODE) %>%
                  summarise(commodity = n_distinct(AGCOMCODE)) %>%
                  filter(commodity != 1) %>%
                  ungroup()
head(TEST_DUP_PROD)

CONS_PRIO %>%
  select(COUNTRY_CODE, YEAR, AGCOMCODE) %>%
  anyDuplicated() %>%
  if (.) stop("Duplicates!")

#source("Exclusion.R") # there is modification in the exclusion.R file 

#CHECK which source is used for which country
CHECK_SOURCE <- CONS_PRIO %>% select(COUNTRY_CODE, NUMSOURCE) %>% distinct()
View(CHECK_SOURCE)

CHECK_COUNTRYPROD <- CONS_PRIO %>% select(COUNTRY_CODE, AGCOMCODE, AGCOMNAME) %>% distinct()
View(CHECK_COUNTRYPROD)
write.csv(CHECK_COUNTRYPROD, "./Tests/TEST_COUNTRYxPRODUCT.csv")

###############################################################################
# Create Aggregate -----------------------------------------------------
###############################################################################
# Function definiton ---------------------------------------------------------- 
AG_COUNTRY_PRODUCT <- aggregateNRP(CONS_PRIO,COUNTRY_CODE,AGPROCODE,"COUNTRY_PRODUCT",-1)
AG_GLOBAL_TOTAL <- aggregateNRP(CONS_PRIO,,,"GLOBAL_TOTAL",)

AG_INCOME_TOTAL <- aggregateNRP(CONS_PRIO,WBCODE2015,,"INCOME_TOTAL",)
AG_REG_TOTAL <- aggregateNRP(CONS_PRIO,REGIONFAOCODE,,"REG_TOTAL",)
AG_REGINC_TOTAL <- aggregateNRP(CONS_PRIO,COMPOSITE,,"REGINC_TOTAL",)
AG_COUNTRY_TOTAL <- aggregateNRP(CONS_PRIO,COUNTRY_CODE,,"COUNTRY_TOTAL",)

AG_GLOBAL_PRODUCT <- aggregateNRP(CONS_PRIO,,AGPROCODE,"GLOBAL_PRODUCT",)
AG_REGINC_PRODUCT <- aggregateNRP(CONS_PRIO,WBCODE2015,AGPROCODE,"INCOME_PRODUCT",)

AG_GLOBAL_GROUP <- aggregateNRP(CONS_PRIO,,AGGRPCODE,"GLOBAL_GROUP",)
AG_COUNTRY_GROUP <- aggregateNRP(CONS_PRIO,COUNTRY_CODE,AGGRPCODE,"COUNTRY_GROUP",)
AG_REGINC_GROUP <- aggregateNRP(CONS_PRIO,WBCODE2015,AGGRPCODE,"INCOME_GROUP",)

AG_GLOBAL_LVL1 <- aggregateNRP(CONS_PRIO,,LVL1,"GLOBAL_LVL1",5)
AG_REGINC_LVL1 <- aggregateNRP(CONS_PRIO,WBCODE2015,LVL1,"INCOME_LVL1",5)


file.dim <- function() {
  dm <- sapply(ls(envir=globalenv()), function(x)dim(get(x)))
  print(dm)
  class(dm) <- "Dimension"
}

###############################################################################
# Consolidate Categories ------------------------------------------------------

TOTALX <-  mget(ls(pattern="^AG_+")) %>%
           bind_rows()


LIST_CAT <- TOTALX %>% select(CAT) %>% distinct()

TEST_NA <-  TOTALX %>% 
            group_by(YEAR) %>%
            summarise_all(funs(sum(is.na(.)))) %>% 
            bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total"))) %>%
            mutate(TOTAL=rowSums(.[2:16])) %>%
            filter(TOTAL>=1)

# TREATMENT FOR NPC
print("Treatment NPC to track")
View(TEST_NA)

TOTALS <- TOTALX %>% mutate(NPC=ifelse(is.na(NPC),0,NPC)) %>% 
          na.omit() %>%
          filter(YEAR<paste0(YEARMAX)) 


# Quick Analysis ------------------------------------------------------
TOGRAPH <- rbind(AG_GLOBAL_TOTAL,AG_INCOME_TOTAL) %>%
           filter(YEAR<paste0(YEARMAX))

ggplot(data = TOGRAPH, aes(x = factor(YEAR), y = NRP, color = REGION_CODE))+
  geom_line(aes(group = REGION_CODE)) + geom_point()+xlab("")+
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot(data = TOGRAPH, aes(x = factor(YEAR), y = SAVGNRP, color = REGION_CODE))+
  geom_line(aes(group = REGION_CODE)) + geom_point()

TOP_DIST <- AG_COUNTRY_TOTAL %>%
            filter(YEAR<paste0(YEARMAX)) %>%
            mutate(DIST=VP_PROP-VP_REFP,ABSDIST=abs(DIST)) %>% 
            na.omit() %>%
            group_by(YEAR) %>%
            top_n(7,ABSDIST) %>%
            ungroup() 

ggplot(data = TOP_DIST, aes(x=factor(YEAR),y=DIST,fill=REGION_CODE))+
  geom_bar(stat="identity", colour="black")

TOP_PROD <- AG_COUNTRY_TOTAL %>%
            filter(YEAR<paste0(YEARMAX)) %>%
            na.omit() %>%
            group_by(YEAR) %>%
            top_n(10,VP_PROP) %>%
            ungroup() 

ggplot(data = TOP_PROD, aes(x=factor(YEAR),y=VP_PROP,fill=REGION_CODE))+
  geom_bar(stat="identity", colour="black" )

# nrpoldnew <- read_xlsx('NRP-Global and Income Group-Comparison.xlsx', sheet = 'Sheet1')
# 
# ggplot(data = nrpoldnew, aes(x = factor(YEAR), y = NRP_Diff, color = REGION_CODE))+
#   geom_line(aes(group = REGION_CODE)) + geom_point()+
#   ylab("NRP, weighted: update vs web (difference in percentage points)")+
#   theme(axis.title.x = element_blank(), legend.position = 'bottom', legend.title = element_blank(), 
#         axis.text.x = element_text('bold'))
# 
# ggplot(data = nrpoldnew, aes(x = factor(YEAR), y = Simple_diff, color = REGION_CODE))+
#   geom_line(aes(group = REGION_CODE)) + geom_point()+
#   ylab("NRP, simple average: update vs web (difference in percentage points)")+
#   theme(axis.title.x = element_blank(), legend.position = 'bottom', legend.title = element_blank(),
#         axis.text.x = element_text('bold'))


###############################################################################
# Publicview Calculations -----------------------------------------------------
TPUBLICVIEW <-  TOTALS %>%
                mutate(KEYSOURCE2=paste0(NUMSOURCE,REGION_CODE, YEAR, PDCT_CODE))%>%
                left_join(REGION_LABEL) %>%
                left_join(PDCT_LABEL) %>%
                left_join(SOURCEMAP) %>%
                left_join(CONS_MAPPED_NOTES) %>%
                mutate(
                       PRODQ = round(case_when(PRODQ_PHY_UNIT == "INDEX"  ~ VP_PROP , TRUE ~ PRODQ), digits = 4 ),
                       PROP = round(case_when(PRODQ_PHY_UNIT == "INDEX" ~ 1 ,
                                    TRUE ~ VP_PROP/PRODQ), digits = 4 ),
                       REFP = round(case_when(PRODQ_PHY_UNIT == "INDEX"  ~ VP_REFP/VP_PROP, # didn't get it?
                                    TRUE ~ VP_REFP/PRODQ), digits = 4 ),
                       NRP = round(((VP_PROP/VP_REFP) -1) * 100, digits = 4 ),
                       NRPSA = round(SAVGNRP, digits = 4 ),
                       NPC_SOURCE = round(NPC, digits = 4),
                       DIST = round(VP_PROP - VP_REFP, digits = 4 ),
                       VP_PROP = round(VP_PROP, digits = 4),
                       VP_REFP = round(VP_REFP, digits = 4), 
                       MUNIT = "USD",
                       PRODQ_PHY_UNIT= ifelse(NUMSOURCE == 5,"INDEX",paste0(PRODQ_PHY_UNIT)), 
                       NOTES = paste0("[", NOTES,"]"),
                       NOTES = case_when(NUMSOURCE == 5 ~ "[9]",
                                         NOTES == "[]" ~ "[0]", 
                                         TRUE ~ NOTES)) %>%
                select(CAT, SOURCE_VERSION, SOURCE, REGION_CODE, REGION_DESC,
                       YEAR,PRODNUM, COUNTRYNUM, COMNUM, PDCT_CODE, PDCT_DESC,
                       PRODQ, PRODQ_PHY_UNIT,REFP, PROP, VP_PROP, VP_REFP, MUNIT, NRP, SAVGNRP, NPC_SOURCE, DIST, NOTES)

TPUBLICVIEW$COMNUM[is.na(TPUBLICVIEW$COMNUM)]<- ""

# Test Publicview
print("CHECK Nb Rows") 
nrow(TOTALS)-nrow(TPUBLICVIEW)

TPUBLICVIEW %>%
  select(CAT, REGION_CODE, YEAR, PDCT_CODE) %>%
  anyDuplicated() %>%
  if (.) stop("Duplicates!")

TEST_DUP_PVIEW <- TPUBLICVIEW %>%
                  group_by(CAT, REGION_CODE, YEAR, PDCT_CODE) %>%
                  summarise(count = n()) %>%
                  filter(count != 1) %>%
                  left_join(TPUBLICVIEW)


TPUBLICVIEW %>%
 anyNA() %>%
if (.) stop("NAs!")

TEST_NA_TPUBLICVIEW <-TPUBLICVIEW%>% group_by(YEAR)%>%
  summarise_all(funs(sum(is.na(.))))%>% 
  bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

save(TPUBLICVIEW,file="TPUBLICVIEW.Rdata")

# write_csv(TPUBLICVIEW, 'TPUBLICVIEW_new.csv' )



# NRP for select commodities  ---------------------------------------------

commodities <- c('Cattle meat',"Cassava",'Coffee, green','Hen eggs, in shell','Maize','Cow milk, whole, fresh','Palm oil','Pig meat',
                 'Chicken meat','Rice, paddy','Soybeans','Tea','Wheat')
year <- c(2019,2020,2021,2022,2023)

countrylist <- c('ARG','AUS','BRA','CAN','CHE','CHL','CHN','COL','CRI','EUR','GBR','IDN','IND','ISL','ISR', 
                'JPN','KAZ','KOR','MEX','MWI','NOR','NZL','PHL','RUS', 
                'RWA','TUR','UKR','URY','USA','VNM','ZAF')

NRP_select <- CONS_PRIO %>% filter(AGCOMNAME %in% commodities) %>% 
                filter(YEAR %in% year) %>% select(COUNTRY_CODE,AGCOMNAME,YEAR,VP_PROP,VP_REFP) %>% 
                filter(COUNTRY_CODE %in% countrylist) %>% 
                group_by(AGCOMNAME,YEAR) %>% 
                summarise(VP_PROP=sum(VP_PROP,na.rm=T),VP_REFP=sum(VP_REFP,na.rm=T)) %>% ungroup() %>% 
                mutate(NRP = ( (VP_PROP/VP_REFP) -1) * 100) %>% select(AGCOMNAME,YEAR,NRP) %>% 
                spread(YEAR,NRP) %>% 
                mutate(AGCOMNAME=case_when(AGCOMNAME=='Cattle meat'~'Bovine meat', 
                                           AGCOMNAME=='Hen eggs, in shell'~'Eggs', 
                                           AGCOMNAME=='Cow milk, whole, fresh'~'Milk', 
                                           AGCOMNAME=='Chicken meat'~'Poultry meat', 
                                           AGCOMNAME=='Rice, paddy'~'Rice', 
                                           AGCOMNAME=='Coffee, green'~'Coffee',
                                           TRUE~AGCOMNAME
                                           )) %>% 
               arrange(AGCOMNAME)


write.csv(NRP_select, './NRA_Output/Data_for_website/NRP_CommonCountries_commodities_2019_2023.csv', row.names = F)


# countrn <- NRP_select %>% select(COUNTRY_CODE,YEAR) %>% distinct() %>% arrange(YEAR,COUNTRY_CODE) %>% 
#            group_by(YEAR) %>% summarise(countrylist=paste(COUNTRY_CODE,collapse = ", ")) %>% 
#             group_by(YEAR) %>%
#             summarise(col1 = toString(Reduce(intersect, strsplit(countrylist, ','))))

