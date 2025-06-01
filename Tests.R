
# -------------------------------------------------------------------------------

# This is code to test tables needed for the Ag-Incentives Consortium: 
# Tests on final tables of Ag-Incentives

# Last Updated by Abdullah Mamun in 05/2020

# Source Versions
#   OECD: July 2016
#   MAFAP: December 2016
#   Agrimonitor:  December 2016
#   World Bank (SA): November 2016

# Ag-Incentives Version: Ag-Incentives Consortium (August 2017)

# -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
# library(RMySQL)
library(xlsx)


source("AgIncentivesProcessing.R")
source("FinalFiles.R")

# mydb <- dbConnect(MySQL(), user='tess', password='te20005', dbname='AGINCENTIVES_DEV', host= '209.95.39.223')
# 
# agmapped <- dbReadTable(mydb,"AG_DATAMAPPED_TEST" )
# agmapped_prod <- dbReadTable(mydb,"AG_DATAMAPPED_PROD" )
# PUBLIC <- agmapped_prod <- dbReadTable(mydb,"PUBLICVIEW_08_11_2017")

PUBLIC <- read_csv("./CurrentlyOnline/AgIncentivesNRP.csv")%>%
            mutate(Category=ifelse(Category==trimws("ALL_TOTAL","r"),"GLOBAL_TOTAL",paste0(Category)),
            CountryCode=ifelse(CountryCode==trimws("ALL","r"),"GLOBAL",paste0(CountryCode)),
            ProductCode=ifelse(ProductCode==trimws("TOTAL","r"),"TOTAL",paste0(ProductCode))
                  )

# Compare Versions of Public Views - Updated vs Web  --------------------------
COMPARE0 <- PUBLICVIEW %>%
            filter(Category=="GLOBAL_TOTAL") %>%
            left_join(PUBLIC, by = c("Category" = "Category", "Source" = "Source",
                           "CountryCode" = "CountryCode", "Year" = "Year", 
                           "ProductCode" = "ProductCode"),  suffix = c("_update", "_web"))

COMPARE0_summary <- COMPARE0 %>% select(Source,CountryCode,ProductCode,Year,starts_with("NRP"),starts_with("NumberCou"))

COMPARE1 <- PUBLICVIEW %>%
            filter(Source != "Ag-Incentives Consortium") %>%
            left_join(PUBLIC, by = c("Category" = "Category", "Source" = "Source",
                           "CountryCode" = "CountryCode", "Year" = "Year", 
                           "ProductCode" = "ProductCode"),  suffix = c("_update", "_web")) %>%
            mutate(NumberProducts = round(( NumberProducts_update - NumberProducts_web ), digits= 3),
                   NumberCountries = round(( NumberCountries_update - NumberCountries_web ), digits= 3),
                   DPQ = abs(round((( ProductionQuantity_update - ProductionQuantity_web)/
                                     ProductionQuantity_update)*100, digits= 3)),
                   PhysicalUnit = PhysicalUnit_update == PhysicalUnit_web,
                   DPP = abs(round((( ProducerPriceAtFGL_update - ProducerPriceAtFGL_web )/
                                     ProducerPriceAtFGL_update)*100, digits= 3)),
                   DRP = abs(round((( ReferencePriceAtFGL_update - ReferencePriceAtFGL_web )/
                                       ReferencePriceAtFGL_update)*100, digits= 3)),
                   DVP_PP = abs(round((( ValueProduction_PP_update - ValueProduction_PP_web)/
                                         ValueProduction_PP_update )*100, digits= 3)),
                   DVP_RP = abs(round((( ValueProduction_REF_update - ValueProduction_REF_web )/
                                          ValueProduction_REF_update)*100, digits= 3)),
                   MonetaryUnit = MonetaryUnit_update == MonetaryUnit_web,
                   NRPabsdif = abs( round(( NRP_update - NRP_web), digits= 3 )),
                   NOTES = NOTES_update == NOTES_web) %>%
            rename(ProductName = ProductName_update)

# Compare Versions of Public Views - Web vs Updated Version --------------------

COMPARE2 <- PUBLIC %>%
            filter(Source != "Ag-Incentives Consortium") %>%
            left_join(PUBLICVIEW, by = c("Category" = "Category", "Source" = "Source",
                          "CountryCode" = "CountryCode","Year" = "Year", 
                          "ProductCode" = "ProductCode"), suffix = c( "_web","_update")) %>%
            rename(ProductName = ProductName_web)

# Compare Versions of Public Views - Update vs Web Version --------------------
COMPARE_T <- COMPARE1 %>%
              select(Source, CountryCode, Year, ProductCode, ProductName, starts_with("Value"),
                      starts_with("ProducerPriceAtFGL"), starts_with("ReferencePriceAtFGL"),
                      starts_with("Production"),starts_with("NRP"), starts_with("D"), - starts_with("Dist"))

# Compare Versions of Public Views - When VPs or NRP difference > 5% ----------

COMPARE_D <- COMPARE1 %>%
              mutate(NPC_update=ProducerPriceAtFGL_update/ReferencePriceAtFGL_update,NPC_web=ProducerPriceAtFGL_web/ReferencePriceAtFGL_web)%>%
              filter( DVP_RP > 5 |
                      DVP_PP > 5 |
                      NRPabsdif > 5)%>%
              select(Source, CountryCode, Year, ProductCode, ProductName, starts_with("Value"),
                     starts_with("ProducerPriceAtFGL"), starts_with("ReferencePriceAtFGL"),
                     starts_with("Production"),
                     starts_with("NRP"), starts_with("D"), - starts_with("Dist"))

COMPARE_Quantity <- COMPARE1 %>%
                    filter( DPQ > 50 )%>%
                    select(Source, CountryCode, Year, ProductCode, ProductName,
                           starts_with("Production"))%>%
                    arrange(Source)

#starts_with("Production"), starts_with("ProducerPriceAtFGL"),starts_with("ReferencePriceAtFGL"), 
# ProductName, starts_with("Value"),

# Compare Versions of Public Views - Summary Table of Tests -------------------

COMP_STATS1 <- COMPARE1 %>%
                filter(is.na(ProductionQuantity_web ))%>%
                group_by(Source) %>%
                summarise(NewData = n())
  
COMP_STATS2 <- COMPARE2 %>%
                filter(is.na(ProductionQuantity_update )) %>%
                group_by(Source) %>%
                summarise(MissingData = n())

CASES <- TEST_CASES %>%
          left_join(SOURCEMAP) %>%
          select(-SOURCE_VERSION, -NUMSOURCE) %>%
          rename(Source = SOURCE, Case_A = A, Case_B = B, 
                 Case_C = C, Case_D = D, Case_E = E)
  
COMP_STATS <- COMPARE_D %>%
              select(Source, DVP_RP, DVP_PP, NRPabsdif) %>%
              gather(VAR, VAL, 2:4) %>%
              filter(VAL > 5) %>%
              group_by(Source, VAR) %>%
              summarise(VAL = n()) %>%
              spread(VAR, VAL) %>%
              left_join(COMP_STATS1) %>%
              left_join(COMP_STATS2) %>%
              left_join(CASES)

COMP_NEW <- COMPARE1 %>%
            filter(is.na(ProductionQuantity_web )) %>%
            select(Source, CountryCode, Year, ProductCode, ProductName)

COMP_MISSING <- COMPARE2 %>%
                filter(is.na(ProductionQuantity_update )) %>%
                select(Source, CountryCode, Year, ProductCode, ProductName)

CASES <- TEST_CASES_D %>%
          left_join(COMMODITY_M) %>%
          select(SOURCE, COUNTRY_CODE, AGCOMCODE, AGCOMNAME, YEAR, starts_with("CASE"))

# Compare Versions of Public Views - Write Test Results in csv ----------------

dir.create(paste0("./Tests/",Sys.Date(), sep = ""))


write.csv(COMPARE_T, paste0("./Tests/COMPARE_ALL-", Sys.Date(),
                             ".csv", sep = ""), row.names = FALSE)


write.csv(COMP_STATS, paste0("./Tests/COMP_STATS-", Sys.Date(),
                            ".csv", sep = ""), row.names = FALSE)


write.csv(COMPARE_D, paste0("./Tests/COMPARE_CHANGE-", Sys.Date(),
                            ".csv", sep = ""), row.names = FALSE)

# write.csv(COMPARE_Quantity, paste0("./Tests/COMPARE_CHANGE-", Sys.Date(),
#                                             ".csv", sep = ""), row.names = FALSE)

write.csv(COMP_NEW, paste0("./Tests/COMPARE_NEW-", Sys.Date(),
                            ".csv", sep = ""), row.names = FALSE)

write.csv(COMP_MISSING , paste0("./Tests/COMPARE_MISSING-", Sys.Date(),
                            ".csv", sep = ""), row.names = FALSE)

write.csv(CASES , paste0("./Tests/CASES-", Sys.Date(),
                                ".csv", sep = ""), row.names = FALSE)

write.csv(COMP_MISSING , paste0("./Consortium/Removed-", Sys.Date(),
                                ".csv", sep = ""), row.names = FALSE)

#write.xlsx(PUBLICVIEW, file="Compare-Results-2020-05-07.xlsx", sheetName="Updated Data", row.names=FALSE)

######## Sugar story
Sugar<-SOURCE_IDB%>%
        filter(COMMODITY_CODE=="34" & COUNTRY_CODE=="JAM")%>%
        mutate(coef=(REFP+MPD_SOURCE)/PROP,NPCC=(PROP+MPD_SOURCE)/REFP,
               coef2=(REFP*PNPC_SOURCE)/PROP)%>%
        select(YEAR, contains("NPC"),coef,coef2)

######## cattle story
Cattle<-TPUBLICVIEW%>%filter(YEAR==2014 & CAT=="COUNTRY_GROUP")%>%
        group_by(REGION_DESC,REGION_CODE)%>%
        mutate(ShareProd=VP_PROP/sum(VP_PROP))%>%
                 filter(PDCT_CODE=="AnPr")%>%arrange(ShareProd)
  



  #select(-PREVWEIGHTx)%>%