# -----------------------------------------------------------------------------
  # This is code to create final outputs needed for the Ag-Incentives Consortium: 
  # PublicView, VerificationTool, WorldMap
# -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)

source("AgIncentivesProcessing.R")

dir.create(paste0("./Final/",Sys.Date(), sep = ""))

#Additional Aggregation Level
REGION_WB <- COUNTRY %>%
             select(ISO3CODE,WBCODE2015,WBNAME2015) %>%
             rename(REGION_CODE=ISO3CODE) %>%
             distinct

PDCT_GRP <- COMMODITY_A %>%
            mutate(AGGRPNAME=LVL1) %>% # LVL0 is not found. Instead LVL1 is found. 
            select(AGPROCODE,AGGRPNAME) %>%
            distinct()

# Rename Final Indicators -----------------------------------------------------
PUBLICVIEW <- TPUBLICVIEW %>%
              left_join(REGI_FULL) %>%
              left_join(PDCT_FULL) %>%
                rename(Category = CAT,
                     SourceVersion = SOURCE_VERSION,
                     Source = SOURCE,
                     CountryCode = REGION_CODE, 
                     CountryName = REGION_DESC,
                     WBIncomeGroup = REGION_INCOME,
                     Year = YEAR,
                     NumberProducts = PRODNUM,
                     NumberCountries = COUNTRYNUM,
                     NumberCommodities = COMNUM,
                     ProductCode = PDCT_CODE,
                     ProductName = PDCT_DESC,
                     ProductGroupName = PDCT2,
                     ProductionQuantity = PRODQ,
                     PhysicalUnit = PRODQ_PHY_UNIT,
                     ProducerPriceAtFGL = PROP,
                     ReferencePriceAtFGL = REFP,
                     ValueProduction_PP = VP_PROP,
                     ValueProduction_REF = VP_REFP,
                     MonetaryUnit = MUNIT,
                     NRP = NRP,
                     NRP_SimpleAverage=SAVGNRP,
                     DistortionValue = DIST,
                     NOTES = NOTES) %>%
              mutate(ProductGroupName=ifelse(is.na(ProductGroupName),"Aggregate",paste0(ProductGroupName)),
                     WBIncomeGroup=ifelse(is.na(WBIncomeGroup),"Aggregate",paste0(WBIncomeGroup)),
              ) 
#%>% select(-REGION_ALL,-REGION_DVPT,-REGION_CONTINENT,-PDCT0,-PDCT1)

source("ReportingConsortium.R")

#########################################################
PUBLICVIEW <- PUBLICVIEW %>% select(-NPC_SOURCE)
TABVIEW <-  PUBLICVIEW %>% filter(Category=="COUNTRY_PRODUCT") %>%
            select(-ProductionQuantity,-NRP,-NRP_SimpleAverage,-MonetaryUnit,-ProducerPriceAtFGL,
                   -ReferencePriceAtFGL,-PhysicalUnit,-ProductCode,-ValueProduction_PP,
                   -Category)

readr::write_excel_csv(PUBLICVIEW, paste0("./Final/", Sys.Date(),"/PUBLICVIEW.csv", sep = ""))
readr::write_excel_csv(TABVIEW, paste0("./Final/", Sys.Date(),"/TABVIEW.csv", sep = ""))

VERIFICATIONTOOL <- CONS_MAPPED %>%
                    left_join(REGION_LABEL, by=c('COUNTRY_CODE'='REGION_CODE')) %>%
                    left_join(SOURCEMAP) %>%
                    left_join(CONS_MAPPED_NOTES) %>%
                    select(-starts_with("NOTE_"),-starts_with("CASE_"),-starts_with("KEYSOURCE"))%>%
                    mutate(PROP = round(PROP, digits = 4 ),
                           REFP = round(REFP, digits = 4 ),
                           NRP = round( (PROP/REFP -1) * 100, digits = 4 ),
                           VP_PROP = round(PROP*PRODQ, digits = 4),
                           VP_REFP = round(REFP*PRODQ, digits = 4),
                           DIST = round(VP_PROP - VP_REFP, digits = 4 ),
                           MUNIT = "USD",
                           NOTES = paste0("[",NOTES,"]")) %>%
                          rename(SourceVersion = SOURCE_VERSION,
                           Source = SOURCE,
                           CountryCode = COUNTRY_CODE, 
                           CountryName = REGION_DESC,
                           Year = YEAR,
                           ProductCode = AGCOMCODE,
                           ProductName = AGCOMNAME,
                           ProductionQuantity = PRODQ,
                           PhysicalUnit = PRODQ_PHY_UNIT,
                           ProducerPriceAtFGL = PROP,
                           ReferencePriceAtFGL = REFP)

# Write Final Dated Tables ----------------------------------------------------
readr::write_excel_csv(VERIFICATIONTOOL, paste0("./Final/", Sys.Date(),"/VERIFICATIONTOOL.csv", sep = ""))


# World Map Calculations ------------------------------------------------------
WMAP <- VERIFICATIONTOOL %>%
        select(Source, CountryCode, CountryName, Year) %>%
        mutate(yesno = "Yes") %>%
        distinct() %>%
        spread(Source, yesno, fill = "No")

NEU <- WMAP %>%
        filter(CountryCode != "EUR")

EU <- WMAP %>%
      filter(CountryCode == "EUR") %>%
      select(-CountryCode)

EUC <- read.csv("./Mapping/COUNTRY.csv", stringsAsFactors=FALSE)  %>%
        filter(EU == 1) %>%
        select(ISO3CODE, LISTNAME_EN) %>%
        merge(EU) %>% select(-CountryName) %>% 
        rename(CountryCode = ISO3CODE, CountryName=LISTNAME_EN)

WORLDMAP <- NEU %>%
            bind_rows(EUC)%>%
            mutate(Other_Indicators="None",
                   Other_Indicators=ifelse(FAO=="Yes","Market Development Gaps",paste0(Other_Indicators)),
                   Other_Indicators=ifelse(`Agrimonitor (IDB)`=="Yes","Total Support Estimate (TSE), including Producer Support Estimate (PSE), General Services Support Estimate (GSSE), and Consumer Support Estimate (CSE)",paste0(Other_Indicators)),
                   Other_Indicators=ifelse(OECD=="Yes","Total Support Estimate (TSE), including Producer Support Estimate (PSE), General Services Support Estimate (GSSE), and Consumer Support Estimate (CSE)",paste0(Other_Indicators))) %>% 
            select(-Other_Indicators)

write.csv(WORLDMAP, paste0("./Final/", Sys.Date(),"/WORLDMAP.csv",
                       sep=""), row.names = FALSE)

