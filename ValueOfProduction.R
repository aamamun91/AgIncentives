
# -------------------------------------------------------------------------------

# This is code to create the Value of Production needed for the Ag-Incentives Consortium

# Last Updated by Tess Lallemant in 07/2018

# FAOSTAT : Value of Agricultural Production, all countries (and Regions), 
#           all products, Gross Production Value (current milion US$), 
#           2005 - 2014,  http://www.fao.org/faostat/en/#data/QV
#           Version: June 13, 2018  # Previous May 29, 2018

#           Trade - Crops and livestock products, Bulk Download
#           Trade_Crops_Livestock_E_All_Data_(Normalized)
#           http://www.fao.org/faostat/en/#data/TP
#           Version: June 27, 2018  # Previous March 8, 2018

#           Production - Crops - Production Quantity
#           Production_Crops_E_All_Data_(Normalized)
#           http://www.fao.org/faostat/en/#data/QC
#           Version: May 28, 2018  # Previous March 21, 2018

#           Production - Livestock Primary
#           Production_LivestockPrimary_E_All_Data_(Normalized)
#           http://www.fao.org/faostat/en/#data/QL 
#           Version: May 18, 2018  # Previous December 15, 2017
# -------------------------------------------------------------------------------

library(tidyverse)
library(readxl)

VP_SOURCE <- read.csv("./Source/FAOSTAT_data_7-2-2018bis.csv")
TRADE <- read.csv("./Source/Trade_Crops_Livestock_E_All_Data_(Normalized).csv")
PQ_CROP <- read.csv("./Source/Production_Crops_E_All_Data_(Normalized).csv")
PQ_LSTCK <- read.csv("./Source/Production_LivestockPrimary_E_All_Data_(Normalized).csv")

COUNTRY <- read.csv("./Mapping/COUNTRY.csv")
COMMODITY <- read.csv("./Mapping/COMMODITY.csv")

#Value of Production
VP <- VP_SOURCE %>%
      select(Area, Area.Code,  Item.Code, Item, Year, Value)  %>%
      filter(Area.Code < 300 & Year > 2004)

# Total Value of Production by Country Year
TVP <-  VP %>%
        filter(Item.Code == "2051") %>%
        left_join(COUNTRY, by = c("Area.Code" = "FAOCODE")) %>%
        select(ISO3CODE, Year, Value)

# CSV Total Value of Production by Country Year
write.csv(TVP, paste0("./Intermediary/FAO_TOTAL_VP-",Sys.Date(), ".csv", sep = ""), row.names = FALSE)

# Make FAO data table -----------------------------------------------------------

# Production Quantity Livestock
PQ_LSTCK1 <-  PQ_LSTCK %>%
              filter(Element.Code %in% c(5510)) %>%
              rename(PQ = Value)

# Production Quantity Crops
PQ <- PQ_CROP %>%
      filter(Element.Code %in% c(5510)) %>%
      rename(PQ = Value) %>%
      bind_rows(PQ_LSTCK1) %>%
      select(Area.Code, Item.Code, Item, Year, PQ)

# Trade Data
TRADECODE <-  TRADE %>%
              select(Item.Code, Item) %>%
              distinct()

TRADE_DATA <- TRADE %>%
              select(Area.Code, Item.Code, Item, Element.Code, Element, Year, Unit, Value) %>%
              filter(Element.Code == "5910" | Element.Code == "5610") %>%
              na.omit() %>%
              select(-Element.Code, -Unit) %>%
              distinct() %>%
              spread(Element, Value)
 
tabulate(TRADE_DATA$Area.Code)

# Trade Data - Net Trade Status Definition
# Gather Value of Production, Production Quantities, Trade Data

TABLE_COMP <- VP %>%
              rename(VP = Value) %>%
              full_join(TRADE_DATA, by = c("Area.Code", "Item.Code", "Year")) %>%
              full_join(PQ, by = c("Area.Code", "Item.Code", "Year")) %>%
              left_join(COUNTRY, by = c("Area.Code" = "FAOCODE")) %>%
              left_join(COMMODITY, by = c("Item.Code" = "FAOCODE")) %>%
              filter(!is.na(ISO3CODE) , !is.na(AGCOMCODE), Item.Code != "2051", Year > 2004) %>%
              mutate(`Export Quantity`  = case_when(is.na(`Export Quantity`) ~ 0,
                                          !is.na(`Export Quantity`) ~ `Export Quantity`),
                     `Import Quantity`  = case_when(is.na(`Import Quantity`) ~ 0,
                                          !is.na(`Import Quantity`) ~ `Import Quantity`),
                     Traded = case_when((`Export Quantity` + `Import Quantity`) < 0.05 * PQ ~ "non-traded",
                                        (`Export Quantity` + `Import Quantity`) >= 0.05 * PQ ~ "traded"),
                     TradeStatus = case_when(`Export Quantity` - `Import Quantity` <= 0 ~ "iMports",
                                        `Export Quantity` - `Import Quantity` > 0 ~ "eXports")) %>%
              select(ISO3CODE, Year, Item.Code, Item, AGCOMCODE, AGGRPCODE, AGGRPNAME, Traded, TradeStatus, VP,
                     `Export Quantity`, `Import Quantity`, PQ, VP) %>%
              rename(COUNTRY = ISO3CODE, YEAR = Year, ValueProduction = VP,
                     ProductionQuantity = PQ, QExport = `Export Quantity`, QImport = `Import Quantity`,
                     PRODUCT = Item, PRODUCT_CODE = Item.Code)

# Fill in Traded - take last available year
TRADE_STATUS1 <-  TABLE_COMP %>%
                  select(COUNTRY, PRODUCT_CODE, AGCOMCODE, YEAR, Traded) %>%
                  spread(YEAR, Traded) %>%
                  gather(YEAR, Traded, 4:15) %>%
                  arrange(COUNTRY, PRODUCT_CODE, AGCOMCODE, YEAR) %>%
                  group_by(COUNTRY, PRODUCT_CODE, AGCOMCODE) %>%
                  fill(Traded) %>%
                  fill(Traded, .direction = "up")

# Fill in Trade Status - take last available year
TRADE_STATUS <- TABLE_COMP %>%
                select(COUNTRY, PRODUCT_CODE, AGCOMCODE, YEAR, TradeStatus) %>%
                spread(YEAR, TradeStatus) %>%
                gather(YEAR, TradeStatus, 4:15) %>%
                arrange(COUNTRY, PRODUCT_CODE, AGCOMCODE, YEAR) %>%
                group_by(COUNTRY, PRODUCT_CODE, AGCOMCODE) %>%
                fill(TradeStatus) %>%
                fill(TradeStatus, .direction = "up") %>%
                left_join(TRADE_STATUS1) 

test <- TRADE_STATUS %>%
        group_by(COUNTRY, PRODUCT_CODE, AGCOMCODE, YEAR) %>%
        summarise(n())


  # TABLE_COMP_miss <- TABLE_COMP %>%
  #   filter(is.na(AGPROCODE)) %>%
  #   select(Item, Item.Code) %>%
  #   distinct()

# Make China list of products in Fruits and Veg -------------------------------

CHNTRD <- TABLE_COMP %>%
          filter(COUNTRY == "CHN", AGGRPCODE == "F&V") %>%
          select(COUNTRY, YEAR, TradeStatus, PRODUCT, PRODUCT_CODE) %>%
          na.omit()

TABLE_COMP[is.na(TABLE_COMP)] <- ""

# CSV FAO data table ----------------------------------------------------------

write.csv(TABLE_COMP, paste0("./Intermediary/TABLE_FAO_", Sys.Date(),".csv"), row.names = FALSE)
write.csv(CHNTRD, paste0("./Intermediary/CHNTRD_", Sys.Date(),".csv"), row.names = FALSE)
write.csv(TRADE_STATUS, paste0("./Intermediary/TRADE_STATUS_", Sys.Date(),".csv"), row.names = FALSE)

# Tests on VP -----------------------------------------------------------------

VP_GROUP <- TABLE_COMP %>%
            select(COUNTRY, YEAR, AGGRPCODE, ValueProduction) %>%
            group_by(COUNTRY, YEAR, AGGRPCODE) %>%
            summarise(VP = sum(ValueProduction)) %>%
            spread(AGGRPCODE, VP) %>%
            gather(AGGRPCODE, VP,3:8)

VP_GROUP_NA <- VP_GROUP %>%
  filter(is.na(VP))

TVP_RK <- TVP %>%
          arrange(desc(Value)) %>%
          head(100) %>%
          select(ISO3CODE) %>%
          distinct()

  
FAOALLCODE <- VP %>%
              select(Item.Code, Item) %>%
              mutate(Item.Code = as.factor(Item.Code)) %>%
              distinct() %>%
              left_join(COMMODITY, by = c("Item.Code" = "AGCOMCODE"))
  
FAOALLCODE[is.na(FAOALLCODE)] <- ""
write.csv(FAOALLCODE, "./Temp/FAO_CODES.csv", row.names = FALSE)

CODES <- COMMODITY %>%
          mutate(AGPROCODE1 = as.numeric(substring(AGPROCODE,2))) %>%
          left_join(FAOALLCODE, by = c("AGCOMCODE1" = "Item.Code")) %>%
          filter(is.na(Item))


write.csv(FAO, "FAO_CODE.csv")
