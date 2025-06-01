

rm(list=ls())
library(readxl)
library(FAOSTAT)
library(tidyverse)
library(stringr)
source("Mapping.R")
source('VoP_NonMPS_Processing.R')


# Data loading --------------------------------------------------------------------------------
## first, payments data file
PAYMENTS <- read.csv(file="Source/PaymentData_2024/Consolidated_Payment_Data.csv") %>%
            dplyr::filter(Year>=2005) %>%
            mutate(Source=ifelse(Source %in% c("OECD","FAO","IDB"), Source, "OthSources"), 
                   Source=ifelse(Source=='IDB', 'Agrimonitor (IDB)', Source)) %>%
            dplyr::filter(Year<=2023) %>% filter(Source!='OthSources') 

## NRP database, currently online 
NRP_CSV <- read.csv(file="Source/PUBLICVIEW_2024/PUBLICVIEW.csv")%>%
            dplyr::filter(Category=="COUNTRY_PRODUCT")

NRP_DET <- read.csv(file="Source/PUBLICVIEW_2024/PUBLICVIEW.csv")%>%
           dplyr::filter(Category=="COUNTRY_PRODUCT") %>% 
            select('CountryName','CountryCode','ProductName','ProductCode','Year','ValueProduction_PP','ValueProduction_REF','NRP') %>% 
            rename(VoP_FG=ValueProduction_PP, VoP_RP=ValueProduction_REF)

NRP_TOTAL <- read.csv(file="Source/PUBLICVIEW_2024/PUBLICVIEW.csv")%>%
              dplyr::filter(Category=="COUNTRY_TOTAL") %>% 
              select(CountryCode,Year,NRP) 

VoP_NRP <- NRP_CSV%>%
            mutate(Value_PPP=ValueProduction_PP,
                   Value_PRF=ValueProduction_REF,
                   Commodity_Code=ProductCode,
                   Commodity_Label=ProductName)%>%
            select(CountryCode, Year, ProductCode, ProductName, Value_PPP, Value_PRF)
              

# List and product mapping --------------------------------------------------------------------
list_NRP <- NRP_TOTAL %>% select(CountryCode) %>% distinct()%>%flatten() %>% unlist 
list_PAY <- PAYMENTS %>% select(Country_Code) %>% distinct()%>%flatten() %>% unlist 
list.country <- as.list(union(list_NRP, list_PAY))

MPS_PROD_ISO <- NRP_CSV%>%select(CountryCode,ProductCode,Year)%>%distinct()%>%rename(PDCT_CODE=ProductCode)


EU27 <- read.table("EU27.txt",col.names = FALSE)%>%flatten%>%unlist
EU28 <- read.table("EU28.txt",col.names = FALSE)%>%flatten%>%unlist

PDCT_fullmap0 <- PDCT_COMMODITY%>%gather(Source2,Commodity_Code,-AGPROCODE)%>%
                mutate(Source=case_when(Source2=="OECD_CODE"~"OECD",
                                        Source2=="MAFAP_CODE"~"FAO",
                                        Source2=="IADB_CODE"~'Agrimonitor (IDB)'))%>%
                dplyr::filter(nchar(Commodity_Code)>0)%>%select(-Source2)

COMMODITY_L <- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE, fileEncoding = "iso-8859-1")%>%
               select(AGPROCODE,AGPRONAME, IADB, MAFAP,OECD) %>% distinct() %>% 
               gather(Source, Commodity_Label, -c(AGPROCODE, AGPRONAME)) %>% filter(Commodity_Label!='') %>% select(-Source) %>% 
                distinct()

PAY_PROD_ISO <- PAYMENTS %>% select(Country_Code, Commodity_Label, Year) %>% distinct() %>% 
                mutate(Commodity_Label= case_when(Commodity_Label=='Potato'~'Potatoes',
                                                  Commodity_Label=='Palm Oil'~'Palm oil',
                                                  Commodity_Label=='Cashew nuts, with shell'~'Cashew nuts',
                                                  Commodity_Label=='Refined Sugar'~'Refined sugar',
                                                  Commodity_Label=='Common Wheat'~'Wheat',
                                                  Commodity_Label=='Durum Wheat'~'Wheat',
                                                  TRUE~Commodity_Label)) %>% 
                  left_join(COMMODITY_L) %>% 
                  # filter(is.na(AGPROCODE)) %>% select(Commodity_Label) %>% distinct()
                  rename(PDCT_CODE=AGPROCODE, CountryCode=Country_Code) %>% select(CountryCode, PDCT_CODE, Year) %>% distinct()


# Data processing from FAOSTAT ----------------------------------------------------------------

COUNTRYFAO <- FAOcountryProfile%>%select(FAOST_CODE,ISO3_CODE)%>%rename(Area.Code=FAOST_CODE) %>%
              mutate(ISO3_CODE=case_when(Area.Code==277~'SSD',
                                         TRUE~ISO3_CODE)) %>% filter(!is.na(ISO3_CODE))

COUNTRYFAO1 <- FAOcountryProfile%>%select(MOTHER_M49_CODE,ISO3_CODE)%>%rename(Area.Code=MOTHER_M49_CODE) %>% 
                mutate(ISO3_CODE=case_when(Area.Code==728~'SSD',
                             TRUE~ISO3_CODE)) %>%
               mutate(Area.Code=ifelse(ISO3_CODE=='TWN',158, Area.Code)) %>% 
                filter(!is.na(ISO3_CODE))

vop_cols <- c('CountryCode','Item', 'Item.Code', 'element', 'element_code', 'Year', 'VoPFAO')
faostat.vop <- FAOSTAT::read_faostat_bulk(".\\FAOSTAT\\Data_2024\\Value_of_Production_E_All_Data_(Normalized).zip") %>%
                rename(Year=year, VoPFAO=value, Item.Code=item_code, Item=item, Area.Code=area_code) %>% 
                filter(Year>=2005 & Year<=2023) %>% left_join(COUNTRYFAO) %>% rename(CountryCode=ISO3_CODE) 

faostat.vop.allcountry <- faostat.vop %>%
                          select(vop_cols) %>% filter(!is.na(CountryCode))

faostat.vop.eur <- faostat.vop %>% filter(area=='European Union (27)') %>% mutate(CountryCode='EUR') %>%
                   select(vop_cols)

faostat.vop.all <- rbind(faostat.vop.allcountry, faostat.vop.eur)

item.list <- faostat.vop.all %>% select(Item, Item.Code) %>% distinct()

vop.total57 <- faostat.vop.all %>% filter(Item=='Agriculture') %>% filter(element_code==57) %>%
                    filter(CountryCode %in% list.country) %>% select(CountryCode,Year,VoPFAO) %>% filter(CountryCode!='ZWE')

vop.total152 <- faostat.vop.all %>% filter(Item=='Agriculture') %>% filter(element_code==152) %>%
                filter(CountryCode %in% c("UGA", "GTM", "HTI", "BHS", "ZWE")) %>%
                select(CountryCode,Year,VoPFAO)

vop.total <- rbind(vop.total57, vop.total152) %>% rename(VoPFAO_TOTAL = VoPFAO) %>% mutate(VoPFAO_TOTAL=VoPFAO_TOTAL*1000)

# faostat.vop2021.total <- read.csv('./FAOSTAT/FAOSTAT_data_en_3-13-2023 (1).csv')

# faostat.vop2021 <- read.csv('./FAOSTAT/Data_2023/FAOSTAT_data_en_3-13-2023.csv') %>% 
#                    rbind(read.csv('./FAOSTAT/Data_2023/FAOSTAT_data_en_3-13-2023 (1).csv')) %>% 
#                    rename(VoPFAO=Value, Area.Code="Area.Code..M49.", element=Element, 
#                           element_code=Element.Code) %>% 
#                     left_join(COUNTRYFAO1) %>% rename(CountryCode=ISO3_CODE) %>% 
#                     select(Area, 'CountryCode','Item', 'element', 'element_code', 'Year', 'VoPFAO') %>% 
#                     filter(!is.na(CountryCode)) %>% 
#                   left_join(item.list) %>% select(vop_cols)
# 
# faostat.vop <- rbind(faostat.vop2023, faostat.vop2021)


VoP_FAOSTAT1 <- faostat.vop %>% filter(element_code==57) %>% select(CountryCode, Item, Item.Code, Year, VoPFAO) %>% 
                filter(!is.na(CountryCode)) %>% filter(Item.Code<=1500)

# VoP_FAOSTAT.EUR <- faostat.vop %>% filter(element_code==57) %>% filter(area=='European Union (27)')  %>% mutate(CountryCode='EUR') %>% 
#                    select(CountryCode, Item, Item.Code, Year, VoPFAO) 

# VoP_FAOSTAT1 <- rbind(VoP_FAOSTAT_All, VoP_FAOSTAT.EUR)
  

item.itemcode <- VoP_FAOSTAT1 %>% select(Item, Item.Code) %>% distinct()

# meat.noteur <- VoP_FAOSTAT1 %>% filter(Item.Code<=1500) %>% 
#                 dplyr::filter(grepl("Meat", Item)) %>% filter(CountryCode!='EUR')
# 
# meat.eur <- VoP_FAOSTAT1 %>% filter(Item.Code<=1500) %>% 
#             dplyr::filter(grepl("Meat", Item)) %>% filter(CountryCode=='EUR') %>% 
#             mutate (TypeMeat= case_when( str_detect(Item, "Meat of pig with") ~ "PIG",
#                                         str_detect(Item, "cattle") ~ "CATTLE",
#                                         str_detect(Item, "other domestic camelids") ~ "CAMELIDS",
#                                         str_detect(Item, "camels") ~ "CAMEL",
#                                         str_detect(Item, "chickens") ~ "CHICKEN",
#                                         str_detect(Item, "buffalo") ~ "BUFFALO",
#                                         str_detect(Item, "goat") ~ "GOAT",
#                                         str_detect(Item, "sheep") ~ "SHEEP",
#                                         str_detect(Item, "rabbits") ~ "RABBIT",
#                                         str_detect(Item, "turkeys") ~ "TURKEY",
#                                         str_detect(Item, "asses") ~ "ASS",
#                                         str_detect(Item, "mules") ~ "MULE",
#                                         str_detect(Item, "ducks") ~ "DUCK",
#                                         str_detect(Item, "geese") ~ "GEESE",
#                                         str_detect(Item, "pigeons") ~ "BIRD NES",
#                                         TRUE ~ "OTHER"),
#               Indigenous= ifelse ( grepl("indigenous",Item) ,"INDIGENOUS", "NOT_INDIGENOUS") ) %>% 
#               group_by(CountryCode, Year, TypeMeat,Indigenous) %>%
#               summarise(tVoP=sum(VoPFAO, na.rm=TRUE)) %>% ungroup() %>%
#               spread(Indigenous,tVoP) %>% ungroup() %>% mutate(indigenous_high=ifelse(NOT_INDIGENOUS<=INDIGENOUS, 1, 0), 
#                                    indigenous_equal =ifelse(NOT_INDIGENOUS==INDIGENOUS, 1, 0) ) 


# write.csv(meat, 'QualityCheckFiles/Meat_Indigenous_vs_Not_Indigenous.csv', row.names = F)

# meat.codes <- VoP_FAOSTAT1 %>% filter(Item.Code<=1500) %>% dplyr::filter(grepl("Meat", Item)) %>% 
#               select(Item, Item.Code) %>% distinct() %>% mutate (
#                 TypeProduct= case_when( grepl("Meat of pig with",Item) ~ "PIG",
#                                      grepl("cattle",Item) ~ "CATTLE",
#                                      grepl("other domestic camelids",Item) ~ "CAMELIDS",
#                                      grepl("camels",Item) ~ "CAMEL",
#                                      grepl("chickens",Item) ~ "CHICKEN",
#                                      grepl("buffalo",Item) ~ "BUFFALO",
#                                      grepl("goat",Item) ~ "GOAT",
#                                      grepl("sheep",Item) ~ "SHEEP",
#                                      grepl("rabbits",Item) ~ "RABBIT",
#                                      grepl("turkeys",Item) ~ "TURKEY",
#                                      grepl("asses",Item) ~ "ASS",
#                                      grepl("mules",Item) ~ "MULE",
#                                      grepl("ducks",Item) ~ "DUCK",
#                                      grepl("geese",Item) ~ "GEESE",
#                                      grepl("pigeons",Item) ~ "BIRD NES",
#                                      TRUE ~ "OTHER"),
#                 Indigenous= ifelse ( grepl("indigenous",Item) ,"INDIGENOUS", "NOT_INDIGENOUS") ) 
#    
# 
# sel_cols <- c('CountryCode', 'Item', 'Item.Code', 'Year', 'VoPFAO')
# meat.indi.codes <- meat.codes %>% filter(Indigenous=='INDIGENOUS') %>% select(-Indigenous)
# meat.notindi.codes <- meat.codes %>% filter(Indigenous=='NOT_INDIGENOUS') %>% select(-Indigenous)

# write.csv(meat.code.touse, "Mapping/Meat_Indigenous_Codes.csv", row.names = F)

# meat.eur.indi <- meat.eur %>% filter(indigenous_high==1) %>% mutate(VoPFAO=INDIGENOUS) %>% 
#                  left_join(meat.indi.codes, by=c('TypeMeat'='TypeProduct')) %>% 
#                  select(sel_cols)
# 
# meat.eur.notindi <- meat.eur %>% filter(indigenous_high!=1) %>% mutate(VoPFAO=NOT_INDIGENOUS) %>% 
#                     select(CountryCode,Year,TypeMeat,VoPFAO) %>% 
#                     left_join(meat.notindi.codes, by=c('TypeMeat'='TypeProduct')) %>% 
#                       select(sel_cols)
# 
# meat.missing <- meat %>% filter(is.na(indigenous_high)) %>% filter(!TypeMeat %in% c('OTHER', 'RODENTS')) %>% 
#                 mutate(Type = ifelse(is.na(INDIGENOUS), 'NOT_INDIGENOUS', 'INDIGENOUS'),
#                   VoPFAO=ifelse(is.na(INDIGENOUS), NOT_INDIGENOUS, INDIGENOUS)) %>% 
#                 left_join(meat.codes, by=c('TypeMeat'='TypeProduct', 'Type'='Indigenous')) %>% select(sel_cols)

# meat.other <- VoP_FAOSTAT1 %>% filter(CountryCode=='EUR') %>% filter(Item.Code %in% c(1163, 1073, 1166, 1151)) %>% select(sel_cols)

# meat.eur.f <- rbind(meat.eur.indi, meat.eur.notindi) 
# 
# meat_57_f <- rbind(meat.noteur,meat.eur.f)

# duplcte <- VoP_FAOSTAT.meat %>% group_by(CountryCode,Year,Item.Code) %>% summarise(n=n()) %>% filter(n==2)

# nonmeat_57 <- VoP_FAOSTAT1 %>% filter(Item.Code<=1500) %>% dplyr::filter(!grepl("Meat", Item)) %>% 
#               select(sel_cols) 
# 
# VoP_FAOSTAT1.f <- rbind(nonmeat_57, meat_57_f)


available.countries <- VoP_FAOSTAT1 %>% select(CountryCode,Item, Item.Code,Year) %>% distinct() %>% 
                       mutate(TypeProduct= case_when( grepl("Meat of pig with",Item) ~ "PIG",
                                                      grepl("Meat of cattle",Item) ~ "CATTLE",
                                                      grepl("other domestic camelids",Item) ~ "CAMELIDS",
                                                      grepl("Meat of camels",Item) ~ "CAMEL",
                                                      grepl("chickens",Item) ~ "CHICKEN",
                                                      grepl("horse",Item) ~ "HORSE",
                                                      grepl("Meat of buffalo",Item) ~ "BUFFALO",
                                                      grepl("goat",Item) ~ "GOAT",
                                                      grepl("Meat of sheep",Item) ~ "SHEEP",
                                                      grepl("rabbits",Item) ~ "RABBIT",
                                                      grepl("turkeys",Item) ~ "TURKEY",
                                                      grepl("asses",Item) ~ "ASS",
                                                      grepl("mules",Item) ~ "MULE",
                                                      grepl("ducks",Item) ~ "DUCK",
                                                      grepl("geese",Item) ~ "GEESE",
                                                      grepl("pigeons",Item) ~ "BIRD NES",
                                                      grepl("rodents",Item) ~ "RODENTS",
                                                      TRUE ~ Item)
                              ) %>%
                        select(-Item, -Item.Code) %>%
                        mutate(VoPUSD=1)
  

VoP_FAOSTAT2 <- faostat.vop %>% filter(element_code==152) %>% filter(Item.Code<=1500) %>% filter(!is.na(CountryCode)) %>% 
                mutate(TypeProduct= case_when( grepl("Meat of pig with",Item) ~ "PIG",
                                               grepl("Meat of cattle",Item) ~ "CATTLE",
                                               grepl("other domestic camelids",Item) ~ "CAMELIDS",
                                               grepl("Meat of camels",Item) ~ "CAMEL",
                                               grepl("chickens",Item) ~ "CHICKEN",
                                               grepl("horse",Item) ~ "HORSE",
                                               grepl("Meat of buffalo",Item) ~ "BUFFALO",
                                               grepl("Meat of goat",Item) ~ "GOAT",
                                               grepl("Meat of sheep",Item) ~ "SHEEP",
                                               grepl("rabbits",Item) ~ "RABBIT",
                                               grepl("turkeys",Item) ~ "TURKEY",
                                               grepl("asses",Item) ~ "ASS",
                                               grepl("mules",Item) ~ "MULE",
                                               grepl("ducks",Item) ~ "DUCK",
                                               grepl("geese",Item) ~ "GEESE",
                                               grepl("pigeons",Item) ~ "BIRD NES",
                                               grepl("rodents",Item) ~ "RODENTS",
                                               TRUE ~ Item)) %>%
                left_join(available.countries) %>% 
                dplyr::filter(is.na(VoPUSD)) %>% select(-VoPUSD) %>% 
                select(CountryCode, Item, Item.Code, Year, VoPFAO)


# item.fao2 <- VoP_FAOSTAT2 %>% select(Item, Item.Code) %>% distinct()

# meat.152 <- VoP_FAOSTAT2 %>% dplyr::filter(grepl("Meat", Item)) %>% select(CountryCode, Item, Item.Code, Year, VoPFAO)


  # rbind(meat.missing.152, meat.other.152) %>% distinct()
# 
# nonmeat.152 <- VoP_FAOSTAT2 %>% filter(Item.Code<=1500) %>% dplyr::filter(!grepl("Meat", Item)) %>% select(sel_cols)
# 
# VoP_FAOSTAT2.f <- rbind(nonmeat.152, meat.152)

VoP_FAOSTAT <- rbind(VoP_FAOSTAT1,VoP_FAOSTAT2) %>% mutate(VoPFAO=VoPFAO*1000) 

# duplcte2 <- VoP_FAOSTAT %>% group_by(CountryCode,Year,Item.Code) %>% summarise(n=n()) %>% filter(n==2)

vop.testimate <- VoP_FAOSTAT %>% group_by(CountryCode, Year) %>% summarise(VoPFAO_est = sum(VoPFAO, na.rm = T)) %>% ungroup() %>% 
                 mutate(Item='Agriculture', Item.Code=2051) %>% 
                 mutate(CountryCode=ifelse( CountryCode %in% EU27, "EUR",CountryCode)) %>% 
                 group_by(CountryCode, Item, Item.Code, Year) %>% 
                 summarise(VoPFAO_est=sum(VoPFAO_est, na.rm = T)) %>% ungroup()

## Check for total agricultural production 
faostat.vop.57 <- faostat.vop %>% filter(Item.Code==2051) %>% filter(element_code==57) %>% 
                  mutate(VoPFAO=VoPFAO*1000) %>% select(-element, -element_code) 

country.total <- faostat.vop.57 %>% select(CountryCode, Year) %>% distinct() %>% mutate(totalexist =1)

faostat.vop.152 <- faostat.vop %>% filter(Item.Code==2051) %>% filter(element_code==152) %>% left_join(country.total) %>% 
                    filter(is.na(totalexist)) %>% 
                    mutate(VoPFAO=VoPFAO*1000) %>% select(-element, -element_code, -totalexist) 
  
faostat.vop.total <- rbind(faostat.vop.57, faostat.vop.152) %>% 
                      mutate(CountryCode=ifelse( CountryCode %in% EU27, "EUR",CountryCode)) %>% group_by(CountryCode, Item, Item.Code, Year) %>% 
                      summarise(VoPFAO=sum(VoPFAO, na.rm = T)) %>% ungroup() %>% 
                      right_join(vop.testimate) %>% mutate(factor_prod = VoPFAO/VoPFAO_est) %>% 
                      select(CountryCode, Year, factor_prod)
                      
VoP_FAOSTAT.rescaled <- VoP_FAOSTAT %>% mutate(CountryCode=ifelse( CountryCode %in% EU27, "EUR",CountryCode)) %>% 
                        group_by(CountryCode, Item, Item.Code, Year) %>% 
                        summarise(VoPFAO=sum(VoPFAO, na.rm = T)) %>% ungroup() %>% 
                          left_join(faostat.vop.total) %>% mutate(VoPFAO=VoPFAO*factor_prod) %>% select(-factor_prod)

vop.fao.total <- VoP_FAOSTAT.rescaled %>% group_by(CountryCode,Year) %>% summarise(VoPt = sum(VoPFAO))


# Imputation of VoP data for 2022 -----------------------------------------

# VoP_growth <- VoP_FAOSTAT.rescaled %>% filter(Year %in% c(2019,2020,2021,2022)) %>% filter(VoPFAO!=0) %>% 
#               group_by(CountryCode, Item, Item.Code) %>% 
#               mutate(vopfao_l = lag(VoPFAO,1), VoPFAO_growth = log(VoPFAO) - lag(log(VoPFAO),1) ) %>% ungroup() %>% 
#               group_by(CountryCode, Item, Item.Code) %>% 
#               summarise(vop_growth = mean(VoPFAO_growth, na.rm = T)) %>% ungroup()

# vop_growth_ave <- VoP_growth
# 
# save(vop_growth_ave, file= './FAOSTAT/VoP_Average_Growth.Rdata')
# 
# load(file= './FAOSTAT/VoP_Average_Growth.Rdata')


# vop_2023 <- VoP_FAOSTAT.rescaled %>% filter(Year==2022) %>% left_join(VoP_growth) %>% 
#             mutate(VoPFAO = VoPFAO*(1+vop_growth), Year=2023 ) %>% select(-vop_growth)
# 
# VoP_FAOSTAT.imputed <- VoP_FAOSTAT.rescaled %>% rbind(vop_2023)


list.products.fao1 <- VoP_FAOSTAT1 %>% select(Item.Code, Item) %>% distinct
list.products.fao2 <- VoP_FAOSTAT2 %>% select(Item.Code, Item) %>% distinct
list.products.fao <- VoP_FAOSTAT.rescaled %>% select(Item.Code, Item) %>% distinct


# Country-Product-Year tuples -----------------------------------------------------------------

list.code.agInc <- PDCT_FULL %>% mutate(Item.Code=as.numeric(substr(PDCT_CODE,2,7)),
                                      Item.Code=case_when(PDCT_CODE=="c866" ~ 867,
                                                          PDCT_CODE=="c250" ~ 249,
                                                          PDCT_CODE=="c564" ~ 560,
                                                          PDCT_CODE=="c1999" ~ 260,
                                                          PDCT_CODE=="c258" ~ 256,
                                                          PDCT_CODE=="c2005" ~ 108,
                                                          # PDCT_CODE=="c328" ~ 767, # here is a problem 
                                                          # PDCT_CODE=="c254" ~ 257,
                                                          TRUE ~ Item.Code)) %>%
                    left_join(list.products.fao) %>% left_join(PDCT_LABEL)

## aggregate products such as GCT, ACT etc. 
product.aggregate <- readxl::read_xlsx("./Mapping/Aggregate_Commodity.xlsx", sheet = 'AG_COM_Long' )

# FandV.FAO <- list.products.fao%>%dplyr::filter(Item.Code>=449 & Item.Code<=619)

list.code.agInc.missing <- list.code.agInc%>%dplyr::filter(is.na(Item)) %>% 
                            left_join(product.aggregate, by=c('PDCT_DESC'='AGCOMNAME')) %>% 
                           filter(is.na(Item.y)) %>% select(-Item.y, -Item_Code) %>% rename(Item=Item.x)


# fv.match <- product.aggregate %>% filter(AGCOMNAME=='Fruits and vegetables') %>% left_join(FandV.FAO) %>% filter(is.na(Item.Code))

add.code.agInc <- data.frame(PDCT_CODE=c("c866","c866","c866","c1058","c1035","c1035","c977","c977","c1015","c1015","c1015","c1017","c1017","c1058","c866", 
                                         "c882","c882","c882","c882"),
                             Item.Code=c(867,947,944,1058,1035,1055,977,1012,1015,1016,1020,1032,1017,1094, 972, 1020, 982, 951, 1130))

list.code.aggregate <- product.aggregate %>% 
                       mutate(AGCOMNAME=case_when(AGCOMNAME=='Oilseeds + (Total)'~'Oilseeds', 
                                                  AGCOMNAME=='Bovine Meat + (Total)'~'Bovine meat', 
                                                  AGCOMNAME=='Poultry Meat + (Total)'~'Poultry meat',
                                                  AGCOMNAME=='Sugar,Total (Raw Equiv.) + (Total)'~'Sugar', 
                                                  TRUE~AGCOMNAME)) %>% 
                      left_join(PDCT_LABEL, by=c('AGCOMNAME'='PDCT_DESC')) %>% filter(!(PDCT_CODE %in% c('HORT', 'NAL', 'GRN'))) %>% 
                      filter(!is.na(PDCT_CODE)) %>% select(PDCT_CODE, Item_Code) %>% rename(Item.Code=Item_Code)
                      
  
list.code.agInc.0 <- list.code.agInc %>% select(PDCT_CODE,Item.Code) %>% filter(Item.Code<=30000)

list.code.agInc.f <- rbind(list.code.agInc.0, list.code.aggregate, add.code.agInc)%>% distinct() %>% 
                     filter(!(Item.Code %in% c(26999, 1899))) 

# poultry.list <- c(1058,1069,1073,1080,1089,1070,1084,1087,1094)

list.MPS <- merge(MPS_PROD_ISO,list.code.agInc.f, by=c("PDCT_CODE")) %>% mutate(MPS=1) 
            # %>% 
            # filter(!Item.Code %in% c(1070, 1084, 1087)) %>%
            # filter(!(PDCT_CODE=='c866' & (Item.Code %in% c(947, 972)) ) )
list.MPS.smp <- list.MPS %>% select(Year,CountryCode,Item.Code,MPS) %>% distinct()

# list.code.mps <- list.MPS %>% select(-PDCT_CODE, -Year) %>% distinct()

list.payment <- PAY_PROD_ISO %>% left_join(list.MPS) %>% distinct() %>% filter(is.na(MPS)) %>% 
                select(CountryCode, PDCT_CODE, Year) %>% mutate(NotinNRP=1) %>% 
                left_join(list.code.agInc.f) %>% filter(!is.na(Item.Code))

list.payment.smp <- list.payment %>% select(Year,CountryCode,Item.Code,NotinNRP)%>% distinct()


# VoP_FAOSTAT.Ag <- VoP_FAOSTAT%>%dplyr::filter(Item.Code %in% c(2041,2044,2051))

# Value of production aligning: NRP and non NRP commodities -----------------------------------

VoP_FAOSTAT.MPS <- VoP_FAOSTAT.rescaled %>%dplyr::filter(Item.Code <=1500) %>%
                    right_join(list.MPS) %>% group_by(CountryCode,PDCT_CODE, Year, MPS) %>% 
                    summarize(VoPFAO=sum(VoPFAO, na.rm = T)) %>% ungroup() %>% mutate(VoPFAO=ifelse(VoPFAO==0, NA, VoPFAO)) 


VoP_MPS_align <- NRP_DET %>% left_join(list.code.agInc.0, by=c('ProductCode'='PDCT_CODE')) %>% 
                  mutate(Item.Code=case_when(ProductCode=='cIFCHN'~'IFCHN', 
                                             ProductCode=='cXFCHN'~'XFCHN', 
                                             TRUE~as.character(Item.Code))) %>% 
                  left_join(VoP_FAOSTAT.MPS, by=c('CountryCode'='CountryCode', 'ProductCode'='PDCT_CODE','Year'='Year')) %>% 
                  mutate(PDCT_CODE=paste0('c',Item.Code)) %>% 
                  mutate(VoPFAO=ifelse(is.na(VoPFAO), 1e-3, VoPFAO), gamma_fg=VoP_FG/VoPFAO, 
                         gamma_rp = gamma_fg*VoP_RP/VoP_FG, 
                         VoP_FG_hat = gamma_fg*VoPFAO, VoP_RP_hat = gamma_rp*VoPFAO)  
                  
  
VoP_MPS_align.f <- VoP_MPS_align %>% select(CountryCode, Item.Code, Year, MPS, VoP_FG_hat, VoP_RP_hat)

vop_mps_compare <- VoP_MPS_align %>% mutate(equality_FG =VoP_FG/VoP_FG_hat, equality_RP=VoP_RP/VoP_RP_hat)

# write.csv(vop_mps_compare, 'QualityCheckFiles/VoP_MPS_Alignment.csv', na='', row.names = F)

VoP_FAOSTAT.nMPS <- VoP_FAOSTAT.rescaled %>% dplyr::filter(Item.Code <=1500) %>% 
                    left_join(list.MPS.smp) %>% filter(is.na(MPS)) %>% filter(CountryCode %in% list.country) %>% 
                    mutate(MPS=0) 

theta <- VoP_FAOSTAT.nMPS %>% group_by(CountryCode, Year) %>% summarise(VoPFAO=sum(VoPFAO, na.rm=T)) %>% 
         left_join(XE.FINAL, by=c('CountryCode'='COUNTRY_CODE', 'Year'='YEAR')) %>% 
         mutate(theta_fg=VP_XE/VoPFAO, theta_fg=ifelse(is.na(theta_fg), 1, theta_fg) ) %>% 
         left_join(NRP_TOTAL %>% select(CountryCode, Year, NRP)) %>% 
         mutate(NRP=NRP/100, NRP=ifelse(is.na(NRP), 0, NRP), theta_rp=abs(theta_fg/(1+NRP))) %>% 
         select(-VoPFAO, -VP_XE, -NRP)

theta_dist <- theta %>% filter(theta_fg<=0.2 | theta_fg>=1.5)

# che_mps <- VoP_MPS_align %>% filter(CountryCode=='CHE') %>% filter(Year==2005)



# phl.nonmps <- theta %>% filter(CountryCode=='PHL')
# 
# phl.fao <- VoP_FAOSTAT.rescaled %>% filter(CountryCode=='PHL') %>% group_by(CountryCode, Year) %>%
#            summarise(VoPFAO_T = sum(VoPFAO, na.rm = T)) %>% ungroup() %>%
#            select(CountryCode, Year, VoPFAO_T)
# 
# phl.nmps.fao <- VoP_FAOSTAT.nMPS %>% filter(CountryCode=='PHL')
# 
# phl.total.oecd <- XE.TOTAL %>% filter(COUNTRY_CODE=='PHL') %>%
#                   left_join(phl.nonmps, by=c('COUNTRY_CODE'='CountryCode', 'YEAR'='Year')) %>% select(-XE, -NONMPS) %>%
#                   left_join(phl.fao, by=c('COUNTRY_CODE'='CountryCode', 'YEAR'='Year'))
# 
# 
# write.csv(phl.total.oecd, 'QualityCheckFiles/Philippines_theta.csv')

# jpn <- theta %>% filter(CountryCode=='JPN')
# write.csv(jpn, 'QualityCheckFiles/Japan_NonMPS_Alignment_Issue.csv', na='', row.names = F)

VoP_NMPS_align <- VoP_FAOSTAT.nMPS %>% left_join(theta) %>% mutate(VoP_FG_hat=theta_fg*VoPFAO, VoP_RP_hat=theta_rp*VoPFAO) %>% 
                  select(CountryCode, Item.Code, Year, MPS, VoP_FG_hat, VoP_RP_hat)

# che_nmps <- VoP_NMPS_align %>% filter(CountryCode=='CHE') %>% filter(Year==2005)
# 
# write.csv(che_nmps, './Consortium/Switzerland_VP_Non_MPS_Alignment.csv', row.names = F)

VoP_NMPS_compare <- VoP_NMPS_align %>% group_by(CountryCode, Year) %>% 
                    summarise(VoP_FG_hat=sum(VoP_FG_hat, na.rm = T), VoP_RP_hat=sum(VoP_RP_hat, na.rm = T)) %>% 
                    left_join(XE.FINAL, by=c('CountryCode'='COUNTRY_CODE', 'Year'='YEAR')) %>% 
                    mutate(equality.ratio_FG = VP_XE/VoP_FG_hat)

# write.csv(VoP_NMPS_compare, 'QualityCheckFiles/VoP_NonMPS_Alignment.csv', na='', row.names = F)
## Combining value of production for NRP and non NRP commodities
VoP.Complete <- VoP_MPS_align.f %>% rbind(VoP_NMPS_align) %>% arrange(CountryCode, Item.Code, Year) %>% 
                mutate(RelevantValueProduction=VoP_RP_hat) %>% select(-MPS) %>% 
                mutate(Item.Code=ifelse(Item.Code==867, 866, ifelse((CountryCode=='UGA' & Item.Code==1032), 1017, Item.Code) )) 
                # %>% 
                # filter(Year<2021) # FAOSTAT vop data not available for 2021 

## Computing VoP for aggregate as well as non NRP commodities 
VoP.NotinNRP <- VoP.Complete %>% right_join(list.payment %>% select(-NotinNRP) %>% 
                                               mutate(Item.Code=as.character(ifelse(Item.Code==867, 866, Item.Code))) ) %>% 
                 group_by(CountryCode, PDCT_CODE, Year) %>% 
                 summarise(VoP_FG_hat=sum(VoP_FG_hat, na.rm = T), VoP_RP_hat=sum(VoP_RP_hat, na.rm = T)) %>% ungroup() %>% 
                 mutate(VoP_FG_hat=ifelse(VoP_FG_hat==0, NA, VoP_FG_hat), VoP_RP_hat=ifelse(VoP_RP_hat==0, NA, VoP_RP_hat)) %>% 
                 mutate(RelevantValueProduction2=VoP_RP_hat) %>% filter(!is.na(RelevantValueProduction2)) 
                #   %>% 
                # filter(Year<2021) # FAOSTAT vop data not available for 2021 


vop_null <- VoP.NotinNRP %>% filter(is.na(VoP_RP_hat))


VoP.mapped <- VoP.Complete %>% left_join(COUNTRY, by = c("CountryCode"="ISO3CODE")) %>% 
              mutate(AGPROCODE=paste0('c', Item.Code), Item_Code=as.integer(Item.Code), 
                    CATPROD = case_when(Item_Code<=850~'CRP', 
                                        (Item_Code>850 & Item_Code<1500)~'LVS', 
                                        (Item_Code %in% c(26999, 29000, 30000))~'CRP', 
                                         is.na(Item_Code)~'CRP', 
                                         TRUE~as.character(Item_Code))) %>% select(-Item_Code)


# write.csv(VoP.world.allag, 'QualityCheckFiles/VoP_World_AllAg.csv', row.names = F)

VoP_MPS.ag <- VoP_MPS_align.f %>% mutate(Item_Code=as.integer(Item.Code), CATPROD = case_when(Item_Code<=850~'CRP',
                                                             (Item_Code>850 & Item_Code<1500)~'LVS',
                                                             (Item_Code %in% c(26999, 29000, 30000))~'CRP',
                                                             is.na(Item_Code)~'CRP',
                                                             TRUE~as.character(Item_Code))) %>%
              group_by(CountryCode, CATPROD, Year) %>%
              summarise(Production_MPS=sum(VoP_RP_hat)) %>% ungroup()

# VoP_NMPS_check  <- VoP.country.sector %>% left_join(VoP_MPS.ag) %>% na.omit() %>% mutate(VoP_NMPS=RelevantValueProduction-Production_MPS) %>%
#                     select(-RelevantValueProduction, -Production_MPS)

VoP_NMPS.crplvs <- VoP_NMPS_align %>% 
                  mutate(CATPROD = case_when(Item.Code<=850~'CRP', 
                                            (Item.Code>850 & Item.Code<1500)~'LVS', 
                                            TRUE~as.character(Item.Code))) %>% 
                  group_by(CountryCode, CATPROD, Year) %>% 
                  summarise(RelevantValueProduction=sum(VoP_RP_hat)) %>% ungroup() %>% 
                  left_join(NRP_TOTAL) %>% na.omit() %>% mutate(Support_USD=RelevantValueProduction*NRP/100) %>% select(-NRP)


# vop.crplvs.check <- VoP_NMPS.crplvs %>% select(-Support_USD) %>% 
#                       group_by(CountryCode, Year) %>% summarise(VoP=sum(RelevantValueProduction))
# 
# vop.nmps.check <- VoP.NotinNRP %>% 
#                     filter(PDCT_CODE %in% c('c300001','c300002','c300003','c300004','c300005','c300006','c300007','c300008','c300009','c300010',
#                                              'c300011','c300012','c300013','c300014','c300015','c300016','c300017','c300018','c300019',
#                                             'c300020','c300021','c300022','c300023','c300024','c300025','c300026','c300027','c300028')) %>% 
#                   left_join(vop.crplvs.check) %>% mutate(diff=RelevantValueProduction2-VoP)
# 
# write.csv(vop.nmps.check, 'vop_nmps_check.csv')
                
       
# arg.item.nmps <- VoP_NMPS.crplvs %>% filter(CountryCode=='USA') %>% filter(Year==2010) %>% select(Item.Code) %>% distinct()
# arg.c300001 <- list.payment %>% filter(CountryCode=='USA') %>% filter(Year==2010) %>% filter(PDCT_CODE=='c300025') %>%
#                 select(Item.Code) %>% distinct()
  
# write.csv(VoP.mapped, 'QualityCheckFiles/VoP_Mapped.csv')

## dealing with wine commodity, non-existent in FAOSTAT
wine <- VoP.Complete %>% filter(CountryCode=='EUR' & Item.Code==560) %>%  
        mutate(PDCT_CODE='c564') %>% select(CountryCode, PDCT_CODE, Year, RelevantValueProduction) %>% 
        rename(RelevantValueProduction3=RelevantValueProduction)


# Generate NRA details file for Payments and NRP database ---------------------------------------------

col_ord <- c('Source','Country_Label','Country_Code','Commodity_Label','Commodity_Code', 'Commodity_Type',
             'Year','Category','NRA_Cat','RelevantValueProduction','Support_USD','AGPROCODE','CATPROD')

col_ord2 <- c('Source','Country_Label','Country_Code','Commodity_Label','Commodity_Code', 'Commodity_Type',
             'Year','Category','NRA_Cat','ReferencePriceAtFGL','ProducerPriceAtFGL','RelevantValueProduction','Support_USD','AGPROCODE','CATPROD', 'NOTES')

CONSOLIDATED_NRA0a1 <- PAYMENTS%>%select(-Value_LCU) %>% 
                       mutate(Commodity_Type=ifelse(Commodity_Type=='','No',Commodity_Type)) %>% 
                       group_by(Source,Country_Label,Country_Code,Commodity_Label,Commodity_Code,Commodity_Type,Year,Category) %>% 
                       summarise(Value_USD=sum(Value_USD,na.rm = T)) %>% ungroup() %>% 
                      mutate(NRA_Cat=case_when( Category=="A2"~"Outputs",
                                                Category=="B"~"Inputs",
                                                TRUE~"Others") ) %>%  
                      mutate(NRA_Cat=factor(NRA_Cat,level=c("NRP","Outputs","Inputs","Others"))) %>%
                      left_join(PDCT_fullmap0) %>%
                      left_join(PDCT_FULL, by =c("AGPROCODE"="PDCT_CODE")) %>%
                      mutate(CATPROD=case_when(PDCT1=="Animal products"~"LVS",
                                               PDCT1=="NAL"~'NAL', 
                                               TRUE~"CRP") ) %>% 
                      left_join(PDCT_LABEL, by=c('AGPROCODE'='PDCT_CODE'), keep=T) %>% 
                      mutate(Commodity_Label=PDCT_DESC, Commodity_Code=PDCT_CODE) %>% 
                      select(-starts_with("PDCT")) %>%
                       rename(Support_USD=Value_USD) %>% 
                      group_by(Source,Country_Label,Country_Code,Commodity_Label,Commodity_Code,Commodity_Type,
                                  Year,Category,NRA_Cat,AGPROCODE,CATPROD) %>% 
                      summarise(Support_USD=sum(Support_USD,na.rm = T)) %>% ungroup() %>% 
                       left_join(VoP.Complete %>% mutate(PDCT_CODE=paste0('c', Item.Code)) %>% 
                                   select(CountryCode, PDCT_CODE, Year, RelevantValueProduction), 
                                    by=c('Country_Code'='CountryCode', 'AGPROCODE'='PDCT_CODE', 'Year'='Year')
                                   ) %>% 
                       left_join(VoP.NotinNRP %>% select(CountryCode, PDCT_CODE, Year, RelevantValueProduction2), 
                                 by=c('Country_Code'='CountryCode', 'AGPROCODE'='PDCT_CODE', 'Year'='Year')) %>% 
                       mutate(RelevantValueProduction=ifelse(is.na(RelevantValueProduction), RelevantValueProduction2, RelevantValueProduction)) %>% 
                       left_join(wine, by=c('Country_Code'='CountryCode', 'AGPROCODE'='PDCT_CODE', 'Year'='Year')) %>%
                       mutate(RelevantValueProduction=ifelse(is.na(RelevantValueProduction), RelevantValueProduction3, RelevantValueProduction)) %>%
                       dplyr::select(col_ord)



check.eu <- CONSOLIDATED_NRA0a1 %>% filter(Country_Code=='EUR') %>% filter(Year==2005) %>% 
            filter(NRA_Cat=='Others') %>% summarise(Support_USD=sum(Support_USD))

# duplicate <- CONSOLIDATED_NRA0a1 %>% 
#   group_by(Source,Country_Label,Country_Code,Commodity_Label,Commodity_Code,Commodity_Type,Year,Category) %>% 
#   summarise(n=n())
  

# add_column(ReferencePriceAtFGL=as.numeric(NA), ProducerPriceAtFGL=as.numeric(NA)) %>% 

CONSOLIDATED_NRA0a2 <- CONSOLIDATED_NRA0a1 %>% dplyr::filter(is.na(RelevantValueProduction)) %>% 
                              filter(!Commodity_Label %in% c('Feed crops', 'Biomass')) %>% 
                       mutate(Commodity_Label=paste0('Non-MPS other crops - ', Country_Label),
                              Commodity_Code=case_when(Country_Code=='RWA'~'c113001',
                                                      Country_Code=='BDI'~'c113002',
                                                       Country_Code=='BFA'~'c113003',
                                                       Country_Code=='ETH'~'c113004',
                                                       Country_Code=='MOZ'~'c113005',
                                                       Country_Code=='SEN'~'c113006',
                                                       Country_Code=='UGA'~'c113007',
                                                      Country_Code=='NGA'~'c113008',
                                                      Country_Code=='MWI'~'c113009',
                                                      TRUE~Commodity_Code),
                              AGPROCODE=Commodity_Code) %>%
                        group_by(Source, Country_Label, Country_Code, Commodity_Label, Commodity_Code,Commodity_Type,    
                                 Year,Category,NRA_Cat, AGPROCODE, CATPROD, RelevantValueProduction) %>% 
                         summarise(Support_USD=sum(Support_USD, na.rm = T)) %>% ungroup() %>% select(-RelevantValueProduction)


relvopna <- CONSOLIDATED_NRA0a1 %>% filter(is.na(RelevantValueProduction)) 

country.nra0a2 <- CONSOLIDATED_NRA0a2 %>% select(Country_Code) %>% distinct() %>% flatten() %>% unlist

vop.othcrops.mafap <- VoP.mapped %>% filter((CountryCode %in% country.nra0a2) & CATPROD=='CRP') %>% 
                      left_join(list.MPS %>% mutate(Item.Code=as.character(Item.Code))) %>% 
                      filter(is.na(MPS)) %>% group_by(LISTNAME_EN, Year) %>% 
                      summarise(RelevantValueProduction=sum(RelevantValueProduction, na.rm = T)) %>% ungroup() %>% 
                      mutate(Commodity_Label=paste0('Non-MPS other crops - ',LISTNAME_EN )) %>% rename(Country_Label=LISTNAME_EN)

CONSOLIDATED_NRA0a3 <- CONSOLIDATED_NRA0a2 %>% left_join(vop.othcrops.mafap) %>% select(col_ord)

CONSOLIDATED_NRA0a <- CONSOLIDATED_NRA0a1 %>% dplyr::filter(!is.na(RelevantValueProduction)) %>% rbind(CONSOLIDATED_NRA0a3) %>% 
                      rbind(CONSOLIDATED_NRA0a1 %>% filter(Commodity_Label %in% c('Feed crops', 'Biomass'))) %>% 
                      mutate(Country_Label=case_when(Country_Label=='Korea'~'Korea, Republic of', 
                                                     Country_Label=='Russia'~'Russian Federation', 
                                                     Country_Label=='United States'~'United States of America', 
                                                     Country_Label=='Tanzania'~'Tanzania, United Republic of', 
                                                     Country_Label=='Vietnam'~'Viet Nam', 
                                                     Country_Label=='Bolivia'~'Bolivia, Plurinational State of', 
                                                     TRUE~Country_Label
                                                     )) %>% 
                      add_column(ReferencePriceAtFGL=as.numeric(NA), ProducerPriceAtFGL=as.numeric(NA), 
                                 NOTES=as.character(NA)) %>% 
                      select(all_of(col_ord2)) %>% 
                      mutate(NOTES=case_when( (Source=='OECD' & Support_USD<0)~'Negative payment data from the OECD PSE database are maintained', 
                                              (Source=='FAO' & Support_USD<0)~'Negative payment data from FAO PE Commodity database are maintained', 
                                              TRUE~NOTES) )
                      # filter(Support_USD>=0)

# CONSOLIDATED_NRA0a.missing2 <- CONSOLIDATED_NRA0a%>%dplyr::filter(is.na(CATPROD))%>%
#                                select(AGPROCODE,Commodity_Label, Commodity_Code)%>%distinct()

CONSOLIDATED_NRA0b <- NRP_CSV %>% 
                      # filter(Year<2021) %>% 
                      mutate(Production_USD=ValueProduction_REF,
                             Support_USD=DistortionValue,
                             NRA_Cat="NRP",
                             Commodity_Type="Yes",
                             Commodity_Code=ProductCode,
                             Commodity_Label=ProductName,
                             Category='A1')%>%
                      mutate(NRA_Cat=factor(NRA_Cat,level=c("NRP","Outputs","Inputs","Others"))) %>%
                      rename(Country_Label=CountryName, Country_Code=CountryCode,AGPROCODE=ProductCode, 
                             RelevantValueProduction=Production_USD) %>%
                      select(-starts_with("PDCT")) %>%
                      left_join(PDCT_FULL, by =c("AGPROCODE"="PDCT_CODE")) %>%
                      mutate(CATPROD=case_when(PDCT1=="Animal products"~"LVS",
                                               PDCT1=="NAL"~'NAL',
                                               TRUE~"CRP") ) %>%
                      select(col_ord2)

check.fields.b <- colnames(CONSOLIDATED_NRA0b) 

NRP_nonMPS.AddCRP <- VoP_NMPS.crplvs %>% filter(CATPROD=='CRP') %>% 
                      mutate(Category='A1',NRA_Cat="NRP",
                             Commodity_Type="No",
                             Commodity_Label="Crops, Non-MPS",
                             Commodity_Code="nCRP",
                             AGPROCODE="nCRP")

NRP_nonMPS.AddLVS <- VoP_NMPS.crplvs %>% filter(CATPROD=='LVS') %>% 
                      mutate(Category='A1', NRA_Cat="NRP",
                             Commodity_Type="No",
                             Commodity_Label="Livestock, Non-MPS",
                             Commodity_Code="nLVS",
                             AGPROCODE="nLVS")

label_NRP_add <- CONSOLIDATED_NRA0b%>%select(Country_Code, Country_Label, Source, Year) %>% distinct

CONSOLIDATED_NRA0c <- rbind(NRP_nonMPS.AddCRP,NRP_nonMPS.AddLVS) %>% rename(Country_Code=CountryCode) %>% 
                      left_join(label_NRP_add) %>%
                      add_column(ReferencePriceAtFGL=as.numeric(NA), ProducerPriceAtFGL=as.numeric(NA), 
                                 NOTES=as.character(NA)) %>% 
                      select(check.fields.b) 


# Consolidate two databases and compute NRA at details level -------------------------------------------------------------------

col_final <- c("Source","Country_Label","Country_Code", "Commodity_Label","Commodity_Code", "Commodity_Type",     
              "Year","Category","NRA_Cat","ReferencePriceAtFGL","ProducerPriceAtFGL","RelevantValueProduction","Support_USD", "Rate","AGPROCODE","CATPROD",              
              "REGIONFAOCODE","REGIONNAME", "WBCODE2015","WBNAME2015","LISTNAME_EN", "COMPOSITE","COMPOSITE_LABEL","NOTES")  

CONSOLIDATED_NRA <- rbind(CONSOLIDATED_NRA0a, CONSOLIDATED_NRA0b,CONSOLIDATED_NRA0c) %>% 
                      arrange(Country_Label, Commodity_Label, Year, Category) %>% 
                     left_join(COUNTRY, by = c("Country_Code"="ISO3CODE"))%>%
                      mutate(Rate= Support_USD/RelevantValueProduction) %>%
                      mutate(Commodity_Type=ifelse(Commodity_Type=="", 'No', Commodity_Type)) %>% 
                      select(all_of(col_final)) 
                    

missing.tuples <- CONSOLIDATED_NRA %>% filter(is.na(RelevantValueProduction)) %>% 
                  select(Country_Label, Country_Code, Commodity_Label, AGPROCODE, Year) %>% 
                  mutate(Item.Code=as.integer(str_replace(AGPROCODE, 'c',''))) %>% select(-AGPROCODE) %>% distinct()


table(CONSOLIDATED_NRA$Category, CONSOLIDATED_NRA$NRA_Cat)

table(CONSOLIDATED_NRA$CATPROD, CONSOLIDATED_NRA$NRA_Cat)

support.negeative <- CONSOLIDATED_NRA %>% filter(NRA_Cat %in% c('Outputs', 'Inputs', 'Others')) %>% filter(Rate<0)
support.higherthan1 <- CONSOLIDATED_NRA %>% filter(NRA_Cat %in% c('Outputs', 'Inputs', 'Others')) %>% filter(Rate>1)

# write.csv(missing.tuples, 'QualityCheckFiles/Missing_Tuples_57.csv', na='', row.names = F)

dir.create(paste0("NRA_Output/Final/",Sys.Date(), sep = ""))

readr::write_excel_csv(CONSOLIDATED_NRA, paste0("NRA_Output/Final/", Sys.Date(),"/Detailed_NRA.csv", sep = ""))

# write.csv(CONSOLIDATED_NRA, "NRA_Output_Files/Detailed_NRA.csv",row.names = FALSE)


# Check for in production data is available for missing tuples --------------------------------

faostat.production <- FAOSTAT::read_faostat_bulk("FAOSTAT/Data_2024/Production_Crops_Livestock_E_All_Data_(Normalized).zip") %>%
                      filter(year>=2005 & year<=2023) 

check.production <- missing.tuples %>% left_join(faostat.production, by=c('Country_Label'='area', 'Commodity_Label'='item', 
                                                                          'Item.Code'='item_code', 'Year'='year')) %>% 
                    filter(!is.na(value))

readr::write_excel_csv(check.production, 'QualityCheckFiles/Country_Commodity_Missing_Tuples.csv', na='')

# Data coverage -------------------------------------------------------------------------------

coverage.bysource <- CONSOLIDATED_NRA %>% select(Source,Country_Code,Year) %>% distinct() %>% 
                      group_by(Source,Year) %>% summarise(N_Country=n()) %>% ungroup() %>% 
                     spread(Source,N_Country) %>% mutate_all(coalesce, 0)

countrylist.bysource <- CONSOLIDATED_NRA %>% select(Source,Country_Label,Year) %>% distinct() %>% 
                      group_by(Source,Year) %>% summarise(countrylist=paste0(Country_Label,collapse = ", ")) %>% ungroup() %>% 
                      spread(Source,countrylist) 

readr::write_excel_csv(countrylist.bysource, "NRA_Output/Coverage_Country_List.csv")  

Database.Coverage.0 <- CONSOLIDATED_NRA%>%select(Country_Code, Year)%>%distinct()%>%mutate(Included=1)
Database.Coverage.1 <- Database.Coverage.0%>%group_by(Year)%>%summarize(Number_Countries=n())

VoP_FAOSTAT.final <-  VoP_FAOSTAT.rescaled %>% group_by(CountryCode, Year) %>% summarise(VoPFAO=sum(VoPFAO, na.rm = T)) %>% ungroup() %>% 
                      left_join(Database.Coverage.0, by=c('CountryCode'='Country_Code', 'Year'='Year')) %>% 
                      mutate(Included=coalesce(Included,0))

VoP_FAOSTAT.coverage <- VoP_FAOSTAT.final %>% group_by(Year, Included) %>% 
                        summarize(Production=sum(VoPFAO,na.rm=TRUE)) %>% ungroup %>% group_by(Year) %>%
                        mutate(ShareValueOfProduction=Production/sum(Production))%>%
                        dplyr::filter(Included==1) %>%select(-Production) %>%
                        left_join(Database.Coverage.1)%>%select(-Included)

readr::write_excel_csv(VoP_FAOSTAT.final, "NRA_Output/VOP_Final.csv") 
readr::write_excel_csv(VoP_FAOSTAT.coverage, "NRA_Output/VoP_FAOSTAT_coverage.csv")  



# VoP- aggregation ----------------------------------------------------------------------------

# countryincluded <- CONSOLIDATED_NRA %>% select(Country_Code) %>% distinct() %>% mutate(included=1)

country_year <- CONSOLIDATED_NRA %>% select(Country_Code,Year) %>% mutate(cntryear=paste0(Country_Code,'_', Year)) %>% 
                  select(cntryear) %>% distinct() %>% flatten() %>% unlist()

VoP.country.allag <- VoP.mapped %>% 
                      mutate(cntyear=paste0(CountryCode,'_', Year)) %>% filter(cntyear %in% country_year) %>% 
                      group_by(CountryCode, Year) %>% 
                      summarise(RelevantValueProduction=sum(RelevantValueProduction, na.rm = T)) %>% ungroup() 

VoP.country.sector <- VoP.mapped %>% 
                      mutate(cntyear=paste0(CountryCode,'_', Year)) %>% filter(cntyear %in% country_year) %>% 
                      group_by(CountryCode, CATPROD, Year) %>% 
                      summarise(RelevantValueProduction=sum(RelevantValueProduction, na.rm = T)) %>% ungroup() 

VoP.income.sector <- VoP.mapped %>% 
                      mutate(cntyear=paste0(CountryCode,'_', Year)) %>% 
                     filter(cntyear %in% country_year) %>% group_by(WBCODE2015, CATPROD, Year)%>% 
                     summarize(RelevantValueProduction=sum(RelevantValueProduction,na.rm=TRUE)) %>% ungroup()


VoP.income.allag <- VoP.mapped %>% 
                    mutate(cntyear=paste0(CountryCode,'_', Year)) %>% 
                    filter(cntyear %in% country_year) %>% group_by(WBCODE2015, Year)%>%
                    summarize(RelevantValueProduction=sum(RelevantValueProduction,na.rm=TRUE)) %>% ungroup()


VoP.region.sector <- VoP.mapped %>% mutate(cntyear=paste0(CountryCode,'_', Year)) %>% 
                      filter(cntyear %in% country_year) %>% group_by(REGIONFAOCODE, CATPROD, Year) %>%
                      summarize(RelevantValueProduction=sum(RelevantValueProduction,na.rm=TRUE)) %>%ungroup()

VoP.region.allag <- VoP.mapped %>% mutate(cntyear=paste0(CountryCode,'_', Year)) %>% 
                    filter(cntyear %in% country_year) %>%  group_by(REGIONFAOCODE,Year) %>%
                    summarize(RelevantValueProduction=sum(RelevantValueProduction,na.rm=TRUE)) %>%ungroup()

VoP.world.sector <- VoP.mapped %>% mutate(cntyear=paste0(CountryCode,'_', Year)) %>% 
                    filter(cntyear %in% country_year) %>% group_by(CATPROD, Year)%>%
                    summarize(RelevantValueProduction=sum(RelevantValueProduction,na.rm=TRUE)) %>% ungroup()

VoP.world.allag <- VoP.mapped %>% mutate(cntyear=paste0(CountryCode,'_', Year)) %>% 
                  filter(cntyear %in% country_year) %>% group_by(Year)%>%
                  summarize(RelevantValueProduction=sum(RelevantValueProduction,na.rm=TRUE)) %>% ungroup()

# NRA - Aggregation ---------------------------------------------------------------------------------

AGGREGATED_NRA0 <- CONSOLIDATED_NRA %>% dplyr::filter(CATPROD%in% c("CRP","LVS")) %>% 
                    group_by(Year, Country_Code,LISTNAME_EN,REGIONFAOCODE,REGIONNAME, WBCODE2015, WBNAME2015, NRA_Cat, CATPROD)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE))%>%mutate(AGPROCODE=paste0("x",CATPROD)) %>%
                    left_join(VoP.country.sector, by=c('Country_Code'='CountryCode', 'CATPROD'='CATPROD', 'Year'='Year')) %>% 
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="CountryXSector") 

AGGREGATED_NRA1 <- CONSOLIDATED_NRA %>% dplyr::filter(CATPROD %in% c("NAL")) %>% 
                    group_by(Year, Country_Code,LISTNAME_EN,REGIONFAOCODE,REGIONNAME, WBCODE2015, WBNAME2015, NRA_Cat , CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% ungroup() %>% mutate(AGPROCODE="xAGR") %>%
                    left_join(VoP.country.allag, by=c('Country_Code'='CountryCode', 'Year'='Year') ) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="CountryXSector") 

AGGREGATED_NRA2 <- CONSOLIDATED_NRA %>% 
                    group_by(Year, Country_Code,LISTNAME_EN,REGIONFAOCODE,REGIONNAME, WBCODE2015, WBNAME2015,NRA_Cat)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE))%>% ungroup() %>% mutate(CATPROD="TOTAL")%>%mutate(AGPROCODE="xAGR") %>%
                    left_join(VoP.country.allag, by=c('Country_Code'='CountryCode', 'Year'='Year') )%>% 
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="CountryXAllAg") 

AGGREGATED_NRA3 <- CONSOLIDATED_NRA %>% 
                    group_by(Year, Country_Code,LISTNAME_EN,REGIONFAOCODE,REGIONNAME, WBCODE2015, WBNAME2015)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE))%>%  ungroup() %>% mutate(CATPROD="TOTAL")%>%mutate(AGPROCODE="xAGR")%>%
                    mutate(NRA_Cat="NRA_Total") %>%
                    left_join(VoP.country.allag, by=c('Country_Code'='CountryCode', 'Year'='Year')) %>% 
                    mutate(NRA=Support_USD/RelevantValueProduction)%>%mutate(AGGREGATION="CountryXAllAg") 


# world.total <- AGGREGATED_NRA3 %>% filter(Year==2018) %>% group_by(Year) %>% summarise(Support_USD=sum(Support_USD), VoP=sum(RelevantValueProduction))

# vop.country.total <- VoP.country.allag %>% filter(Year==2018) %>% group_by(Year) %>% summarise(VoP=sum(RelevantValueProduction))

AGGREGATED_NRA0x <- AGGREGATED_NRA0 %>%
                    group_by(Year, WBCODE2015, WBNAME2015, NRA_Cat, AGPROCODE,CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.income.sector)%>%
                    mutate(NRA=Support_USD/RelevantValueProduction)%>%mutate(AGGREGATION="IncomeLevelxSector")

AGGREGATED_NRA1x <- AGGREGATED_NRA1 %>%
                    group_by(Year, WBCODE2015, WBNAME2015, NRA_Cat , AGPROCODE,CATPROD)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.income.allag) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="IncomeLevelxSector")

AGGREGATED_NRA2x <- AGGREGATED_NRA2 %>%
                    group_by(Year, WBCODE2015, WBNAME2015, AGPROCODE,CATPROD,NRA_Cat) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.income.allag) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="IncomeLevelxAllAg")

AGGREGATED_NRA3x <- AGGREGATED_NRA3 %>%
                    group_by(Year, WBCODE2015, WBNAME2015, AGPROCODE,CATPROD,NRA_Cat) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.income.allag) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction)%>%mutate(AGGREGATION="IncomeLevelxAllAg")

# world.total <- AGGREGATED_NRA3x %>% filter(Year==2018) %>% group_by(Year) %>% summarise(Support_USD=sum(Support_USD), VoP=sum(RelevantValueProduction))

AGGREGATED_NRA0y <- AGGREGATED_NRA0 %>%
                    group_by(Year, REGIONFAOCODE,REGIONNAME, NRA_Cat, AGPROCODE,CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.region.sector) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="RegionxSector")

AGGREGATED_NRA1y <- AGGREGATED_NRA1 %>%
                    group_by(Year, REGIONFAOCODE,REGIONNAME, NRA_Cat , AGPROCODE,CATPROD)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.region.allag) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction)%>%mutate(AGGREGATION="RegionxSector")

AGGREGATED_NRA2y <- AGGREGATED_NRA2 %>%
                    group_by(Year, REGIONFAOCODE,REGIONNAME, AGPROCODE,CATPROD,NRA_Cat) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.region.allag) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="RegionxAllAg")

AGGREGATED_NRA3y <- AGGREGATED_NRA3 %>%
                    group_by(Year, REGIONFAOCODE,REGIONNAME, AGPROCODE,CATPROD,NRA_Cat) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.region.allag) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="RegionxAllAg")

AGGREGATED_NRA0w <- AGGREGATED_NRA0 %>%
                    group_by(Year, NRA_Cat, AGPROCODE,CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.world.sector) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="WorldxSector")

AGGREGATED_NRA1w <- AGGREGATED_NRA1 %>%
                    group_by(Year, NRA_Cat , AGPROCODE, CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.world.allag)%>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="WorldxSector")

AGGREGATED_NRA2w <- AGGREGATED_NRA2 %>%
                    group_by(Year, NRA_Cat, AGPROCODE,CATPROD)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.world.allag) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="WorldxAllAg")

AGGREGATED_NRA3w <- AGGREGATED_NRA3 %>%
                    group_by(Year, NRA_Cat, AGPROCODE,CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.world.allag) %>%
                    mutate(NRA=Support_USD/RelevantValueProduction) %>% mutate(AGGREGATION="WorldxAllAg")


AGGREGATED_NRA <- rbind(AGGREGATED_NRA0,AGGREGATED_NRA1,AGGREGATED_NRA2,AGGREGATED_NRA3,
                        AGGREGATED_NRA0x,AGGREGATED_NRA1x,AGGREGATED_NRA2x,AGGREGATED_NRA3x,
                        AGGREGATED_NRA0y,AGGREGATED_NRA1y,AGGREGATED_NRA2y,AGGREGATED_NRA3y,
                        AGGREGATED_NRA0w,AGGREGATED_NRA1w,AGGREGATED_NRA2w,AGGREGATED_NRA3w) 



readr::write_excel_csv(AGGREGATED_NRA, paste0("NRA_Output/Final/", Sys.Date(),"/Aggregated_NRA.csv", sep = ""))

# PAYMENTS_COUNTRY_CHECK <- PAYMENTS %>% select(Country_Label) %>% distinct
# PAYMENTS_COUNTRY_YR_SOURCE_CHECK <- PAYMENTS %>% select(Country_Label, Source, Year) %>% distinct
# PAYMENTS_COUNTRY_YR_COUNTRY <- PAYMENTS%>%select(Country_Code, Year)%>%distinct %>% mutate(PAYMENTSDB=1)
# 
# nrow(PAYMENTS_COUNTRY_YR_COUNTRY)
# 
# PAYMENTS_COUNTRY_YR_SOURCE_COUNT <- PAYMENTS%>%select(Country_Label, Source, Year) %>% distinct %>%
#                                     group_by(Source,Year) %>% summarize(Countries=n())
# 
# NRP_CSV_YR_COUNTRY <- NRP_CSV%>%select(CountryCode,Year) %>% mutate(Country_Code=CountryCode)%>%
#                       distinct() %>% mutate(PAYMENTSNRP=1)
# 
# nrow(NRP_CSV_YR_COUNTRY)
# 
# COMPARE_COVERAGE <- PAYMENTS_COUNTRY_YR_COUNTRY %>% full_join(NRP_CSV_YR_COUNTRY)
# nrow(COMPARE_COVERAGE)
# 
# MISSING_FROM_PAYMENTS <- COMPARE_COVERAGE %>% dplyr::filter(is.na(PAYMENTSDB) & PAYMENTSNRP==1) %>%
#                           group_by(Year)%>%summarize(Missing=paste(Country_Code, collapse = "; "))
# 
# write.csv(MISSING_FROM_PAYMENTS,file="QualityCheckFiles/Missing_From_Payments.csv")
# 
# png(file="QualityCheckFiles/MISSING_FROM_PAYMENTS.png")
# 
# p<-tableGrob(MISSING_FROM_PAYMENTS)
# grid.arrange(p)
# dev.off()


countrycode.source <- CONSOLIDATED_NRA %>% select(Country_Code, Source) %>% distinct()


country.average <- AGGREGATED_NRA2 %>% select(Country_Code, Year, NRA_Cat, NRA) %>% filter(Year>2017) %>% 
                   rbind(AGGREGATED_NRA3 %>% select(Country_Code, Year, NRA_Cat, NRA) %>% filter(Year>2017) %>% ungroup()) %>% 
                    group_by(Country_Code, NRA_Cat) %>% summarise(NRA=mean(NRA, na.rm = T)*100) %>% ungroup() %>% 
                   spread(NRA_Cat, NRA) %>% mutate_at(vars('NRP':'NRA_Total'), ~if_else(is.na(.),0,.)) %>%
                   left_join(countrycode.source)

eur.average <- country.average %>% filter(Country_Code=='EUR')

eu27.average <- data.frame(Region=EU27) %>% mutate(Country_Code='EUR') %>% left_join(eur.average) %>% select(-Country_Code) %>% rename(Country_Code=Region)

country.all.average <- country.average %>% filter(Country_Code!='EUR') %>% rbind(eu27.average) %>% 
                        mutate(Source=ifelse(Source=='Agrimonitor (IDB)', 'IDB', Source))


write.csv(country.all.average, './NRA_Output/Data_for_website/Country_NRA_Average.csv', row.names = F)


psedata <- read.csv('./Source/PSE_Share_Data/Update_2023/PSEShare.csv') 

vop_nrp <- read.csv('./Consortium/ToCheckOECD_NRP.csv') %>% mutate(Support=(NRP/100)*ValueProduction_REF) %>% 
           group_by(CountryCode,Year) %>% summarise(ValueProduction_REF=sum(ValueProduction_REF,na.rm = T),
                                                    Support=sum(Support,na.rm = T)) %>% 
           ungroup()

oecd.countries <- CONSOLIDATED_NRA %>% filter(Source=='OECD') %>% select(Country_Code) %>% distinct()

AGGREGATED_NRA_OECD <- AGGREGATED_NRA %>% filter(AGGREGATION=='CountryXAllAg') %>% filter(Country_Code %in% unlist(oecd.countries))

nra.pse <- AGGREGATED_NRA_OECD %>% left_join(psedata) %>% 
           mutate(NRA=NRA*100,PSEShare=PSEShare*100, AbsDif_NRA_PSE=abs(NRA-PSEShare)) %>% 
           left_join(vop_nrp, by=c('Country_Code'='CountryCode','Year'='Year')) %>% 
           mutate(Rate=Support*100/ValueProduction_REF)
            

write.csv(nra.pse, './Consortium/ToCheckOECD_NRA.csv', row.names = F)


# NRP by selected commodities ---------------------------------------------



