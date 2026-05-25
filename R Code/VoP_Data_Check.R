
library(readxl)
library(FAOSTAT)
library(tidyverse)
source("Mapping.R")

fao.country.code <- FAOcountryProfile%>%select(FAOST_CODE,ISO3_CODE)%>%rename(Area.Code=FAOST_CODE)

fao.list.country <- FAOcountryProfile %>% select(ISO3_CODE, FAO_TABLE_NAME) %>% na.omit() %>% 
                    mutate(FAO_TABLE_NAME = case_when(FAO_TABLE_NAME=='China'~'China, mainland',
                                    FAO_TABLE_NAME=='Taiwan, Province of China'~'China, Taiwan Province of',
                                    FAO_TABLE_NAME=='the Democratic Republic of the Congo'~'Democratic Republic of the Congo',
                                    FAO_TABLE_NAME=='the Dominican Republic'~'Dominican Republic',
                                    FAO_TABLE_NAME=='Swaziland'~'Eswatini',
                                    FAO_TABLE_NAME=='the Netherlands'~'Netherlands',
                                    FAO_TABLE_NAME=='the Niger'~'Niger',
                                    FAO_TABLE_NAME=='the Sudan'~'Sudan',
                                    FAO_TABLE_NAME=='the United Republic of Tanzania'~'United Republic of Tanzania',
                                    FAO_TABLE_NAME=='the United States of America'~'United States of America',
                                    FAO_TABLE_NAME=='The former Yugoslav Republic of Macedonia'~'North Macedonia',
                                    FAO_TABLE_NAME=='Occupied Palestinian Territory'~'Palestine',
                                    FAO_TABLE_NAME=='the Philippines'~'Philippines',
                                    FAO_TABLE_NAME=='the Russian Federation'~'Russian Federation',
                                    FAO_TABLE_NAME=='the United Kingdom of Great Britain and Northern Ireland'~'United Kingdom of Great Britain and Northern Ireland',
                                    FAO_TABLE_NAME=='the United Arab Emirates'~'United Arab Emirates',
                                    FAO_TABLE_NAME=='the Syrian Arab Republic'~'Syrian Arab Republic',
                                    FAO_TABLE_NAME=="the Lao People's Democratic Republic"~"Lao People's Democratic Republic",
                                    FAO_TABLE_NAME=='the Gambia'~'Gambia',
                                    FAO_TABLE_NAME=='the Bahamas'~'Bahamas',
                                    FAO_TABLE_NAME=='the Comoros'~'Comoros',
                                    FAO_TABLE_NAME=='Micronesia (Federated States of)'~'Micronesia',
                                    FAO_TABLE_NAME=='French Polynesia'~'Polynesia',
                                    FAO_TABLE_NAME=='the Czech Republic'~'Czechia',
                                    TRUE~FAO_TABLE_NAME)) %>% rename(CountryCode=ISO3_CODE)



nrp <- read.csv(file="Source/PUBLICVIEW2020.csv")%>%
        dplyr::filter(Category=="COUNTRY_PRODUCT")

nrp.country <- read.csv(file="Source/PUBLICVIEW2020.csv")%>%
                mutate(CountryName=case_when(CountryName=='Korea, Republic of'~'Republic of Korea', 
                               CountryName=='Tanzania, United Republic of '~'United Republic of Tanzania', 
                               CountryName=='Bolivia, Plurinational State of'~'Bolivia (Plurinational State of)',
                               TRUE~CountryName)) 

list.country <- nrp.country%>% dplyr::filter(Category=="COUNTRY_TOTAL")%>% select(CountryName)%>% distinct() %>% 
                rbind(payments %>%select(Country_Label)%>%rename(CountryName=Country_Label) %>% distinct()) %>% 
                distinct() %>% flatten()%>%unlist

eu28 <- read.table("EU28.txt",col.names = FALSE)%>%flatten%>%unlist

payments <- read.csv(file="Source/Consolidated_Payment_Data.csv") %>%
            dplyr::filter(Year>=2005) %>%
            mutate(Country_Code=ifelse(Country_Code=="E28","EUR",Country_Code)) %>%
            mutate(Source=ifelse(Source %in% c("OECD","MAFAP","IDB"), Source, "OthSources")) %>%
            dplyr::filter(Year<=2018) %>% mutate(Commodity_Code = ifelse(Commodity_Code== 50 & Country_Code=="JAM","XE",Commodity_Code)) %>% 
            mutate(CountryName=case_when(Country_Label=='Russia'~'Russian Federation', 
                                           Country_Label=='Vietnam'~'Viet Nam', 
                                           Country_Label=='United States'~'United States of America', 
                                           Country_Label=='Korea'~'Republic of Korea', 
                                           Country_Label=='Bolivia'~'Bolivia (Plurinational State of)', 
                                           Country_Label=='Tanzania'~'United Republic of Tanzania',
                                           TRUE~Country_Label)) %>% select(-Country_Label)


fao.vop <- FAOSTAT::read_faostat_bulk("FAOSTAT/Value_of_Production_E_All_Data_(Normalized).zip") %>%
            filter(area!='China') %>%
            left_join(fao.list.country, by=c('area'='FAO_TABLE_NAME')) %>%
            filter(!is.na(CountryCode)) %>% select(-area_code, -flag, -year_code) %>% rename(CountryName=area, Year=year) %>% 
            select(CountryName, CountryCode, everything()) %>% filter(Year>=2005) %>% 
            mutate(CountryCode=ifelse(CountryCode %in% eu28, "EUR",CountryCode)) %>% 
            mutate(CountryName=ifelse(CountryCode=='EUR', 'European Union', ifelse(CountryCode=='CHN', 'China', CountryName))) %>% 
            filter(item_code<=1500)


broad.sector <- c('Agriculture', 'Crops', 'Food', 'Livestock', 'Non Food', 'Vegetables and Fruit Primary', 'Sugar Crops Primary')

item.fao.vop <- fao.vop %>% select(item, item_code) %>% distinct()

country.item.vop <- fao.vop %>% filter(CountryName %in% list.country) %>% select(CountryName, item, item_code) %>% distinct()


## check if FAO provides data for all countries in NRP and payment data 
country.missing.vop <- fao.vop %>% filter(CountryName %in% list.country) %>% 
                        select(CountryName, element_code) %>% distinct() %>% spread(element_code, element_code) %>% 
                        filter(!(`55` %in% c(55)))

# Country-Commodity Tuple ---------------------------------------------------------------------

commodity.class <- read_excel('Source/Aggregate_Commodity_Composition.xlsx', sheet = 'Commodity_Class') %>% 
                    filter(Group %in% c('Crops', 'Livestock', 'Other'))

commodity <- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE) 

com.withVoP.match <- commodity %>% select(FAONAME, FAOCODE) %>% na.omit() %>% filter(FAOCODE<=1500) %>% 
                      left_join(item.fao.vop, by=c('FAOCODE'='item_code')) %>% filter(is.na(item)) %>% arrange(FAONAME)

nrp.commodity <- nrp %>% mutate(ProductCode=as.integer(str_replace(ProductCode, 'c',''))) %>%  
                  left_join(commodity, by=c('ProductCode'='FAOCODE')) %>% 
                  mutate(ProductName=ifelse(!is.na(FAONAME), FAONAME, ProductName)) %>% 
                  select(CountryName,ProductName, ProductCode) %>% distinct() %>% 
                  arrange(CountryName, ProductName) %>% 
                  mutate(ProductCode=case_when(ProductName=='China - Fruits and Vegetables Exported'~26999, 
                                               ProductName=='China - Fruits and Vegetables Imported'~26999, 
                                               TRUE~as.double(ProductCode) ), 
                         ProductName=case_when(ProductName=='China - Fruits and Vegetables Exported'~'Fruits and vegetables', 
                                               ProductName=='China - Fruits and Vegetables Imported'~'Fruits and vegetables', 
                                               TRUE~ProductName )
                  ) %>%
                  mutate(ProductCode=case_when(ProductCode==866~867, 
                                               ProductCode==564~560, 
                                               ProductCode==2005~108, 
                                               ProductCode==328~767, 
                                               TRUE~ProductCode), 
                         ProductName=case_when(ProductName=='Cattle'~'Meat, cattle', 
                                               ProductName=='Wine'~'Grapes', 
                                               ProductName=='Teff'~'Cereals nes', 
                                               ProductName=='Seed cotton'~'Cotton lint', 
                                               TRUE~ProductName), 
                         CountryName=case_when(CountryName=='Korea, Republic of'~'Republic of Korea', 
                                               CountryName=='Tanzania, United Republic of '~'United Republic of Tanzania', 
                                               CountryName=='Bolivia, Plurinational State of'~'Bolivia (Plurinational State of)',
                                               TRUE~CountryName), 
                         ProductName=case_when(ProductName=='Pulses, nes'~'Pulses nes', TRUE~ProductName)) %>% 
                  distinct() %>% mutate(MPS='MPS') %>% filter(!is.na(ProductCode))


commodity.bymps <- fao.vop %>% filter(CountryName %in% list.country) %>% 
                    filter(element_code==57) %>%
                    full_join(nrp.commodity, by=c('CountryName'='CountryName', 'item'='ProductName')) %>% 
                    mutate(MPS=ifelse(is.na(MPS), 'NonMPS', MPS),
                           ProductCode=ifelse(is.na(ProductCode), item_code, ProductCode)) 

country.com.mps <- commodity.bymps %>% filter(MPS=='MPS') %>% select(CountryName, item, ProductCode) %>% distinct()

## commodity missing FAOSTAT  at this stage 
com.nrp.mis.vop <- commodity.bymps %>% filter(is.na(item_code)) %>% select(CountryName, item) %>% distinct() 

write.csv(com.nrp.mis.vop, 'VoP_Missing_Commodities/Commodity_Missing_in57.csv', na='', row.names = F)


# Payments: Country-Commodity tuple -----------------------------------------------------------

pmnt.country.com <- payments %>% select(CountryName, Commodity_Label, Commodity_Code) %>% distinct() %>% 
                    filter(!grepl('GCT|XE|AC|9999|9990|9991|9992|9993|9994', Commodity_Code)) %>% 
                    mutate(Commodity_Label=case_when(Commodity_Label=="Milk"~"Cow milk, whole, fresh",
                                                     Commodity_Label=="Rice"~"Rice, paddy",
                                                     Commodity_Label=="Beef and veal"~"Cattle meat",
                                                     Commodity_Label=="Poultry meat"~"Chicken meat",
                                                     Commodity_Label=="Coffee"~"Coffee, green",
                                                     Commodity_Label=="Cotton"~"Cotton lint",
                                                     Commodity_Label=="Common Wheat"~"Wheat",
                                                     Commodity_Label=="Durum Wheat"~"Wheat",
                                                     Commodity_Label=="Sunflower"~"Sunflower seed",
                                                     Commodity_Label=="Eggs"~"Hen eggs, in shell",
                                                     Commodity_Label=="Potato"~"Potatoes",
                                                     Commodity_Label=="Wool"~"Wool, greasy",
                                                     Commodity_Label=="Cabbage"~"Cabbages and other brassicas",
                                                     Commodity_Label=="Cucumber"~"Cucumbers and gherkins",
                                                     Commodity_Label=="Mandarin"~"Tangerines, mandarins, clem.",
                                                     Commodity_Label=="Spinaches"~"Spinach",
                                                     Commodity_Label=="Welsh Onion"~"Onions, dry",
                                                     Commodity_Label=="Beans"~"Beans, dry",
                                                     Commodity_Label=="Rubber"~"Natural rubber",
                                                     Commodity_Label=="Coconut"~"Coconuts",
                                                     Commodity_Label=="Dry peas"~"Peas, dry",
                                                     Commodity_Label=="Lentils"~"Lentils, dry",
                                                     Commodity_Label=="Flaxseed"~"Flax fibre and tow",
                                                     Commodity_Label=="Tobacco"~"Tobacco, unmanufactured",
                                                     Commodity_Label=="Poultry Meat"~"Chicken meat",
                                                     Commodity_Label=="Beef and Veal"~"Cattle meat",
                                                     Commodity_Label=="Cocoa Beans"~"Cocoa beans",
                                                     Commodity_Label=="Pigmeat"~"Pig meat",
                                                     Commodity_Label=="Sweet Potatoes"~"Sweet potatoes",
                                                     Commodity_Label=="Yam"~"Yams",
                                                     Commodity_Label=="Peppers"~"Pepper (piper spp.)",
                                                     Commodity_Label=="Sheep Meat"~"Sheep meat",
                                                     Commodity_Label=="Milk, cow"~"Cow milk, whole, fresh",
                                                     Commodity_Label=="Oil, palm fruit"~"Oil palm fruit",
                                                     Commodity_Label=="Onions"~"Onions, dry",
                                                     Commodity_Label=="Chickens"~"Chicken meat",
                                                     Commodity_Label=="Goats"~"Goat meat",
                                                     Commodity_Label=="Pigs"~"Pig meat",
                                                     Commodity_Label=="Sheep"~"Sheep meat",
                                                     TRUE~Commodity_Label)
                           ) %>% 
                    left_join(commodity, by=c('Commodity_Label'='AGCOMNAME') ) %>% select(CountryName, Commodity_Label, FAOCODE)

country.com.payment <- fao.vop %>% filter(CountryName %in% list.country) %>% filter(element_code==152) %>% 
                        right_join(pmnt.country.com, by=c('CountryName'='CountryName', 'item'='Commodity_Label')) 

com.pmnt.missing <- country.com.payment %>% filter(is.na(FAOCODE)) %>% select(CountryName, item) %>% distinct() 


fao.prod <- FAOSTAT::read_faostat_bulk('FAOSTAT/Production_Crops_Livestock_E_All_Data_(Normalized).zip') %>% 
            filter(element=='Production') %>% filter(area!='China') %>%
            left_join(fao.list.country, by=c('area'='FAO_TABLE_NAME')) %>%
            filter(!is.na(CountryCode)) %>% select(-area_code, -flag, -year_code) %>% rename(CountryName=area, Year=year) %>% 
            select(CountryName, CountryCode, everything()) %>% filter(Year>=2005) %>% 
            mutate() %>% mutate(CountryCode=ifelse(CountryCode %in% eu28, "EUR",CountryCode)) %>% 
            mutate(CountryName=ifelse(CountryCode=='EUR', 'European Union', ifelse(CountryCode=='CHN', 'China', CountryName))) %>% 
            filter(item_code<=1500)

country.prod.missing <- fao.prod %>% filter(CountryName %in% list.country) %>% select(CountryName) %>% distinct()

item.fao.prod <- fao.prod %>% select(item, item_code) %>% distinct()

country.item.prod <- fao.prod %>% filter(CountryName %in% list.country) %>% select(CountryName, item, item_code) %>% distinct() %>% 
                      mutate(Exist='Yes')

com.withProd.match <- commodity %>% select(FAONAME, FAOCODE) %>% na.omit() %>% filter(FAOCODE<=1500) %>% 
                      left_join(item.fao.prod, by=c('FAOCODE'='item_code')) %>% filter(is.na(item)) %>% arrange(FAONAME)

com.nrp.prod <- fao.prod %>% filter(CountryName %in% list.country) %>%  
                  full_join(nrp.commodity, by=c('CountryName'='CountryName', 'item'='ProductName')) %>% 
                  mutate(MPS=ifelse(is.na(MPS), 'NonMPS', MPS),
                         ProductCode=ifelse(is.na(ProductCode), item_code, ProductCode)) 

country.com.prod <- com.nrp.prod %>% filter(MPS=='MPS') %>% select(CountryName, item, ProductCode) %>% distinct()

## commodity missing FAOSTAT  at this stage 
com.nrp.mis.prod <- com.nrp.prod %>% filter(is.na(item_code)) %>% select(CountryName, item) %>% distinct() %>% mutate(Exist='No') 

com.vop.prod.miss <- com.nrp.mis.vop %>% left_join(com.nrp.mis.prod)

country.item.missinginVop <- country.item.prod %>% full_join(country.item.vop)

## check if meat equals indigenous 
meat.indigenous <- FAOSTAT::read_faostat_bulk(".\\FAOSTAT\\Value_of_Production_E_All_Data_(Normalized).zip")%>%
                      filter(area!='China') %>%
                      left_join(fao.list.country, by=c('area'='FAO_TABLE_NAME')) %>%
                      filter(!is.na(CountryCode)) %>% select(-area_code, -flag, -year_code) %>% 
                      rename(Area=area, Year=year, Item.Code=item_code, Item=item ) %>% 
                      select(Area, CountryCode, everything()) %>% filter(Year>=2005) %>% 
                      mutate(CountryCode=ifelse(CountryCode %in% eu28, "EUR",CountryCode)) %>% 
                      mutate(Area=ifelse(CountryCode=='EUR', 'European Union', ifelse(CountryCode=='CHN', 'China', Area))) %>% 
                      filter(Item.Code<=1500) %>% filter(grepl('Meat', Item)) %>% 
                      filter(element_code==57) %>% select(-unit, -element, -element_code, -CountryCode)

country.check  <-     meat.indigenous %>% 
                      filter(Area=='Iceland') %>% filter(Item.Code %in% c(944,867, 972))  %>% select(-Item) %>% 
                      spread(Item.Code, value) %>% mutate(diff_meat = `944`-`867`)



# write.csv(pmnt.country.com, 'Payment_Commodity_Missing.csv', na='', row.names = F)

# commodity.bymps %>% select(-Item.Code) %>% 
#                     arrange(CountryName, Item) %>% 
#                     left_join(commodity.class, by=c()) %>% 
#                     mutate(Item_Code=case_when(Item=='Honey, natural'~1182, 
#                                                Item=='Beeswax'~1183,
#                                                Item=='Silk-worm cocoons, reelable'~1185,
#                                                TRUE~Item_Code), 
#                            Group=case_when(Item=='Honey, natural'~'Other',
#                                            Item=='Coca'~'Crops', 
#                                            Item=='Fruits and vegetables'~'Crops', 
#                                            Item=='Beeswax'~'Other',
#                                            Item=='Silk-worm cocoons, reelable'~'Other',
#                                            TRUE~Group)) %>% 
#                     mutate(F_V=ifelse(is.na(F_V), 'NonFV', F_V), 
#                            F_V=case_when(Item=='Fruits and vegetables'~'Fruits and vegetables', 
#                                          TRUE~F_V)) %>%
#                     mutate(MPS=case_when((CountryName=='China' & F_V=='Fruits and vegetables')~'MPS',
#                                          (CountryName=='Israel' & F_V=='Fruits and vegetables')~'MPS',
#                                          (CountryName=='Argentina' & F_V=='Fruits and vegetables')~'MPS',
#                                          TRUE~MPS
#                     ))






