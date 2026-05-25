
library(readxl)
library(FAOSTAT)
library(tidyverse)
library(stringr)
source("Mapping.R")
source('VoP_NonMPS_Processing.R')


# Data loading --------------------------------------------------------------------------------
## first, payments data file
PAYMENTS <- read.csv(file="Source/Consolidated_Payment_Data.csv") %>%
            dplyr::filter(Year>=2005) %>%
            mutate(Source=ifelse(Source %in% c("OECD","MAFAP","IDB"), Source, "OthSources")) %>%
            dplyr::filter(Year<=2018) %>% filter(Source!='OthSources') 

## NRP database, currently online 
NRP_CSV <- read.csv(file="Source/PUBLICVIEW.csv")%>%
            dplyr::filter(Category=="COUNTRY_PRODUCT")

NRP_DET <- read.csv(file="Source/PUBLICVIEW.csv")%>%
           dplyr::filter(Category=="COUNTRY_PRODUCT") %>% 
            select('CountryName','CountryCode','ProductName','ProductCode','Year','ValueProduction_PP','ValueProduction_REF','NRP') %>% 
            rename(VoP_FG=ValueProduction_PP, VoP_RP=ValueProduction_REF)

NRP_TOTAL <- read.csv(file="Source/PUBLICVIEW.csv")%>%
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
list.country <- union(list_NRP, list_PAY)

MPS_PROD_ISO <- NRP_CSV%>%select(CountryCode,ProductCode,Year)%>%distinct()%>%rename(PDCT_CODE=ProductCode)


EU28 <- read.table("EU28.txt",col.names = FALSE)%>%flatten%>%unlist

PDCT_fullmap0 <- PDCT_COMMODITY%>%gather(Source2,Commodity_Code,-AGPROCODE)%>%
                mutate(Source=case_when(Source2=="OECD_CODE"~"OECD",
                                        Source2=="MAFAP_CODE"~"MAFAP",
                                        Source2=="IADB_CODE"~"IDB"))%>%
                dplyr::filter(nchar(Commodity_Code)>0)%>%select(-Source2)

COMMODITY_L <- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE)%>%
               select(AGPROCODE,AGPRONAME, IADB, MAFAP,OECD) %>% distinct() %>% 
               gather(Source, Commodity_Label, -c(AGPROCODE, AGPRONAME)) %>% filter(Commodity_Label!='') %>% select(-Source) %>% 
                distinct()

PAY_PROD_ISO <- PAYMENTS %>% select(Country_Code, Commodity_Label, Year) %>% distinct() %>% 
                  left_join(COMMODITY_L) %>% distinct() %>% 
                  rename(PDCT_CODE=AGPROCODE, CountryCode=Country_Code) %>% select(CountryCode, PDCT_CODE, Year) 


# Data processing from FAOSTAT ----------------------------------------------------------------

COUNTRYFAO <- FAOcountryProfile%>%select(FAOST_CODE,ISO3_CODE)%>%rename(Area.Code=FAOST_CODE)

vop_cols <- c('CountryCode','Item', 'Item.Code', 'element', 'element_code', 'Year', 'VoPFAO')
faostat.vop <- FAOSTAT::read_faostat_bulk("Source/Value_of_Production_E_All_Data_(Normalized).zip") %>%
            rename(Year=year, VoPFAO=value, Item.Code=item_code, Item=item, Area.Code=area_code) %>% 
            filter(Year>=2005 & Year<=2018) %>% left_join(COUNTRYFAO) %>% filter(!is.na(ISO3_CODE)) %>% rename(CountryCode=ISO3_CODE) %>% 
            select(vop_cols)
  
VoP_FAOSTAT1 <-faostat.vop %>% filter(element_code==57) %>% select(-element, -element_code)

meat <- VoP_FAOSTAT1 %>% filter(Item.Code<=1500) %>% dplyr::filter(grepl("Meat", Item)) %>% mutate (
                            TypeMeat= case_when( str_detect(Item, "pig") ~ "PIG",
                                                 str_detect(Item, "cattle") ~ "CATTLE",
                                                 str_detect(Item, "other camelids") ~ "CAMELIDS",
                                                 str_detect(Item, "camel") ~ "CAMEL",
                                                 str_detect(Item, "chicken") ~ "CHICKEN",
                                                 str_detect(Item, "horse") ~ "HORSE",
                                                 str_detect(Item, "buffalo") ~ "BUFFALO",
                                                 str_detect(Item, "goat") ~ "GOAT",
                                                 str_detect(Item, "sheep") ~ "SHEEP",
                                                 str_detect(Item, "rabbit") ~ "RABBIT",
                                                 str_detect(Item, "turkey") ~ "TURKEY",
                                                 str_detect(Item, "ass") ~ "ASS",
                                                 str_detect(Item, "mule") ~ "MULE",
                                                 str_detect(Item, "duck") ~ "DUCK",
                                                 str_detect(Item, "geese") ~ "GEESE",
                                                 str_detect(Item, "bird nes") ~ "BIRD NES",
                                                 str_detect(Item, "rodents") ~ "RODENTS", 
                                         TRUE ~ "OTHER"),
          Indigenous= ifelse ( grepl("indigenous",Item) ,"INDIGENOUS", "NOT_INDIGENOUS") ) %>% 
          group_by(CountryCode, Year, TypeMeat,Indigenous) %>%
          summarise(tVoP=sum(VoPFAO, na.rm=TRUE)) %>% ungroup() %>% 
          spread(Indigenous,tVoP) %>%
          ungroup() %>% mutate(indigenous_high=ifelse(NOT_INDIGENOUS<=INDIGENOUS, 1, 0), 
                               NA_values=ifelse( (is.na(INDIGENOUS)|is.na(NOT_INDIGENOUS)), 1, 0), 
                               indigenous_equal =ifelse(NOT_INDIGENOUS==INDIGENOUS, 1, 0) ) 


meat.drule <- meat %>% group_by(CountryCode, Year, TypeMeat) %>% 
              mutate(Item_sel= slice_max(Item, order_by=sort(VoPFAO), with_ties = F))

table(meat$indigenous_high)
table(meat$NA_values)
table(meat$indigenous_equal)
                               
# write.csv(meat, 'QualityCheckFiles/Meat_Indigenous_vs_Not_Indigenous.csv', row.names = F)


meat.codes <- VoP_FAOSTAT1 %>% filter(Item.Code<=1500) %>% dplyr::filter(grepl("Meat", Item)) %>% 
              select(Item, Item.Code) %>% distinct() %>% mutate (
                TypeProduct= case_when( grepl("pig",Item) ~ "PIG",
                                     grepl("cattle",Item) ~ "CATTLE",
                                     grepl("other camelids",Item) ~ "CAMELIDS",
                                     grepl("camel",Item) ~ "CAMEL",
                                     grepl("chicken",Item) ~ "CHICKEN",
                                     grepl("horse",Item) ~ "HORSE",
                                     grepl("buffalo",Item) ~ "BUFFALO",
                                     grepl("goat",Item) ~ "GOAT",
                                     grepl("sheep",Item) ~ "SHEEP",
                                     grepl("rabbit",Item) ~ "RABBIT",
                                     grepl("turkey",Item) ~ "TURKEY",
                                     grepl("ass",Item) ~ "ASS",
                                     grepl("mule",Item) ~ "MULE",
                                     grepl("duck",Item) ~ "DUCK",
                                     grepl("geese",Item) ~ "GEESE",
                                     grepl("bird nes",Item) ~ "BIRD NES",
                                     grepl("rodents",Item) ~ "RODENTS", 
                                     TRUE ~ "OTHER"),
                Indigenous= ifelse ( grepl("indigenous",Item) ,"INDIGENOUS", "NOT_INDIGENOUS") ) 
          #   %>% select(-Item) %>% distinct() %>% 
          # filter(TypeProduct!='OTHER') %>% 
          #       spread(Indigenous, Item.Code) %>%  
          #       arrange(TypeProduct)

sel_cols <- c('CountryCode', 'Item', 'Item.Code', 'Year', 'VoPFAO')
meat.code.touse <- meat.codes %>% filter(Indigenous=='INDIGENOUS') %>% select(-Indigenous)

# write.csv(meat.code.touse, "Mapping/Meat_Indigenous_Codes.csv", row.names = F)

meat.indigenous <- meat %>% filter(!is.na(indigenous_high)) %>% mutate(VoPFAO=INDIGENOUS) %>% select(CountryCode, TypeMeat, Year,VoPFAO) %>% 
                    left_join(meat.code.touse, by=c('TypeMeat'='TypeProduct')) %>% 
                   select(sel_cols)

meat.mixed <- meat %>% filter(!is.na(indigenous_high)) %>% 
              mutate(Type = ifelse(indigenous_high==1, 'INDIGENOUS', 'NOT_INDIGENOUS'), 
                     VoPFAO=ifelse(indigenous_high==1, INDIGENOUS, NOT_INDIGENOUS)) %>% 
              left_join(meat.codes, by=c('TypeMeat'='TypeProduct', 'Type'='Indigenous')) %>% 
              select(sel_cols)


meat.missing <- meat %>% filter(is.na(indigenous_high)) %>% filter(!TypeMeat %in% c('OTHER', 'RODENTS')) %>% 
                mutate(Type = ifelse(is.na(INDIGENOUS), 'NOT_INDIGENOUS', 'INDIGENOUS'),
                  VoPFAO=ifelse(is.na(INDIGENOUS), NOT_INDIGENOUS, INDIGENOUS)) %>% 
                left_join(meat.codes, by=c('TypeMeat'='TypeProduct', 'Type'='Indigenous')) %>% select(sel_cols)

meat.other <- VoP_FAOSTAT1 %>% filter(Item.Code %in% c(1163, 1073, 1166, 1151)) %>% select(sel_cols)

VoP_FAOSTAT.meat <- rbind(meat.mixed, meat.missing, meat.other)

VoP_FAOSTAT.nonmeat <- VoP_FAOSTAT1 %>% filter(Item.Code<=1500) %>% dplyr::filter(!grepl("Meat", Item)) %>% 
                       select(sel_cols) 

VoP_FAOSTAT1.f <- rbind(VoP_FAOSTAT.nonmeat, VoP_FAOSTAT.meat)


# write.csv(meat.codes, 'Mapping/Meat_indigenous_vs_notIndigenous_codes.csv', row.names = F)

# add.meat.exist <- data.frame(ProductCode=c(867, 867, 1017, 1017, 1035, 1035, 1058, 1058,1141,1141,1127,1127,1158,1158,947,947,1089,1089,1108,1108,1111,1111,1069,1069,1080,1080,1097,1097 ), 
#                     Item_Code=c(867, 944, 1017, 1032, 1035,1055,1058,1094, 1141, 1144,1127,1137,1158,1161,947,972,1089,1084,1108,1122,1111,1124,1069,1070,1080,1087,1097,1120))

available.countries <- VoP_FAOSTAT1.f %>% select(CountryCode,Item, Item.Code,Year) %>% distinct() %>% 
                       mutate(TypeProduct= case_when( grepl("pig",Item) ~ "PIG",
                                                      grepl("cattle",Item) ~ "CATTLE",
                                                      grepl("other camelids",Item) ~ "CAMELIDS",
                                                      grepl("camel",Item) ~ "CAMEL",
                                                      grepl("chicken",Item) ~ "CHICKEN",
                                                      grepl("horse",Item) ~ "HORSE",
                                                      grepl("buffalo",Item) ~ "BUFFALO",
                                                      grepl("goat",Item) ~ "GOAT",
                                                      grepl("sheep",Item) ~ "SHEEP",
                                                      grepl("rabbit",Item) ~ "RABBIT",
                                                      grepl("turkey",Item) ~ "TURKEY",
                                                      grepl("ass",Item) ~ "ASS",
                                                      grepl("mule",Item) ~ "MULE",
                                                      grepl("duck",Item) ~ "DUCK",
                                                      grepl("geese",Item) ~ "GEESE",
                                                      grepl("bird nes",Item) ~ "BIRD NES",
                                                      grepl("rodents",Item) ~ "RODENTS", 
                                                      TRUE ~ Item)
                              ) %>% select(-Item, -Item.Code) %>% mutate(VoPUSD=1)
  

                   

VoP_FAOSTAT2 <- faostat.vop %>% filter(element_code==152) %>% filter(Item.Code<=1500) %>% 
                mutate(TypeProduct= case_when( grepl("pig",Item) ~ "PIG",
                                               grepl("cattle",Item) ~ "CATTLE",
                                               grepl("other camelids",Item) ~ "CAMELIDS",
                                               grepl("camel",Item) ~ "CAMEL",
                                               grepl("chicken",Item) ~ "CHICKEN",
                                               grepl("horse",Item) ~ "HORSE",
                                               grepl("buffalo",Item) ~ "BUFFALO",
                                               grepl("goat",Item) ~ "GOAT",
                                               grepl("sheep",Item) ~ "SHEEP",
                                               grepl("rabbit",Item) ~ "RABBIT",
                                               grepl("turkey",Item) ~ "TURKEY",
                                               grepl("ass",Item) ~ "ASS",
                                               grepl("mule",Item) ~ "MULE",
                                               grepl("duck",Item) ~ "DUCK",
                                               grepl("geese",Item) ~ "GEESE",
                                               grepl("bird nes",Item) ~ "BIRD NES",
                                               grepl("rodents",Item) ~ "RODENTS", 
                                               TRUE ~ Item)) %>% 
                left_join(available.countries) %>% 
                dplyr::filter(is.na(VoPUSD)) %>% select(-VoPUSD) 


meat.152 <- VoP_FAOSTAT2 %>% dplyr::filter(grepl("Meat", Item)) %>% mutate (
              TypeMeat= case_when( str_detect(Item, "pig") ~ "PIG",
                                   str_detect(Item, "cattle") ~ "CATTLE",
                                   str_detect(Item, "other camelids") ~ "CAMELIDS",
                                   str_detect(Item, "camel") ~ "CAMEL",
                                   str_detect(Item, "chicken") ~ "CHICKEN",
                                   str_detect(Item, "horse") ~ "HORSE",
                                   str_detect(Item, "buffalo") ~ "BUFFALO",
                                   str_detect(Item, "goat") ~ "GOAT",
                                   str_detect(Item, "sheep") ~ "SHEEP",
                                   str_detect(Item, "rabbit") ~ "RABBIT",
                                   str_detect(Item, "turkey") ~ "TURKEY",
                                   str_detect(Item, "ass") ~ "ASS",
                                   str_detect(Item, "mule") ~ "MULE",
                                   str_detect(Item, "duck") ~ "DUCK",
                                   str_detect(Item, "geese") ~ "GEESE",
                                   str_detect(Item, "bird nes") ~ "BIRD NES",
                                   str_detect(Item, "rodents") ~ "RODENTS", 
                                   TRUE ~ "OTHER"),
              Indigenous= ifelse ( grepl("indigenous",Item) ,"INDIGENOUS", "NOT_INDIGENOUS") ) %>% 
              group_by(CountryCode, Year, TypeMeat,Indigenous) %>%
              summarise(tVoP=sum(VoPFAO, na.rm=TRUE)) %>% ungroup() %>% 
              spread(Indigenous,tVoP) %>%
              ungroup() %>% mutate(indigenous_high=ifelse(NOT_INDIGENOUS<=INDIGENOUS, 1, 0), 
                                   NA_values=ifelse( (is.na(INDIGENOUS)|is.na(NOT_INDIGENOUS)), 1, 0) ) 

meat.missing.152 <- meat.152 %>% filter(is.na(indigenous_high)) %>% filter(!TypeMeat %in% c('OTHER', 'RODENTS')) %>% 
                    mutate(Type = ifelse(is.na(INDIGENOUS), 'NOT_INDIGENOUS', 'INDIGENOUS'),
                           VoPFAO=ifelse(is.na(INDIGENOUS), NOT_INDIGENOUS, INDIGENOUS)) %>% 
                    left_join(meat.codes, by=c('TypeMeat'='TypeProduct', 'Type'='Indigenous')) %>% select(sel_cols)

meat.other.152 <- VoP_FAOSTAT2 %>% filter(Item.Code %in% c(1163, 1073, 1166, 1151)) %>% select(sel_cols)

VoP_FAOSTAT.meat.152 <- rbind(meat.missing.152, meat.other.152)

VoP_FAOSTAT.nonmeat.152 <- VoP_FAOSTAT2 %>% filter(Item.Code<=1500) %>% dplyr::filter(!grepl("Meat", Item)) %>% select(sel_cols)

VoP_FAOSTAT2.f <- rbind(VoP_FAOSTAT.nonmeat.152, VoP_FAOSTAT.meat.152)

VoP_FAOSTAT <- rbind(VoP_FAOSTAT1.f,VoP_FAOSTAT2.f) %>% mutate(VoPFAO=VoPFAO*1000)

faostat.vop.testimate <- VoP_FAOSTAT %>% group_by(CountryCode, Year) %>% summarise(VoPFAO_est = sum(VoPFAO, na.rm = T)) %>% ungroup() %>% 
                         mutate(Item='Agriculture', Item.Code=2051) %>% mutate(CountryCode=ifelse( CountryCode %in% EU28, "EUR",CountryCode)) %>% 
                          group_by(CountryCode, Item, Item.Code, Year) %>% 
                          summarise(VoPFAO_est=sum(VoPFAO_est, na.rm = T)) %>% ungroup()

## Check for total agricultural production 
faostat.vop.57 <- faostat.vop %>% filter(Item.Code==2051) %>% filter(element_code==57) %>% 
                      mutate(VoPFAO=VoPFAO*1000) %>% select(-element, -element_code) 

country.total <- faostat.vop.57 %>% select(CountryCode, Year) %>% distinct() %>% mutate(totalexist =1)

faostat.vop.152 <- faostat.vop %>% filter(Item.Code==2051) %>% filter(element_code==152) %>% left_join(country.total) %>% filter(is.na(totalexist)) %>% 
                    mutate(VoPFAO=VoPFAO*1000) %>% select(-element, -element_code, -totalexist) 
  
faostat.vop.total <- rbind(faostat.vop.57, faostat.vop.152) %>% 
                      mutate(CountryCode=ifelse( CountryCode %in% EU28, "EUR",CountryCode)) %>% group_by(CountryCode, Item, Item.Code, Year) %>% 
                      summarise(VoPFAO=sum(VoPFAO, na.rm = T)) %>% ungroup() %>% 
                      right_join(faostat.vop.testimate) %>% mutate(factor_prod = VoPFAO/VoPFAO_est) %>% 
                      select(CountryCode, Year, factor_prod)
                      
VoP_FAOSTAT.rescaled <- VoP_FAOSTAT %>% mutate(CountryCode=ifelse( CountryCode %in% EU28, "EUR",CountryCode)) %>% 
                        group_by(CountryCode, Item, Item.Code, Year) %>% 
                        summarise(VoPFAO=sum(VoPFAO, na.rm = T)) %>% ungroup() %>% 
                          left_join(faostat.vop.total) %>% mutate(VoPFAO=VoPFAO*factor_prod) %>% select(-factor_prod)


# VoP_FAOSTAT <- VoP_FAOSTAT1 %>% mutate(VoPFAO=VoPFAO*1000)

# VoP_FAOSTAT.Det <- VoP_FAOSTAT%>%dplyr::filter(Item.Code <=1500)%>%
#                     mutate(AGGCODE=ifelse(Item.Code<=850,"x2041","x2044"),
#                            CATCODE=ifelse(Item.Code<=850,"CRP","LVS"))%>%
#                     left_join(COUNTRYFAO)%>%na.omit()%>%
#                     select(-Area.Code)%>%rename(CountryCode=ISO3_CODE)%>%
#                     mutate(CountryCode=ifelse( CountryCode %in% EU28, "EUR",CountryCode)) %>%
#                     group_by(CountryCode, Item, Item.Code,AGGCODE,CATCODE, Year )%>%
#                     summarize(VoPFAO=sum(VoPFAO,na.rm=TRUE)) %>% ungroup() %>%
#                     left_join(list.MPS.smp)%>%mutate(MPSx=ifelse(is.na(MPS),"noM","MPS"),
#                                    GRPCODE=paste0("VoP_",MPSx,"_",CATCODE))
# #
# VoP_FAOSTAT.total <- VoP_FAOSTAT%>%dplyr::filter(Item.Code <=1500)%>%
#                       mutate(AGGCODE=ifelse(Item.Code<=850,"x2041","x2044"),
#                              CATCODE=ifelse(Item.Code<=850,"CRP","LVS"))%>%
#                       left_join(COUNTRYFAO)%>%na.omit()%>%
#                       select(-Area.Code)%>%rename(CountryCode=ISO3_CODE)%>%
#                       mutate(CountryCode=ifelse( CountryCode %in% EU28, "EUR",CountryCode))%>%
#                       group_by(Year,CountryCode)%>%
#                       summarize(VoPFAO=sum(VoPFAO,na.rm=TRUE))%>%mutate(AGPROCODE="xAGR")
#
# VoP_NRP.Ag <- VoP_NRP%>%group_by(CountryCode,Year)%>%
#               summarize(VoP_MPS_REF=sum(Value_PRF,na.rm=TRUE),
#                         VoP_MPS_PPP=sum(Value_PPP,na.rm=TRUE))%>%ungroup()
#
# VoP_MPS.Ag <- VoP_FAOSTAT.Det %>% group_by(Year,CountryCode,GRPCODE) %>%
#               summarize(VoPFAO=sum(VoPFAO,na.rm=TRUE))%>%ungroup() %>%
#               spread(GRPCODE,VoPFAO) %>%
#               gather(GRPCODE,VoPFAO,-c(Year,CountryCode)) %>%
#               mutate(VoPFAO=coalesce(VoPFAO,0))%>%spread(GRPCODE,VoPFAO) %>%
#               mutate(shMPS=(VoP_MPS_CRP+VoP_MPS_LVS)/(VoP_MPS_CRP+VoP_MPS_LVS+VoP_noM_CRP+VoP_noM_LVS)) %>%
#               left_join(VoP_NRP.Ag) %>%
#               left_join(NRP_COUNTRY) %>%
#               select(-NRA_pct1, -NRA_pct2, -Country_Code)%>%rename(Country_Code=CountryCode)
#
# factor.prod <- VoP_MPS.Ag %>% mutate(factorProd= (VoP_MPS_CRP+VoP_MPS_LVS)/VoP_MPS_REF) %>%
#                select(Country_Code, Year, factorProd) %>% na.omit()



# VoP_FAOSTAT.Ag <- VoP_FAOSTAT %>% dplyr::filter(Item.Code %in% c(2041,2044,2051))%>%
#                   select(-Item) %>% spread(Item.Code,PROD_USD) %>% mutate(chk=`2041`+`2044`,chk2=`2041`+`2044`-`2051`)

list.products.fao1 <- VoP_FAOSTAT1.f %>% select(Item.Code) %>% distinct
list.products.fao2 <- VoP_FAOSTAT2.f %>% select(Item.Code, Item) %>% distinct
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
                                                          TRUE ~ Item.Code))%>%
                    left_join(list.products.fao) %>% left_join(PDCT_LABEL)

## aggregate products such as GCT, ACT etc. 
product.aggregate <- read_excel("./Mapping/Aggregate_Commodity.xlsx", sheet = 'AG_COM_Long')

# FandV.FAO <- list.products.fao%>%dplyr::filter(Item.Code>=449 & Item.Code<=619)

list.code.agInc.missing <- list.code.agInc%>%dplyr::filter(is.na(Item)) %>% 
                            left_join(product.aggregate, by=c('PDCT_DESC'='AGCOMNAME')) %>% 
                           filter(is.na(Item.y)) %>% select(-Item.y, -Item_Code) %>% rename(Item=Item.x)


# fv.match <- product.aggregate %>% filter(AGCOMNAME=='Fruits and vegetables') %>% left_join(FandV.FAO) %>% filter(is.na(Item.Code))

add.code.agInc <- data.frame(PDCT_CODE=c("c866","c866","c866","c1058","c1035","c1035","c977","c977","c1015","c1015","c1015","c1017","c1017", "c1058", "c866"),
                             Item.Code=c(867,947,944         ,1058,1035,1055      ,977, 1012       , 1015     ,1016,1020,1032,1017, 1094, 972))

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
                     filter(!(Item.Code %in% c(26999, 1899, 1069,1073,1080,1089))) 


list.MPS <- merge(MPS_PROD_ISO,list.code.agInc.f, by=c("PDCT_CODE"))%>%mutate(MPS=1) %>% filter(!Item.Code %in% c(1070, 1084, 1087)) %>% 
            filter(!(PDCT_CODE=='c866' & (Item.Code %in% c(947, 972)) ) )
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
                    mutate(MPS=0) %>% 
                    group_by(CountryCode,Item, Item.Code, Year, MPS) %>% 
                    summarize(VoPFAO=sum(VoPFAO, na.rm = T)) %>% ungroup() 

# chile <- VoP_FAOSTAT %>% dplyr::filter(Item.Code <=1500) %>%
#          left_join(COUNTRYFAO) %>% na.omit() %>%
#         select(-Area.Code) %>% rename(CountryCode=ISO3_CODE) %>% filter(CountryCode=='CHL') %>% left_join(list.MPS) %>% 
#          mutate(MPS=ifelse(is.na(MPS), 0, MPS)) %>% group_by(CountryCode, Year, MPS) %>% summarise(VoPFAO=sum(VoPFAO)) %>% 
#          ungroup() %>% mutate(MPS=ifelse(MPS==1, 'MPS', 'NMPS')) %>% 
#          spread(MPS, VoPFAO) %>% left_join(XE.TOTAL, by=c('CountryCode'='COUNTRY_CODE', 'Year'='YEAR')) %>% 
#          select(-XE, -SOURCE, -COUNTRY_LABEL) %>% mutate(VPFAO_TOTAL = MPS+NMPS, total_rat=TOTAL/VPFAO_TOTAL,
#                                                          XE_rat = NONMPS/NMPS)

theta <- VoP_FAOSTAT.nMPS %>% group_by(CountryCode, Year) %>% summarise(VoPFAO=sum(VoPFAO, na.rm=T)) %>% 
         left_join(XE.FINAL, by=c('CountryCode'='COUNTRY_CODE', 'Year'='YEAR')) %>% 
         mutate(theta_fg=VP_XE/VoPFAO, theta_fg=ifelse(is.na(theta_fg), 1, theta_fg) ) %>% 
         left_join(NRP_TOTAL %>% select(CountryCode, Year, NRP)) %>% 
         mutate(NRP=NRP/100, NRP=ifelse(is.na(NRP), 0, NRP), theta_rp=abs(theta_fg/(1+NRP))) %>% 
         select(-VoPFAO, -VP_XE, -NRP)

theta_dist <- theta %>% filter(theta_fg<=0.2 | theta_fg>=1.5)

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

VoP_NMPS_compare <- VoP_NMPS_align %>% group_by(CountryCode, Year) %>% 
                    summarise(VoP_FG_hat=sum(VoP_FG_hat, na.rm = T), VoP_RP_hat=sum(VoP_RP_hat, na.rm = T)) %>% 
                    left_join(XE.FINAL, by=c('CountryCode'='COUNTRY_CODE', 'Year'='YEAR')) %>% 
                    mutate(equality.ratio_FG = VP_XE/VoP_FG_hat)

# write.csv(VoP_NMPS_compare, 'QualityCheckFiles/VoP_NonMPS_Alignment.csv', na='', row.names = F)
## Combining value of production for NRP and non NRP commodities
VoP.Complete <- VoP_MPS_align.f %>% rbind(VoP_NMPS_align) %>% arrange(CountryCode, Item.Code, Year) %>% 
                mutate(RelevantProduction=VoP_RP_hat) %>% select(-MPS) %>% 
                mutate(Item.Code=ifelse(Item.Code==867, 866, ifelse((CountryCode=='UGA' & Item.Code==1032), 1017, Item.Code) ))

## Computing VoP for aggregate as well as non NRP commodities 
VoP.NotinNRP <- VoP.Complete %>% right_join(list.payment %>% select(-NotinNRP) %>% 
                                               mutate(Item.Code=as.character(ifelse(Item.Code==867, 866, Item.Code))) ) %>% 
                 group_by(CountryCode, PDCT_CODE, Year) %>% 
                 summarise(VoP_FG_hat=sum(VoP_FG_hat, na.rm = T), VoP_RP_hat=sum(VoP_RP_hat, na.rm = T)) %>% ungroup() %>% 
                 mutate(VoP_FG_hat=ifelse(VoP_FG_hat==0, NA, VoP_FG_hat), VoP_RP_hat=ifelse(VoP_RP_hat==0, NA, VoP_RP_hat)) %>% 
                 mutate(RelevantProduction2=VoP_RP_hat)

# benin <- VoP.NotinNRP %>% filter(PDCT_CODE=='c224001') %>% filter(Year==2018) %>% group_by(CountryCode, PDCT_CODE, Year) %>% 
#                   summarise(vop=sum(RelevantProduction, na.rm = T))

# com.notInNRP <- VoP.NotinNRP %>% left_join(PDCT_LABEL) %>% select(PDCT_DESC, PDCT_CODE) %>% distinct()
# com.complete <- VoP.Complete %>% mutate(PDCT_CODE=paste0('c', Item.Code)) %>% left_join(PDCT_LABEL) %>% filter(is.na(PDCT_DESC)) %>%
#                 select(Item.Code) %>% distinct()

vop_null <- VoP.NotinNRP %>% filter(is.na(VoP_RP_hat))


VoP.mapped <- VoP.Complete %>% left_join(COUNTRY, by = c("CountryCode"="ISO3CODE")) %>% 
              mutate(AGPROCODE=paste0('c', Item.Code), Item_Code=as.integer(Item.Code), 
                    CATPROD = case_when(Item_Code<=850~'CRP', 
                                        (Item_Code>850 & Item_Code<1500)~'LVS', 
                                        (Item_Code %in% c(26999, 29000, 30000))~'CRP', 
                                         is.na(Item_Code)~'CRP', 
                                         TRUE~as.character(Item_Code))) %>% select(-Item_Code)


# VoP- aggregation ----------------------------------------------------------------------------

# countryincluded <- CONSOLIDATED_NRA %>% select(Country_Code) %>% distinct() %>% mutate(included=1)

VoP.country.allag <- VoP.mapped %>% group_by(CountryCode, Year) %>% 
                      summarise(RelevantProduction=sum(RelevantProduction, na.rm = T)) %>% ungroup() 

VoP.country.sector <- VoP.mapped %>% group_by(CountryCode, CATPROD, Year) %>% 
                      summarise(RelevantProduction=sum(RelevantProduction, na.rm = T)) %>% ungroup() 

VoP.income.sector <- VoP.mapped %>% group_by(WBCODE2015, CATPROD, Year)%>%
                      summarize(RelevantProduction=sum(RelevantProduction,na.rm=TRUE)) %>% ungroup()

VoP.income.allag <- VoP.mapped %>% group_by(WBCODE2015, Year)%>%
                    summarize(RelevantProduction=sum(RelevantProduction,na.rm=TRUE)) %>% ungroup()


VoP.region.sector <- VoP.mapped %>% group_by(REGIONFAOCODE, CATPROD, Year) %>%
                      summarize(RelevantProduction=sum(RelevantProduction,na.rm=TRUE)) %>%ungroup()

VoP.region.allag <- VoP.mapped %>% group_by(REGIONFAOCODE,Year) %>%
                    summarize(RelevantProduction=sum(RelevantProduction,na.rm=TRUE)) %>%ungroup()

VoP.world.sector <- VoP.mapped %>% group_by(CATPROD, Year)%>%
                    summarize(RelevantProduction=sum(RelevantProduction,na.rm=TRUE)) %>% ungroup()

VoP.world.allag <- VoP.mapped %>% group_by(Year)%>%
                    summarize(RelevantProduction=sum(RelevantProduction,na.rm=TRUE)) %>% ungroup()

# write.csv(VoP.world.allag, 'QualityCheckFiles/VoP_World_AllAg.csv', row.names = F)

VoP_MPS.ag <- VoP_MPS_align.f %>% mutate(Item_Code=as.integer(Item.Code), CATPROD = case_when(Item_Code<=850~'CRP',
                                                             (Item_Code>850 & Item_Code<1500)~'LVS',
                                                             (Item_Code %in% c(26999, 29000, 30000))~'CRP',
                                                             is.na(Item_Code)~'CRP',
                                                             TRUE~as.character(Item_Code))) %>%
              group_by(CountryCode, CATPROD, Year) %>%
              summarise(Production_MPS=sum(VoP_RP_hat)) %>% ungroup()

VoP_NMPS_check  <- VoP.country.sector %>% left_join(VoP_MPS.ag) %>% na.omit() %>% mutate(VoP_NMPS=RelevantProduction-Production_MPS) %>%
                    select(-RelevantProduction, -Production_MPS)

VoP_NMPS.crplvs <- VoP_NMPS_align %>% 
                  mutate(CATPROD = case_when(Item.Code<=850~'CRP', 
                                            (Item.Code>850 & Item.Code<1500)~'LVS', 
                                            TRUE~as.character(Item.Code))) %>% 
                  group_by(CountryCode, CATPROD, Year) %>% 
                  summarise(RelevantProduction=sum(VoP_RP_hat)) %>% ungroup() %>% 
                  left_join(NRP_TOTAL) %>% na.omit() %>% mutate(Support_USD=RelevantProduction*NRP/100) %>% select(-NRP)
                  
  
# write.csv(VoP.mapped, 'QualityCheckFiles/VoP_Mapped.csv')

## dealing with wine commodity, non-existent in FAOSTAT
wine <- VoP.Complete %>% filter(CountryCode=='EUR' & Item.Code==560) %>%  
        mutate(PDCT_CODE='c564') %>% select(CountryCode, PDCT_CODE, Year, RelevantProduction) %>% 
        rename(RelevantProduction3=RelevantProduction)


# Generate NRA details file for Payments and NRP database ---------------------------------------------

col_ord <- c('Source','Country_Label','Country_Code','Commodity_Label', 'Commodity_Code', 'Commodity_Type',
             'Year', 'Category', 'Support_USD', 'NRA_Cat', 'AGPROCODE', 'CATPROD', 'RelevantProduction')

CONSOLIDATED_NRA0a1 <- PAYMENTS%>%select(-Value_LCU)%>% 
                      mutate(NRA_Cat=case_when( Category=="A2"~"Outputs",
                                                Category=="B"~"Inputs",
                                                TRUE~"Others") ) %>%  
                      mutate(NRA_Cat=factor(NRA_Cat,level=c("NRP","Outputs","Inputs","Others"))) %>%
                      left_join(PDCT_fullmap0) %>%
                      left_join(PDCT_FULL, by =c("AGPROCODE"="PDCT_CODE")) %>%
                      mutate(CATPROD=case_when(PDCT1=="Animal products"~"LVS",
                                               PDCT1=="NAL"~'NAL', 
                                               TRUE~"CRP") ) %>% 
                      left_join(PDCT_LABEL, by=c('AGPROCODE'='PDCT_CODE'), keep=T) %>% mutate(Commodity_Label=PDCT_DESC, Commodity_Code=PDCT_CODE) %>% 
                      select(-starts_with("PDCT")) %>%
                       rename(Support_USD=Value_USD) %>%  
                       left_join(VoP.Complete %>% mutate(PDCT_CODE=paste0('c', Item.Code)) %>% 
                                   select(CountryCode, PDCT_CODE, Year, RelevantProduction), 
                                    by=c('Country_Code'='CountryCode', 'AGPROCODE'='PDCT_CODE', 'Year'='Year')
                                   ) %>% 
                       left_join(VoP.NotinNRP %>% select(CountryCode, PDCT_CODE, Year, RelevantProduction2), 
                                 by=c('Country_Code'='CountryCode', 'AGPROCODE'='PDCT_CODE', 'Year'='Year')) %>% 
                       mutate(RelevantProduction=ifelse(is.na(RelevantProduction), RelevantProduction2, RelevantProduction)) %>% 
                       left_join(wine, by=c('Country_Code'='CountryCode', 'AGPROCODE'='PDCT_CODE', 'Year'='Year')) %>%
                       mutate(RelevantProduction=ifelse(is.na(RelevantProduction), RelevantProduction3, RelevantProduction)) %>%
                       select(col_ord)

CONSOLIDATED_NRA0a2 <- CONSOLIDATED_NRA0a1 %>% dplyr::filter(is.na(RelevantProduction)) %>% 
                              filter(!Commodity_Label %in% c('Feed crops', 'Biomass')) %>% 
                       mutate(Commodity_Label=paste0('Non-MPS other crops - ', Country_Label), 
                              Commodity_Code=case_when(Country_Code=='BDI'~'c113002', 
                                                       Country_Code=='BFA'~'c113003',
                                                       Country_Code=='ETH'~'c113004',
                                                       Country_Code=='MOZ'~'c113005',
                                                      TRUE~Commodity_Code), 
                              AGPROCODE=Commodity_Code) %>% 
                        group_by(Source, Country_Label, Country_Code, Commodity_Label, Commodity_Code,Commodity_Type,    
                                 Year,Category,NRA_Cat, AGPROCODE, CATPROD, RelevantProduction) %>% 
                         summarise(Support_USD=sum(Support_USD, na.rm = T)) %>% ungroup() %>% select(-RelevantProduction)

country.nra0a2 <- CONSOLIDATED_NRA0a2 %>% select(Country_Code) %>% distinct() %>% flatten() %>% unlist

vop.othcrops.mafap <- VoP.mapped %>% filter((CountryCode %in% country.nra0a2) & CATPROD=='CRP') %>% 
                      left_join(list.MPS %>% mutate(Item.Code=as.character(Item.Code))) %>% 
                      filter(is.na(MPS)) %>% group_by(LISTNAME_EN, Year) %>% 
                      summarise(RelevantProduction=sum(RelevantProduction, na.rm = T)) %>% ungroup() %>% 
                      mutate(Commodity_Label=paste0('Non-MPS other crops - ',LISTNAME_EN )) %>% rename(Country_Label=LISTNAME_EN)

CONSOLIDATED_NRA0a3 <- CONSOLIDATED_NRA0a2 %>% left_join(vop.othcrops.mafap) %>% select(col_ord)

CONSOLIDATED_NRA0a <- CONSOLIDATED_NRA0a1 %>% dplyr::filter(!is.na(RelevantProduction)) %>% rbind(CONSOLIDATED_NRA0a3) %>% 
                      rbind(CONSOLIDATED_NRA0a1 %>% filter(Commodity_Label %in% c('Feed crops', 'Biomass')))

# CONSOLIDATED_NRA0a.missing2 <- CONSOLIDATED_NRA0a%>%dplyr::filter(is.na(CATPROD))%>%
#                                select(AGPROCODE,Commodity_Label, Commodity_Code)%>%distinct()

CONSOLIDATED_NRA0b <- NRP_CSV %>%
                      mutate(Production_USD=ValueProduction_REF,
                             Support_USD=DistortionValue,
                             NRA_Cat="NRP",
                             Commodity_Type="Yes",
                             Commodity_Code=ProductCode,
                             Commodity_Label=ProductName,
                             Category='A1')%>%
                      mutate(NRA_Cat=factor(NRA_Cat,level=c("NRP","Outputs","Inputs","Others"))) %>%
                      rename(Country_Label=CountryName, Country_Code=CountryCode,AGPROCODE=ProductCode, 
                             RelevantProduction=Production_USD) %>%
                      select(-starts_with("PDCT")) %>%
                      left_join(PDCT_FULL, by =c("AGPROCODE"="PDCT_CODE")) %>%
                      mutate(CATPROD=case_when(PDCT1=="Animal products"~"LVS",
                                               PDCT1=="NAL"~'NAL',
                                               TRUE~"CRP") ) %>%
                      select(col_ord)

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
                             Commodity_Label="Livestocks, Non-MPS",
                             Commodity_Code="nLVS",
                             AGPROCODE="nLVS")

label_NRP_add <- CONSOLIDATED_NRA0b%>%select(Country_Code, Country_Label, Source, Year) %>% distinct

CONSOLIDATED_NRA0c <- rbind(NRP_nonMPS.AddCRP,NRP_nonMPS.AddLVS) %>% rename(Country_Code=CountryCode) %>% 
                      left_join(label_NRP_add) %>%
                      select(check.fields.b) %>% na.omit()


# Consolidate two databases and compute NRA at details level -------------------------------------------------------------------

CONSOLIDATED_NRA <- rbind(CONSOLIDATED_NRA0a, CONSOLIDATED_NRA0b,CONSOLIDATED_NRA0c) %>% 
                      arrange(Country_Label, Commodity_Label, Year, Category) %>% 
                     left_join(COUNTRY, by = c("Country_Code"="ISO3CODE"))%>%
                      mutate(rate= Support_USD/RelevantProduction)

missing.tuples <- CONSOLIDATED_NRA %>% filter(is.na(RelevantProduction)) %>% 
                  select(Country_Label, Country_Code, Commodity_Label, AGPROCODE, Year) %>% 
                  mutate(Item.Code=as.integer(str_replace(AGPROCODE, 'c',''))) %>% select(-AGPROCODE) %>% distinct()


table(CONSOLIDATED_NRA$Category, CONSOLIDATED_NRA$NRA_Cat)

table(CONSOLIDATED_NRA$CATPROD, CONSOLIDATED_NRA$NRA_Cat)

support.negeative <- CONSOLIDATED_NRA %>% filter(NRA_Cat %in% c('Outputs', 'Inputs', 'Others')) %>% filter(rate<0)
support.higherthan1 <- CONSOLIDATED_NRA %>% filter(NRA_Cat %in% c('Outputs', 'Inputs', 'Others')) %>% filter(rate>1)

# write.csv(missing.tuples, 'QualityCheckFiles/Missing_Tuples_57.csv', na='', row.names = F)

dir.create(paste0("NRA_Output/Final/",Sys.Date(), sep = ""))

write.csv(CONSOLIDATED_NRA, paste0("NRA_Output/Final/", Sys.Date(),"/Detailed_NRA.csv", sep = ""), row.names = FALSE)

# write.csv(CONSOLIDATED_NRA, "NRA_Output_Files/Detailed_NRA.csv",row.names = FALSE)


# Check for in production data is available for missing tuples --------------------------------

faostat.production <- FAOSTAT::read_faostat_bulk("FAOSTAT/Production_Crops_Livestock_E_All_Data_(Normalized).zip") %>%
                      filter(year>=2005 & year<=2018) 

check.production <- missing.tuples %>% left_join(faostat.production, by=c('Country_Label'='area', 'Commodity_Label'='item', 
                                                                          'Item.Code'='item_code', 'Year'='year')) %>% 
                    filter(!is.na(value))

write.csv(check.production, 'QualityCheckFiles/Country_Commodity_Missing_Tuples.csv', na='', row.names = F)

# Data coverage -------------------------------------------------------------------------------

Database.Coverage.0 <- CONSOLIDATED_NRA%>%select(Country_Code, Year)%>%distinct()%>%mutate(Included=1)
Database.Coverage.1 <- Database.Coverage.0%>%group_by(Year)%>%summarize(Number_Countries=n())

VoP_FAOSTAT.final <-  VoP_FAOSTAT.rescaled %>% group_by(CountryCode, Year) %>% summarise(VoPFAO=sum(VoPFAO, na.rm = T)) %>% ungroup() %>% 
                      left_join(Database.Coverage.0, by=c('CountryCode'='Country_Code', 'Year'='Year'))

VoP_FAOSTAT.coverage <- VoP_FAOSTAT.final %>% group_by(Year, Included)%>%
                        summarize(Production=sum(VoPFAO,na.rm=TRUE))%>%ungroup%>%group_by(Year)%>%
                        mutate(ShareValueOfProduction=Production/sum(Production))%>%
                        dplyr::filter(Included==1) %>%select(-Production) %>%
                        left_join(Database.Coverage.1)%>%select(-Included)

write.csv(VoP_FAOSTAT.coverage, "NRA_Output/Coverage_NRA.csv",row.names = FALSE)  

# NRA - Aggregation ---------------------------------------------------------------------------------

AGGREGATED_NRA0 <- CONSOLIDATED_NRA %>% dplyr::filter(CATPROD%in% c("CRP","LVS")) %>%
                    group_by(Year, Country_Code,LISTNAME_EN,REGIONFAOCODE,REGIONNAME, WBCODE2015, WBNAME2015, NRA_Cat, CATPROD)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE))%>%mutate(AGPROCODE=paste0("x",CATPROD)) %>%
                    left_join(VoP.country.sector, by=c('Country_Code'='CountryCode', 'CATPROD'='CATPROD', 'Year'='Year')) %>% 
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="CountryXSector")

AGGREGATED_NRA1 <- CONSOLIDATED_NRA %>% dplyr::filter(CATPROD %in% c("NAL")) %>%
                    group_by(Year, Country_Code,LISTNAME_EN,REGIONFAOCODE,REGIONNAME, WBCODE2015, WBNAME2015, NRA_Cat , CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% ungroup() %>% mutate(AGPROCODE="xAGR") %>%
                    left_join(VoP.country.allag, by=c('Country_Code'='CountryCode', 'Year'='Year') ) %>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="CountryXSector")

AGGREGATED_NRA2 <- CONSOLIDATED_NRA %>%
                    group_by(Year, Country_Code,LISTNAME_EN,REGIONFAOCODE,REGIONNAME, WBCODE2015, WBNAME2015,NRA_Cat)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE))%>%mutate(CATPROD="TOTAL")%>%mutate(AGPROCODE="xAGR") %>%
                    left_join(VoP.country.allag, by=c('Country_Code'='CountryCode', 'Year'='Year') )%>% 
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="CountryXAllAg")

AGGREGATED_NRA3 <- CONSOLIDATED_NRA %>%
                    group_by(Year, Country_Code,LISTNAME_EN,REGIONFAOCODE,REGIONNAME, WBCODE2015, WBNAME2015)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE))%>%  ungroup() %>% mutate(CATPROD="TOTAL")%>%mutate(AGPROCODE="xAGR")%>%
                    mutate(NRA_Cat="NRA_Total") %>%
                    right_join(VoP.country.allag, by=c('Country_Code'='CountryCode', 'Year'='Year')) %>% filter(!is.na(Support_USD)) %>% 
                    mutate(NRA=Support_USD/RelevantProduction)%>%mutate(AGGREGATION="CountryXAllAg")

AGGREGATED_NRA0x <- AGGREGATED_NRA0 %>%
                    group_by(Year, WBCODE2015, WBNAME2015, NRA_Cat, AGPROCODE,CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.income.sector)%>%
                    mutate(NRA=Support_USD/RelevantProduction)%>%mutate(AGGREGATION="IncomeLevelxSector")

AGGREGATED_NRA1x <- AGGREGATED_NRA1 %>%
                    group_by(Year, WBCODE2015, WBNAME2015, NRA_Cat , AGPROCODE,CATPROD)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.income.allag) %>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="IncomeLevelxSector")

AGGREGATED_NRA2x <- AGGREGATED_NRA2 %>%
                    group_by(Year, WBCODE2015, WBNAME2015, AGPROCODE,CATPROD,NRA_Cat) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.income.allag) %>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="IncomeLevelxAllAg")

AGGREGATED_NRA3x <- AGGREGATED_NRA3 %>%
                    group_by(Year, WBCODE2015, WBNAME2015, AGPROCODE,CATPROD,NRA_Cat) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.income.allag) %>%
                    mutate(NRA=Support_USD/RelevantProduction)%>%mutate(AGGREGATION="IncomeLevelxAllAg")

AGGREGATED_NRA0y <- AGGREGATED_NRA0 %>%
                    group_by(Year, REGIONFAOCODE,REGIONNAME, NRA_Cat, AGPROCODE,CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.region.sector) %>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="RegionxSector")

AGGREGATED_NRA1y <- AGGREGATED_NRA1 %>%
                    group_by(Year, REGIONFAOCODE,REGIONNAME, NRA_Cat , AGPROCODE,CATPROD)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.region.allag) %>%
                    mutate(NRA=Support_USD/RelevantProduction)%>%mutate(AGGREGATION="RegionxSector")

AGGREGATED_NRA2y <- AGGREGATED_NRA2 %>%
                    group_by(Year, REGIONFAOCODE,REGIONNAME, AGPROCODE,CATPROD,NRA_Cat) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.region.allag) %>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="RegionxAllAg")

AGGREGATED_NRA3y <- AGGREGATED_NRA3 %>%
                    group_by(Year, REGIONFAOCODE,REGIONNAME, AGPROCODE,CATPROD,NRA_Cat) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.region.allag) %>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="RegionxAllAg")

AGGREGATED_NRA0w <- AGGREGATED_NRA0 %>%
                    group_by(Year, NRA_Cat, AGPROCODE,CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.world.sector) %>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="WorldxSector")

AGGREGATED_NRA1w <- AGGREGATED_NRA1 %>%
                    group_by(Year, NRA_Cat , AGPROCODE, CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.world.allag)%>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="WorldxSector")

AGGREGATED_NRA2w <- AGGREGATED_NRA2 %>%
                    group_by(Year, NRA_Cat, AGPROCODE,CATPROD)%>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.world.allag) %>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="WorldxAllAg")

AGGREGATED_NRA3w <- AGGREGATED_NRA3 %>%
                    group_by(Year, NRA_Cat, AGPROCODE,CATPROD) %>%
                    summarize(Support_USD=sum(Support_USD,na.rm=TRUE)) %>% left_join(VoP.world.allag) %>%
                    mutate(NRA=Support_USD/RelevantProduction) %>% mutate(AGGREGATION="WorldxAllAg")


AGGREGATED_NRA <- rbind(AGGREGATED_NRA0,AGGREGATED_NRA1,AGGREGATED_NRA2,AGGREGATED_NRA3,
                        AGGREGATED_NRA0x,AGGREGATED_NRA1x,AGGREGATED_NRA2x,AGGREGATED_NRA3x,
                        AGGREGATED_NRA0y,AGGREGATED_NRA1y,AGGREGATED_NRA2y,AGGREGATED_NRA3y,
                        AGGREGATED_NRA0w,AGGREGATED_NRA1w,AGGREGATED_NRA2w,AGGREGATED_NRA3w) %>% mutate(NRA=NRA*100) %>% 
                  mutate(CATPROD=case_when(CATPROD=='CRP'~'Crops', 
                                           CATPROD=='LVS'~'Livestock', 
                                           CATPROD=='NAL'~'Non-allocated', 
                                           CATPROD=='TOTAL'~'Aggregate'))

write.csv(AGGREGATED_NRA, paste0("NRA_Output/Final/", Sys.Date(),"/Aggregated_NRA.csv", sep = ""), row.names = FALSE)

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




