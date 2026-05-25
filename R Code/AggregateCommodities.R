
library(readxl)
library(FAOSTAT)
library(tidyverse)
source("Mapping.R")


# selected columns ----------------------------------------------------------------------------
cols.longlist <-  c('AGCOMNAME', 'Item', 'Item_Code', 'AGDCOMDIC', 'AGDCOMCODE')

COUNTRYFAO <- FAOcountryProfile%>%select(FAOST_CODE,ISO3_CODE)%>%rename(Area.Code=FAOST_CODE) %>%
              mutate(ISO3_CODE=case_when(Area.Code==277~'SSD',
                             TRUE~ISO3_CODE)) %>% filter(!is.na(ISO3_CODE))

# Function to concat ------------------------------------------------------------------------------------
concat.function <- function(dataconcat){
  
  dataag <- dataconcat %>% group_by(AGCOMNAME) %>% mutate(AGDCOMDIC = paste0(Item, collapse = "; "), 
                                 AGDCOMCODE=paste0(Item_Code, collapse = "; ")) %>% 
    ungroup() %>% select(cols.longlist)
}


# NRP and Payments Database -------------------------------------------------------------------
nrp <- read.csv(file="Source/PUBLICVIEW_2024/PUBLICVIEW.csv")%>%
            dplyr::filter(Category=="COUNTRY_PRODUCT") %>% 
            mutate(CountryName=case_when(CountryName=='Korea, Republic of'~'Republic of Korea', 
                                         CountryName=='Tanzania, United Republic of '~'United Republic of Tanzania', 
                                         CountryName=='Bolivia, Plurinational State of'~'Bolivia (Plurinational State of)',
                                         CountryName=='Turkey'~"Türkiye",
                                         TRUE~CountryName))

payments <- read.csv(file="Source/PaymentData_2024/Consolidated_Payment_Data.csv") %>%
            dplyr::filter(Year>=2004) %>%
            mutate(Source=ifelse(Source %in% c("OECD","FAO","IDB"), Source, "OthSources")) %>%
            dplyr::filter(Year<=2023) %>% mutate(Commodity_Code = ifelse(Commodity_Code== 50 & Country_Code=="JAM","XE",Commodity_Code)) %>% 
            mutate(Country_Label=case_when(Country_Label=='Russia'~'Russian Federation', 
                                           Country_Label=='Vietnam'~'Viet Nam', 
                                           Country_Label=='United States'~'United States of America', 
                                           Country_Label=='Korea'~'Republic of Korea', 
                                           Country_Label=='Bolivia'~'Bolivia (Plurinational State of)', 
                                           Country_Label=='Tanzania'~'United Republic of Tanzania',
                                           Country_Label=='Turkey'~"Türkiye",
                                           TRUE~Country_Label))

commodity <- read.csv("./Mapping/COMMODITY.csv", stringsAsFactors=FALSE) %>% select(FAOCODE, FAONAME, AGCOMNAME) %>% na.omit() 

add.code.milk <- data.frame(ProductCode=c(882, 882, 882, 882, 882), 
                            Item.Code=c(882,1020,982,951,1130))

nrp.commodity.o <- nrp %>% mutate(ProductCode=as.integer(str_replace(ProductCode, 'c',''))) %>%  
                 left_join(commodity, by=c('ProductCode'='FAOCODE')) %>% 
                 mutate(ProductName=ifelse(!is.na(FAONAME), FAONAME, ProductName)) %>% 
                select(CountryCode, CountryName,ProductName, ProductCode) %>% distinct() %>% 
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
                       ProductName=case_when(ProductName=='Pulses, nes'~'Pulses nes', TRUE~ProductName)) %>% 
                distinct() %>% mutate(MPS='MPS') %>% 
                left_join(add.code.milk) %>% mutate(Item.Code=coalesce(Item.Code, ProductCode)) %>% select(-ProductCode) %>% 
                rename(ProductCode=Item.Code) %>% 
                mutate(ProductName=case_when(ProductCode==1020~'Milk, whole fresh goat', 
                                             ProductCode==982~'Milk, whhole fresh sheep', 
                                             ProductCode==951~'Milk, whole fresh buffalo', 
                                             ProductCode==1130~'Milk, whole fresh camel', 
                                             TRUE~ProductName))

nrp.cotton <- nrp %>% filter(ProductCode=='c328') %>% 
              mutate(ProductName='Seed cotton', ProductCode=328) %>% mutate(MPS='MPS') %>% 
              select(CountryCode, CountryName, ProductName, MPS, ProductCode) %>% distinct()

nrp.sugar <- nrp %>% filter(ProductCode=='c156') %>% 
             mutate(ProductName='Sugar beet', ProductCode=157) %>% mutate(MPS='MPS') %>% 
              select(CountryCode, CountryName, ProductName, MPS, ProductCode) %>% distinct()

add.code.meat <- data.frame(ProductName=c("Meat, cattle","Meat, cattle","Meat, cattle","Meat, chicken","Meat, pig","Meat, pig","Meat, sheep",
                                         "Meat, sheep","Meat, goat","Meat, goat", "Meat, chicken", "Meat, cattle", 
                                         "Meat, chicken","Meat, chicken","Meat, chicken","Meat, chicken","Meat, chicken","Meat, chicken","Meat, chicken"),
                             Item.Code=c(867,947,944,1058,1035,1055,977,1012, 1032,1017, 1094, 972, 1069,1073,1080,1089,1070,1084,1087))



nrp.commodity <- rbind(nrp.commodity.o, nrp.cotton, nrp.sugar) %>% left_join(add.code.meat) %>% mutate(Item.Code=ifelse(is.na(Item.Code), ProductCode, Item.Code)) %>% 
                  select(-ProductCode) %>% rename(ProductCode=Item.Code) %>% select(-ProductName)


# write.csv(nrp.commidity, 'MPS_Commodity.csv', na = '', row.names = F)

# Country List --------------------------------------------------------------------------------
fao.country <- FAOcountryProfile%>%select(FAOST_CODE,ISO3_CODE)%>%rename(Area.Code=FAOST_CODE)

nrp.country <- read.csv(file="Source/PUBLICVIEW_2024/PUBLICVIEW.csv")%>%
              mutate(CountryName=case_when(CountryName=='Korea, Republic of'~'Republic of Korea', 
                                           CountryName=='Tanzania, United Republic of '~'United Republic of Tanzania', 
                                           CountryName=='Bolivia, Plurinational State of'~'Bolivia (Plurinational State of)',
                                           CountryName=='Turkey'~"Türkiye",
                                           TRUE~CountryName)) 

oecd.country <- nrp.country %>% filter(Source=='OECD') %>% select(CountryName) %>% distinct() %>% flatten()%>%unlist
mafap.country <- nrp.country %>% filter(Source=='FAO') %>% select(CountryName) %>% distinct() %>% flatten()%>%unlist
idb.country <- nrp.country %>% filter(Source=='Agrimonitor (IDB)') %>% select(CountryName) %>% distinct() %>% flatten()%>%unlist

list.country <- nrp.country%>% dplyr::filter(Category=="COUNTRY_TOTAL")%>% select(CountryName)%>% distinct() %>% 
                rbind(payments %>%select(Country_Label)%>%rename(CountryName=Country_Label) %>% distinct()) %>% 
                distinct() %>% flatten()%>%unlist

eu27 <- read.table("EU27.txt",col.names = FALSE)%>%flatten%>%unlist


# Commodity List by Groups --------------------------------------------------------------------
commodity.class <- read_excel('Source/Aggregate_Commodity_Composition.xlsx', sheet = 'Commodity_Class')

crops.livestock <- commodity.class %>% filter(Group %in% c('Crops', 'Livestock'))
fruits <- commodity.class %>% filter(Group=='Fruits') %>% rename(Fruits=Group)
vegetables <- commodity.class %>% filter(Group=='Vegetables excluding roots and tubers') %>% rename(Vegetables=Group)
f_v <- commodity.class %>% filter(Group=='Fruits and vegetables') %>% rename(F_V=Group)


faostat_voP <- FAOSTAT::read_faostat_bulk(".\\FAOSTAT\\Data_2024\\Value_of_Production_E_All_Data_(Normalized).zip")

faostat.commodity <- faostat_voP %>% filter(area!='China') %>%
                      left_join(COUNTRYFAO,by=c('area_code'='Area.Code')) %>% rename(CountryCode='ISO3_CODE') %>% 
                      mutate(area=ifelse(CountryCode=='TUR',"Türkiye",area)) %>% 
                      filter(!is.na(CountryCode)) %>% select(-area_code, -flag, -year_code) %>% 
                      rename(Area=area, Year=year, Item.Code=item_code, Item=item ) %>% 
                      select(Area, CountryCode, everything()) %>% filter(Year>=2004) %>% 
                      mutate(CountryCode=ifelse(CountryCode %in% eu27, "EUR",CountryCode)) %>% 
                      mutate(Area=ifelse(CountryCode=='EUR', 'European Union', ifelse(CountryCode=='CHN', 'China', Area))) %>% 
                      select(CountryCode, Area, Item.Code, Item) %>% distinct() %>% filter(Item.Code<=1500) 
                      # %>% filter(!grepl('indigenous', Item))

faostat.commodity.list <- faostat.commodity %>% select(Item) %>% distinct()

## check for missing commodity 
commodity.bycountry <- faostat.commodity %>% select(Area, Item, Item.Code) %>% distinct()

commodity.fao <- faostat.commodity %>% select(Item, Item.Code) %>% distinct() %>% 
                    left_join(commodity.class %>% filter(Group %in% c('Crops', 'Livestock')))

commodity.mysheet <- faostat.commodity %>% select(Item, Item.Code) %>% distinct() %>% 
                      right_join(commodity.class %>% filter(Group %in% c('Crops', 'Livestock')))


commodity.notinmysheet <- commodity.fao %>% filter(is.na(Item_Code))
commodity.notinfao <- commodity.mysheet %>% filter(is.na(Item.Code))

# commodity.complete <- faostat.commodity %>% select(Item, Item.Code) %>% distinct() %>% 
#                       full_join(commodity.class, by=c('Item.Code'='Item_Code')) %>% arrange(Group)
# 
# write.csv(commodity.complete, 'Source/COMMODITY_COMPLETE.csv', row.names = F )


# Commodity by MPS type -----------------------------------------------------------------------

item.map <- commodity.class %>% select(Item, Item_Code) %>% distinct() %>% rename(ProductName=Item)
commodity.bymps <- faostat.commodity %>%   
                   full_join(nrp.commodity, by=c('Area'='CountryName', 'CountryCode'='CountryCode', 'Item.Code'='ProductCode')) %>% 
                   mutate(MPS=ifelse(is.na(MPS), 'NonMPS', MPS)) %>%  
                   rename(CountryName=Area, ProductCode=Item.Code) %>% arrange(CountryName, Item) %>% 
                  left_join(item.map, by=c('ProductCode'='Item_Code')) %>% filter(!ProductCode %in% c(767, 257, 641)) %>% 
                  select(-Item) %>% rename(Item=ProductName) %>% 
                   left_join(crops.livestock) %>% left_join(f_v) %>% 
                   mutate(F_V=ifelse(is.na(F_V), 'NonFV', F_V), 
                          F_V=case_when(Item=='Fruits and vegetables'~'Fruits and vegetables', 
                                        TRUE~F_V)) %>%
                   mutate(MPS=case_when((CountryName=='China' & F_V=='Fruits and vegetables')~'MPS',
                                        (CountryName=='Israel' & F_V=='Fruits and vegetables')~'MPS',
                                        (CountryName=='Argentina' & F_V=='Fruits and vegetables')~'MPS',
                                        TRUE~MPS
                                        ))


# Non-MPS commodities -------------------------------------------------------------------------
livestock <- commodity.class %>% filter(Group=='Livestock') %>% filter(grepl('Meat',Item )) %>% mutate(MPS='NonMPS') %>% 
              rename(ProductName=Item, ProductCode=Item_Code)

all.nmps.o <- commodity.bymps %>% filter(MPS=='NonMPS') %>%  
            mutate(Source=case_when((CountryName %in% oecd.country)~'OECD', 
                                    (CountryName %in% idb.country)~'IDB', 
                                    (CountryName %in% mafap.country)~'FAO',
                                    TRUE~CountryName), 
                    CountryName=case_when(CountryName=='United States of America'~'United States', 
                                          CountryName=='Russian Federation'~'Russia', 
                                          CountryName=='Republic of Korea'~'Korea',
                                          TRUE~CountryName) ) 

nmps.meat <- all.nmps.o %>% filter(Group=='Livestock') %>% filter(grepl('Meat', Item)) 

# add.nmps.meat <- all.nmps.o %>% filter(Group=='Livestock') %>% filter(grepl('Meat', Item)) %>% 
#                   # mutate(Item=str_replace(Item, '(indigenous)', '')) %>% 
#                   select(-ProductCode, -Item_Code) %>% 
#                   left_join(livestock, by=c('Item'='ProductName', 'Group'='Group', 'MPS'='MPS')) %>% mutate(Item_Code=ProductCode) %>% 
#                   filter(!is.na(Item_Code)) %>% 
#                   select(CountryCode, CountryName, ProductCode, MPS, Item, Group, Item_Code, F_V, Source)

# all.nmps.meat <- rbind(nmps.meat, add.nmps.meat) %>% distinct()

nmps.nonmeat <- all.nmps.o %>% filter(!grepl('Meat', Item)) 

all.nmps <- rbind(nmps.nonmeat, nmps.meat)



  
mafap.nmps.lvs <- all.nmps %>% filter(Source=='FAO' & Group=='Livestock') %>% group_by(CountryName) %>% 
                  mutate(AGDCOMDIC = paste0(Item, collapse = "; "),
                          AGDCOMCODE = paste0(ProductCode, collapse = "; ") ) %>% ungroup() %>%
                  mutate(AGCOMNAME=paste0('Non-MPS other livestock - ', CountryName))
  
mafap.nmps.crp <- all.nmps %>% filter(Source=='FAO' & Group=='Crops') %>% group_by(CountryName) %>% 
                  mutate(AGDCOMDIC = paste0(Item, collapse = "; "),
                          AGDCOMCODE = paste0(ProductCode, collapse = "; ") ) %>% ungroup() %>%
                  mutate(AGCOMNAME=paste0('Non-MPS other crops - ', CountryName))


oecd.idb.nmps <- all.nmps %>% filter(Source %in% c('OECD', 'IDB') ) %>% 
                # mutate(Item_Code=ifelse(Item=='Kapok fruit', 310, Item_Code)) %>%  filter(!is.na(Item_Code)) %>%
                group_by(CountryName) %>% 
                mutate(AGDCOMDIC = paste0(Item, collapse = "; "),
                   AGDCOMCODE = paste0(ProductCode, collapse = "; ") ) %>% ungroup() %>% 
                mutate(AGCOMNAME=paste0('NonMPS from Workbook - ', CountryName) ) 


select.nmps <- bind_rows(oecd.idb.nmps, mafap.nmps.lvs,mafap.nmps.crp) %>% filter(Source %in% c('OECD', 'IDB', 'FAO')) %>% 
               # filter(AGCOMNAME!='MAFAP') %>% 
              filter((Source=='OECD')| (CountryName %in% c('Benin','Burkina Faso','Ethiopia','Burundi',
                                                           'Kenya','Mali','Mozambique','Malawi','Senegal', 'Rwanda', 'Azerbaijan', 'Nigeria'))) %>% 
              select(cols.longlist)


# commodity.wide <- commodity.class %>% filter(Group %in% c('All arable crops', 'Grains', 'Oilseeds', 'Leguminous crops', 
#                                                           'Horticulture', 'Industrial crops', 'Roots and tubers', 'Fruits', 'Tree and vineyard')) %>% 
#                   spread(Group, 'Item_Code')


# Dictionary of other crops by countries  -----------------------------------------------------

othercrops.oecd <- commodity.bymps %>% filter(CountryName %in% oecd.country) %>% filter(Group=='Crops') %>% rename(Crops=Group) %>% 
                   mutate(Crops='All crops') %>% 
                   left_join(commodity.class %>% filter(Group=='All arable crops') %>% rename(Arable=Group)) %>% 
                   mutate(Arable=ifelse(is.na(Arable), 'Non-arable crops', Arable)) %>% 
                    left_join(commodity.class %>% filter(Group=='Grains') %>% rename(Grains=Group)) %>% 
                    mutate(Grains=ifelse(is.na(Grains), 'Non-grains', Grains)) %>% 
                    left_join(commodity.class %>% filter(Group=='Oilseeds') %>% rename(O_S=Group)) %>% 
                    mutate(O_S=ifelse(is.na(O_S), 'Non-oilseeds', O_S)) %>% 
                    left_join(commodity.class %>% filter(Group=='Leguminous crops') %>% rename(Legum=Group)) %>% 
                    mutate(Legum=ifelse(is.na(Legum), 'Non-legume', Legum)) %>%
                    left_join(commodity.class %>% filter(Group=='Protein crops') %>% rename(Protein=Group)) %>% 
                    mutate(Protein=ifelse(is.na(Protein), 'Non-protein', Protein)) %>%
                    left_join(commodity.class %>% filter(Group=='Horticulture') %>% rename(Horti=Group)) %>% 
                    mutate(Horti=ifelse(is.na(Horti), 'Non-horti', Horti)) %>%
                    left_join(commodity.class %>% filter(Group=='Cereals') %>% rename(Cereal=Group)) %>% 
                    mutate(Cereal=ifelse(is.na(Cereal), 'Non-cereals', Cereal)) %>% 
                    left_join(commodity.class %>% filter(Group=='Fruits') %>% rename(Frt=Group)) %>% 
                    mutate(Frt=ifelse(is.na(Frt), 'Non-fruit', Frt)) %>% 
                    left_join(commodity.class %>% filter(Group=='Industrial crops') %>% rename(Ind=Group)) %>% 
                    mutate(Ind=ifelse(is.na(Ind), 'Non-industrial', Ind)) %>% 
                    left_join(commodity.class %>% filter(Group=='Roots and tubers') %>% rename(Tuber=Group)) %>% 
                    mutate(Tuber=ifelse(is.na(Tuber), 'Non-tuber', Tuber)) %>%
                    left_join(commodity.class %>% filter(Group=='Tree and vineyard') %>% rename(TreV=Group)) %>% 
                    mutate(TreV=ifelse(is.na(TreV), 'Non-treeV', TreV)) %>%
                    left_join(commodity.class %>% filter(Group=='Non-insured crops') %>% rename(NonIns=Group)) %>% 
                    mutate(NonIns=ifelse(is.na(NonIns), 'Not-NonIns', NonIns)) 
  
  
  country.othcrp <- c('Switzerland', 'European Union', 'China', 'Norway', 'Mexico', 'United States of America', 'Türkiye', 'South Africa') 

  othercrops.oecd.rest <- othercrops.oecd %>% filter(!(CountryName %in% country.othcrp)) %>% 
                              mutate(OTH_CRP=case_when((F_V=='NonFV' & Arable=='Non-arable crops' & 
                                                          Grains=='Non-grains' & O_S=='Non-oilseeds')~'Other crops', 
                                                       TRUE~'Non-other crops'))
  
  othercrops.oecd.sel <- othercrops.oecd %>% filter(CountryName %in% country.othcrp) %>% 
                          mutate(OTH_CRP=case_when(
                                             (CountryName=='Switzerland' & F_V=='NonFV' & Arable=='Non-arable crops' & 
                                                Grains=='Non-grains' & O_S=='Non-oilseeds'& Legum=='Non-legume')~'Other crops', 
                                             (CountryName=='European Union' & F_V=='NonFV' & Arable=='Non-arable crops' & 
                                                Grains=='Non-grains' & O_S=='Non-oilseeds' & Protein=='Non-protein' & Cereal=='Non-cereals')~'Other crops', 
                                             (CountryName=='Norway' & F_V=='NonFV' & Arable=='Non-arable crops' & 
                                                Grains=='Non-grains' & O_S=='Non-oilseeds'& Tuber=='Non-tuber')~'Other crops',
                                             (CountryName=='China' & F_V=='NonFV' & Arable=='Non-arable crops' & 
                                                Grains=='Non-grains' & O_S=='Non-oilseeds'& 
                                                      (!(Item %in% c('Seed cotton, unginned'))))~'Other crops', 
                                             (CountryName=='Mexico' & F_V=='NonFV' & Arable=='Non-arable crops' & 
                                                Grains=='Non-grains' & O_S=='Non-oilseeds'& Frt=='Non-fruit' & Ind=='Non-industrial')~'Other crops', 
                                             (CountryName=='United States of America' & F_V=='NonFV' & Arable=='Non-arable crops' & 
                                                Grains=='Non-grains' & O_S=='Non-oilseeds'& TreV=='Non-treeV' & NonIns=='Not-NonIns')~'Other crops',
                                             (CountryName=='South Africa' & F_V=='NonFV' & Arable=='Non-arable crops' & 
                                                Grains=='Non-grains' & O_S=='Non-oilseeds'& Horti=='Non-horti')~'Other crops',
                                             (CountryName=='Türkiye' & F_V=='NonFV' & Arable=='Non-arable crops' & 
                                                Grains=='Non-grains' & O_S=='Non-oilseeds'& 
                                                (Item %in% c('Seed cotton, unginned', 'Sunflower seed', 'Sugar cane', 
                                                             'Sugar beet', 'Other sugar crops n.e.c.', 'Hazelnuts, in shell', 
                                                             'Unmanufactured tobacco')) )~'Other crops', 
                                             TRUE~'Non-other crops') ) 
  
  othcrp.oecd <- bind_rows(othercrops.oecd.rest, othercrops.oecd.sel) %>% mutate(
                           CountryName=case_when(CountryName=='United States of America'~'United States', 
                                                 CountryName=='Russian Federation'~'Russia', 
                                                 CountryName=='Republic of Korea'~'Korea',
                                                 TRUE~CountryName)) %>% 
                  select(CountryName, Item_Code, Item, OTH_CRP) %>% filter(OTH_CRP=='Other crops') %>% group_by(CountryName) %>% 
                  mutate(AGDCOMDIC = paste0(Item, collapse = "; "), 
                         AGDCOMCODE=paste0(Item_Code, collapse = "; ")) %>% ungroup() %>% 
                  mutate(AGCOMNAME=paste0(OTH_CRP, ' - ', CountryName)) %>%  select(cols.longlist)


# cols.list <- c('AGCOMNAME', 'AGDCOMDIC', 'AGDCOMCODE')
# cols.longlist <-  c('AGCOMNAME', 'Item', 'Item_Code', 'AGDCOMDIC', 'AGDCOMCODE')


# Other aggregate commodities and their decomposition -----------------------------------------------------------------

com.class <- commodity.class %>% rename(AGCOMNAME=Group) 

china.fv.imp <- commodity.bymps %>% filter(CountryName=='China' & F_V=='Fruits and vegetables') %>%  
                select(Item, ProductCode) %>% mutate(AGCOMNAME='China - Fruits and Vegetables Imported') %>% 
                rename(Item_Code=ProductCode) %>% filter(Item_Code!=26999)

china.fv.exp <- commodity.bymps %>% filter(CountryName=='China' & F_V=='Fruits and vegetables') %>%  
                select(Item, ProductCode) %>% mutate(AGCOMNAME='China - Fruits and Vegetables Exported') %>% 
                rename(Item_Code=ProductCode) %>% filter(Item_Code!=26999)

bovine.meat <- commodity.class %>% filter(Group=='Livestock' & grepl('Meat of cattle|Meat of buffalo', Item)) %>% 
                mutate(AGCOMNAME='Bovine Meat + (Total)') 

poultry.meat <- commodity.class %>% filter(Group=='Livestock' & 
                                             grepl('Meat of chickens|Meat of ducks|Meat of turkeys|Meat of pigeons|geese|rabbits', Item)) %>% 
                mutate(AGCOMNAME='Poultry Meat + (Total)') 

ag.total <- commodity.class %>% filter(Group %in% c('Crops', 'Livestock')) %>% mutate(AGCOMNAME='TOTAL') 


sugar.total <- commodity.class %>% filter(Group=='Crops' & (Item_Code %in% c(156,157,161))) %>% 
                mutate(AGCOMNAME="Sugar,Total (Raw Equiv.) + (Total)") 

unallocated <- ag.total %>% mutate(AGCOMNAME='Unallocated')

non.allocated.crops <- commodity.class %>% filter(Group=='Crops') %>% mutate(AGCOMNAME='Non-allocated crops') 
non.allocated.lvs <- commodity.class %>% filter(Group=='Livestock') %>% mutate(AGCOMNAME='Non-allocated livestock') 

all.crops.except.wine <- commodity.class %>% filter(Group=='Crops') %>% filter(Item!='Grapes') %>% 
                          mutate(AGCOMNAME='All crops except wine') 

all.crops.cattle.sheep <- commodity.class %>% filter(Group=='Crops') %>% 
                           rbind(commodity.class %>% filter(Group=='Livestock' & grepl('Meat of cattle|Meat of sheep', Item))) %>% 
                          mutate(AGCOMNAME='All crops, cattle and sheep') 

cereals.os.prtn <- commodity.class %>% filter(Group %in% c('Cereals', 'Oilseeds', 'Protein crops')) %>% 
                    mutate(AGCOMNAME='Cereals, oilseeds and protein crops') 

soy.wht.mze <- commodity.class %>% filter(Group=='Crops' & (Item_Code %in% c(236,56,446,15,89))) %>% 
                mutate(AGCOMNAME="Soybeans, wheat and maize") 

rc.mz.soy <- commodity.class %>% filter(Group=='Crops' & (Item_Code %in% c(27,236,56,446))) %>% 
              mutate(AGCOMNAME="Rice, maize and soybeans") 

wht.rc.mz.soy.co.ra <- commodity.class %>% filter(Group=='Crops' & (Item_Code %in% c(15,89,236,27,56,446,328,270))) %>% 
                      mutate(AGCOMNAME="Wheat, rice, maize, soybeans, cotton and rapeseed") 

wht.bar.soy <- commodity.class %>% filter(Group=='Crops' & (Item_Code %in% c(15,89,44,236))) %>% 
                mutate(AGCOMNAME="Wheat, barley and soybeans") 

mz.bn <- commodity.class %>% filter(Group=='Crops' & (Item_Code %in% c(56,446, 176,181,423,203,420,461,414))) %>% 
          mutate(AGCOMNAME="Maize and beans") 

mz.bn.rc <- commodity.class %>% filter(Group=='Crops' & (Item_Code %in% c(56,446,176,181,423,203,420,461,414,27))) %>% 
          mutate(AGCOMNAME="Maize, beans and rice") 


sor.mz.os <- commodity.class %>% filter((Group=='Crops' & (Item_Code %in% c(83,56,446)))|(Group=='Oilseeds')) %>% 
              mutate(AGCOMNAME="Sorghum, maize and oilseeds") 

wht.sug.cot.sun <- commodity.class %>% filter(Group=='Crops' & 
                                                (Item_Code %in% c(15,89, 156,157,161,328,267))) %>% 
                    mutate(AGCOMNAME="Wheat, sugar, cotton and sunflower") 

frt.minus.or.gr <- commodity.class %>% filter(Group=='Fruits') %>% filter(!(Item_Code %in% c(490,507))) %>% 
                    mutate(AGCOMNAME='All Fruits -(Oranges & Grapefruit)') 


orng.grap <- commodity.class %>% filter(Item_Code %in% c(490,507)) %>% 
              mutate(AGCOMNAME='Oranges and grapefruit') 

haz.tob <- commodity.class %>% filter(Group=='Crops' & (Item_Code %in% c(225,826))) %>% 
            mutate(AGCOMNAME="Hazelnuts and tobacco") 

soy.rap <- commodity.class %>% filter(Group=='Crops' & (Item_Code %in% c(236,270))) %>% 
            mutate(AGCOMNAME="Soybeans and rapeseed") 

grn.os <- commodity.class %>% filter(Group %in% c("Grains", "Oilseeds")) %>% mutate(AGCOMNAME='Grains and oilseeds')

bf.mk <- commodity.class %>% filter(Group=='Livestock' & grepl('Meat of cattle|milk', Item)) %>% 
          mutate(AGCOMNAME="Beef and milk") 

bf.sh <- commodity.class %>% filter(Group=='Livestock' & grepl('Meat of cattle|Meat of sheep', Item)) %>% 
          mutate(AGCOMNAME="Beef and sheep meat") 

sh.wool.bf.mk <- commodity.class %>% filter(Group=='Livestock' & grepl('Meat of sheep|milk|Meat of cattle|milk|wool', Item)) %>% 
                  mutate(AGCOMNAME="Sheep meat, wool, beef and milk") 

sh.wool <- commodity.class %>% filter(Group=='Livestock' & grepl('Meat of sheep|wool', Item)) %>% 
            mutate(AGCOMNAME="Sheep meat and wool") 

pg.eggs <- commodity.class %>% filter(Group=='Livestock' & grepl('Meat of pig with|Eggs|Hen eggs', Item)) %>% 
            mutate(AGCOMNAME="Pig meat and eggs") 

pl.eggs <- commodity.class %>% filter(Group=='Livestock' & 
                                        grepl('Meat of chickens|Meat of ducks|Meat of turkeys|Eggs|Hen eggs|Meat of pigeons|geese|rabbits', Item)) %>% 
             mutate(AGCOMNAME="Poultry and eggs") 

pl.pig <- commodity.class %>% filter(Group=='Livestock' & 
                                       grepl('Meat of chickens|Meat of ducks|Meat of turkeys|Meat of pigeons|geese|Meat of pig with|rabbits', Item)) %>% 
          mutate(AGCOMNAME="Poultry and pig") 

bf.pig <- commodity.class %>% filter(Group=='Livestock' & grepl('Meat of cattle|Meat of pig with', Item)) %>% 
          mutate(AGCOMNAME="Beef and pig meat") 

bv.ov.pg.gt.hn <- commodity.class %>% filter(Group=='Livestock' & 
                                               grepl('Meat of cattle|Meat of pig with|Meat of sheep|Meat of goat|honey|Meat of buffalo', Item)) %>% 
                    mutate(AGCOMNAME="Bovine, ovine, caprine, porcine and honey")

all.exc.mk.mt <- commodity.class %>% filter(Group %in% c('Crops', 'Livestock', 'Other')) %>% 
                  filter(!grepl('Meat|milk', Item)) %>% 
                  mutate(AGCOMNAME='All except milk and meat') 

all.sup.com <- commodity.class %>% filter(Group=='Livestock') %>% 
                mutate(AGCOMNAME='All supply managed commodities')

frt.flw.ind <- commodity.class %>% filter(Group %in% c('Fruits', 'Industrial crops')) %>% 
                mutate(AGCOMNAME='Fruits, flowers, industrial crops')

noninsured <- commodity.class %>% filter(Group=='Non-insured crops') %>% 
                mutate(AGCOMNAME='Non-insured crops')

greenhouse <- commodity.class %>% filter(Group=='Greenhouse vegetables') %>% 
              mutate(AGCOMNAME='Greenhouse vegetables')

alternative <- commodity.class %>% filter(Group=='Alternative crops') %>% 
                mutate(AGCOMNAME='Alternative crops')

olive.cit.nuts.leg.garlic <- commodity.class %>% filter(Group %in% c('Citruses','Nuts','Leguminous crops')) %>% 
                             rbind(commodity.class %>% 
                                     filter(Item %in% c('Raw milk of camel',
                                                        'Raw milk of cattle',
                                                        'Raw milk of goats',
                                                        'Raw milk of sheep', 
                                                        'Raw milk of buffalo'))) %>% 
                              rbind(commodity.class %>% filter(Item=='Olives' & Group=='Crops')) %>% 
                              rbind(commodity.class %>% filter(Item=='Green garlic' & Group=='Crops')) %>% 
                              mutate(AGCOMNAME='Olive, citrus fruits, nuts, legumes, cheese and garlic')
                        


# Merging of all commodity dataframes ---------------------------------------------------------
df.list <- list(com.class, bovine.meat, poultry.meat, ag.total, sugar.total, unallocated, non.allocated.crops, non.allocated.lvs, all.crops.except.wine, 
                all.crops.cattle.sheep, cereals.os.prtn, soy.wht.mze, wht.rc.mz.soy.co.ra, wht.bar.soy, mz.bn, sor.mz.os, wht.sug.cot.sun,
                frt.minus.or.gr, orng.grap, haz.tob, grn.os, bf.mk, bf.sh,sh.wool.bf.mk, sh.wool, pg.eggs, pl.eggs, pl.pig, bf.pig, 
                all.exc.mk.mt, all.sup.com, frt.flw.ind,china.fv.imp, china.fv.exp, noninsured, greenhouse, alternative,rc.mz.soy,mz.bn.rc, soy.rap,
                bv.ov.pg.gt.hn,olive.cit.nuts.leg.garlic)

df.concat=NULL

for (i in df.list){
  
  i <- concat.function(i)
  df.concat <- rbind(df.concat, i)
}

df.concat.final <- df.concat %>% 
                   filter(!(AGCOMNAME %in% c('Industrial crops', 'Fibre crops', 'Nuts', 'Cereals', 'Other'))) %>% 
                   rbind(select.nmps) %>% mutate(AGCOMNAME=case_when(AGCOMNAME=='Crops'~'All crops', 
                                                                     AGCOMNAME=='Oilseeds'~'Oilseeds + (Total)',
                                                                     AGCOMNAME=='Livestock'~'All livestock',
                                                                     AGCOMNAME=='Roots and tubers'~'Tubers',
                                                                     TRUE~AGCOMNAME)) %>% 
                    rbind(othcrp.oecd)
                  

# Producing final outputs ---------------------------------------------------------------------
aggregatecom <- read_excel('Source/Aggregate_Commodity_Composition.xlsx', sheet = 'AggregateCommodity') %>%
                select(AGCOMCODE, AGCOMNAME) %>% distinct()


dff.concat.long <- df.concat.final %>% select(-AGDCOMCODE, -AGDCOMDIC) %>% distinct()

readr::write_excel_csv(dff.concat.long, 'Mapping/AggregateCommodity_Long.csv', na='')

dff.concat.dic <- df.concat.final %>% select(-AGDCOMCODE, -AGDCOMDIC) %>% distinct() %>% group_by(AGCOMNAME) %>% 
                  mutate(AGDCOMCODE = paste0(Item, collapse = "; "),
                         AGDCOMDIC = paste0(Item_Code, collapse = "; ") ) %>% ungroup() %>% select(-Item, -Item_Code) %>% distinct() 
                  # %>% 
                  # right_join(aggregatecom) %>% select(AGCOMCODE, AGCOMNAME, AGDCOMCODE, AGDCOMDIC) 

readr::write_excel_csv(dff.concat.dic, 'Mapping/AggregateCommodity_DIC.csv', na='')



# f_v.na <- commodity.bymps %>% filter(Group=='Crops') %>% left_join(f_v) %>% filter(is.na(F_V)) %>% select(Item, ProductCode) %>% distinct()
# 
# commodity.bymps.na <- commodity.bymps %>% filter(is.na(Group)) %>% select(Item, ProductCode) %>% distinct()

# write.csv(commodity.bymps.na, 'Commodity_Missing_List.csv', na='', row.names = F)
# write.csv(f_v.na, 'F_V_Missing_List.csv', na='', row.names = F)



# faostat.na <- faostat.commodity %>% filter(is.na(Item.Code))
# 
# write.csv(faostat.na, 'FAOSTAT_VoP_Data_Missing.csv', na='', row.names = F)













