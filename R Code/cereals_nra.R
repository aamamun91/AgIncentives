
library(readxl)
library(FAOSTAT)
library(tidyverse)
library(stringr)

cereals.list <- read_excel('./Mapping/Aggregate_Commodity.xlsx', sheet = 'AG_COM_Long') %>% select(-Item_Code) %>% 
                filter(AGCOMNAME=='Grains') %>% mutate(Item=case_when(Item=='Rice, paddy'~'Rice', TRUE~Item))

detailed_NRA <- read_excel('./NRA_Output/Support_Database_2021_version3a.xlsx', sheet = 'Detailed_NRA')

cerealsdata <- detailed_NRA %>% filter(CATPROD=='CRP') %>% 
               select(Country_Label, Country_Code,Commodity_Label, Year, Category,Support_USD,NRA_Cat,RelevantProduction,rate) %>% 
             left_join(cereals.list, by=c('Commodity_Label'='Item')) %>% filter(AGCOMNAME=='Grains')

aggregateNRA <- read_excel('./NRA_Output/Support_Database_2021_version3a.xlsx', sheet = 'Aggregated_NRA')

aggregatenra_country <- aggregateNRA %>% filter(AGGREGATION=='CountryXSector') %>% filter(CATPROD=='CRP')